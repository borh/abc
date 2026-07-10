//! `tokenize-plaintext`: tokenize rendered plaintext files with one analyzer
//! and write per-work `<work-id>.tokens.jsonl` token files. Replaces the
//! Clojure-side MeCab-stdout parser in the abc join-stats pipeline.

use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use ab_morph_diff::Analysis;
use ab_plaintext::{PlainTextDocument, SourceFormat};
use anyhow::{Context, Result, bail};
use serde::Serialize;

use crate::{LoadedAnalyzer, load_analyzers, parse_analyzer_specs};

/// File name of the per-work analyzer-error sidecar, always written into
/// `--out-dir` (zero-byte when every work tokenized cleanly).
pub const TOKENIZE_ERRORS_FILE: &str = "tokenize-errors.jsonl";

/// Summary printed as a single compact JSON line on stdout on success.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TokenizePlaintextSummary {
    pub works: u64,
    pub tokens: u64,
    pub analyzer: String,
    pub warnings: u64,
    pub errors: u64,
}

#[derive(Serialize)]
struct TokenRow<'a> {
    surface: &'a str,
    char_start: usize,
    char_end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct WorkErrorRow {
    work_id: String,
    error: String,
}

#[derive(Debug, Default)]
struct ShardOutput {
    works: u64,
    tokens: u64,
    warnings: u64,
    errors: Vec<WorkErrorRow>,
}

enum WorkOutcome {
    Tokenized { tokens: u64, warnings: u64 },
    AnalyzerFailed(WorkErrorRow),
}

/// The per-document tokenization seam: `run_tokenize_plaintext` drives it with
/// a [`LoadedAnalyzer`]; unit tests substitute a dictionary-free fake.
trait DocumentTokenizer: Sync {
    fn analyzer_id(&self) -> &str;
    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis>;
}

impl DocumentTokenizer for LoadedAnalyzer {
    fn analyzer_id(&self) -> &str {
        LoadedAnalyzer::analyzer_id(self)
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis> {
        LoadedAnalyzer::analyze(self, document)
    }
}

/// Tokenize every `*.txt` file directly in `plaintext_dir` (not recursive)
/// with exactly one analyzer, writing `<out_dir>/<work-id>.tokens.jsonl`
/// per work (one compact JSON object per morpheme). Works whose analysis
/// fails (a known per-work degenerate class, e.g. span-reconstruction
/// mismatches) get no tokens file; they are recorded in
/// `<out_dir>/tokenize-errors.jsonl` instead and the run still succeeds,
/// mirroring the analyze-aat warehouse error lane.
///
/// # Errors
///
/// Returns an error when more than one analyzer is requested, the analyzer
/// cannot be loaded, no `.txt` inputs are found, or any read/write step
/// fails (no partial-success exit for IO failures).
pub fn run_tokenize_plaintext(
    analyzer_ids: &[String],
    plaintext_dir: &Path,
    out_dir: &Path,
    jobs: usize,
) -> Result<TokenizePlaintextSummary> {
    if analyzer_ids.len() != 1 {
        bail!(
            "tokenize-plaintext requires exactly one --analyzer (got {})",
            analyzer_ids.len()
        );
    }
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer = analyzers
        .into_iter()
        .next()
        .context("internal error: no analyzer loaded")?;
    tokenize_all(&*analyzer, plaintext_dir, out_dir, jobs)
}

fn tokenize_all<T: DocumentTokenizer>(
    tokenizer: &T,
    plaintext_dir: &Path,
    out_dir: &Path,
    jobs: usize,
) -> Result<TokenizePlaintextSummary> {
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
    let inputs = discover_plaintext_inputs(plaintext_dir)?;
    fs::create_dir_all(out_dir)
        .with_context(|| format!("failed to create {}", out_dir.display()))?;

    let mut output = if jobs == 1 {
        tokenize_shard(tokenizer, &inputs, out_dir)?
    } else {
        tokenize_parallel(tokenizer, inputs, out_dir, jobs)?
    };

    // Sorted by work_id so the error file is deterministic across --jobs.
    output.errors.sort_by(|a, b| a.work_id.cmp(&b.work_id));
    write_error_file(out_dir, &output.errors)?;

    Ok(TokenizePlaintextSummary {
        works: output.works,
        tokens: output.tokens,
        analyzer: tokenizer.analyzer_id().to_owned(),
        warnings: output.warnings,
        errors: output.errors.len() as u64,
    })
}

fn discover_plaintext_inputs(plaintext_dir: &Path) -> Result<Vec<PathBuf>> {
    if !plaintext_dir.is_dir() {
        bail!(
            "--plaintext-dir must point to an existing directory: {}",
            plaintext_dir.display()
        );
    }
    let mut paths = Vec::new();
    for entry in fs::read_dir(plaintext_dir)
        .with_context(|| format!("failed to read {}", plaintext_dir.display()))?
    {
        let path = entry
            .with_context(|| format!("failed to read entry in {}", plaintext_dir.display()))?
            .path();
        if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("txt") {
            paths.push(path);
        }
    }
    paths.sort();
    if paths.is_empty() {
        bail!("no .txt files found in {}", plaintext_dir.display());
    }
    Ok(paths)
}

fn tokenize_parallel<T: DocumentTokenizer>(
    tokenizer: &T,
    inputs: Vec<PathBuf>,
    out_dir: &Path,
    jobs: usize,
) -> Result<ShardOutput> {
    let partitions = crate::pipeline::partition_inputs(inputs, jobs);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for partition in &partitions {
            if partition.is_empty() {
                continue;
            }
            handles.push(scope.spawn(move || tokenize_shard(tokenizer, partition, out_dir)));
        }
        let mut totals = ShardOutput::default();
        for handle in handles {
            let output = handle.join().expect("tokenize-plaintext worker panicked")?;
            totals.works += output.works;
            totals.tokens += output.tokens;
            totals.warnings += output.warnings;
            totals.errors.extend(output.errors);
        }
        Ok(totals)
    })
}

fn tokenize_shard<T: DocumentTokenizer>(
    tokenizer: &T,
    inputs: &[PathBuf],
    out_dir: &Path,
) -> Result<ShardOutput> {
    let mut output = ShardOutput::default();
    for input in inputs {
        match tokenize_file(tokenizer, input, out_dir)? {
            WorkOutcome::Tokenized { tokens, warnings } => {
                output.works += 1;
                output.tokens += tokens;
                output.warnings += warnings;
            }
            WorkOutcome::AnalyzerFailed(row) => output.errors.push(row),
        }
    }
    Ok(output)
}

fn tokenize_file<T: DocumentTokenizer>(
    tokenizer: &T,
    input: &Path,
    out_dir: &Path,
) -> Result<WorkOutcome> {
    let work_id = input
        .file_stem()
        .and_then(|stem| stem.to_str())
        .with_context(|| format!("non-UTF-8 file name: {}", input.display()))?;
    let text =
        fs::read_to_string(input).with_context(|| format!("failed to read {}", input.display()))?;
    // Analyze before creating the tokens file so a failed work leaves no
    // tokens file behind. An empty text produces an empty (zero-byte) tokens
    // file without consulting the analyzer, so the contract holds for every
    // analyzer.
    let analysis = if text.is_empty() {
        None
    } else {
        let document = PlainTextDocument {
            text_id: work_id.to_owned(),
            source_format: SourceFormat::AatVisibleText,
            text,
        };
        match tokenizer.analyze(&document) {
            Ok(analysis) => Some(analysis),
            Err(error) => {
                return Ok(WorkOutcome::AnalyzerFailed(WorkErrorRow {
                    work_id: work_id.to_owned(),
                    error: format!("{error:#}"),
                }));
            }
        }
    };
    let out_path = out_dir.join(format!("{work_id}.tokens.jsonl"));
    let file = File::create(&out_path)
        .with_context(|| format!("failed to create {}", out_path.display()))?;
    let mut writer = BufWriter::new(file);
    let morphemes = analysis
        .as_ref()
        .map(|a| a.morphemes.as_slice())
        .unwrap_or(&[]);
    for morpheme in morphemes {
        serde_json::to_writer(
            &mut writer,
            &TokenRow {
                surface: &morpheme.surface,
                char_start: morpheme.char_span.start,
                char_end: morpheme.char_span.end,
            },
        )
        .with_context(|| format!("failed to write {}", out_path.display()))?;
        writer
            .write_all(b"\n")
            .with_context(|| format!("failed to write {}", out_path.display()))?;
    }
    writer
        .flush()
        .with_context(|| format!("failed to write {}", out_path.display()))?;
    Ok(WorkOutcome::Tokenized {
        tokens: morphemes.len() as u64,
        warnings: analysis
            .as_ref()
            .map(|a| a.warnings.len() as u64)
            .unwrap_or(0),
    })
}

fn write_error_file(out_dir: &Path, errors: &[WorkErrorRow]) -> Result<()> {
    let path = out_dir.join(TOKENIZE_ERRORS_FILE);
    let file =
        File::create(&path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut writer = BufWriter::new(file);
    for row in errors {
        serde_json::to_writer(&mut writer, row)
            .with_context(|| format!("failed to write {}", path.display()))?;
        writer
            .write_all(b"\n")
            .with_context(|| format!("failed to write {}", path.display()))?;
    }
    writer
        .flush()
        .with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{TestAnalyzerKind, test_analysis};

    /// Splits like `test:split`, but fails span reconstruction (like the real
    /// qkana degenerate-work class) for any document containing `壊`.
    struct FlakySplitTokenizer;

    impl DocumentTokenizer for FlakySplitTokenizer {
        fn analyzer_id(&self) -> &str {
            "test:flaky-split"
        }

        fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis> {
            if document.text.contains('壊') {
                bail!(
                    "emitted surface `壊` that did not match text `{}` at byte 0",
                    document.text_id
                );
            }
            Ok(test_analysis(TestAnalyzerKind::Split, document))
        }
    }

    fn write_input(dir: &Path, name: &str, content: &str) {
        fs::write(dir.join(name), content).unwrap();
    }

    fn run(
        analyzer: &str,
        plaintext_dir: &Path,
        out_dir: &Path,
        jobs: usize,
    ) -> Result<TokenizePlaintextSummary> {
        run_tokenize_plaintext(&[analyzer.to_owned()], plaintext_dir, out_dir, jobs)
    }

    #[test]
    fn tokenizes_txt_files_and_writes_jsonl_per_work() {
        let temp = tempfile::tempdir().unwrap();
        let input_dir = temp.path().join("plain");
        let out_dir = temp.path().join("out");
        fs::create_dir_all(&input_dir).unwrap();
        // `test:split` splits exactly "今日" into two morphemes.
        write_input(&input_dir, "work-a.txt", "今日");
        write_input(&input_dir, "work-b.txt", "今日");
        write_input(&input_dir, "empty.txt", "");
        write_input(&input_dir, "ignored.md", "今日");

        let summary = run("test:split", &input_dir, &out_dir, 1).unwrap();

        assert_eq!(
            summary,
            TokenizePlaintextSummary {
                works: 3,
                tokens: 4,
                analyzer: "test:split".to_owned(),
                warnings: 0,
                errors: 0,
            }
        );
        let work_a = fs::read_to_string(out_dir.join("work-a.tokens.jsonl")).unwrap();
        assert_eq!(
            work_a,
            "{\"surface\":\"今\",\"char_start\":0,\"char_end\":1}\n{\"surface\":\"日\",\"char_start\":1,\"char_end\":2}\n"
        );
        assert!(out_dir.join("work-b.tokens.jsonl").is_file());
        let empty = fs::metadata(out_dir.join("empty.tokens.jsonl")).unwrap();
        assert_eq!(empty.len(), 0);
        assert!(!out_dir.join("ignored.tokens.jsonl").exists());
        let errors = fs::metadata(out_dir.join(TOKENIZE_ERRORS_FILE)).unwrap();
        assert_eq!(errors.len(), 0, "clean run writes a zero-byte error file");
    }

    #[test]
    fn analyzer_failure_is_a_per_work_error_not_fatal() {
        let temp = tempfile::tempdir().unwrap();
        let input_dir = temp.path().join("plain");
        let out_dir = temp.path().join("out");
        fs::create_dir_all(&input_dir).unwrap();
        write_input(&input_dir, "work-a.txt", "今日");
        write_input(&input_dir, "work-bad.txt", "今日壊今日");
        write_input(&input_dir, "work-c.txt", "今日");

        let summary = tokenize_all(&FlakySplitTokenizer, &input_dir, &out_dir, 1).unwrap();

        assert_eq!(
            summary,
            TokenizePlaintextSummary {
                works: 2,
                tokens: 4,
                analyzer: "test:flaky-split".to_owned(),
                warnings: 0,
                errors: 1,
            }
        );
        assert!(out_dir.join("work-a.tokens.jsonl").is_file());
        assert!(out_dir.join("work-c.tokens.jsonl").is_file());
        assert!(
            !out_dir.join("work-bad.tokens.jsonl").exists(),
            "a failed work must leave no tokens file"
        );
        let errors = fs::read_to_string(out_dir.join(TOKENIZE_ERRORS_FILE)).unwrap();
        let rows: Vec<serde_json::Value> = errors
            .lines()
            .map(|line| serde_json::from_str(line).unwrap())
            .collect();
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0]["work_id"], "work-bad");
        assert!(
            rows[0]["error"]
                .as_str()
                .unwrap()
                .contains("did not match text"),
            "{errors}"
        );
    }

    #[test]
    fn analyzer_failures_are_deterministic_across_jobs() {
        let temp = tempfile::tempdir().unwrap();
        let input_dir = temp.path().join("plain");
        fs::create_dir_all(&input_dir).unwrap();
        for index in 0..4 {
            write_input(&input_dir, &format!("work-{index}.txt"), "今日");
        }
        write_input(&input_dir, "bad-a.txt", "壊");
        write_input(&input_dir, "bad-b.txt", "壊");

        let serial_out = temp.path().join("serial");
        let parallel_out = temp.path().join("parallel");
        let serial = tokenize_all(&FlakySplitTokenizer, &input_dir, &serial_out, 1).unwrap();
        let parallel = tokenize_all(&FlakySplitTokenizer, &input_dir, &parallel_out, 2).unwrap();

        assert_eq!(serial, parallel);
        assert_eq!(serial.works, 4);
        assert_eq!(serial.errors, 2);
        for entry in fs::read_dir(&serial_out).unwrap() {
            let name = entry.unwrap().file_name();
            let left = fs::read(serial_out.join(&name)).unwrap();
            let right = fs::read(parallel_out.join(&name)).unwrap();
            assert_eq!(left, right, "output mismatch for {name:?}");
        }
        let errors = fs::read_to_string(serial_out.join(TOKENIZE_ERRORS_FILE)).unwrap();
        let work_ids: Vec<String> = errors
            .lines()
            .map(|line| {
                serde_json::from_str::<serde_json::Value>(line).unwrap()["work_id"]
                    .as_str()
                    .unwrap()
                    .to_owned()
            })
            .collect();
        assert_eq!(work_ids, vec!["bad-a", "bad-b"], "sorted by work_id");
    }

    #[test]
    fn parallel_jobs_match_serial_output() {
        let temp = tempfile::tempdir().unwrap();
        let input_dir = temp.path().join("plain");
        fs::create_dir_all(&input_dir).unwrap();
        for index in 0..5 {
            write_input(&input_dir, &format!("work-{index}.txt"), "今日");
        }
        write_input(&input_dir, "empty.txt", "");

        let serial_out = temp.path().join("serial");
        let parallel_out = temp.path().join("parallel");
        let serial = run("test:split", &input_dir, &serial_out, 1).unwrap();
        let parallel = run("test:split", &input_dir, &parallel_out, 3).unwrap();

        assert_eq!(serial, parallel);
        assert_eq!(serial.works, 6);
        for entry in fs::read_dir(&serial_out).unwrap() {
            let name = entry.unwrap().file_name();
            let left = fs::read(serial_out.join(&name)).unwrap();
            let right = fs::read(parallel_out.join(&name)).unwrap();
            assert_eq!(left, right, "output mismatch for {name:?}");
        }
    }

    #[test]
    fn rejects_more_than_one_analyzer() {
        let temp = tempfile::tempdir().unwrap();
        let err = run_tokenize_plaintext(
            &["test:split".to_owned(), "test:single".to_owned()],
            temp.path(),
            temp.path(),
            1,
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one --analyzer"), "{err}");
    }

    #[test]
    fn rejects_zero_jobs() {
        let temp = tempfile::tempdir().unwrap();
        let err = run_tokenize_plaintext(&["test:split".to_owned()], temp.path(), temp.path(), 0)
            .unwrap_err();
        assert!(err.to_string().contains("--jobs"), "{err}");
    }

    #[test]
    fn fails_on_missing_plaintext_dir() {
        let temp = tempfile::tempdir().unwrap();
        let missing = temp.path().join("does-not-exist");
        let err = run("test:split", &missing, &temp.path().join("out"), 1).unwrap_err();
        assert!(err.to_string().contains("does-not-exist"), "{err}");
    }

    #[test]
    fn fails_on_dir_without_txt_files() {
        let temp = tempfile::tempdir().unwrap();
        let input_dir = temp.path().join("plain");
        fs::create_dir_all(&input_dir).unwrap();
        write_input(&input_dir, "notes.md", "今日");

        let err = run("test:split", &input_dir, &temp.path().join("out"), 1).unwrap_err();
        assert!(err.to_string().contains("no .txt files"), "{err}");
    }
}
