//! `tokenize-plaintext`: tokenize rendered plaintext files with one analyzer
//! and write per-work `<work-id>.tokens.jsonl` token files. Replaces the
//! Clojure-side MeCab-stdout parser in the abc join-stats pipeline.

use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ab_plaintext::{PlainTextDocument, SourceFormat};
use anyhow::{Context, Result, bail};
use serde::Serialize;

use crate::{LoadedAnalyzer, load_analyzers, parse_analyzer_specs};

/// Summary printed as a single compact JSON line on stdout on success.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TokenizePlaintextSummary {
    pub works: u64,
    pub tokens: u64,
    pub analyzer: String,
    pub warnings: u64,
}

#[derive(Serialize)]
struct TokenRow<'a> {
    surface: &'a str,
    char_start: usize,
    char_end: usize,
}

#[derive(Debug, Default, Clone, Copy)]
struct ShardCounts {
    works: u64,
    tokens: u64,
    warnings: u64,
}

/// Tokenize every `*.txt` file directly in `plaintext_dir` (not recursive)
/// with exactly one analyzer, writing `<out_dir>/<work-id>.tokens.jsonl`
/// per work (one compact JSON object per morpheme).
///
/// # Errors
///
/// Returns an error when more than one analyzer is requested, the analyzer
/// cannot be loaded, no `.txt` inputs are found, or any read/analyze/write
/// step fails (no partial-success exit).
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
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer = analyzers
        .into_iter()
        .next()
        .context("internal error: no analyzer loaded")?;

    let inputs = discover_plaintext_inputs(plaintext_dir)?;
    fs::create_dir_all(out_dir)
        .with_context(|| format!("failed to create {}", out_dir.display()))?;

    let counts = if jobs == 1 {
        tokenize_shard(&analyzer, &inputs, out_dir)?
    } else {
        tokenize_parallel(&analyzer, inputs, out_dir, jobs)?
    };

    Ok(TokenizePlaintextSummary {
        works: counts.works,
        tokens: counts.tokens,
        analyzer: analyzer.analyzer_id().to_owned(),
        warnings: counts.warnings,
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

fn tokenize_parallel(
    analyzer: &Arc<LoadedAnalyzer>,
    inputs: Vec<PathBuf>,
    out_dir: &Path,
    jobs: usize,
) -> Result<ShardCounts> {
    let partitions = crate::pipeline::partition_inputs(inputs, jobs);
    std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for partition in &partitions {
            if partition.is_empty() {
                continue;
            }
            let analyzer = Arc::clone(analyzer);
            handles.push(scope.spawn(move || tokenize_shard(&analyzer, partition, out_dir)));
        }
        let mut totals = ShardCounts::default();
        for handle in handles {
            let counts = handle.join().expect("tokenize-plaintext worker panicked")?;
            totals.works += counts.works;
            totals.tokens += counts.tokens;
            totals.warnings += counts.warnings;
        }
        Ok(totals)
    })
}

fn tokenize_shard(
    analyzer: &LoadedAnalyzer,
    inputs: &[PathBuf],
    out_dir: &Path,
) -> Result<ShardCounts> {
    let mut counts = ShardCounts::default();
    for input in inputs {
        let file_counts = tokenize_file(analyzer, input, out_dir)?;
        counts.works += 1;
        counts.tokens += file_counts.tokens;
        counts.warnings += file_counts.warnings;
    }
    Ok(counts)
}

fn tokenize_file(analyzer: &LoadedAnalyzer, input: &Path, out_dir: &Path) -> Result<ShardCounts> {
    let work_id = input
        .file_stem()
        .and_then(|stem| stem.to_str())
        .with_context(|| format!("non-UTF-8 file name: {}", input.display()))?;
    let text =
        fs::read_to_string(input).with_context(|| format!("failed to read {}", input.display()))?;
    let out_path = out_dir.join(format!("{work_id}.tokens.jsonl"));
    let file = File::create(&out_path)
        .with_context(|| format!("failed to create {}", out_path.display()))?;
    let mut writer = BufWriter::new(file);
    // An empty text produces an empty (zero-byte) tokens file without
    // consulting the analyzer, so the contract holds for every analyzer.
    if text.is_empty() {
        writer
            .flush()
            .with_context(|| format!("failed to write {}", out_path.display()))?;
        return Ok(ShardCounts {
            works: 1,
            tokens: 0,
            warnings: 0,
        });
    }
    let document = PlainTextDocument {
        text_id: work_id.to_owned(),
        source_format: SourceFormat::AatVisibleText,
        text,
    };
    let analysis = analyzer
        .analyze(&document)
        .with_context(|| format!("failed to analyze {}", input.display()))?;
    for morpheme in &analysis.morphemes {
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
    Ok(ShardCounts {
        works: 1,
        tokens: analysis.morphemes.len() as u64,
        warnings: analysis.warnings.len() as u64,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

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
