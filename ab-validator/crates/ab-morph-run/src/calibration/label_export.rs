//! Pooled blind labeling TSV export (spec §Calibration Plan step 4): union
//! of several ranking JSONs' pattern pools, deduped, seeded-Fisher-Yates
//! shuffled into blind `label_id`s, with char-sliced text snippets pulled
//! from the AAT corpus. `mapping.json` is the *only* place `pattern_id` and
//! per-method ranks are recorded — the TSV itself carries no method, rank,
//! score, or pattern_id column, so a human labeler cannot infer which
//! method(s) favored a row.
//!
//! Snippet sourcing: for a pooled pattern, the first `--input` (in
//! argument order) that carries it wins its `region_examples` — full-corpus
//! rankings share region examples for patterns they hold in common, so
//! which one wins is immaterial to snippet content in practice.

use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};

use super::read_ranking;
use crate::summary::{InterestingRow, InterestingSummary, splitmix64};

/// Options for [`run_export_labels`].
#[derive(Debug, Clone)]
pub struct ExportLabelsOptions {
    /// Ranking artifact JSONs to pool; the file stem of each is its method
    /// name in `mapping.json` (stems must be unique).
    pub inputs: Vec<PathBuf>,
    /// Directory of `<source_id>.json` AAT files backing the snippets.
    pub aat_dir: PathBuf,
    /// Blind labeling TSV output path.
    pub output: PathBuf,
    /// `mapping.json` sidecar output path (label_id → pattern_id + ranks).
    pub mapping_output: PathBuf,
    /// Fisher–Yates seed for the blind shuffle.
    pub seed: u64,
    /// Max `region_examples` (and thus snippet columns) per pattern.
    pub snippets_per_pattern: usize,
    /// Chars of context on each side of the bracketed disagreement span.
    pub context_chars: usize,
    /// Overwrite existing outputs.
    pub force: bool,
}

/// Summary of an export run, for a one-line CLI status message.
#[derive(Debug, Clone, Serialize)]
pub struct ExportSummary {
    pub rows: usize,
    pub methods: Vec<String>,
}

const VERDICTS: [&str; 6] = [
    "bug",
    "expected-policy",
    "expected-dictionary",
    "corpus-artifact",
    "noise",
    "unclear",
];

const HEADER_COMMENT_INTRO: &str = "# Blind pooled labeling export (spec §Calibration Plan). Fill in `verdict` \
     with exactly one of:";

const VERDICT_DESCRIPTIONS: [&str; 6] = [
    "genuine analyzer/segmentation bug, not explained by policy or dictionary",
    "disagreement follows from an intentional, documented segmentation/feature policy",
    "disagreement traces to differing dictionary entries (known, acceptable)",
    "disagreement traces to OCR/encoding/corpus noise, not the analyzers",
    "disagreement is uninformative (e.g. tie-breaking on a trivial span)",
    "insufficient evidence in the snippet to decide",
];

/// `mapping.json` entry (label_id → pattern_id + per-method ranks). Shared
/// (via `pub(crate)`) with `label_score`, which deserializes a filled
/// `mapping.json` sidecar to score p@k/nDCG@k per method.
#[derive(Serialize, Deserialize)]
pub(crate) struct MappingEntry {
    pub(crate) pattern_id: String,
    pub(crate) ranks: BTreeMap<String, usize>,
}

struct LabelRow {
    label_id: String,
    kind: String,
    pattern: String,
    examples: usize,
    source_count: usize,
    text_count: usize,
    snippets: Vec<String>,
}

/// Runs the pooled blind labeling export: reads `opts.inputs` (ranking
/// JSONs), pools their `pattern_id`s (deduped, union), blind-shuffles the
/// pool into `label_id`s, slices text snippets from the AAT corpus, and
/// writes the labeling TSV plus its `mapping.json` sidecar.
///
/// # Errors
///
/// Returns an error when: an output already exists and `--force` was not
/// given; two inputs share a file stem (method name); a ranking JSON fails
/// to parse; a `region_examples` span is out of range for its source text;
/// or an AAT file referenced by a `region_examples.source_id` is missing.
pub fn run_export_labels(opts: &ExportLabelsOptions) -> Result<ExportSummary> {
    if opts.output.exists() && !opts.force {
        bail!(
            "refusing to overwrite {} (pass --force to allow)",
            opts.output.display()
        );
    }
    if opts.mapping_output.exists() && !opts.force {
        bail!(
            "refusing to overwrite {} (pass --force to allow)",
            opts.mapping_output.display()
        );
    }
    if opts.inputs.is_empty() {
        bail!("provide at least one ranking input");
    }

    // Method names first (cheap), duplicate stems are a hard error before
    // any ranking JSON is parsed — they'd silently merge in the mapping.
    let mut methods = Vec::with_capacity(opts.inputs.len());
    let mut seen_stems = BTreeSet::new();
    for input in &opts.inputs {
        let method = method_name(input)?;
        if !seen_stems.insert(method.clone()) {
            bail!(
                "duplicate method name `{method}` derived from {} (input file stems must be unique)",
                input.display()
            );
        }
        methods.push(method);
    }

    let summaries: Vec<(String, InterestingSummary)> = opts
        .inputs
        .iter()
        .zip(methods.iter())
        .map(|(input, method)| -> Result<(String, InterestingSummary)> {
            Ok((method.clone(), read_ranking(input)?))
        })
        .collect::<Result<_>>()?;

    // Pool = union of pattern_id over all inputs. First input that carries a
    // pattern wins its row content (kind/pattern/counts/region_examples).
    let mut ranks: BTreeMap<String, BTreeMap<String, usize>> = BTreeMap::new();
    let mut pattern_rows: BTreeMap<String, &InterestingRow> = BTreeMap::new();
    for (method, summary) in &summaries {
        for (index, row) in summary.rows.iter().enumerate() {
            ranks
                .entry(row.pattern_id.clone())
                .or_default()
                .insert(method.clone(), index + 1);
            pattern_rows.entry(row.pattern_id.clone()).or_insert(row);
        }
    }

    let pool_ids: Vec<String> = pattern_rows.keys().cloned().collect();
    let order = blind_order(pool_ids, opts.seed);
    let width = order.len().to_string().len().max(3);

    let mut aat_cache: BTreeMap<String, ab_plaintext::PlainTextDocument> = BTreeMap::new();
    let mut mapping: BTreeMap<String, MappingEntry> = BTreeMap::new();
    let mut label_rows = Vec::with_capacity(order.len());

    for (index, pattern_id) in order.iter().enumerate() {
        let label_id = format!("L{:0width$}", index + 1, width = width);
        let row = pattern_rows[pattern_id];

        let mut snippets = Vec::with_capacity(opts.snippets_per_pattern);
        for example in row.region_examples.iter().take(opts.snippets_per_pattern) {
            load_aat_document(&mut aat_cache, &opts.aat_dir, &example.source_id)?;
            let document = &aat_cache[&example.source_id];
            let text = snippet(
                &document.text,
                example.char_start,
                example.char_end,
                opts.context_chars,
            )
            .with_context(|| {
                format!(
                    "pattern `{pattern_id}`, source_id `{}`, region_index {}",
                    example.source_id, example.region_index
                )
            })?;
            snippets.push(text);
        }

        mapping.insert(
            label_id.clone(),
            MappingEntry {
                pattern_id: pattern_id.clone(),
                ranks: ranks.get(pattern_id).cloned().unwrap_or_default(),
            },
        );
        label_rows.push(LabelRow {
            label_id,
            kind: row.kind.clone(),
            pattern: row.pattern.clone(),
            examples: row.examples,
            source_count: row.source_count,
            text_count: row.text_count,
            snippets,
        });
    }

    write_labels_tsv(&opts.output, opts.snippets_per_pattern, &label_rows)?;
    write_mapping_json(&opts.mapping_output, &mapping)?;

    Ok(ExportSummary {
        rows: label_rows.len(),
        methods,
    })
}

fn load_aat_document(
    cache: &mut BTreeMap<String, ab_plaintext::PlainTextDocument>,
    aat_dir: &Path,
    source_id: &str,
) -> Result<()> {
    if cache.contains_key(source_id) {
        return Ok(());
    }
    let path = aat_dir.join(format!("{source_id}.json"));
    let file = File::open(&path).with_context(|| format!("missing AAT file {}", path.display()))?;
    let value: serde_json::Value = serde_json::from_reader(BufReader::new(file))
        .with_context(|| format!("failed to parse {}", path.display()))?;
    let document = ab_plaintext::from_aat_value(&value)
        .with_context(|| format!("failed to project AAT visible text from {}", path.display()))?;
    cache.insert(source_id.to_owned(), document);
    Ok(())
}

fn write_labels_tsv(path: &Path, snippets_per_pattern: usize, rows: &[LabelRow]) -> Result<()> {
    crate::create_parent_dir(path)?;
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut out = BufWriter::new(file);

    writeln!(out, "{HEADER_COMMENT_INTRO}")?;
    for (verdict, description) in VERDICTS.iter().zip(VERDICT_DESCRIPTIONS.iter()) {
        writeln!(out, "#   {verdict} - {description}")?;
    }
    writeln!(out, "# `notes` is free text.")?;

    write!(
        out,
        "label_id\tkind\tpattern\texamples\tsource_count\ttext_count"
    )?;
    for n in 1..=snippets_per_pattern {
        write!(out, "\tsnippet_{n}")?;
    }
    writeln!(out, "\tverdict\tnotes")?;

    for row in rows {
        write!(
            out,
            "{}\t{}\t{}\t{}\t{}\t{}",
            row.label_id, row.kind, row.pattern, row.examples, row.source_count, row.text_count
        )?;
        for index in 0..snippets_per_pattern {
            let text = row.snippets.get(index).map(String::as_str).unwrap_or("");
            write!(out, "\t{text}")?;
        }
        writeln!(out, "\t\t")?;
    }
    out.flush()?;
    Ok(())
}

fn write_mapping_json(path: &Path, mapping: &BTreeMap<String, MappingEntry>) -> Result<()> {
    crate::create_parent_dir(path)?;
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut out = BufWriter::new(file);
    serde_json::to_writer_pretty(&mut out, mapping)?;
    writeln!(out)?;
    Ok(())
}

/// Fisher–Yates over `ids` pre-sorted ascending, keyed by `seed` — the same
/// shuffle primitive as `score_mode: "random"` (spec §Tie-Breaking).
fn blind_order(mut ids: Vec<String>, seed: u64) -> Vec<String> {
    ids.sort();
    let mut state = seed;
    for i in (1..ids.len()).rev() {
        let j = (splitmix64(&mut state) % (i as u64 + 1)) as usize;
        ids.swap(i, j);
    }
    ids
}

/// Slices `text` by **char** index (never byte index) into
/// `…before【span】after…`, bracketing `[char_start, char_end)` with up to
/// `context` chars on each side, and scrubbing `\t`/`\n`/`\r` to `␣` (TSV
/// safety). Errors when the span is inverted or exceeds the text's char
/// count; the caller (which has `source_id`/`region_index`) should attach
/// that context via [`anyhow::Context`].
fn snippet(text: &str, char_start: u64, char_end: u64, context: usize) -> Result<String> {
    let chars: Vec<char> = text.chars().collect();
    let total = chars.len() as u64;
    if char_start > char_end || char_end > total {
        bail!("span [{char_start}, {char_end}) is out of range for a text of {total} chars");
    }
    let start = char_start as usize;
    let end = char_end as usize;
    let context_start = start.saturating_sub(context);
    let context_end = (end + context).min(chars.len());
    let before: String = chars[context_start..start].iter().collect();
    let span: String = chars[start..end].iter().collect();
    let after: String = chars[end..context_end].iter().collect();
    Ok(scrub(&format!("…{before}【{span}】{after}…")))
}

fn scrub(text: &str) -> String {
    text.chars()
        .map(|ch| {
            if matches!(ch, '\t' | '\n' | '\r') {
                '␣'
            } else {
                ch
            }
        })
        .collect()
}

/// Method name for a ranking input = its file stem (`left.json` → `left`).
fn method_name(path: &Path) -> Result<String> {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .map(str::to_owned)
        .with_context(|| format!("cannot derive a method name from {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blind_order_is_a_complete_permutation_of_the_sorted_pool() {
        let ids = vec!["p3".to_owned(), "p1".to_owned(), "p2".to_owned()];
        let order = blind_order(ids.clone(), 20260706);
        let mut sorted_order = order.clone();
        sorted_order.sort();
        let mut sorted_ids = ids;
        sorted_ids.sort();
        assert_eq!(sorted_order, sorted_ids);
    }

    #[test]
    fn blind_order_is_seed_deterministic_and_seed_sensitive() {
        let ids = vec![
            "p1".to_owned(),
            "p2".to_owned(),
            "p3".to_owned(),
            "p4".to_owned(),
            "p5".to_owned(),
        ];
        let a = blind_order(ids.clone(), 42);
        let b = blind_order(ids.clone(), 42);
        let c = blind_order(ids.clone(), 43);
        assert_eq!(a, b);
        assert_ne!(a, c);
    }

    #[test]
    fn snippet_slices_by_char_index_not_byte_index() {
        // "abc吾輩は猫であるxyz": chars 0..13, kanji/kana each multi-byte so
        // byte-slicing at these char offsets would split a UTF-8 sequence.
        let text = "abc吾輩は猫であるxyz";
        let result = snippet(text, 6, 10, 2).unwrap();
        assert_eq!(result, "…輩は【猫である】xy…");
    }

    #[test]
    fn snippet_scrubs_tabs_and_newlines() {
        let text = "a\tb\nc猫d";
        // span covers the whole string; context 0 keeps before/after empty.
        let result = snippet(text, 0, 7, 0).unwrap();
        assert!(!result.contains('\t'));
        assert!(!result.contains('\n'));
        assert!(result.contains('␣'));
    }

    #[test]
    fn snippet_rejects_out_of_range_span() {
        let err = snippet("abc", 0, 10, 0).unwrap_err();
        assert!(err.to_string().contains("out of range"));
    }

    #[test]
    fn method_name_uses_file_stem() {
        assert_eq!(
            method_name(std::path::Path::new("scratch/rankings/left.json")).unwrap(),
            "left"
        );
    }

    // chars: a b c 吾 輩 は 猫 で あ る x y z P Q R  (16 chars, indices 0..16)
    const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"abc吾輩は猫であるxyzPQR"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;

    fn region_example(source_id: &str, char_start: u64, char_end: u64) -> serde_json::Value {
        serde_json::json!({
            "source_id": source_id,
            "text_id": "source-a",
            "region_index": 0,
            "char_start": char_start,
            "char_end": char_end,
        })
    }

    fn fixture_row(pattern_id: &str, region_examples: Vec<serde_json::Value>) -> serde_json::Value {
        serde_json::json!({
            "pattern_id": pattern_id,
            "pattern": format!("pattern-{pattern_id}"),
            "kind": "feature",
            "rrf_score": 0.0,
            "signal_profile": [],
            "signals": [],
            "examples": 1,
            "source_count": 1,
            "text_count": 1,
            "sample_source_ids": ["source-a"],
            "sample_text_ids": ["source-a"],
            "region_examples": region_examples,
        })
    }

    fn fixture_summary(run_id: &str, rows: Vec<serde_json::Value>) -> serde_json::Value {
        serde_json::json!({
            "score_version": {
                "score_version": 1,
                "pattern_id_version": 1,
                "rrf_k": 60,
                "lambda_missing_policy": "rank-floor",
                "rank_scope": "within-kind",
                "score_mode": "rrf",
                "sample_seed": null,
                "anomaly_w_cov": 5.0,
                "signal_profile": ["coverage", "rarity", "impact", "span"],
                "feature_profile": "core",
                "rarity_basis": "source",
                "granularity_profile": "suw",
                "cause_classification_profile": "absent",
                "literal_context_policy": null,
                "surprise": "absent",
            },
            "run_id": run_id,
            "rows": rows,
            "anomalies": [],
        })
    }

    /// Writes the standard 2-ranking + 1-AAT fixture (shared pattern
    /// `p-shared` ranked in both, plus `p-left`/`p-right` unique to one
    /// side) into a fresh tempdir. Returns (dir, left.json, right.json,
    /// aat_dir).
    fn write_fixture() -> (tempfile::TempDir, PathBuf, PathBuf, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let aat_dir = dir.path().join("aat");
        std::fs::create_dir_all(&aat_dir).unwrap();
        std::fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        // "abc吾輩は猫であるxyzPQR": "abc"=[0,3) "猫である"=[6,10) "xyz"=[10,13) "PQR"=[13,16)
        let left = fixture_summary(
            "run-left",
            vec![
                fixture_row("p-shared", vec![region_example("source-a", 6, 10)]),
                fixture_row("p-left", vec![region_example("source-a", 0, 3)]),
            ],
        );
        // Right's region example for the shared pattern differs
        // from left's (and from every other pattern's span), so the "first
        // input wins" contract is falsifiable.
        let right = fixture_summary(
            "run-right",
            vec![
                fixture_row("p-right", vec![region_example("source-a", 13, 16)]),
                fixture_row("p-shared", vec![region_example("source-a", 10, 13)]),
            ],
        );

        let left_path = dir.path().join("left.json");
        let right_path = dir.path().join("right.json");
        std::fs::write(&left_path, serde_json::to_vec(&left).unwrap()).unwrap();
        std::fs::write(&right_path, serde_json::to_vec(&right).unwrap()).unwrap();

        (dir, left_path, right_path, aat_dir)
    }

    fn base_options(
        inputs: Vec<PathBuf>,
        aat_dir: PathBuf,
        output: PathBuf,
        mapping_output: PathBuf,
    ) -> ExportLabelsOptions {
        ExportLabelsOptions {
            inputs,
            aat_dir,
            output,
            mapping_output,
            seed: 20260706,
            snippets_per_pattern: 3,
            context_chars: 2,
            force: false,
        }
    }

    #[test]
    fn export_labels_end_to_end() {
        let (dir, left, right, aat_dir) = write_fixture();
        let output = dir.path().join("labels.tsv");
        let mapping_output = dir.path().join("mapping.json");

        let summary = run_export_labels(&base_options(
            vec![left.clone(), right.clone()],
            aat_dir.clone(),
            output.clone(),
            mapping_output.clone(),
        ))
        .unwrap();

        assert_eq!(summary.rows, 3);
        assert_eq!(summary.methods, vec!["left".to_owned(), "right".to_owned()]);

        let tsv = std::fs::read_to_string(&output).unwrap();
        let lines: Vec<&str> = tsv.lines().collect();
        // Header comment block documents all six verdicts verbatim.
        for verdict in VERDICTS {
            assert!(
                lines
                    .iter()
                    .any(|line| line.starts_with('#') && line.contains(verdict)),
                "missing verdict `{verdict}` in header block"
            );
        }
        let header_row = lines
            .iter()
            .find(|line| line.starts_with("label_id"))
            .expect("column header row");
        assert_eq!(
            *header_row,
            "label_id\tkind\tpattern\texamples\tsource_count\ttext_count\tsnippet_1\tsnippet_2\tsnippet_3\tverdict\tnotes"
        );
        assert!(!header_row.contains("method"));
        assert!(!header_row.contains("rank"));
        assert!(!header_row.contains("score"));
        assert!(!header_row.contains("pattern_id"));

        let data_rows: Vec<&str> = lines
            .iter()
            .filter(|line| !line.starts_with('#') && !line.starts_with("label_id"))
            .copied()
            .collect();
        assert_eq!(data_rows.len(), 3);
        // Verdict/notes columns are the two trailing empty fields.
        for row in &data_rows {
            assert!(row.ends_with("\t\t"));
        }
        // The p-shared row must carry left's snippet ("猫である", chars
        // [6,10)), proving "first input wins" even though right also
        // carries the pattern with a different span.
        let shared_snippet = "…輩は【猫である】xy…";
        assert!(
            tsv.contains(shared_snippet),
            "expected left's snippet {shared_snippet:?} to win for p-shared, got:\n{tsv}"
        );
        // Right's own version of the span for p-shared must NOT appear.
        assert!(!tsv.contains("…ある【xyz】PQ…"));

        let mapping: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&mapping_output).unwrap()).unwrap();
        let mapping = mapping.as_object().unwrap();
        assert_eq!(mapping.len(), 3);
        let mut by_pattern: BTreeMap<String, &serde_json::Value> = BTreeMap::new();
        for entry in mapping.values() {
            let pattern_id = entry["pattern_id"].as_str().unwrap().to_owned();
            by_pattern.insert(pattern_id, entry);
        }
        assert_eq!(by_pattern["p-shared"]["ranks"]["left"], 1);
        assert_eq!(by_pattern["p-shared"]["ranks"]["right"], 2);
        assert_eq!(by_pattern["p-left"]["ranks"]["left"], 2);
        assert!(by_pattern["p-left"]["ranks"].get("right").is_none());
        assert_eq!(by_pattern["p-right"]["ranks"]["right"], 1);
        assert!(by_pattern["p-right"]["ranks"].get("left").is_none());

        // label_id keys are L001, L002, L003 in some shuffled assignment.
        let mut ids: Vec<&String> = mapping.keys().collect();
        ids.sort();
        assert_eq!(ids, vec!["L001", "L002", "L003"]);
    }

    #[test]
    fn export_labels_is_seed_deterministic_byte_identical() {
        let (dir, left, right, aat_dir) = write_fixture();
        let output_a = dir.path().join("a.tsv");
        let mapping_a = dir.path().join("a.json");
        let output_b = dir.path().join("b.tsv");
        let mapping_b = dir.path().join("b.json");

        run_export_labels(&base_options(
            vec![left.clone(), right.clone()],
            aat_dir.clone(),
            output_a.clone(),
            mapping_a.clone(),
        ))
        .unwrap();
        run_export_labels(&base_options(
            vec![left, right],
            aat_dir,
            output_b.clone(),
            mapping_b.clone(),
        ))
        .unwrap();

        assert_eq!(
            std::fs::read(&output_a).unwrap(),
            std::fs::read(&output_b).unwrap()
        );
        assert_eq!(
            std::fs::read(&mapping_a).unwrap(),
            std::fs::read(&mapping_b).unwrap()
        );
    }

    #[test]
    fn export_labels_rejects_duplicate_method_stems() {
        let (dir, left, _right, aat_dir) = write_fixture();
        let duplicate_dir = dir.path().join("other");
        std::fs::create_dir_all(&duplicate_dir).unwrap();
        let duplicate = duplicate_dir.join("left.json");
        std::fs::copy(&left, &duplicate).unwrap();

        let err = run_export_labels(&base_options(
            vec![left, duplicate],
            aat_dir,
            dir.path().join("out.tsv"),
            dir.path().join("mapping.json"),
        ))
        .unwrap_err();
        assert!(err.to_string().contains("duplicate method name"));
        assert!(err.to_string().contains("left"));
    }

    #[test]
    fn export_labels_reports_out_of_range_span_with_source_and_region() {
        let dir = tempfile::tempdir().unwrap();
        let aat_dir = dir.path().join("aat");
        std::fs::create_dir_all(&aat_dir).unwrap();
        std::fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let bad = fixture_summary(
            "run-bad",
            vec![fixture_row(
                "p-bad",
                vec![region_example("source-a", 0, 999)],
            )],
        );
        let bad_path = dir.path().join("bad.json");
        std::fs::write(&bad_path, serde_json::to_vec(&bad).unwrap()).unwrap();

        let err = run_export_labels(&base_options(
            vec![bad_path],
            aat_dir,
            dir.path().join("out.tsv"),
            dir.path().join("mapping.json"),
        ))
        .unwrap_err();
        let message = format!("{err:#}");
        assert!(message.contains("source-a"));
        assert!(message.contains("region_index"));
        assert!(message.contains("out of range"));
    }

    #[test]
    fn export_labels_reports_missing_aat_file() {
        let dir = tempfile::tempdir().unwrap();
        let aat_dir = dir.path().join("aat");
        std::fs::create_dir_all(&aat_dir).unwrap();
        // No source-a.json written.

        let summary = fixture_summary(
            "run-missing",
            vec![fixture_row(
                "p-missing",
                vec![region_example("source-a", 0, 1)],
            )],
        );
        let path = dir.path().join("missing.json");
        std::fs::write(&path, serde_json::to_vec(&summary).unwrap()).unwrap();

        let err = run_export_labels(&base_options(
            vec![path],
            aat_dir.clone(),
            dir.path().join("out.tsv"),
            dir.path().join("mapping.json"),
        ))
        .unwrap_err();
        assert!(
            err.to_string()
                .contains(&aat_dir.join("source-a.json").display().to_string())
        );
    }

    #[test]
    fn export_labels_refuses_to_overwrite_without_force() {
        let (dir, left, right, aat_dir) = write_fixture();
        let output = dir.path().join("labels.tsv");
        let mapping_output = dir.path().join("mapping.json");

        run_export_labels(&base_options(
            vec![left.clone(), right.clone()],
            aat_dir.clone(),
            output.clone(),
            mapping_output.clone(),
        ))
        .unwrap();

        let err = run_export_labels(&base_options(
            vec![left.clone(), right.clone()],
            aat_dir.clone(),
            output.clone(),
            mapping_output.clone(),
        ))
        .unwrap_err();
        assert!(err.to_string().contains("--force"));

        let mut opts = base_options(vec![left, right], aat_dir, output, mapping_output);
        opts.force = true;
        run_export_labels(&opts).unwrap();
    }

    #[test]
    fn method_name_rejects_path_without_stem() {
        let err = method_name(std::path::Path::new("..")).unwrap_err();
        assert!(err.to_string().contains("cannot derive a method name"));
    }
}
