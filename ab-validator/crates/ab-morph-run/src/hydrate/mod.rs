//! Hydrates a `summarize-warehouse-interesting` ranking artifact into a
//! self-contained example bundle (examples.md + examples.json). Pure
//! post-processor: reads the artifact, the warehouse run dir, the AAT files
//! referenced by `sources.aat_path`, and (optionally) an ABC catalog export;
//! never modifies any input. Spec:
//! docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use serde::Serialize;

use crate::summary::{AnomalyRow, InterestingRow, InterestingSummary, ScoreVersionBlock};

pub mod analyses;
pub mod metadata;
pub mod source_context;
pub mod tables;

use analyses::{AnalyzerAnalysis, group_analyses, short_analyzer_ids};
use metadata::{WorkMeta, resolve_work_meta};
use source_context::{AatNodeRef, AozoraMarkup, Snippet, SourceContext, SourceLoadError};
use tables::{
    RegionAnalyzerRow, Token, WorkRow, read_region_analyzers_for, read_sources_for,
    read_tokens_for, read_works_for,
};

/// Options for [`run_hydrate_interesting`].
#[derive(Debug, Clone)]
pub struct HydrateOptions {
    /// Ranking artifact (JSON format from `summarize-warehouse-interesting`).
    pub interesting: PathBuf,
    /// Warehouse run directory (sources/morphemes/… parquet).
    pub run_dir: PathBuf,
    /// Output directory; receives `examples.md` and `examples.json`.
    pub output_dir: PathBuf,
    /// ABC catalog export with `works/` and `persons/` (author names).
    pub abc_catalog: Option<PathBuf>,
    /// Chars of context on each side of the region (default 40).
    pub context_chars: usize,
    /// Hydrate only the first N ranked rows (anomalies are always hydrated).
    pub limit: Option<usize>,
    /// Overwrite existing outputs.
    pub force: bool,
    /// Provenance timestamp, filled by the caller (CLI: now; tests: constant).
    pub built_at_utc: String,
}

/// One hydrated region example: every independently-degradable layer plus
/// the errors accumulated while resolving them.
#[derive(Debug, Clone, Serialize)]
pub struct HydratedExample {
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub snippet: Option<Snippet>,
    pub analyzer_analyses: Vec<AnalyzerAnalysis>,
    pub aozora_markup: Option<AozoraMarkup>,
    pub aat_nodes: Vec<AatNodeRef>,
    pub work: Option<WorkMeta>,
    pub errors: Vec<String>,
}

/// A ranked pattern row with its `region_examples` hydrated.
#[derive(Debug, Clone, Serialize)]
pub struct HydratedRow {
    #[serde(flatten)]
    pub row: InterestingRow,
    pub hydrated_examples: Vec<HydratedExample>,
}

/// An anomaly row hydrated (anomalies carry exactly one example each).
#[derive(Debug, Clone, Serialize)]
pub struct HydratedAnomaly {
    #[serde(flatten)]
    pub row: AnomalyRow,
    pub hydrated: HydratedExample,
}

/// Bundle-level provenance: what produced this bundle and from what inputs.
#[derive(Debug, Clone, Serialize)]
pub struct Provenance {
    pub run_id: String,
    pub interesting_path: String,
    pub interesting_sha256: String,
    pub abc_catalog: Option<String>,
    pub context_chars: usize,
    pub limit: Option<usize>,
    pub built_at_utc: String,
    /// Short analyzer id → full analyzer id (inverse of
    /// [`short_analyzer_ids`]; collisions leave short == full, so the
    /// inversion never loses information).
    pub analyzer_legend: BTreeMap<String, String>,
}

/// The full hydrated bundle written to `examples.json`.
#[derive(Debug, Clone, Serialize)]
pub struct HydratedBundle {
    pub provenance: Provenance,
    pub score_version: ScoreVersionBlock,
    pub rows: Vec<HydratedRow>,
    pub anomalies: Vec<HydratedAnomaly>,
}

/// Per-run tallies of hydration quality, surfaced on the CLI status line.
#[derive(Debug, Clone, Default, Serialize)]
pub struct HydrateRunSummary {
    pub examples_full: usize,
    pub examples_partial: usize,
    pub examples_failed: usize,
    pub error_counts: BTreeMap<String, usize>,
}

/// Read-only context shared by every [`build_example`] call in one run.
struct BuildCtx<'a> {
    context_chars: usize,
    region_analyzers: &'a BTreeMap<(String, u64), Vec<RegionAnalyzerRow>>,
    tokens: &'a BTreeMap<(String, String, u64), Token>,
    works: &'a Option<BTreeMap<String, WorkRow>>,
    abc_catalog: Option<&'a Path>,
}

/// Hydrates a `summarize-warehouse-interesting` ranking artifact (`opts.interesting`)
/// against a warehouse run (`opts.run_dir`) into a self-contained `examples.json`
/// bundle under `opts.output_dir`. Pure post-processor: never modifies any input.
/// Per-example degradation (a missing AAT, a projection mismatch, an unresolved
/// work record, …) is recorded in that example's `errors` and never fails the run;
/// only structural failures (unreadable ranking JSON, an unwritable output
/// directory, …) return `Err`.
///
/// # Errors
///
/// Returns an error when `examples.json`/`examples.md` already exist and
/// `opts.force` is false, when `opts.output_dir` cannot be created, when
/// `opts.interesting` cannot be read or parsed, when the warehouse parquet
/// tables cannot be read, or when `examples.json` cannot be written.
pub fn run_hydrate_interesting(opts: &HydrateOptions) -> Result<HydrateRunSummary> {
    // Step 1: refuse to clobber existing outputs unless forced.
    let examples_json_path = opts.output_dir.join("examples.json");
    let examples_md_path = opts.output_dir.join("examples.md");
    if !opts.force && (examples_json_path.exists() || examples_md_path.exists()) {
        bail!(
            "refusing to overwrite existing hydrate output in {} (pass --force to allow)",
            opts.output_dir.display()
        );
    }
    fs::create_dir_all(&opts.output_dir)
        .with_context(|| format!("failed to create {}", opts.output_dir.display()))?;

    // Step 2: read + hash the ranking artifact, then parse it.
    let bytes = fs::read(&opts.interesting)
        .with_context(|| format!("failed to read {}", opts.interesting.display()))?;
    let interesting_sha256 = ab_encoding::hex_sha256(&bytes);
    let ranking: InterestingSummary = serde_json::from_slice(&bytes).with_context(|| {
        format!(
            "failed to parse {} as an InterestingSummary",
            opts.interesting.display()
        )
    })?;

    // Step 3: apply `limit`, then collect every example key referenced by
    // the (possibly truncated) rows and by the always-hydrated anomalies.
    let InterestingSummary {
        score_version,
        run_id,
        mut rows,
        anomalies,
    } = ranking;
    if let Some(limit) = opts.limit {
        rows.truncate(limit);
    }

    let mut source_ids: BTreeSet<String> = BTreeSet::new();
    let mut region_keys: BTreeSet<(String, u64)> = BTreeSet::new();
    for row in &rows {
        for example in &row.region_examples {
            source_ids.insert(example.source_id.clone());
            region_keys.insert((example.source_id.clone(), example.region_index));
        }
    }
    for anomaly in &anomalies {
        source_ids.insert(anomaly.source_id.clone());
        region_keys.insert((anomaly.source_id.clone(), anomaly.region_index));
    }

    // Step 4: batch-read every warehouse table exactly once, filtered to
    // the wanted keys.
    let sources = read_sources_for(&opts.run_dir, &source_ids)?;
    let region_analyzers = read_region_analyzers_for(&opts.run_dir, &region_keys)?;
    let mut ranges: BTreeMap<(String, String), Vec<(u64, u64)>> = BTreeMap::new();
    let mut all_analyzer_ids: BTreeSet<String> = BTreeSet::new();
    for ((source_id, _region_index), region_rows) in &region_analyzers {
        for region_row in region_rows {
            all_analyzer_ids.insert(region_row.analyzer_id.clone());
            ranges
                .entry((source_id.clone(), region_row.analyzer_id.clone()))
                .or_default()
                .push((region_row.morpheme_start, region_row.morpheme_end));
        }
    }
    let tokens = read_tokens_for(&opts.run_dir, &ranges)?;
    let works = read_works_for(&opts.run_dir, &source_ids)?;

    // Step 6: analyzer legend, inverted (short → full) for provenance.
    let analyzer_legend: BTreeMap<String, String> = short_analyzer_ids(&all_analyzer_ids)
        .into_iter()
        .map(|(full, short)| (short, full))
        .collect();

    // Step 5: load each source's AAT exactly once, then hydrate every
    // example against its source's context.
    let mut source_contexts: BTreeMap<String, std::result::Result<SourceContext, SourceLoadError>> =
        BTreeMap::new();
    for source_id in &source_ids {
        let context = match sources.get(source_id) {
            Some(info) => SourceContext::load(&info.aat_path, info.source_chars),
            None => Err(SourceLoadError {
                code: "aat-missing",
                detail: format!("source_id {source_id} has no row in sources.parquet"),
            }),
        };
        source_contexts.insert(source_id.clone(), context);
    }

    let build_ctx = BuildCtx {
        context_chars: opts.context_chars,
        region_analyzers: &region_analyzers,
        tokens: &tokens,
        works: &works,
        abc_catalog: opts.abc_catalog.as_deref(),
    };
    let mut work_meta_cache: BTreeMap<String, (Option<WorkMeta>, Vec<String>)> = BTreeMap::new();
    let mut run_summary = HydrateRunSummary::default();

    let mut hydrated_rows = Vec::with_capacity(rows.len());
    for row in rows {
        let mut hydrated_examples = Vec::with_capacity(row.region_examples.len());
        for example in &row.region_examples {
            let context_result = &source_contexts[&example.source_id];
            let hydrated = build_example(
                &example.source_id,
                &example.text_id,
                example.region_index,
                example.char_start,
                example.char_end,
                context_result,
                &build_ctx,
                &mut work_meta_cache,
            );
            tally(&mut run_summary, &hydrated);
            hydrated_examples.push(hydrated);
        }
        hydrated_rows.push(HydratedRow {
            row,
            hydrated_examples,
        });
    }

    let mut hydrated_anomalies = Vec::with_capacity(anomalies.len());
    for anomaly in anomalies {
        let context_result = &source_contexts[&anomaly.source_id];
        let hydrated = build_example(
            &anomaly.source_id,
            &anomaly.text_id,
            anomaly.region_index,
            anomaly.char_start,
            anomaly.char_end,
            context_result,
            &build_ctx,
            &mut work_meta_cache,
        );
        tally(&mut run_summary, &hydrated);
        hydrated_anomalies.push(HydratedAnomaly {
            row: anomaly,
            hydrated,
        });
    }

    let bundle = HydratedBundle {
        provenance: Provenance {
            run_id,
            interesting_path: opts.interesting.display().to_string(),
            interesting_sha256,
            abc_catalog: opts
                .abc_catalog
                .as_ref()
                .map(|path| path.display().to_string()),
            context_chars: opts.context_chars,
            limit: opts.limit,
            built_at_utc: opts.built_at_utc.clone(),
            analyzer_legend,
        },
        score_version,
        rows: hydrated_rows,
        anomalies: hydrated_anomalies,
    };

    // Step 7: write examples.json, matching the `SummarizeWarehouseInteresting`
    // handler's JSON convention (main.rs: `to_writer_pretty` + trailing newline).
    let file = File::create(&examples_json_path)
        .with_context(|| format!("failed to create {}", examples_json_path.display()))?;
    let mut writer = std::io::BufWriter::new(file);
    serde_json::to_writer_pretty(&mut writer, &bundle)
        .with_context(|| format!("failed to write {}", examples_json_path.display()))?;
    use std::io::Write as _;
    writeln!(writer)
        .with_context(|| format!("failed to write {}", examples_json_path.display()))?;

    Ok(run_summary)
}

/// Resolves every layer of one example: snippet/markup/AAT-node context
/// (from the example's source context, loaded once per `source_id`),
/// analyzer analyses (from the region's fetched analyzer rows), and work
/// metadata (memoized per `work_id`). Every layer degrades independently;
/// failures are appended to `errors` rather than propagated.
#[allow(clippy::too_many_arguments)]
fn build_example(
    source_id: &str,
    text_id: &str,
    region_index: u64,
    char_start: u64,
    char_end: u64,
    context_result: &std::result::Result<SourceContext, SourceLoadError>,
    ctx: &BuildCtx<'_>,
    work_meta_cache: &mut BTreeMap<String, (Option<WorkMeta>, Vec<String>)>,
) -> HydratedExample {
    let mut errors = Vec::new();

    let (snippet, aozora_markup, aat_nodes) = match context_result {
        Ok(source_context) => {
            let layers = source_context.hydrate_region(char_start, char_end, ctx.context_chars);
            errors.extend(layers.errors);
            (layers.snippet, layers.aozora_markup, layers.aat_nodes)
        }
        Err(load_error) => {
            errors.push(format!("{}: {}", load_error.code, load_error.detail));
            (None, None, Vec::new())
        }
    };

    let analyzer_analyses = ctx
        .region_analyzers
        .get(&(source_id.to_owned(), region_index))
        .map(|region_rows| group_analyses(region_rows, ctx.tokens, source_id))
        .unwrap_or_default();

    let work_row = ctx.works.as_ref().and_then(|works| works.get(source_id));
    let (work, meta_errors) = match work_row {
        Some(row) => work_meta_cache
            .entry(row.work_id.clone())
            .or_insert_with(|| resolve_work_meta(Some(row), ctx.abc_catalog))
            .clone(),
        None => resolve_work_meta(None, ctx.abc_catalog),
    };
    errors.extend(meta_errors);

    HydratedExample {
        source_id: source_id.to_owned(),
        text_id: text_id.to_owned(),
        region_index,
        char_start,
        char_end,
        snippet,
        analyzer_analyses,
        aozora_markup,
        aat_nodes,
        work,
        errors,
    }
}

/// Step 8: tallies one example into the run summary. `full` requires no
/// errors at all; `failed` means every layer came back empty; anything else
/// is `partial`. `error_counts` is keyed by the code prefix of each error
/// string (the text before its first `:`, or the whole string).
fn tally(summary: &mut HydrateRunSummary, example: &HydratedExample) {
    if example.errors.is_empty() {
        summary.examples_full += 1;
    } else if example.snippet.is_none()
        && example.aozora_markup.is_none()
        && example.analyzer_analyses.is_empty()
    {
        summary.examples_failed += 1;
    } else {
        summary.examples_partial += 1;
    }
    for error in &example.errors {
        let code = error.split(':').next().unwrap_or(error).to_owned();
        *summary.error_counts.entry(code).or_insert(0) += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    use arrow_array::{Int32Array, RecordBatch, StringArray};
    use arrow_schema::{DataType, Field, Schema};
    use parquet::arrow::ArrowWriter;

    /// Copied from Task 5's `metadata::tests::write_catalog` — one work
    /// record (000080, 煙管) plus its author person record (000879, 芥川竜之介).
    fn write_catalog_fixture(dir: &Path) {
        std::fs::create_dir_all(dir.join("works")).unwrap();
        std::fs::create_dir_all(dir.join("persons")).unwrap();
        std::fs::write(
            dir.join("works/000080.json"),
            serde_json::json!({
                "work": {"work_id": "000080", "title": "煙管", "first_published": "1916",
                         "ndc": "NDC 913", "card_url": "https://www.aozora.gr.jp/cards/000879/card80.html",
                         "orthographic_style": "新字新仮名"},
                "contributors": [{"person_id": "000879", "relation_to_work": "著者"}]
            })
            .to_string(),
        )
        .unwrap();
        std::fs::write(
            dir.join("persons/000879.json"),
            serde_json::json!({
                "person_id": "000879", "family_name": "芥川", "given_name": "竜之介"
            })
            .to_string(),
        )
        .unwrap();
    }

    /// Writes a single-row `aozora_works.parquet` sidecar mapping
    /// `src-a` → work `000080`, matching the columns `tables::read_works_for`
    /// expects (see its doc comment). `import_aozora::write_sidecar` is a
    /// private helper writing 9 columns via a run-specific accumulator; not
    /// reusable here, so this writes the batch directly with the subset of
    /// columns this fixture needs.
    fn write_aozora_works_parquet(run_dir: &Path) {
        let schema = Arc::new(Schema::new(vec![
            Field::new("work_id", DataType::Utf8, false),
            Field::new("source_id", DataType::Utf8, false),
            Field::new("title", DataType::Utf8, false),
            Field::new("author_person_id", DataType::Utf8, true),
            Field::new("publication_year", DataType::Int32, true),
            Field::new("orthographic_style", DataType::Utf8, true),
        ]));
        let batch = RecordBatch::try_new(
            schema.clone(),
            vec![
                Arc::new(StringArray::from(vec!["000080"])),
                Arc::new(StringArray::from(vec!["src-a"])),
                Arc::new(StringArray::from(vec!["煙管"])),
                Arc::new(StringArray::from(vec![Some("000879")])),
                Arc::new(Int32Array::from(vec![Some(1916)])),
                Arc::new(StringArray::from(vec![Some("新字新仮名")])),
            ],
        )
        .unwrap();
        let file = File::create(run_dir.join("aozora_works.parquet")).unwrap();
        let mut writer = ArrowWriter::try_new(file, schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    /// `calibration/label_export.rs`'s `region_example` (verbatim, see below)
    /// hardcodes `text_id: "source-a"` and `region_index: 0`; this variant
    /// takes both explicitly for the e2e fixture's region 2.
    fn region_example_at(
        source_id: &str,
        text_id: &str,
        region_index: u64,
        char_start: u64,
        char_end: u64,
    ) -> serde_json::Value {
        serde_json::json!({
            "source_id": source_id,
            "text_id": text_id,
            "region_index": region_index,
            "char_start": char_start,
            "char_end": char_end,
        })
    }

    // Copied verbatim from `calibration/label_export.rs` tests (~lines
    // 416-456): the plan intentionally duplicates these builders rather than
    // exporting test helpers across modules.
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

    /// One tempdir holding: warehouse run (Task 3's write_fixture, with
    /// SourceRow.aat_path pointing at src-a.json and source_chars matching
    /// the typed AAT fixture's 12 projected chars), the typed AAT file,
    /// a Task 5 ABC catalog, an aozora_works.parquet mapping src-a→000080,
    /// and a ranking JSON whose single row exemplifies chars [2,5) region 2.
    fn write_e2e_fixture() -> (tempfile::TempDir, HydrateOptions) {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = crate::hydrate::tables::tests::write_fixture(dir.path());
        std::fs::write(
            dir.path().join("src-a.json"),
            serde_json::to_vec(&crate::hydrate::source_context::tests::typed_aat_fixture())
                .unwrap(),
        )
        .unwrap();
        let catalog = dir.path().join("catalog");
        write_catalog_fixture(&catalog);
        write_aozora_works_parquet(&run_dir); // work_id 000080, source_id src-a, title 煙管, author 000879
        let interesting = dir.path().join("interesting.json");
        let ranking = fixture_summary(
            "run-h",
            vec![fixture_row(
                "p-ruby",
                vec![region_example_at("src-a", "txt-a", 2, 2, 5)],
            )],
        );
        std::fs::write(&interesting, serde_json::to_vec(&ranking).unwrap()).unwrap();
        let output_dir = dir.path().join("bundle");
        let opts = HydrateOptions {
            interesting,
            run_dir,
            output_dir,
            abc_catalog: Some(catalog),
            context_chars: 2,
            limit: None,
            force: false,
            built_at_utc: "2026-07-10T00:00:00Z".to_owned(),
        };
        (dir, opts)
    }

    #[test]
    fn hydrate_end_to_end_writes_json_bundle() {
        let (_dir, opts) = write_e2e_fixture();
        let summary = run_hydrate_interesting(&opts).unwrap();
        assert_eq!(summary.examples_failed, 0);
        let bundle: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(opts.output_dir.join("examples.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(bundle["provenance"]["run_id"], "run-h");
        assert!(
            bundle["provenance"]["interesting_sha256"]
                .as_str()
                .unwrap()
                .len()
                == 64
        );
        let example = &bundle["rows"][0]["hydrated_examples"][0];
        assert_eq!(example["snippet"]["region"], "仏蘭西");
        assert_eq!(example["aozora_markup"]["text"], "｜仏蘭西《フランス》");
        assert_eq!(example["work"]["title"], "煙管");
        assert!(example["errors"].as_array().unwrap().is_empty());
        // flattened InterestingRow fields present alongside hydration:
        assert!(bundle["rows"][0]["pattern_id"].is_string());
    }

    #[test]
    fn hydrate_is_deterministic_byte_identical() {
        let (_dir, opts) = write_e2e_fixture();
        let mut opts_a = opts.clone();
        opts_a.output_dir = opts.output_dir.with_file_name("bundle-a");
        let mut opts_b = opts.clone();
        opts_b.output_dir = opts.output_dir.with_file_name("bundle-b");
        run_hydrate_interesting(&opts_a).unwrap();
        run_hydrate_interesting(&opts_b).unwrap();
        for name in ["examples.json", "examples.md"] {
            // Guard until Task 8 adds examples.md: only compare files that
            // actually exist yet, so this test passes trivially for the
            // not-yet-written file.
            if !opts_a.output_dir.join(name).exists() {
                continue;
            }
            assert_eq!(
                std::fs::read(opts_a.output_dir.join(name)).unwrap(),
                std::fs::read(opts_b.output_dir.join(name)).unwrap(),
                "{name} differs between identical runs"
            );
        }
    }

    #[test]
    fn hydrate_refuses_overwrite_without_force() {
        let (_dir, opts) = write_e2e_fixture();
        run_hydrate_interesting(&opts).unwrap();
        let err = run_hydrate_interesting(&opts).unwrap_err();
        assert!(err.to_string().contains("--force"));
        let mut forced = opts.clone();
        forced.force = true;
        run_hydrate_interesting(&forced).unwrap();
    }
}
