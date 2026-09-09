mod auto_jobs;
mod calibration;
mod compact;
mod hydrate;
mod import_aozora;
mod nway;
mod options;
mod oracle;
mod orthographic_select;
mod output;
mod pipeline;
mod script;
mod select;
mod summary;
mod tokenize_plaintext;
mod warehouse;

use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use ab_morph_analyzers::{
    MorphAnalyzer, SudachiAnalyzer, SudachiMode, VaporettoAnalyzer, VibratoAnalyzer,
};
use ab_morph_diff::{
    Analysis, Comparison, MorphDiffError, NwayFeatureValueGroup, compare_pair,
    compare_pair_compact_with_source_text,
};
use ab_ortho_detect::OrthoDetector;
use ab_plaintext::{PlainTextDocument, from_aat_value, from_aat_value_with_spans};
use anyhow::{Context, Result, bail};
pub use options::{OrthoDetectMode, OutputProfile, WarehouseProfile};
use options::{
    PreparedOrthoDetector, RunNormalizationProvenance, SerialProgress, SerialRunOptions,
    WarehouseParallelOptions, WarehouseRunOptions,
};
use output::{open_output_writer, read_jsonl_or_zst_to_string};
use serde::Serialize;
use serde_json::Value;
use warehouse::schema::{
    ErrorRow as WarehouseErrorRow, FeaturePatternCountRow, NwayFeatureDiffRow, NwayRegionRow,
    RunAnalyzerRow, RunRow, WarehousePaths, WarehouseTable,
};
use warehouse::writer::{
    MorphemeFeaturesColumns, WarehouseWriter, parquet_table_row_count, stage_parquet_table_part,
};

const LARGE_INPUT_THRESHOLD_BYTES: u64 = 5 * 1024 * 1024;
const WAREHOUSE_MORPHEME_ROW_BATCH_SIZE: usize = 50_000;
const WAREHOUSE_REGULAR_BATCH_SIZE: usize = 32;

pub use calibration::{
    ExportLabelsOptions, ExportSummary, LabelScores, MethodScores, RankingComparison,
    run_compare_rankings, run_export_labels, run_score_labels,
};
pub use hydrate::{HydrateOptions, HydrateRunSummary, run_hydrate_interesting};
pub use import_aozora::{ImportSummary, run_import_aozora_metadata};
pub use nway::{NwayFeatureScopeRow, NwayFeatureValueGroupRow, NwaySegmentationGroupRow};
// Re-exported for the `oracle_adjudicate` criterion bench (benches/ compile as
// an external crate), not part of the CLI's stable surface.
pub use oracle::ruby::{RegionSpan, RubyBase, adjudicate};
pub use script::ScriptCategory;
pub use select::resolve_source_id_aat_paths;
pub(crate) use summary::WAREHOUSE_CORE_FEATURE_KEYS;
pub use summary::{
    AnomalyRow, CompactDifferenceKindFilter, CompactDifferenceSummaryOptions,
    CompactDifferenceSummaryRow, CompactExampleFilter, CompactExampleSummaryOptions,
    CompactExampleSummaryRow, CompactExampleSummarySort, CompactSummaryGroupBy,
    CompactSummaryOptions, CompactSummaryRow, CompactSummarySort, InterestingEngine,
    InterestingOutputFormat, InterestingRow, InterestingSummary, InterestingTextFilter,
    LambdaMissingPolicy, NwayPatternKind, NwayPatternOptions, NwayPatternRow, NwaySummaryOptions,
    NwaySummaryRow, NwaySummarySort, RankScope, RegionExampleOut, ScoreMode, ScoreVersionBlock,
    SignalExplain, SummaryExclusions, WarehouseErrorGroupBy, WarehouseErrorSummaryOptions,
    WarehouseErrorSummaryRow, WarehouseFeatureDiffExampleRow, WarehouseFeatureProfile,
    WarehouseInterestingOptions, WarehousePairwiseSort, WarehousePairwiseSummaryOptions,
    WarehousePairwiseSummaryRow, WarehousePatternExampleOptions, WarehousePatternOptions,
    WarehouseRegionAnalyzerExampleRow, WarehouseRegionExampleRow, WarehouseRegionKind,
    WarehouseRegionOptions, WarehouseTextFilter, materialize_warehouse_core_feature_pattern_counts,
    summarize_compact_comparisons, summarize_compact_differences, summarize_compact_examples,
    summarize_nway, summarize_nway_pattern_counts, summarize_nway_patterns,
    summarize_warehouse_errors, summarize_warehouse_interesting, summarize_warehouse_nway,
    summarize_warehouse_nway_patterns, summarize_warehouse_pairwise,
    summarize_warehouse_pattern_examples, summarize_warehouse_regions, write_interesting_tsv,
    write_warehouse_nway_patterns_duckdb_tsv, write_warehouse_pattern_examples_duckdb_tsv,
    write_warehouse_regions_duckdb_tsv,
};
pub use tokenize_plaintext::{
    TOKENIZE_ERRORS_FILE, TokenizePlaintextSummary, run_tokenize_plaintext,
};

/// Run the selected analysis pipeline over AAT input(s).
///
/// # Errors
///
/// Returns an error when input selection is invalid, analyzers cannot be loaded,
/// job parameters are invalid, or IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
) -> Result<()> {
    pipeline::run_analyze_aat(
        aat,
        aat_dir,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
    )
}

/// Run the selected analysis pipeline with N-way outputs enabled.
///
/// # Errors
///
/// Returns an error when N-way output prerequisites are not met, input selection is
/// invalid, analyzers cannot be loaded, or IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_with_nway(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: Option<usize>,
    string_stats_output: Option<&Path>,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    pipeline::run_analyze_aat_with_nway(
        aat,
        aat_dir,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        nway_output,
        nway_pattern_counts_output,
        max_nway_examples_per_text,
        string_stats_output,
        ortho_detect,
        ortho_ml_model,
    )
}

/// Run the selected analysis pipeline and write warehouse outputs.
///
/// # Errors
///
/// Returns an error when input selection is invalid, analyzers cannot be loaded,
/// job parameters are invalid, or warehouse/IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_warehouse(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
    warehouse_profile: WarehouseProfile,
    zstd_level: i32,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
    eligible_source_ids: Option<&std::collections::BTreeSet<String>>,
) -> Result<()> {
    pipeline::run_analyze_aat_warehouse(
        aat,
        aat_dir,
        analyzer_ids,
        warehouse_dir,
        run_id,
        jobs,
        warehouse_profile,
        zstd_level,
        ortho_detect,
        ortho_ml_model,
        eligible_source_ids,
    )
}

/// Lane A (historical-kana): resolve the set of eligible `source_id`s for a run
/// from an `aozora_works.parquet` sidecar, keeping works whose
/// `orthographic_style` is in `styles`. When `styles` is empty, defaults to the
/// old-kana set (`新字旧仮名`, `旧字旧仮名`). Pass the result to
/// [`run_analyze_aat_warehouse`]'s `eligible_source_ids`.
///
/// # Errors
///
/// Returns an error if the sidecar cannot be read or lacks the required columns.
pub fn resolve_orthographic_eligibility(
    works_parquet: &Path,
    styles: &[String],
) -> Result<std::collections::BTreeSet<String>> {
    let allowed: std::collections::BTreeSet<String> = if styles.is_empty() {
        orthographic_select::old_kana_styles()
    } else {
        styles.iter().cloned().collect()
    };
    orthographic_select::eligible_source_ids(works_parquet, &allowed)
}

/// Run over an explicit list of AAT inputs.
///
/// # Errors
///
/// Returns an error when inputs are empty, selection is invalid, analyzer ids are
/// missing, or IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_selected(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    pipeline::run_analyze_aat_selected(
        inputs,
        input_mode,
        input_path,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        ortho_detect,
        ortho_ml_model,
    )
}

#[cfg(test)]
pub(crate) use pipeline::{
    WarehouseWorkQueue, filter_resume_inputs, merge_warehouse_shard_runs, partition_inputs,
    symlink_input_file,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum AnalyzerSpec {
    Vibrato(Option<String>),
    Vaporetto(Option<String>),
    Sudachi(SudachiMode),
    TestSingle,
    TestSplit,
}

impl AnalyzerSpec {
    fn parse(value: &str) -> Result<Self> {
        match value {
            "vibrato" => Ok(Self::Vibrato(None)),
            "vaporetto" => Ok(Self::Vaporetto(None)),
            _ => {
                if let Some(name) = value.strip_prefix("vibrato:") {
                    if name.is_empty() {
                        bail!("vibrato analyzer requires a dictionary name");
                    }
                    Ok(Self::Vibrato(Some(name.to_owned())))
                } else if let Some(name) = value.strip_prefix("vaporetto:") {
                    if name.is_empty() {
                        bail!("vaporetto analyzer requires a dictionary name");
                    }
                    Ok(Self::Vaporetto(Some(name.to_owned())))
                } else {
                    match value {
                        "sudachi-a" => Ok(Self::Sudachi(SudachiMode::A)),
                        "sudachi-b" => Ok(Self::Sudachi(SudachiMode::B)),
                        "sudachi-c" => Ok(Self::Sudachi(SudachiMode::C)),
                        "test:single" => Ok(Self::TestSingle),
                        "test:split" => Ok(Self::TestSplit),
                        other => bail!("unknown analyzer `{other}`"),
                    }
                }
            }
        }
    }

    fn arg(&self) -> String {
        match self {
            Self::Vibrato(None) => "vibrato".to_owned(),
            Self::Vibrato(Some(name)) => format!("vibrato:{name}"),
            Self::Vaporetto(None) => "vaporetto".to_owned(),
            Self::Vaporetto(Some(name)) => format!("vaporetto:{name}"),
            Self::Sudachi(SudachiMode::A) => "sudachi-a".to_owned(),
            Self::Sudachi(SudachiMode::B) => "sudachi-b".to_owned(),
            Self::Sudachi(SudachiMode::C) => "sudachi-c".to_owned(),
            Self::TestSingle => "test:single".to_owned(),
            Self::TestSplit => "test:split".to_owned(),
        }
    }

    fn family(&self) -> &'static str {
        match self {
            Self::Vibrato(_) => "vibrato",
            Self::Vaporetto(_) => "vaporetto",
            Self::Sudachi(_) => "sudachi",
            Self::TestSingle | Self::TestSplit => "test",
        }
    }

    fn canonical_analyzer_id(&self) -> String {
        match self {
            Self::Vibrato(None) => "vibrato:unidic-cwj-202512".to_owned(),
            Self::Vibrato(Some(name)) => format!("vibrato:{name}"),
            Self::Vaporetto(None) => "vaporetto:unidic-cwj-202512".to_owned(),
            Self::Vaporetto(Some(name)) => format!("vaporetto:{name}"),
            Self::Sudachi(SudachiMode::A) => "sudachi-a".to_owned(),
            Self::Sudachi(SudachiMode::B) => "sudachi-b".to_owned(),
            Self::Sudachi(SudachiMode::C) => "sudachi-c".to_owned(),
            Self::TestSingle => "test:single".to_owned(),
            Self::TestSplit => "test:split".to_owned(),
        }
    }
}

fn parse_analyzer_specs(values: &[String]) -> Result<Vec<AnalyzerSpec>> {
    let mut seen = BTreeSet::new();
    let mut specs = Vec::new();

    for value in values {
        let spec = AnalyzerSpec::parse(value)?;
        let canonical = spec.canonical_analyzer_id();
        if seen.insert(canonical) {
            specs.push(spec);
        }
    }

    Ok(specs)
}

fn discover_aat_inputs(aat: Option<&Path>, aat_dir: Option<&Path>) -> Result<Vec<PathBuf>> {
    match (aat, aat_dir) {
        (Some(path), None) => {
            if !path.is_file() {
                bail!("--aat must point to a regular file: {}", path.display());
            }
            Ok(vec![path.to_owned()])
        }
        (None, Some(dir)) => {
            if !dir.is_dir() {
                bail!(
                    "--aat-dir must point to an existing directory: {}",
                    dir.display()
                );
            }
            let mut paths = Vec::new();
            collect_aat_json_files(dir, &mut paths)?;
            paths.sort();
            if paths.is_empty() {
                bail!("no AAT JSON files found in {}", dir.display());
            }
            Ok(paths)
        }
        _ => bail!("provide exactly one of --aat or --aat-dir"),
    }
}

fn collect_aat_json_files(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let path = entry
            .with_context(|| format!("failed to read entry in {}", dir.display()))?
            .path();
        if path.is_dir() {
            collect_aat_json_files(&path, paths)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
            paths.push(path);
        }
    }
    Ok(())
}

fn load_analyzers(specs: &[AnalyzerSpec]) -> Result<Vec<Arc<LoadedAnalyzer>>> {
    let mut analyzers = Vec::new();
    let sudachi_dictionary = if specs
        .iter()
        .any(|spec| matches!(spec, AnalyzerSpec::Sudachi(_)))
    {
        let dict = std::env::var_os("AB_SUDACHI_DICT")
            .context("AB_SUDACHI_DICT is required for Sudachi analyzers")?;
        Some(SudachiAnalyzer::load_dictionary("sudachi", dict)?)
    } else {
        None
    };

    for spec in specs {
        match spec {
            AnalyzerSpec::Vibrato(None) => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vibrato(
                    VibratoAnalyzer::unidic_cwj_default()?,
                )));
            }
            AnalyzerSpec::Vibrato(Some(dictionary_name)) => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vibrato(
                    VibratoAnalyzer::from_dictionary_name(dictionary_name)?,
                )));
            }
            AnalyzerSpec::Vaporetto(None) => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vaporetto(Box::new(
                    VaporettoAnalyzer::unidic_cwj_default()?,
                ))));
            }
            AnalyzerSpec::Vaporetto(Some(dictionary_name)) => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vaporetto(Box::new(
                    VaporettoAnalyzer::from_dictionary_name(dictionary_name)?,
                ))));
            }
            AnalyzerSpec::Sudachi(mode) => {
                analyzers.push(Arc::new(LoadedAnalyzer::Sudachi(
                    SudachiAnalyzer::from_dictionary(
                        *mode,
                        Arc::clone(
                            sudachi_dictionary
                                .as_ref()
                                .expect("Sudachi dictionary loaded"),
                        ),
                    ),
                )));
            }
            AnalyzerSpec::TestSingle => {
                analyzers.push(Arc::new(LoadedAnalyzer::Test(TestAnalyzerKind::Single)));
            }
            AnalyzerSpec::TestSplit => {
                analyzers.push(Arc::new(LoadedAnalyzer::Test(TestAnalyzerKind::Split)));
            }
        }
    }

    Ok(analyzers)
}

fn warehouse_analyzer_rows(
    run_id: &str,
    specs: &[AnalyzerSpec],
    analyzers: &[Arc<LoadedAnalyzer>],
) -> Result<Vec<RunAnalyzerRow>> {
    if specs.len() != analyzers.len() {
        bail!(
            "internal error: analyzer spec count {} does not match loaded analyzer count {}",
            specs.len(),
            analyzers.len()
        );
    }
    Ok(specs
        .iter()
        .zip(analyzers)
        .map(|(spec, analyzer)| RunAnalyzerRow {
            run_id: run_id.to_owned(),
            analyzer_id: analyzer.analyzer_id().to_owned(),
            analyzer_arg: spec.arg(),
            analyzer_family: spec.family().to_owned(),
        })
        .collect())
}

/// Resolve the run-level normalization provenance persisted on the `runs` row.
///
/// The policy is uniform per run (one `--ortho-detect` mode applied to every
/// document before every analyzer). For `Ml` the model file is loaded once here
/// to bind its `model_hash` into the policy identity; the per-document detector
/// is still constructed downstream (see the P0 per-batch note).
///
/// # Errors
///
/// Returns an error when `--ortho-detect=ml` is requested without a model path
/// or the model file cannot be loaded, or when the detector id fails to
/// serialize.
/// The historical UniDic that backs the Lane B (M2) segmentation/reading oracle.
/// Its archive hash is bound into the `HistoricalRewriteV1` detector identity.
pub const M2_ORACLE_DICTIONARY: &str = "unidic-kindai-bungo-202512";

fn resolve_run_normalization(
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<&Path>,
) -> Result<RunNormalizationProvenance> {
    use ab_ortho_detect::{
        NormalizationPolicy, OrthoDetector, OrthoDetectorId, OrthoNormalization,
    };
    let (mode, detector_id, policy) = match ortho_detect {
        OrthoDetectMode::Off => ("off", None, NormalizationPolicy::identity()),
        OrthoDetectMode::Heuristic => {
            let id = OrthoDetectorId::HeuristicV1;
            // v1 kata→hira path applies only ScriptKatakanaToHiragana.
            let policy = NormalizationPolicy::ortho_normalize_v1(
                id.clone(),
                vec![OrthoNormalization::ScriptKatakanaToHiragana],
            );
            ("heuristic", Some(id), policy)
        }
        OrthoDetectMode::Ml => {
            let path = ortho_ml_model.ok_or_else(|| {
                anyhow::anyhow!("--ortho-ml-model is required for --ortho-detect=ml")
            })?;
            let model = ab_ortho_detect::ml::MlLogisticRegression::load(path).map_err(|e| {
                anyhow::anyhow!("failed to load ML model from {}: {e}", path.display())
            })?;
            let id = model.detector_id();
            let policy = NormalizationPolicy::ortho_normalize_v1(
                id.clone(),
                vec![OrthoNormalization::ScriptKatakanaToHiragana],
            );
            ("ml", Some(id), policy)
        }
        OrthoDetectMode::Historical => {
            // Lane B (M2): bind the kindai-bungo oracle archive + rule-set into
            // identity (I2-D17). The hash is read cheaply (no dictionary load);
            // the pipeline's detector recomputes the same value from the loaded
            // analyzer, so the runs-row policy and the applied detector agree.
            let dictionary_hash = ab_morph_analyzers::dictionary_archive_hash(M2_ORACLE_DICTIONARY)
                .with_context(|| {
                    format!(
                        "failed to hash M2 oracle dictionary {M2_ORACLE_DICTIONARY} for policy identity"
                    )
                })?;
            let id = OrthoDetectorId::HistoricalRewriteV1 {
                dictionary_hash,
                rules_hash: ab_ortho_detect::historical::rules_hash(),
            };
            let policy = NormalizationPolicy::ortho_normalize_v1(
                id.clone(),
                vec![OrthoNormalization::HistoricalToModern],
            );
            // I2-D17 coupling: a HistoricalToModern policy is only admissible
            // with a dictionary-backed detector. This is the guard the spec
            // requires.
            policy
                .validate()
                .context("M2 historical normalization policy failed validation")?;
            ("historical", Some(id), policy)
        }
    };
    let detector_id = detector_id
        .map(|id| serde_json::to_string(&id))
        .transpose()
        .context("failed to serialize ortho detector id")?;
    Ok(RunNormalizationProvenance {
        mode: mode.to_owned(),
        detector_id,
        policy_hash: policy.policy_hash(),
    })
}

/// File name of the run-normalization provenance sidecar (T1 transport, spec
/// ). ABC reads this JSON to learn the applied normalization policy
/// without depending on a parquet reader.
pub const RUN_NORMALIZATION_PROVENANCE_FILE: &str = "run-normalization-provenance.json";

/// Emit the run-level normalization provenance as a JSON sidecar in the final
/// run directory (T1). Mirrors the `runs` warehouse columns; the value ABC
/// reads is `input_normalization_policy_hash`.
///
/// # Errors
///
/// Returns an error if the JSON cannot be serialized or written.
fn write_run_normalization_provenance(
    final_dir: &Path,
    run_id: &str,
    normalization: &RunNormalizationProvenance,
) -> Result<()> {
    let value = serde_json::json!({
        "run_id": run_id,
        "ortho_detect_mode": normalization.mode,
        "input_normalization_detector_id": normalization.detector_id,
        "input_normalization_policy_hash": normalization.policy_hash,
    });
    let path = final_dir.join(RUN_NORMALIZATION_PROVENANCE_FILE);
    let text = serde_json::to_string_pretty(&value)
        .context("failed to serialize run-normalization provenance")?;
    std::fs::write(&path, text).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

fn append_warehouse_nway_fact_rows(
    writer: &mut WarehouseWriter,
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
    ruby_bases: &[crate::oracle::ruby::RubyBase],
) -> Result<()> {
    let want_oracle = analyses.len() >= 2
        && !ruby_bases.is_empty()
        && writer.writes_table(WarehouseTable::NwayRegionOracleEvidence);
    let mut region_lookup: Vec<crate::oracle::ruby::RegionSpan> = Vec::new();
    let mut feature_pattern_counts = WarehouseFeaturePatternAccumulator::default();
    warehouse::rows::visit_nway_fact_row_batches(
        run_id,
        source_id,
        source_text,
        analyses,
        10_000,
        &mut feature_pattern_counts,
        |batch| {
            if want_oracle {
                region_lookup.extend(batch.regions.iter().map(|r| {
                    crate::oracle::ruby::RegionSpan {
                        region_index: r.region_index,
                        char_start: r.char_start,
                        char_end: r.char_end,
                        is_disagreement: !r.is_agreement,
                    }
                }));
            }
            writer.append_nway_regions(&batch.regions)?;
            writer.append_nway_region_analyzers(&batch.region_analyzers)?;
            let feature_diff_columns = std::mem::take(&mut batch.feature_diffs);
            writer.append_nway_feature_diff_columns(feature_diff_columns)?;
            Ok(())
        },
    )?;
    writer.append_feature_pattern_counts(&feature_pattern_counts.into_rows())?;
    if want_oracle {
        let text_id = analyses
            .first()
            .map(|a| a.text_id.clone())
            .unwrap_or_default();
        let rows = crate::oracle::ruby::adjudicate(
            run_id,
            source_id,
            &text_id,
            ruby_bases,
            analyses,
            &region_lookup,
        );
        for chunk in rows.chunks(10_000) {
            writer.append_nway_region_oracle_evidence(chunk)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct WarehouseFeaturePatternKey {
    feature_key: String,
    is_nonempty_whitespace: bool,
    pattern: String,
}

// `pub` (beyond `pub(crate)`) solely so the
// `warehouse_feature_pattern_accumulator` criterion bench (benches/ compile as
// an external crate) can construct and drive it directly; not part of the
// CLI's stable surface.
#[derive(Debug, Default)]
pub struct WarehouseFeaturePatternAccumulator {
    patterns: BTreeMap<WarehouseFeaturePatternKey, WarehouseFeaturePatternEntry>,
}

#[derive(Debug, Default)]
struct WarehouseFeaturePatternEntry {
    examples: u64,
    source_ids: BTreeSet<String>,
    text_ids: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct WarehouseFeatureGroupKey {
    source_id: Arc<str>,
    text_id: Arc<str>,
    region_index: u64,
    feature_key: Arc<str>,
    scope_type: Arc<str>,
    scope_position: Option<u64>,
    scope_surface: Option<Arc<str>>,
}

impl WarehouseFeaturePatternAccumulator {
    /// Aggregates warehouse `feature_pattern_counts` from one batch of n-way
    /// region / feature-diff facts (see `visit_nway_fact_row_batches`).
    ///
    /// `feature_diffs` is filtered to `WAREHOUSE_CORE_FEATURE_KEYS` and then
    /// scanned linearly for `WarehouseFeatureGroupKey` run boundaries, instead
    /// of being bucketed through a `BTreeMap`. This is only correct because
    /// `push_region_rows` (`warehouse::rows`) emits `feature_diffs` as
    /// *maximal contiguous runs* per group key: `region_index` is monotone
    /// non-decreasing and each region's `feature_groups` is emitted once as a
    /// block (see the producer-invariant test
    /// `push_region_rows_emits_maximal_contiguous_feature_diff_runs`). If a
    /// future change to the producer ever breaks that guarantee, the
    /// monotonicity guard below returns an error rather than silently
    /// under-merging groups (a debug-only assertion would not be safe here,
    /// since release builds must not silently miscount).
    ///
    /// Since `nway_feature_diffs` moved to a direct Arrow-column producer
    /// (`push_region_rows` no longer materializes `Vec<NwayFeatureDiffRow>`
    /// on the production path), production code calls
    /// [`record_region_feature_group`](Self::record_region_feature_group)
    /// instead of this method. `record` is retained as: (a) the reference
    /// oracle for the differential test
    /// `feature_pattern_accumulator_region_group_path_matches_reference_row_path`,
    /// and (b) a still-`pub` API this crate's own
    /// `warehouse_feature_pattern_accumulator` micro-bench (and its existing
    /// `feature_pattern_accumulator_linear_scan_matches_reference_grouping`
    /// test) drives directly.
    pub fn record(
        &mut self,
        regions: &[NwayRegionRow],
        feature_diffs: &[NwayFeatureDiffRow],
    ) -> Result<()> {
        let region_whitespace = Self::region_whitespace_lookup(regions);
        let keyed = Self::keyed_core_diffs(feature_diffs)?;

        #[cfg(debug_assertions)]
        let mut seen_group_keys = std::collections::HashSet::new();

        for run in keyed.chunk_by(|a, b| a.0 == b.0) {
            let group = &run[0].0;
            // Development invariant check: `chunk_by` only guarantees adjacent runs differ, so
            // a group key reappearing in a *later*, non-adjacent run means
            // `keyed` was not in maximal contiguous runs. Release builds rely
            // solely on the region_index monotonicity guard above (checked in
            // `keyed_core_diffs`); this is a debug-only early warning of a
            // deeper producer change (e.g. a region's `feature_groups`
            // repeating a key without changing `region_index`).
            #[cfg(debug_assertions)]
            debug_assert!(
                seen_group_keys.insert(group.clone()),
                "WarehouseFeatureGroupKey {group:?} recurred in a non-adjacent \
                 run of feature_diffs; push_region_rows must emit maximal \
                 contiguous runs per group key",
            );
            let facts = run.iter().map(|(_, diff)| *diff).collect::<Vec<_>>();
            self.apply_group(&region_whitespace, group, &facts);
        }
        Ok(())
    }

    fn region_whitespace_lookup(regions: &[NwayRegionRow]) -> BTreeMap<(&str, &str, u64), bool> {
        regions
            .iter()
            .map(|region| {
                (
                    (
                        region.source_id.as_ref(),
                        region.text_id.as_ref(),
                        region.region_index,
                    ),
                    region.is_nonempty_whitespace,
                )
            })
            .collect()
    }

    /// Filters `feature_diffs` to `WAREHOUSE_CORE_FEATURE_KEYS`, computes each
    /// surviving diff's `WarehouseFeatureGroupKey`, and asserts `region_index`
    /// is monotone non-decreasing across the filtered sequence, which is the
    /// release-safe half of the contiguity guard `record` depends on (see its
    /// doc comment). Bails instead of silently miscounting if violated.
    fn keyed_core_diffs(
        feature_diffs: &[NwayFeatureDiffRow],
    ) -> Result<Vec<(WarehouseFeatureGroupKey, &NwayFeatureDiffRow)>> {
        let mut keyed = Vec::with_capacity(feature_diffs.len());
        let mut last_region_index: Option<u64> = None;
        for diff in feature_diffs {
            if !WAREHOUSE_CORE_FEATURE_KEYS.contains(&diff.feature_key.as_ref()) {
                continue;
            }
            if let Some(last) = last_region_index
                && diff.region_index < last
            {
                bail!(
                    "feature_diffs region_index regressed from {last} to {} \
                     while grouping feature-pattern diffs; push_region_rows \
                     must emit monotone non-decreasing region_index for the \
                     linear group-key scan in `record` to assume maximal \
                     contiguous runs",
                    diff.region_index
                );
            }
            last_region_index = Some(diff.region_index);
            keyed.push((
                WarehouseFeatureGroupKey {
                    source_id: diff.source_id.clone(),
                    text_id: diff.text_id.clone(),
                    region_index: diff.region_index,
                    feature_key: diff.feature_key.clone(),
                    scope_type: diff.scope_type.clone(),
                    scope_position: diff.scope_position,
                    scope_surface: diff.scope_surface.clone(),
                },
                diff,
            ));
        }
        Ok(keyed)
    }

    fn apply_group(
        &mut self,
        region_whitespace: &BTreeMap<(&str, &str, u64), bool>,
        group: &WarehouseFeatureGroupKey,
        facts: &[&NwayFeatureDiffRow],
    ) {
        let Some(is_nonempty_whitespace) = region_whitespace
            .get(&(
                group.source_id.as_ref(),
                group.text_id.as_ref(),
                group.region_index,
            ))
            .copied()
        else {
            return;
        };
        let Some(pattern) = warehouse_feature_pattern_from_rows(group, facts) else {
            return;
        };
        let entry = self
            .patterns
            .entry(WarehouseFeaturePatternKey {
                feature_key: group.feature_key.as_ref().to_owned(),
                is_nonempty_whitespace,
                pattern,
            })
            .or_default();
        entry.examples += 1;
        entry.source_ids.insert(group.source_id.as_ref().to_owned());
        entry.text_ids.insert(group.text_id.as_ref().to_owned());
    }

    /// Reference grouping kept as the differential-test oracle for the linear
    /// scan in `record` (see
    /// `feature_pattern_accumulator_linear_scan_matches_reference_grouping`).
    /// This is the original `BTreeMap`-based bucketing `record` used before
    /// the linear-scan refactor.
    #[cfg(test)]
    fn group_feature_diffs_reference(
        feature_diffs: &[NwayFeatureDiffRow],
    ) -> Vec<(WarehouseFeatureGroupKey, Vec<&NwayFeatureDiffRow>)> {
        let mut groups = BTreeMap::<WarehouseFeatureGroupKey, Vec<&NwayFeatureDiffRow>>::new();
        for diff in feature_diffs {
            if !WAREHOUSE_CORE_FEATURE_KEYS.contains(&diff.feature_key.as_ref()) {
                continue;
            }
            groups
                .entry(WarehouseFeatureGroupKey {
                    source_id: diff.source_id.clone(),
                    text_id: diff.text_id.clone(),
                    region_index: diff.region_index,
                    feature_key: diff.feature_key.clone(),
                    scope_type: diff.scope_type.clone(),
                    scope_position: diff.scope_position,
                    scope_surface: diff.scope_surface.clone(),
                })
                .or_default()
                .push(diff);
        }
        groups.into_iter().collect()
    }

    #[cfg(test)]
    fn record_via_reference_grouping(
        &mut self,
        regions: &[NwayRegionRow],
        feature_diffs: &[NwayFeatureDiffRow],
    ) {
        let region_whitespace = Self::region_whitespace_lookup(regions);
        for (group, facts) in Self::group_feature_diffs_reference(feature_diffs) {
            self.apply_group(&region_whitespace, &group, &facts);
        }
    }

    pub fn into_rows(self) -> Vec<FeaturePatternCountRow> {
        self.patterns
            .into_iter()
            .map(|(key, entry)| FeaturePatternCountRow {
                kind: "feature".to_owned(),
                feature_profile: "core".to_owned(),
                feature_key: key.feature_key,
                is_nonempty_whitespace: key.is_nonempty_whitespace,
                pattern: key.pattern,
                examples: entry.examples,
                source_count: entry.source_ids.len() as u64,
                text_count: entry.text_ids.len() as u64,
                sample_source_ids: warehouse_sample_ids(&entry.source_ids),
                sample_text_ids: warehouse_sample_ids(&entry.text_ids),
                script_categories: String::new(),
            })
            .collect()
    }

    /// Aggregates one n-way region's feature-group directly from
    /// `ab_morph_diff::NwayFeatureGroup` data (`values`), without
    /// materializing a `NwayFeatureDiffRow` first. This is the production
    /// path `push_region_rows` (`warehouse::rows`) calls per feature-group,
    /// in the same pass that appends into the `NwayFeatureDiffsColumns`
    /// arrow builder.
    ///
    /// Unlike `record`/`keyed_core_diffs`, no re-grouping or contiguous-run
    /// scan is needed here: `values` is already partitioned by distinct
    /// feature value (one call per `NwayRegion::feature_groups` entry,
    /// already scoped to a single region), and already ordered exactly like
    /// the `BTreeMap<Option<FeatureValue>, Vec<AnalyzerId>>` that `record`'s
    /// `warehouse_feature_pattern_from_rows` reconstructs from flattened rows
    /// -- `ab_morph_diff::nway::value_group` builds `NwayFeatureGroup::values`
    /// from such a `BTreeMap` and sorts each value's analyzers, so iterating
    /// `values` in order and joining already-sorted `analyzers` reproduces
    /// the identical formatted pattern string. Content-identity with the
    /// row-based reference path is characterized by the differential test
    /// `feature_pattern_accumulator_region_group_path_matches_reference_row_path`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn record_region_feature_group(
        &mut self,
        source_id: &str,
        text_id: &str,
        is_nonempty_whitespace: bool,
        feature_key: &str,
        scope_type: &str,
        scope_position: Option<u64>,
        scope_surface: Option<&str>,
        values: &[NwayFeatureValueGroup],
    ) {
        if !WAREHOUSE_CORE_FEATURE_KEYS.contains(&feature_key) {
            return;
        }
        let Some(pattern) = warehouse_feature_pattern_from_value_groups(
            feature_key,
            scope_type,
            scope_position,
            scope_surface,
            values,
        ) else {
            return;
        };
        let entry = self
            .patterns
            .entry(WarehouseFeaturePatternKey {
                feature_key: feature_key.to_owned(),
                is_nonempty_whitespace,
                pattern,
            })
            .or_default();
        entry.examples += 1;
        entry.source_ids.insert(source_id.to_owned());
        entry.text_ids.insert(text_id.to_owned());
    }
}

fn warehouse_feature_pattern_from_rows(
    group: &WarehouseFeatureGroupKey,
    facts: &[&NwayFeatureDiffRow],
) -> Option<String> {
    let mut by_value = BTreeMap::<Option<Arc<str>>, Vec<Arc<str>>>::new();
    for fact in facts {
        by_value
            .entry(fact.feature_value.clone())
            .or_default()
            .extend(fact.analyzers.iter().cloned());
    }
    if by_value.len() <= 1 {
        return None;
    }
    let values = by_value
        .into_iter()
        .map(|(value, mut analyzers)| {
            analyzers.sort();
            format!(
                "{}=>{}",
                value.as_deref().unwrap_or_default(),
                analyzers
                    .iter()
                    .map(Arc::as_ref)
                    .collect::<Vec<_>>()
                    .join("+")
            )
        })
        .collect::<Vec<_>>()
        .join(" ; ");
    Some(format!(
        "{} {} {}",
        group.feature_key,
        warehouse_feature_scope_label(group),
        values
    ))
}

/// Same formatting as [`warehouse_feature_pattern_from_rows`], but fed
/// directly from an `ab_morph_diff::NwayFeatureGroup`'s already-partitioned
/// `values` instead of re-partitioning a flattened `&[&NwayFeatureDiffRow]`
/// via a `BTreeMap`. See
/// [`record_region_feature_group`](WarehouseFeaturePatternAccumulator::record_region_feature_group)
/// for why this produces an identical string.
fn warehouse_feature_pattern_from_value_groups(
    feature_key: &str,
    scope_type: &str,
    scope_position: Option<u64>,
    scope_surface: Option<&str>,
    values: &[NwayFeatureValueGroup],
) -> Option<String> {
    if values.len() <= 1 {
        return None;
    }
    let values_str = values
        .iter()
        .map(|value_group| {
            format!(
                "{}=>{}",
                value_group.value.as_deref().unwrap_or_default(),
                value_group
                    .analyzers
                    .iter()
                    .map(String::as_str)
                    .collect::<Vec<_>>()
                    .join("+")
            )
        })
        .collect::<Vec<_>>()
        .join(" ; ");
    Some(format!(
        "{feature_key} {} {values_str}",
        warehouse_feature_scope_label_parts(scope_type, scope_position, scope_surface)
    ))
}

fn warehouse_feature_scope_label(group: &WarehouseFeatureGroupKey) -> String {
    warehouse_feature_scope_label_parts(
        group.scope_type.as_ref(),
        group.scope_position,
        group.scope_surface.as_deref(),
    )
}

fn warehouse_feature_scope_label_parts(
    scope_type: &str,
    scope_position: Option<u64>,
    scope_surface: Option<&str>,
) -> String {
    match scope_type {
        "whole_region" => "whole_region".to_owned(),
        "token_position" => format!("token_position:{}", scope_position.unwrap_or_default()),
        "surface" => format!("surface:{}", scope_surface.unwrap_or_default()),
        other => other.to_owned(),
    }
}

fn warehouse_sample_ids(ids: &BTreeSet<String>) -> String {
    let mut sample = ids.iter().take(5).cloned().collect::<Vec<_>>().join(",");
    if ids.len() > 5 {
        sample.push_str(&format!(",...+{}", ids.len() - 5));
    }
    sample
}

fn read_aat_value(path: &Path) -> Result<Value> {
    // serde_json does not buffer its reader, so parse from bytes instead of
    // issuing tiny read() syscalls against the File.
    let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_slice(&bytes).with_context(|| format!("failed to parse {}", path.display()))
}

fn create_parent_dir(path: &Path) -> Result<()> {
    if let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    Ok(())
}

fn read_resume_ids(
    analyses_output: &Path,
    errors_output: Option<&Path>,
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    output_profile: OutputProfile,
) -> Result<BTreeSet<String>> {
    let prefer_source_id = output_profile == OutputProfile::Compact;
    let mut analysis_ids = BTreeSet::new();
    read_resume_ids_from_path(analyses_output, prefer_source_id, &mut analysis_ids)?;
    let mut ids = analysis_ids;
    if let Some(path) = nway_output {
        let mut nway_ids = BTreeSet::new();
        read_resume_ids_from_path(path, true, &mut nway_ids)?;
        ids = ids.intersection(&nway_ids).cloned().collect();
    }
    if let Some(path) = nway_pattern_counts_output {
        let mut nway_pattern_count_ids = BTreeSet::new();
        read_resume_ids_from_path(path, true, &mut nway_pattern_count_ids)?;
        ids = ids.intersection(&nway_pattern_count_ids).cloned().collect();
    }
    if let Some(path) = errors_output {
        read_resume_ids_from_path(path, prefer_source_id, &mut ids)?;
    }
    Ok(ids)
}

fn read_resume_ids_from_path(
    path: &Path,
    prefer_source_id: bool,
    ids: &mut BTreeSet<String>,
) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }

    let content = read_jsonl_or_zst_to_string(path)?;
    for (line_index, line) in content.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let value: Value = serde_json::from_str(line).with_context(|| {
            format!("failed to parse {} line {}", path.display(), line_index + 1)
        })?;
        let key = if prefer_source_id {
            value.get("source_id").and_then(Value::as_str)
        } else {
            value.get("text_id").and_then(Value::as_str)
        };
        if let Some(key) = key {
            ids.insert(key.to_owned());
        }
    }
    Ok(())
}

fn write_jsonl_row<T: Serialize, W: Write + ?Sized>(writer: &mut W, row: &T) -> Result<()> {
    serde_json::to_writer(&mut *writer, row)?;
    writer.write_all(b"\n")?;
    Ok(())
}

#[derive(Serialize)]
struct AnalysisRow<'a> {
    text_id: &'a str,
    analyzer: &'a str,
    analysis: &'a Analysis,
}

#[derive(Serialize)]
struct ComparisonRow<'a> {
    text_id: &'a str,
    from_analyzer: &'a str,
    to_analyzer: &'a str,
    comparison: &'a Comparison,
}

#[derive(Serialize)]
struct RunErrorRow {
    input_path: String,
    source_id: Option<String>,
    text_id: Option<String>,
    analyzer: Option<String>,
    stage: String,
    error: String,
}

fn warehouse_error_row(
    run_id: &str,
    source_id: Option<String>,
    text_id: Option<String>,
    analyzer_id: Option<String>,
    stage: &str,
    error_code: &str,
    message: &str,
) -> WarehouseErrorRow {
    WarehouseErrorRow {
        run_id: run_id.to_owned(),
        source_id,
        text_id,
        analyzer_id,
        stage: stage.to_owned(),
        error_code: error_code.to_owned(),
        message: message.to_owned(),
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct StringStatsReport {
    pub analysis_count: usize,
    pub morpheme_count: usize,
    pub analyzer_ids: StringCategoryStats,
    pub surfaces: StringCategoryStats,
    pub feature_keys: StringCategoryStats,
    pub feature_values: StringCategoryStats,
    pub warnings: Vec<RunWarning>,
}

#[derive(Debug, Clone, Serialize)]
pub struct RunWarning {
    pub stage: String,
    pub message: String,
}

impl StringStatsReport {
    pub fn record_analysis(&mut self, analysis: &Analysis) {
        self.analysis_count += 1;
        self.morpheme_count += analysis.morphemes.len();
        self.analyzer_ids.record(&analysis.analyzer);
        for morpheme in &analysis.morphemes {
            self.surfaces.record(&morpheme.surface);
            for (key, value) in morpheme.features.iter() {
                self.feature_keys.record(key);
                if let Some(value) = value {
                    self.feature_values.record(value);
                }
            }
        }
    }

    fn merge(&mut self, other: &Self) {
        self.analysis_count += other.analysis_count;
        self.morpheme_count += other.morpheme_count;
        self.analyzer_ids.merge(&other.analyzer_ids);
        self.surfaces.merge(&other.surfaces);
        self.feature_keys.merge(&other.feature_keys);
        self.feature_values.merge(&other.feature_values);
        self.warnings.extend(other.warnings.iter().cloned());
    }

    fn record_warning(&mut self, stage: impl Into<String>, message: impl Into<String>) {
        self.warnings.push(RunWarning {
            stage: stage.into(),
            message: message.into(),
        });
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct StringCategoryStats {
    pub total_occurrences: usize,
    pub unique_values: usize,
    pub total_bytes: usize,
    pub unique_bytes: usize,
    pub duplicate_occurrences: usize,
    pub duplicate_bytes: usize,
    #[serde(skip)]
    counts: HashMap<String, usize>,
}

impl StringCategoryStats {
    fn record(&mut self, value: &str) {
        self.record_many(value, 1);
    }

    fn record_many(&mut self, value: &str, count: usize) {
        if count == 0 {
            return;
        }
        let len = value.len();
        self.total_occurrences += count;
        self.total_bytes += len * count;
        match self.counts.get_mut(value) {
            Some(existing) => {
                *existing += count;
                self.duplicate_occurrences += count;
                self.duplicate_bytes += len * count;
            }
            None => {
                self.counts.insert(value.to_owned(), count);
                self.unique_values += 1;
                self.unique_bytes += len;
                self.duplicate_occurrences += count - 1;
                self.duplicate_bytes += len * count.saturating_sub(1);
            }
        }
    }

    fn merge(&mut self, other: &Self) {
        for (value, count) in &other.counts {
            self.record_many(value, *count);
        }
    }
}

fn write_error_row<W: Write + ?Sized>(writer: &mut W, row: &RunErrorRow) -> Result<()> {
    write_jsonl_row(writer, row)
}

/// Serialize an [`OrthoMapError`] from `remap_spans` as a `RunErrorRow`-shaped
/// JSONL row, so ortho remap failures are routed to the same errors output as
/// analyzer failures instead of crashing the pipeline.
fn write_ortho_remap_error<W: Write + ?Sized>(
    writer: &mut W,
    source_id: &str,
    e: &ab_ortho_detect::OrthoMapError,
) -> Result<()> {
    let (code, msg) = match e {
        ab_ortho_detect::OrthoMapError::CrossesBoundary { range, boundary } => (
            "crosses_boundary",
            format!("range {range:?} crosses boundary at byte {boundary}"),
        ),
        ab_ortho_detect::OrthoMapError::UncoveredOffset { offset } => {
            ("uncovered_offset", format!("offset {offset} not covered"))
        }
    };
    write_error_row(
        writer,
        &RunErrorRow {
            input_path: String::new(),
            source_id: Some(source_id.to_owned()),
            text_id: None,
            analyzer: None,
            stage: "ortho_remap".to_owned(),
            error: format!("{code}: {msg}"),
        },
    )
}

fn write_analysis_row<W: Write + ?Sized>(
    writer: &mut W,
    output_profile: OutputProfile,
    source_id: &str,
    analysis: &Analysis,
) -> Result<()> {
    match output_profile {
        OutputProfile::Full => {
            let row = AnalysisRow {
                text_id: &analysis.text_id,
                analyzer: &analysis.analyzer,
                analysis,
            };
            write_jsonl_row(writer, &row)
        }
        OutputProfile::Compact => {
            let row = compact::AnalysisSummaryRow::from_analysis(source_id.to_owned(), analysis);
            write_jsonl_row(writer, &row)
        }
    }
}

fn write_comparison_rows(
    mut writer: Option<&mut dyn Write>,
    mut examples_writer: Option<&mut dyn Write>,
    analyses: &[Analysis],
    source_id: &str,
    source_text: &str,
    output_profile: OutputProfile,
    max_examples_per_comparison: usize,
) -> Result<()> {
    for from_index in 0..analyses.len() {
        for to_index in (from_index + 1)..analyses.len() {
            match output_profile {
                OutputProfile::Full => {
                    let comparison = compare_pair(&analyses[from_index], &analyses[to_index], &[])?;
                    if let Some(writer) = writer.as_deref_mut() {
                        let row = ComparisonRow {
                            text_id: &comparison.text_id,
                            from_analyzer: &comparison.from_analyzer,
                            to_analyzer: &comparison.to_analyzer,
                            comparison: &comparison,
                        };
                        write_jsonl_row(writer, &row)?;
                    }

                    if let Some(examples_writer) = examples_writer.as_deref_mut() {
                        for row in compact::example_rows_from_comparison(
                            source_id.to_owned(),
                            source_text,
                            &comparison,
                            analyses,
                            max_examples_per_comparison,
                        ) {
                            write_jsonl_row(examples_writer, &row)?;
                        }
                    }
                }
                OutputProfile::Compact => {
                    let comparison = compare_pair_compact_with_source_text(
                        &analyses[from_index],
                        &analyses[to_index],
                        source_text,
                        &[],
                        max_examples_per_comparison,
                    )?;
                    if let Some(writer) = writer.as_deref_mut() {
                        let row = compact::ComparisonSummaryRow::from_compact_comparison(
                            source_id.to_owned(),
                            &comparison,
                            source_text,
                        );
                        write_jsonl_row(writer, &row)?;
                    }

                    if let Some(examples_writer) = examples_writer.as_deref_mut() {
                        for row in compact::example_rows_from_compact_comparison(
                            source_id.to_owned(),
                            source_text,
                            &comparison,
                        ) {
                            write_jsonl_row(examples_writer, &row)?;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn write_string_stats_report(path: &Path, report: &StringStatsReport) -> Result<()> {
    create_parent_dir(path)?;
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut writer = std::io::BufWriter::new(file);
    serde_json::to_writer_pretty(&mut writer, report)?;
    writeln!(writer)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_manifest(
    path: &Path,
    output_profile: OutputProfile,
    analyzer_ids: &[String],
    jobs: usize,
    input_mode: &str,
    input_path: &str,
    input_file_count: usize,
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    examples_output: Option<&Path>,
    errors_output: Option<&Path>,
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
) -> Result<()> {
    create_parent_dir(path)?;
    let manifest = compact::RunManifest {
        version: 1,
        output_profile: output_profile.as_str().to_owned(),
        analyzer_args: analyzer_ids.to_vec(),
        jobs,
        input_mode: input_mode.to_owned(),
        input_path: input_path.to_owned(),
        input_file_count,
        analyses_output: analyses_output.display().to_string(),
        comparisons_output: comparisons_output.map(|path| path.display().to_string()),
        examples_output: examples_output.map(|path| path.display().to_string()),
        errors_output: errors_output.map(|path| path.display().to_string()),
        nway_output: nway_output.map(|path| path.display().to_string()),
        nway_pattern_counts_output: nway_pattern_counts_output
            .map(|path| path.display().to_string()),
    };
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut writer = std::io::BufWriter::new(file);
    serde_json::to_writer_pretty(&mut writer, &manifest)?;
    writeln!(writer)?;
    Ok(())
}

enum LoadedAnalyzer {
    Vibrato(VibratoAnalyzer),
    Vaporetto(Box<VaporettoAnalyzer>),
    Sudachi(SudachiAnalyzer),
    Test(TestAnalyzerKind),
}

#[derive(Debug, Clone, Copy)]
enum TestAnalyzerKind {
    Single,
    Split,
}

impl LoadedAnalyzer {
    fn analyzer_id(&self) -> &str {
        match self {
            Self::Vibrato(analyzer) => analyzer.analyzer_id(),
            Self::Vaporetto(analyzer) => analyzer.analyzer_id(),
            Self::Sudachi(analyzer) => analyzer.analyzer_id(),
            Self::Test(TestAnalyzerKind::Single) => "test:single",
            Self::Test(TestAnalyzerKind::Split) => "test:split",
        }
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis> {
        match self {
            Self::Vibrato(analyzer) => Ok(analyzer.analyze(document)?),
            Self::Vaporetto(analyzer) => Ok(analyzer.analyze(document)?),
            Self::Sudachi(analyzer) => Ok(analyzer.analyze(document)?),
            Self::Test(kind) => Ok(test_analysis(*kind, document)),
        }
    }
}

fn test_analysis(kind: TestAnalyzerKind, document: &PlainTextDocument) -> Analysis {
    let mut morphemes = Vec::new();
    match kind {
        TestAnalyzerKind::Single => {
            morphemes.push(test_morpheme(
                document.text.clone(),
                0..document.text.len(),
                0..document.text.chars().count(),
            ));
        }
        TestAnalyzerKind::Split if document.text == "今日" => {
            morphemes.push(test_morpheme("今".to_owned(), 0..3, 0..1));
            morphemes.push(test_morpheme("日".to_owned(), 3..6, 1..2));
        }
        TestAnalyzerKind::Split => {
            morphemes.push(test_morpheme(
                document.text.clone(),
                0..document.text.len(),
                0..document.text.chars().count(),
            ));
        }
    }
    Analysis {
        analyzer: match kind {
            TestAnalyzerKind::Single => "test:single".to_owned(),
            TestAnalyzerKind::Split => "test:split".to_owned(),
        },
        text_id: document.text_id.clone(),
        source_text: Arc::from(document.text.as_str()),
        morphemes,
        warnings: Vec::new(),
        ortho_annotations: None,
        ortho_offset_map: None,
    }
}

fn test_morpheme(
    surface: String,
    byte_span: std::ops::Range<usize>,
    char_span: std::ops::Range<usize>,
) -> ab_morph_diff::Morpheme {
    let mut features = ab_morph_diff::FeatureMap::new();
    let _ = features.insert("pos1".into(), Some("名詞".into()));
    ab_morph_diff::Morpheme {
        surface,
        byte_span,
        char_span,
        features,
    }
}

#[cfg(test)]
mod tests {
    use std::time::{SystemTime, UNIX_EPOCH};

    use ab_morph_diff::{FeatureMap, Morpheme};

    use super::*;

    const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;

    #[test]
    fn resolve_run_normalization_off_is_identity_policy() {
        let prov = resolve_run_normalization(OrthoDetectMode::Off, None).unwrap();
        assert_eq!(prov.mode, "off");
        assert_eq!(prov.detector_id, None);
        // The persisted hash is the policy module's identity sentinel.
        assert_eq!(
            prov.policy_hash,
            ab_ortho_detect::NormalizationPolicy::identity().policy_hash()
        );
    }

    #[test]
    fn resolve_run_normalization_heuristic_matches_policy_hash() {
        let prov = resolve_run_normalization(OrthoDetectMode::Heuristic, None).unwrap();
        assert_eq!(prov.mode, "heuristic");
        // Serialized OrthoDetectorId::HeuristicV1.
        assert_eq!(prov.detector_id.as_deref(), Some("\"HeuristicV1\""));
        let expected = ab_ortho_detect::NormalizationPolicy::ortho_normalize_v1(
            ab_ortho_detect::OrthoDetectorId::HeuristicV1,
            vec![ab_ortho_detect::OrthoNormalization::ScriptKatakanaToHiragana],
        )
        .policy_hash();
        assert_eq!(prov.policy_hash, expected);
        assert_ne!(
            prov.policy_hash,
            ab_ortho_detect::NormalizationPolicy::identity().policy_hash()
        );
    }

    #[test]
    fn resolve_run_normalization_ml_requires_model() {
        let err = resolve_run_normalization(OrthoDetectMode::Ml, None).unwrap_err();
        assert!(err.to_string().contains("--ortho-ml-model"), "{err}");
    }

    #[test]
    #[ignore = "requires the kindai-bungo dictionary (AB_VIBRATO_DICT_DIR)"]
    fn resolve_run_normalization_historical_binds_oracle_and_validates() {
        // Exercises the real archive-hash path and the I2-D17 validation.
        let prov = resolve_run_normalization(OrthoDetectMode::Historical, None).unwrap();
        assert_eq!(prov.mode, "historical");
        let id = prov.detector_id.expect("historical detector id");
        assert!(id.contains("HistoricalRewriteV1"), "{id}");
        assert!(id.contains("dictionary_hash"), "{id}");
        assert!(id.contains("rules_hash"), "{id}");
        // The runs-row policy hash must equal the policy built from the same
        // oracle archive hash (resolve and the pipeline detector agree).
        let dict_hash = ab_morph_analyzers::dictionary_archive_hash(M2_ORACLE_DICTIONARY).unwrap();
        let expected = ab_ortho_detect::NormalizationPolicy::ortho_normalize_v1(
            ab_ortho_detect::OrthoDetectorId::HistoricalRewriteV1 {
                dictionary_hash: dict_hash,
                rules_hash: ab_ortho_detect::historical::rules_hash(),
            },
            vec![ab_ortho_detect::OrthoNormalization::HistoricalToModern],
        )
        .policy_hash();
        assert_eq!(prov.policy_hash, expected);
    }

    #[test]
    fn rejects_missing_input() {
        let err = run_default(None, None, &["vibrato".to_owned()]).unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_both_input_modes() {
        let err = run_default(
            Some(Path::new("a.json")),
            Some(Path::new("aat")),
            &["vibrato".to_owned()],
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_empty_analyzer_list() {
        let err = run_default(Some(Path::new("a.json")), None, &[]).unwrap_err();
        assert!(err.to_string().contains("at least one"));
    }

    #[test]
    fn zero_jobs_resolves_to_auto_instead_of_erroring() {
        // `--jobs 0` now means "auto" (memory-aware resolution), so it no
        // longer bails on the jobs check; the run proceeds to the next
        // validation step (a missing --aat file, here) instead.
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            None,
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
            None,
            false,
            0,
            OutputProfile::Full,
            None,
            10,
            None,
        )
        .unwrap_err();
        assert!(
            err.to_string()
                .contains("--aat must point to a regular file")
        );
    }

    #[test]
    fn parses_analyzer_specs() {
        assert_eq!(
            AnalyzerSpec::parse("vibrato").unwrap(),
            AnalyzerSpec::Vibrato(None)
        );
        assert_eq!(
            AnalyzerSpec::parse("vibrato:unidic-csj-202512").unwrap(),
            AnalyzerSpec::Vibrato(Some("unidic-csj-202512".to_owned()))
        );
        assert_eq!(
            AnalyzerSpec::parse("vaporetto").unwrap(),
            AnalyzerSpec::Vaporetto(None)
        );
        assert_eq!(
            AnalyzerSpec::parse("vaporetto:unidic-cwj-202512").unwrap(),
            AnalyzerSpec::Vaporetto(Some("unidic-cwj-202512".to_owned()))
        );
        assert_eq!(
            AnalyzerSpec::parse("sudachi-c").unwrap(),
            AnalyzerSpec::Sudachi(SudachiMode::C)
        );
    }

    #[test]
    fn rejects_unknown_analyzer_spec() {
        let err = AnalyzerSpec::parse("unknown").unwrap_err();
        assert!(err.to_string().contains("unknown analyzer"));
    }

    #[test]
    fn dedupes_analyzer_specs_in_first_seen_order() {
        let specs = parse_analyzer_specs(&[
            "vibrato".to_owned(),
            "vibrato:unidic-csj-202512".to_owned(),
            "sudachi-c".to_owned(),
            "vibrato".to_owned(),
            "vibrato:unidic-csj-202512".to_owned(),
            "vaporetto".to_owned(),
            "vaporetto:unidic-csj-202512".to_owned(),
        ])
        .unwrap();

        assert_eq!(
            specs,
            vec![
                AnalyzerSpec::Vibrato(None),
                AnalyzerSpec::Vibrato(Some("unidic-csj-202512".to_owned())),
                AnalyzerSpec::Sudachi(SudachiMode::C),
                AnalyzerSpec::Vaporetto(None),
                AnalyzerSpec::Vaporetto(Some("unidic-csj-202512".to_owned())),
            ]
        );
    }

    #[test]
    fn dedupes_default_and_explicit_vibrato_analyzers() {
        let specs = parse_analyzer_specs(&[
            "vibrato".to_owned(),
            "vibrato:unidic-cwj-202512".to_owned(),
            "sudachi-c".to_owned(),
            "vibrato:unidic-csj-202512".to_owned(),
        ])
        .unwrap();

        assert_eq!(
            specs,
            vec![
                AnalyzerSpec::Vibrato(None),
                AnalyzerSpec::Sudachi(SudachiMode::C),
                AnalyzerSpec::Vibrato(Some("unidic-csj-202512".to_owned())),
            ]
        );
    }

    #[test]
    fn discovers_single_aat_file() {
        let dir = temp_dir("single");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("work.json");
        fs::write(&file, "{}").unwrap();

        assert_eq!(
            discover_aat_inputs(Some(&file), None).unwrap(),
            vec![file.clone()]
        );

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn discovers_sorted_json_files_in_directory() {
        let dir = temp_dir("sorted");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("b.json"), "{}").unwrap();
        fs::write(dir.join("a.json"), "{}").unwrap();
        fs::write(dir.join("ignored.txt"), "{}").unwrap();

        let paths = discover_aat_inputs(None, Some(&dir)).unwrap();
        assert_eq!(paths, vec![dir.join("a.json"), dir.join("b.json")]);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn discovers_json_files_recursively_in_directory() {
        let dir = temp_dir("recursive");
        let nested = dir.join("aozora-rs-adapter");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("b.json"), "{}").unwrap();
        fs::write(dir.join("a.json"), "{}").unwrap();
        fs::write(nested.join("ignored.txt"), "{}").unwrap();

        let paths = discover_aat_inputs(None, Some(&dir)).unwrap();
        assert_eq!(paths, vec![dir.join("a.json"), nested.join("b.json")]);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn rejects_empty_aat_directory() {
        let dir = temp_dir("empty");
        fs::create_dir_all(&dir).unwrap();

        let err = discover_aat_inputs(None, Some(&dir)).unwrap_err();
        assert!(err.to_string().contains("no AAT JSON files"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn rejects_non_file_aat_path() {
        let dir = temp_dir("not-file");
        fs::create_dir_all(&dir).unwrap();

        let err = discover_aat_inputs(Some(&dir), None).unwrap_err();
        assert!(err.to_string().contains("regular file"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_one_comparison_row_for_two_valid_analyses() {
        let analyses = vec![valid_analysis("from"), valid_analysis("to")];
        let mut out = Vec::new();

        write_comparison_rows(
            Some(&mut out),
            None,
            &analyses,
            "source-a",
            "今日",
            OutputProfile::Full,
            10,
        )
        .unwrap();

        let lines = String::from_utf8(out).unwrap();
        let rows = lines.lines().collect::<Vec<_>>();
        assert_eq!(rows.len(), 1);
        assert!(rows[0].contains("\"text_id\":\"t1\""));
        assert!(rows[0].contains("\"from_analyzer\":\"from\""));
        assert!(rows[0].contains("\"to_analyzer\":\"to\""));
    }

    #[test]
    fn writes_error_row_for_failed_analyzer() {
        let mut out = Vec::new();
        write_error_row(
            &mut out,
            &RunErrorRow {
                input_path: "aat/work.json".to_owned(),
                source_id: Some("source-a".to_owned()),
                text_id: Some("work".to_owned()),
                analyzer: Some("sudachi-c".to_owned()),
                stage: "analyze".to_owned(),
                error: "input too long".to_owned(),
            },
        )
        .unwrap();

        let row: serde_json::Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(row["input_path"], "aat/work.json");
        assert_eq!(row["source_id"], "source-a");
        assert_eq!(row["text_id"], "work");
        assert_eq!(row["analyzer"], "sudachi-c");
        assert_eq!(row["stage"], "analyze");
        assert_eq!(row["error"], "input too long");
    }

    #[test]
    fn reads_resume_ids_from_existing_jsonl_outputs() {
        let dir = temp_dir("resume");
        fs::create_dir_all(&dir).unwrap();
        let analyses = dir.join("analyses.jsonl");
        let errors = dir.join("errors.jsonl");
        fs::write(
            &analyses,
            "{\"text_id\":\"done-analysis\",\"analyzer\":\"vibrato\"}\n",
        )
        .unwrap();
        fs::write(
            &errors,
            "{\"text_id\":\"done-error\",\"stage\":\"analyze\"}\n{\"stage\":\"read_aat\"}\n",
        )
        .unwrap();

        let ids =
            read_resume_ids(&analyses, Some(&errors), None, None, OutputProfile::Full).unwrap();
        assert!(ids.contains("done-analysis"));
        assert!(ids.contains("done-error"));
        assert!(!ids.contains("read_aat"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_resume_uses_source_id_from_zstd_outputs() {
        let dir = temp_dir("resume-zst");
        fs::create_dir_all(&dir).unwrap();
        let analyses = dir.join("analyses.jsonl.zst");
        {
            let mut writer = open_output_writer(&analyses, false).unwrap();
            writer
                .write_all(b"{\"source_id\":\"source-a\",\"text_id\":\"same\"}\n")
                .unwrap();
            writer.flush().unwrap();
        }

        let ids = read_resume_ids(&analyses, None, None, None, OutputProfile::Compact).unwrap();
        assert!(ids.contains("source-a"));
        assert!(!ids.contains("same"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_resume_filter_preserves_duplicate_text_ids() {
        let dir = temp_dir("resume-filter");
        fs::create_dir_all(&dir).unwrap();
        let first = dir.join("same-a.json");
        let second = dir.join("same-b.json");
        fs::write(&first, TINY_AAT).unwrap();
        fs::write(&second, TINY_AAT.replace("source-a", "source-b")).unwrap();
        let mut resume_ids = BTreeSet::new();
        resume_ids.insert("same-a".to_owned());

        let filtered = filter_resume_inputs(
            vec![first.clone(), second.clone()],
            &resume_ids,
            OutputProfile::Compact,
        )
        .unwrap();

        assert_eq!(filtered, vec![second]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn selected_runner_manifest_preserves_original_input_path() {
        let dir = temp_dir("selected-manifest");
        let aat_dir = dir.join("aats");
        fs::create_dir_all(&aat_dir).unwrap();
        let input = aat_dir.join("source-a.json");
        fs::write(&input, TINY_AAT).unwrap();

        let out = dir.join("out");
        fs::create_dir_all(&out).unwrap();
        run_analyze_aat_selected(
            vec![input],
            "aat_dir",
            &aat_dir.display().to_string(),
            &["test:single".to_owned()],
            &out.join("analyses.jsonl"),
            None,
            Some(&out.join("errors.jsonl")),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            Some(&out.join("manifest.json")),
            OrthoDetectMode::Off,
            None,
        )
        .unwrap();

        let manifest: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(out.join("manifest.json")).unwrap()).unwrap();
        assert_eq!(manifest["input_path"], aat_dir.display().to_string());
        assert_eq!(manifest["input_file_count"], 1);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_mode_writes_sealed_parquet_without_jsonl_outputs() {
        let dir = temp_dir("warehouse-mode");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Full,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(run_dir.join("runs.parquet").is_file());
        assert!(run_dir.join("run_analyzers.parquet").is_file());
        assert!(run_dir.join("sources.parquet").is_file());
        assert!(run_dir.join("morphemes.parquet").is_file());
        assert!(run_dir.join("nway_regions.parquet").is_file());
        assert!(!run_dir.join("analyses.jsonl").exists());
        assert!(!run_dir.join("comparisons.jsonl").exists());

        // T1 run-normalization provenance sidecar: source-identity run records
        // the identity policy hash for ABC to read.
        let provenance: serde_json::Value = serde_json::from_str(
            &fs::read_to_string(run_dir.join(RUN_NORMALIZATION_PROVENANCE_FILE)).unwrap(),
        )
        .unwrap();
        assert_eq!(provenance["run_id"], "run-a");
        assert_eq!(provenance["ortho_detect_mode"], "off");
        assert!(provenance["input_normalization_detector_id"].is_null());
        assert_eq!(
            provenance["input_normalization_policy_hash"],
            ab_ortho_detect::NormalizationPolicy::identity().policy_hash()
        );

        let staging = warehouse_dir.join(".staging");
        if staging.exists() {
            assert!(fs::read_dir(&staging).unwrap().next().is_none());
        }

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_prepares_ortho_detector_once_across_batches() {
        struct CountingDetector(Arc<std::sync::atomic::AtomicUsize>);

        impl OrthoDetector for CountingDetector {
            fn detector_id(&self) -> ab_ortho_detect::OrthoDetectorId {
                ab_ortho_detect::OrthoDetectorId::HeuristicV1
            }

            fn detect(
                &self,
                _sentences: &[ab_plaintext::SentenceSpan<'_>],
            ) -> Vec<ab_ortho_detect::OrthoAnnotation> {
                self.0.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                Vec::new()
            }
        }

        struct DetectorReset;

        impl Drop for DetectorReset {
            fn drop(&mut self) {
                pipeline::set_test_ortho_detector(None);
            }
        }

        let dir = temp_dir("warehouse-detector-lifetime");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        for index in 0..65 {
            fs::write(
                aat_dir.join(format!("source-{index:03}.json")),
                tiny_aat(&format!("work-{index:03}")),
            )
            .unwrap();
        }

        pipeline::reset_detector_build_count();
        let detect_calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        pipeline::set_test_ortho_detector(Some(Arc::new(CountingDetector(detect_calls.clone()))));
        let _detector_reset = DetectorReset;
        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &warehouse_dir,
            "run-detector-lifetime",
            2,
            WarehouseProfile::Triage,
            1,
            OrthoDetectMode::Heuristic,
            None,
            None,
        )
        .unwrap();

        assert_eq!(pipeline::detector_build_count(), 1);
        assert_eq!(detect_calls.load(std::sync::atomic::Ordering::SeqCst), 65);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_eligibility_filter_narrows_run_to_selected_source_ids() {
        // Lane A: only works in the eligible set are analyzed; the rest are
        // dropped before analysis (run-eligibility filter, outside normalization).
        let dir = temp_dir("warehouse-eligibility");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-old.json"),
            tiny_aat("work-old").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();
        fs::write(
            aat_dir.join("source-modern.json"),
            tiny_aat("work-modern").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        let eligible: BTreeSet<String> = ["source-old".to_owned()].into_iter().collect();
        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Full,
            3,
            OrthoDetectMode::Off,
            None,
            Some(&eligible),
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        let file = fs::File::open(run_dir.join("sources.parquet")).unwrap();
        let reader = parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder::try_new(file)
            .unwrap()
            .build()
            .unwrap();
        let mut source_ids = BTreeSet::new();
        for batch in reader {
            let batch = batch.unwrap();
            let idx = batch.schema().index_of("source_id").unwrap();
            let values = batch
                .column(idx)
                .as_any()
                .downcast_ref::<arrow_array::StringArray>()
                .unwrap();
            for row in 0..batch.num_rows() {
                source_ids.insert(values.value(row).to_owned());
            }
        }
        assert_eq!(
            source_ids, eligible,
            "only the eligible work should be analyzed"
        );

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_triage_profile_omits_raw_feature_tables() {
        let dir = temp_dir("warehouse-triage-profile");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Triage,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(run_dir.join("runs.parquet").is_file());
        assert!(run_dir.join("sources.parquet").is_file());
        assert!(run_dir.join("morphemes.parquet").is_file());
        assert!(run_dir.join("nway_regions.parquet").is_file());
        assert!(run_dir.join("nway_region_analyzers.parquet").is_file());
        assert!(run_dir.join("feature_pattern_counts.parquet").is_file());
        assert!(!run_dir.join("morpheme_features.parquet").exists());
        assert!(!run_dir.join("nway_feature_diffs.parquet").exists());
        let views_sql = fs::read_to_string(run_dir.join("views.sql")).unwrap();
        assert!(!views_sql.contains("warehouse_nway_feature_diffs"));
        assert!(!views_sql.contains("top_feature_differences"));

        let rows = summarize_warehouse_nway_patterns(
            &run_dir,
            WarehousePatternOptions {
                kind: NwayPatternKind::Feature,
                feature_key: Some("pos1".to_owned()),
                feature_profile: WarehouseFeatureProfile::Core,
                text_filter: WarehouseTextFilter::LexicalOnly,
                excluded_feature_values: BTreeSet::new(),
                exclusions: SummaryExclusions::default(),
                limit: 10,
            },
        )
        .unwrap();
        assert!(rows.is_empty());

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_full_profile_writes_projection_spans() {
        let dir = temp_dir("warehouse-projection-spans");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Full,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert_eq!(
            warehouse::writer::parquet_table_row_count(&run_dir, WarehouseTable::ProjectionSpans)
                .unwrap(),
            1,
            "TINY_AAT has exactly one contributing text node"
        );
        let views_sql = fs::read_to_string(run_dir.join("views.sql")).unwrap();
        assert!(views_sql.contains("warehouse_projection_spans"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_triage_profile_omits_projection_spans() {
        let dir = temp_dir("warehouse-triage-spans");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Triage,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(!run_dir.join("projection_spans.parquet").exists());
        let views_sql = fs::read_to_string(run_dir.join("views.sql")).unwrap();
        assert!(!views_sql.contains("warehouse_projection_spans"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_mode_accepts_parallel_jobs_and_publishes_one_run() {
        let dir = temp_dir("warehouse-parallel-mode");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();
        fs::write(
            aat_dir.join("source-b.json"),
            tiny_aat("work-b").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            2,
            WarehouseProfile::Full,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(run_dir.join("runs.parquet").is_file());
        assert!(run_dir.join("sources.parquet").is_dir());
        assert!(run_dir.join("nway_regions.parquet").is_dir());
        assert!(run_dir.join("projection_spans.parquet").is_dir());
        assert_eq!(
            fs::read_dir(warehouse_dir.join("runs"))
                .unwrap()
                .filter_map(Result::ok)
                .filter(|entry| entry.path().is_dir())
                .count(),
            1
        );
        assert!(!dir.join("analyses.jsonl").exists());

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_mode_rejects_shards_prefixed_run_id() {
        let dir = temp_dir("warehouse-rejects-shards-run-id");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        let err = run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "shards-run-a",
            1,
            WarehouseProfile::Full,
            3,
            OrthoDetectMode::Off,
            None,
            None,
        )
        .unwrap_err();
        assert!(
            err.to_string().contains(r#"must not start with "shards-""#),
            "{err}"
        );

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn partitions_inputs_by_size_to_balance_worker_load() {
        let dir = temp_dir("partition-sizes");
        fs::create_dir_all(&dir).unwrap();
        let inputs = [90usize, 80, 70, 60, 50]
            .into_iter()
            .enumerate()
            .map(|(index, size)| {
                let path = dir.join(format!("work-{index}.json"));
                fs::write(&path, vec![b'x'; size]).unwrap();
                path
            })
            .collect::<Vec<_>>();

        let partitions = partition_inputs(inputs, 3);

        let partition_sizes = partitions
            .iter()
            .map(|partition| {
                partition
                    .iter()
                    .map(|path| fs::metadata(path).unwrap().len())
                    .sum::<u64>()
            })
            .collect::<Vec<_>>();

        assert_eq!(partition_sizes, vec![90, 130, 130]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn partitions_delay_largest_inputs_to_reduce_peak_worker_memory() {
        let dir = temp_dir("partition-memory");
        fs::create_dir_all(&dir).unwrap();
        let inputs = [
            8 * 1024 * 1024usize,
            7 * 1024 * 1024,
            6 * 1024 * 1024,
            5 * 1024 * 1024,
            10,
            9,
            8,
            7,
        ]
        .into_iter()
        .enumerate()
        .map(|(index, size)| {
            let path = dir.join(format!("work-{index}.json"));
            File::create(&path).unwrap().set_len(size as u64).unwrap();
            path
        })
        .collect::<Vec<_>>();

        let partitions = partition_inputs(inputs, 4);
        let first_wave_sizes = partitions
            .iter()
            .map(|partition| fs::metadata(partition.first().unwrap()).unwrap().len())
            .collect::<Vec<_>>();

        assert_eq!(first_wave_sizes, vec![10, 9, 8, 7]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_work_queue_limits_concurrent_large_batches() {
        let dir = temp_dir("dynamic-large-lanes");
        fs::create_dir_all(&dir).unwrap();
        let inputs = [8 * 1024 * 1024usize, 7 * 1024 * 1024]
            .into_iter()
            .enumerate()
            .map(|(index, size)| {
                let path = dir.join(format!("large-{index}.json"));
                File::create(&path).unwrap().set_len(size as u64).unwrap();
                path
            })
            .collect::<Vec<_>>();
        let mut queue = WarehouseWorkQueue::new(inputs, 1);

        let first = queue.take_batch().unwrap();

        assert!(first.is_large);
        assert!(queue.take_batch().is_none());
        queue.complete_batch(first.is_large);
        assert!(queue.take_batch().unwrap().is_large);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_work_queue_front_loads_large_work_before_regular() {
        // P1: large docs dispatch first (within the lane cap) so they overlap the
        // abundant regular work instead of forming an idle tail at the end of the run.
        let dir = temp_dir("dynamic-large-first");
        fs::create_dir_all(&dir).unwrap();
        let inputs = [8 * 1024 * 1024usize, 12, 11]
            .into_iter()
            .enumerate()
            .map(|(index, size)| {
                let path = dir.join(format!("work-{index}.json"));
                File::create(&path).unwrap().set_len(size as u64).unwrap();
                path
            })
            .collect::<Vec<_>>();
        let mut queue = WarehouseWorkQueue::new(inputs, 1);

        let large = queue.take_batch().unwrap();
        let regular = queue.take_batch().unwrap();

        assert!(large.is_large);
        assert!(!regular.is_large);
        assert_eq!(regular.inputs.len(), 2);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn symlink_input_file_uses_readable_absolute_target() {
        let dir = temp_dir("symlink");
        let source_dir = dir.join("source");
        let link_dir = dir.join("links");
        fs::create_dir_all(&source_dir).unwrap();
        fs::create_dir_all(&link_dir).unwrap();
        let source = source_dir.join("work.json");
        let link = link_dir.join("work.json");
        fs::write(&source, "{}").unwrap();

        symlink_input_file(&source, &link).unwrap();

        assert_eq!(fs::read_to_string(&link).unwrap(), "{}");

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_profile_writes_summary_rows_without_full_regions() {
        let dir = temp_dir("compact-profile");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let analyses = dir.join("analyses.jsonl");
        let comparisons = dir.join("comparisons.jsonl");
        let errors = dir.join("errors.jsonl");

        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = fs::read_to_string(&analyses).unwrap();
        assert!(analysis_text.contains("\"source_id\":\"source-a\""));
        assert!(analysis_text.contains("\"morpheme_count\""));
        assert!(!analysis_text.contains("source_text"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_parallel_profile_writes_compressed_summary_rows() {
        let dir = temp_dir("compact-parallel");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        for name in ["source-a", "source-b"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }

        let analyses = dir.join("analyses.jsonl.zst");
        let comparisons = dir.join("comparisons.jsonl.zst");
        let errors = dir.join("errors.jsonl.zst");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = read_jsonl_or_zst_to_string(&analyses).unwrap();
        assert_eq!(analysis_text.lines().count(), 2);
        assert!(analysis_text.contains("\"source_id\":\"source-a\""));
        assert!(analysis_text.contains("\"source_id\":\"source-b\""));
        assert!(!analysis_text.contains("source_text"));

        let error_text = read_jsonl_or_zst_to_string(&errors).unwrap();
        assert_eq!(error_text.lines().count(), 0);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_parallel_resume_appends_zstd_outputs_by_source_id() {
        let dir = temp_dir("compact-parallel-resume-zstd");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        for name in ["source-a", "source-b"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }

        let analyses = dir.join("analyses.jsonl.zst");
        let comparisons = dir.join("comparisons.jsonl.zst");
        let errors = dir.join("errors.jsonl.zst");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        for name in ["source-c", "source-d"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            true,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = read_jsonl_or_zst_to_string(&analyses).unwrap();
        assert_eq!(analysis_text.lines().count(), 4);
        for name in ["source-a", "source-b", "source-c", "source-d"] {
            assert_eq!(
                analysis_text
                    .matches(&format!("\"source_id\":\"{name}\""))
                    .count(),
                1
            );
        }

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_manifest_for_compact_run() {
        let dir = temp_dir("manifest");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let manifest = dir.join("manifest.json");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &dir.join("analyses.jsonl"),
            Some(&dir.join("comparisons.jsonl")),
            Some(&dir.join("errors.jsonl")),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            Some(&manifest),
        )
        .unwrap();

        let value: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&manifest).unwrap()).unwrap();
        assert_eq!(value["output_profile"], "compact");
        assert_eq!(value["jobs"], 1);
        assert_eq!(value["input_file_count"], 1);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_string_stats_report_when_requested() {
        let dir = temp_dir("string-stats-output");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let stats_path = dir.join("reports").join("string-stats.json");
        run_analyze_aat_with_nway(
            None,
            Some(&aat_dir),
            &["test:single".to_owned()],
            &dir.join("analyses.jsonl"),
            None,
            Some(&dir.join("errors.jsonl")),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            None,
            None,
            None,
            None,
            Some(&stats_path),
            crate::OrthoDetectMode::Off,
            None,
        )
        .unwrap();

        let value: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&stats_path).unwrap()).unwrap();
        assert_eq!(value["analysis_count"], 1);
        assert!(value["morpheme_count"].as_u64().unwrap() > 0);
        assert!(value["surfaces"]["total_occurrences"].as_u64().unwrap() > 0);
        assert!(value["feature_keys"]["unique_values"].as_u64().unwrap() > 0);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn string_stats_report_counts_repeated_features_and_surfaces() {
        let analysis = Analysis {
            analyzer: "fixture".to_owned(),
            text_id: "t1".to_owned(),
            source_text: Arc::from("日日"),
            morphemes: vec![
                Morpheme {
                    surface: "日".to_owned(),
                    byte_span: 0..3,
                    char_span: 0..1,
                    features: [
                        ("pos1".into(), Some("名詞".into())),
                        ("lemma".into(), Some("日".into())),
                    ]
                    .into_iter()
                    .collect(),
                },
                Morpheme {
                    surface: "日".to_owned(),
                    byte_span: 3..6,
                    char_span: 1..2,
                    features: [
                        ("pos1".into(), Some("名詞".into())),
                        ("lemma".into(), Some("日".into())),
                    ]
                    .into_iter()
                    .collect(),
                },
            ],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };

        let mut report = StringStatsReport::default();
        report.record_analysis(&analysis);

        assert_eq!(report.analysis_count, 1);
        assert_eq!(report.morpheme_count, 2);
        assert_eq!(report.surfaces.total_occurrences, 2);
        assert_eq!(report.surfaces.unique_values, 1);
        assert_eq!(report.feature_keys.total_occurrences, 4);
        assert_eq!(report.feature_keys.unique_values, 2);
        assert_eq!(report.feature_values.total_occurrences, 4);
        assert_eq!(report.feature_values.unique_values, 2);
        assert!(report.feature_keys.duplicate_bytes > 0);
    }

    #[test]
    fn merge_warehouse_shard_runs_coalesces_small_part_tables() {
        use warehouse::schema::SourceRow;
        let root = temp_dir("merge-coalesce");
        let warehouse_dir = root.join("wh");
        // 65 shards, each with a tiny 1-row sources.parquet -> 65 staged parts,
        // median well under 1 MiB -> compaction triggers.
        let mut shard_run_dirs = Vec::new();
        for i in 0..65u64 {
            let shard_dir = root.join(format!("shard-{i}"));
            let shard_paths = WarehousePaths::new(&shard_dir, "shard");
            let mut writer = WarehouseWriter::create(shard_paths.clone()).unwrap();
            writer
                .append_sources(&[SourceRow {
                    run_id: "r".to_owned(),
                    source_id: format!("s{i}"),
                    text_id: format!("t{i}"),
                    aat_path: format!("aat/{i}.json"),
                    source_bytes: 1,
                    source_chars: 1,
                }])
                .unwrap();
            writer.finalize().unwrap();
            shard_run_dirs.push(shard_paths.final_dir);
        }

        let options = WarehouseParallelOptions {
            warehouse_dir: warehouse_dir.clone(),
            run_id: "merged".to_owned(),
            jobs: 1,
            input_mode: "test",
            input_path: "test".to_owned(),
            analyzer_rows: vec![],
            warehouse_profile: WarehouseProfile::Full,
            zstd_level: 3,
            ortho_detect: OrthoDetectMode::Off,
            ortho_ml_model: None,
            prepared_ortho_detector: PreparedOrthoDetector(None),
            normalization: RunNormalizationProvenance {
                mode: "off".to_owned(),
                detector_id: None,
                policy_hash: "sha256:identity".to_owned(),
            },
        };
        merge_warehouse_shard_runs(&options, &shard_run_dirs).unwrap();

        let merged = WarehousePaths::new(&warehouse_dir, "merged");
        let sources_dir = merged.final_dir.join(WarehouseTable::Sources.file_name());
        let sources_parts: Vec<_> = fs::read_dir(&sources_dir)
            .unwrap()
            .filter_map(Result::ok)
            .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "parquet"))
            .collect();
        assert_eq!(
            sources_parts.len(),
            1,
            "sources should be coalesced to 1 file, got {}",
            sources_parts.len()
        );

        let _ = fs::remove_dir_all(root);
    }

    fn run_default(
        aat: Option<&Path>,
        aat_dir: Option<&Path>,
        analyzer_ids: &[String],
    ) -> Result<()> {
        run_analyze_aat(
            aat,
            aat_dir,
            analyzer_ids,
            Path::new("out.jsonl"),
            None,
            None,
            false,
            1,
            OutputProfile::Full,
            None,
            10,
            None,
        )
    }

    fn valid_analysis(analyzer: &str) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t1".to_owned(),
            source_text: Arc::from("今日"),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features: FeatureMap::new(),
            }],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }

    fn tiny_aat(work_id: &str) -> String {
        TINY_AAT.replace("source-a", work_id)
    }

    fn temp_dir(label: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-{label}-{}-{unique}",
            std::process::id()
        ))
    }

    // --- WarehouseFeaturePatternAccumulator: BTreeMap -> linear scan ---

    fn warehouse_test_diff_row(
        region_index: u64,
        feature_key: &str,
        scope_type: &str,
        scope_position: Option<u64>,
        scope_surface: Option<&str>,
        feature_value: Option<&str>,
        analyzer_id: &str,
    ) -> NwayFeatureDiffRow {
        NwayFeatureDiffRow {
            run_id: Arc::from("run-a"),
            source_id: Arc::from("source-a"),
            text_id: Arc::from("text-a"),
            region_index,
            feature_key: Arc::from(feature_key),
            scope_type: Arc::from(scope_type),
            scope_position,
            scope_surface: scope_surface.map(Arc::from),
            feature_value: feature_value.map(Arc::from),
            analyzers: vec![Arc::from(analyzer_id)],
        }
    }

    fn warehouse_test_region_row(region_index: u64, is_nonempty_whitespace: bool) -> NwayRegionRow {
        NwayRegionRow {
            run_id: Arc::from("run-a"),
            source_id: Arc::from("source-a"),
            text_id: Arc::from("text-a"),
            region_index,
            byte_start: 0,
            byte_end: 0,
            char_start: 0,
            char_end: 0,
            is_nonempty_whitespace,
            is_agreement: false,
            has_coverage_mismatch: false,
            has_segmentation_disagreement: false,
            has_feature_disagreement: true,
        }
    }

    /// Shared invariant check used both by the hand-crafted differential test
    /// (as a fixture validation check) and by the producer-invariant CI guard
    /// (against the real `push_region_rows` output): `region_index` must be
    /// monotone non-decreasing, and no `WarehouseFeatureGroupKey` may recur
    /// in a non-adjacent run.
    fn assert_feature_diffs_form_maximal_contiguous_runs(feature_diffs: &[NwayFeatureDiffRow]) {
        let mut seen_group_keys = std::collections::HashSet::new();
        let mut previous_key: Option<WarehouseFeatureGroupKey> = None;
        let mut previous_region_index: Option<u64> = None;
        for diff in feature_diffs {
            if let Some(previous) = previous_region_index {
                assert!(
                    diff.region_index >= previous,
                    "region_index regressed from {previous} to {}",
                    diff.region_index
                );
            }
            previous_region_index = Some(diff.region_index);

            let key = WarehouseFeatureGroupKey {
                source_id: diff.source_id.clone(),
                text_id: diff.text_id.clone(),
                region_index: diff.region_index,
                feature_key: diff.feature_key.clone(),
                scope_type: diff.scope_type.clone(),
                scope_position: diff.scope_position,
                scope_surface: diff.scope_surface.clone(),
            };
            if previous_key.as_ref() != Some(&key) {
                assert!(
                    seen_group_keys.insert(key.clone()),
                    "WarehouseFeatureGroupKey {key:?} recurred in a non-adjacent run"
                );
            }
            previous_key = Some(key);
        }
    }

    #[test]
    fn feature_pattern_accumulator_linear_scan_matches_reference_grouping() {
        // Hand-crafted input: a non-core feature key (must be dropped by both
        // paths), two regions, multiple scopes (whole_region, surface,
        // token_position), and -- within region 0 -- groups emitted in
        // non-alphabetical ("interleaved") order (pos2 before pos1) so the
        // test does not accidentally pass just because the input happens to
        // already be sorted like the reference `BTreeMap`. The fixture itself
        // still satisfies the maximal-contiguous-runs invariant (asserted
        // below), matching what the real producer guarantees.
        let regions = vec![
            warehouse_test_region_row(0, false),
            warehouse_test_region_row(1, true),
        ];
        let feature_diffs = vec![
            warehouse_test_diff_row(
                0,
                "reading",
                "whole_region",
                None,
                None,
                Some("キョウ"),
                "vibrato",
            ),
            warehouse_test_diff_row(0, "pos2", "whole_region", None, None, Some("B"), "vibrato"),
            warehouse_test_diff_row(
                0,
                "pos2",
                "whole_region",
                None,
                None,
                Some("C"),
                "sudachi-a",
            ),
            warehouse_test_diff_row(0, "pos1", "whole_region", None, None, Some("A"), "vibrato"),
            warehouse_test_diff_row(
                0,
                "pos1",
                "whole_region",
                None,
                None,
                Some("D"),
                "sudachi-a",
            ),
            warehouse_test_diff_row(
                1,
                "pos3",
                "surface",
                None,
                Some("東京"),
                Some("E"),
                "vibrato",
            ),
            warehouse_test_diff_row(1, "pos3", "surface", None, Some("東京"), None, "sudachi-a"),
            warehouse_test_diff_row(
                1,
                "pos1",
                "token_position",
                Some(0),
                None,
                Some("F"),
                "vibrato",
            ),
            warehouse_test_diff_row(
                1,
                "pos1",
                "token_position",
                Some(0),
                None,
                Some("G"),
                "sudachi-a",
            ),
        ];

        assert_feature_diffs_form_maximal_contiguous_runs(&feature_diffs);

        let mut linear = WarehouseFeaturePatternAccumulator::default();
        linear.record(&regions, &feature_diffs).unwrap();

        let mut reference = WarehouseFeaturePatternAccumulator::default();
        reference.record_via_reference_grouping(&regions, &feature_diffs);

        assert_eq!(linear.into_rows(), reference.into_rows());
    }

    #[test]
    fn feature_pattern_accumulator_rejects_regressing_region_index() {
        // A release-safe guard, not a debug-only assert: if a future producer
        // change ever regresses `region_index` mid-batch, `record` must bail
        // rather than silently under-merge groups.
        let regions = vec![warehouse_test_region_row(0, false)];
        let feature_diffs = vec![
            warehouse_test_diff_row(1, "pos1", "whole_region", None, None, Some("A"), "vibrato"),
            warehouse_test_diff_row(
                0,
                "pos1",
                "whole_region",
                None,
                None,
                Some("B"),
                "sudachi-a",
            ),
        ];

        let mut accumulator = WarehouseFeaturePatternAccumulator::default();
        let error = accumulator.record(&regions, &feature_diffs).unwrap_err();
        assert!(error.to_string().contains("region_index regressed"));
    }

    fn nway_test_morpheme(
        surface: &str,
        byte_span: std::ops::Range<usize>,
        char_span: std::ops::Range<usize>,
        features: &[(&str, Option<&str>)],
    ) -> Morpheme {
        let mut map = FeatureMap::new();
        for (key, value) in features {
            let _ = map.insert((*key).into(), value.map(Into::into));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span,
            char_span,
            features: map,
        }
    }

    /// Representative multi-region, multi-scope, multi-analyzer fixture
    /// shared by `push_region_rows_emits_maximal_contiguous_feature_diff_runs`
    /// and `feature_pattern_accumulator_region_group_path_matches_reference_row_path`.
    ///
    /// The "晴れ" morpheme (aligned across all three analyzers, so
    /// whole-region-eligible) also carries a `reading` feature -- a
    /// NON-core key (`WAREHOUSE_CORE_FEATURE_KEYS` is only
    /// `pos1`/`pos2`/`pos3`/`pos4`) with two distinct values across
    /// analyzers, so it forms a real `NwayFeatureGroup` that appears in
    /// `feature_diffs` but must be excluded from `feature_pattern_counts`.
    /// This exercises the core-key filter
    /// (`record_region_feature_group`/`keyed_core_diffs`) on both the
    /// production and reference paths, which the `pos1`/`pos2`-only fixture
    /// alone never did.
    fn nway_test_fixture_analyses(source_text: &str) -> Vec<Analysis> {
        let vibrato = Analysis {
            analyzer: "vibrato".to_owned(),
            text_id: "text-a".to_owned(),
            source_text: Arc::from(source_text),
            morphemes: vec![
                nway_test_morpheme(
                    "今日",
                    0..6,
                    0..2,
                    &[("pos1", Some("名詞")), ("pos2", Some("A"))],
                ),
                nway_test_morpheme("は", 6..9, 2..3, &[("pos2", Some("X"))]),
                nway_test_morpheme(
                    "晴れ",
                    9..15,
                    3..5,
                    &[("pos1", Some("動詞")), ("reading", Some("ハレ"))],
                ),
            ],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };
        let sudachi_a = Analysis {
            analyzer: "sudachi-a".to_owned(),
            text_id: "text-a".to_owned(),
            source_text: Arc::from(source_text),
            morphemes: vec![
                nway_test_morpheme(
                    "今日",
                    0..6,
                    0..2,
                    &[("pos1", Some("固有名詞")), ("pos2", Some("A"))],
                ),
                nway_test_morpheme("は", 6..9, 2..3, &[("pos2", Some("Y"))]),
                nway_test_morpheme(
                    "晴れ",
                    9..15,
                    3..5,
                    &[("pos1", Some("名詞")), ("reading", Some("ハレ"))],
                ),
            ],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };
        let sudachi_c = Analysis {
            analyzer: "sudachi-c".to_owned(),
            text_id: "text-a".to_owned(),
            source_text: Arc::from(source_text),
            morphemes: vec![
                nway_test_morpheme("今日は", 0..9, 0..3, &[("pos1", Some("感動詞"))]),
                nway_test_morpheme(
                    "晴れ",
                    9..15,
                    3..5,
                    &[("pos1", Some("名詞")), ("reading", Some("ハレル"))],
                ),
            ],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };
        vec![vibrato, sudachi_a, sudachi_c]
    }

    #[test]
    fn push_region_rows_emits_maximal_contiguous_feature_diff_runs() {
        // Runs the row-based
        // REFERENCE producer (`warehouse::rows::nway_fact_rows`, which
        // drives `push_region_rows_reference`; production
        // calls `push_region_rows` directly against a `NwayFeatureDiffsColumns`
        // Arrow builder instead) over representative multi-region,
        // multi-scope input and asserts the emitted `feature_diffs` form
        // maximal contiguous `WarehouseFeatureGroupKey` runs.
        // `WarehouseFeaturePatternAccumulator::record`'s linear scan depends
        // on this; if a future change to the producer breaks it, this test
        // (not a silent miscount in release) is what should fail. The
        // production path is characterized against the same invariant below,
        // via `decode_feature_diff_rows`.
        let source_text = "今日は晴れ";
        let analyses = nway_test_fixture_analyses(source_text);

        let facts =
            crate::warehouse::rows::nway_fact_rows("run-a", "source-a", source_text, &analyses)
                .unwrap();

        // Precondition check: the fixture is non-trivial, spanning multiple
        // regions and multiple scope types.
        assert!(
            facts
                .feature_diffs
                .iter()
                .map(|diff| diff.region_index)
                .collect::<BTreeSet<_>>()
                .len()
                >= 2,
            "fixture must span multiple regions"
        );
        assert!(
            facts
                .feature_diffs
                .iter()
                .map(|diff| diff.scope_type.as_ref())
                .collect::<BTreeSet<_>>()
                .len()
                >= 2,
            "fixture must exercise multiple scope types"
        );

        assert_feature_diffs_form_maximal_contiguous_runs(&facts.feature_diffs);

        // Production path: `push_region_rows` (driven by
        // `visit_nway_fact_row_batches`, appending straight into a
        // `NwayFeatureDiffsColumns` Arrow builder) must emit the identical
        // maximal-contiguous order, not just the reference producer above.
        let mut production_feature_diffs = Vec::new();
        let mut pattern_counts = WarehouseFeaturePatternAccumulator::default();
        warehouse::rows::visit_nway_fact_row_batches(
            "run-a",
            "source-a",
            source_text,
            &analyses,
            10_000,
            &mut pattern_counts,
            |batch| {
                production_feature_diffs.extend(warehouse::rows::decode_feature_diff_rows(
                    &batch.feature_diffs.finish(),
                ));
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(production_feature_diffs, facts.feature_diffs);
        assert_feature_diffs_form_maximal_contiguous_runs(&production_feature_diffs);
    }

    #[test]
    fn feature_pattern_accumulator_region_group_path_matches_reference_row_path() {
        // Content-neutrality differential test for nway_feature_diffs
        // direct-column producer): the production path feeds
        // `feature_pattern_counts` via
        // `WarehouseFeaturePatternAccumulator::record_region_feature_group`,
        // called once per `NwayRegion::feature_groups` entry from
        // `push_region_rows` (`warehouse::rows`), directly from
        // `ab_morph_diff` types -- no `Vec<NwayFeatureDiffRow>` is ever
        // materialized on this path. The retained row-based `record` (fed by
        // `nway_fact_rows`'s `Vec<NwayFeatureDiffRow>`, itself built by the
        // `#[cfg(test)]`-only `push_region_rows_reference`) is this test's
        // oracle. Both must aggregate identical `feature_pattern_counts`
        // rows -- see `record_region_feature_group`'s doc comment for why
        // `NwayFeatureGroup::values`'s pre-sorted ordering guarantees this.
        //
        // The fixture also carries a NON-core `reading` feature key (see
        // `nway_test_fixture_analyses`'s doc comment), so this test also
        // exercises the `WAREHOUSE_CORE_FEATURE_KEYS` filter
        // (`record_region_feature_group` vs. `record`/`keyed_core_diffs`) on
        // both paths, not just the core `pos1`/`pos2` keys.
        let source_text = "今日は晴れ";
        let analyses = nway_test_fixture_analyses(source_text);

        let facts =
            crate::warehouse::rows::nway_fact_rows("run-a", "source-a", source_text, &analyses)
                .unwrap();
        // Precondition check: the fixture's non-core key reaches `feature_diffs`
        // (forming a `>=2`-value `NwayFeatureGroup`), ensuring the core-key filter
        // below is tested against real data rather than operating as a no-op.
        assert!(
            facts
                .feature_diffs
                .iter()
                .any(|diff| diff.feature_key.as_ref() == "reading"),
            "fixture must include a non-core feature key that reaches feature_diffs"
        );

        let mut reference = WarehouseFeaturePatternAccumulator::default();
        reference
            .record(&facts.regions, &facts.feature_diffs)
            .unwrap();

        let mut region_group = WarehouseFeaturePatternAccumulator::default();
        warehouse::rows::visit_nway_fact_row_batches(
            "run-a",
            "source-a",
            source_text,
            &analyses,
            10_000,
            &mut region_group,
            |_batch| Ok(()),
        )
        .unwrap();

        let reference_rows = reference.into_rows();
        let region_group_rows = region_group.into_rows();
        // The non-core `reading` key must be excluded from
        // `feature_pattern_counts` on both paths -- the core-key filter
        // behaves identically whether fed row-by-row (`record`) or
        // group-by-group (`record_region_feature_group`).
        assert!(
            reference_rows
                .iter()
                .all(|row| row.feature_key != "reading"),
            "non-core feature key must be excluded from the reference path's feature_pattern_counts"
        );
        assert!(
            region_group_rows
                .iter()
                .all(|row| row.feature_key != "reading"),
            "non-core feature key must be excluded from the production path's feature_pattern_counts"
        );
        assert_eq!(region_group_rows, reference_rows);
    }
}
