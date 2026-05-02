mod compact;
mod nway;
mod output;
mod script;
mod select;
mod summary;
mod warehouse;

use std::collections::{BTreeSet, HashMap, VecDeque};
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use ab_morph_analyzers::{MorphAnalyzer, SudachiAnalyzer, SudachiMode, VibratoAnalyzer};
use ab_morph_diff::{
    Analysis, Comparison, MorphDiffError, compare_pair, compare_pair_compact_with_source_text,
};
use ab_plaintext::{PlainTextDocument, from_aat_value};
use anyhow::{Context, Result, bail};
use clap::ValueEnum;
use output::{open_output_writer, read_jsonl_or_zst_to_string};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use warehouse::schema::{
    ErrorRow as WarehouseErrorRow, RunAnalyzerRow, RunRow, WarehousePaths, WarehouseTable,
};
use warehouse::writer::{WarehouseWriter, parquet_table_row_count, stage_parquet_table_part};

const LARGE_INPUT_THRESHOLD_BYTES: u64 = 5 * 1024 * 1024;
const WAREHOUSE_MORPHEME_ROW_BATCH_SIZE: usize = 50_000;
const WAREHOUSE_REGULAR_BATCH_SIZE: usize = 32;

pub use nway::{NwayFeatureScopeRow, NwayFeatureValueGroupRow, NwaySegmentationGroupRow};
pub use script::ScriptCategory;
pub use select::resolve_source_id_aat_paths;
pub use summary::{
    CompactDifferenceKindFilter, CompactDifferenceSummaryOptions, CompactDifferenceSummaryRow,
    CompactExampleFilter, CompactExampleSummaryOptions, CompactExampleSummaryRow,
    CompactExampleSummarySort, CompactSummaryGroupBy, CompactSummaryOptions, CompactSummaryRow,
    CompactSummarySort, NwayPatternKind, NwayPatternOptions, NwayPatternRow, NwaySummaryOptions,
    NwaySummaryRow, NwaySummarySort, SummaryExclusions, WarehouseErrorGroupBy,
    WarehouseErrorSummaryOptions, WarehouseErrorSummaryRow, WarehouseFeatureDiffExampleRow,
    WarehousePairwiseSort, WarehousePairwiseSummaryOptions, WarehousePairwiseSummaryRow,
    WarehousePatternExampleOptions, WarehousePatternOptions, WarehouseRegionAnalyzerExampleRow,
    WarehouseRegionExampleRow, WarehouseRegionKind, WarehouseRegionOptions, WarehouseTextFilter,
    summarize_compact_comparisons, summarize_compact_differences, summarize_compact_examples,
    summarize_nway, summarize_nway_pattern_counts, summarize_nway_patterns,
    summarize_warehouse_errors, summarize_warehouse_nway, summarize_warehouse_nway_patterns,
    summarize_warehouse_pairwise, summarize_warehouse_pattern_examples,
    summarize_warehouse_regions, write_warehouse_nway_patterns_duckdb_tsv,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputProfile {
    Full,
    Compact,
}

impl OutputProfile {
    fn as_str(self) -> &'static str {
        match self {
            OutputProfile::Full => "full",
            OutputProfile::Compact => "compact",
        }
    }
}

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
    run_analyze_aat_with_nway(
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
        None,
        None,
        None,
        None,
    )
}

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
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat
        .or(aat_dir)
        .map(|path| path.display().to_string())
        .unwrap_or_default();
    run_analyze_aat_inputs(
        inputs,
        input_mode,
        &input_path,
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
    )
}

pub fn run_analyze_aat_warehouse(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
) -> Result<()> {
    if jobs == 0 {
        bail!("--jobs must be greater than zero");
    }
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat
        .or(aat_dir)
        .map(|path| path.display().to_string())
        .unwrap_or_default();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer_rows = warehouse_analyzer_rows(run_id, &specs, &analyzers)?;
    if jobs == 1 {
        let input_count = inputs.len();
        run_analyze_aat_serial(
            inputs,
            &analyzers,
            SerialRunOptions {
                analyses_output: None,
                comparisons_output: None,
                errors_output: None,
                resume: false,
                output_profile: OutputProfile::Compact,
                examples_output: None,
                max_examples_per_comparison: 0,
                nway_output: None,
                nway_pattern_counts_output: None,
                max_nway_examples_per_text: 0,
                collect_string_stats: false,
                warehouse: Some(WarehouseRunOptions {
                    paths: WarehousePaths::new(warehouse_dir, run_id),
                    input_mode,
                    input_path,
                    analyzer_rows,
                }),
                progress: Some(SerialProgress {
                    label: format!("warehouse:{run_id}"),
                    total: input_count,
                }),
            },
        )?;
    } else {
        run_analyze_aat_warehouse_parallel(
            inputs,
            analyzers,
            WarehouseParallelOptions {
                warehouse_dir: warehouse_dir.to_path_buf(),
                run_id: run_id.to_owned(),
                jobs,
                input_mode,
                input_path,
                analyzer_rows,
            },
        )?;
    }
    Ok(())
}

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
) -> Result<()> {
    if inputs.is_empty() {
        bail!("provide at least one AAT input");
    }
    run_analyze_aat_inputs(
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
        None,
        None,
        None,
        None,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_analyze_aat_inputs(
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
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: Option<usize>,
    string_stats_output: Option<&Path>,
) -> Result<()> {
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
    if (nway_output.is_some() || nway_pattern_counts_output.is_some())
        && output_profile != OutputProfile::Compact
    {
        bail!(
            "--nway-output and --nway-pattern-counts-output require --output-profile compact in phase 1"
        );
    }
    if (nway_output.is_some() || nway_pattern_counts_output.is_some()) && analyzer_ids.len() < 2 {
        bail!("N-way outputs require at least two --analyzer values");
    }
    let max_nway_examples_per_text =
        max_nway_examples_per_text.unwrap_or(max_examples_per_comparison);
    let collect_string_stats = string_stats_output.is_some();

    let input_file_count = inputs.len();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;

    let string_stats = if jobs > 1 {
        run_analyze_aat_parallel(
            inputs,
            analyzers,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            jobs,
            output_profile,
            examples_output,
            max_examples_per_comparison,
            nway_output,
            nway_pattern_counts_output,
            max_nway_examples_per_text,
            collect_string_stats,
        )?
    } else {
        run_analyze_aat_serial(
            inputs,
            &analyzers,
            SerialRunOptions {
                analyses_output: Some(analyses_output),
                comparisons_output,
                errors_output,
                resume,
                output_profile,
                examples_output,
                max_examples_per_comparison,
                nway_output,
                nway_pattern_counts_output,
                max_nway_examples_per_text,
                collect_string_stats,
                warehouse: None,
                progress: None,
            },
        )?
    };

    if let Some(path) = manifest_output {
        write_manifest(
            path,
            output_profile,
            analyzer_ids,
            jobs,
            input_mode,
            input_path,
            input_file_count,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
            nway_output,
            nway_pattern_counts_output,
        )?;
    }
    if let Some(path) = string_stats_output {
        write_string_stats_report(path, &string_stats)?;
    }

    Ok(())
}

struct SerialRunOptions<'a> {
    analyses_output: Option<&'a Path>,
    comparisons_output: Option<&'a Path>,
    errors_output: Option<&'a Path>,
    resume: bool,
    output_profile: OutputProfile,
    examples_output: Option<&'a Path>,
    max_examples_per_comparison: usize,
    nway_output: Option<&'a Path>,
    nway_pattern_counts_output: Option<&'a Path>,
    max_nway_examples_per_text: usize,
    collect_string_stats: bool,
    warehouse: Option<WarehouseRunOptions>,
    progress: Option<SerialProgress>,
}

#[derive(Clone)]
struct SerialProgress {
    label: String,
    total: usize,
}

struct WarehouseRunOptions {
    paths: WarehousePaths,
    input_mode: &'static str,
    input_path: String,
    analyzer_rows: Vec<RunAnalyzerRow>,
}

struct WarehouseParallelOptions {
    warehouse_dir: PathBuf,
    run_id: String,
    jobs: usize,
    input_mode: &'static str,
    input_path: String,
    analyzer_rows: Vec<RunAnalyzerRow>,
}

fn run_analyze_aat_serial(
    inputs: Vec<PathBuf>,
    analyzers: &[Arc<LoadedAnalyzer>],
    options: SerialRunOptions<'_>,
) -> Result<StringStatsReport> {
    let resume_ids = if options.resume {
        let analyses_output = options
            .analyses_output
            .context("resume requires an analyses output path")?;
        read_resume_ids(
            analyses_output,
            options.errors_output,
            options.nway_output,
            options.nway_pattern_counts_output,
            options.output_profile,
        )?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_ids, options.output_profile)?;
    let input_count = inputs.len() as u64;

    let mut analyses_writer = if let Some(path) = options.analyses_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut comparisons_writer = if let Some(path) = options.comparisons_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut examples_writer = if let Some(path) = options.examples_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut errors_writer = if let Some(path) = options.errors_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut nway_writer = if let Some(path) = options.nway_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut nway_pattern_counts_writer = if let Some(path) = options.nway_pattern_counts_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut warehouse_writer = if let Some(warehouse) = &options.warehouse {
        let mut writer = WarehouseWriter::create(warehouse.paths.clone())?;
        writer.append_run_analyzers(&warehouse.analyzer_rows)?;
        Some(writer)
    } else {
        None
    };
    let mut warehouse_error_count = 0u64;
    let mut string_stats = StringStatsReport::default();
    let progress = options.progress.clone();

    for (input_index, input) in inputs.into_iter().enumerate() {
        let input_path = input.display().to_string();
        let source_id = compact::source_id_from_aat_path(&input);
        if let Some(progress) = &progress {
            eprintln!(
                "ab-morph-run: {} analyzing {}/{} source_id={}",
                progress.label,
                input_index + 1,
                progress.total,
                source_id
            );
        }
        let aat = match read_aat_value(&input) {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut warehouse_writer {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        options
                            .warehouse
                            .as_ref()
                            .expect("warehouse options")
                            .paths
                            .run_id
                            .as_str(),
                        Some(source_id.clone()),
                        None,
                        None,
                        "read_aat",
                        "read_aat_failed",
                        &error.to_string(),
                    )])?;
                    continue;
                }
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        &mut **writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
                            text_id: None,
                            analyzer: None,
                            stage: "read_aat".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                    continue;
                }
                return Err(error);
            }
        };
        let document = match from_aat_value(&aat) {
            Ok(document) => document,
            Err(error) => {
                if let Some(writer) = &mut warehouse_writer {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        options
                            .warehouse
                            .as_ref()
                            .expect("warehouse options")
                            .paths
                            .run_id
                            .as_str(),
                        Some(source_id.clone()),
                        None,
                        None,
                        "project_aat",
                        "project_aat_failed",
                        &error.to_string(),
                    )])?;
                    continue;
                }
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        &mut **writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
                            text_id: None,
                            analyzer: None,
                            stage: "project_aat".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                    continue;
                }
                return Err(error.into());
            }
        };
        let mut analyses = Vec::new();

        for analyzer in analyzers {
            let mut analysis = match analyzer.analyze(&document) {
                Ok(analysis) => analysis,
                Err(error) => {
                    if let Some(writer) = &mut warehouse_writer {
                        warehouse_error_count += 1;
                        writer.append_errors(&[warehouse_error_row(
                            options
                                .warehouse
                                .as_ref()
                                .expect("warehouse options")
                                .paths
                                .run_id
                                .as_str(),
                            Some(source_id.clone()),
                            Some(document.text_id.clone()),
                            Some(analyzer.analyzer_id().to_owned()),
                            "analyze",
                            "analyze_failed",
                            &error.to_string(),
                        )])?;
                        continue;
                    }
                    if let Some(writer) = &mut errors_writer {
                        write_error_row(
                            &mut **writer,
                            &RunErrorRow {
                                input_path: input_path.clone(),
                                source_id: Some(source_id.clone()),
                                text_id: Some(document.text_id.clone()),
                                analyzer: Some(analyzer.analyzer_id().to_owned()),
                                stage: "analyze".to_owned(),
                                error: error.to_string(),
                            },
                        )?;
                        continue;
                    }
                    return Err(error);
                }
            };
            if options.collect_string_stats {
                string_stats.record_analysis(&analysis);
            }

            if let Some(writer) = &mut analyses_writer {
                write_analysis_row(&mut **writer, options.output_profile, &source_id, &analysis)?;
            }
            if options.output_profile == OutputProfile::Compact && options.warehouse.is_none() {
                analysis.source_text.clear();
            }
            analyses.push(analysis);
        }

        if let Some(writer) = &mut warehouse_writer
            && let Some(first_analysis) = analyses.first()
        {
            let run_id = options
                .warehouse
                .as_ref()
                .expect("warehouse options")
                .paths
                .run_id
                .as_str();
            let source =
                warehouse::rows::source_row(run_id, &source_id, &input_path, first_analysis);
            writer.append_sources(&[source])?;
            let analysis_rows = analyses
                .iter()
                .map(|analysis| warehouse::rows::analysis_row(run_id, &source_id, analysis))
                .collect::<Vec<_>>();
            writer.append_analyses(&analysis_rows)?;
            for analysis in &mut analyses {
                analysis.source_text.clear();
            }
            for analysis in &analyses {
                for start in
                    (0..analysis.morphemes.len()).step_by(WAREHOUSE_MORPHEME_ROW_BATCH_SIZE)
                {
                    let end =
                        (start + WAREHOUSE_MORPHEME_ROW_BATCH_SIZE).min(analysis.morphemes.len());
                    let morphemes = warehouse::rows::morpheme_rows_for_range(
                        run_id,
                        &source_id,
                        analysis,
                        start..end,
                    );
                    writer.append_morphemes(&morphemes)?;
                    let features = warehouse::rows::morpheme_feature_rows_for_range(
                        run_id,
                        &source_id,
                        analysis,
                        start..end,
                    );
                    writer.append_morpheme_features(&features)?;
                }
            }
            match append_warehouse_nway_fact_rows(
                writer,
                run_id,
                &source_id,
                &document.text,
                &analyses,
            ) {
                Ok(()) => {}
                Err(error) if error.downcast_ref::<MorphDiffError>().is_some() => {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        run_id,
                        Some(source_id.clone()),
                        Some(document.text_id.clone()),
                        None,
                        "compare_nway",
                        "compare_nway_failed",
                        &error.to_string(),
                    )])?;
                }
                Err(error) => return Err(error),
            }
        }

        let comparison_result = if comparisons_writer.is_some() || examples_writer.is_some() {
            write_comparison_rows(
                comparisons_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                examples_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                &analyses,
                &source_id,
                &document.text,
                options.output_profile,
                options.max_examples_per_comparison,
            )
        } else {
            Ok(())
        };
        if let Err(error) = comparison_result {
            if let Some(error_writer) = &mut errors_writer {
                write_error_row(
                    &mut **error_writer,
                    &RunErrorRow {
                        input_path: input_path.clone(),
                        source_id: Some(source_id.clone()),
                        text_id: Some(document.text_id.clone()),
                        analyzer: None,
                        stage: "compare".to_owned(),
                        error: error.to_string(),
                    },
                )?;
            } else {
                return Err(error);
            }
        }
        if nway_writer.is_some() || nway_pattern_counts_writer.is_some() {
            match nway::row_and_pattern_counts_from_analyses(
                source_id.clone(),
                &document.text,
                &analyses,
                options.max_nway_examples_per_text,
            ) {
                Ok((row, pattern_counts)) => {
                    if let Some(writer) = &mut nway_writer {
                        write_jsonl_row(&mut **writer, &row)?;
                    }
                    if let Some(writer) = &mut nway_pattern_counts_writer {
                        for pattern_count in pattern_counts {
                            write_jsonl_row(&mut **writer, &pattern_count)?;
                        }
                    }
                }
                Err(error) => {
                    if let Some(error_writer) = &mut errors_writer {
                        write_error_row(
                            &mut **error_writer,
                            &RunErrorRow {
                                input_path: input_path.clone(),
                                source_id: Some(source_id.clone()),
                                text_id: Some(document.text_id.clone()),
                                analyzer: None,
                                stage: "compare_nway".to_owned(),
                                error: error.to_string(),
                            },
                        )?;
                    } else {
                        return Err(error.into());
                    }
                }
            }
        }
    }

    if let Some(writer) = &mut analyses_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut comparisons_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut examples_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut errors_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut nway_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut nway_pattern_counts_writer {
        writer.flush()?;
    }
    if let Some(mut writer) = warehouse_writer {
        let warehouse = options.warehouse.as_ref().expect("warehouse options");
        writer.append_runs(&[RunRow {
            schema_version: warehouse::schema::SCHEMA_VERSION,
            run_id: warehouse.paths.run_id.clone(),
            created_at_utc: chrono::Utc::now().to_rfc3339(),
            input_mode: warehouse.input_mode.to_owned(),
            input_path: warehouse.input_path.clone(),
            source_count: input_count,
            analyzer_count: warehouse.analyzer_rows.len() as u64,
            error_count: warehouse_error_count,
        }])?;
        writer.finalize()?;
    }
    Ok(string_stats)
}

fn run_analyze_aat_warehouse_parallel(
    inputs: Vec<PathBuf>,
    analyzers: Vec<Arc<LoadedAnalyzer>>,
    options: WarehouseParallelOptions,
) -> Result<()> {
    let total_inputs = inputs.len();
    let large_lanes = bounded_large_lane_count(options.jobs);
    let queue = Arc::new(Mutex::new(WarehouseWorkQueue::new(inputs, large_lanes)));
    let temp_root = std::env::temp_dir().join(format!(
        "ab-morph-run-warehouse-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    let shard_warehouse_dir = temp_root.join("warehouse");
    fs::create_dir_all(&shard_warehouse_dir)
        .with_context(|| format!("failed to create {}", shard_warehouse_dir.display()))?;

    let result = std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for job_index in 0..options.jobs {
            let analyzers = analyzers.clone();
            let shard_warehouse_dir = shard_warehouse_dir.clone();
            let shard_run_id = options.run_id.clone();
            let input_mode = options.input_mode;
            let input_path = options.input_path.clone();
            let analyzer_rows = options.analyzer_rows.clone();
            let queue = Arc::clone(&queue);
            handles.push(scope.spawn(move || -> Result<WarehouseShardOutput> {
                let mut shard_run_dirs = Vec::new();
                loop {
                    let Some(batch) = take_warehouse_work_batch(&queue) else {
                        break;
                    };
                    let shard_index = batch.shard_index;
                    let batch_is_large = batch.is_large;
                    let batch_len = batch.inputs.len();
                    let paths = WarehousePaths::new(
                        shard_warehouse_dir.join(format!("shard-{shard_index}")),
                        shard_run_id.clone(),
                    );
                    let result = run_analyze_aat_serial(
                        batch.inputs,
                        &analyzers,
                        SerialRunOptions {
                            analyses_output: None,
                            comparisons_output: None,
                            errors_output: None,
                            resume: false,
                            output_profile: OutputProfile::Compact,
                            examples_output: None,
                            max_examples_per_comparison: 0,
                            nway_output: None,
                            nway_pattern_counts_output: None,
                            max_nway_examples_per_text: 0,
                            collect_string_stats: false,
                            warehouse: Some(WarehouseRunOptions {
                                paths: paths.clone(),
                                input_mode,
                                input_path: input_path.clone(),
                                analyzer_rows: analyzer_rows.clone(),
                            }),
                            progress: Some(SerialProgress {
                                label: format!("warehouse-worker-{job_index}/shard-{shard_index}"),
                                total: batch_len,
                            }),
                        },
                    );
                    complete_warehouse_work_batch(&queue, batch_is_large);
                    result?;
                    shard_run_dirs.push(paths.final_dir);
                }
                Ok(WarehouseShardOutput { shard_run_dirs })
            }));
        }

        let mut outputs = Vec::new();
        for handle in handles {
            outputs.push(handle.join().expect("warehouse worker panicked")?);
        }
        Ok::<_, anyhow::Error>(outputs)
    });

    let outputs = match result {
        Ok(outputs) => outputs,
        Err(error) => {
            let _ = fs::remove_dir_all(&temp_root);
            return Err(error);
        }
    };
    let mut shard_run_dirs = outputs
        .into_iter()
        .flat_map(|output| output.shard_run_dirs)
        .collect::<Vec<_>>();
    shard_run_dirs.sort();
    eprintln!(
        "ab-morph-run: warehouse merging {} dynamic shard(s) from {} input(s) into run_id={}",
        shard_run_dirs.len(),
        total_inputs,
        options.run_id
    );
    merge_warehouse_shard_runs(&options, &shard_run_dirs)?;
    eprintln!(
        "ab-morph-run: warehouse merge complete run_id={}",
        options.run_id
    );
    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
    Ok(())
}

struct WarehouseShardOutput {
    shard_run_dirs: Vec<PathBuf>,
}

struct WarehouseWorkBatch {
    shard_index: usize,
    inputs: Vec<PathBuf>,
    is_large: bool,
}

struct WarehouseWorkQueue {
    regular: VecDeque<PathBuf>,
    large: VecDeque<PathBuf>,
    next_shard_index: usize,
    active_large_batches: usize,
    large_lanes: usize,
}

impl WarehouseWorkQueue {
    fn new(inputs: Vec<PathBuf>, large_lanes: usize) -> Self {
        let mut regular = Vec::new();
        let mut large = Vec::new();
        for input in inputs {
            let size = fs::metadata(&input)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            if size >= LARGE_INPUT_THRESHOLD_BYTES {
                large.push((input, size));
            } else {
                regular.push((input, size));
            }
        }
        regular.sort_by(|(left_path, left_size), (right_path, right_size)| {
            left_size
                .cmp(right_size)
                .then_with(|| left_path.cmp(right_path))
        });
        large.sort_by(|(left_path, left_size), (right_path, right_size)| {
            left_size
                .cmp(right_size)
                .then_with(|| left_path.cmp(right_path))
        });
        Self {
            regular: regular.into_iter().map(|(path, _)| path).collect(),
            large: large.into_iter().map(|(path, _)| path).collect(),
            next_shard_index: 0,
            active_large_batches: 0,
            large_lanes,
        }
    }

    fn take_batch(&mut self) -> Option<WarehouseWorkBatch> {
        if !self.regular.is_empty() {
            let shard_index = self.next_shard_index;
            self.next_shard_index += 1;
            let mut inputs = Vec::new();
            for _ in 0..WAREHOUSE_REGULAR_BATCH_SIZE {
                let Some(input) = self.regular.pop_front() else {
                    break;
                };
                inputs.push(input);
            }
            return Some(WarehouseWorkBatch {
                shard_index,
                inputs,
                is_large: false,
            });
        }

        if self.active_large_batches < self.large_lanes
            && let Some(input) = self.large.pop_front()
        {
            let shard_index = self.next_shard_index;
            self.next_shard_index += 1;
            self.active_large_batches += 1;
            return Some(WarehouseWorkBatch {
                shard_index,
                inputs: vec![input],
                is_large: true,
            });
        }

        None
    }

    fn complete_batch(&mut self, is_large: bool) {
        if is_large {
            self.active_large_batches = self.active_large_batches.saturating_sub(1);
        }
    }
}

fn take_warehouse_work_batch(queue: &Arc<Mutex<WarehouseWorkQueue>>) -> Option<WarehouseWorkBatch> {
    queue
        .lock()
        .expect("warehouse work queue poisoned")
        .take_batch()
}

fn complete_warehouse_work_batch(queue: &Arc<Mutex<WarehouseWorkQueue>>, is_large: bool) {
    queue
        .lock()
        .expect("warehouse work queue poisoned")
        .complete_batch(is_large);
}

fn bounded_large_lane_count(jobs: usize) -> usize {
    (jobs / 4).max(1)
}

fn merge_warehouse_shard_runs(
    options: &WarehouseParallelOptions,
    shard_run_dirs: &[PathBuf],
) -> Result<()> {
    let paths = WarehousePaths::new(&options.warehouse_dir, &options.run_id);
    let error_count = shard_run_dirs
        .iter()
        .map(|run_dir| parquet_table_row_count(run_dir, WarehouseTable::Errors))
        .try_fold(0u64, |total, count| count.map(|count| total + count))?;
    let source_count = shard_run_dirs
        .iter()
        .map(|run_dir| parquet_table_row_count(run_dir, WarehouseTable::Sources))
        .try_fold(0u64, |total, count| count.map(|count| total + count))?;
    let mut writer = WarehouseWriter::create_for_tables(
        paths.clone(),
        &[WarehouseTable::Runs, WarehouseTable::RunAnalyzers],
    )?;
    writer.append_run_analyzers(&options.analyzer_rows)?;
    for table in WarehouseTable::MERGED_DATA {
        for (shard_index, run_dir) in shard_run_dirs.iter().enumerate() {
            stage_parquet_table_part(
                &paths.staging_dir,
                *table,
                shard_index,
                &run_dir.join(table.file_name()),
            )?;
        }
    }
    writer.append_runs(&[RunRow {
        schema_version: warehouse::schema::SCHEMA_VERSION,
        run_id: options.run_id.clone(),
        created_at_utc: chrono::Utc::now().to_rfc3339(),
        input_mode: options.input_mode.to_owned(),
        input_path: options.input_path.clone(),
        source_count,
        analyzer_count: options.analyzer_rows.len() as u64,
        error_count,
    }])?;
    writer.finalize()
}

#[allow(clippy::too_many_arguments)]
fn run_analyze_aat_parallel(
    inputs: Vec<PathBuf>,
    analyzers: Vec<Arc<LoadedAnalyzer>>,
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: usize,
    collect_string_stats: bool,
) -> Result<StringStatsReport> {
    let resume_ids = if resume {
        read_resume_ids(
            analyses_output,
            errors_output,
            nway_output,
            nway_pattern_counts_output,
            output_profile,
        )?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_ids, output_profile)?;
    let partitions = partition_inputs(inputs, jobs);
    let temp_root = std::env::temp_dir().join(format!(
        "ab-morph-run-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    fs::create_dir_all(&temp_root)
        .with_context(|| format!("failed to create {}", temp_root.display()))?;

    let result = std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for (job_index, partition) in partitions.into_iter().enumerate() {
            if partition.is_empty() {
                continue;
            }
            let analyzers = analyzers.clone();
            let input_dir = temp_root.join(format!("inputs-{job_index}"));
            let output_dir = temp_root.join(format!("outputs-{job_index}"));
            handles.push(scope.spawn(move || -> Result<ShardOutput> {
                fs::create_dir_all(&input_dir)
                    .with_context(|| format!("failed to create {}", input_dir.display()))?;
                fs::create_dir_all(&output_dir)
                    .with_context(|| format!("failed to create {}", output_dir.display()))?;
                let mut shard_inputs = Vec::new();
                for input in partition {
                    let link = input_dir.join(input.file_name().ok_or_else(|| {
                        anyhow::anyhow!("missing file name for {}", input.display())
                    })?);
                    symlink_input_file(&input, &link)?;
                    shard_inputs.push(link);
                }

                let analyses = shard_output_path(&output_dir, analyses_output, "analyses");
                let comparisons = comparisons_output
                    .map(|path| shard_output_path(&output_dir, path, "comparisons"));
                let examples =
                    examples_output.map(|path| shard_output_path(&output_dir, path, "examples"));
                let nway = nway_output.map(|path| shard_output_path(&output_dir, path, "nway"));
                let nway_pattern_counts = nway_pattern_counts_output
                    .map(|path| shard_output_path(&output_dir, path, "nway-pattern-counts"));
                let errors =
                    errors_output.map(|path| shard_output_path(&output_dir, path, "errors"));
                let string_stats = run_analyze_aat_serial(
                    shard_inputs,
                    &analyzers,
                    SerialRunOptions {
                        analyses_output: Some(&analyses),
                        comparisons_output: comparisons.as_deref(),
                        errors_output: errors.as_deref(),
                        resume: false,
                        output_profile,
                        examples_output: examples.as_deref(),
                        max_examples_per_comparison,
                        nway_output: nway.as_deref(),
                        nway_pattern_counts_output: nway_pattern_counts.as_deref(),
                        max_nway_examples_per_text,
                        collect_string_stats,
                        warehouse: None,
                        progress: None,
                    },
                )?;
                Ok(ShardOutput {
                    job_index,
                    analyses,
                    comparisons,
                    examples,
                    nway,
                    nway_pattern_counts,
                    errors,
                    string_stats,
                })
            }));
        }

        let mut outputs = Vec::new();
        for handle in handles {
            outputs.push(handle.join().expect("morph worker panicked")?);
        }
        Ok::<_, anyhow::Error>(outputs)
    });

    let mut outputs = match result {
        Ok(outputs) => outputs,
        Err(error) => {
            let _ = fs::remove_dir_all(&temp_root);
            return Err(error);
        }
    };
    outputs.sort_by_key(|output| output.job_index);

    merge_shard_files(
        outputs.iter().map(|output| output.analyses.as_path()),
        analyses_output,
        resume,
    )?;
    if let Some(path) = comparisons_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.comparisons.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = examples_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.examples.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = nway_output {
        merge_shard_files(
            outputs.iter().filter_map(|output| output.nway.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = nway_pattern_counts_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.nway_pattern_counts.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = errors_output {
        merge_shard_files(
            outputs.iter().filter_map(|output| output.errors.as_deref()),
            path,
            resume,
        )?;
    }
    let mut string_stats = StringStatsReport::default();
    if collect_string_stats {
        for output in &outputs {
            string_stats.merge(&output.string_stats);
        }
    }

    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
    Ok(string_stats)
}

struct ShardOutput {
    job_index: usize,
    analyses: PathBuf,
    comparisons: Option<PathBuf>,
    examples: Option<PathBuf>,
    nway: Option<PathBuf>,
    nway_pattern_counts: Option<PathBuf>,
    errors: Option<PathBuf>,
    string_stats: StringStatsReport,
}

fn shard_output_path(output_dir: &Path, final_path: &Path, stem: &str) -> PathBuf {
    let file_name = final_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(stem);
    if file_name.ends_with(".jsonl.zst") {
        output_dir.join(format!("{stem}.jsonl.zst"))
    } else {
        output_dir.join(format!("{stem}.jsonl"))
    }
}

fn partition_inputs(inputs: Vec<PathBuf>, jobs: usize) -> Vec<Vec<PathBuf>> {
    let mut partitions = vec![Vec::new(); jobs];
    let mut partition_sizes = vec![0u64; jobs];
    let inputs = inputs
        .into_iter()
        .map(|input| {
            let size = fs::metadata(&input)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            (input, size)
        })
        .collect::<Vec<_>>();
    let (large_inputs, regular_inputs): (Vec<_>, Vec<_>) = inputs
        .into_iter()
        .partition(|(_, size)| *size >= LARGE_INPUT_THRESHOLD_BYTES);
    assign_inputs_to_partitions(&mut partitions, &mut partition_sizes, regular_inputs, jobs);
    let large_lanes = if large_inputs.is_empty() {
        jobs
    } else {
        (jobs / 4).max(1)
    };
    assign_inputs_to_partitions(
        &mut partitions,
        &mut partition_sizes,
        large_inputs,
        large_lanes,
    );
    for partition in &mut partitions {
        partition.sort_by(|left, right| {
            let left_size = fs::metadata(left)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            let right_size = fs::metadata(right)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            left_size.cmp(&right_size).then_with(|| left.cmp(right))
        });
    }
    partitions
}

fn assign_inputs_to_partitions(
    partitions: &mut [Vec<PathBuf>],
    partition_sizes: &mut [u64],
    mut inputs: Vec<(PathBuf, u64)>,
    lane_count: usize,
) {
    inputs.sort_by(|(left_path, left_size), (right_path, right_size)| {
        right_size
            .cmp(left_size)
            .then_with(|| left_path.cmp(right_path))
    });
    for (input, size) in inputs {
        let partition_index = partition_sizes
            .iter()
            .take(lane_count)
            .enumerate()
            .min_by(|(left_index, left_size), (right_index, right_size)| {
                left_size
                    .cmp(right_size)
                    .then_with(|| left_index.cmp(right_index))
            })
            .map(|(index, _)| index)
            .expect("at least one partition");
        partition_sizes[partition_index] += size;
        partitions[partition_index].push(input);
    }
}

fn symlink_input_file(input: &Path, link: &Path) -> Result<()> {
    let target = input
        .canonicalize()
        .with_context(|| format!("failed to canonicalize {}", input.display()))?;
    std::os::unix::fs::symlink(&target, link).with_context(|| {
        format!(
            "failed to symlink {} to {}",
            target.display(),
            link.display()
        )
    })
}

fn filter_resume_inputs(
    inputs: Vec<PathBuf>,
    resume_ids: &BTreeSet<String>,
    output_profile: OutputProfile,
) -> Result<Vec<PathBuf>> {
    if resume_ids.is_empty() {
        return Ok(inputs);
    }

    let mut filtered = Vec::new();
    for input in inputs {
        let should_skip = match output_profile {
            OutputProfile::Compact => {
                resume_ids.contains(&compact::source_id_from_aat_path(&input))
            }
            OutputProfile::Full => read_aat_value(&input)
                .ok()
                .and_then(|value| {
                    value
                        .get("work_id")
                        .and_then(Value::as_str)
                        .map(|text_id| resume_ids.contains(text_id))
                })
                .unwrap_or(false),
        };
        if !should_skip {
            filtered.push(input);
        }
    }
    Ok(filtered)
}

fn merge_shard_files<'a>(
    shard_paths: impl IntoIterator<Item = &'a Path>,
    output_path: &Path,
    append: bool,
) -> Result<()> {
    create_parent_dir(output_path)?;
    let mut output: Box<dyn Write> = if append {
        Box::new(
            OpenOptions::new()
                .create(true)
                .append(true)
                .open(output_path)
                .with_context(|| format!("failed to open {}", output_path.display()))?,
        )
    } else {
        Box::new(
            File::create(output_path)
                .with_context(|| format!("failed to create {}", output_path.display()))?,
        )
    };
    for shard_path in shard_paths {
        if !shard_path.exists() {
            continue;
        }
        let mut input = File::open(shard_path)
            .with_context(|| format!("failed to open {}", shard_path.display()))?;
        std::io::copy(&mut input, &mut output)?;
    }
    output.flush()?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnalyzerSpec {
    Vibrato,
    Sudachi(SudachiMode),
    #[cfg(test)]
    TestSingle,
    #[cfg(test)]
    TestSplit,
}

impl AnalyzerSpec {
    fn parse(value: &str) -> Result<Self> {
        match value {
            "vibrato" => Ok(Self::Vibrato),
            "sudachi-a" => Ok(Self::Sudachi(SudachiMode::A)),
            "sudachi-b" => Ok(Self::Sudachi(SudachiMode::B)),
            "sudachi-c" => Ok(Self::Sudachi(SudachiMode::C)),
            #[cfg(test)]
            "test:single" => Ok(Self::TestSingle),
            #[cfg(test)]
            "test:split" => Ok(Self::TestSplit),
            other => bail!("unknown analyzer `{other}`"),
        }
    }

    fn arg(self) -> &'static str {
        match self {
            Self::Vibrato => "vibrato",
            Self::Sudachi(SudachiMode::A) => "sudachi-a",
            Self::Sudachi(SudachiMode::B) => "sudachi-b",
            Self::Sudachi(SudachiMode::C) => "sudachi-c",
            #[cfg(test)]
            Self::TestSingle => "test:single",
            #[cfg(test)]
            Self::TestSplit => "test:split",
        }
    }

    fn family(self) -> &'static str {
        match self {
            Self::Vibrato => "vibrato",
            Self::Sudachi(_) => "sudachi",
            #[cfg(test)]
            Self::TestSingle | Self::TestSplit => "test",
        }
    }
}

fn parse_analyzer_specs(values: &[String]) -> Result<Vec<AnalyzerSpec>> {
    let mut seen = BTreeSet::new();
    let mut specs = Vec::new();

    for value in values {
        if seen.insert(value.clone()) {
            specs.push(AnalyzerSpec::parse(value)?);
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
            AnalyzerSpec::Vibrato => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vibrato(
                    VibratoAnalyzer::unidic_cwj_default()?,
                )));
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
            #[cfg(test)]
            AnalyzerSpec::TestSingle => {
                analyzers.push(Arc::new(LoadedAnalyzer::Test(TestAnalyzerKind::Single)));
            }
            #[cfg(test)]
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
            analyzer_arg: spec.arg().to_owned(),
            analyzer_family: spec.family().to_owned(),
        })
        .collect())
}

fn append_warehouse_nway_fact_rows(
    writer: &mut WarehouseWriter,
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
) -> Result<()> {
    warehouse::rows::visit_nway_fact_row_batches(
        run_id,
        source_id,
        source_text,
        analyses,
        10_000,
        |facts| {
            writer.append_nway_regions(&facts.regions)?;
            writer.append_nway_region_analyzers(&facts.region_analyzers)?;
            writer.append_nway_feature_diffs(&facts.feature_diffs)?;
            Ok(())
        },
    )
}

fn read_aat_value(path: &Path) -> Result<Value> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    serde_json::from_reader(file).with_context(|| format!("failed to parse {}", path.display()))
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
struct AnalysisRow {
    text_id: String,
    analyzer: String,
    analysis: Analysis,
}

#[derive(Serialize)]
struct ComparisonRow {
    text_id: String,
    from_analyzer: String,
    to_analyzer: String,
    comparison: Comparison,
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
}

impl StringStatsReport {
    pub fn record_analysis(&mut self, analysis: &Analysis) {
        self.analysis_count += 1;
        self.morpheme_count += analysis.morphemes.len();
        self.analyzer_ids.record(&analysis.analyzer);
        for morpheme in &analysis.morphemes {
            self.surfaces.record(&morpheme.surface);
            for (key, value) in morpheme.features.iter() {
                self.feature_keys.record(key.as_str());
                if let Some(value) = value {
                    self.feature_values.record(value.as_str());
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

fn write_analysis_row<W: Write + ?Sized>(
    writer: &mut W,
    output_profile: OutputProfile,
    source_id: &str,
    analysis: &Analysis,
) -> Result<()> {
    match output_profile {
        OutputProfile::Full => {
            let row = AnalysisRow {
                text_id: analysis.text_id.clone(),
                analyzer: analysis.analyzer.clone(),
                analysis: analysis.clone(),
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
                            text_id: comparison.text_id.clone(),
                            from_analyzer: comparison.from_analyzer.clone(),
                            to_analyzer: comparison.to_analyzer.clone(),
                            comparison: comparison.clone(),
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
    Sudachi(SudachiAnalyzer),
    #[cfg(test)]
    Test(TestAnalyzerKind),
}

#[cfg(test)]
#[derive(Debug, Clone, Copy)]
enum TestAnalyzerKind {
    Single,
    Split,
}

impl LoadedAnalyzer {
    fn analyzer_id(&self) -> &str {
        match self {
            Self::Vibrato(analyzer) => analyzer.analyzer_id(),
            Self::Sudachi(analyzer) => analyzer.analyzer_id(),
            #[cfg(test)]
            Self::Test(TestAnalyzerKind::Single) => "test:single",
            #[cfg(test)]
            Self::Test(TestAnalyzerKind::Split) => "test:split",
        }
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis> {
        match self {
            Self::Vibrato(analyzer) => Ok(analyzer.analyze(document)?),
            Self::Sudachi(analyzer) => Ok(analyzer.analyze(document)?),
            #[cfg(test)]
            Self::Test(kind) => Ok(test_analysis(*kind, document)),
        }
    }
}

#[cfg(test)]
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
        source_text: document.text.clone(),
        morphemes,
    }
}

#[cfg(test)]
fn test_morpheme(
    surface: String,
    byte_span: std::ops::Range<usize>,
    char_span: std::ops::Range<usize>,
) -> ab_morph_diff::Morpheme {
    let mut features = ab_morph_diff::FeatureMap::new();
    features.insert("pos1".into(), Some("名詞".into()));
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
    fn rejects_zero_jobs() {
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
        assert!(err.to_string().contains("at least 1"));
    }

    #[test]
    fn parses_analyzer_specs() {
        assert_eq!(
            AnalyzerSpec::parse("vibrato").unwrap(),
            AnalyzerSpec::Vibrato
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
            "sudachi-c".to_owned(),
            "vibrato".to_owned(),
        ])
        .unwrap();

        assert_eq!(
            specs,
            vec![AnalyzerSpec::Vibrato, AnalyzerSpec::Sudachi(SudachiMode::C)]
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
            &["vibrato".to_owned()],
            &out.join("analyses.jsonl"),
            None,
            Some(&out.join("errors.jsonl")),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            Some(&out.join("manifest.json")),
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

        let staging = warehouse_dir.join(".staging");
        if staging.exists() {
            assert!(fs::read_dir(&staging).unwrap().next().is_none());
        }

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
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(run_dir.join("runs.parquet").is_file());
        assert!(run_dir.join("sources.parquet").is_dir());
        assert!(run_dir.join("nway_regions.parquet").is_dir());
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
    fn warehouse_work_queue_batches_regular_work_before_large_work() {
        let dir = temp_dir("dynamic-regular-first");
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

        let regular = queue.take_batch().unwrap();
        let large = queue.take_batch().unwrap();

        assert!(!regular.is_large);
        assert_eq!(regular.inputs.len(), 2);
        assert!(large.is_large);
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
            &["vibrato".to_owned()],
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
            &["vibrato".to_owned()],
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
            &["vibrato".to_owned()],
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
            &["vibrato".to_owned()],
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
            &["vibrato".to_owned()],
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
            &["vibrato".to_owned()],
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
            source_text: "日日".to_owned(),
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
            source_text: "今日".to_owned(),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features: FeatureMap::new(),
            }],
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
}
