use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use anyhow::{Result, bail};
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(version, about = "Run morph analyzers over checked AAT JSON")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    AnalyzeAat {
        #[arg(long, conflicts_with = "aat_dir")]
        aat: Option<PathBuf>,
        #[arg(long, conflicts_with = "aat")]
        aat_dir: Option<PathBuf>,
        #[arg(long, required = true)]
        analyzer: Vec<String>,
        #[arg(long)]
        output_dir: Option<PathBuf>,
        #[arg(long, conflicts_with_all = [
            "output_dir",
            "analyses_output",
            "comparisons_output",
            "examples_output",
            "errors_output",
            "manifest_output",
            "nway_output",
            "nway_pattern_counts_output",
        ])]
        warehouse_dir: Option<PathBuf>,
        #[arg(long, requires = "warehouse_dir")]
        run_id: Option<String>,
        #[arg(long)]
        analyses_output: Option<PathBuf>,
        #[arg(long)]
        comparisons_output: Option<PathBuf>,
        #[arg(long)]
        examples_output: Option<PathBuf>,
        #[arg(long)]
        errors_output: Option<PathBuf>,
        #[arg(long)]
        manifest_output: Option<PathBuf>,
        #[arg(long)]
        nway_output: Option<PathBuf>,
        #[arg(long)]
        nway_pattern_counts_output: Option<PathBuf>,
        #[arg(long)]
        nway: bool,
        #[arg(long)]
        nway_pattern_counts: bool,
        #[arg(long)]
        string_stats_output: Option<PathBuf>,
        #[arg(long)]
        max_nway_examples_per_text: Option<usize>,
        #[arg(long)]
        resume: bool,
        #[arg(long, default_value_t = 1)]
        jobs: usize,
        #[arg(long, value_enum, default_value_t = ab_morph_run::OutputProfile::Full)]
        output_profile: ab_morph_run::OutputProfile,
        #[arg(long, default_value_t = 10)]
        max_examples_per_comparison: usize,
        #[arg(long)]
        progress: bool,
        #[arg(long)]
        progress_interval_seconds: Option<u64>,
    },
    SummarizeCompact {
        #[arg(long)]
        comparisons: PathBuf,
        #[arg(long, value_enum, default_value_t = SummaryGroupByArg::SourceId)]
        group_by: SummaryGroupByArg,
        #[arg(long, value_enum, default_value_t = SummarySortArg::BoundaryF1)]
        sort_by: SummarySortArg,
        #[arg(long, value_enum)]
        script_category: Option<ScriptCategoryArg>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeExamples {
        #[arg(long)]
        examples: PathBuf,
        #[arg(long, value_enum, default_value_t = SummaryGroupByArg::SourceId)]
        group_by: SummaryGroupByArg,
        #[arg(long, value_enum, default_value_t = ExampleFilterArg::All)]
        filter: ExampleFilterArg,
        #[arg(long, value_enum)]
        script_category: Option<ScriptCategoryArg>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, value_enum, default_value_t = ExampleSortArg::Examples)]
        sort_by: ExampleSortArg,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeDifferences {
        #[arg(long)]
        examples: PathBuf,
        #[arg(long, value_enum, default_value_t = DifferenceKindArg::All)]
        kind: DifferenceKindArg,
        #[arg(long)]
        feature_key: Option<String>,
        #[arg(long)]
        exclude_feature_value: Vec<String>,
        #[arg(long)]
        one_to_one_lexical_features: bool,
        #[arg(long, value_enum, default_value_t = ExampleFilterArg::All)]
        filter: ExampleFilterArg,
        #[arg(long, value_enum)]
        script_category: Option<ScriptCategoryArg>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeNway {
        #[arg(long)]
        nway: PathBuf,
        #[arg(long, value_enum, default_value_t = SummaryGroupByArg::SourceId)]
        group_by: SummaryGroupByArg,
        #[arg(long, value_enum, default_value_t = NwaySummarySortArg::RegionsWithSegmentationDisagreement)]
        sort_by: NwaySummarySortArg,
        #[arg(long, value_enum)]
        script_category: Option<ScriptCategoryArg>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeNwayPatterns {
        #[arg(long)]
        nway: Option<PathBuf>,
        #[arg(long)]
        pattern_counts: Option<PathBuf>,
        #[arg(long, value_enum, default_value_t = NwayPatternKindArg::Segmentation)]
        kind: NwayPatternKindArg,
        #[arg(long)]
        feature_key: Option<String>,
        #[arg(long)]
        exclude_feature_value: Vec<String>,
        #[arg(long, value_enum)]
        script_category: Option<ScriptCategoryArg>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    RerunFull {
        #[arg(long)]
        aat_dir: PathBuf,
        #[arg(long, required = true)]
        source_id: Vec<String>,
        #[arg(long, required = true)]
        analyzer: Vec<String>,
        #[arg(long)]
        output_dir: PathBuf,
        #[arg(long, default_value_t = 1)]
        jobs: usize,
        #[arg(long)]
        examples_output: Option<PathBuf>,
        #[arg(long, default_value_t = 10)]
        max_examples_per_comparison: usize,
        #[arg(long, value_enum, default_value_t = RerunDetailArg::Full)]
        detail: RerunDetailArg,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum SummaryGroupByArg {
    SourceId,
    TextId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum SummarySortArg {
    BoundaryF1,
    SegmentationRegions,
    LexicalSegmentationRegions,
    WhitespaceSegmentationRegions,
    FeatureDifferences,
    LexicalFeatureDifferences,
    WhitespaceFeatureDifferences,
    CoverageMismatchRegions,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum ExampleFilterArg {
    All,
    WhitespaceOnly,
    LexicalOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum ExampleSortArg {
    Examples,
    WhitespaceExamples,
    LexicalExamples,
    SegmentationExamples,
    FeatureDiffExamples,
    CoverageExamples,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum DifferenceKindArg {
    All,
    Segmentation,
    Feature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum NwaySummarySortArg {
    RegionsWithSegmentationDisagreement,
    RegionsWithFeatureDisagreement,
    RegionsWithCoverageMismatch,
    VariableBoundaryCount,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum NwayPatternKindArg {
    Segmentation,
    Feature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum ScriptCategoryArg {
    Whitespace,
    Japanese,
    LatinCode,
    Numeric,
    Mixed,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum RerunDetailArg {
    Full,
    ExamplesOnly,
}

impl SummaryGroupByArg {
    fn into_library(self) -> ab_morph_run::CompactSummaryGroupBy {
        match self {
            Self::SourceId => ab_morph_run::CompactSummaryGroupBy::SourceId,
            Self::TextId => ab_morph_run::CompactSummaryGroupBy::TextId,
        }
    }
}

impl SummarySortArg {
    fn into_library(self) -> ab_morph_run::CompactSummarySort {
        match self {
            Self::BoundaryF1 => ab_morph_run::CompactSummarySort::BoundaryF1,
            Self::SegmentationRegions => ab_morph_run::CompactSummarySort::SegmentationRegions,
            Self::LexicalSegmentationRegions => {
                ab_morph_run::CompactSummarySort::LexicalSegmentationRegions
            }
            Self::WhitespaceSegmentationRegions => {
                ab_morph_run::CompactSummarySort::WhitespaceSegmentationRegions
            }
            Self::FeatureDifferences => ab_morph_run::CompactSummarySort::FeatureDifferences,
            Self::LexicalFeatureDifferences => {
                ab_morph_run::CompactSummarySort::LexicalFeatureDifferences
            }
            Self::WhitespaceFeatureDifferences => {
                ab_morph_run::CompactSummarySort::WhitespaceFeatureDifferences
            }
            Self::CoverageMismatchRegions => {
                ab_morph_run::CompactSummarySort::CoverageMismatchRegions
            }
        }
    }
}

impl ExampleFilterArg {
    fn into_library(self) -> ab_morph_run::CompactExampleFilter {
        match self {
            Self::All => ab_morph_run::CompactExampleFilter::All,
            Self::WhitespaceOnly => ab_morph_run::CompactExampleFilter::WhitespaceOnly,
            Self::LexicalOnly => ab_morph_run::CompactExampleFilter::LexicalOnly,
        }
    }
}

impl ExampleSortArg {
    fn into_library(self) -> ab_morph_run::CompactExampleSummarySort {
        match self {
            Self::Examples => ab_morph_run::CompactExampleSummarySort::Examples,
            Self::WhitespaceExamples => ab_morph_run::CompactExampleSummarySort::WhitespaceExamples,
            Self::LexicalExamples => ab_morph_run::CompactExampleSummarySort::LexicalExamples,
            Self::SegmentationExamples => {
                ab_morph_run::CompactExampleSummarySort::SegmentationExamples
            }
            Self::FeatureDiffExamples => {
                ab_morph_run::CompactExampleSummarySort::FeatureDiffExamples
            }
            Self::CoverageExamples => ab_morph_run::CompactExampleSummarySort::CoverageExamples,
        }
    }
}

impl DifferenceKindArg {
    fn into_library(self) -> ab_morph_run::CompactDifferenceKindFilter {
        match self {
            Self::All => ab_morph_run::CompactDifferenceKindFilter::All,
            Self::Segmentation => ab_morph_run::CompactDifferenceKindFilter::Segmentation,
            Self::Feature => ab_morph_run::CompactDifferenceKindFilter::Feature,
        }
    }
}

impl NwaySummarySortArg {
    fn into_library(self) -> ab_morph_run::NwaySummarySort {
        match self {
            Self::RegionsWithSegmentationDisagreement => {
                ab_morph_run::NwaySummarySort::RegionsWithSegmentationDisagreement
            }
            Self::RegionsWithFeatureDisagreement => {
                ab_morph_run::NwaySummarySort::RegionsWithFeatureDisagreement
            }
            Self::RegionsWithCoverageMismatch => {
                ab_morph_run::NwaySummarySort::RegionsWithCoverageMismatch
            }
            Self::VariableBoundaryCount => ab_morph_run::NwaySummarySort::VariableBoundaryCount,
        }
    }
}

impl NwayPatternKindArg {
    fn into_library(self) -> ab_morph_run::NwayPatternKind {
        match self {
            Self::Segmentation => ab_morph_run::NwayPatternKind::Segmentation,
            Self::Feature => ab_morph_run::NwayPatternKind::Feature,
        }
    }
}

impl ScriptCategoryArg {
    fn into_library(self) -> ab_morph_run::ScriptCategory {
        match self {
            Self::Whitespace => ab_morph_run::ScriptCategory::Whitespace,
            Self::Japanese => ab_morph_run::ScriptCategory::Japanese,
            Self::LatinCode => ab_morph_run::ScriptCategory::LatinCode,
            Self::Numeric => ab_morph_run::ScriptCategory::Numeric,
            Self::Mixed => ab_morph_run::ScriptCategory::Mixed,
            Self::Other => ab_morph_run::ScriptCategory::Other,
        }
    }
}

fn summary_exclusions(
    source_ids: Vec<String>,
    text_ids: Vec<String>,
) -> ab_morph_run::SummaryExclusions {
    ab_morph_run::SummaryExclusions::from_values(source_ids, text_ids)
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::AnalyzeAat {
            aat,
            aat_dir,
            analyzer,
            output_dir,
            warehouse_dir,
            run_id,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
            manifest_output,
            nway_output,
            nway_pattern_counts_output,
            nway,
            nway_pattern_counts,
            string_stats_output,
            max_nway_examples_per_text,
            resume,
            jobs,
            output_profile,
            max_examples_per_comparison,
            progress,
            progress_interval_seconds,
        } => {
            validate_warehouse_cli(warehouse_dir.as_ref(), run_id.as_deref(), resume, jobs)?;
            if let Some(warehouse_dir) = warehouse_dir {
                return ab_morph_run::run_analyze_aat_warehouse(
                    aat.as_deref(),
                    aat_dir.as_deref(),
                    &analyzer,
                    &warehouse_dir,
                    run_id
                        .as_deref()
                        .expect("validate_warehouse_cli requires --run-id"),
                    jobs,
                );
            }
            let progress_enabled = progress || progress_interval_seconds.is_some();
            let progress_interval_seconds = progress_interval_seconds.unwrap_or(30).max(1);
            let input_count = if progress_enabled {
                Some(count_aat_json_inputs(aat.as_deref(), aat_dir.as_deref())?)
            } else {
                None
            };
            let start = Instant::now();
            let progress_stop = if progress_enabled {
                emit_progress_summary(start, input_count);
                Some(spawn_progress_thread(
                    start,
                    input_count,
                    std::time::Duration::from_secs(progress_interval_seconds),
                ))
            } else {
                None
            };
            let outputs = resolve_analyze_outputs(AnalyzeOutputArgs {
                output_dir,
                analyses_output,
                comparisons_output,
                examples_output,
                errors_output,
                manifest_output,
                nway_output,
                nway_pattern_counts_output,
                nway,
                nway_pattern_counts,
            })?;
            let result = ab_morph_run::run_analyze_aat_with_nway(
                aat.as_deref(),
                aat_dir.as_deref(),
                &analyzer,
                &outputs.analyses_output,
                outputs.comparisons_output.as_deref(),
                outputs.errors_output.as_deref(),
                resume,
                jobs,
                output_profile,
                outputs.examples_output.as_deref(),
                max_examples_per_comparison,
                outputs.manifest_output.as_deref(),
                outputs.nway_output.as_deref(),
                outputs.nway_pattern_counts_output.as_deref(),
                max_nway_examples_per_text,
                string_stats_output.as_deref(),
            );
            if let Some(stop) = progress_stop {
                stop.stop();
            }
            if progress_enabled {
                emit_progress_summary(start, input_count);
            }
            result
        }
        Command::SummarizeCompact {
            comparisons,
            group_by,
            sort_by,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_comparisons(
                &comparisons,
                ab_morph_run::CompactSummaryOptions {
                    group_by: group_by.into_library(),
                    sort_by: sort_by.into_library(),
                    script_category: script_category.map(ScriptCategoryArg::into_library),
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_summary_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeExamples {
            examples,
            group_by,
            filter,
            script_category,
            exclude_source_id,
            exclude_text_id,
            sort_by,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_examples(
                &examples,
                ab_morph_run::CompactExampleSummaryOptions {
                    group_by: group_by.into_library(),
                    filter: filter.into_library(),
                    script_category: script_category.map(ScriptCategoryArg::into_library),
                    sort_by: sort_by.into_library(),
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_example_summary_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeDifferences {
            examples,
            kind,
            feature_key,
            exclude_feature_value,
            one_to_one_lexical_features,
            filter,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_differences(
                &examples,
                ab_morph_run::CompactDifferenceSummaryOptions {
                    filter: filter.into_library(),
                    script_category: script_category.map(ScriptCategoryArg::into_library),
                    kind: kind.into_library(),
                    feature_key,
                    excluded_feature_values: exclude_feature_value.into_iter().collect(),
                    one_to_one_lexical_features,
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_difference_summary_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeNway {
            nway,
            group_by,
            sort_by,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_nway(
                &nway,
                ab_morph_run::NwaySummaryOptions {
                    group_by: group_by.into_library(),
                    sort_by: sort_by.into_library(),
                    script_category: script_category.map(ScriptCategoryArg::into_library),
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_nway_summary_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeNwayPatterns {
            nway,
            pattern_counts,
            kind,
            feature_key,
            exclude_feature_value,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let options = ab_morph_run::NwayPatternOptions {
                kind: kind.into_library(),
                feature_key,
                script_category: script_category.map(ScriptCategoryArg::into_library),
                excluded_feature_values: exclude_feature_value.into_iter().collect(),
                exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                limit,
            };
            let rows = match (nway.as_deref(), pattern_counts.as_deref()) {
                (Some(_), Some(_)) => {
                    anyhow::bail!("provide only one of --nway or --pattern-counts")
                }
                (Some(path), None) => ab_morph_run::summarize_nway_patterns(path, options)?,
                (None, Some(path)) => ab_morph_run::summarize_nway_pattern_counts(path, options)?,
                (None, None) => anyhow::bail!("provide one of --nway or --pattern-counts"),
            };
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_nway_pattern_table(&rows);
            }
            Ok(())
        }
        Command::RerunFull {
            aat_dir,
            source_id,
            analyzer,
            output_dir,
            jobs,
            examples_output,
            max_examples_per_comparison,
            detail,
        } => run_rerun_full(
            &aat_dir,
            &source_id,
            &analyzer,
            &output_dir,
            jobs,
            examples_output.as_deref(),
            max_examples_per_comparison,
            detail,
        ),
    }
}

#[derive(Debug)]
struct AnalyzeOutputArgs {
    output_dir: Option<PathBuf>,
    analyses_output: Option<PathBuf>,
    comparisons_output: Option<PathBuf>,
    examples_output: Option<PathBuf>,
    errors_output: Option<PathBuf>,
    manifest_output: Option<PathBuf>,
    nway_output: Option<PathBuf>,
    nway_pattern_counts_output: Option<PathBuf>,
    nway: bool,
    nway_pattern_counts: bool,
}

#[derive(Debug, PartialEq, Eq)]
struct AnalyzeOutputPaths {
    analyses_output: PathBuf,
    comparisons_output: Option<PathBuf>,
    examples_output: Option<PathBuf>,
    errors_output: Option<PathBuf>,
    manifest_output: Option<PathBuf>,
    nway_output: Option<PathBuf>,
    nway_pattern_counts_output: Option<PathBuf>,
}

fn resolve_analyze_outputs(args: AnalyzeOutputArgs) -> Result<AnalyzeOutputPaths> {
    let Some(output_dir) = args.output_dir else {
        if args.nway && args.nway_output.is_none() {
            bail!("--nway requires --output-dir or --nway-output");
        }
        if args.nway_pattern_counts && args.nway_pattern_counts_output.is_none() {
            bail!("--nway-pattern-counts requires --output-dir or --nway-pattern-counts-output");
        }
        let Some(analyses_output) = args.analyses_output else {
            bail!("provide --analyses-output or --output-dir");
        };
        return Ok(AnalyzeOutputPaths {
            analyses_output,
            comparisons_output: args.comparisons_output,
            examples_output: args.examples_output,
            errors_output: args.errors_output,
            manifest_output: args.manifest_output,
            nway_output: args.nway_output,
            nway_pattern_counts_output: args.nway_pattern_counts_output,
        });
    };

    Ok(AnalyzeOutputPaths {
        analyses_output: args
            .analyses_output
            .unwrap_or_else(|| output_dir.join("analyses.jsonl.zst")),
        comparisons_output: Some(
            args.comparisons_output
                .unwrap_or_else(|| output_dir.join("comparisons.jsonl.zst")),
        ),
        examples_output: Some(
            args.examples_output
                .unwrap_or_else(|| output_dir.join("examples.jsonl.zst")),
        ),
        errors_output: Some(
            args.errors_output
                .unwrap_or_else(|| output_dir.join("errors.jsonl.zst")),
        ),
        manifest_output: Some(
            args.manifest_output
                .unwrap_or_else(|| output_dir.join("manifest.json")),
        ),
        nway_output: args
            .nway_output
            .or_else(|| args.nway.then(|| output_dir.join("nway.jsonl.zst"))),
        nway_pattern_counts_output: args.nway_pattern_counts_output.or_else(|| {
            args.nway_pattern_counts
                .then(|| output_dir.join("nway-pattern-counts.jsonl.zst"))
        }),
    })
}

fn validate_warehouse_cli(
    warehouse_dir: Option<&PathBuf>,
    run_id: Option<&str>,
    resume: bool,
    jobs: usize,
) -> Result<()> {
    if warehouse_dir.is_none() {
        return Ok(());
    }
    if run_id.is_none() {
        bail!("--warehouse-dir requires --run-id");
    }
    if resume {
        bail!("warehouse mode does not support --resume in phase 1");
    }
    if jobs != 1 {
        bail!("warehouse mode requires --jobs 1 in phase 1");
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn run_rerun_full(
    aat_dir: &Path,
    source_ids: &[String],
    analyzer: &[String],
    output_dir: &Path,
    jobs: usize,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    detail: RerunDetailArg,
) -> Result<()> {
    let inputs = ab_morph_run::resolve_source_id_aat_paths(aat_dir, source_ids)?;
    let output_profile = match detail {
        RerunDetailArg::Full => ab_morph_run::OutputProfile::Full,
        RerunDetailArg::ExamplesOnly => ab_morph_run::OutputProfile::Compact,
    };
    let comparisons_output = match detail {
        RerunDetailArg::Full => Some(output_dir.join("comparisons.jsonl")),
        RerunDetailArg::ExamplesOnly => None,
    };
    let default_examples_output = match (detail, examples_output) {
        (RerunDetailArg::ExamplesOnly, None) => Some(output_dir.join("examples.jsonl")),
        _ => None,
    };
    let examples_output = examples_output.or(default_examples_output.as_deref());
    ab_morph_run::run_analyze_aat_selected(
        inputs,
        "aat_dir",
        &aat_dir.display().to_string(),
        analyzer,
        &output_dir.join("analyses.jsonl"),
        comparisons_output.as_deref(),
        Some(&output_dir.join("errors.jsonl")),
        false,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        Some(&output_dir.join("manifest.json")),
    )
}

struct ProgressStop {
    stop: Option<std::sync::mpsc::Sender<()>>,
    handle: Option<std::thread::JoinHandle<()>>,
}

impl ProgressStop {
    fn stop(mut self) {
        if let Some(stop) = self.stop.take() {
            let _ = stop.send(());
        }
        if let Some(handle) = self.handle.take() {
            let _ = handle.join();
        }
    }
}

fn spawn_progress_thread(
    start: Instant,
    input_count: Option<usize>,
    interval: std::time::Duration,
) -> ProgressStop {
    let (stop, stopped) = std::sync::mpsc::channel();
    let handle = std::thread::spawn(move || {
        while stopped.recv_timeout(interval).is_err() {
            emit_progress_summary(start, input_count);
        }
    });
    ProgressStop {
        stop: Some(stop),
        handle: Some(handle),
    }
}

fn print_summary_table(rows: &[ab_morph_run::CompactSummaryRow]) {
    println!(
        "key\tsource_ids\ttext_ids\tscript_categories\tcomparisons\tworst_boundary_f1\ttotal_segmentation_regions\ttotal_whitespace_segmentation_regions\ttotal_lexical_segmentation_regions\ttotal_feature_difference_regions\ttotal_whitespace_feature_difference_regions\ttotal_lexical_feature_difference_regions\ttotal_coverage_mismatch_regions"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
            row.script_categories.join(","),
            row.comparisons,
            row.worst_boundary_f1
                .map(|value| value.to_string())
                .unwrap_or_else(|| "null".to_owned()),
            row.total_segmentation_regions,
            row.total_whitespace_segmentation_regions,
            row.total_lexical_segmentation_regions,
            row.total_feature_difference_regions,
            row.total_whitespace_feature_difference_regions,
            row.total_lexical_feature_difference_regions,
            row.total_coverage_mismatch_regions,
        );
    }
}

fn print_example_summary_table(rows: &[ab_morph_run::CompactExampleSummaryRow]) {
    println!(
        "key\tsource_ids\ttext_ids\tscript_categories\texamples\twhitespace_examples\tlexical_examples\tsegmentation_examples\tfeature_diff_examples\tcoverage_examples"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
            row.script_categories.join(","),
            row.examples,
            row.whitespace_examples,
            row.lexical_examples,
            row.segmentation_examples,
            row.feature_diff_examples,
            row.coverage_examples,
        );
    }
}

fn print_difference_summary_table(rows: &[ab_morph_run::CompactDifferenceSummaryRow]) {
    println!(
        "kind\tfrom_analyzer\tto_analyzer\texamples\tsource_count\ttext_count\tsample_source_ids\tsample_text_ids\tscript_categories\tregion_kind\tfrom_surfaces\tto_surfaces\tfeature_key\tfeature_from\tfeature_to"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.kind,
            row.from_analyzer,
            row.to_analyzer,
            row.examples,
            row.source_ids.len(),
            row.text_ids.len(),
            sample_values(&row.source_ids, 5),
            sample_values(&row.text_ids, 5),
            row.script_categories.join(","),
            row.region_kind.as_deref().unwrap_or(""),
            row.from_surfaces.join(" + "),
            row.to_surfaces.join(" + "),
            row.feature_key.as_deref().unwrap_or(""),
            row.feature_from.as_deref().unwrap_or(""),
            row.feature_to.as_deref().unwrap_or(""),
        );
    }
}

fn print_nway_summary_table(rows: &[ab_morph_run::NwaySummaryRow]) {
    println!(
        "key\tsource_ids\ttext_ids\tscript_categories\trows\tanalyzer_count\tregions\tagreement_regions\tregions_with_feature_disagreement\tregions_with_segmentation_disagreement\tregions_with_coverage_mismatch\tvariable_boundary_count"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
            row.script_categories.join(","),
            row.rows,
            row.analyzer_count,
            row.regions,
            row.agreement_regions,
            row.regions_with_feature_disagreement,
            row.regions_with_segmentation_disagreement,
            row.regions_with_coverage_mismatch,
            row.variable_boundary_count,
        );
    }
}

fn print_nway_pattern_table(rows: &[ab_morph_run::NwayPatternRow]) {
    println!(
        "kind\texamples\tsource_count\ttext_count\tsample_source_ids\tsample_text_ids\tscript_categories\tpattern"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.kind,
            row.examples,
            row.source_ids.len(),
            row.text_ids.len(),
            sample_values(&row.source_ids, 5),
            sample_values(&row.text_ids, 5),
            row.script_categories.join(","),
            row.pattern,
        );
    }
}

fn sample_values(values: &[String], limit: usize) -> String {
    let mut sample = values.iter().take(limit).cloned().collect::<Vec<_>>();
    if values.len() > limit {
        sample.push(format!("...+{}", values.len() - limit));
    }
    sample.join(",")
}

fn emit_progress_summary(start: Instant, input_count: Option<usize>) {
    let elapsed_seconds = start.elapsed().as_secs();
    match read_linux_memory_kb() {
        Some(memory) => eprintln!(
            "ab-morph-run: elapsed_seconds={elapsed_seconds} inputs={} rss_kb={} pss_kb={}",
            input_count
                .map(|count| count.to_string())
                .unwrap_or_else(|| "unknown".to_owned()),
            memory.rss_kb,
            memory
                .pss_kb
                .map(|value| value.to_string())
                .unwrap_or_else(|| "unknown".to_owned())
        ),
        None => eprintln!(
            "ab-morph-run: elapsed_seconds={elapsed_seconds} inputs={} rss_kb=unknown pss_kb=unknown",
            input_count
                .map(|count| count.to_string())
                .unwrap_or_else(|| "unknown".to_owned())
        ),
    }
}

fn count_aat_json_inputs(aat: Option<&Path>, aat_dir: Option<&Path>) -> Result<usize> {
    match (aat, aat_dir) {
        (Some(_), None) => Ok(1),
        (None, Some(dir)) => {
            let mut count = 0usize;
            count_json_files_recursive(dir, &mut count)?;
            Ok(count)
        }
        _ => Ok(0),
    }
}

fn count_json_files_recursive(dir: &Path, count: &mut usize) -> Result<()> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            count_json_files_recursive(&path, count)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
            *count += 1;
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MemorySnapshot {
    rss_kb: u64,
    pss_kb: Option<u64>,
}

fn read_linux_memory_kb() -> Option<MemorySnapshot> {
    let smaps = fs::read_to_string("/proc/self/smaps_rollup").ok()?;
    parse_smaps_rollup(&smaps)
}

fn parse_smaps_rollup(text: &str) -> Option<MemorySnapshot> {
    let mut rss_kb = None;
    let mut pss_kb = None;
    for line in text.lines() {
        if let Some(value) = parse_kb_line(line, "Rss:") {
            rss_kb = Some(value);
        } else if let Some(value) = parse_kb_line(line, "Pss:") {
            pss_kb = Some(value);
        }
    }
    rss_kb.map(|rss_kb| MemorySnapshot { rss_kb, pss_kb })
}

fn parse_kb_line(line: &str, key: &str) -> Option<u64> {
    let rest = line.strip_prefix(key)?;
    rest.split_whitespace().next()?.parse().ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_smaps_rollup_memory_values() {
        let snapshot =
            parse_smaps_rollup("Rss:             6390588 kB\nPss:             6388652 kB\n")
                .unwrap();

        assert_eq!(
            snapshot,
            MemorySnapshot {
                rss_kb: 6_390_588,
                pss_kb: Some(6_388_652),
            }
        );
    }

    #[test]
    fn parses_summarize_compact_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-compact",
            "--comparisons",
            "comparisons.jsonl.zst",
            "--group-by",
            "text-id",
            "--sort-by",
            "lexical-segmentation-regions",
            "--exclude-text-id",
            "JISTABLE",
            "--limit",
            "25",
            "--json",
        ]);

        let Command::SummarizeCompact {
            comparisons,
            group_by,
            sort_by,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } = args.command
        else {
            panic!("expected summarize-compact command");
        };

        assert_eq!(comparisons, PathBuf::from("comparisons.jsonl.zst"));
        assert_eq!(group_by, SummaryGroupByArg::TextId);
        assert_eq!(sort_by, SummarySortArg::LexicalSegmentationRegions);
        assert_eq!(script_category, None);
        assert!(exclude_source_id.is_empty());
        assert_eq!(exclude_text_id, vec!["JISTABLE"]);
        assert_eq!(limit, 25);
        assert!(json);
    }

    #[test]
    fn parses_summarize_examples_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-examples",
            "--examples",
            "examples.jsonl.zst",
            "--group-by",
            "text-id",
            "--filter",
            "whitespace-only",
            "--sort-by",
            "whitespace-examples",
            "--exclude-source-id",
            "src-a",
            "--limit",
            "30",
            "--json",
        ]);

        let Command::SummarizeExamples {
            examples,
            group_by,
            filter,
            script_category,
            exclude_source_id,
            exclude_text_id,
            sort_by,
            limit,
            json,
        } = args.command
        else {
            panic!("expected summarize-examples command");
        };

        assert_eq!(examples, PathBuf::from("examples.jsonl.zst"));
        assert_eq!(group_by, SummaryGroupByArg::TextId);
        assert_eq!(filter, ExampleFilterArg::WhitespaceOnly);
        assert_eq!(script_category, None);
        assert_eq!(exclude_source_id, vec!["src-a"]);
        assert!(exclude_text_id.is_empty());
        assert_eq!(sort_by, ExampleSortArg::WhitespaceExamples);
        assert_eq!(limit, 30);
        assert!(json);
    }

    #[test]
    fn parses_summarize_differences_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-differences",
            "--examples",
            "examples.jsonl.zst",
            "--kind",
            "feature",
            "--feature-key",
            "pos1",
            "--exclude-feature-value",
            "空白",
            "--one-to-one-lexical-features",
            "--filter",
            "lexical-only",
            "--script-category",
            "japanese",
            "--exclude-text-id",
            "JISTABLE",
            "--limit",
            "50",
            "--json",
        ]);

        let Command::SummarizeDifferences {
            examples,
            kind,
            feature_key,
            exclude_feature_value,
            one_to_one_lexical_features,
            filter,
            script_category,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } = args.command
        else {
            panic!("expected summarize-differences command");
        };

        assert_eq!(examples, PathBuf::from("examples.jsonl.zst"));
        assert_eq!(kind, DifferenceKindArg::Feature);
        assert_eq!(feature_key, Some("pos1".to_owned()));
        assert_eq!(exclude_feature_value, vec!["空白"]);
        assert!(one_to_one_lexical_features);
        assert_eq!(filter, ExampleFilterArg::LexicalOnly);
        assert_eq!(script_category, Some(ScriptCategoryArg::Japanese));
        assert!(exclude_source_id.is_empty());
        assert_eq!(exclude_text_id, vec!["JISTABLE"]);
        assert_eq!(limit, 50);
        assert!(json);
    }

    #[test]
    fn parses_script_category_filters() {
        let compact = Args::parse_from([
            "ab-morph-run",
            "summarize-compact",
            "--comparisons",
            "comparisons.jsonl.zst",
            "--script-category",
            "japanese",
        ]);
        let Command::SummarizeCompact {
            script_category, ..
        } = compact.command
        else {
            panic!("expected summarize-compact command");
        };
        assert_eq!(script_category, Some(ScriptCategoryArg::Japanese));

        let examples = Args::parse_from([
            "ab-morph-run",
            "summarize-examples",
            "--examples",
            "examples.jsonl.zst",
            "--script-category",
            "latin-code",
        ]);
        let Command::SummarizeExamples {
            script_category, ..
        } = examples.command
        else {
            panic!("expected summarize-examples command");
        };
        assert_eq!(script_category, Some(ScriptCategoryArg::LatinCode));
    }

    #[test]
    fn parses_rerun_full_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "rerun-full",
            "--aat-dir",
            "aats",
            "--source-id",
            "src-a",
            "--source-id",
            "src-b",
            "--analyzer",
            "vibrato",
            "--analyzer",
            "sudachi-c",
            "--output-dir",
            "full-out",
            "--jobs",
            "2",
            "--examples-output",
            "examples.jsonl",
            "--max-examples-per-comparison",
            "50",
            "--detail",
            "examples-only",
        ]);

        let Command::RerunFull {
            aat_dir,
            source_id,
            analyzer,
            output_dir,
            jobs,
            examples_output,
            max_examples_per_comparison,
            detail,
        } = args.command
        else {
            panic!("expected rerun-full command");
        };

        assert_eq!(aat_dir, PathBuf::from("aats"));
        assert_eq!(source_id, vec!["src-a".to_owned(), "src-b".to_owned()]);
        assert_eq!(analyzer, vec!["vibrato".to_owned(), "sudachi-c".to_owned()]);
        assert_eq!(output_dir, PathBuf::from("full-out"));
        assert_eq!(jobs, 2);
        assert_eq!(examples_output, Some(PathBuf::from("examples.jsonl")));
        assert_eq!(max_examples_per_comparison, 50);
        assert_eq!(detail, RerunDetailArg::ExamplesOnly);
    }

    #[test]
    fn parses_progress_interval_seconds() {
        let args = Args::parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat",
            "one.json",
            "--analyzer",
            "vibrato",
            "--analyses-output",
            "analyses.jsonl",
            "--progress-interval-seconds",
            "5",
        ]);

        let Command::AnalyzeAat {
            progress,
            progress_interval_seconds,
            ..
        } = args.command
        else {
            panic!("expected analyze-aat command");
        };

        assert!(!progress);
        assert_eq!(progress_interval_seconds, Some(5));
    }

    #[test]
    fn parses_nway_output_flags() {
        let args = Args::parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat",
            "one.json",
            "--analyzer",
            "vibrato",
            "--analyzer",
            "sudachi-a",
            "--analyzer",
            "sudachi-c",
            "--output-profile",
            "compact",
            "--analyses-output",
            "analyses.jsonl.zst",
            "--nway-output",
            "nway.jsonl.zst",
            "--nway-pattern-counts-output",
            "nway-pattern-counts.jsonl.zst",
            "--max-nway-examples-per-text",
            "25",
        ]);

        let Command::AnalyzeAat {
            nway_output,
            nway_pattern_counts_output,
            max_nway_examples_per_text,
            ..
        } = args.command
        else {
            panic!("expected analyze-aat");
        };

        assert_eq!(nway_output, Some(PathBuf::from("nway.jsonl.zst")));
        assert_eq!(
            nway_pattern_counts_output,
            Some(PathBuf::from("nway-pattern-counts.jsonl.zst"))
        );
        assert_eq!(max_nway_examples_per_text, Some(25));
    }

    #[test]
    fn parses_output_dir_with_standard_optional_outputs() {
        let args = Args::parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat",
            "one.json",
            "--analyzer",
            "vibrato",
            "--output-profile",
            "compact",
            "--output-dir",
            "scratch/out",
            "--nway",
            "--nway-pattern-counts",
        ]);

        let Command::AnalyzeAat {
            analyses_output,
            output_dir,
            nway,
            nway_pattern_counts,
            ..
        } = args.command
        else {
            panic!("expected analyze-aat");
        };

        assert_eq!(analyses_output, None);
        assert_eq!(output_dir, Some(PathBuf::from("scratch/out")));
        assert!(nway);
        assert!(nway_pattern_counts);
    }

    #[test]
    fn parses_warehouse_mode_without_jsonl_outputs() {
        let args = Args::try_parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat-dir",
            "scratch/aats",
            "--analyzer",
            "vibrato",
            "--warehouse-dir",
            "scratch/morph-warehouse",
            "--run-id",
            "smoke-2026-05-01",
        ])
        .unwrap();

        let Command::AnalyzeAat {
            warehouse_dir,
            run_id,
            output_dir,
            analyses_output,
            ..
        } = args.command
        else {
            panic!("expected analyze-aat");
        };

        assert_eq!(
            warehouse_dir,
            Some(PathBuf::from("scratch/morph-warehouse"))
        );
        assert_eq!(run_id, Some("smoke-2026-05-01".to_owned()));
        assert_eq!(output_dir, None);
        assert_eq!(analyses_output, None);
    }

    #[test]
    fn rejects_warehouse_with_jsonl_output_dir() {
        let err = Args::try_parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat-dir",
            "scratch/aats",
            "--analyzer",
            "vibrato",
            "--warehouse-dir",
            "scratch/morph-warehouse",
            "--run-id",
            "smoke-2026-05-01",
            "--output-dir",
            "scratch/jsonl",
        ])
        .unwrap_err();

        assert_eq!(err.kind(), clap::error::ErrorKind::ArgumentConflict);
    }

    #[test]
    fn warehouse_validation_rejects_resume_and_parallel_jobs() {
        let err = validate_warehouse_cli(
            Some(&PathBuf::from("scratch/warehouse")),
            Some("run-a"),
            true,
            1,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("does not support --resume"));

        let err = validate_warehouse_cli(
            Some(&PathBuf::from("scratch/warehouse")),
            Some("run-a"),
            false,
            2,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("requires --jobs 1"));
    }

    #[test]
    fn output_dir_resolves_standard_artifact_paths() {
        let paths = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: Some(PathBuf::from("scratch/out")),
            analyses_output: None,
            comparisons_output: None,
            examples_output: None,
            errors_output: None,
            manifest_output: None,
            nway_output: None,
            nway_pattern_counts_output: None,
            nway: true,
            nway_pattern_counts: true,
        })
        .unwrap();

        assert_eq!(
            paths,
            AnalyzeOutputPaths {
                analyses_output: PathBuf::from("scratch/out/analyses.jsonl.zst"),
                comparisons_output: Some(PathBuf::from("scratch/out/comparisons.jsonl.zst")),
                examples_output: Some(PathBuf::from("scratch/out/examples.jsonl.zst")),
                errors_output: Some(PathBuf::from("scratch/out/errors.jsonl.zst")),
                manifest_output: Some(PathBuf::from("scratch/out/manifest.json")),
                nway_output: Some(PathBuf::from("scratch/out/nway.jsonl.zst")),
                nway_pattern_counts_output: Some(PathBuf::from(
                    "scratch/out/nway-pattern-counts.jsonl.zst"
                )),
            }
        );
    }

    #[test]
    fn output_dir_keeps_explicit_output_overrides() {
        let paths = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: Some(PathBuf::from("scratch/out")),
            analyses_output: Some(PathBuf::from("custom/analyses.jsonl")),
            comparisons_output: None,
            examples_output: None,
            errors_output: None,
            manifest_output: None,
            nway_output: Some(PathBuf::from("custom/nway.jsonl")),
            nway_pattern_counts_output: None,
            nway: false,
            nway_pattern_counts: true,
        })
        .unwrap();

        assert_eq!(
            paths.analyses_output,
            PathBuf::from("custom/analyses.jsonl")
        );
        assert_eq!(paths.nway_output, Some(PathBuf::from("custom/nway.jsonl")));
        assert_eq!(
            paths.nway_pattern_counts_output,
            Some(PathBuf::from("scratch/out/nway-pattern-counts.jsonl.zst"))
        );
    }

    #[test]
    fn explicit_output_mode_requires_analyses_output() {
        let error = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: None,
            analyses_output: None,
            comparisons_output: None,
            examples_output: None,
            errors_output: None,
            manifest_output: None,
            nway_output: None,
            nway_pattern_counts_output: None,
            nway: false,
            nway_pattern_counts: false,
        })
        .unwrap_err();

        assert!(error.to_string().contains("--analyses-output"));
    }

    #[test]
    fn parses_summarize_nway_patterns_from_pattern_counts() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-nway-patterns",
            "--pattern-counts",
            "nway-pattern-counts.jsonl.zst",
            "--kind",
            "feature",
            "--feature-key",
            "pos1",
        ]);

        let Command::SummarizeNwayPatterns {
            nway,
            pattern_counts,
            kind,
            feature_key,
            ..
        } = args.command
        else {
            panic!("expected summarize-nway-patterns");
        };

        assert_eq!(nway, None);
        assert_eq!(
            pattern_counts,
            Some(PathBuf::from("nway-pattern-counts.jsonl.zst"))
        );
        assert_eq!(kind, NwayPatternKindArg::Feature);
        assert_eq!(feature_key, Some("pos1".to_owned()));
    }

    #[test]
    fn parses_string_stats_output_flag() {
        let args = Args::parse_from([
            "ab-morph-run",
            "analyze-aat",
            "--aat",
            "one.json",
            "--analyzer",
            "vibrato",
            "--analyses-output",
            "analyses.jsonl",
            "--string-stats-output",
            "string-stats.json",
        ]);

        let Command::AnalyzeAat {
            string_stats_output,
            ..
        } = args.command
        else {
            panic!("expected analyze-aat");
        };

        assert_eq!(
            string_stats_output,
            Some(PathBuf::from("string-stats.json"))
        );
    }

    #[test]
    fn sample_values_truncates_large_id_lists() {
        let values = ["a", "b", "c", "d"]
            .into_iter()
            .map(str::to_owned)
            .collect::<Vec<_>>();

        assert_eq!(sample_values(&values, 2), "a,b,...+2");
    }

    #[test]
    fn rerun_full_writes_manifest_with_original_aat_dir() {
        const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;
        let dir = temp_dir("rerun-full");
        let aat_dir = dir.join("aats");
        let out = dir.join("out");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::create_dir_all(&out).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        run_rerun_full(
            &aat_dir,
            &["source-a".to_owned()],
            &["vibrato".to_owned()],
            &out,
            1,
            None,
            10,
            RerunDetailArg::Full,
        )
        .unwrap();

        let manifest: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(out.join("manifest.json")).unwrap()).unwrap();
        assert_eq!(manifest["input_path"], aat_dir.display().to_string());
        assert!(out.join("analyses.jsonl").exists());
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn rerun_full_examples_only_skips_full_comparisons() {
        const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;
        let dir = temp_dir("rerun-full-examples-only");
        let aat_dir = dir.join("aats");
        let out = dir.join("out");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::create_dir_all(&out).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        run_rerun_full(
            &aat_dir,
            &["source-a".to_owned()],
            &["vibrato".to_owned()],
            &out,
            1,
            None,
            10,
            RerunDetailArg::ExamplesOnly,
        )
        .unwrap();

        assert!(out.join("analyses.jsonl").exists());
        assert!(out.join("examples.jsonl").exists());
        assert!(!out.join("comparisons.jsonl").exists());
        let analyses = fs::read_to_string(out.join("analyses.jsonl")).unwrap();
        assert!(analyses.contains("\"morpheme_count\""));
        assert!(!analyses.contains("\"source_text\""));
        let manifest: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(out.join("manifest.json")).unwrap()).unwrap();
        assert_eq!(manifest["output_profile"], "compact");

        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> PathBuf {
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-main-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
