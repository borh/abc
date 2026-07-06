use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(version, about = "Run morph analyzers over checked AAT JSON")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[allow(clippy::large_enum_variant)]
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
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseProfile::Full)]
        warehouse_profile: ab_morph_run::WarehouseProfile,
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
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::OrthoDetectMode::Off,
            help = "Orthographic-detection layer for pre-war Japanese text (kata→hira normalization).

[off] No normalization (default).
[heuristic] v1 char-cascade detector. Default threshold 0.40 (tuned Phase 2.5);
recall 0.939 on a 300-sentence LLM-labeled evaluation set (>= 0.85 floor).
See reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md.
[ml] Logistic-regression detector on character features. Stable (not default):
5-fold CV mean recall 0.959 on the 300-record LLM-labeled set. Requires
--ortho-ml-model. CAVEAT: the validation set is LLM-labeled (single-annotator,
not human ground truth); real-world recall on unseen authors/eras is unverified.
model_hash proves byte identity only, not training provenance. Do not promote
to default or delete the heuristic path without a human-annotated gold set."
        )]
        ortho_detect: ab_morph_run::OrthoDetectMode,
        #[arg(
            long = "ortho-ml-model",
            value_name = "PATH",
            help = "Path to a trained ML model file (bincode MlModel). Required for --ortho-detect=ml.
The model carries no training-provenance metadata; verify its source before trusting output."
        )]
        ortho_ml_model: Option<PathBuf>,
    },
    SummarizeWarehouseNway {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long, value_enum, default_value_t = ab_morph_run::CompactSummaryGroupBy::SourceId)]
        group_by: ab_morph_run::CompactSummaryGroupBy,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::NwaySummarySort::RegionsWithSegmentationDisagreement
        )]
        sort_by: ab_morph_run::NwaySummarySort,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeWarehousePairwise {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::WarehousePairwiseSort::SegmentationRegions
        )]
        sort_by: ab_morph_run::WarehousePairwiseSort,
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseTextFilter::All)]
        filter: ab_morph_run::WarehouseTextFilter,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeWarehousePatterns {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long, value_enum, default_value_t = ab_morph_run::NwayPatternKind::Segmentation)]
        kind: ab_morph_run::NwayPatternKind,
        #[arg(long)]
        feature_key: Option<String>,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::WarehouseFeatureProfile::Raw
        )]
        feature_profile: ab_morph_run::WarehouseFeatureProfile,
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseTextFilter::All)]
        filter: ab_morph_run::WarehouseTextFilter,
        #[arg(long)]
        exclude_feature_value: Vec<String>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    MaterializeWarehouseFeaturePatterns {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long)]
        feature_key: Option<String>,
    },
    SummarizeWarehousePatternExamples {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long, value_enum, default_value_t = ab_morph_run::NwayPatternKind::Segmentation)]
        kind: ab_morph_run::NwayPatternKind,
        #[arg(long)]
        pattern: String,
        #[arg(long)]
        feature_key: Option<String>,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::WarehouseFeatureProfile::Raw
        )]
        feature_profile: ab_morph_run::WarehouseFeatureProfile,
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseTextFilter::All)]
        filter: ab_morph_run::WarehouseTextFilter,
        #[arg(long)]
        exclude_feature_value: Vec<String>,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeWarehouseTriage {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long)]
        output_dir: PathBuf,
        #[arg(long, default_value_t = 50)]
        limit: usize,
    },
    SummarizeWarehouseRegions {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseRegionKind::All)]
        kind: ab_morph_run::WarehouseRegionKind,
        #[arg(long, value_enum, default_value_t = ab_morph_run::WarehouseTextFilter::All)]
        filter: ab_morph_run::WarehouseTextFilter,
        #[arg(long)]
        exclude_source_id: Vec<String>,
        #[arg(long)]
        exclude_text_id: Vec<String>,
        #[arg(long, default_value_t = 20)]
        limit: usize,
        #[arg(long)]
        json: bool,
    },
    SummarizeWarehouseInteresting {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long, default_value_t = 50)]
        limit: usize,
        #[arg(long, value_enum, default_value_t = ab_morph_run::InterestingOutputFormat::Table)]
        format: ab_morph_run::InterestingOutputFormat,
        #[arg(long, value_enum, default_value_t = ab_morph_run::InterestingTextFilter::All)]
        filter: ab_morph_run::InterestingTextFilter,
        #[arg(long)]
        explain: Option<String>,
        #[arg(long, default_value_t = 10)]
        anomalies: usize,
        #[arg(long, value_enum, default_value_t = ab_morph_run::InterestingEngine::Auto)]
        engine: ab_morph_run::InterestingEngine,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::WarehouseFeatureProfile::Core
        )]
        feature_profile: ab_morph_run::WarehouseFeatureProfile,
        #[arg(long, value_enum, default_value_t = ab_morph_run::RankScope::WithinKind)]
        rank_scope: ab_morph_run::RankScope,
        #[arg(long, default_value = "rank-floor")]
        lambda_missing_policy: ab_morph_run::LambdaMissingPolicy,
        #[arg(long, default_value_t = 5.0)]
        anomaly_w_cov: f64,
        #[arg(long)]
        output: Option<PathBuf>,
        #[arg(long)]
        force: bool,
    },
    SummarizeWarehouseErrors {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(
            long,
            value_enum,
            default_value_t = ab_morph_run::WarehouseErrorGroupBy::ErrorCode
        )]
        group_by: ab_morph_run::WarehouseErrorGroupBy,
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
    ImportAozoraMetadata {
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long)]
        from: PathBuf,
        #[arg(long)]
        force: bool,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
enum RerunDetailArg {
    Full,
    ExamplesOnly,
}

fn summary_exclusions(
    source_ids: Vec<String>,
    text_ids: Vec<String>,
) -> ab_morph_run::SummaryExclusions {
    ab_morph_run::SummaryExclusions::from_values(source_ids, text_ids)
}

fn validate_warehouse_feature_profile(
    kind: ab_morph_run::NwayPatternKind,
    feature_profile: ab_morph_run::WarehouseFeatureProfile,
    feature_key: Option<&str>,
) -> Result<()> {
    if kind == ab_morph_run::NwayPatternKind::Feature
        && feature_profile == ab_morph_run::WarehouseFeatureProfile::Schema
        && feature_key.is_none()
    {
        bail!(
            "--feature-profile schema requires --feature-key until warehouse feature-pattern aggregates are materialized"
        );
    }
    Ok(())
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
            warehouse_profile,
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
            ortho_detect,
            ortho_ml_model,
        } => {
            validate_warehouse_cli(warehouse_dir.as_ref(), run_id.as_deref(), resume, jobs)?;
            if ortho_detect == ab_morph_run::OrthoDetectMode::Ml && ortho_ml_model.is_none() {
                bail!("--ortho-ml-model PATH is required when --ortho-detect=ml");
            }
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
                    warehouse_profile,
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
                ortho_detect,
                ortho_ml_model,
            );
            if let Some(stop) = progress_stop {
                stop.stop();
            }
            if progress_enabled {
                emit_progress_summary(start, input_count);
            }
            result
        }
        Command::SummarizeWarehouseNway {
            run_dir,
            group_by,
            sort_by,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_warehouse_nway(
                &run_dir,
                ab_morph_run::NwaySummaryOptions {
                    group_by,
                    sort_by,
                    script_category: None,
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
        Command::SummarizeWarehousePairwise {
            run_dir,
            sort_by,
            filter,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_warehouse_pairwise(
                &run_dir,
                ab_morph_run::WarehousePairwiseSummaryOptions {
                    sort_by,
                    text_filter: filter,
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_warehouse_pairwise_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeWarehousePatterns {
            run_dir,
            kind,
            feature_key,
            feature_profile,
            filter,
            exclude_feature_value,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            validate_warehouse_feature_profile(kind, feature_profile, feature_key.as_deref())?;
            let options = ab_morph_run::WarehousePatternOptions {
                kind,
                feature_key,
                feature_profile,
                text_filter: filter,
                excluded_feature_values: exclude_feature_value.into_iter().collect(),
                exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                limit,
            };
            if !json
                && ab_morph_run::write_warehouse_nway_patterns_duckdb_tsv(
                    &run_dir,
                    &options,
                    std::io::stdout(),
                )?
            {
                return Ok(());
            }
            let rows = ab_morph_run::summarize_warehouse_nway_patterns(&run_dir, options)?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_nway_pattern_table(&rows);
            }
            Ok(())
        }
        Command::MaterializeWarehouseFeaturePatterns {
            run_dir,
            feature_key,
        } => {
            if ab_morph_run::materialize_warehouse_core_feature_pattern_counts(
                &run_dir,
                feature_key.as_deref(),
            )? {
                eprintln!(
                    "wrote {}",
                    run_dir.join("feature_pattern_counts.parquet").display()
                );
            }
            Ok(())
        }
        Command::SummarizeWarehousePatternExamples {
            run_dir,
            kind,
            pattern,
            feature_key,
            feature_profile,
            filter,
            exclude_feature_value,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            validate_warehouse_feature_profile(kind, feature_profile, feature_key.as_deref())?;
            let options = ab_morph_run::WarehousePatternExampleOptions {
                kind,
                pattern,
                feature_key,
                feature_profile,
                text_filter: filter,
                excluded_feature_values: exclude_feature_value.into_iter().collect(),
                exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                limit,
            };
            if !json
                && ab_morph_run::write_warehouse_pattern_examples_duckdb_tsv(
                    &run_dir,
                    &options,
                    std::io::stdout(),
                )?
            {
                return Ok(());
            }
            let rows = ab_morph_run::summarize_warehouse_pattern_examples(&run_dir, options)?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_warehouse_region_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeWarehouseTriage {
            run_dir,
            output_dir,
            limit,
        } => run_warehouse_triage(&run_dir, &output_dir, limit),
        Command::SummarizeWarehouseRegions {
            run_dir,
            kind,
            filter,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let options = ab_morph_run::WarehouseRegionOptions {
                kind,
                text_filter: filter,
                exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                limit,
            };
            if !json
                && ab_morph_run::write_warehouse_regions_duckdb_tsv(
                    &run_dir,
                    &options,
                    std::io::stdout(),
                )?
            {
                return Ok(());
            }
            let rows = ab_morph_run::summarize_warehouse_regions(&run_dir, options)?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_warehouse_region_table(&rows);
            }
            Ok(())
        }
        Command::SummarizeWarehouseInteresting {
            run_dir,
            limit,
            format,
            filter,
            explain,
            anomalies,
            engine,
            feature_profile,
            rank_scope,
            lambda_missing_policy,
            anomaly_w_cov,
            output,
            force,
        } => {
            let summary = ab_morph_run::summarize_warehouse_interesting(
                &run_dir,
                ab_morph_run::WarehouseInterestingOptions {
                    limit,
                    filter,
                    anomalies,
                    explain,
                    max_region_examples: 5,
                    engine,
                    feature_profile,
                    rank_scope,
                    lambda_policy: lambda_missing_policy,
                    anomaly_w_cov,
                },
            )?;
            let mut writer: Box<dyn Write> = match &output {
                Some(path) => {
                    if path.exists() && !force {
                        bail!(
                            "refusing to overwrite {} (pass --force to allow)",
                            path.display()
                        );
                    }
                    Box::new(File::create(path).with_context(|| {
                        format!("failed to create {}", path.display())
                    })?)
                }
                None => Box::new(std::io::stdout()),
            };
            match format {
                ab_morph_run::InterestingOutputFormat::Json => {
                    serde_json::to_writer_pretty(&mut writer, &summary)?;
                    writeln!(writer)?;
                }
                ab_morph_run::InterestingOutputFormat::Table => {
                    ab_morph_run::write_interesting_tsv(&summary, &mut writer)?;
                }
            }
            Ok(())
        }
        Command::SummarizeWarehouseErrors {
            run_dir,
            group_by,
            exclude_source_id,
            exclude_text_id,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_warehouse_errors(
                &run_dir,
                ab_morph_run::WarehouseErrorSummaryOptions {
                    group_by,
                    exclusions: summary_exclusions(exclude_source_id, exclude_text_id),
                    limit,
                },
            )?;
            if json {
                serde_json::to_writer_pretty(std::io::stdout(), &rows)?;
                println!();
            } else {
                print_warehouse_error_table(&rows);
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
        Command::ImportAozoraMetadata {
            run_dir,
            from,
            force,
        } => {
            let summary = ab_morph_run::run_import_aozora_metadata(&run_dir, &from, force)?;
            let skipped = if summary.skipped_source_ids.is_empty() {
                String::new()
            } else {
                format!(": {}", summary.skipped_source_ids.join(", "))
            };
            eprintln!(
                "imported {} works covering {} sources into {}; skipped {} source(s){}",
                summary.works_imported,
                summary.sources_mapped,
                run_dir.join("aozora_works.parquet").display(),
                summary.skipped_source_ids.len(),
                skipped,
            );
            Ok(())
        }
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

    if args.nway && args.nway_output.is_none() {
        bail!("--nway requires --nway-output");
    }
    if args.nway_pattern_counts && args.nway_pattern_counts_output.is_none() {
        bail!("--nway-pattern-counts requires --nway-pattern-counts-output");
    }

    Ok(AnalyzeOutputPaths {
        analyses_output: {
            let Some(analyses_output) = args.analyses_output else {
                bail!(
                    "provide --analyses-output when using --output-dir; implicit default artifact names are no longer provided"
                )
            };
            analyses_output
        },
        comparisons_output: args.comparisons_output,
        examples_output: args.examples_output,
        errors_output: args.errors_output,
        manifest_output: Some(
            args.manifest_output
                .unwrap_or_else(|| output_dir.join("manifest.json")),
        ),
        nway_output: args.nway_output,
        nway_pattern_counts_output: args.nway_pattern_counts_output,
    })
}

fn run_warehouse_triage(run_dir: &Path, output_dir: &Path, limit: usize) -> Result<()> {
    fs::create_dir_all(output_dir)
        .with_context(|| format!("failed to create {}", output_dir.display()))?;
    write_warehouse_errors_tsv(run_dir, &output_dir.join("errors.tsv"), limit)?;
    write_warehouse_patterns_tsv(
        run_dir,
        &output_dir.join("top-segmentation.tsv"),
        ab_morph_run::WarehousePatternOptions {
            kind: ab_morph_run::NwayPatternKind::Segmentation,
            feature_key: None,
            feature_profile: ab_morph_run::WarehouseFeatureProfile::Raw,
            text_filter: ab_morph_run::WarehouseTextFilter::LexicalOnly,
            excluded_feature_values: Default::default(),
            exclusions: ab_morph_run::SummaryExclusions::default(),
            limit,
        },
    )?;
    write_warehouse_patterns_tsv(
        run_dir,
        &output_dir.join("top-pos1.tsv"),
        warehouse_triage_pos1_options(limit),
    )?;
    eprintln!("wrote warehouse triage to {}", output_dir.display());
    Ok(())
}

fn warehouse_triage_pos1_options(limit: usize) -> ab_morph_run::WarehousePatternOptions {
    ab_morph_run::WarehousePatternOptions {
        kind: ab_morph_run::NwayPatternKind::Feature,
        feature_key: Some("pos1".to_owned()),
        feature_profile: ab_morph_run::WarehouseFeatureProfile::Core,
        text_filter: ab_morph_run::WarehouseTextFilter::LexicalOnly,
        excluded_feature_values: Default::default(),
        exclusions: ab_morph_run::SummaryExclusions::default(),
        limit,
    }
}

fn write_warehouse_errors_tsv(run_dir: &Path, output_path: &Path, limit: usize) -> Result<()> {
    let rows = ab_morph_run::summarize_warehouse_errors(
        run_dir,
        ab_morph_run::WarehouseErrorSummaryOptions {
            group_by: ab_morph_run::WarehouseErrorGroupBy::ErrorCode,
            exclusions: ab_morph_run::SummaryExclusions::default(),
            limit,
        },
    )?;
    let mut writer = File::create(output_path)
        .with_context(|| format!("failed to create {}", output_path.display()))?;
    writeln!(
        writer,
        "key\terrors\tsource_count\ttext_count\tanalyzers\tstages\terror_codes\tsample_source_ids\tsample_messages"
    )?;
    for row in rows {
        writeln!(
            writer,
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.errors,
            row.source_ids.len(),
            row.text_ids.len(),
            row.analyzer_ids.join(","),
            row.stages.join(","),
            row.error_codes.join(","),
            sample_values(&row.source_ids, 5),
            sample_values(&row.sample_messages, 5),
        )?;
    }
    Ok(())
}

fn write_warehouse_patterns_tsv(
    run_dir: &Path,
    output_path: &Path,
    options: ab_morph_run::WarehousePatternOptions,
) -> Result<()> {
    let mut writer = File::create(output_path)
        .with_context(|| format!("failed to create {}", output_path.display()))?;
    if ab_morph_run::write_warehouse_nway_patterns_duckdb_tsv(run_dir, &options, &mut writer)? {
        return Ok(());
    }
    let rows = ab_morph_run::summarize_warehouse_nway_patterns(run_dir, options)?;
    writeln!(
        writer,
        "kind\texamples\tsource_count\ttext_count\tsample_source_ids\tsample_text_ids\tscript_categories\tpattern"
    )?;
    for row in rows {
        writeln!(
            writer,
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.kind,
            row.examples,
            row.source_ids.len(),
            row.text_ids.len(),
            sample_values(&row.source_ids, 5),
            sample_values(&row.text_ids, 5),
            row.script_categories.join(","),
            row.pattern,
        )?;
    }
    Ok(())
}

fn validate_warehouse_cli(
    warehouse_dir: Option<&PathBuf>,
    run_id: Option<&str>,
    resume: bool,
    _jobs: usize,
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

fn print_warehouse_region_table(rows: &[ab_morph_run::WarehouseRegionExampleRow]) {
    println!(
        "source_id\ttext_id\tregion_index\tchar_span\tbytes\tsegmentation\tfeature\tcoverage\tanalyzers\tfeatures"
    );
    for row in rows {
        let analyzers = row
            .analyzers
            .iter()
            .map(|analyzer| format!("{}:[{}]", analyzer.analyzer_id, analyzer.surfaces.join("|")))
            .collect::<Vec<_>>()
            .join(" ; ");
        let features = row
            .feature_diffs
            .iter()
            .map(|feature| {
                format!(
                    "{}:{}={}",
                    feature.analyzer_id,
                    feature.feature_key,
                    feature.feature_value.as_deref().unwrap_or("<null>")
                )
            })
            .collect::<Vec<_>>()
            .join(" ; ");
        println!(
            "{}\t{}\t{}\t{}..{}\t{}..{}\t{}\t{}\t{}\t{}\t{}",
            row.source_id,
            row.text_id,
            row.region_index,
            row.char_start,
            row.char_end,
            row.byte_start,
            row.byte_end,
            row.has_segmentation_disagreement,
            row.has_feature_disagreement,
            row.has_coverage_mismatch,
            analyzers,
            features,
        );
    }
}

fn print_warehouse_error_table(rows: &[ab_morph_run::WarehouseErrorSummaryRow]) {
    println!(
        "key\terrors\tsource_count\ttext_count\tanalyzers\tstages\terror_codes\tsample_source_ids\tsample_messages"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.errors,
            row.source_ids.len(),
            row.text_ids.len(),
            row.analyzer_ids.join(","),
            row.stages.join(","),
            row.error_codes.join(","),
            sample_values(&row.source_ids, 5),
            row.sample_messages.join(" | "),
        );
    }
}

fn print_warehouse_pairwise_table(rows: &[ab_morph_run::WarehousePairwiseSummaryRow]) {
    println!(
        "source_id\ttext_id\tfrom_analyzer\tto_analyzer\tregions\tsegmentation_regions\tfeature_regions\tcoverage_regions\tunanimous_boundary_count\tvariable_boundary_count"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.source_id,
            row.text_id,
            row.from_analyzer,
            row.to_analyzer,
            row.regions,
            row.segmentation_regions,
            row.feature_regions,
            row.coverage_regions,
            row.unanimous_boundary_count,
            row.variable_boundary_count,
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
    fn parses_warehouse_profile_triage() {
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
            "triage-2026-05-03",
            "--warehouse-profile",
            "triage",
        ])
        .unwrap();

        let Command::AnalyzeAat {
            warehouse_profile, ..
        } = args.command
        else {
            panic!("expected analyze-aat");
        };

        assert_eq!(warehouse_profile, ab_morph_run::WarehouseProfile::Triage);
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
    fn warehouse_validation_rejects_resume_but_allows_parallel_jobs() {
        let err = validate_warehouse_cli(
            Some(&PathBuf::from("scratch/warehouse")),
            Some("run-a"),
            true,
            1,
        )
        .unwrap_err()
        .to_string();
        assert!(err.contains("does not support --resume"));

        validate_warehouse_cli(
            Some(&PathBuf::from("scratch/warehouse")),
            Some("run-a"),
            false,
            2,
        )
        .unwrap();
    }

    #[test]
    fn output_dir_uses_explicit_paths() {
        let paths = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: Some(PathBuf::from("scratch/out")),
            analyses_output: Some(PathBuf::from("scratch/out/analyses.jsonl")),
            comparisons_output: Some(PathBuf::from("scratch/out/comparisons.jsonl")),
            examples_output: Some(PathBuf::from("scratch/out/examples.jsonl")),
            errors_output: Some(PathBuf::from("scratch/out/errors.jsonl")),
            manifest_output: None,
            nway_output: Some(PathBuf::from("scratch/out/nway.jsonl")),
            nway_pattern_counts_output: Some(PathBuf::from(
                "scratch/out/nway-pattern-counts.jsonl",
            )),
            nway: true,
            nway_pattern_counts: true,
        })
        .unwrap();

        assert_eq!(
            paths,
            AnalyzeOutputPaths {
                analyses_output: PathBuf::from("scratch/out/analyses.jsonl"),
                comparisons_output: Some(PathBuf::from("scratch/out/comparisons.jsonl")),
                examples_output: Some(PathBuf::from("scratch/out/examples.jsonl")),
                errors_output: Some(PathBuf::from("scratch/out/errors.jsonl")),
                manifest_output: Some(PathBuf::from("scratch/out/manifest.json")),
                nway_output: Some(PathBuf::from("scratch/out/nway.jsonl")),
                nway_pattern_counts_output: Some(PathBuf::from(
                    "scratch/out/nway-pattern-counts.jsonl"
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
            nway_pattern_counts: false,
        })
        .unwrap();

        assert_eq!(
            paths.analyses_output,
            PathBuf::from("custom/analyses.jsonl")
        );
        assert_eq!(paths.nway_output, Some(PathBuf::from("custom/nway.jsonl")));
        assert_eq!(paths.nway_pattern_counts_output, None);
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
    fn output_dir_without_analyses_output_is_rejected() {
        let err = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: Some(PathBuf::from("scratch/out")),
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

        assert!(
            err.to_string()
                .contains("provide --analyses-output when using --output-dir")
        );
    }

    #[test]
    fn output_dir_nway_requires_nway_output_when_enabled() {
        let err = resolve_analyze_outputs(AnalyzeOutputArgs {
            output_dir: Some(PathBuf::from("scratch/out")),
            analyses_output: Some(PathBuf::from("scratch/out/analyses.jsonl")),
            comparisons_output: None,
            examples_output: None,
            errors_output: None,
            manifest_output: None,
            nway_output: None,
            nway_pattern_counts_output: None,
            nway: true,
            nway_pattern_counts: false,
        })
        .unwrap_err();

        assert!(err.to_string().contains("--nway requires --nway-output"));
    }

    #[test]
    fn parses_summarize_warehouse_nway_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-nway",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--group-by",
            "text-id",
            "--sort-by",
            "variable-boundary-count",
            "--exclude-text-id",
            "JISTABLE",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehouseNway {
            run_dir,
            group_by,
            sort_by,
            exclude_text_id,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-nway");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(group_by, ab_morph_run::CompactSummaryGroupBy::TextId);
        assert_eq!(
            sort_by,
            ab_morph_run::NwaySummarySort::VariableBoundaryCount
        );
        assert_eq!(exclude_text_id, vec!["JISTABLE"]);
        assert_eq!(limit, 15);
        assert!(json);
    }

    #[test]
    fn parses_summarize_warehouse_interesting_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-interesting",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--limit",
            "25",
            "--format",
            "json",
            "--filter",
            "lexical-only",
            "--anomalies",
            "5",
            "--output",
            "scratch/interesting.json",
            "--force",
        ]);

        let Command::SummarizeWarehouseInteresting {
            run_dir,
            limit,
            format,
            filter,
            explain,
            anomalies,
            engine,
            feature_profile,
            rank_scope,
            lambda_missing_policy,
            anomaly_w_cov,
            output,
            force,
        } = args.command
        else {
            panic!("expected summarize-warehouse-interesting");
        };

        assert_eq!(engine, ab_morph_run::InterestingEngine::Auto);
        assert_eq!(
            feature_profile,
            ab_morph_run::WarehouseFeatureProfile::Core
        );

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(limit, 25);
        assert_eq!(format, ab_morph_run::InterestingOutputFormat::Json);
        assert_eq!(filter, ab_morph_run::InterestingTextFilter::LexicalOnly);
        assert_eq!(explain, None);
        assert_eq!(anomalies, 5);
        assert_eq!(output, Some(PathBuf::from("scratch/interesting.json")));
        assert!(force);
        assert_eq!(rank_scope, ab_morph_run::RankScope::WithinKind);
        assert_eq!(
            lambda_missing_policy,
            ab_morph_run::LambdaMissingPolicy::RankFloor
        );
        assert!((anomaly_w_cov - 5.0).abs() < f64::EPSILON);
    }

    #[test]
    fn parses_summarize_interesting_scoring_knobs() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-interesting",
            "--run-dir",
            "/tmp/run",
            "--rank-scope",
            "global",
            "--lambda-missing-policy",
            "fixed:0.005",
            "--anomaly-w-cov",
            "2",
        ]);
        let Command::SummarizeWarehouseInteresting {
            rank_scope,
            lambda_missing_policy,
            anomaly_w_cov,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-interesting");
        };
        assert_eq!(rank_scope, ab_morph_run::RankScope::Global);
        assert_eq!(
            lambda_missing_policy,
            ab_morph_run::LambdaMissingPolicy::Fixed(0.005)
        );
        assert!((anomaly_w_cov - 2.0).abs() < f64::EPSILON);
    }

    #[test]
    fn parses_summarize_warehouse_patterns_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-patterns",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--kind",
            "feature",
            "--feature-key",
            "pos1",
            "--feature-profile",
            "core",
            "--filter",
            "lexical-only",
            "--exclude-source-id",
            "source-a",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehousePatterns {
            run_dir,
            kind,
            feature_key,
            feature_profile,
            filter,
            exclude_source_id,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-patterns");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(kind, ab_morph_run::NwayPatternKind::Feature);
        assert_eq!(feature_key, Some("pos1".to_owned()));
        assert_eq!(feature_profile, ab_morph_run::WarehouseFeatureProfile::Core);
        assert_eq!(filter, ab_morph_run::WarehouseTextFilter::LexicalOnly);
        assert_eq!(exclude_source_id, vec!["source-a"]);
        assert_eq!(limit, 15);
        assert!(json);
    }

    #[test]
    fn warehouse_feature_profiles_require_feature_key_for_full_corpus_queries() {
        assert!(
            validate_warehouse_feature_profile(
                ab_morph_run::NwayPatternKind::Feature,
                ab_morph_run::WarehouseFeatureProfile::Core,
                None,
            )
            .is_ok()
        );
        assert!(
            validate_warehouse_feature_profile(
                ab_morph_run::NwayPatternKind::Feature,
                ab_morph_run::WarehouseFeatureProfile::Schema,
                Some("goshu"),
            )
            .is_ok()
        );
        assert!(
            validate_warehouse_feature_profile(
                ab_morph_run::NwayPatternKind::Feature,
                ab_morph_run::WarehouseFeatureProfile::Schema,
                None,
            )
            .is_err()
        );
        assert!(
            validate_warehouse_feature_profile(
                ab_morph_run::NwayPatternKind::Segmentation,
                ab_morph_run::WarehouseFeatureProfile::Schema,
                None,
            )
            .is_ok()
        );
    }

    #[test]
    fn warehouse_triage_pos1_uses_materialized_core_profile() {
        let options = warehouse_triage_pos1_options(7);

        assert_eq!(
            options.feature_profile,
            ab_morph_run::WarehouseFeatureProfile::Core
        );
        assert_eq!(options.feature_key, Some("pos1".to_owned()));
        assert!(options.excluded_feature_values.is_empty());
        assert_eq!(options.limit, 7);
    }

    #[test]
    fn parses_summarize_warehouse_pattern_examples_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-pattern-examples",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--kind",
            "feature",
            "--pattern",
            "pos1 whole_region 名詞=>vibrato ; 動詞=>sudachi-a",
            "--feature-key",
            "pos1",
            "--feature-profile",
            "schema",
            "--filter",
            "lexical-only",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehousePatternExamples {
            run_dir,
            kind,
            pattern,
            feature_key,
            feature_profile,
            filter,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-pattern-examples");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(kind, ab_morph_run::NwayPatternKind::Feature);
        assert_eq!(pattern, "pos1 whole_region 名詞=>vibrato ; 動詞=>sudachi-a");
        assert_eq!(feature_key, Some("pos1".to_owned()));
        assert_eq!(
            feature_profile,
            ab_morph_run::WarehouseFeatureProfile::Schema
        );
        assert_eq!(filter, ab_morph_run::WarehouseTextFilter::LexicalOnly);
        assert_eq!(limit, 15);
        assert!(json);
    }

    #[test]
    fn parses_summarize_warehouse_triage_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-triage",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--output-dir",
            "scratch/triage",
            "--limit",
            "30",
        ]);

        let Command::SummarizeWarehouseTriage {
            run_dir,
            output_dir,
            limit,
        } = args.command
        else {
            panic!("expected summarize-warehouse-triage");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(output_dir, PathBuf::from("scratch/triage"));
        assert_eq!(limit, 30);
    }

    #[test]
    fn parses_summarize_warehouse_regions_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-regions",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--kind",
            "segmentation",
            "--filter",
            "lexical-only",
            "--exclude-source-id",
            "source-a",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehouseRegions {
            run_dir,
            kind,
            filter,
            exclude_source_id,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-regions");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(kind, ab_morph_run::WarehouseRegionKind::Segmentation);
        assert_eq!(filter, ab_morph_run::WarehouseTextFilter::LexicalOnly);
        assert_eq!(exclude_source_id, vec!["source-a"]);
        assert_eq!(limit, 15);
        assert!(json);
    }

    #[test]
    fn parses_summarize_warehouse_errors_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-errors",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--group-by",
            "analyzer",
            "--exclude-source-id",
            "source-a",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehouseErrors {
            run_dir,
            group_by,
            exclude_source_id,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-errors");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(group_by, ab_morph_run::WarehouseErrorGroupBy::Analyzer);
        assert_eq!(exclude_source_id, vec!["source-a"]);
        assert_eq!(limit, 15);
        assert!(json);
    }

    #[test]
    fn parses_summarize_warehouse_pairwise_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "summarize-warehouse-pairwise",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-05-01",
            "--sort-by",
            "feature-regions",
            "--filter",
            "whitespace-only",
            "--exclude-source-id",
            "source-a",
            "--limit",
            "15",
            "--json",
        ]);

        let Command::SummarizeWarehousePairwise {
            run_dir,
            sort_by,
            filter,
            exclude_source_id,
            limit,
            json,
            ..
        } = args.command
        else {
            panic!("expected summarize-warehouse-pairwise");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-05-01")
        );
        assert_eq!(sort_by, ab_morph_run::WarehousePairwiseSort::FeatureRegions);
        assert_eq!(filter, ab_morph_run::WarehouseTextFilter::WhitespaceOnly);
        assert_eq!(exclude_source_id, vec!["source-a"]);
        assert_eq!(limit, 15);
        assert!(json);
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
    fn parses_import_aozora_metadata_command() {
        let args = Args::parse_from([
            "ab-morph-run",
            "import-aozora-metadata",
            "--run-dir",
            "scratch/morph-warehouse/runs/full-2026-07-05",
            "--from",
            "../abc/out/corpus",
            "--force",
        ]);

        let Command::ImportAozoraMetadata {
            run_dir,
            from,
            force,
        } = args.command
        else {
            panic!("expected import-aozora-metadata command");
        };

        assert_eq!(
            run_dir,
            PathBuf::from("scratch/morph-warehouse/runs/full-2026-07-05")
        );
        assert_eq!(from, PathBuf::from("../abc/out/corpus"));
        assert!(force);
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
            &["test:single".to_owned()],
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
            &["test:single".to_owned()],
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
