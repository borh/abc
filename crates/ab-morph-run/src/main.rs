use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use anyhow::Result;
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
        analyses_output: PathBuf,
        #[arg(long)]
        comparisons_output: Option<PathBuf>,
        #[arg(long)]
        examples_output: Option<PathBuf>,
        #[arg(long)]
        errors_output: Option<PathBuf>,
        #[arg(long)]
        manifest_output: Option<PathBuf>,
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
        #[arg(long, value_enum, default_value_t = ExampleSortArg::Examples)]
        sort_by: ExampleSortArg,
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

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::AnalyzeAat {
            aat,
            aat_dir,
            analyzer,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
            manifest_output,
            resume,
            jobs,
            output_profile,
            max_examples_per_comparison,
            progress,
            progress_interval_seconds,
        } => {
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
            let result = ab_morph_run::run_analyze_aat(
                aat.as_deref(),
                aat_dir.as_deref(),
                &analyzer,
                &analyses_output,
                comparisons_output.as_deref(),
                errors_output.as_deref(),
                resume,
                jobs,
                output_profile,
                examples_output.as_deref(),
                max_examples_per_comparison,
                manifest_output.as_deref(),
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
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_comparisons(
                &comparisons,
                ab_morph_run::CompactSummaryOptions {
                    group_by: group_by.into_library(),
                    sort_by: sort_by.into_library(),
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
            sort_by,
            limit,
            json,
        } => {
            let rows = ab_morph_run::summarize_compact_examples(
                &examples,
                ab_morph_run::CompactExampleSummaryOptions {
                    group_by: group_by.into_library(),
                    filter: filter.into_library(),
                    sort_by: sort_by.into_library(),
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
        "key\tsource_ids\ttext_ids\tcomparisons\tworst_boundary_f1\ttotal_segmentation_regions\ttotal_whitespace_segmentation_regions\ttotal_lexical_segmentation_regions\ttotal_feature_difference_regions\ttotal_whitespace_feature_difference_regions\ttotal_lexical_feature_difference_regions\ttotal_coverage_mismatch_regions"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
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
        "key\tsource_ids\ttext_ids\texamples\twhitespace_examples\tlexical_examples\tsegmentation_examples\tfeature_diff_examples\tcoverage_examples"
    );
    for row in rows {
        println!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            row.key,
            row.source_ids.join(","),
            row.text_ids.join(","),
            row.examples,
            row.whitespace_examples,
            row.lexical_examples,
            row.segmentation_examples,
            row.feature_diff_examples,
            row.coverage_examples,
        );
    }
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
            "--limit",
            "25",
            "--json",
        ]);

        let Command::SummarizeCompact {
            comparisons,
            group_by,
            sort_by,
            limit,
            json,
        } = args.command
        else {
            panic!("expected summarize-compact command");
        };

        assert_eq!(comparisons, PathBuf::from("comparisons.jsonl.zst"));
        assert_eq!(group_by, SummaryGroupByArg::TextId);
        assert_eq!(sort_by, SummarySortArg::LexicalSegmentationRegions);
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
            "--limit",
            "30",
            "--json",
        ]);

        let Command::SummarizeExamples {
            examples,
            group_by,
            filter,
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
        assert_eq!(sort_by, ExampleSortArg::WhitespaceExamples);
        assert_eq!(limit, 30);
        assert!(json);
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
