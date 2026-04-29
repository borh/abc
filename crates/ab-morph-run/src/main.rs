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
    },
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
        } => {
            let input_count = if progress {
                Some(count_aat_json_inputs(aat.as_deref(), aat_dir.as_deref())?)
            } else {
                None
            };
            let start = Instant::now();
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
            if progress {
                emit_progress_summary(start, input_count);
            }
            result
        }
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
}
