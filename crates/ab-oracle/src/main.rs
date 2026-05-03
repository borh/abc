use std::path::PathBuf;

use ab_oracle::{
    adapter_run::{parse_adapter_spec, run_adapter_aat},
    data::{load_oracle_cases, load_upstream_observations},
    evaluate::evaluate_case,
    oracle_quality::validate_oracle_quality,
    report::{OracleReport, ReportRow, read_json_report, render_markdown, write_json_report},
};
use anyhow::{Result, bail};
use clap::Parser;

#[derive(Debug, Parser)]
struct Args {
    #[arg(long, default_value = "data/aat-oracle-cases.toml")]
    oracle: PathBuf,

    #[arg(long, default_value = "data/aat-upstream-observations.toml")]
    upstream: PathBuf,

    #[arg(long = "adapter")]
    adapters: Vec<String>,

    #[arg(long)]
    case_id: Option<String>,

    #[arg(long)]
    report_json: Option<PathBuf>,

    #[arg(long)]
    report_md_from_json: Option<PathBuf>,
}

fn should_evaluate_case(
    case: &ab_oracle::data::OracleCase,
    requested_case_id: Option<&str>,
) -> bool {
    if matches!(
        case.current_review_status(),
        ab_oracle::data::ReviewStatus::Retired
    ) {
        return false;
    }
    requested_case_id.is_none_or(|case_id| case.id == case_id)
}

fn main() -> Result<()> {
    let args = Args::parse();
    if let Some(report_json) = &args.report_md_from_json {
        let report = read_json_report(report_json)?;
        print!("{}", render_markdown(&report));
        return Ok(());
    }

    let oracle = load_oracle_cases(&args.oracle)?;
    let quality_errors = validate_oracle_quality(&oracle);
    if !quality_errors.is_empty() {
        for error in &quality_errors {
            eprintln!(
                "oracle quality error in {}: {}",
                error.case_id, error.message
            );
        }
        bail!("oracle quality validation failed");
    }
    let observations = load_upstream_observations(&args.upstream)?;

    if !args.adapters.is_empty() {
        let mut rows = Vec::new();
        let cases = oracle
            .case
            .iter()
            .filter(|case| should_evaluate_case(case, args.case_id.as_deref()));
        for case in cases {
            for adapter_arg in &args.adapters {
                let adapter = parse_adapter_spec(adapter_arg);
                let aat = run_adapter_aat(&adapter, &case.source_utf8, &case.id)?;
                let evaluation =
                    evaluate_case(case, &oracle.evidence, &observations, &adapter.id, aat);
                rows.push(ReportRow {
                    case_id: evaluation.case_id,
                    adapter: adapter.id.clone(),
                    schema_status: evaluation.schema_status,
                    upstream_status: evaluation.upstream_status,
                    oracle_status: evaluation.oracle_status,
                    oracle_review_status: evaluation.oracle_review_status,
                    oracle_evidence_strength: evaluation.oracle_evidence_strength,
                    failures: evaluation.failures,
                });
            }
        }
        if rows.is_empty() {
            bail!("no oracle cases matched the requested filters");
        }
        let report = OracleReport { rows };
        if let Some(path) = &args.report_json {
            write_json_report(&report, path)?;
        } else {
            serde_json::to_writer_pretty(std::io::stdout(), &report)?;
            println!();
        }
        return Ok(());
    }

    println!(
        "loaded {} oracle cases and {} upstream observations",
        oracle.case.len(),
        observations.observation.len()
    );
    Ok(())
}
