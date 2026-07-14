//! Regenerates the neutral parser-comparison reports from committed inputs.
//!
//! Usage: `generate-parser-study-report [AB_VALIDATOR_ROOT]`
//!
//! Reads the frozen preregistration and the committed raw run manifests, then
//! writes the machine-readable `comparison-result.json` and the narrative
//! `comparison-report.md`. Output is a deterministic function of the inputs, so
//! rerunning it must leave the working tree unchanged (the drift test proves it).

use std::path::PathBuf;
use std::process::ExitCode;
use std::{env, fs};

use ab_parser_study_report::generate::generate_reports;

const STUDY: &str = "aozora-parser-neutral-comparison-2026-07";

fn main() -> ExitCode {
    let root = env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."));

    let run_manifests_path = root
        .join("reports/parser-study/runs")
        .join(STUDY)
        .join("run-manifests.json");
    let appendix_manifests_path = root
        .join("reports/parser-study/runs")
        .join(STUDY)
        .join("appendix-run-manifests.json");
    let prereg_path = root.join("docs/studies/aozora-parser-comparison-preregistration.json");
    let out_dir = root.join("reports/parser-study/reports").join(STUDY);
    let machine_path = out_dir.join("comparison-result.json");
    let narrative_path = out_dir.join("comparison-report.md");

    let run_manifests = match fs::read_to_string(&run_manifests_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("failed to read {}: {err}", run_manifests_path.display());
            return ExitCode::FAILURE;
        }
    };
    let prereg = match fs::read_to_string(&prereg_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!("failed to read {}: {err}", prereg_path.display());
            return ExitCode::FAILURE;
        }
    };
    let appendix_manifests = match fs::read_to_string(&appendix_manifests_path) {
        Ok(text) => text,
        Err(err) => {
            eprintln!(
                "failed to read {}: {err}",
                appendix_manifests_path.display()
            );
            return ExitCode::FAILURE;
        }
    };

    let reports = match generate_reports(&run_manifests, &prereg, &appendix_manifests) {
        Ok(reports) => reports,
        Err(err) => {
            eprintln!("report generation failed: {err}");
            return ExitCode::FAILURE;
        }
    };

    if let Err(err) = fs::create_dir_all(&out_dir) {
        eprintln!("failed to create {}: {err}", out_dir.display());
        return ExitCode::FAILURE;
    }
    if let Err(err) = fs::write(&machine_path, reports.machine_json.as_bytes()) {
        eprintln!("failed to write {}: {err}", machine_path.display());
        return ExitCode::FAILURE;
    }
    if let Err(err) = fs::write(&narrative_path, reports.narrative_markdown.as_bytes()) {
        eprintln!("failed to write {}: {err}", narrative_path.display());
        return ExitCode::FAILURE;
    }

    println!("wrote {}", machine_path.display());
    println!("wrote {}", narrative_path.display());
    ExitCode::SUCCESS
}
