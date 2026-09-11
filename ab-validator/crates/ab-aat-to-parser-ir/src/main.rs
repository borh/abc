use std::path::PathBuf;

use ab_aat_to_parser_ir::{
    ConversionOptions, MappingDocument, PreparedConverter, SchemaSet,
    structural_probe::{
        StructuralProbeConfig, TeiEajStructuralExpansionConfig, parse_input_spec,
        run_structural_probe, run_tei_eaj_structural_expansion, write_structural_probe_reports,
        write_tei_eaj_expansion_reports,
    },
    tei_eaj_alignment_probe::{
        TeiEajAlignmentProbeConfig, run_tei_eaj_alignment_probe,
        write_tei_eaj_alignment_probe_reports,
    },
};
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

mod audit;

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Convert {
        #[arg(long)]
        aat: PathBuf,
        #[arg(long)]
        ortho_annotations: Option<PathBuf>,
        #[arg(long)]
        work_content_hash: Option<String>,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        parser_ir_out: PathBuf,
        #[arg(long)]
        divergence_out: PathBuf,
        /// Fail closed unless the loaded mapping's `mapping_version` equals this.
        #[arg(long = "expect-mapping-version")]
        expect_mapping_version: Option<String>,
        /// Fail closed unless the loaded mapping's content hash equals this
        /// (`sha256:…`, the same hash the audit summary reports as `mapping_hash`).
        #[arg(long = "expect-mapping-hash")]
        expect_mapping_hash: Option<String>,
    },
    DetectOrthoAnnotations {
        #[arg(long)]
        aat: PathBuf,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        ortho_annotations_out: PathBuf,
        #[arg(long = "expect-mapping-version")]
        expect_mapping_version: Option<String>,
        #[arg(long = "expect-mapping-hash")]
        expect_mapping_hash: Option<String>,
    },
    AuditCorpus {
        #[arg(long = "aat-dir", required = true)]
        aat_dirs: Vec<PathBuf>,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        summary_json: PathBuf,
        #[arg(long)]
        report_md: PathBuf,
        #[arg(long)]
        compat_edn_out: Option<PathBuf>,
        #[arg(long, default_value_t = 0)]
        jobs: usize,
        #[arg(long = "expect-mapping-version")]
        expect_mapping_version: Option<String>,
        #[arg(long = "expect-mapping-hash")]
        expect_mapping_hash: Option<String>,
    },
    StructuralProbe {
        #[arg(long = "aat", required = true)]
        aat_inputs: Vec<String>,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        summary_json: PathBuf,
        #[arg(long)]
        report_md: PathBuf,
        #[arg(long = "expect-mapping-version")]
        expect_mapping_version: Option<String>,
        #[arg(long = "expect-mapping-hash")]
        expect_mapping_hash: Option<String>,
    },
    TeiEajStructuralExpansion {
        #[arg(long)]
        workset: PathBuf,
        #[arg(long = "aat-dir", required = true)]
        aat_dirs: Vec<String>,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        summary_json: PathBuf,
        #[arg(long)]
        report_md: PathBuf,
        #[arg(long = "expect-mapping-version")]
        expect_mapping_version: Option<String>,
        #[arg(long = "expect-mapping-hash")]
        expect_mapping_hash: Option<String>,
    },
    TeiEajAlignmentProbe {
        #[arg(long)]
        workset: PathBuf,
        #[arg(long)]
        summary_json: PathBuf,
        #[arg(long)]
        report_md: PathBuf,
        #[arg(long)]
        max_rows: Option<usize>,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Convert {
            aat,
            ortho_annotations,
            work_content_hash,
            mapping,
            parser_ir_out,
            divergence_out,
            expect_mapping_version,
            expect_mapping_hash,
        } => {
            let aat = ab_aat_to_parser_ir::schema::read_json(&aat)?;
            let mapping = MappingDocument::from_path(&mapping)?;
            mapping.check_expected_generation(
                expect_mapping_version.as_deref(),
                expect_mapping_hash.as_deref(),
            )?;
            let schemas = SchemaSet::for_aat_version(mapping.source_aat_version)?;
            let orthographic_annotations = match ortho_annotations {
                Some(path) => Some(
                    ab_aat_to_parser_ir::ortho_annotations::read_ortho_annotations_bundle(&path)?,
                ),
                None => None,
            };
            let output = PreparedConverter::new(mapping, schemas)?.convert(
                aat,
                ConversionOptions {
                    orthographic_annotations,
                    work_content_hash,
                    ..ConversionOptions::default()
                },
            )?;
            std::fs::write(
                parser_ir_out,
                ab_aat_to_parser_ir::to_canonical_json_pretty(output.parser_ir)? + "\n",
            )?;
            std::fs::write(
                divergence_out,
                ab_aat_to_parser_ir::to_canonical_json_pretty(output.divergence_bundle)? + "\n",
            )?;
        }
        Command::DetectOrthoAnnotations {
            aat,
            mapping,
            ortho_annotations_out,
            expect_mapping_version,
            expect_mapping_hash,
        } => {
            let aat = ab_aat_to_parser_ir::schema::read_json(&aat)?;
            let mapping = MappingDocument::from_path(&mapping)?;
            mapping.check_expected_generation(
                expect_mapping_version.as_deref(),
                expect_mapping_hash.as_deref(),
            )?;
            let schemas = SchemaSet::for_aat_version(mapping.source_aat_version)?;
            let vibrato = std::sync::Arc::new(
                ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()
                    .context("detect-ortho-annotations requires AB_VIBRATO_DICT or the flake-provided Unidic CWJ dictionary")?,
            );
            let detector = ab_ortho_detect::heuristic::HeuristicV1::new(
                vibrato,
                ab_ortho_detect::heuristic::HeuristicConfig::default(),
            );
            let bundle = ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations(
                aat, mapping, schemas, &detector,
            )?;
            std::fs::write(
                ortho_annotations_out,
                serde_json::to_string_pretty(&bundle)? + "\n",
            )?;
        }
        Command::AuditCorpus {
            aat_dirs,
            mapping,
            summary_json,
            report_md,
            compat_edn_out,
            jobs,
            expect_mapping_version,
            expect_mapping_hash,
        } => {
            let summary = audit::run_audit(audit::CorpusAuditConfig {
                aat_dirs,
                mapping_path: mapping,
                summary_json,
                report_md,
                compat_edn_out,
                jobs,
                expect_mapping_version,
                expect_mapping_hash,
            })?;
            eprintln!(
                "audited {} AAT files: {} succeeded, {} failed",
                summary.files_attempted(),
                summary.files_succeeded(),
                summary.files_failed()
            );
        }
        Command::StructuralProbe {
            aat_inputs,
            mapping,
            summary_json,
            report_md,
            expect_mapping_version,
            expect_mapping_hash,
        } => {
            let inputs = aat_inputs
                .iter()
                .map(|spec| parse_input_spec(spec))
                .collect::<Result<Vec<_>>>()?;
            let mapping = MappingDocument::from_path(&mapping)?;
            mapping.check_expected_generation(
                expect_mapping_version.as_deref(),
                expect_mapping_hash.as_deref(),
            )?;
            let schemas = SchemaSet::for_aat_version(mapping.source_aat_version)?;
            let summary = run_structural_probe(StructuralProbeConfig {
                inputs,
                mapping,
                schemas,
            })?;
            write_structural_probe_reports(&summary, &summary_json, &report_md)?;
            eprintln!(
                "probed {} AAT inputs: {} conversion(s) succeeded, {} failed",
                summary.totals.inputs,
                summary.totals.conversions_succeeded,
                summary.totals.conversions_failed
            );
        }
        Command::TeiEajStructuralExpansion {
            workset,
            aat_dirs,
            mapping,
            summary_json,
            report_md,
            expect_mapping_version,
            expect_mapping_hash,
        } => {
            let aat_dirs = aat_dirs
                .iter()
                .map(|spec| parse_input_spec(spec))
                .collect::<Result<Vec<_>>>()?;
            let mapping = MappingDocument::from_path(&mapping)?;
            mapping.check_expected_generation(
                expect_mapping_version.as_deref(),
                expect_mapping_hash.as_deref(),
            )?;
            let schemas = SchemaSet::for_aat_version(mapping.source_aat_version)?;
            let summary = run_tei_eaj_structural_expansion(TeiEajStructuralExpansionConfig {
                workset_path: workset,
                aat_dirs,
                mapping,
                schemas,
            })?;
            write_tei_eaj_expansion_reports(&summary, &summary_json, &report_md)?;
            eprintln!(
                "expanded {} TEI-EAJ row(s): {} with AAT evidence, {} parser-IR gap row(s), {} evidence gap row(s)",
                summary.rows.len(),
                summary.totals.rows_with_aat_evidence,
                summary.totals.parser_ir_gap_rows,
                summary.totals.evidence_gap_rows
            );
        }
        Command::TeiEajAlignmentProbe {
            workset,
            summary_json,
            report_md,
            max_rows,
        } => {
            let report = run_tei_eaj_alignment_probe(TeiEajAlignmentProbeConfig {
                workset_path: workset,
                max_rows,
            })?;
            write_tei_eaj_alignment_probe_reports(&report, &summary_json, &report_md)?;
            eprintln!("wrote {} TEI-EAJ alignment probe row(s)", report.rows.len());
        }
    }
    Ok(())
}
