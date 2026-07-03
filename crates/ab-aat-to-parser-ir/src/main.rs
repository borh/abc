use std::path::PathBuf;

use ab_aat_to_parser_ir::{
    ConversionOptions, ConversionRequest, MappingDocument, SchemaSet, convert,
};
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};

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
        mapping: PathBuf,
        #[arg(long)]
        parser_ir_out: PathBuf,
        #[arg(long)]
        divergence_out: PathBuf,
        #[arg(long)]
        abc_root: Option<PathBuf>,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Convert {
            aat,
            mapping,
            parser_ir_out,
            divergence_out,
            abc_root,
        } => {
            let repo_root = resolve_repo_root(&mapping)?;
            let abc_root = abc_root
                .or_else(|| std::env::var_os("AB_ABC_ROOT").map(PathBuf::from))
                .unwrap_or_else(|| repo_root.join("data/abc-schemas"));
            let aat = ab_aat_to_parser_ir::schema::read_json(&aat)?;
            let mapping = MappingDocument::from_path(&mapping)?;
            let schemas = SchemaSet::load(&repo_root, &abc_root)?;
            let output = convert(ConversionRequest {
                aat,
                mapping,
                schemas,
                options: ConversionOptions::default(),
            })?;
            std::fs::write(
                parser_ir_out,
                serde_json::to_string_pretty(&output.parser_ir)? + "\n",
            )?;
            std::fs::write(
                divergence_out,
                serde_json::to_string_pretty(&output.divergence_bundle)? + "\n",
            )?;
        }
    }
    Ok(())
}

fn resolve_repo_root(mapping: &std::path::Path) -> Result<PathBuf> {
    if let Some(value) = std::env::var_os("AB_VALIDATOR_REPO_ROOT") {
        return Ok(PathBuf::from(value));
    }

    let cwd = std::env::current_dir().context("failed to read current directory")?;
    if let Some(mapping_dir) = mapping.parent()
        && mapping_dir.file_name().and_then(|name| name.to_str()) == Some("data")
        && let Some(candidate) = mapping_dir.parent()
    {
        let candidate = if candidate.as_os_str().is_empty() {
            cwd.clone()
        } else if candidate.is_absolute() {
            candidate.to_path_buf()
        } else {
            cwd.join(candidate)
        };
        if candidate.join("data/aat-schema.json").is_file() {
            return Ok(candidate);
        }
    }

    if cwd.join("data/aat-schema.json").is_file() {
        return Ok(cwd);
    }

    Ok(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."))
}
