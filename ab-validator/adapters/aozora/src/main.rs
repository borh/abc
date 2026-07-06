use std::io::{self, Read, Write};

use anyhow::Result;
use aozora_adapter::{aat_json_from_bytes, adapter_version, html_from_bytes};
use clap::{Parser, ValueEnum};

#[derive(Debug, Parser)]
struct Args {
    #[arg(long)]
    mode: Option<Mode>,

    #[arg(long)]
    version: bool,
}

#[derive(Debug, Clone, ValueEnum)]
enum Mode {
    Aat,
    Html,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if args.version {
        println!("{}", adapter_version());
        return Ok(());
    }

    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    let out = match args.mode.unwrap_or(Mode::Aat) {
        Mode::Aat => aat_json_from_bytes(&bytes)?,
        Mode::Html => html_from_bytes(&bytes)?,
    };
    io::stdout().write_all(&out)?;
    Ok(())
}
