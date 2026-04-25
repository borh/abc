use std::io::{self, Read, Write};

use anyhow::Result;
use aozora_rs_adapter::{VERSION, aat_json_from_bytes, html_from_bytes};
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
        println!("{VERSION}");
        return Ok(());
    }

    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    match args.mode.unwrap_or(Mode::Aat) {
        Mode::Aat => io::stdout().write_all(&aat_json_from_bytes(&bytes)?)?,
        Mode::Html => print!("{}", html_from_bytes(&bytes)?),
    }
    Ok(())
}
