use anyhow::Result;
use aozora2html_adapter::{ADAPTER_VERSION, map_with_error_message, map_with_protocol_bytes};
use clap::{Parser, ValueEnum};
use std::io::{self, Read, Write};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(name = "aozora2html-adapter", version = ADAPTER_VERSION)]
struct Args {
    #[arg(long)]
    mode: Option<Mode>,

    #[arg(long)]
    source: Option<PathBuf>,

    #[arg(long)]
    xhtml: Option<PathBuf>,

    #[arg(long)]
    version: bool,

    #[arg(long)]
    parser_failed: bool,

    #[arg(long)]
    parser_error_file: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Mode {
    Aat,
    Html,
}

fn read_file(path: &PathBuf) -> Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut file = std::fs::File::open(path)?;
    file.read_to_end(&mut bytes)?;
    Ok(bytes)
}

fn main() -> Result<()> {
    let args = Args::parse();

    if args.version {
        println!("{ADAPTER_VERSION}");
        return Ok(());
    }

    let mode = args.mode.unwrap_or(Mode::Aat);
    match mode {
        Mode::Html => {
            if args.xhtml.is_none() {
                eprintln!("error: --xhtml is required for --mode html");
                return Err(anyhow::anyhow!("missing xhtml"));
            }
            let xhtml_path = args.xhtml.unwrap();
            let bytes = read_file(&xhtml_path)?;
            io::stdout().write_all(&bytes)?;
            Ok(())
        }
        Mode::Aat => {
            let source_path = args
                .source
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("error: --source is required for --mode aat"))?;
            let xhtml_path = args
                .xhtml
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("error: --xhtml is required for --mode aat"))?;

            let source_bytes = read_file(source_path)?;
            let xhtml_bytes = read_file(xhtml_path)?;
            let parser_error_message = args
                .parser_error_file
                .as_ref()
                .and_then(|path| std::fs::read_to_string(path).ok());

            let message = if args.parser_failed {
                parser_error_message.unwrap_or_else(|| "aozora2html parser aborted".to_string())
            } else {
                String::new()
            };

            let out = if args.parser_failed {
                map_with_error_message(&xhtml_bytes, &source_bytes, true, Some(message))?
            } else {
                map_with_protocol_bytes(&xhtml_bytes, &source_bytes, true)?
            };

            println!("{}", serde_json::to_string(&out)?);
            Ok(())
        }
    }
}
