use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process;

use anyhow::Result;
use aozora_epub3_adapter::{
    map_to_aat, map_to_html, MappingInput, XhtmlDocument, XhtmlDocumentKind, CLI_VERSION,
};
use clap::{Parser, ValueEnum};

/// `aozora-epub3-adapter` — shell out to AozoraEpub3-JDK21 and project its
/// rendered XHTML into AAT JSON (`--mode aat`) or HTML (`--mode html`).
#[derive(Debug, Parser)]
#[command(name = "aozora-epub3-adapter", version = CLI_VERSION)]
struct Args {
    /// Output mode: `aat` (default) emits AAT JSON; `html` concatenates body
    /// XHTML (and any colophon section) for debugging.
    #[arg(long)]
    mode: Option<Mode>,

    /// Raw work source file (required for `--mode aat`).
    #[arg(long)]
    source: Option<PathBuf>,

    /// Body XHTML files in spine order.
    #[arg(long, num_args = 1..)]
    xhtml: Vec<PathBuf>,

    /// Colophon XHTML file, classified by the wrapper from spine position.
    /// `--mode aat` ignores colophon content for block production; `--mode html`
    /// appends it after the body sections.
    #[arg(long, num_args = 1..)]
    xhtml_colophon: Vec<PathBuf>,

    /// Mark the upstream parser as having failed. AAT is still emitted with
    /// `meta.parse_complete=false`, and the process exits with code 2.
    #[arg(long)]
    parser_failed: bool,

    /// Optional file containing the upstream parser's error message.
    #[arg(long)]
    parser_error_file: Option<PathBuf>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Mode {
    Aat,
    Html,
}

fn run() -> Result<()> {
    let args = Args::parse();
    let mode = args.mode.unwrap_or(Mode::Aat);

    let mut docs: Vec<XhtmlDocument> = Vec::new();
    for path in &args.xhtml {
        docs.push(XhtmlDocument {
            bytes: read_file(path)?,
            kind: XhtmlDocumentKind::BodySection,
        });
    }
    for path in &args.xhtml_colophon {
        docs.push(XhtmlDocument {
            bytes: read_file(path)?,
            kind: XhtmlDocumentKind::Colophon,
        });
    }

    match mode {
        Mode::Html => {
            let input = MappingInput {
                source_bytes: Vec::new(),
                xhtml_documents: docs,
                parser_failed: false,
                parser_error_message: None,
            };
            let html = map_to_html(&input)?;
            io::stdout().write_all(html.as_bytes())?;
            Ok(())
        }
        Mode::Aat => {
            let source_path = args
                .source
                .ok_or_else(|| anyhow::anyhow!("--source required for --mode aat"))?;
            let source_bytes = read_file(&source_path)?;

            let parser_error_message = args
                .parser_error_file
                .as_ref()
                .and_then(|path| std::fs::read_to_string(path).ok())
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty());

            let input = MappingInput {
                source_bytes,
                xhtml_documents: docs,
                parser_failed: args.parser_failed,
                parser_error_message,
            };

            let out = map_to_aat(&input)?;
            println!("{}", serde_json::to_string(&out)?);

            if args.parser_failed {
                process::exit(2);
            }
            Ok(())
        }
    }
}

fn read_file(path: &PathBuf) -> Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut file = std::fs::File::open(path)?;
    file.read_to_end(&mut bytes)?;
    Ok(bytes)
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e:?}");
        process::exit(1);
    }
}