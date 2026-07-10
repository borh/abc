//! Parity shim (Phase 1 only): reproduces `aozora inspect
//! {nodes,diagnostics,gaiji} -` byte-for-byte over the lifted
//! ab-aozora-facade (fork of P4suta/aozora at
//! 1a4f864603970983719655aa4af4525958ac2d38; ADR 0031).
use std::io::{Read, Write};

use ab_aozora_facade::{Document, json};

const VERSION_LINE: &str = concat!(
    "ab-aozora-cli ",
    env!("CARGO_PKG_VERSION"),
    " (fork of P4suta/aozora @ 1a4f864, ADR 0031)"
);

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|a| a == "--version") {
        println!("{VERSION_LINE}");
        return;
    }
    let kind = match args.as_slice() {
        [_, cmd, kind, dash]
            if cmd == "inspect"
                && dash == "-"
                && matches!(kind.as_str(), "nodes" | "diagnostics" | "gaiji") =>
        {
            kind.clone()
        }
        _ => {
            eprintln!("usage: ab-aozora-cli inspect {{nodes|diagnostics|gaiji}} -  |  --version");
            std::process::exit(64);
        }
    };
    // Mirrors upstream `read_source`: `-` reads raw bytes from stdin (not
    // `read_to_string`) because the default `Encoding::Auto` policy decodes
    // valid UTF-8 as-is and falls back to Shift_JIS otherwise (Step 1
    // mapping) — decoding must happen after the read, not during it.
    let mut raw = Vec::new();
    if let Err(err) = std::io::stdin().read_to_end(&mut raw) {
        eprintln!("ab-aozora-cli: read stdin: {err}");
        std::process::exit(1);
    }
    match emit(&kind, &raw) {
        Ok(bytes) => {
            if std::io::stdout().lock().write_all(&bytes).is_err() {
                std::process::exit(1);
            }
        }
        Err(err) => {
            eprintln!("ab-aozora-cli: {err}");
            std::process::exit(1);
        }
    }
}

/// Calls the same `ab_aozora_facade::json` envelope functions the upstream
/// CLI's inspect subcommand calls (mapped in Task 5 Step 1), with the same
/// `Document` construction (`Document::new`, default `DiagnosticPolicy`) and
/// the same trailing-newline behavior (upstream's `run_inspect_once` does
/// `writeln!(stdout, "{json}")`).
fn emit(kind: &str, raw: &[u8]) -> Result<Vec<u8>, String> {
    // Upstream's default `Encoding::Auto` (`resolved_encoding` with no
    // `-E`/config override) decodes valid UTF-8 as-is and falls back to
    // Shift_JIS otherwise; `ab_aozora_facade::encoding::decode_auto` is the
    // exact same function upstream's `aozora_fmt::decode(_, Encoding::Auto)`
    // arm delegates to.
    let source = ab_aozora_facade::encoding::decode_auto(raw)
        .map_err(|err| format!("input is neither valid UTF-8 nor Shift_JIS: {err}"))?
        .into_owned();

    let json = match kind {
        "gaiji" => json::gaiji(&source),
        "nodes" => json::nodes(&Document::new(source).parse()),
        "diagnostics" => json::diagnostics(Document::new(source).parse().diagnostics()),
        _ => unreachable!("kind validated in main"),
    };
    Ok(format!("{json}\n").into_bytes())
}
