//! Locate the first character where the AAT visible-text projection is not a
//! subsequence of the source lossy-body projection, mirroring the exact
//! `visible_text_body_order` property in `crates/ab-check/src/properties.rs`.
//! Used to characterize the 668 VTBO failures.

use std::path::PathBuf;

use ab_check::aat::comparison_visible_text_projection;
use ab_check::encoding::decode_source_bytes;
use ab_check::properties::body_text;
use ab_check::source_projection::comparison_lossy_body;
use serde_json::Value;

#[derive(clap::Parser)]
#[command(name = "vtbo_locate")]
struct Args {
    /// Path to the AAT JSON file (persisted filenames are suffixed, e.g.
    /// 000081_4418-04cb6bb131bc.json; resolve by glob outside this binary).
    #[arg(long)]
    aat: PathBuf,
    /// Path to the RAW Aozora source BYTES (often Windows-31J; decoded here).
    #[arg(long)]
    source: PathBuf,
    /// How many chars of surrounding context to print around the divergence.
    #[arg(long, default_value_t = 40)]
    context: usize,
}

/// Exact copy of `ab_check::properties::normalize_visible` (which is private).
/// NFKC-normalize then collapse runs of whitespace to a single space.
fn normalize_visible(value: &str) -> String {
    use unicode_normalization::UnicodeNormalization;
    value
        .nfkc()
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

/// Same subsequence check the property uses (`is_subsequence` in properties.rs):
/// walk projection chars, consuming the haystack via `.any()`; the first char
/// that cannot be matched is the divergence. Returns
/// (divergence_index_in_projection, source_chars_consumed_so_far).
fn first_non_subsequence(projection: &str, source: &str) -> Option<(usize, usize)> {
    let mut si = 0usize;
    let mut source_chars = source.chars();
    for (pi, pc) in projection.chars().enumerate() {
        let mut found = false;
        for sc in source_chars.by_ref() {
            si += 1;
            if sc == pc {
                found = true;
                break;
            }
        }
        if !found {
            return Some((pi, si));
        }
    }
    None
}

fn main() -> anyhow::Result<()> {
    use clap::Parser;
    let args = Args::parse();

    let aat: Value = serde_json::from_str(&std::fs::read_to_string(&args.aat)?)?;

    // Read raw BYTES and decode the same way the harness does (properties.rs is
    // called from check.rs with decode_source_bytes(&txt_bytes).text). Reading
    // with read_to_string would fail on Windows-31J sources.
    let source_bytes = std::fs::read(&args.source)?;
    let decoded = decode_source_bytes(&source_bytes)?;

    // Mirror properties.rs:81 exactly:
    //   normalize_visible(&source_projection::comparison_lossy_body(body_text(txt)))
    let source = normalize_visible(&comparison_lossy_body(body_text(&decoded.text)));
    let projection = normalize_visible(&comparison_visible_text_projection(&aat));

    match first_non_subsequence(&projection, &source) {
        None => {
            println!("OK: projection is a subsequence of source");
            Ok(())
        }
        Some((pi, si)) => {
            let p: Vec<char> = projection.chars().collect();
            let s: Vec<char> = source.chars().collect();
            let pstart = pi.saturating_sub(args.context);
            let pend = (pi + args.context).min(p.len());
            let sstart = si.saturating_sub(args.context);
            let send = (si + args.context).min(s.len());
            println!("divergence_at_projection_index={}", pi);
            println!("source_cursor_consumed={}", si);
            println!("projection[{}..{}]={}", pstart, pend, p[pstart..pend].iter().collect::<String>());
            println!("source[{}..{}]={}", sstart, send, s[sstart..send].iter().collect::<String>());
            Ok(())
        }
    }
}
