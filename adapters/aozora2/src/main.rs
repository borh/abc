use std::io::{self, Read};

use anyhow::{Result, bail};
use clap::{Parser, ValueEnum};
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::Serialize;
use serde_json::json;
use sha2::{Digest, Sha256};

const VERSION: &str = "aozora2-adapter 0.1.0 93420b53c7d52579a0ca3fde466cef8ce6d89879";

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

#[derive(Debug)]
struct DecodedSource {
    text: String,
    encoding: &'static str,
    source_hash: String,
}

#[derive(Debug, Serialize)]
struct Span {
    line_start: usize,
    line_end: usize,
    byte_start: usize,
    byte_end: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();
    if args.version {
        println!("{VERSION}");
        return Ok(());
    }

    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    let decoded = decode_source_bytes(&bytes)?;

    match args.mode.unwrap_or(Mode::Aat) {
        Mode::Aat => {
            let aat = build_aat(&decoded);
            serde_json::to_writer_pretty(io::stdout(), &aat)?;
            println!();
        }
        Mode::Html => {
            println!("<p>{}</p>", html_escape(&decoded.text));
        }
    }
    Ok(())
}

fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    if had_errors {
        bail!("source is neither valid UTF-8 nor decodable Windows-31J");
    }
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: "windows-31j",
        source_hash,
    })
}

fn build_aat(decoded: &DecodedSource) -> serde_json::Value {
    let content = parse_inline_content(&decoded.text);
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [
            {
                "kind": "paragraph",
                "content": content
            }
        ],
        "meta": {
            "adapter": "aozora2",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": []
        }
    })
}

fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    let pattern = Regex::new(
        r"(?P<ruby_base>[^｜\s《》※［＃]+)《(?P<reading>[^》]+)》|※［＃(?P<gaiji>[^］]+)］",
    )
    .unwrap();
    let mut content = Vec::new();
    let mut pos = 0;
    for capture in pattern.captures_iter(text) {
        let whole = capture.get(0).unwrap();
        if whole.start() > pos {
            content.push(json!({
                "kind": "text",
                "value": &text[pos..whole.start()],
                "span": span_for(text, pos, whole.start())
            }));
        }
        if let Some(reading) = capture.name("reading") {
            content.push(json!({
                "kind": "ruby",
                "base": capture.name("ruby_base").unwrap().as_str(),
                "reading": reading.as_str(),
                "span": span_for(text, whole.start(), whole.end())
            }));
        } else if let Some(gaiji) = capture.name("gaiji") {
            let description = gaiji.as_str();
            let resolved = unicode_from_description(description);
            content.push(json!({
                "kind": "gaiji",
                "description": description,
                "resolved": resolved,
                "jis_code": null,
                "unresolved_reason": if resolved.is_some() { None::<String> } else { Some("not resolved by aozora2-adapter".to_owned()) },
                "span": span_for(text, whole.start(), whole.end())
            }));
        }
        pos = whole.end();
    }
    if pos < text.len() {
        content.push(json!({
            "kind": "text",
            "value": &text[pos..],
            "span": span_for(text, pos, text.len())
        }));
    }
    if content.is_empty() {
        content.push(json!({"kind": "text", "value": ""}));
    }
    content
}

fn span_for(text: &str, start: usize, end: usize) -> Span {
    let line = text[..start].chars().filter(|ch| *ch == '\n').count() + 1;
    Span {
        line_start: line,
        line_end: line,
        byte_start: text[..start].len(),
        byte_end: text[..end].len(),
    }
}

fn unicode_from_description(description: &str) -> Option<String> {
    let codepoint = Regex::new(r"U\+([0-9A-Fa-f]{4,6})").unwrap();
    let value = u32::from_str_radix(codepoint.captures(description)?.get(1)?.as_str(), 16).ok()?;
    char::from_u32(value).map(|ch| ch.to_string())
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

fn html_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}
