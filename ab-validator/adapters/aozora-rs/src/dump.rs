//! Measurement-only mode: emit aozora-rs-core's `retokenized` token stream as
//! JSON, bypassing the AAT fidelity gate/fallback. This exposes what the parser
//! *actually recognizes*, so its capability can be measured faithfully against
//! the conformance suite (the production AAT path masks it behind a lexer
//! fallback whenever the typed projection does not round-trip). Not part of the
//! production AAT contract.
use anyhow::Result;
use aozora_rs_core::{Deco, Retokenized};

use crate::{decode_source_bytes, parser, source};

fn deco_tag(deco: &Deco<'_>) -> &'static str {
    match deco {
        Deco::Bold => "bold",
        Deco::Italic => "italic",
        Deco::Ruby(_) => "ruby",
        Deco::Bosen(_) => "bosen",
        Deco::Boten(_) => "boten",
        Deco::Indent(_) => "indent",
        Deco::Hanging(_) => "hanging",
        Deco::Grounded => "grounded",
        Deco::LowFlying(_) => "low_flying",
        Deco::AHead => "a_head",
        Deco::BHead => "b_head",
        Deco::CHead => "c_head",
        Deco::HinV => "hin_v",
        Deco::Mama => "mama",
        Deco::Smaller(_) => "smaller",
        Deco::Bigger(_) => "bigger",
        Deco::VHCentre => "vh_centre",
        Deco::Warichu => "warichu",
        Deco::HorizontalLayout => "horizontal_layout",
        Deco::Kerning(_) => "kerning",
        Deco::Sub => "sub",
        Deco::Sup => "sup",
    }
}

pub fn retokenized_dump_json(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let selection = source::select_body(&decoded);
    let parsed = parser::parse_with_aozora_rs(selection)?;

    let tokens: Vec<serde_json::Value> = parsed
        .retokenized
        .iter()
        .map(|token| match token {
            Retokenized::Text(text) => serde_json::json!({"t": "text", "v": text}),
            Retokenized::Kunten(text) => serde_json::json!({"t": "kunten", "v": text}),
            Retokenized::Okurigana(text) => serde_json::json!({"t": "okurigana", "v": text}),
            Retokenized::Br => serde_json::json!({"t": "br"}),
            Retokenized::Figure(_) => serde_json::json!({"t": "figure"}),
            Retokenized::DecoBegin(deco) => {
                serde_json::json!({"t": "deco_begin", "d": deco_tag(deco)})
            }
            Retokenized::DecoEnd(deco) => {
                serde_json::json!({"t": "deco_end", "d": deco_tag(deco)})
            }
        })
        .collect();

    let mut out = serde_json::to_vec(&serde_json::json!({ "retokenized": tokens }))?;
    out.push(b'\n');
    Ok(out)
}
