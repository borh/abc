use anyhow::{Result, bail};
use serde_json::json;

mod aat;
mod metrics;
mod parser;
mod projection;
mod source;

use metrics::{FallbackDecision, FallbackReason};
use parser::ParsedSource;
pub use source::{DecodedSource, decode_source_bytes};

pub const VERSION: &str = "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let selection = source::select_body(&decoded);
    let parsed = parser::parse_with_aozora_rs(selection)?;
    let aat = build_aat(&decoded, &parsed);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(_bytes: &[u8]) -> Result<String> {
    bail!("aozora-rs-adapter --mode html is intentionally deferred to the render-diff phase")
}

fn build_aat(decoded: &DecodedSource, parsed: &ParsedSource<'_>) -> serde_json::Value {
    let result = build_aat_result(parsed);
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": aat::blocks_to_json(&result.blocks),
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": parsed.warnings.iter().map(|message| json!({ "message": message })).collect::<Vec<_>>()
        }
    })
}

fn build_aat_result(parsed: &ParsedSource<'_>) -> aat::AatBuildResult {
    let initial = aat::build_initial(parsed);
    let projection =
        projection::check(parsed.body.validation_body, &initial.projected.visible_text);
    let fallback = if parsed.body.validation_body.len() > 500_000 {
        FallbackDecision {
            used: true,
            reason: FallbackReason::LargeBody,
        }
    } else if !projection.in_source_order {
        FallbackDecision {
            used: true,
            reason: FallbackReason::ProjectionMismatch,
        }
    } else {
        FallbackDecision::none()
    };

    let (blocks, projected) = if fallback.used {
        aat::build_fallback(parsed.body.validation_body)
    } else {
        (initial.blocks, initial.projected)
    };

    aat::AatBuildResult {
        blocks,
        projected,
        fallback,
        timings: initial.timings,
        projection,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emits_schema_shaped_aat_for_ruby_and_gaiji() {
        let input = "\
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
吾輩《わがはい》は※［＃「口＋世」、U+546D］である。"
            .as_bytes();
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();

        assert_eq!(value["work_id"], "stdin");
        assert_eq!(value["meta"]["adapter"], "aozora-rs");
        assert_eq!(value["meta"]["source_encoding"], "utf-8");
        assert!(value["meta"]["parse_complete"].as_bool().unwrap());
        assert_eq!(value["blocks"][0]["kind"], "paragraph");
        assert!(
            value["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "ruby")
        );
        assert!(
            value["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "gaiji")
        );
    }
}
