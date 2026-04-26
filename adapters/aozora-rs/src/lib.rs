use std::time::{Duration, Instant};

use anyhow::{Result, bail};
use serde_json::json;

mod aat;
mod metrics;
mod parser;
mod projection;
mod source;

use metrics::{AdapterMetrics, AdapterMetricsParts, FallbackDecision, FallbackReason};
use parser::ParsedSource;
pub use source::{DecodedSource, decode_source_bytes};

pub const VERSION: &str = "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decode_start = Instant::now();
    let decoded = decode_source_bytes(bytes)?;
    let decode = decode_start.elapsed();
    let selection = source::select_body(&decoded);
    let parsed = parser::parse_with_aozora_rs(selection)?;
    let (result, projection_check, fallback_build) = build_aat_result(&parsed);
    let metrics = AdapterMetrics::from_parts(AdapterMetricsParts {
        decoded: &decoded,
        decode,
        body: &parsed.body,
        parse: parsed.timings,
        aat: result.timings,
        projection_check,
        fallback_build,
        parse_body_strategy: parsed.parse_body.strategy,
        parser_body_bytes: parsed.parse_body.parser_body.len(),
        tokenized_count: parsed.tokenized_count,
        retokenized_count: parsed.retokenized_count,
        provenance: ab_ir::provenance_counts(&result.blocks),
        fallback: result.fallback.clone(),
    });
    let aat = build_aat(&decoded, &parsed.warnings, &result, metrics.to_json());
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(_bytes: &[u8]) -> Result<String> {
    bail!("aozora-rs-adapter --mode html is intentionally deferred to the render-diff phase")
}

fn build_aat(
    decoded: &DecodedSource,
    warnings: &[String],
    result: &aat::AatBuildResult,
    metrics: serde_json::Value,
) -> serde_json::Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": ab_ir::blocks_to_aat_json(&result.blocks),
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": warnings.iter().map(|message| json!({ "message": message })).collect::<Vec<_>>(),
            "metrics": metrics
        }
    })
}

fn build_aat_result(parsed: &ParsedSource<'_>) -> (aat::AatBuildResult, Duration, Duration) {
    let initial = aat::build_initial(parsed);
    let projection_start = Instant::now();
    let projection =
        projection::check(parsed.body.validation_body, &initial.projected.visible_text);
    let projection_check = projection_start.elapsed();
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

    let mut fallback_build = Duration::ZERO;
    let blocks = if fallback.used {
        let fallback_start = Instant::now();
        let (blocks, _) = aat::build_fallback(parsed.body.validation_body);
        fallback_build = fallback_start.elapsed();
        blocks
    } else {
        initial.blocks
    };

    (
        aat::AatBuildResult {
            blocks,
            fallback,
            timings: initial.timings,
        },
        projection_check,
        fallback_build,
    )
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
        let metrics = &value["meta"]["metrics"];
        assert!(metrics["decode_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["body_selection_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["tokenize_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["scopenize_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["retokenize_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["aat_build_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["projection_check_ms"].as_f64().unwrap() >= 0.0);
        assert!(metrics["fallback_build_ms"].as_f64().unwrap() >= 0.0);
        assert_eq!(metrics["fallback_used"], false);
        assert_eq!(metrics["fallback_reason"], "none");
        assert_eq!(metrics["source_bytes"], input.len());
        assert!(metrics["tokenized_count"].as_u64().unwrap() > 0);
        assert!(metrics["retokenized_count"].as_u64().unwrap() > 0);
        assert!(metrics["parser_normalized_nodes"].as_u64().unwrap() > 0);
        assert!(metrics["regex_supplement_nodes"].as_u64().unwrap() > 0);
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

    #[test]
    fn fallback_is_reported_in_metrics_for_large_body() {
        let large_body = "本文".repeat(260_000);
        let decoded = decode_source_bytes(large_body.as_bytes()).unwrap();
        let mut parsed = parser::parse_with_aozora_rs(source::BodySelection {
            validation_body: "本文\n",
            elapsed: Duration::ZERO,
        })
        .unwrap();
        parsed.body = source::BodySelection {
            validation_body: &large_body,
            elapsed: Duration::ZERO,
        };

        let (result, projection_check, fallback_build) = build_aat_result(&parsed);
        let metrics = AdapterMetrics::from_parts(AdapterMetricsParts {
            decoded: &decoded,
            decode: Duration::ZERO,
            body: &parsed.body,
            parse: parsed.timings,
            aat: result.timings,
            projection_check,
            fallback_build,
            parse_body_strategy: parsed.parse_body.strategy,
            parser_body_bytes: parsed.parse_body.parser_body.len(),
            tokenized_count: parsed.tokenized_count,
            retokenized_count: parsed.retokenized_count,
            provenance: ab_ir::provenance_counts(&result.blocks),
            fallback: result.fallback.clone(),
        });
        let value = build_aat(&decoded, &parsed.warnings, &result, metrics.to_json());

        assert_eq!(value["meta"]["metrics"]["fallback_used"], true);
        assert_eq!(value["meta"]["metrics"]["fallback_reason"], "large_body");
        assert_eq!(value["meta"]["parse_complete"], true);
    }
}
