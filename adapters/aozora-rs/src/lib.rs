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
const SOURCE_SUPPLEMENT_FALLBACK_THRESHOLD: usize = 1_024;

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
    let projection = ab_ir::blocks_to_aat_projection(&result.blocks);
    let semantic_summary = ab_ir::semantic_summary(&result.blocks, &projection.warnings);
    let mut warnings = warnings
        .iter()
        .map(|message| json!({ "message": message }))
        .collect::<Vec<_>>();
    warnings.extend(projection.warnings.iter().map(|warning| {
        json!({
            "message": warning.message,
            "path": warning.syntax_id
        })
    }));

    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": projection.blocks,
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": warnings,
            "metrics": metrics,
            "semantic_summary": semantic_summary
        }
    })
}

fn build_aat_result(parsed: &ParsedSource<'_>) -> (aat::AatBuildResult, Duration, Duration) {
    let initial = aat::build_initial(parsed);
    let large_body = parsed.body.validation_body.len() > 500_000;
    let mut projection_check = Duration::ZERO;
    let source_supplement_hotspot = ab_ir::provenance_counts(&initial.blocks).source_supplement
        > SOURCE_SUPPLEMENT_FALLBACK_THRESHOLD;
    if !large_body && source_supplement_hotspot {
        let fallback_start = Instant::now();
        let (blocks, _) = aat::build_fallback(parsed.body.validation_body);
        let fallback_build = fallback_start.elapsed();
        let fallback_provenance = ab_ir::provenance_counts(&blocks);
        if fallback_provenance.source_supplement == 0
            && aat::blocks_cover_validation_annotations(parsed.body.validation_body, &blocks)
        {
            return (
                aat::AatBuildResult {
                    blocks,
                    fallback: FallbackDecision {
                        used: true,
                        reason: FallbackReason::SourceSupplementHotspot,
                    },
                    timings: initial.timings,
                },
                projection_check,
                fallback_build,
            );
        }
    }

    let (fallback, source_visible_for_fallback) = if large_body {
        (
            FallbackDecision {
                used: true,
                reason: FallbackReason::LargeBody,
            },
            None,
        )
    } else {
        let projection_start = Instant::now();
        let projection =
            projection::check(parsed.body.validation_body, &initial.projected.visible_text);
        projection_check = projection_start.elapsed();
        if projection.in_source_order {
            (FallbackDecision::none(), None)
        } else {
            (
                FallbackDecision {
                    used: true,
                    reason: FallbackReason::ProjectionMismatch,
                },
                projection.source_visible_text,
            )
        }
    };

    let mut fallback_build = Duration::ZERO;
    let blocks = if fallback.used {
        let fallback_start = Instant::now();
        let (blocks, _) = if let Some(source_visible) = source_visible_for_fallback {
            aat::build_fallback_from_source_visible(parsed.body.validation_body, source_visible)
        } else {
            aat::build_fallback(parsed.body.validation_body)
        };
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
        assert!(metrics["source_supplement_nodes"].as_u64().unwrap() > 0);
        assert!(
            value["meta"]["semantic_summary"]["syntax"]
                .as_object()
                .unwrap()
                .contains_key("ruby.basic")
        );
        assert!(
            value["meta"]["semantic_summary"]["syntax"]
                .as_object()
                .unwrap()
                .contains_key("gaiji.marker")
        );
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
            found_separators: true,
            elapsed: Duration::ZERO,
        })
        .unwrap();
        parsed.body = source::BodySelection {
            validation_body: &large_body,
            found_separators: true,
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

    #[test]
    fn fallback_is_reported_for_source_supplement_hotspots() {
        let body = (0..1_100)
            .map(|idx| format!("侍童《こしゃう{idx}》。\n"))
            .collect::<String>();
        let decoded = decode_source_bytes(body.as_bytes()).unwrap();
        let selection = source::BodySelection {
            validation_body: &decoded.text,
            found_separators: true,
            elapsed: Duration::ZERO,
        };
        let parsed = ParsedSource {
            body: selection,
            parse_body: parser::ParseBodyDecision {
                parser_body: &decoded.text,
                strategy: parser::ParseBodyStrategy::SeparatorFallback,
            },
            retokenized: Vec::new(),
            warnings: Vec::new(),
            tokenized_count: 0,
            retokenized_count: 0,
            timings: parser::ParseTimings {
                body_selection: Duration::ZERO,
                tokenize: Duration::ZERO,
                scopenize: Duration::ZERO,
                retokenize: Duration::ZERO,
            },
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
        let value = metrics.to_json();

        assert_eq!(value["fallback_used"], true);
        assert_eq!(value["fallback_reason"], "source_supplement_hotspot");
        assert_eq!(value["source_supplement_nodes"], 0);
        assert!(value["source_fallback_nodes"].as_u64().unwrap() > 1_000);
    }

    #[test]
    fn source_supplement_hotspot_keeps_initial_when_fallback_drops_validation_markers() {
        let body = (0..1_100)
            .map(|idx| {
                format!(
                    "軌［＃「軌」に「（ママ）」の注記］り［＃「軌［＃「軌」に「（ママ）」の注記］り」は底本では「軌《きし{idx}》り」］\n"
                )
            })
            .collect::<String>();
        let decoded = decode_source_bytes(body.as_bytes()).unwrap();
        let selection = source::BodySelection {
            validation_body: &decoded.text,
            found_separators: true,
            elapsed: Duration::ZERO,
        };
        let parsed = ParsedSource {
            body: selection,
            parse_body: parser::ParseBodyDecision {
                parser_body: &decoded.text,
                strategy: parser::ParseBodyStrategy::SeparatorFallback,
            },
            retokenized: Vec::new(),
            warnings: Vec::new(),
            tokenized_count: 0,
            retokenized_count: 0,
            timings: parser::ParseTimings {
                body_selection: Duration::ZERO,
                tokenize: Duration::ZERO,
                scopenize: Duration::ZERO,
                retokenize: Duration::ZERO,
            },
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
        let value = metrics.to_json();

        assert_eq!(value["fallback_used"], false);
        assert_eq!(value["fallback_reason"], "none");
        assert!(value["source_supplement_nodes"].as_u64().unwrap() > 1_000);
    }
}
