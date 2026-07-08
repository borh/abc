use std::time::{Duration, Instant};

use ab_source_syntax::SourceEvent;
use anyhow::{Result, bail};
use serde_json::json;
mod aat;
mod dump;
mod metrics;
mod parser;
mod projection;
mod source;

use metrics::{AdapterMetrics, AdapterMetricsParts, FallbackDecision, FallbackReason};
use parser::ParsedSource;
pub use dump::retokenized_dump_json;
pub use source::{DecodedSource, decode_source_bytes};

const LARGE_BODY_BYTES: usize = 500_000;

pub const VERSION: &str = "aozora-rs-adapter 0.1.0 aozora-rs-v0.6.0";

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decode_start = Instant::now();
    let decoded = decode_source_bytes(bytes)?;
    let decode = decode_start.elapsed();
    let selection = source::select_body(&decoded);

    if selection.validation_body.len() > LARGE_BODY_BYTES {
        let aat = build_large_body_fallback_aat(&decoded, &selection);
        let mut out = Vec::new();
        serde_json::to_writer(&mut out, &aat)?;
        out.push(b'\n');
        return Ok(out);
    }

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

fn build_large_body_fallback_aat(
    decoded: &DecodedSource,
    selection: &source::BodySelection<'_>,
) -> serde_json::Value {
    let has_markup = ab_source_syntax::needs_lossy_projection(selection.validation_body);
    let (blocks, fallback_build) =
        build_fallback_blocks(selection.validation_body, has_markup, None, None, None);
    let parse = parser::ParseTimings {
        body_selection: selection.elapsed,
        tokenize: Duration::ZERO,
        scopenize: Duration::ZERO,
        retokenize: Duration::ZERO,
    };
    let result = aat::AatBuildResult {
        blocks,
        fallback: FallbackDecision {
            used: true,
            reason: FallbackReason::LargeBody,
        },
        timings: aat::AatBuildTimings {
            build: Duration::ZERO,
        },
    };
    let metrics = AdapterMetrics::from_parts(AdapterMetricsParts {
        decoded,
        decode: Duration::ZERO,
        body: selection,
        parse,
        aat: result.timings,
        projection_check: Duration::ZERO,
        fallback_build,
        parse_body_strategy: parser::ParseBodyStrategy::SeparatorFallback,
        parser_body_bytes: selection.validation_body.len(),
        tokenized_count: 0,
        retokenized_count: 0,
        provenance: ab_ir::provenance_counts(&result.blocks),
        fallback: result.fallback.clone(),
    });
    let warnings = Vec::<String>::new();
    build_aat(decoded, &warnings, &result, metrics.to_json())
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
            "parse_complete": !result.fallback.used,
            "warnings": warnings,
            "metrics": metrics,
            "semantic_summary": semantic_summary
        }
    })
}

fn build_fallback_blocks(
    validation_body: &str,
    has_markup: bool,
    source_events: Option<&[SourceEvent<'_>]>,
    source_annotations_both: Option<&ab_source_syntax::SourceAnnotationsBoth<'_>>,
    source_visible: Option<String>,
) -> (Vec<ab_ir::Block>, Duration) {
    let fallback_start = Instant::now();
    let (blocks, _) = if has_markup {
        let borrowed_events = if let Some(events) = source_events {
            events
        } else {
            return {
                let events = ab_source_syntax::source_events(validation_body);
                let (blocks, _) =
                    aat::build_fallback_from_events_with_annotations(&events, source_visible, None);
                (blocks, fallback_start.elapsed())
            };
        };
        aat::build_fallback_from_events_with_annotations(
            borrowed_events,
            source_visible,
            source_annotations_both,
        )
    } else {
        aat::build_fallback_without_annotations(validation_body)
    };
    (blocks, fallback_start.elapsed())
}

fn build_aat_result(parsed: &ParsedSource<'_>) -> (aat::AatBuildResult, Duration, Duration) {
    let validation_body = parsed.body.validation_body;
    let large_body = validation_body.len() > 500_000;
    let has_markup = ab_source_syntax::needs_lossy_projection(validation_body);
    let needs_structured_source = needs_structured_source_fallback(validation_body);
    let parser_failed = parsed.warnings_summary.has_parser_failures();

    let mut projection_check = Duration::ZERO;
    let source_artifacts: Option<source::SourceArtifacts<'_>> =
        if (has_markup || needs_structured_source) && !large_body {
        Some(source::SourceArtifacts::collect(validation_body))
    } else {
        None
    };
    let mut fallback = FallbackDecision::none();
    let mut fallback_source_visible = None;
    let mut initial = None;

    if needs_structured_source && !large_body {
        fallback = FallbackDecision {
            used: true,
            reason: if parser_failed {
                FallbackReason::ParserFailure
            } else {
                FallbackReason::ProjectionMismatch
            },
        };
    } else if has_markup && !large_body {
        if parsed.retokenized.is_empty() {
            fallback = FallbackDecision {
                used: true,
                reason: if parser_failed {
                    FallbackReason::ParserFailure
                } else {
                    FallbackReason::ProjectionMismatch
                },
            };
        } else if parser_failed {
            fallback = FallbackDecision {
                used: true,
                reason: FallbackReason::ParserFailure,
            };
            fallback_source_visible =
                Some(source_artifacts.as_ref().unwrap().source_visible.to_owned());
        } else {
            let source_artifacts = source_artifacts
        .as_ref()
        .expect("source artifacts should exist for markup validation when not using large body fallback");
            let source_visible = &source_artifacts.source_visible;
            let projected_visible = aat::projected_visible_from_retokenized(&parsed.retokenized);
            let projection_start = Instant::now();
            let projection =
                projection::check_with_source_visible(source_visible, &projected_visible);
            projection_check = projection_start.elapsed();

            if !projection.in_source_order
                || projection.source_visible_chars != projection.projected_visible_chars
            {
                fallback = FallbackDecision {
                    used: true,
                    reason: FallbackReason::ProjectionMismatch,
                };
                fallback_source_visible = projection
                    .source_visible_text
                    .or_else(|| Some(source_visible.to_owned()));
            } else {
                let base = aat::build_initial_without_source_annotations(parsed);
                let result = aat::build_initial_with_existing_blocks_with_annotations(
                    base,
                    validation_body,
                    &source_artifacts.events,
                    &source_artifacts.annotations_both,
                );
                initial = Some(result);
            }
        }
    } else if large_body {
        fallback = FallbackDecision {
            used: true,
            reason: FallbackReason::LargeBody,
        };
    }

    let mut fallback_build = Duration::ZERO;
    let (blocks, timings) = if fallback.used {
        let source_events = source_artifacts
            .as_ref()
            .map(|artifacts| artifacts.events.as_slice());
        let source_annotations_both = source_artifacts
            .as_ref()
            .map(|artifacts| &artifacts.annotations_both);
        let (blocks, fallback_ms) = build_fallback_blocks(
            validation_body,
            has_markup || needs_structured_source,
            source_events,
            source_annotations_both,
            fallback_source_visible,
        );
        fallback_build = fallback_ms;
        (
            blocks,
            aat::AatBuildTimings {
                build: Duration::ZERO,
            },
        )
    } else if has_markup {
        let result = initial.expect("initial should be built when markup exists and no fallback");
        (result.blocks, result.timings)
    } else {
        let initial = aat::build_initial_without_source_annotations(parsed);
        (initial.blocks, initial.timings)
    };

    (
        aat::AatBuildResult {
            blocks,
            fallback,
            timings,
        },
        projection_check,
        fallback_build,
    )
}

fn needs_structured_source_fallback(body: &str) -> bool {
    body.contains("／＼")
        || body.contains('〔')
        || body.contains("）入る")
        || ab_source_syntax::source_events(body)
            .iter()
            .any(|event| matches!(event.kind, ab_source_syntax::SourceEventKind::Command { .. }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ParserWarnings;

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
        assert_eq!(metrics["source_supplement_nodes"], 0);
        assert!(metrics["parser_normalized_nodes"].as_u64().unwrap() > 0);
        assert_eq!(metrics["source_fallback_nodes"], 0);
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
        assert_eq!(value["meta"]["parse_complete"], false);
    }

    #[test]
    fn fallback_is_reported_for_empty_parser_projection() {
        let body = "侍童《こしゃう》。\n".to_owned();
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
            warnings_summary: ParserWarnings::default(),
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
        assert_eq!(value["fallback_reason"], "projection_mismatch");
        assert_eq!(value["source_supplement_nodes"], 0);
        assert!(value["source_fallback_nodes"].as_u64().unwrap() > 0);
    }

    #[test]
    fn fallback_is_reported_as_parser_failure_when_parser_errors_present() {
        let body = "侍童《こしゃう》。\n".to_owned();
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
            warnings: vec!["scopenize failure".to_owned()],
            warnings_summary: ParserWarnings {
                meta_parse_warning: false,
                scopenize_errors: 1,
                retokenize_errors: 0,
            },
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

        assert_eq!(metrics.to_json()["fallback_used"], true);
        assert_eq!(metrics.to_json()["fallback_reason"], "parser_failure");
    }

    #[test]
    fn parser_normalized_gaiji_does_not_force_whole_work_fallback() {
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
        let metrics = &value["meta"]["metrics"];
        let content = value["blocks"][0]["content"].as_array().unwrap();
        let gaiji = content
            .iter()
            .find(|node| node["kind"] == "gaiji")
            .expect("gaiji node");

        assert_eq!(metrics["fallback_used"], false);
        assert_eq!(metrics["fallback_reason"], "none");
        assert!(metrics["parser_nodes"].as_u64().unwrap() > 0);
        assert_eq!(metrics["source_supplement_nodes"], 0);
        assert!(metrics["parser_normalized_nodes"].as_u64().unwrap() > 0);
        assert_eq!(metrics["source_fallback_nodes"], 0);
        assert_eq!(gaiji["resolved"], "呭");
    }

    #[test]
    fn parser_normalized_jis_gaiji_uses_oracle_jis_supplement() {
        let input = "\
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて"
            .as_bytes();
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();
        let content = value["blocks"][0]["content"].as_array().unwrap();
        let gaiji = content
            .iter()
            .find(|node| node["kind"] == "gaiji")
            .expect("gaiji node");

        assert_eq!(gaiji["description"], "「てへん＋掌」、第4水準2-13-47");
        assert_eq!(gaiji["resolved"], "撑");
        assert_eq!(gaiji["jis_code"], "2-13-47");
        assert_eq!(gaiji["unresolved_reason"], serde_json::Value::Null);
    }
}
