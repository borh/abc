use std::time::Duration;

use serde::Serialize;
use serde_json::json;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FallbackDecision {
    pub used: bool,
    pub reason: FallbackReason,
}

impl FallbackDecision {
    pub fn none() -> Self {
        Self {
            used: false,
            reason: FallbackReason::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FallbackReason {
    None,
    ProjectionMismatch,
    LargeBody,
    ParserFailure,
}

impl Serialize for FallbackReason {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::None => serializer.serialize_str("none"),
            Self::ProjectionMismatch => serializer.serialize_str("projection_mismatch"),
            Self::LargeBody => serializer.serialize_str("large_body"),
            Self::ParserFailure => serializer.serialize_str("parser_failure"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdapterMetrics {
    pub decode: Duration,
    pub body_selection: Duration,
    pub tokenize: Duration,
    pub scopenize: Duration,
    pub retokenize: Duration,
    pub aat_build: Duration,
    pub projection_check: Duration,
    pub fallback_build: Duration,
    pub source_bytes: usize,
    pub validation_body_bytes: usize,
    pub parse_body_strategy: &'static str,
    pub parser_body_bytes: usize,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub parser_nodes: usize,
    pub parser_normalized_nodes: usize,
    pub source_supplement_nodes: usize,
    pub source_fallback_nodes: usize,
    pub fallback: FallbackDecision,
}

#[derive(Debug, Clone)]
pub struct AdapterMetricsParts<'a> {
    pub decoded: &'a crate::source::DecodedSource,
    pub decode: Duration,
    pub body: &'a crate::source::BodySelection<'a>,
    pub parse: crate::parser::ParseTimings,
    pub aat: crate::aat::AatBuildTimings,
    pub projection_check: Duration,
    pub fallback_build: Duration,
    pub parse_body_strategy: crate::parser::ParseBodyStrategy,
    pub parser_body_bytes: usize,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub provenance: ab_ir::ProvenanceCounts,
    pub fallback: FallbackDecision,
}

impl AdapterMetrics {
    pub fn from_parts(parts: AdapterMetricsParts<'_>) -> Self {
        Self {
            decode: parts.decode,
            body_selection: parts.parse.body_selection,
            tokenize: parts.parse.tokenize,
            scopenize: parts.parse.scopenize,
            retokenize: parts.parse.retokenize,
            aat_build: parts.aat.build,
            projection_check: parts.projection_check,
            fallback_build: parts.fallback_build,
            source_bytes: parts.decoded.source_bytes,
            validation_body_bytes: parts.body.validation_body.len(),
            parse_body_strategy: parse_body_strategy_name(parts.parse_body_strategy),
            parser_body_bytes: parts.parser_body_bytes,
            tokenized_count: parts.tokenized_count,
            retokenized_count: parts.retokenized_count,
            parser_nodes: parts.provenance.parser,
            parser_normalized_nodes: parts.provenance.parser_normalized,
            source_supplement_nodes: parts.provenance.source_supplement,
            source_fallback_nodes: parts.provenance.source_fallback,
            fallback: parts.fallback,
        }
    }

    pub fn to_json(&self) -> serde_json::Value {
        json!({
            "decode_ms": ms(self.decode),
            "body_selection_ms": ms(self.body_selection),
            "tokenize_ms": ms(self.tokenize),
            "scopenize_ms": ms(self.scopenize),
            "retokenize_ms": ms(self.retokenize),
            "aat_build_ms": ms(self.aat_build),
            "projection_check_ms": ms(self.projection_check),
            "fallback_build_ms": ms(self.fallback_build),
            "source_bytes": self.source_bytes,
            "validation_body_bytes": self.validation_body_bytes,
            "parse_body_strategy": self.parse_body_strategy,
            "parser_body_bytes": self.parser_body_bytes,
            "tokenized_count": self.tokenized_count,
            "retokenized_count": self.retokenized_count,
            "parser_nodes": self.parser_nodes,
            "parser_normalized_nodes": self.parser_normalized_nodes,
            "source_supplement_nodes": self.source_supplement_nodes,
            "source_fallback_nodes": self.source_fallback_nodes,
            "fallback_used": self.fallback.used,
            "fallback_reason": self.fallback.reason,
        })
    }
}

fn ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

fn parse_body_strategy_name(strategy: crate::parser::ParseBodyStrategy) -> &'static str {
    match strategy {
        crate::parser::ParseBodyStrategy::ParserMeta => "parser_meta",
        crate::parser::ParseBodyStrategy::SeparatorFallback => "separator_fallback",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fallback_reason_serializes_as_plain_string() {
        assert_eq!(serde_json::to_value(FallbackReason::None).unwrap(), "none");
        assert_eq!(
            serde_json::to_value(FallbackReason::ProjectionMismatch).unwrap(),
            "projection_mismatch"
        );
        assert_eq!(
            serde_json::to_value(FallbackReason::LargeBody).unwrap(),
            "large_body"
        );
        assert_eq!(
            serde_json::to_value(FallbackReason::ParserFailure).unwrap(),
            "parser_failure"
        );
    }

    #[test]
    fn adapter_metrics_serializes_parse_body_strategy() {
        let metrics = AdapterMetrics {
            decode: Duration::ZERO,
            body_selection: Duration::ZERO,
            tokenize: Duration::ZERO,
            scopenize: Duration::ZERO,
            retokenize: Duration::ZERO,
            aat_build: Duration::ZERO,
            projection_check: Duration::ZERO,
            fallback_build: Duration::ZERO,
            source_bytes: 10,
            validation_body_bytes: 11,
            parse_body_strategy: "separator_fallback",
            parser_body_bytes: 12,
            tokenized_count: 13,
            retokenized_count: 14,
            parser_nodes: 15,
            parser_normalized_nodes: 16,
            source_supplement_nodes: 17,
            source_fallback_nodes: 18,
            fallback: FallbackDecision {
                used: true,
                reason: FallbackReason::ProjectionMismatch,
            },
        };

        assert_eq!(
            metrics.to_json()["parse_body_strategy"],
            "separator_fallback"
        );
        assert_eq!(metrics.to_json()["source_supplement_nodes"], 17);
    }
}
