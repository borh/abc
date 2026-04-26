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
    pub parser_body_bytes: usize,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub fallback: FallbackDecision,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StageTiming {
    pub stage: &'static str,
    pub elapsed: Duration,
}

impl AdapterMetrics {
    pub fn from_parts(
        decoded: &crate::source::DecodedSource,
        decode: Duration,
        body: &crate::source::BodySelection<'_>,
        parse: crate::parser::ParseTimings,
        aat: crate::aat::AatBuildTimings,
        projection_check: Duration,
        fallback_build: Duration,
        parser_body_bytes: usize,
        tokenized_count: usize,
        retokenized_count: usize,
        fallback: FallbackDecision,
    ) -> Self {
        Self {
            decode,
            body_selection: parse.body_selection,
            tokenize: parse.tokenize,
            scopenize: parse.scopenize,
            retokenize: parse.retokenize,
            aat_build: aat.build,
            projection_check,
            fallback_build,
            source_bytes: decoded.source_bytes,
            validation_body_bytes: body.validation_body.len(),
            parser_body_bytes,
            tokenized_count,
            retokenized_count,
            fallback,
        }
    }

    pub fn stages(&self) -> Vec<StageTiming> {
        vec![
            StageTiming {
                stage: "decode",
                elapsed: self.decode,
            },
            StageTiming {
                stage: "body_selection",
                elapsed: self.body_selection,
            },
            StageTiming {
                stage: "tokenize",
                elapsed: self.tokenize,
            },
            StageTiming {
                stage: "scopenize",
                elapsed: self.scopenize,
            },
            StageTiming {
                stage: "retokenize",
                elapsed: self.retokenize,
            },
            StageTiming {
                stage: "aat_build",
                elapsed: self.aat_build,
            },
            StageTiming {
                stage: "projection_check",
                elapsed: self.projection_check,
            },
            StageTiming {
                stage: "fallback_build",
                elapsed: self.fallback_build,
            },
        ]
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
            "parser_body_bytes": self.parser_body_bytes,
            "tokenized_count": self.tokenized_count,
            "retokenized_count": self.retokenized_count,
            "fallback_used": self.fallback.used,
            "fallback_reason": self.fallback.reason,
        })
    }
}

fn ms(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1000.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

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
    }

    #[test]
    fn adapter_metrics_exposes_ordered_stages() {
        let metrics = AdapterMetrics {
            decode: Duration::from_millis(1),
            body_selection: Duration::from_millis(2),
            tokenize: Duration::from_millis(3),
            scopenize: Duration::from_millis(4),
            retokenize: Duration::from_millis(5),
            aat_build: Duration::from_millis(6),
            projection_check: Duration::from_millis(7),
            fallback_build: Duration::from_millis(8),
            source_bytes: 10,
            validation_body_bytes: 11,
            parser_body_bytes: 12,
            tokenized_count: 13,
            retokenized_count: 14,
            fallback: FallbackDecision {
                used: true,
                reason: FallbackReason::ProjectionMismatch,
            },
        };

        let stages = metrics.stages();
        assert_eq!(stages.first().unwrap().stage, "decode");
        assert_eq!(stages.last().unwrap().stage, "fallback_build");
        assert_eq!(
            stages.iter().map(|stage| stage.elapsed).sum::<Duration>(),
            Duration::from_millis(36)
        );
    }
}
