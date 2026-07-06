---
plan_id: "2026-04-26-aozora-rs-performance"
status: done
started: 2026-04-27
next_update: 2026-05-10
owner: unassigned
target_prerequisites: []
---

# Aozora-rs Performance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

**Goal:** Refactor the existing `aozora-rs` adapter into measured, composable phases, then use those measurements to make at least one evidence-backed performance improvement while preserving full-corpus validation results.

**Architecture:** Keep the external adapter protocol stable. Split `adapters/aozora-rs/src/lib.rs` into source decoding, parser invocation, typed AAT construction, projection checking, and metrics modules. The orchestrator in `lib.rs` owns phase sequencing, fallback decisions, root JSON construction, and the projection/fallback timings it directly measures.

**Tech Stack:** Rust 2024, `aozora-rs-core`, `serde_json`, `regex`, `criterion`, `ab-check`, `ab-compare`, Bash benchmark scripts, Aozora Bunko corpus in `references/aozorabunko`.

---

## File Structure

- Modify: `adapters/aozora-rs/src/lib.rs`
  - Keep public API: `VERSION`, `decode_source_bytes`, `aat_json_from_bytes`, `html_from_bytes`.
  - Orchestrate decode -> body selection -> parse -> typed AAT build -> projection -> fallback -> metrics -> root JSON serialization.
- Create: `adapters/aozora-rs/src/source.rs`
  - Own `DecodedSource`, `BodySelection`, source decoding, separator-body extraction, colophon trimming, and source-visible normalization helpers.
  - Add `DecodedSource::source_bytes` as an intentional public field used by metrics.
- Create: `adapters/aozora-rs/src/parser.rs`
  - Own `ParsedSource`, `ParseTimings`, `ParseBodyDecision`, `ParseBodyStrategy`, and `parse_with_aozora_rs`.
- Create: `adapters/aozora-rs/src/aat.rs`
  - Own `AatBlock`, `AatInline`, `ProjectedText`, `InitialAatBuildResult`, `AatBuildResult`, `AatBuildTimings`, fallback block construction, typed-to-JSON conversion, and retokenized-to-AAT mapping.
- Create: `adapters/aozora-rs/src/projection.rs`
  - Own `ProjectionSummary`, `check`, visible normalization, and subsequence testing.
- Create: `adapters/aozora-rs/src/metrics.rs`
  - Own `AdapterMetrics`, `FallbackDecision`, `FallbackReason`, `StageTiming`, JSON metadata conversion, and summary aggregation types.
- Create: `crates/ab-compare/src/metrics.rs`
  - Own reading AAT metric artifacts and aggregating summary JSON for one adapter.
- Modify: `crates/ab-compare/src/lib.rs`
  - Export metrics aggregation.
- Modify: `crates/ab-compare/src/main.rs`
  - Add optional CLI flags for AAT metric summary generation.
- Modify: `crates/ab-compare/tests/integration.rs`
  - Add aggregation tests.
- Modify: `benchmarks/run-parser-comparison.sh`
  - Generate `aozora-rs-metrics-summary.json` and include it in final `summary.json`.
- Modify: `adapters/aozora-rs/benches/adapter_bench.rs`
  - Keep current large synthetic benchmark and add smaller phase-sensitive benchmark inputs if metrics show a useful split.

## Task 0: Record Pre-Refactor Baseline

**Files:**
- Create: `benchmarks/baselines/2026-04-26-aozora-rs-pre-refactor.json`

- [x] **Step 1: Run the existing full-corpus comparison before code changes**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-pre-refactor benchmarks/run-parser-comparison.sh
```

Expected:
- `aozora2.reports == 17894`
- `aozora2.failures == 0`
- `aozora_rs.reports == 17894`
- `aozora_rs.failures == 0`
- `comparison.common_reports == 17894`
- `comparison.result_differences == []`

- [x] **Step 2: Commit the pre-refactor baseline**

Run:

```bash
cp /tmp/ab-validator-compare-pre-refactor/summary.json benchmarks/baselines/2026-04-26-aozora-rs-pre-refactor.json
git add benchmarks/baselines/2026-04-26-aozora-rs-pre-refactor.json
git commit -m "bench: record aozora-rs pre-refactor baseline (task 0)"
```

## Task 1: Establish Metrics Types and Serialization

**Files:**
- Create: `adapters/aozora-rs/src/metrics.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/Cargo.toml`

- [x] **Step 1: Write failing unit tests for fallback reason serialization and stage listing**

Add this test module to `adapters/aozora-rs/src/metrics.rs`:

```rust
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
        assert_eq!(stages.iter().map(|stage| stage.elapsed).sum::<Duration>(), Duration::from_millis(36));
    }
}
```

- [x] **Step 2: Run tests to verify they fail**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml metrics
```

Expected: FAIL because `metrics.rs` and its types do not exist.

- [x] **Step 3: Implement metrics types**

Add this dependency to `adapters/aozora-rs/Cargo.toml`:

```toml
serde = { version = "1.0", features = ["derive"] }
```

Create `adapters/aozora-rs/src/metrics.rs` with:

```rust
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
    pub fn stages(&self) -> Vec<StageTiming> {
        vec![
            StageTiming { stage: "decode", elapsed: self.decode },
            StageTiming { stage: "body_selection", elapsed: self.body_selection },
            StageTiming { stage: "tokenize", elapsed: self.tokenize },
            StageTiming { stage: "scopenize", elapsed: self.scopenize },
            StageTiming { stage: "retokenize", elapsed: self.retokenize },
            StageTiming { stage: "aat_build", elapsed: self.aat_build },
            StageTiming { stage: "projection_check", elapsed: self.projection_check },
            StageTiming { stage: "fallback_build", elapsed: self.fallback_build },
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
```

Add this line near the top of `adapters/aozora-rs/src/lib.rs`:

```rust
mod metrics;
```

- [x] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml metrics
```

Expected: PASS.

- [x] **Step 5: Commit**

```bash
git add adapters/aozora-rs/Cargo.toml adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/metrics.rs
git commit -m "refactor: add aozora-rs adapter metrics types (task 1)"
```

## Task 2: Extract Source Decoding and Body Selection

**Files:**
- Create: `adapters/aozora-rs/src/source.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [x] **Step 1: Write failing source tests**

Create `adapters/aozora-rs/src/source.rs` with only the tests first:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_utf8_and_records_source_bytes() {
        let decoded = decode_source_bytes("本文".as_bytes()).unwrap();
        assert_eq!(decoded.text, "本文");
        assert_eq!(decoded.encoding, "utf-8");
        assert_eq!(decoded.source_bytes, 6);
        assert!(decoded.source_hash.starts_with("sha256:"));
    }

    #[test]
    fn selects_validation_body_from_separators() {
        let text = "題名\n著者\n--------------------\n凡例\n--------------------\n本文\n底本：x\n";
        let decoded = DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:test".to_owned(),
            source_bytes: text.len(),
        };

        let selection = select_body(&decoded);
        assert_eq!(selection.validation_body, "本文\n");
    }
}
```

Also add this temporary module declaration to `adapters/aozora-rs/src/lib.rs` so the failing tests compile far enough to fail on missing items:

```rust
mod source;
```

- [x] **Step 2: Run tests to verify they fail**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml source
```

Expected: FAIL because `source.rs` contains tests referencing missing items.

- [x] **Step 3: Move source code from `lib.rs` into `source.rs`**

Implement `source.rs` by moving and adapting existing `DecodedSource`, `decode_source_bytes`, `trim_colophon`, `body_text`, `starts_with_separator`, `source_visible_text`, `remove_bottom_note_fragments`, and `hex_sha256`. `DecodedSource::source_bytes` is a deliberate public-field addition for metrics; update all struct literals in tests and adapter internals. Add:

```rust
use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use sha2::{Digest, Sha256};
use std::time::{Duration, Instant};

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
    pub source_bytes: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct BodySelection<'a> {
    pub validation_body: &'a str,
    pub elapsed: Duration,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
            source_bytes: bytes.len(),
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
            source_bytes: bytes.len(),
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors { "windows-31j-lossy" } else { "windows-31j" },
        source_hash,
        source_bytes: bytes.len(),
    })
}

pub fn select_body(decoded: &DecodedSource) -> BodySelection<'_> {
    let start = Instant::now();
    let validation_body = trim_colophon(body_text(&decoded.text));
    BodySelection {
        validation_body,
        elapsed: start.elapsed(),
    }
}
```

Keep the moved helper bodies unchanged unless compiler errors require visibility changes. Make `trim_colophon`, `body_text`, `starts_with_separator`, `source_visible_text`, and `remove_bottom_note_fragments` `pub(crate)`.

In `lib.rs`, replace the old definitions with:

```rust
mod source;

pub use source::{decode_source_bytes, DecodedSource};
```

- [x] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml source
```

Expected: PASS.

- [x] **Step 5: Run adapter tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/source.rs
git commit -m "refactor: extract aozora-rs source handling (task 2)"
```

## Task 3: Extract Parser Invocation and Parse Timings

**Files:**
- Create: `adapters/aozora-rs/src/parser.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/src/source.rs`

- [x] **Step 1: Write failing parser tests**

Create `adapters/aozora-rs/src/parser.rs` with:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::BodySelection;
    use std::time::Duration;

    #[test]
    fn parses_ruby_and_records_counts_and_timings() {
        let body = "吾輩《わがはい》は猫である。\n";
        let selection = BodySelection {
            validation_body: body,
            elapsed: Duration::ZERO,
        };

        let parsed = parse_with_aozora_rs(selection).unwrap();
        assert_eq!(parsed.body.validation_body, body);
        assert_eq!(parsed.parse_body.parser_body, body);
        assert_eq!(parsed.parse_body.strategy, ParseBodyStrategy::SeparatorFallback);
        assert!(parsed.tokenized_count > 0);
        assert!(parsed.retokenized_count > 0);
        assert!(parsed.timings.tokenize >= Duration::ZERO);
        assert!(parsed.timings.scopenize >= Duration::ZERO);
        assert!(parsed.timings.retokenize >= Duration::ZERO);
    }
}
```

Also add this module declaration to `adapters/aozora-rs/src/lib.rs`:

```rust
mod parser;
```

- [x] **Step 2: Run parser tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml parser
```

Expected: FAIL because parser types/functions are not implemented.

- [x] **Step 3: Implement `parser.rs`**

Move `ParsedSource` and `parse_with_aozora_rs` from `lib.rs` into `parser.rs`. Use:

```rust
use std::time::{Duration, Instant};

use anyhow::{anyhow, Result};
use aozora_rs_core::{parse_meta, retokenize, scopenize, tokenize, Retokenized};
use winnow::LocatingSlice;

use crate::source::{starts_with_separator, trim_colophon, BodySelection};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseBodyStrategy {
    ParserMeta,
    SeparatorFallback,
}

#[derive(Debug, Clone, Copy)]
pub struct ParseBodyDecision<'a> {
    pub parser_body: &'a str,
    pub strategy: ParseBodyStrategy,
}

#[derive(Debug)]
pub struct ParsedSource<'a> {
    pub body: BodySelection<'a>,
    pub parse_body: ParseBodyDecision<'a>,
    pub retokenized: Vec<Retokenized<'a>>,
    pub warnings: Vec<String>,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub timings: ParseTimings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseTimings {
    pub body_selection: Duration,
    pub tokenize: Duration,
    pub scopenize: Duration,
    pub retokenize: Duration,
}

pub fn parse_with_aozora_rs(body: BodySelection<'_>) -> Result<ParsedSource<'_>> {
    let mut warnings = Vec::new();
    let mut parsed_body = body.validation_body;
    let meta_ok = match parse_meta(&mut parsed_body) {
        Ok(_) => true,
        Err(error) => {
            warnings.push(format!("meta parse warning: {error}"));
            false
        }
    };
    let parsed_body = trim_colophon(parsed_body);
    let parse_body = if meta_ok && !starts_with_separator(parsed_body) {
        ParseBodyDecision {
            parser_body: parsed_body,
            strategy: ParseBodyStrategy::ParserMeta,
        }
    } else {
        ParseBodyDecision {
            parser_body: body.validation_body,
            strategy: ParseBodyStrategy::SeparatorFallback,
        }
    };

    let tokenize_start = Instant::now();
    let mut input = LocatingSlice::new(parse_body.parser_body);
    let tokenized = tokenize(&mut input).map_err(|()| anyhow!("aozora-rs-core tokenize failed"))?;
    let tokenize = tokenize_start.elapsed();
    let tokenized_count = tokenized.len();

    let scopenize_start = Instant::now();
    let ((scopenized, flat_tokens), scopenize_errors) = scopenize(tokenized).into_tuple();
    let scopenize = scopenize_start.elapsed();

    let retokenize_start = Instant::now();
    let (retokenized, retokenize_errors) = retokenize(flat_tokens, scopenized).into_tuple();
    let retokenize = retokenize_start.elapsed();
    let retokenized_count = retokenized.len();

    warnings.extend(scopenize_errors.into_iter().map(|error| format!("{error:?}")));
    warnings.extend(retokenize_errors.into_iter().map(|error| error.to_string()));

    Ok(ParsedSource {
        body,
        parse_body,
        retokenized,
        warnings,
        tokenized_count,
        retokenized_count,
        timings: ParseTimings {
            body_selection: body.elapsed,
            tokenize,
            scopenize,
            retokenize,
        },
    })
}
```

In `lib.rs`, add:

```rust
mod parser;
```

Remove the old parser definitions/imports from `lib.rs`.

- [x] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml parser
```

Expected: PASS.

- [x] **Step 5: Run adapter tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/parser.rs adapters/aozora-rs/src/source.rs
git commit -m "refactor: extract aozora-rs parser phase (task 3)"
```

## Task 4: Extract Typed AAT Construction

**Files:**
- Create: `adapters/aozora-rs/src/aat.rs`
- Create: `adapters/aozora-rs/tests/golden.rs`
- Create: `adapters/aozora-rs/tests/fixtures/ruby_gaiji.txt`
- Create: `adapters/aozora-rs/tests/fixtures/ruby_gaiji.aat.json`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [x] **Step 1: Create a golden-master fixture before the typed refactor**

Run:

```bash
mkdir -p adapters/aozora-rs/tests/fixtures
cat > adapters/aozora-rs/tests/fixtures/ruby_gaiji.txt <<'EOF'
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
吾輩《わがはい》は※［＃「口＋世」、U+546D］である。
底本：テスト
EOF
cargo run --manifest-path adapters/aozora-rs/Cargo.toml -- --mode aat \
  < adapters/aozora-rs/tests/fixtures/ruby_gaiji.txt \
  > adapters/aozora-rs/tests/fixtures/ruby_gaiji.aat.json
```

Expected: `ruby_gaiji.aat.json` contains one valid AAT JSON object generated by the pre-refactor adapter.

- [x] **Step 2: Add a golden-master regression test and verify it passes before the refactor**

Create `adapters/aozora-rs/tests/golden.rs`:

```rust
use aozora_rs_adapter::aat_json_from_bytes;

#[test]
fn preserves_ruby_gaiji_fixture_output() {
    let input = include_bytes!("fixtures/ruby_gaiji.txt");
    let mut expected: serde_json::Value =
        serde_json::from_slice(include_bytes!("fixtures/ruby_gaiji.aat.json")).unwrap();
    let mut actual: serde_json::Value = serde_json::from_slice(&aat_json_from_bytes(input).unwrap()).unwrap();
    expected["meta"].as_object_mut().unwrap().remove("metrics");
    actual["meta"].as_object_mut().unwrap().remove("metrics");
    assert_eq!(actual, expected);
}
```

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml preserves_ruby_gaiji_fixture_output
```

Expected: PASS before any typed-AAT changes.

- [x] **Step 3: Write failing AAT unit tests**

Create `adapters/aozora-rs/src/aat.rs` with:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_with_aozora_rs;
    use crate::source::BodySelection;
    use std::time::Duration;

    #[test]
    fn builds_typed_blocks_and_projected_text() {
        let body = "吾輩《わがはい》は猫である。\n";
        let parsed = parse_with_aozora_rs(BodySelection {
            validation_body: body,
            elapsed: Duration::ZERO,
        })
        .unwrap();

        let built = build_initial(&parsed);
        assert!(matches!(built.blocks[0], AatBlock::Paragraph { .. }));
        assert!(built.projected.visible_text.contains("吾輩"));
        assert!(built.timings.build >= Duration::ZERO);
    }

    #[test]
    fn typed_blocks_serialize_to_schema_shape() {
        let blocks = vec![AatBlock::Paragraph {
            content: vec![AatInline::Ruby {
                base: "吾輩".to_owned(),
                reading: "わがはい".to_owned(),
            }],
        }];

        let json = blocks_to_json(&blocks);
        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
    }
}
```

Also add this module declaration to `adapters/aozora-rs/src/lib.rs`:

```rust
mod aat;
```

- [x] **Step 4: Run AAT tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml aat
```

Expected: FAIL because AAT types/functions are not implemented.

- [x] **Step 5: Implement typed AAT module**

Move retokenized mapping helpers from `lib.rs` into `aat.rs`. Replace `serde_json::Value` during construction with:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AatBlock {
    Paragraph { content: Vec<AatInline> },
    Heading { level: u8, style: &'static str, content: Vec<AatInline> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AatInline {
    Text(String),
    Ruby { base: String, reading: String },
    Gaiji { description: String, resolved: String, description_format: Option<&'static str> },
    Style { style_type: &'static str, content: Vec<AatInline> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedText {
    pub visible_text: String,
}

#[derive(Debug, Clone)]
pub struct InitialAatBuildResult {
    pub blocks: Vec<AatBlock>,
    pub projected: ProjectedText,
    pub timings: AatBuildTimings,
}

#[derive(Debug, Clone)]
pub struct AatBuildResult {
    pub blocks: Vec<AatBlock>,
    pub projected: ProjectedText,
    pub fallback: crate::metrics::FallbackDecision,
    pub timings: AatBuildTimings,
    pub projection: crate::projection::ProjectionSummary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AatBuildTimings {
    pub build: std::time::Duration,
}
```

Implement:

```rust
pub fn build_initial(parsed: &crate::parser::ParsedSource<'_>) -> InitialAatBuildResult;
pub fn build_fallback(body: &str) -> (Vec<AatBlock>, ProjectedText);
pub fn blocks_to_json(blocks: &[AatBlock]) -> Vec<serde_json::Value>;
```

Port the existing logic as directly as possible:

- `flush_paragraph` takes `&mut Vec<AatBlock>` and `&mut Vec<AatInline>`.
- `push_text` pushes `AatInline::Text`.
- heading decorations produce `AatBlock::Heading`.
- figures produce `AatInline::Gaiji`.
- style decorations produce `AatInline::Style`.
- `append_source_annotation_supplements` operates on typed blocks.
- `strip_cross_node_commands` operates on typed blocks/inlines.
- `build_initial` does not perform fallback decisions.
- `build_fallback` returns a single paragraph containing `source_visible_text(body)` plus ruby/gaiji supplements.
- `ProjectedText` is computed from typed blocks, not from JSON.

Use this mapping as the implementation contract:

| Source item | Typed representation | JSON representation |
| --- | --- | --- |
| `Retokenized::Text(text)` | `AatInline::Text(source_visible_text(text))` | `{ "kind": "text", "value": ... }` |
| `Retokenized::Odoriji(_)` | `AatInline::Text(odoriji_source_text(...))` | `{ "kind": "text", "value": ... }` |
| `Retokenized::Figure(figure)` | `AatInline::Gaiji { description: figure.to_string(), resolved: "", description_format: Some(...) }` | existing gaiji object including `x-description-format` |
| `Deco::Ruby(reading)` | `AatInline::Ruby { base, reading }` unless pathological | `{ "kind": "ruby", "base": ..., "reading": ... }` |
| `Deco::AHead/BHead/CHead` | `AatBlock::Heading { level: 1/2/3, style, content }` | existing heading object |
| other `DecoBegin` | `AatInline::Style { style_type, content }` | existing style object |
| `Break(_)` | paragraph flush | paragraph boundary |

`AatInline::Style` may contain nested `AatInline` values, but the first implementation may preserve the existing flattened behavior by collecting decorated visible text into one `AatInline::Text` child.

- [x] **Step 6: Wire `lib.rs` to use typed AAT construction without changing output**

Add to `lib.rs`:

```rust
mod aat;
```

Temporarily construct the root using `aat::blocks_to_json(&built.blocks)` and the existing metadata. Do not add metrics yet.

- [x] **Step 7: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.
The golden-master test must still pass; this is the Task 4 gate.

- [x] **Step 8: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/aat.rs adapters/aozora-rs/tests/golden.rs adapters/aozora-rs/tests/fixtures
git commit -m "refactor: build typed aozora-rs AAT blocks (task 4)"
```

## Task 5: Extract Projection Checking and Explicit Fallback Decisions

**Files:**
- Create: `adapters/aozora-rs/src/projection.rs`
- Modify: `adapters/aozora-rs/src/aat.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [x] **Step 1: Write failing projection tests**

Create `adapters/aozora-rs/src/projection.rs` with:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_projected_text_in_source_order() {
        let summary = check("吾輩《わがはい》は猫である。", "吾輩は猫");
        assert!(summary.in_source_order);
        assert!(summary.source_visible_chars >= summary.projected_visible_chars);
    }

    #[test]
    fn rejects_projected_text_out_of_order() {
        let summary = check("吾輩は猫である。", "猫吾輩");
        assert!(!summary.in_source_order);
    }
}
```

Also add this module declaration to `adapters/aozora-rs/src/lib.rs`:

```rust
mod projection;
```

- [x] **Step 2: Run projection tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml projection
```

Expected: FAIL because projection functions are missing.

- [x] **Step 3: Implement projection module**

Move `normalize_visible` and `is_subsequence` into `projection.rs`. Use `crate::source::source_visible_text` for source-side normalization:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProjectionSummary {
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

pub fn check(validation_body: &str, projected_visible_text: &str) -> ProjectionSummary {
    let source = normalize_visible(&crate::source::source_visible_text(validation_body));
    let projected = normalize_visible(projected_visible_text);
    ProjectionSummary {
        source_visible_chars: source.chars().count(),
        projected_visible_chars: projected.chars().count(),
        in_source_order: projected.is_empty() || is_subsequence(&projected, &source),
    }
}
```

- [x] **Step 4: Wire fallback decision in `lib.rs`**

In `aat_json_from_bytes`, use this flow:

```rust
let decoded = decode_source_bytes(bytes)?;
let selection = source::select_body(&decoded);
let parsed = parser::parse_with_aozora_rs(selection)?;
let initial = aat::build_initial(&parsed);
let projection_start = Instant::now();
let projection = projection::check(parsed.body.validation_body, &initial.projected.visible_text);
let projection_check = projection_start.elapsed();

let mut fallback_build = Duration::ZERO;
let fallback = if parsed.body.validation_body.len() > 500_000 {
    FallbackDecision { used: true, reason: FallbackReason::LargeBody }
} else if !projection.in_source_order {
    FallbackDecision { used: true, reason: FallbackReason::ProjectionMismatch }
} else {
    FallbackDecision::none()
};

let (blocks, projected) = if fallback.used {
    let fallback_start = Instant::now();
    let built = aat::build_fallback(parsed.body.validation_body);
    fallback_build = fallback_start.elapsed();
    built
} else {
    (initial.blocks, initial.projected)
};
```

Assemble `AatBuildResult` with `timings: initial.timings` and `projection`.

- [x] **Step 5: Add direct fallback block test**

Add to `adapters/aozora-rs/src/aat.rs` tests:

```rust
#[test]
fn fallback_blocks_use_source_visible_text() {
    let (blocks, projected) = build_fallback("吾輩《わがはい》は※［＃「口＋世」、U+546D］である。");
    assert!(matches!(blocks[0], AatBlock::Paragraph { .. }));
    assert!(projected.visible_text.contains("吾輩"));
    assert!(projected.visible_text.contains("「口＋世」、U+546D"));
    assert!(!projected.visible_text.contains("わがはい"));
}
```

- [x] **Step 6: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 7: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/aat.rs adapters/aozora-rs/src/projection.rs
git commit -m "refactor: make aozora-rs projection fallback explicit (task 5)"
```

## Task 6: Add Adapter Metrics to AAT Metadata

**Files:**
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/src/metrics.rs`
- Modify: `adapters/aozora-rs/src/parser.rs`

- [x] **Step 1: Write failing metadata test**

Update `emits_schema_shaped_aat_for_ruby_and_gaiji` in `lib.rs` to assert:

```rust
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
```

- [x] **Step 2: Run test to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml emits_schema_shaped_aat_for_ruby_and_gaiji
```

Expected: FAIL because `meta.metrics` is not present.

- [x] **Step 3: Add `AdapterMetrics::from_parts`**

In `metrics.rs`, implement:

```rust
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
}
```

- [x] **Step 4: Measure decode, projection, and fallback in `lib.rs`**

Update `aat_json_from_bytes`:

```rust
let decode_start = Instant::now();
let decoded = decode_source_bytes(bytes)?;
let decode = decode_start.elapsed();
...
let metrics = AdapterMetrics::from_parts(
    &decoded,
    decode,
    &parsed.body,
    parsed.timings,
    result.timings,
    projection_check,
    fallback_build,
    parsed.parse_body.parser_body.len(),
    parsed.tokenized_count,
    parsed.retokenized_count,
    result.fallback.clone(),
);
let aat = build_root_json(&decoded, &parsed.warnings, &result, metrics.to_json());
```

Keep root construction localized in a helper:

```rust
fn build_root_json(
    decoded: &DecodedSource,
    warnings: &[String],
    result: &aat::AatBuildResult,
    metrics: serde_json::Value,
) -> serde_json::Value
```

Add this large-body metadata test to `adapters/aozora-rs/src/lib.rs`:

```rust
#[test]
fn fallback_is_reported_in_metrics_for_large_body() {
    let mut body = String::new();
    body.push_str("題名\n著者\n--------------------\n凡例\n--------------------\n");
    body.push_str(&"本文\n".repeat(130_000));
    body.push_str("底本：x\n");

    let out = aat_json_from_bytes(body.as_bytes()).unwrap();
    let value: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(value["meta"]["metrics"]["fallback_used"], true);
    assert_eq!(value["meta"]["metrics"]["fallback_reason"], "large_body");
    assert_eq!(value["meta"]["parse_complete"], true);
}
```

- [x] **Step 5: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/metrics.rs adapters/aozora-rs/src/parser.rs
git commit -m "feat: emit aozora-rs adapter metrics (task 6)"
```

## Task 7: Aggregate Metrics in `ab-compare`

**Files:**
- Create: `crates/ab-compare/src/metrics.rs`
- Modify: `crates/ab-compare/src/lib.rs`
- Modify: `crates/ab-compare/src/main.rs`
- Modify: `crates/ab-compare/tests/integration.rs`

This aggregation intentionally reads AAT artifacts, not `ab-check` reports. It is generic over any adapter that emits the required `meta.metrics` fields; missing or malformed metrics should be an error, not silently skipped.

- [x] **Step 1: Write failing aggregation test**

Add to `crates/ab-compare/tests/integration.rs`:

```rust
#[test]
fn summarizes_aat_metrics() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path();
    std::fs::create_dir_all(root.join("aozora-rs-adapter")).unwrap();
    std::fs::write(
        root.join("aozora-rs-adapter/one.aat.json"),
        r#"{
          "work_id": "one",
          "meta": {
            "adapter": "aozora-rs",
            "metrics": {
              "decode_ms": 1.0,
              "body_selection_ms": 2.0,
              "tokenize_ms": 3.0,
              "scopenize_ms": 4.0,
              "retokenize_ms": 5.0,
              "aat_build_ms": 6.0,
              "projection_check_ms": 7.0,
              "fallback_build_ms": 0.0,
              "fallback_used": false,
              "fallback_reason": "none"
            }
          }
        }"#,
    )
    .unwrap();

    let summary = ab_compare::metrics::summarize_aat_metrics(root).unwrap();
    assert_eq!(summary.adapter, "aozora-rs");
    assert_eq!(summary.works, 1);
    assert_eq!(summary.fallbacks, 0);
    assert_eq!(summary.stage_totals_ms["tokenize"], 3.0);
    assert_eq!(summary.slowest_works[0].work_id, "one");
    assert_eq!(summary.slowest_works[0].stages_ms["projection_check"], 7.0);
}
```

- [x] **Step 2: Run test to verify failure**

Run:

```bash
cargo test -p ab-compare summarizes_aat_metrics
```

Expected: FAIL because `ab_compare::metrics` does not exist.

- [x] **Step 3: Implement metrics aggregation**

Create `crates/ab-compare/src/metrics.rs`:

```rust
use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Serialize)]
pub struct MetricsSummary {
    pub adapter: String,
    pub works: usize,
    pub fallbacks: usize,
    pub stage_totals_ms: BTreeMap<String, f64>,
    pub slowest_works: Vec<SlowWork>,
}

#[derive(Debug, Serialize)]
pub struct SlowWork {
    pub work_id: String,
    pub total_ms: f64,
    pub dominant_stage: String,
    pub fallback_used: bool,
    pub stages_ms: BTreeMap<String, f64>,
}

#[derive(Debug, Deserialize)]
struct AatRoot {
    work_id: String,
    meta: AatMeta,
}

#[derive(Debug, Deserialize)]
struct AatMeta {
    adapter: String,
    metrics: AatMetrics,
}

#[derive(Debug, Deserialize)]
struct AatMetrics {
    decode_ms: f64,
    body_selection_ms: f64,
    tokenize_ms: f64,
    scopenize_ms: f64,
    retokenize_ms: f64,
    aat_build_ms: f64,
    projection_check_ms: f64,
    fallback_build_ms: f64,
    fallback_used: bool,
}

pub fn summarize_aat_metrics(root: &Path) -> Result<MetricsSummary> {
    let mut adapter = String::new();
    let mut works = 0usize;
    let mut fallbacks = 0usize;
    let mut stage_totals_ms = BTreeMap::new();
    let mut slowest_works = Vec::new();

    for entry in WalkDir::new(root) {
        let entry = entry?;
        if !entry.file_type().is_file()
            || entry.path().extension().is_none_or(|extension| extension != "json")
        {
            continue;
        }
        let bytes = fs::read(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        let root: AatRoot = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", entry.path().display()))?;
        works += 1;
        if adapter.is_empty() {
            adapter = root.meta.adapter.clone();
        }
        if root.meta.metrics.fallback_used {
            fallbacks += 1;
        }
        let stages = stages(&root.meta.metrics);
        for (name, value) in &stages {
            *stage_totals_ms.entry(name.clone()).or_insert(0.0) += value;
        }
        let total_ms = stages.values().sum();
        let dominant_stage = stages
            .iter()
            .max_by(|a, b| a.1.total_cmp(b.1))
            .map(|(name, _)| name.clone())
            .unwrap_or_default();
        slowest_works.push(SlowWork {
            work_id: root.work_id,
            total_ms,
            dominant_stage,
            fallback_used: root.meta.metrics.fallback_used,
            stages_ms: stages,
        });
    }

    slowest_works.sort_by(|a, b| b.total_ms.total_cmp(&a.total_ms));
    slowest_works.truncate(20);
    anyhow::ensure!(works > 0, "no AAT metric JSON files found under {}", root.display());
    Ok(MetricsSummary {
        adapter,
        works,
        fallbacks,
        stage_totals_ms,
        slowest_works,
    })
}

fn stages(metrics: &AatMetrics) -> BTreeMap<String, f64> {
    BTreeMap::from([
        ("decode".to_owned(), metrics.decode_ms),
        ("body_selection".to_owned(), metrics.body_selection_ms),
        ("tokenize".to_owned(), metrics.tokenize_ms),
        ("scopenize".to_owned(), metrics.scopenize_ms),
        ("retokenize".to_owned(), metrics.retokenize_ms),
        ("aat_build".to_owned(), metrics.aat_build_ms),
        ("projection_check".to_owned(), metrics.projection_check_ms),
        ("fallback_build".to_owned(), metrics.fallback_build_ms),
    ])
}
```

- [x] **Step 4: Export module and add CLI flags**

In `crates/ab-compare/src/lib.rs`:

```rust
pub mod metrics;
```

In `crates/ab-compare/src/main.rs`, add optional args:

```rust
#[arg(long)]
metrics_root: Option<PathBuf>,

#[arg(long)]
metrics_output: Option<PathBuf>,
```

After existing report comparison output, add:

```rust
if let Some(metrics_root) = &args.metrics_root {
    let summary = ab_compare::metrics::summarize_aat_metrics(metrics_root)?;
    if let Some(path) = &args.metrics_output {
        let file = std::fs::File::create(path)?;
        serde_json::to_writer_pretty(file, &summary)?;
    } else {
        serde_json::to_writer_pretty(std::io::stdout(), &summary)?;
    }
}
```

- [x] **Step 5: Run tests**

Run:

```bash
cargo test -p ab-compare
```

Expected: PASS.

- [x] **Step 6: Commit**

```bash
git add crates/ab-compare/src/lib.rs crates/ab-compare/src/main.rs crates/ab-compare/src/metrics.rs crates/ab-compare/tests/integration.rs
git commit -m "feat: summarize adapter AAT metrics (task 7)"
```

## Task 8: Add AAT Structural Comparison

**Files:**
- Create: `crates/ab-compare/src/aat_diff.rs`
- Modify: `crates/ab-compare/src/lib.rs`
- Modify: `crates/ab-compare/src/main.rs`
- Modify: `crates/ab-compare/tests/integration.rs`

This task closes the gap where `ab-compare` reports "0 differences" while only comparing validation report pass/fail results. It compares persisted AAT artifacts and reports structural/projection differences separately from validation differences.

- [x] **Step 1: Write a failing AAT structural comparison test**

Add to `crates/ab-compare/tests/integration.rs`:

```rust
#[test]
fn detects_aat_structural_differences_when_reports_match() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    std::fs::write(
        a.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "吾輩は猫"}]}
          ],
          "meta": {"adapter": "a"}
        }"#,
    )
    .unwrap();
    std::fs::write(
        b.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "heading", "level": 1, "content": [{"kind": "text", "value": "吾輩は猫"}]}
          ],
          "meta": {"adapter": "b"}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();
    assert_eq!(summary.common_aat, 1);
    assert_eq!(summary.structural_differences.len(), 1);
    assert_eq!(summary.structural_differences[0].work_id, "one");
    assert_eq!(summary.structural_differences[0].a_block_kinds["paragraph"], 1);
    assert_eq!(summary.structural_differences[0].b_block_kinds["heading"], 1);
    assert!(!summary.structural_differences[0].visible_text_differs);
}
```

- [x] **Step 2: Run test to verify failure**

Run:

```bash
cargo test -p ab-compare detects_aat_structural_differences_when_reports_match
```

Expected: FAIL because `ab_compare::aat_diff` does not exist.

- [x] **Step 3: Implement AAT artifact summaries**

Create `crates/ab-compare/src/aat_diff.rs`:

```rust
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

#[derive(Debug, Serialize)]
pub struct AatCompareSummary {
    pub common_aat: usize,
    pub only_a: usize,
    pub only_b: usize,
    pub structural_differences: Vec<AatStructuralDifference>,
}

#[derive(Debug, Serialize)]
pub struct AatStructuralDifference {
    pub work_id: String,
    pub visible_text_differs: bool,
    pub a_structure_hash: String,
    pub b_structure_hash: String,
    pub a_visible_hash: String,
    pub b_visible_hash: String,
    pub a_block_kinds: BTreeMap<String, usize>,
    pub b_block_kinds: BTreeMap<String, usize>,
    pub a_inline_kinds: BTreeMap<String, usize>,
    pub b_inline_kinds: BTreeMap<String, usize>,
}

#[derive(Debug, Deserialize)]
struct AatRoot {
    work_id: String,
    blocks: Value,
}

#[derive(Debug)]
struct AatSummary {
    work_id: String,
    structure_hash: String,
    visible_hash: String,
    block_kinds: BTreeMap<String, usize>,
    inline_kinds: BTreeMap<String, usize>,
}

pub fn compare_aat_dirs(a: &Path, b: &Path) -> Result<AatCompareSummary> {
    let a = read_aat_summaries(a)?;
    let b = read_aat_summaries(b)?;
    let keys_a = a.keys().cloned().collect::<BTreeSet<_>>();
    let keys_b = b.keys().cloned().collect::<BTreeSet<_>>();
    let common = keys_a.intersection(&keys_b).cloned().collect::<Vec<_>>();
    let mut structural_differences = Vec::new();

    for key in &common {
        let left = &a[key];
        let right = &b[key];
        if left.structure_hash != right.structure_hash || left.visible_hash != right.visible_hash {
            structural_differences.push(AatStructuralDifference {
                work_id: left.work_id.clone(),
                visible_text_differs: left.visible_hash != right.visible_hash,
                a_structure_hash: left.structure_hash.clone(),
                b_structure_hash: right.structure_hash.clone(),
                a_visible_hash: left.visible_hash.clone(),
                b_visible_hash: right.visible_hash.clone(),
                a_block_kinds: left.block_kinds.clone(),
                b_block_kinds: right.block_kinds.clone(),
                a_inline_kinds: left.inline_kinds.clone(),
                b_inline_kinds: right.inline_kinds.clone(),
            });
        }
    }

    Ok(AatCompareSummary {
        common_aat: common.len(),
        only_a: keys_a.difference(&keys_b).count(),
        only_b: keys_b.difference(&keys_a).count(),
        structural_differences,
    })
}

fn read_aat_summaries(root: &Path) -> Result<BTreeMap<String, AatSummary>> {
    let mut out = BTreeMap::new();
    for entry in WalkDir::new(root) {
        let entry = entry?;
        if !entry.file_type().is_file()
            || entry.path().extension().is_none_or(|extension| extension != "json")
        {
            continue;
        }
        let bytes = fs::read(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        let root: AatRoot = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", entry.path().display()))?;
        out.insert(root.work_id.clone(), summarize(root)?);
    }
    Ok(out)
}

fn summarize(root: AatRoot) -> Result<AatSummary> {
    let structure_hash = hash_json(&root.blocks)?;
    let mut block_kinds = BTreeMap::new();
    let mut inline_kinds = BTreeMap::new();
    let mut visible = String::new();
    collect_blocks(&root.blocks, &mut block_kinds, &mut inline_kinds, &mut visible);
    Ok(AatSummary {
        work_id: root.work_id,
        structure_hash,
        visible_hash: hash_bytes(visible.as_bytes()),
        block_kinds,
        inline_kinds,
    })
}

fn hash_json(value: &Value) -> Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}
```

Then implement `collect_blocks(...)` by walking `blocks` recursively:

- Count block object `kind` values at the top-level `blocks` array.
- Count inline object `kind` values inside `content`, `children`, `upper`, and `lower`.
- Visible text rules:
  - `text`: append `value`
  - `ruby`: append `base`
  - `gaiji`: append `resolved` when non-empty, otherwise `description`
  - containers: recurse into `content`, `children`, `upper`, and `lower`

- [x] **Step 4: Add dependency and exports**

The workspace already defines `sha2 = "0.10"` in `Cargo.toml`.

Add to `crates/ab-compare/Cargo.toml`:

```toml
sha2.workspace = true
```

Add to `crates/ab-compare/src/lib.rs`:

```rust
pub mod aat_diff;
```

- [x] **Step 5: Add CLI flags**

In `crates/ab-compare/src/main.rs`, add optional args:

```rust
#[arg(long)]
aats_a: Option<PathBuf>,

#[arg(long)]
aats_b: Option<PathBuf>,

#[arg(long)]
aat_diff_output: Option<PathBuf>,
```

After the report comparison output, add:

```rust
if let (Some(aats_a), Some(aats_b), Some(output)) =
    (&args.aats_a, &args.aats_b, &args.aat_diff_output)
{
    let summary = ab_compare::aat_diff::compare_aat_dirs(aats_a, aats_b)?;
    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = std::fs::File::create(output)?;
    serde_json::to_writer_pretty(file, &summary)?;
}
```

- [x] **Step 6: Run tests**

Run:

```bash
cargo test -p ab-compare
```

Expected: PASS.

- [x] **Step 7: Commit**

```bash
git add crates/ab-compare/Cargo.toml crates/ab-compare/src/lib.rs crates/ab-compare/src/main.rs crates/ab-compare/src/aat_diff.rs crates/ab-compare/tests/integration.rs
git commit -m "feat: compare persisted AAT structure (task 8)"
```

## Task 9: Update Benchmark Runner and Measure Baseline

**Files:**
- Modify: `benchmarks/run-parser-comparison.sh`
- Modify: `benchmarks/baselines/2026-04-26-parser-comparison.json` or add a new dated baseline file after a stable full run

- [x] **Step 1: Update runner to write metrics summary**

After the existing `target/release/ab-compare --reports-a ...` command in `benchmarks/run-parser-comparison.sh`, add:

```bash
target/release/ab-compare \
  --reports-a "$out_dir/reports/aozora2/aozora2-adapter" \
  --reports-b "$out_dir/reports/aozora-rs/aozora-rs-adapter" \
  --aats-a "$out_dir/aats/aozora2/aozora2-adapter" \
  --aats-b "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --aat-diff-output "$out_dir/aat-structure-comparison.json" \
  --metrics-root "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --metrics-output "$out_dir/aozora-rs-metrics-summary.json" \
  --output "$out_dir/comparison.json"
```

Remove the old `ab-compare` invocation so the comparison is not run twice.

In the final `jq -n` command, add:

```bash
--slurpfile metrics "$out_dir/aozora-rs-metrics-summary.json"
--slurpfile aatdiff "$out_dir/aat-structure-comparison.json"
```

and include:

```jq
aozora_rs_metrics: $metrics[0],
aat_structure_comparison: $aatdiff[0]
```

- [x] **Step 2: Run script syntax check**

Run:

```bash
bash -n benchmarks/run-parser-comparison.sh
```

Expected: no output and exit code 0.

- [x] **Step 3: Run a small corpus smoke comparison**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-smoke AB_BENCH_JOBS=2 AB_BENCH_TIMEOUT=120s benchmarks/run-parser-comparison.sh
```

Expected: script completes; `/tmp/ab-validator-compare-smoke/summary.json` contains `aozora_rs_metrics`.

- [x] **Step 4: Commit**

```bash
git add benchmarks/run-parser-comparison.sh
git commit -m "bench: include aozora-rs metrics summary (task 9)"
```

## Task 10: Profile and Implement One Evidence-Backed Optimization

**Files:**
- Modify: `adapters/aozora-rs/src/projection.rs` if projection dominates measured adapter-owned time.
- Modify: `adapters/aozora-rs/src/source.rs` and `adapters/aozora-rs/src/aat.rs` if repeated regex construction dominates measured adapter-owned time.
- Modify: `benchmarks/baselines/2026-04-26-aozora-rs-performance.json` after a stable full-corpus run.

- [x] **Step 1: Run adapter Criterion benchmark before optimization**

Run:

```bash
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench
```

Expected: benchmark completes and reports `aozora_rs_adapter_aat_json_large`.

- [x] **Step 2: Run full-corpus comparison with metrics**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-metrics benchmarks/run-parser-comparison.sh
```

Expected:
- `aozora_rs.reports == 17894`
- `aozora_rs.failures == 0`
- `comparison.common_reports == 17894`
- `comparison.result_differences == []`
- `aozora_rs_metrics.stage_totals_ms` identifies the dominant stage.

- [x] **Step 3: Verify schema stability after the refactor**

Compare AAT artifacts from the pre-refactor and metrics runs for the golden fixture and, when both full AAT directories are available, the full corpus:

```bash
diff -ur \
  /tmp/ab-validator-compare-pre-refactor/aats/aozora-rs/aozora-rs-adapter \
  /tmp/ab-validator-compare-metrics/aats/aozora-rs/aozora-rs-adapter
```

Expected: differences are limited to the newly added `meta.metrics` object. Validation reports must remain unchanged by `ab-compare`.

- [x] **Step 4: Choose one optimization from the measured dominant adapter-owned stage**

Use this decision table:

- Define total measured time as the sum of `aozora_rs_metrics.stage_totals_ms`.
- Define core parser time as `tokenize + scopenize + retokenize`.
- Define adapter-owned time as total measured time minus core parser time.
- If core parser time is at least 90% of total measured time, do not replace parser internals in this phase; the deliverable is the refactor, metrics, and evidence that the remaining bottleneck is inside `aozora-rs-core`.
- Otherwise, a stage is dominant if it is at least 30% of adapter-owned time.
- If `projection_check` is dominant, optimize `projection::is_subsequence` by scanning normalized source once and early-returning when the projected text is empty or longer than source.
- If `aat_build` is dominant, first confirm repeated regex construction with a benchmark or profiler sample, then replace local `Regex::new(...).unwrap()` calls with `std::sync::OnceLock<Regex>` statics in `source.rs` and `aat.rs`.
- If `fallback_build` is dominant or non-zero for more than 1% of works, reuse the source-visible string computed during `projection::check` by returning it in a separate value only if this does not force long-lived borrow chains.

- [x] **Step 5: Write a focused failing benchmark or test for the chosen optimization**

For repeated regex construction, add to `adapters/aozora-rs/src/source.rs`:

```rust
#[test]
fn source_visible_text_handles_ruby_gaiji_and_commands_repeatedly() {
    for _ in 0..100 {
        let visible = source_visible_text("吾輩《わがはい》は※［＃「口＋世」、U+546D］［＃ここは注記］");
        assert!(visible.contains("吾輩"));
        assert!(visible.contains("「口＋世」、U+546D"));
        assert!(!visible.contains("わがはい"));
        assert!(!visible.contains("ここは注記"));
    }
}
```

For projection optimization, add to `adapters/aozora-rs/src/projection.rs`:

```rust
#[test]
fn projection_rejects_when_projected_text_is_longer_than_source() {
    let summary = check("短い本文", "短い本文より長い投影テキスト");
    assert!(!summary.in_source_order);
}
```

- [x] **Step 6: Implement only the measured optimization**

For regex construction, use `OnceLock`:

```rust
use std::sync::OnceLock;

fn gaiji_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap())
}
```

Then replace repeated `Regex::new(...).unwrap()` with helper calls.

For projection early exit, update:

```rust
if projected.is_empty() {
    return ProjectionSummary {
        source_visible_chars,
        projected_visible_chars,
        in_source_order: true,
    };
}
if projected_visible_chars > source_visible_chars {
    return ProjectionSummary {
        source_visible_chars,
        projected_visible_chars,
        in_source_order: false,
    };
}
```

- [x] **Step 7: Run focused tests and benchmark**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench
```

Expected: tests pass. The benchmark median for `aozora_rs_adapter_aat_json_large` is not more than 5% slower than the Task 10 Step 1 pre-optimization run.

- [x] **Step 8: Run full-corpus validation again**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-optimized benchmarks/run-parser-comparison.sh
```

Expected:
- `aozora_rs.reports == 17894`
- `aozora_rs.failures == 0`
- `comparison.result_differences == []`
- Runtime is at least 25% lower than 1690.014773 seconds, or metrics show at least 90% of remaining measured time inside `tokenize + scopenize + retokenize`.

- [x] **Step 9: Commit optimization and baseline**

If the run is stable, copy `/tmp/ab-validator-compare-optimized/summary.json` to a new baseline path such as:

```bash
cp /tmp/ab-validator-compare-optimized/summary.json benchmarks/baselines/2026-04-26-aozora-rs-performance.json
```

Then commit:

```bash
git add adapters/aozora-rs/src benchmarks/baselines/2026-04-26-aozora-rs-performance.json
git commit -m "perf: optimize measured aozora-rs adapter stage (task 10)"
```

## Task 11: Final Verification and Hickey/Rust Review

**Files:**
- Modify only if review finds issues.

- [x] **Step 1: Run formatting**

```bash
cargo fmt --check
cargo fmt --check --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 2: Run tests**

```bash
cargo test --workspace
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [x] **Step 3: Run clippy**

```bash
cargo clippy --workspace --all-targets -- -D warnings
cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings
```

Expected: PASS.

- [x] **Step 4: Run benchmark compile check**

```bash
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench -- --test
```

Expected: PASS.

- [x] **Step 5: Run Nix verification**

```bash
nix flake check
```

Expected: PASS.

- [x] **Step 6: Perform Rich Hickey review**

Check and record answers in the final response:

- `lib.rs` is orchestration and public API glue only.
- `source.rs`, `parser.rs`, `aat.rs`, `projection.rs`, and `metrics.rs` each own one concern.
- Projection checks are text-only and do not parse JSON.
- Metrics are values and summary aggregation is testable without the full corpus.
- Fallback is represented as data in AAT metadata and aggregate summaries.
- Any optimization is justified by metrics rather than speculation.

- [x] **Step 7: Commit any verification fixes**

If fixes were needed:

```bash
git status --short
git add adapters/aozora-rs/src crates/ab-compare/src crates/ab-compare/tests benchmarks/run-parser-comparison.sh benchmarks/baselines
git commit -m "fix: address aozora-rs performance verification"
```

If no fixes were needed, do not create an empty commit.
