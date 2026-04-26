# Aozora-rs Performance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Refactor the existing `aozora-rs` adapter into measured, composable phases, then use those measurements to make at least one evidence-backed performance improvement while preserving full-corpus validation results.

**Architecture:** Keep the external adapter protocol stable. Split `adapters/aozora-rs/src/lib.rs` into source decoding, parser invocation, typed AAT construction, projection checking, and metrics modules. The orchestrator in `lib.rs` owns phase sequencing, fallback decisions, root JSON construction, and orchestration-owned timings.

**Tech Stack:** Rust 2024, `aozora-rs-core`, `serde_json`, `regex`, `criterion`, `ab-check`, `ab-compare`, Bash benchmark scripts, Aozora Bunko corpus in `references/aozorabunko`.

---

## File Structure

- Modify: `adapters/aozora-rs/src/lib.rs`
  - Keep public API: `VERSION`, `decode_source_bytes`, `aat_json_from_bytes`, `html_from_bytes`.
  - Orchestrate decode -> body selection -> parse -> typed AAT build -> projection -> fallback -> metrics -> root JSON serialization.
- Create: `adapters/aozora-rs/src/source.rs`
  - Own `DecodedSource`, `BodySelection`, `BodySelectionStrategy`, source decoding, separator-body extraction, colophon trimming, and source-visible normalization helpers.
- Create: `adapters/aozora-rs/src/parser.rs`
  - Own `ParsedSource`, `ParseTimings`, and `parse_with_aozora_rs`.
- Create: `adapters/aozora-rs/src/aat.rs`
  - Own `AatBlock`, `AatInline`, `ProjectedText`, `InitialAatBuildResult`, `AatBuildResult`, `AatBuildTimings`, fallback block construction, typed-to-JSON conversion, and retokenized-to-AAT mapping.
- Create: `adapters/aozora-rs/src/projection.rs`
  - Own `ProjectionSummary`, `check`, visible normalization, and subsequence testing.
- Create: `adapters/aozora-rs/src/metrics.rs`
  - Own `AdapterMetrics`, `OrchestrationTimings`, `FallbackDecision`, `FallbackReason`, `StageTiming`, JSON metadata conversion, and summary aggregation types.
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

## Task 1: Establish Metrics Types and Serialization

**Files:**
- Create: `adapters/aozora-rs/src/metrics.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/Cargo.toml`

- [ ] **Step 1: Write failing unit tests for fallback reason serialization and stage listing**

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
        assert_eq!(
            serde_json::to_value(FallbackReason::Other("synthetic_reason".to_owned())).unwrap(),
            "synthetic_reason"
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
            root_construct: Duration::from_millis(9),
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
        assert_eq!(stages.last().unwrap().stage, "aat_root");
        assert_eq!(stages.iter().map(|stage| stage.elapsed).sum::<Duration>(), Duration::from_millis(45));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml metrics
```

Expected: FAIL because `metrics.rs` and its types do not exist.

- [ ] **Step 3: Implement metrics types**

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
    Other(String),
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
            Self::Other(reason) => serializer.serialize_str(reason),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OrchestrationTimings {
    pub projection_check: Duration,
    pub fallback_build: Duration,
    pub root_construct: Duration,
}

impl OrchestrationTimings {
    pub fn empty() -> Self {
        Self {
            projection_check: Duration::ZERO,
            fallback_build: Duration::ZERO,
            root_construct: Duration::ZERO,
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
    pub root_construct: Duration,
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
            StageTiming { stage: "aat_root", elapsed: self.root_construct },
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
            "aat_root_ms": ms(self.root_construct),
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

- [ ] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml metrics
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add adapters/aozora-rs/Cargo.toml adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/metrics.rs
git commit -m "refactor: add aozora-rs adapter metrics types"
```

## Task 2: Extract Source Decoding and Body Selection

**Files:**
- Create: `adapters/aozora-rs/src/source.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [ ] **Step 1: Write failing source tests**

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
    fn selects_parser_and_validation_bodies_from_separators() {
        let text = "題名\n著者\n--------------------\n凡例\n--------------------\n本文\n底本：x\n";
        let decoded = DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:test".to_owned(),
            source_bytes: text.len(),
        };

        let selection = select_body(&decoded);
        assert_eq!(selection.validation_body, "本文\n");
        assert_eq!(selection.parser_body, "本文\n");
        assert_eq!(selection.strategy, BodySelectionStrategy::SeparatorFallback);
    }
}
```

Also add this temporary module declaration to `adapters/aozora-rs/src/lib.rs` so the failing tests compile far enough to fail on missing items:

```rust
mod source;
```

- [ ] **Step 2: Run tests to verify they fail**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml source
```

Expected: FAIL because `source.rs` contains tests referencing missing items.

- [ ] **Step 3: Move source code from `lib.rs` into `source.rs`**

Implement `source.rs` by moving and adapting existing `DecodedSource`, `decode_source_bytes`, `trim_colophon`, `body_text`, `starts_with_separator`, `source_visible_text`, `remove_bottom_note_fragments`, and `hex_sha256`. Add:

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BodySelectionStrategy {
    ParserMeta,
    SeparatorFallback,
}

#[derive(Debug, Clone, Copy)]
pub struct BodySelection<'a> {
    pub parser_body: &'a str,
    pub validation_body: &'a str,
    pub strategy: BodySelectionStrategy,
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
        parser_body: validation_body,
        validation_body,
        strategy: BodySelectionStrategy::SeparatorFallback,
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

- [ ] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml source
```

Expected: PASS.

- [ ] **Step 5: Run adapter tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/source.rs
git commit -m "refactor: extract aozora-rs source handling"
```

## Task 3: Extract Parser Invocation and Parse Timings

**Files:**
- Create: `adapters/aozora-rs/src/parser.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/src/source.rs`

- [ ] **Step 1: Write failing parser tests**

Create `adapters/aozora-rs/src/parser.rs` with:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{BodySelection, BodySelectionStrategy};
    use std::time::Duration;

    #[test]
    fn parses_ruby_and_records_counts_and_timings() {
        let body = "吾輩《わがはい》は猫である。\n";
        let selection = BodySelection {
            parser_body: body,
            validation_body: body,
            strategy: BodySelectionStrategy::SeparatorFallback,
            elapsed: Duration::ZERO,
        };

        let parsed = parse_with_aozora_rs(selection).unwrap();
        assert_eq!(parsed.body.validation_body, body);
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

- [ ] **Step 2: Run parser tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml parser
```

Expected: FAIL because parser types/functions are not implemented.

- [ ] **Step 3: Implement `parser.rs`**

Move `ParsedSource` and `parse_with_aozora_rs` from `lib.rs` into `parser.rs`. Use:

```rust
use std::time::{Duration, Instant};

use anyhow::{anyhow, Result};
use aozora_rs_core::{parse_meta, retokenize, scopenize, tokenize, Retokenized};
use winnow::LocatingSlice;

use crate::source::{starts_with_separator, trim_colophon, BodySelection, BodySelectionStrategy};

#[derive(Debug)]
pub struct ParsedSource<'a> {
    pub body: BodySelection<'a>,
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

pub fn parse_with_aozora_rs(mut body: BodySelection<'_>) -> Result<ParsedSource<'_>> {
    let mut warnings = Vec::new();
    let mut parsed_body = body.parser_body;
    let meta_ok = match parse_meta(&mut parsed_body) {
        Ok(_) => true,
        Err(error) => {
            warnings.push(format!("meta parse warning: {error}"));
            false
        }
    };
    let parsed_body = trim_colophon(parsed_body);
    if meta_ok && !starts_with_separator(parsed_body) {
        body.parser_body = parsed_body;
        body.strategy = BodySelectionStrategy::ParserMeta;
    }

    let tokenize_start = Instant::now();
    let mut input = LocatingSlice::new(body.parser_body);
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

- [ ] **Step 4: Run focused tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml parser
```

Expected: PASS.

- [ ] **Step 5: Run adapter tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/parser.rs adapters/aozora-rs/src/source.rs
git commit -m "refactor: extract aozora-rs parser phase"
```

## Task 4: Extract Typed AAT Construction

**Files:**
- Create: `adapters/aozora-rs/src/aat.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [ ] **Step 1: Write failing AAT tests**

Create `adapters/aozora-rs/src/aat.rs` with:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_with_aozora_rs;
    use crate::source::{BodySelection, BodySelectionStrategy};
    use std::time::Duration;

    #[test]
    fn builds_typed_blocks_and_projected_text() {
        let body = "吾輩《わがはい》は猫である。\n";
        let parsed = parse_with_aozora_rs(BodySelection {
            parser_body: body,
            validation_body: body,
            strategy: BodySelectionStrategy::SeparatorFallback,
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

- [ ] **Step 2: Run AAT tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml aat
```

Expected: FAIL because AAT types/functions are not implemented.

- [ ] **Step 3: Implement typed AAT module**

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

- [ ] **Step 4: Wire `lib.rs` to use typed AAT construction without changing output**

Add to `lib.rs`:

```rust
mod aat;
```

Temporarily construct the root using `aat::blocks_to_json(&built.blocks)` and the existing metadata. Do not add metrics yet.

- [ ] **Step 5: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/aat.rs
git commit -m "refactor: build typed aozora-rs AAT blocks"
```

## Task 5: Extract Projection Checking and Explicit Fallback Decisions

**Files:**
- Create: `adapters/aozora-rs/src/projection.rs`
- Modify: `adapters/aozora-rs/src/aat.rs`
- Modify: `adapters/aozora-rs/src/lib.rs`

- [ ] **Step 1: Write failing projection tests**

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

- [ ] **Step 2: Run projection tests to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml projection
```

Expected: FAIL because projection functions are missing.

- [ ] **Step 3: Implement projection module**

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

- [ ] **Step 4: Wire fallback decision in `lib.rs`**

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

- [ ] **Step 5: Add fallback metadata test**

Add to `adapters/aozora-rs/src/lib.rs` tests:

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

- [ ] **Step 6: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/aat.rs adapters/aozora-rs/src/projection.rs
git commit -m "refactor: make aozora-rs projection fallback explicit"
```

## Task 6: Add Adapter Metrics to AAT Metadata

**Files:**
- Modify: `adapters/aozora-rs/src/lib.rs`
- Modify: `adapters/aozora-rs/src/metrics.rs`
- Modify: `adapters/aozora-rs/src/parser.rs`

- [ ] **Step 1: Write failing metadata test**

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
assert!(metrics["aat_root_ms"].as_f64().unwrap() >= 0.0);
assert_eq!(metrics["fallback_used"], false);
assert_eq!(metrics["fallback_reason"], "none");
assert_eq!(metrics["source_bytes"], input.len());
assert!(metrics["tokenized_count"].as_u64().unwrap() > 0);
assert!(metrics["retokenized_count"].as_u64().unwrap() > 0);
```

- [ ] **Step 2: Run test to verify failure**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml emits_schema_shaped_aat_for_ruby_and_gaiji
```

Expected: FAIL because `meta.metrics` is not present.

- [ ] **Step 3: Add `AdapterMetrics::from_parts`**

In `metrics.rs`, implement:

```rust
impl AdapterMetrics {
    pub fn from_parts(
        decoded: &crate::source::DecodedSource,
        decode: Duration,
        body: &crate::source::BodySelection<'_>,
        parse: crate::parser::ParseTimings,
        aat: crate::aat::AatBuildTimings,
        orchestration: OrchestrationTimings,
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
            projection_check: orchestration.projection_check,
            fallback_build: orchestration.fallback_build,
            root_construct: orchestration.root_construct,
            source_bytes: decoded.source_bytes,
            validation_body_bytes: body.validation_body.len(),
            parser_body_bytes: body.parser_body.len(),
            tokenized_count,
            retokenized_count,
            fallback,
        }
    }
}
```

- [ ] **Step 4: Measure decode and root construction in `lib.rs`**

Update `aat_json_from_bytes`:

```rust
let decode_start = Instant::now();
let decoded = decode_source_bytes(bytes)?;
let decode = decode_start.elapsed();
...
let root_start = Instant::now();
let metrics = AdapterMetrics::from_parts(
    &decoded,
    decode,
    &parsed.body,
    parsed.timings,
    result.timings,
    OrchestrationTimings {
        projection_check,
        fallback_build,
        root_construct: Duration::ZERO,
    },
    parsed.tokenized_count,
    parsed.retokenized_count,
    result.fallback.clone(),
);
let mut aat = build_root_json(&decoded, &parsed.warnings, &result, metrics.to_json());
let root_construct = root_start.elapsed();
aat["meta"]["metrics"]["aat_root_ms"] = serde_json::Value::from(root_construct.as_secs_f64() * 1000.0);
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

- [ ] **Step 5: Run tests**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add adapters/aozora-rs/src/lib.rs adapters/aozora-rs/src/metrics.rs adapters/aozora-rs/src/parser.rs
git commit -m "feat: emit aozora-rs adapter metrics"
```

## Task 7: Aggregate Metrics in `ab-compare`

**Files:**
- Create: `crates/ab-compare/src/metrics.rs`
- Modify: `crates/ab-compare/src/lib.rs`
- Modify: `crates/ab-compare/src/main.rs`
- Modify: `crates/ab-compare/tests/integration.rs`

- [ ] **Step 1: Write failing aggregation test**

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
              "aat_root_ms": 8.0,
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
    assert_eq!(summary.slowest_works[0].stages_ms["aat_root"], 8.0);
}
```

- [ ] **Step 2: Run test to verify failure**

Run:

```bash
cargo test -p ab-compare summarizes_aat_metrics
```

Expected: FAIL because `ab_compare::metrics` does not exist.

- [ ] **Step 3: Implement metrics aggregation**

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
    aat_root_ms: f64,
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
        ("aat_root".to_owned(), metrics.aat_root_ms),
    ])
}
```

- [ ] **Step 4: Export module and add CLI flags**

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

- [ ] **Step 5: Run tests**

Run:

```bash
cargo test -p ab-compare
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-compare/src/lib.rs crates/ab-compare/src/main.rs crates/ab-compare/src/metrics.rs crates/ab-compare/tests/integration.rs
git commit -m "feat: summarize adapter AAT metrics"
```

## Task 8: Update Benchmark Runner and Measure Baseline

**Files:**
- Modify: `benchmarks/run-parser-comparison.sh`
- Modify: `benchmarks/baselines/2026-04-26-parser-comparison.json` or add a new dated baseline file after a stable full run

- [ ] **Step 1: Update runner to write metrics summary**

After the existing `target/release/ab-compare --reports-a ...` command in `benchmarks/run-parser-comparison.sh`, add:

```bash
target/release/ab-compare \
  --reports-a "$out_dir/reports/aozora2/aozora2-adapter" \
  --reports-b "$out_dir/reports/aozora-rs/aozora-rs-adapter" \
  --metrics-root "$out_dir/aats/aozora-rs/aozora-rs-adapter" \
  --metrics-output "$out_dir/aozora-rs-metrics-summary.json" \
  --output "$out_dir/comparison.json"
```

Remove the old `ab-compare` invocation so the comparison is not run twice.

In the final `jq -n` command, add:

```bash
--slurpfile metrics "$out_dir/aozora-rs-metrics-summary.json"
```

and include:

```jq
aozora_rs_metrics: $metrics[0]
```

- [ ] **Step 2: Run script syntax check**

Run:

```bash
bash -n benchmarks/run-parser-comparison.sh
```

Expected: no output and exit code 0.

- [ ] **Step 3: Run a small corpus smoke comparison**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-smoke AB_BENCH_JOBS=2 AB_BENCH_TIMEOUT=120s benchmarks/run-parser-comparison.sh
```

Expected: script completes; `/tmp/ab-validator-compare-smoke/summary.json` contains `aozora_rs_metrics`.

- [ ] **Step 4: Commit**

```bash
git add benchmarks/run-parser-comparison.sh
git commit -m "bench: include aozora-rs metrics summary"
```

## Task 9: Profile and Implement One Evidence-Backed Optimization

**Files:**
- Modify: `adapters/aozora-rs/src/projection.rs` if projection dominates measured adapter-owned time.
- Modify: `adapters/aozora-rs/src/source.rs` and `adapters/aozora-rs/src/aat.rs` if repeated regex construction dominates measured adapter-owned time.
- Modify: `benchmarks/baselines/2026-04-26-aozora-rs-performance.json` after a stable full-corpus run.

- [ ] **Step 1: Run adapter Criterion benchmark before optimization**

Run:

```bash
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench
```

Expected: benchmark completes and reports `aozora_rs_adapter_aat_json_large`.

- [ ] **Step 2: Run full-corpus comparison with metrics**

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

- [ ] **Step 3: Choose one optimization from the measured dominant adapter-owned stage**

Use this decision table:

- If `projection_check` is dominant outside `aozora-rs-core`, optimize `projection::is_subsequence` by scanning normalized source once and early-returning when the projected text is empty or longer than source.
- If `aat_build` is dominant, remove repeated regex construction by replacing local `Regex::new(...).unwrap()` calls with `std::sync::OnceLock<Regex>` statics in `source.rs` and `aat.rs`.
- If `fallback_build` is non-zero for many works, reuse the source-visible string computed during `projection::check` by returning it in `ProjectionSummary` or a separate value only if this does not force long-lived borrow chains.
- If at least 90% of runtime is `tokenize + scopenize + retokenize`, do not replace parser internals in this phase; record that result and focus the code-quality refactor as the deliverable.

- [ ] **Step 4: Write a focused failing benchmark or test for the chosen optimization**

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

- [ ] **Step 5: Implement only the measured optimization**

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

- [ ] **Step 6: Run focused tests and benchmark**

Run:

```bash
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench
```

Expected: tests pass and benchmark does not regress materially.

- [ ] **Step 7: Run full-corpus validation again**

Run:

```bash
AB_BENCH_OUT=/tmp/ab-validator-compare-optimized benchmarks/run-parser-comparison.sh
```

Expected:
- `aozora_rs.reports == 17894`
- `aozora_rs.failures == 0`
- `comparison.result_differences == []`
- Runtime is at least 25% lower than 1690.014773 seconds, or metrics show at least 90% of remaining time inside `tokenize + scopenize + retokenize`.

- [ ] **Step 8: Commit optimization and baseline**

If the run is stable, copy `/tmp/ab-validator-compare-optimized/summary.json` to a new baseline path such as:

```bash
cp /tmp/ab-validator-compare-optimized/summary.json benchmarks/baselines/2026-04-26-aozora-rs-performance.json
```

Then commit:

```bash
git add adapters/aozora-rs/src benchmarks/baselines/2026-04-26-aozora-rs-performance.json
git commit -m "perf: optimize measured aozora-rs adapter stage"
```

## Task 10: Final Verification and Hickey/Rust Review

**Files:**
- Modify only if review finds issues.

- [ ] **Step 1: Run formatting**

```bash
cargo fmt --check
cargo fmt --check --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 2: Run tests**

```bash
cargo test --workspace
cargo test --manifest-path adapters/aozora-rs/Cargo.toml
```

Expected: PASS.

- [ ] **Step 3: Run clippy**

```bash
cargo clippy --workspace --all-targets -- -D warnings
cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings
```

Expected: PASS.

- [ ] **Step 4: Run benchmark compile check**

```bash
cargo bench --manifest-path adapters/aozora-rs/Cargo.toml --bench adapter_bench -- --test
```

Expected: PASS.

- [ ] **Step 5: Run Nix verification**

```bash
nix flake check
```

Expected: PASS.

- [ ] **Step 6: Perform Rich Hickey review**

Check and record answers in the final response:

- `lib.rs` is orchestration and public API glue only.
- `source.rs`, `parser.rs`, `aat.rs`, `projection.rs`, and `metrics.rs` each own one concern.
- Projection checks are text-only and do not parse JSON.
- Metrics are values and summary aggregation is testable without the full corpus.
- Fallback is represented as data in AAT metadata and aggregate summaries.
- Any optimization is justified by metrics rather than speculation.

- [ ] **Step 7: Commit any verification fixes**

If fixes were needed:

```bash
git status --short
git add adapters/aozora-rs/src crates/ab-compare/src crates/ab-compare/tests benchmarks/run-parser-comparison.sh benchmarks/baselines
git commit -m "fix: address aozora-rs performance verification"
```

If no fixes were needed, do not create an empty commit.
