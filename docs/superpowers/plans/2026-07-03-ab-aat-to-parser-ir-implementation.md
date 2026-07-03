# ab-aat-to-parser-ir Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a Rust library and CLI that converts measured AAT v1 JSON into ABC parser-IR JSON plus a deterministic divergence bundle, with explicit refusal when runtime divergence identity is not present in the measured mapping artifact.

**Architecture:** Implement a measured policy engine, not a generic mapping DSL and not a hard-coded rule table. This plan is not a clean "deepen" until it closes the traversal/protocol gates below: the mapping identity is depth-faithful replay of the generator's folded paths, not merely semantic loss taxonomy. Rust traversal code projects AAT values into parser-IR, but every divergence record is authorized by `data/aat-to-parser-ir-mapping-v1.json` through folded pointer/category/target lookup. The public seam is `convert(request) -> ConversionOutput`; callers do not know about traversal, span synthesis, schema identity, or divergence aggregation.

**Tech Stack:** Rust 2024, `serde`, `serde_json`, `jsonschema`, `clap`, `anyhow`, `sha2`, `regex`, local `ab-check` AAT validation, ABC JSON schemas from `../abc/schemas`.

## Global Constraints

- Mapping artifact authority: use `data/aat-to-parser-ir-mapping-v1.json`, `mapping_version = "0.1.1"`, 118 `transform_rule_descriptions`, 15 `UNSUPPORTED` folded rules.
- Do not hand-copy or resurrect the historical synthesized 27-rule table.
- Treat `transform_rule_descriptions[]` as measured authorization evidence, not executable DSL text.
- Compute schema hashes with `abc-legacy-json-c14n-v0`: parse JSON, serialize sorted compact UTF-8 JSON, escape every `/` as `\/`, SHA-256, prefix `sha256:`.
- Preflight must reproduce `mapping_schema_hash = sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`.
- Preflight must reproduce `target_parser_ir_schema_hash = sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`.
- ABC owns divergence record shape at `../abc/schemas/aat-parser-ir-divergence.schema.json`; ab-validator owns only `data/aat-parser-ir-divergence-bundle-v1.schema.json`.
- `records[]` items must have no fields outside ABC's record schema.
- Default unmeasured divergence behavior is `UnmeasuredDivergencePolicy::Refuse`.
- AAT validation must call `ab_check::check::validate_aat_value`; the `ab_check::aat` module does not expose that function.
- `ruby.direction` is direct projection after ADR 0024 and must not emit an L-02-style loss.
- Keep parser-IR `derived_from` unset in v1; current measured mapping records adapter provenance as lost and bundle-preserved.
- Runtime traversal must reproduce the generator's folded path protocol: replace numeric indexes with `[]`, strip any `=value` suffix, preserve `children[]` depth, and preserve pseudo parser-IR pointers such as `(none)` and `(emphasis.text)`.
- `children[]` traversal is mandatory before the converter is considered real-input capable. Mapping v1.1 has 59 folded rules under `children[]`; a content-only block traversal is structurally wrong.
- The mapping guard authorizes emitted records only. It does not prove that every applicable measured divergence was emitted, so the plan must add applicability tests for known measured rules and a documented coverage limitation.
- Do not emit divergence records that mapping v1.1 does not authorize. In v1, that means no `span.line_end -> span.line`, no `meta.warnings[].line -> warnings[].span.line`, and no `quote_block`/`caption_block` structural records unless the mapping artifact is extended.
- Warigaki is context-sensitive in the mapping. Plain block-content warigaki records with `parser_ir_pointer = null`; nested emphasis/heading contexts that measure `(emphasis.text)` must record that exact target pointer.
- CLI default `--abc-root` must be resolved from `CARGO_MANIFEST_DIR`/repo root as `repo_root.join("../abc")`, not from the caller's current working directory.
- Flake checks must not write to `/db` or require runtime network access.
- Pure flake schema validation must use an explicit `abc-src = { url = "path:../abc"; flake = false; }` input or an explicitly vendored schema copy. Do not have a derivation reach through `$PWD/../abc`.

---

## Incorporated Review Corrections

The critical review is accepted as a protocol/trust-boundary correction, not as ordinary polish. The implementation route is **Upstream Design + fake-seam/trust risk** until these gates are represented in the work:

- C1: Fix the AAT validation path to `ab_check::check::validate_aat_value`.
- C2/M1: Treat folded AAT pointer depth as rule identity. Implement `children[]` traversal and nested fixtures before claiming real-input support.
- C3: Remove or defer unmeasured divergence emissions for `span.line_end`, warning line spans, `quote_block`, and `caption_block`; handle heading/nested warigaki with the measured `(emphasis.text)` target pointer.
- M2: Record that the mapping guard is one-directional. Add applicability tests for measured divergences that the converter must emit, including `gaiji.jis_code -> gaiji.reference`.
- M3: Pin generator/consumer folding compatibility with a test over concrete first-path examples.
- M5: Add checked-in real measured AAT fixtures from aozora-rs and aozora2html buckets. The flake check uses those fixtures and never reads `/db`.
- M6/M7: Replace previously vague flake wiring and CWD-relative ABC defaults with concrete Nix and CLI behavior.

## Design Synthesis

**Understood**

- AAT JSON is the adapter contract in this repo.
- ABC parser-IR is the downstream conversion contract for ABC TEI/plaintext publication.
- The current mapping artifact is generated from measured aozora-rs and aozora2html buckets.
- The generated mapping's `(category, folded_aat_pointer, parser_ir_pointer)` key is depth-sensitive identity. Two semantically similar losses at different nesting depths are different measured rules.
- aozora2html warigaki is measured as `UNSUPPORTED`; parser-IR has no warigaki node.
- Production AAT spans are often absent; parser-IR spans are required.
- The Python mapper in `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py` is disposable prior art, not production code.

**Decided**

- Implement the accepted Alternative C from the spec: measured policy engine with mapping guard.
- Fold runtime AAT occurrence paths with the same rule as `mapping_doc.py`: replace `[number]` with `[]`, then strip any `=value` suffix.
- Build divergence rule identity from `(category, folded_aat_pointer, parser_ir_pointer)`.
- Preserve path depth as part of rule identity for v1. Do not semantically collapse all warigaki, style, heading, or gaiji losses into one rule.
- Use runtime first occurrence paths for `first_path`; use the mapping rule text, minus the generated corpus prefix, as the deterministic `message`.
- Aggregate divergence records per work and per mapping rule.
- Treat unsupported or lossy constructs without a measured mapping rule as outside v1 support under `Refuse`.

**Still Unknown, Consciously Deferred**

- Whether a future mapping should populate parser-IR `derived_from`.
- Whether ABC should add first-class warigaki/kunten nodes to parser-IR.
- Whether a later mapping protocol should grow executable transform expressions.
- Whether a later mapping should collapse depth-specialized rules into semantic rule families.
- Whether batch conversion needs resumable manifests; v1 only needs a single-document CLI.

## Evidence Ledger

| Claim | Type | Source | Confidence | How to verify | Impact if wrong |
|---|---|---|---|---|---|
| Mapping v1.1 is the authority for this crate | Observation | `docs/superpowers/specs/2026-07-03-ab-aat-to-parser-ir-crate-design.md` | High | Validate mapping against ABC schema | Manual mappings could drift from measured evidence |
| Mapping document uses `transform_rule_descriptions[]` | Observation | `jq 'keys' data/aat-to-parser-ir-mapping-v1.json` | High | Unit test deserializes this exact field | Wrong model would silently skip rules |
| ABC record schema allows only rule/category/message/count/first_path plus optional pointers and values | Observation | `../abc/schemas/aat-parser-ir-divergence.schema.json` | High | Schema validation test | Sidecar incompatibility with ABC |
| Missing AAT spans are an AMBIGUITY, not an INVENTION | Decision | Corrected mapping spec Gate 5 | High | Conversion test asserts span rule category | Incorrect loss taxonomy |
| Direct projection of `ruby.direction` is safe | Decision | Post-ADR 0024 correction | High | Conversion test asserts no divergence for direction | Reintroduces stale review finding |
| Generic mapping interpreter is premature | Inference | Mapping rules are descriptive strings | Medium | Revisit if mapping schema grows executable transforms | Overbuilds a fake seam |

## Use Cases

| Actor | Objective | Current obstacle | Capability when solved | How the design supports it |
|---|---|---|---|---|
| Parser comparison operator | Compare existing parsers through a shared downstream shape | AAT captures adapter output, ABC uses parser-IR | Convert measured AAT inputs whose divergence keys are authorized by mapping v1.1 | CLI validates input, target schema, and divergence bundle |
| ABC publication pipeline | Drive TEI/plaintext from parser-IR | ABC needs parser-IR fixtures and provenance | Consume deterministic parser-IR artifacts | Output validates against `../abc/schemas/parser-ir.schema.json` |
| Mapping maintainer | Detect when conversion logic outpaces measurement | Rust code can emit new divergence buckets accidentally | Refuse unmeasured divergence by default | `MappingIndex` authorizes runtime records from the generated artifact |
| Future parser implementer | Know which AAT constructs are unsupported or ambiguous | Loss is easy to hide in conversion output | Read per-work divergence records | Aggregator records count and first runtime path per rule |

## Decision Matrix

| Criterion | Status Quo | Direct Rust Port | Generic Mapping Interpreter | Measured Policy Engine With Mapping Guard |
|---|---|---|---|---|
| Data-driven behavior | No conversion tool exists | Copies probe behavior but rule drift is unchecked | Could be data-driven only after a real DSL exists | Uses generated measured mapping as runtime authorization |
| Manual mapping risk | None because no implementation | High: rule ids and loss cases tend to become hand tables | Medium: the DSL itself becomes a new manual protocol | Medium: emitted records are artifact-backed, but omission coverage must be tested |
| Complexity | Low short-term, no value | Low initial code, weak boundary | High surface area before evidence supports it | Moderate, with one deep module seam |
| ABC compatibility | No artifacts | Possible but sidecar drift likely | Unknown until DSL semantics are specified | Validates parser-IR, ABC records, and local bundle |
| Revisit trigger | ABC cannot test parser-IR publication flow | Probe and Rust results diverge | Mapping schema adds executable transforms | Mapping artifact changes categories, parser-IR schema changes, or generator folding/depth convention changes |

## File Structure

- Modify `Cargo.toml`: add workspace member and workspace dependency for `ab-aat-to-parser-ir`.
- Modify `flake.nix`: add an explicit `abc-src` schema input, expose package/app, and add sandbox-pure smoke check.
- Create `crates/ab-aat-to-parser-ir/Cargo.toml`: crate metadata and dependencies.
- Create `crates/ab-aat-to-parser-ir/src/lib.rs`: public conversion interface and module exports.
- Create `crates/ab-aat-to-parser-ir/src/schema.rs`: schema loading, `abc-legacy-json-c14n-v0`, JSON Schema validation.
- Create `crates/ab-aat-to-parser-ir/src/mapping.rs`: typed mapping document, loss taxonomy checks, folded rule lookup.
- Create `crates/ab-aat-to-parser-ir/src/divergence.rs`: runtime divergence event aggregation and bundle construction.
- Create `crates/ab-aat-to-parser-ir/src/convert.rs`: AAT traversal, parser-IR construction, span synthesis.
- Create `crates/ab-aat-to-parser-ir/src/main.rs`: thin CLI wrapper.
- Create `crates/ab-aat-to-parser-ir/tests/integration.rs`: library and CLI-level integration tests.
- Create `tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json`: checked-in measured AAT fixture.
- Create `tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json`: checked-in measured AAT fixture with at least one policy bucket.
- Create `tests/aat-to-parser-ir-cli-smoke.sh`: checked-in fixture smoke for Nix.

## Component Interfaces

```rust
pub struct ConversionRequest {
    pub aat: serde_json::Value,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
    pub options: ConversionOptions,
}

pub struct ConversionOptions {
    pub validate_input_aat: bool,
    pub validate_output_parser_ir: bool,
    pub on_unmeasured_divergence: UnmeasuredDivergencePolicy,
}

pub enum UnmeasuredDivergencePolicy {
    Refuse,
    RecordExploratory,
}

pub struct ConversionOutput {
    pub parser_ir: serde_json::Value,
    pub divergence_bundle: serde_json::Value,
    pub emitted_rule_ids: std::collections::BTreeSet<String>,
}

pub fn convert(request: ConversionRequest) -> anyhow::Result<ConversionOutput>;
```

---

### Task 1: Crate Scaffold And Canonical Schema Hashes

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-aat-to-parser-ir/Cargo.toml`
- Create: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Create: `crates/ab-aat-to-parser-ir/src/schema.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Produces: `SchemaSet::load(repo_root: &Path, abc_root: &Path) -> anyhow::Result<SchemaSet>`
- Produces: `abc_legacy_json_c14n_v0(value: &serde_json::Value) -> anyhow::Result<Vec<u8>>`
- Produces: `schema_hash(value: &serde_json::Value) -> anyhow::Result<String>`

- [ ] **Step 1: Add the failing schema hash tests**

```rust
use std::path::{Path, PathBuf};

use ab_aat_to_parser_ir::schema::{schema_hash, SchemaSet};
use serde_json::Value;

fn abc_root(repo: &Path) -> PathBuf {
    std::env::var_os("AB_ABC_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo.join("../abc"))
}

#[test]
fn legacy_schema_hashes_match_mapping_artifact() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();

    assert_eq!(
        schema_hash(&schemas.mapping_schema).unwrap(),
        "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
    );
    assert_eq!(
        schema_hash(&schemas.parser_ir_schema).unwrap(),
        "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
    );
}

#[test]
fn legacy_canonicalization_escapes_slashes() {
    let value: Value = serde_json::json!({"id": "https://abc.local/x", "b": 1, "a": 2});
    let payload = ab_aat_to_parser_ir::schema::abc_legacy_json_c14n_v0(&value).unwrap();
    assert_eq!(
        String::from_utf8(payload).unwrap(),
        r#"{"a":2,"b":1,"id":"https:\/\/abc.local\/x"}"#
    );
}
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir legacy_ -- --nocapture`

Expected: FAIL because the package and `schema` module do not exist.

- [ ] **Step 3: Add the crate scaffold**

```toml
# Cargo.toml
[workspace]
members = [
    "crates/ab-source-syntax",
    "crates/ab-encoding",
    "crates/ab-ir",
    "crates/ab-index",
    "crates/ab-check",
    "crates/ab-compare",
    "crates/ab-coverage",
    "crates/ab-diff-utils",
    "crates/ab-morph-diff",
    "crates/ab-plaintext",
    "crates/ab-morph-analyzers",
    "crates/ab-morph-run",
    "crates/ab-warehouse",
    "crates/ab-oracle",
    "crates/ab-aat-to-parser-ir",
]

[workspace.dependencies]
ab-aat-to-parser-ir = { path = "crates/ab-aat-to-parser-ir" }
```

```toml
# crates/ab-aat-to-parser-ir/Cargo.toml
[package]
name = "ab-aat-to-parser-ir"
version.workspace = true
edition.workspace = true
license.workspace = true
description = "Convert measured AAT v1 JSON into ABC parser-IR plus divergence evidence"
repository = "https://github.com/your-org/ab-validator"
readme = "../../README.md"
keywords = ["japanese", "text-processing", "parser-ir"]
categories = ["text-processing"]

[dependencies]
ab-check = { workspace = true }
anyhow = { workspace = true }
clap = { workspace = true }
jsonschema = { workspace = true }
regex = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
sha2 = { workspace = true }
```

- [ ] **Step 4: Add schema loading and hash code**

```rust
// crates/ab-aat-to-parser-ir/src/lib.rs
pub mod convert;
pub mod divergence;
pub mod mapping;
pub mod schema;

pub use convert::{convert, ConversionOptions, ConversionOutput, ConversionRequest, UnmeasuredDivergencePolicy};
pub use mapping::MappingDocument;
pub use schema::SchemaSet;
```

```rust
// crates/ab-aat-to-parser-ir/src/schema.rs
use std::{fs, path::Path};

use anyhow::{Context, Result};
use serde::Serialize;
use serde_json::Value;
use sha2::{Digest, Sha256};

#[derive(Debug, Clone)]
pub struct SchemaSet {
    pub aat_schema: Value,
    pub mapping_schema: Value,
    pub parser_ir_schema: Value,
    pub abc_divergence_record_schema: Value,
    pub bundle_schema: Value,
}

impl SchemaSet {
    pub fn load(repo_root: &Path, abc_root: &Path) -> Result<Self> {
        Ok(Self {
            aat_schema: read_json(&repo_root.join("data/aat-schema.json"))?,
            mapping_schema: read_json(&abc_root.join("schemas/aat-parser-ir-mapping.schema.json"))?,
            parser_ir_schema: read_json(&abc_root.join("schemas/parser-ir.schema.json"))?,
            abc_divergence_record_schema: read_json(&abc_root.join("schemas/aat-parser-ir-divergence.schema.json"))?,
            bundle_schema: read_json(&repo_root.join("data/aat-parser-ir-divergence-bundle-v1.schema.json"))?,
        })
    }
}

pub fn read_json(path: &Path) -> Result<Value> {
    let text = fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("failed to parse {}", path.display()))
}

pub fn validate_value(schema: &Value, value: &Value, label: &str) -> Result<()> {
    let validator = jsonschema::validator_for(schema)
        .with_context(|| format!("failed to compile {label} schema"))?;
    validator.validate(value).map_err(|error| {
        anyhow::anyhow!("{label} validation failed at {}: {error}", error.instance_path())
    })
}

pub fn abc_legacy_json_c14n_v0(value: &Value) -> Result<Vec<u8>> {
    let text = sort_json_text(value)?;
    Ok(text.replace('/', "\\/").into_bytes())
}

fn sort_json_text(value: &Value) -> Result<String> {
    let mut bytes = Vec::new();
    let formatter = serde_json::ser::CompactFormatter;
    let mut serializer = serde_json::Serializer::with_formatter(&mut bytes, formatter);
    value.serialize(&mut serializer)?;
    Ok(String::from_utf8(bytes)?)
}

pub fn schema_hash(value: &Value) -> Result<String> {
    let payload = abc_legacy_json_c14n_v0(value)?;
    let mut hasher = Sha256::new();
    hasher.update(payload);
    Ok(format!("sha256:{:x}", hasher.finalize()))
}
```

Implementation note: `serde_json::Value` object keys are backed by `BTreeMap` unless the `preserve_order` feature is enabled. Confirm this with the slash-order test above. If a future feature changes object ordering, replace `sort_json_text` with explicit recursive `BTreeMap` normalization.

- [ ] **Step 5: Run the focused test and verify it passes**

Run: `cargo test -p ab-aat-to-parser-ir legacy_ -- --nocapture`

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add Cargo.toml crates/ab-aat-to-parser-ir
git commit -m "feat: scaffold aat to parser-ir crate"
```

### Task 2: Mapping Document Model And Preflight Guard

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/mapping.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `SchemaSet`, `schema_hash`, `validate_value`
- Produces: `MappingDocument::from_path(path: &Path) -> anyhow::Result<MappingDocument>`
- Produces: `MappingDocument::preflight(&self, schemas: &SchemaSet) -> anyhow::Result<MappingIndex>`
- Produces: `MappingIndex::require_rule(category, aat_pointer, parser_ir_pointer) -> anyhow::Result<&MappingRule>`

- [ ] **Step 1: Add failing mapping preflight tests**

```rust
use ab_aat_to_parser_ir::{MappingDocument, SchemaSet};

#[test]
fn mapping_preflight_accepts_checked_in_v1_1_artifact() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();

    let index = mapping.preflight(&schemas).unwrap();

    assert_eq!(mapping.mapping_version, "0.1.1");
    assert_eq!(mapping.transform_rule_descriptions.len(), 118);
    assert!(index
        .require_rule("UNSUPPORTED", Some("blocks[].content[].warigaki"), None)
        .unwrap()
        .description
        .contains("warigaki"));
}

#[test]
fn mapping_preflight_rejects_wrong_target_schema_hash() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mut mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    mapping.target_parser_ir_schema_hash = format!("sha256:{}", "0".repeat(64));

    let error = mapping.preflight(&schemas).unwrap_err().to_string();

    assert!(error.contains("target parser-IR schema hash"));
}

#[test]
fn mapping_preflight_rejects_stale_gaiji_pointers() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mut mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    mapping.transform_rule_descriptions.push(ab_aat_to_parser_ir::mapping::MappingRule {
        rule_id: "A-99".to_owned(),
        category: "AMBIGUITY".to_owned(),
        aat_pointer: Some("blocks[].content[].gaiji.raw_marker".to_owned()),
        parser_ir_pointer: Some("gaiji.raw_marker".to_owned()),
        action: "project".to_owned(),
        description: "stale pointer fixture".to_owned(),
    });

    let error = mapping.preflight(&schemas).unwrap_err().to_string();

    assert!(error.contains("A-99"));
    assert!(error.contains("gaiji.raw_marker"));
}

#[test]
fn folded_pointer_protocol_matches_generator_examples() {
    use ab_aat_to_parser_ir::mapping::fold_aat_pointer;

    assert_eq!(
        fold_aat_pointer("blocks[3].children[1].heading.content[0].warigaki"),
        "blocks[].children[].heading.content[].warigaki"
    );
    assert_eq!(
        fold_aat_pointer("blocks[9].content[2].content[0].warigaki"),
        "blocks[].content[].content[].warigaki"
    );
    assert_eq!(
        fold_aat_pointer("meta.source_encoding=windows-31j-lossy"),
        "meta.source_encoding"
    );
    assert_eq!(fold_aat_pointer("(emphasis.text)"), "(emphasis.text)");
}
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir mapping_preflight -- --nocapture`

Expected: FAIL because `mapping.rs` is not implemented.

- [ ] **Step 3: Implement typed mapping and folded lookup**

```rust
// crates/ab-aat-to-parser-ir/src/mapping.rs
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    sync::OnceLock,
};

use anyhow::{bail, Context, Result};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::schema::{schema_hash, validate_value, SchemaSet};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MappingDocument {
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_schema_hash: String,
    pub source_aat_version: u64,
    pub target_parser_ir_schema_id: String,
    pub target_parser_ir_schema_hash: String,
    pub transform_rule_descriptions: Vec<MappingRule>,
    pub loss_taxonomy: BTreeMap<String, LossTaxonomyEntry>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MappingRule {
    pub rule_id: String,
    pub category: String,
    pub aat_pointer: Option<String>,
    pub parser_ir_pointer: Option<String>,
    pub action: String,
    pub description: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LossTaxonomyEntry {
    pub description: String,
    pub default_action: String,
    pub records_sidecar: bool,
}

#[derive(Debug, Clone)]
pub struct MappingIndex {
    rules: BTreeMap<(String, Option<String>, Option<String>), MappingRule>,
}

impl MappingDocument {
    pub fn from_path(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
        serde_json::from_str(&text).with_context(|| format!("failed to parse {}", path.display()))
    }

    pub fn preflight(&self, schemas: &SchemaSet) -> Result<MappingIndex> {
        let value = serde_json::to_value(self)?;
        validate_value(&schemas.mapping_schema, &value, "AAT parser-IR mapping")?;

        let mapping_schema_hash = schema_hash(&schemas.mapping_schema)?;
        if self.mapping_schema_hash != mapping_schema_hash {
            bail!("mapping schema hash mismatch: document={} computed={mapping_schema_hash}", self.mapping_schema_hash);
        }

        let parser_ir_schema_hash = schema_hash(&schemas.parser_ir_schema)?;
        if self.target_parser_ir_schema_hash != parser_ir_schema_hash {
            bail!("target parser-IR schema hash mismatch: document={} computed={parser_ir_schema_hash}", self.target_parser_ir_schema_hash);
        }

        if self.source_aat_version != 1 {
            bail!("unsupported source_aat_version {}", self.source_aat_version);
        }

        for category in ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"] {
            if !self.loss_taxonomy.contains_key(category) {
                bail!("loss taxonomy missing {category}");
            }
        }

        let allowed = allowed_aat_pointers(&schemas.aat_schema);
        let mut rules = BTreeMap::new();
        for rule in &self.transform_rule_descriptions {
            if !self.loss_taxonomy.contains_key(&rule.category) {
                bail!("{} uses category without taxonomy entry: {}", rule.rule_id, rule.category);
            }
            if let Some(pointer) = &rule.aat_pointer {
                let folded = fold_aat_pointer(pointer);
                if !allowed.contains(&folded) {
                    bail!("{} has non-schema AAT pointer {pointer}", rule.rule_id);
                }
            }
            let key = (
                rule.category.clone(),
                rule.aat_pointer.as_deref().map(fold_aat_pointer),
                rule.parser_ir_pointer.clone(),
            );
            if rules.insert(key, rule.clone()).is_some() {
                bail!("duplicate folded mapping rule {}", rule.rule_id);
            }
        }
        Ok(MappingIndex { rules })
    }
}

impl MappingIndex {
    pub fn has_rule(
        &self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
    ) -> bool {
        let key = (
            category.to_owned(),
            aat_pointer.map(fold_aat_pointer),
            parser_ir_pointer.map(ToOwned::to_owned),
        );
        self.rules.contains_key(&key)
    }

    pub fn require_rule(
        &self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
    ) -> Result<&MappingRule> {
        let key = (
            category.to_owned(),
            aat_pointer.map(fold_aat_pointer),
            parser_ir_pointer.map(ToOwned::to_owned),
        );
        self.rules.get(&key).ok_or_else(|| {
            anyhow::anyhow!(
                "unmeasured divergence: category={category} aat_pointer={} parser_ir_pointer={}",
                aat_pointer.map(fold_aat_pointer).unwrap_or_else(|| "null".to_owned()),
                parser_ir_pointer.unwrap_or("null")
            )
        })
    }
}

pub fn fold_aat_pointer(pointer: &str) -> String {
    static INDEX_RE: OnceLock<Regex> = OnceLock::new();
    let index_re = INDEX_RE.get_or_init(|| Regex::new(r"\[[0-9]+\]").expect("valid index regex"));
    let folded = index_re.replace_all(pointer.trim_start_matches("$."), "[]").to_string();
    folded
        .split_once('=')
        .map(|(path, _)| path.to_owned())
        .unwrap_or(folded)
}

fn allowed_aat_pointers(aat_schema: &Value) -> BTreeSet<String> {
    let mut allowed = BTreeSet::new();
    collect_paths(aat_schema, aat_schema, "", &mut allowed, 0);
    allowed
}

fn collect_paths(schema: &Value, node: &Value, prefix: &str, out: &mut BTreeSet<String>, depth: usize) {
    if depth > 12 {
        return;
    }
    let node = deref(schema, node);
    if let Some(branches) = node.get("oneOf").and_then(Value::as_array) {
        for branch in branches {
            let resolved = deref(schema, branch);
            for kind in kind_values(resolved) {
                if !prefix.is_empty() {
                    let branch_prefix = format!("{prefix}.{kind}");
                    out.insert(branch_prefix.clone());
                    collect_paths(schema, resolved, &branch_prefix, out, depth + 1);
                }
            }
            collect_paths(schema, resolved, prefix, out, depth + 1);
        }
        return;
    }
    for keyword in ["allOf", "anyOf"] {
        if let Some(branches) = node.get(keyword).and_then(Value::as_array) {
            for branch in branches {
                collect_paths(schema, branch, prefix, out, depth + 1);
            }
            return;
        }
    }
    if node.get("type").and_then(Value::as_str) == Some("array") {
        if !prefix.is_empty() {
            out.insert(prefix.to_owned());
        }
        if prefix.matches("[]").count() >= 6 {
            return;
        }
        if let Some(items) = node.get("items") {
            collect_paths(schema, items, &format!("{prefix}[]"), out, depth + 1);
        }
        return;
    }
    if node.get("type").and_then(Value::as_str) == Some("object") || node.get("properties").is_some() {
        if !prefix.is_empty() {
            out.insert(prefix.to_owned());
        }
        if let Some(properties) = node.get("properties").and_then(Value::as_object) {
            for (name, child) in properties {
                if name == "kind" {
                    continue;
                }
                let child_prefix = if prefix.is_empty() {
                    name.to_owned()
                } else {
                    format!("{prefix}.{name}")
                };
                out.insert(child_prefix.clone());
                collect_paths(schema, child, &child_prefix, out, depth + 1);
            }
        }
    }
}

fn deref<'a>(schema: &'a Value, node: &'a Value) -> &'a Value {
    let Some(reference) = node.get("$ref").and_then(Value::as_str) else {
        return node;
    };
    let prefix = "#/$defs/";
    if let Some(name) = reference.strip_prefix(prefix) {
        &schema["$defs"][name]
    } else {
        node
    }
}

fn kind_values(node: &Value) -> Vec<String> {
    let Some(kind) = node.pointer("/properties/kind") else {
        return Vec::new();
    };
    if let Some(value) = kind.get("const").and_then(Value::as_str) {
        return vec![value.to_owned()];
    }
    kind.get("enum")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
        .map(ToOwned::to_owned)
        .collect()
}
```

- [ ] **Step 4: Run the focused test and verify it passes**

Run: `cargo test -p ab-aat-to-parser-ir mapping_preflight -- --nocapture`

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/mapping.rs crates/ab-aat-to-parser-ir/src/lib.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat: guard aat parser-ir mapping preflight"
```

### Task 3: Divergence Aggregation And Bundle Validation

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/divergence.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `MappingIndex`
- Produces: `DivergenceRecorder::record(category: &str, aat_pointer: Option<&str>, parser_ir_pointer: Option<&str>, source_value: Option<Value>, target_value: Option<Value>) -> anyhow::Result<()>`
- Produces: `DivergenceRecorder::emitted_rule_ids(&self) -> BTreeSet<String>` for fixture-level applicability checks.
- Produces: `DivergenceRecorder::bundle(work: AatMeta, schemas: &SchemaSet, mapping: &MappingDocument) -> anyhow::Result<Value>`

- [ ] **Step 1: Add failing aggregation and schema tests**

```rust
#[test]
fn divergence_records_aggregate_by_mapping_rule_and_validate_against_abc_schema() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let index = mapping.preflight(&schemas).unwrap();

    let mut recorder = ab_aat_to_parser_ir::divergence::DivergenceRecorder::new(index);
    recorder.record("AMBIGUITY", Some("meta.source_hash"), Some("source.work_content_hash"), Some("sha256:0000000000000000000000000000000000000000000000000000000000000000".into()), Some("sha256:0000000000000000000000000000000000000000000000000000000000000000".into())).unwrap();
    recorder.record("AMBIGUITY", Some("meta.source_hash"), Some("source.work_content_hash"), None, None).unwrap();

    let bundle = recorder.bundle(
        ab_aat_to_parser_ir::divergence::AatMeta {
            work_id: "fixture".to_owned(),
            version: 1,
            adapter: "fixture".to_owned(),
            adapter_version: "fixture 0.1.0".to_owned(),
            source_hash: "sha256:0000000000000000000000000000000000000000000000000000000000000000".to_owned(),
            parse_complete: true,
            metrics: serde_json::Value::Null,
            semantic_summary: serde_json::Value::Null,
        },
        &schemas,
        &mapping,
    ).unwrap();

    assert_eq!(bundle.pointer("/summary/AMBIGUITY").and_then(serde_json::Value::as_u64), Some(2));
    assert_eq!(bundle.pointer("/records/0/count").and_then(serde_json::Value::as_u64), Some(2));
    assert_eq!(bundle.pointer("/records/0/first_path").and_then(serde_json::Value::as_str), Some("meta.source_hash"));

    ab_aat_to_parser_ir::schema::validate_value(&schemas.bundle_schema, &bundle, "bundle").unwrap();
    let record = bundle.pointer("/records/0").unwrap();
    ab_aat_to_parser_ir::schema::validate_value(&schemas.abc_divergence_record_schema, record, "ABC divergence record").unwrap();
}
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir divergence_records_aggregate -- --nocapture`

Expected: FAIL because `DivergenceRecorder` is not implemented.

- [ ] **Step 3: Implement recorder and bundle**

```rust
// crates/ab-aat-to-parser-ir/src/divergence.rs
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use anyhow::Result;
use serde_json::{json, Value};

use crate::{
    mapping::{MappingDocument, MappingIndex, MappingRule},
    schema::{validate_value, SchemaSet},
};

#[derive(Debug, Clone)]
pub struct AatMeta {
    pub work_id: String,
    pub version: u64,
    pub adapter: String,
    pub adapter_version: String,
    pub source_hash: String,
    pub parse_complete: bool,
    pub metrics: Value,
    pub semantic_summary: Value,
}

#[derive(Debug, Clone)]
struct AggregatedRecord {
    rule: MappingRule,
    count: u64,
    first_path: Option<String>,
    source_value: Option<Value>,
    target_value: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct DivergenceRecorder {
    index: MappingIndex,
    records: BTreeMap<String, AggregatedRecord>,
}

impl DivergenceRecorder {
    pub fn new(index: MappingIndex) -> Self {
        Self { index, records: BTreeMap::new() }
    }

    pub fn record(
        &mut self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
        source_value: Option<Value>,
        target_value: Option<Value>,
    ) -> Result<()> {
        let rule = self.index.require_rule(category, aat_pointer, parser_ir_pointer)?.clone();
        let entry = self.records.entry(rule.rule_id.clone()).or_insert_with(|| AggregatedRecord {
            rule,
            count: 0,
            first_path: aat_pointer.map(ToOwned::to_owned),
            source_value: source_value.clone(),
            target_value: target_value.clone(),
        });
        entry.count += 1;
        Ok(())
    }

    pub fn emitted_rule_ids(&self) -> BTreeSet<String> {
        self.records.keys().cloned().collect()
    }

    pub fn bundle(self, meta: AatMeta, schemas: &SchemaSet, mapping: &MappingDocument) -> Result<Value> {
        let mut summary = BTreeMap::from([
            ("LOSS", 0_u64),
            ("INVENTION", 0_u64),
            ("AMBIGUITY", 0_u64),
            ("UNSUPPORTED", 0_u64),
            ("STRUCTURAL", 0_u64),
        ]);

        let records: Vec<Value> = self.records.into_values().map(|record| {
            *summary.get_mut(record.rule.category.as_str()).expect("known category") += record.count;
            json!({
                "rule_id": record.rule.rule_id,
                "category": record.rule.category,
                "aat_pointer": record.rule.aat_pointer,
                "parser_ir_pointer": record.rule.parser_ir_pointer,
                "source_value": record.source_value,
                "target_value": record.target_value,
                "message": rule_message(&record.rule.description),
                "count": record.count,
                "first_path": record.first_path,
            })
        }).collect();

        let bundle = json!({
            "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
            "schema_version": "0.1.0",
            "work_id": meta.work_id,
            "mapping": {
                "mapping_id": mapping.mapping_id,
                "mapping_version": mapping.mapping_version,
                "mapping_schema_hash": mapping.mapping_schema_hash,
            },
            "target": {
                "parser_ir_schema_id": mapping.target_parser_ir_schema_id,
                "parser_ir_schema_hash": mapping.target_parser_ir_schema_hash,
            },
            "aat": {
                "version": meta.version,
                "adapter": meta.adapter,
                "adapter_version": meta.adapter_version,
                "source_hash": meta.source_hash,
                "parse_complete": meta.parse_complete,
            },
            "preserved_aat_meta": {
                "metrics": meta.metrics,
                "semantic_summary": meta.semantic_summary,
            },
            "summary": summary,
            "records": records,
        });

        validate_value(&schemas.bundle_schema, &bundle, "AAT parser-IR divergence bundle")?;
        for record in bundle["records"].as_array().into_iter().flatten() {
            validate_value(&schemas.abc_divergence_record_schema, record, "ABC divergence record")?;
        }
        Ok(bundle)
    }
}

fn rule_message(description: &str) -> String {
    description
        .split_once(". ")
        .map(|(_, rest)| rest.to_owned())
        .unwrap_or_else(|| description.to_owned())
}
```

Implementation note: aggregation keeps `source_value` and `target_value` from the first occurrence of a rule and increments `count` for later occurrences. That is a deliberate v1 evidence tradeoff; tests must assert `first_path` and `count`, not expect every occurrence value in the bundle.

- [ ] **Step 4: Run the focused test and verify it passes**

Run: `cargo test -p ab-aat-to-parser-ir divergence_records_aggregate -- --nocapture`

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/divergence.rs crates/ab-aat-to-parser-ir/src/lib.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat: aggregate parser-ir divergence bundle"
```

### Task 4: Minimal Text, Ruby, Gaiji, Source, And Span Conversion

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `MappingDocument::preflight`, `DivergenceRecorder`
- Produces: `convert(request: ConversionRequest) -> anyhow::Result<ConversionOutput>`

- [ ] **Step 1: Add failing minimal conversion tests**

```rust
#[test]
fn converts_text_ruby_gaiji_and_validates_parser_ir() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let aat = serde_json::json!({
        "version": 1,
        "work_id": "minimal",
        "meta": {
            "adapter": "fixture",
            "adapter_version": "fixture 0.1.0",
            "source_encoding": "utf-8",
            "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
            "parse_complete": true,
            "warnings": []
        },
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "text", "value": "吾輩"},
                {"kind": "ruby", "base": "猫", "reading": "ねこ", "direction": "right"},
                {"kind": "gaiji", "description": "※［＃猫］", "resolved": "猫", "jis_code": "1-2-3", "unresolved_reason": null}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ab_aat_to_parser_ir::ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ab_aat_to_parser_ir::ConversionOptions::default(),
    }).unwrap();

    assert_eq!(output.parser_ir.pointer("/nodes/0/type").and_then(serde_json::Value::as_str), Some("text"));
    assert_eq!(output.parser_ir.pointer("/nodes/1/ruby/direction").and_then(serde_json::Value::as_str), Some("right"));
    assert_eq!(output.parser_ir.pointer("/nodes/2/gaiji/raw_marker").and_then(serde_json::Value::as_str), Some("※［＃猫］"));
    assert!(output.emitted_rule_ids.contains("A-18"), "gaiji.jis_code ambiguity must not be silently omitted");
    assert!(!output.divergence_bundle["records"].as_array().unwrap().iter().any(|record| {
        record["message"].as_str().unwrap_or("").contains("direction")
    }));

    ab_aat_to_parser_ir::schema::validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir converts_text_ruby_gaiji -- --nocapture`

Expected: FAIL because `convert` is not implemented.

- [ ] **Step 3: Implement conversion skeleton and node projection**

```rust
// crates/ab-aat-to-parser-ir/src/convert.rs
use anyhow::{bail, Result};
use serde_json::{json, Value};

use crate::{
    divergence::{AatMeta, DivergenceRecorder},
    mapping::MappingDocument,
    schema::{validate_value, SchemaSet},
};

#[derive(Debug, Clone)]
pub struct ConversionRequest {
    pub aat: Value,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
    pub options: ConversionOptions,
}

#[derive(Debug, Clone)]
pub struct ConversionOptions {
    pub validate_input_aat: bool,
    pub validate_output_parser_ir: bool,
    pub on_unmeasured_divergence: UnmeasuredDivergencePolicy,
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            on_unmeasured_divergence: UnmeasuredDivergencePolicy::Refuse,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnmeasuredDivergencePolicy {
    Refuse,
    RecordExploratory,
}

#[derive(Debug, Clone)]
pub struct ConversionOutput {
    pub parser_ir: Value,
    pub divergence_bundle: Value,
    pub emitted_rule_ids: std::collections::BTreeSet<String>,
}

pub fn convert(request: ConversionRequest) -> Result<ConversionOutput> {
    if request.options.validate_input_aat {
        ab_check::check::validate_aat_value(&request.aat)?;
    }

    let index = request.mapping.preflight(&request.schemas)?;
    let mut recorder = DivergenceRecorder::new(index);
    let mut nodes = Vec::new();
    let mut offset = 0_u64;

    for (block_index, block) in request.aat["blocks"].as_array().into_iter().flatten().enumerate() {
        offset = map_block(block, &mut nodes, &mut recorder, offset, &format!("blocks[{block_index}]"))?;
    }

    let source = map_source(&request.aat, &mut recorder)?;
    recorder.record("INVENTION", None, Some("schema_id/schema_hash"), None, None)?;
    let warnings = map_warnings(&request.aat, &mut recorder)?;
    recorder.record("INVENTION", None, Some("errors[]"), None, None)?;

    let parser_ir = json!({
        "schema_id": request.mapping.target_parser_ir_schema_id,
        "schema_hash": request.mapping.target_parser_ir_schema_hash,
        "source": source,
        "nodes": nodes,
        "warnings": warnings,
        "errors": [],
    });

    if request.options.validate_output_parser_ir {
        validate_value(&request.schemas.parser_ir_schema, &parser_ir, "parser-IR")?;
    }

    let emitted_rule_ids = recorder.emitted_rule_ids();
    let divergence_bundle = recorder.bundle(aat_meta(&request.aat), &request.schemas, &request.mapping)?;
    Ok(ConversionOutput { parser_ir, divergence_bundle, emitted_rule_ids })
}
```

Continue this step with private helpers:

```rust
fn map_block(
    block: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let kind = block["kind"].as_str().unwrap_or("unknown");
    let block_pointer = format!("{path}.{kind}");
    recorder.record("STRUCTURAL", Some(block_pointer.as_str()), None, None, None)?;
    let mut current = offset;
    match kind {
        "paragraph" => {
            current = map_inline_content(block.get("content"), nodes, recorder, current, &format!("{path}.content"))?;
        }
        other => bail!("unsupported block kind in minimal slice: {other}"),
    }

    for (index, child_block) in block["children"].as_array().into_iter().flatten().enumerate() {
        current = map_block(child_block, nodes, recorder, current, &format!("{path}.children[{index}]"))?;
    }

    Ok(current)
}

fn map_inline_content(
    content: Option<&Value>,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let mut current = offset;
    for (index, child) in content.and_then(Value::as_array).into_iter().flatten().enumerate() {
        if let Some((node, next)) = map_inline(child, recorder, current, &format!("{path}[{index}]"))? {
            nodes.push(node);
            current = next;
        }
    }
    Ok(current)
}

fn map_inline(node: &Value, recorder: &mut DivergenceRecorder, offset: u64, path: &str) -> Result<Option<(Value, u64)>> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => {
            let text = node["value"].as_str().unwrap_or("");
            let end = offset + utf8_len(text);
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            Ok(Some((json!({"type": "text", "span": span, "text": text}), end)))
        }
        "ruby" => {
            let base = node["base"].as_str().unwrap_or("");
            let end = offset + utf8_len(base);
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            recorder.record("INVENTION", None, Some("ruby.scope"), None, Some(json!("explicit")))?;
            Ok(Some((json!({
                "type": "ruby",
                "span": span,
                "ruby": {
                    "base": base,
                    "reading": node["reading"].as_str().unwrap_or(""),
                    "scope": "explicit",
                    "direction": node.get("direction").and_then(Value::as_str)
                }
            }), end)))
        }
        "gaiji" => {
            let visible = node["resolved"].as_str().unwrap_or_else(|| node["description"].as_str().unwrap_or(""));
            let end = offset + utf8_len(visible);
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            let description_pointer = format!("{path}.gaiji.description");
            let resolved_pointer = format!("{path}.gaiji.resolved");
            recorder.record("INVENTION", Some(description_pointer.as_str()), Some("gaiji.raw_marker"), None, None)?;
            recorder.record("AMBIGUITY", Some(resolved_pointer.as_str()), Some("gaiji.resolved"), None, None)?;
            if node.get("jis_code").is_some_and(|value| !value.is_null()) {
                let jis_pointer = format!("{path}.gaiji.jis_code");
                recorder.record(
                    "AMBIGUITY",
                    Some(jis_pointer.as_str()),
                    Some("gaiji.reference"),
                    node.get("jis_code").cloned(),
                    node.get("jis_code").cloned(),
                )?;
            }
            recorder.record("LOSS", None, Some("gaiji.unicode"), None, Some(Value::Null))?;
            Ok(Some((json!({
                "type": "gaiji",
                "span": span,
                "gaiji": {
                    "raw_marker": node["description"].as_str().unwrap_or(""),
                    "reference": node.get("jis_code").cloned().unwrap_or(Value::Null),
                    "unicode": null,
                    "ivs": null,
                    "image_or_glyph_fallback": null,
                    "resolved": node.get("resolved").is_some_and(|value| !value.is_null())
                }
            }), end)))
        }
        other => bail!("unsupported inline kind in minimal slice: {other}"),
    }
}
```

Use UTF-8 byte length, not Rust character count, for span fallback:

```rust
fn utf8_len(value: &str) -> u64 {
    value.as_bytes().len() as u64
}

fn map_span(
    aat_span: Option<&Value>,
    fallback_start: u64,
    fallback_end: u64,
    recorder: &mut DivergenceRecorder,
    path: &str,
) -> Result<Value> {
    let Some(span) = aat_span else {
        let span_pointer = format!("{path}.span");
        recorder.record("AMBIGUITY", Some(span_pointer.as_str()), Some("span"), None, None)?;
        return Ok(json!({
            "start": fallback_start,
            "end": fallback_end,
            "line": null,
            "column": null,
            "coordinate_system": "decoded_utf8",
        }));
    };
    Ok(json!({
        "start": span.get("byte_start").and_then(Value::as_u64).unwrap_or(fallback_start),
        "end": span.get("byte_end").and_then(Value::as_u64).unwrap_or(fallback_end),
        "line": span.get("line_start").cloned().unwrap_or(Value::Null),
        "column": null,
        "coordinate_system": "decoded_utf8",
    }))
}

// `line_end` is intentionally not recorded in v1 because mapping v1.1 has no
// measured `span.line_end -> span.line` rule. Preserve `line_start` as
// `span.line`; defer multi-line span loss recording until the mapping grows a
// rule for it.
fn map_source(aat: &Value, recorder: &mut DivergenceRecorder) -> Result<Value> {
    let meta = &aat["meta"];
    let source_encoding = meta["source_encoding"].as_str().unwrap_or("utf-8");
    let encoding = match source_encoding {
        "utf-8" | "utf-8-bom" => "UTF-8",
        "windows-31j" | "windows-31j-lossy" => "Shift_JIS",
        _ => "unknown",
    };
    if matches!(source_encoding, "utf-8-bom" | "windows-31j-lossy") {
        let encoding_pointer = format!("meta.source_encoding={source_encoding}");
        recorder.record("AMBIGUITY", Some(encoding_pointer.as_str()), Some("source.encoding"), None, Some(json!(encoding)))?;
    }
    recorder.record("AMBIGUITY", Some("meta.source_hash"), Some("source.work_content_hash"), meta.get("source_hash").cloned(), meta.get("source_hash").cloned())?;
    recorder.record("INVENTION", None, Some("source.normalization"), None, Some(json!("source")))?;
    recorder.record("INVENTION", None, Some("source.source_path"), None, Some(Value::Null))?;
    for field in ["adapter", "adapter_version", "parse_complete"] {
        let field_pointer = format!("meta.{field}");
        recorder.record("LOSS", Some(field_pointer.as_str()), None, meta.get(field).cloned(), None)?;
    }
    if meta.get("metrics").is_some_and(|value| !value.is_null()) {
        recorder.record("LOSS", Some("meta.metrics"), None, meta.get("metrics").cloned(), None)?;
    }
    if meta.get("semantic_summary").is_some_and(|value| !value.is_null()) {
        recorder.record("LOSS", Some("meta.semantic_summary"), None, meta.get("semantic_summary").cloned(), None)?;
    }
    Ok(json!({
        "work_content_hash": meta["source_hash"].as_str().unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000"),
        "source_path": null,
        "encoding": encoding,
        "normalization": "source",
    }))
}

fn map_warnings(aat: &Value, recorder: &mut DivergenceRecorder) -> Result<Value> {
    let mut warnings = Vec::new();
    for warning in aat.pointer("/meta/warnings").and_then(Value::as_array).into_iter().flatten() {
        recorder.record("INVENTION", None, Some("warnings[].severity"), None, Some(json!("warning")))?;
        recorder.record("INVENTION", None, Some("warnings[].code"), None, Some(json!("AAT_WARNING")))?;
        warnings.push(json!({
            "severity": "warning",
            "code": "AAT_WARNING",
            "message": warning["message"].as_str().unwrap_or(""),
            "span": null,
            "construct": null,
            "recovery": null,
        }));
    }
    Ok(Value::Array(warnings))
}

fn aat_meta(aat: &Value) -> AatMeta {
    let meta = &aat["meta"];
    AatMeta {
        work_id: aat["work_id"].as_str().unwrap_or("unknown").to_owned(),
        version: aat["version"].as_u64().unwrap_or(1),
        adapter: meta["adapter"].as_str().unwrap_or("unknown").to_owned(),
        adapter_version: meta["adapter_version"].as_str().unwrap_or("unknown").to_owned(),
        source_hash: meta["source_hash"].as_str().unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000").to_owned(),
        parse_complete: meta["parse_complete"].as_bool().unwrap_or(false),
        metrics: meta.get("metrics").cloned().unwrap_or(Value::Null),
        semantic_summary: meta.get("semantic_summary").cloned().unwrap_or(Value::Null),
    }
}
```

- [ ] **Step 4: Run the focused test and verify it passes**

Run: `cargo test -p ab-aat-to-parser-ir converts_text_ruby_gaiji -- --nocapture`

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs crates/ab-aat-to-parser-ir/src/lib.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat: convert minimal aat nodes to parser-ir"
```

### Task 5: Measured Policy Coverage For Style, Heading, Warnings, Warigaki, And Kunten

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/mapping.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `convert`
- Produces: measured handling for `style`, `heading`, `jisage_block`, `warigaki`, `meta.warnings`, and lossy encoding ambiguity.
- Produces: explicit v1 refusal/defer behavior for unmeasured `quote_block`, `caption_block`, warning line spans, and span line-end loss.

- [ ] **Step 1: Add failing measured policy tests**

```rust
#[test]
fn measured_policy_projects_style_kunten_heading_warning_and_warigaki() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let aat = serde_json::json!({
        "version": 1,
        "work_id": "policy",
        "meta": {
            "adapter": "fixture",
            "adapter_version": "fixture 0.1.0",
            "source_encoding": "windows-31j-lossy",
            "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "parse_complete": true,
            "warnings": [{ "message": "fixture warning", "line": 2 }]
        },
        "blocks": [
            {
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "前"}],
                "children": [{
                    "kind": "heading",
                    "level": 1,
                    "style": "fixture",
                    "content": [{ "kind": "warigaki", "upper": [{"kind": "text", "value": "上"}], "lower": [{"kind": "text", "value": "下"}] }]
                }]
            },
            {
                "kind": "paragraph",
                "content": [
                    {"kind": "style", "style_type": "kaeriten", "content": [{"kind": "text", "value": "レ"}]},
                    {"kind": "warigaki", "upper": [{"kind": "text", "value": "甲"}], "lower": [{"kind": "text", "value": "乙"}]}
                ]
            }
        ]
    });

    let output = ab_aat_to_parser_ir::convert(ab_aat_to_parser_ir::ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ab_aat_to_parser_ir::ConversionOptions::default(),
    }).unwrap();

    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    assert!(nodes.iter().any(|node| node["type"] == "heading" && node["text"] == "上下"));
    assert!(nodes.iter().any(|node| node["type"] == "emphasis" && node["style"] == "kaeriten" && node["text"] == "レ"));
    assert!(nodes.iter().any(|node| node["type"] == "text" && node["text"] == "甲"));
    assert!(nodes.iter().any(|node| node["type"] == "text" && node["text"] == "乙"));
    assert_eq!(output.parser_ir.pointer("/source/encoding").and_then(serde_json::Value::as_str), Some("Shift_JIS"));
    assert_eq!(output.parser_ir.pointer("/warnings/0/code").and_then(serde_json::Value::as_str), Some("AAT_WARNING"));
    assert!(output.divergence_bundle["records"].as_array().unwrap().iter().any(|record| {
        record["category"] == "UNSUPPORTED" && record["message"].as_str().unwrap_or("").contains("warigaki")
    }));
    assert!(output.emitted_rule_ids.contains("U-09"), "nested heading warigaki must use the measured (emphasis.text) target rule");
    assert!(!output.divergence_bundle["records"].as_array().unwrap().iter().any(|record| {
        record["parser_ir_pointer"] == "warnings[].span.line"
    }), "warning line spans are not measured in mapping v1.1");
}
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir measured_policy_projects -- --nocapture`

Expected: FAIL because the minimal converter rejects these nodes.

- [ ] **Step 3: Add policy implementations without hard-coded rule ids**

Implementation requirements:

```rust
// Good: runtime divergence asks the mapping index for a measured rule.
let warigaki_pointer = format!("{path}.warigaki");
recorder.record("UNSUPPORTED", Some(warigaki_pointer.as_str()), None, None, None)?;

// Forbidden: code chooses U-15 or any other rule id directly.
let rule_id = "U-15";
```

Add these behaviors:

- `style`: project nested visible text into parser-IR `emphasis`, record `AMBIGUITY` for style to emphasis.
- `style_type = "kaeriten"` and `style_type = "okurigana"`: same `style -> emphasis` behavior; do not create a separate manual kunten rule table.
- `heading`: concatenate projected content into one `heading.text`, record heading level `AMBIGUITY`, record heading style `LOSS` when present.
- `warigaki`: record measured `UNSUPPORTED`, flatten `upper` then `lower` into visible child nodes when children are representable. Select the `parser_ir_pointer` by folded path: use `None` for rules such as `U-15`/`U-08`, and `(emphasis.text)` for nested emphasis/heading paths such as `U-09`/`U-11`/`U-12`.
- `jisage_block`: emit `indentation` with depth `1`, record `INVENTION`.
- `quote_block`: do not emit a divergence record in v1 unless the mapping artifact grows a measured `quote_block` rule. Under `Refuse`, either project only measured children or return an explicit unsupported-block error; do not invent `AMBIGUITY`.
- `caption_block`: do not emit a divergence record in v1 unless the mapping artifact grows a measured `caption_block` rule. Existing measured `caption` rules are for caption inline/figure pointers, not block-level caption.
- `meta.source_encoding = "windows-31j-lossy"`: output `Shift_JIS`, record `AMBIGUITY`.
- `meta.warnings[]`: output diagnostics with `severity = "warning"` and `code = "AAT_WARNING"`, record required inventions. Ignore warning line spans in v1 because mapping v1.1 has no `meta.warnings[].line` rule.

- [ ] **Step 4: Add default refusal test for unmeasured divergence**

```rust
#[test]
fn unmeasured_inline_kind_refuses_by_default() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping = MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v1.json")).unwrap();
    let aat = serde_json::json!({
        "version": 1,
        "work_id": "unknown",
        "meta": {
            "adapter": "fixture",
            "adapter_version": "fixture 0.1.0",
            "source_encoding": "utf-8",
            "source_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
            "parse_complete": true,
            "warnings": []
        },
        "blocks": [{"kind": "paragraph", "content": [{"kind": "x-local-fixture", "value": "x"}]}]
    });

    let error = ab_aat_to_parser_ir::convert(ab_aat_to_parser_ir::ConversionRequest {
        aat,
        mapping,
        schemas,
        options: ab_aat_to_parser_ir::ConversionOptions::default(),
    }).unwrap_err().to_string();

    assert!(error.contains("unmeasured") || error.contains("unsupported inline kind"));
}
```

- [ ] **Step 5: Run focused tests and verify they pass**

Run: `cargo test -p ab-aat-to-parser-ir measured_policy_projects -- --nocapture`

Run: `cargo test -p ab-aat-to-parser-ir unmeasured_inline_kind_refuses -- --nocapture`

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs crates/ab-aat-to-parser-ir/src/mapping.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat: cover measured aat parser-ir policies"
```

### Task 6: Checked-In Real Measured Fixture Gate

**Files:**
- Create: `tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json`
- Create: `tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json`
- Create: `tests/fixtures/aat-parser-ir/README.md`
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `convert`
- Produces: a stable, sandbox-pure real-input regression gate independent of `/db`.

- [ ] **Step 1: Add checked-in measured fixtures**

Choose one real AAT JSON file from the local aozora-rs measured corpus and one real AAT JSON file from the aozora2html full run, then copy/minimize them only enough to keep schema validity and the measured construct under test. Do not point tests directly at `/db`.

Fixture requirements:

- `real-aozora-rs-sample.aat.json`: contains at least text/ruby/gaiji or style evidence from the measured aozora-rs corpus.
- `real-aozora2html-sample.aat.json`: contains at least one aozora2html measured policy bucket, preferably warigaki or a `children[]` depth path.
- `README.md`: records the source run directory, source filename/work id, and the mapping version used to select it.

- [ ] **Step 2: Add the real-fixture conversion test**

```rust
#[test]
fn converts_checked_in_real_measured_aat_fixtures() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let schemas = SchemaSet::load(&repo, &abc).unwrap();
    let mapping_path = repo.join("data/aat-to-parser-ir-mapping-v1.json");

    for fixture in [
        "tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json",
        "tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json",
    ] {
        let aat = ab_aat_to_parser_ir::schema::read_json(&repo.join(fixture)).unwrap();
        let output = ab_aat_to_parser_ir::convert(ab_aat_to_parser_ir::ConversionRequest {
            aat,
            mapping: MappingDocument::from_path(&mapping_path).unwrap(),
            schemas: schemas.clone(),
            options: ab_aat_to_parser_ir::ConversionOptions::default(),
        }).unwrap_or_else(|error| panic!("{fixture} failed conversion: {error:#}"));

        let node_count = output.parser_ir["nodes"].as_array().map_or(0, |nodes| nodes.len());
        assert!(node_count > 0, "{fixture} produced no parser-IR nodes");
        assert!(
            !output.emitted_rule_ids.is_empty(),
            "{fixture} did not exercise any measured divergence rule"
        );
        ab_aat_to_parser_ir::schema::validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
        ab_aat_to_parser_ir::schema::validate_value(&schemas.bundle_schema, &output.divergence_bundle, "bundle").unwrap();
    }
}
```

- [ ] **Step 3: Run the focused test and verify it passes**

Run: `cargo test -p ab-aat-to-parser-ir converts_checked_in_real_measured -- --nocapture`

Expected: PASS. If this fails with `unmeasured divergence`, do not weaken `MappingIndex`; either fix traversal/pointer target selection or document the unsupported construct and choose a narrower measured fixture.

- [ ] **Step 4: Commit**

```bash
git add tests/fixtures/aat-parser-ir crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "test: add measured aat parser-ir fixtures"
```

### Task 7: CLI, Smoke Check, And Final Verification

**Files:**
- Create: `crates/ab-aat-to-parser-ir/src/main.rs`
- Create: `tests/aat-to-parser-ir-cli-smoke.sh`
- Modify: `flake.nix`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: `convert`
- Produces: `ab-aat-to-parser-ir convert --aat <path> --mapping <path> --parser-ir-out <path> --divergence-out <path> --abc-root <path>`

- [ ] **Step 1: Add failing CLI integration test**

```rust
#[test]
fn cli_convert_writes_parser_ir_and_divergence_bundle() {
    let repo = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(&aat, r#"{
      "version": 1,
      "work_id": "cli",
      "meta": {
        "adapter": "fixture",
        "adapter_version": "fixture 0.1.0",
        "source_encoding": "utf-8",
        "source_hash": "sha256:3333333333333333333333333333333333333333333333333333333333333333",
        "parse_complete": true,
        "warnings": []
      },
      "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "吾輩"}]}]
    }"#).unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat").arg(&aat)
        .arg("--mapping").arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--parser-ir-out").arg(&parser_ir)
        .arg("--divergence-out").arg(&divergence)
        .arg("--abc-root").arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    assert!(parser_ir.exists());
    assert!(divergence.exists());
}
```

- [ ] **Step 2: Run the CLI test and verify it fails**

Run: `cargo test -p ab-aat-to-parser-ir cli_convert_writes -- --nocapture`

Expected: FAIL because `main.rs` and the `tempfile` dev dependency are not wired.

- [ ] **Step 3: Add CLI**

```rust
// crates/ab-aat-to-parser-ir/src/main.rs
use std::path::PathBuf;

use ab_aat_to_parser_ir::{convert, ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Debug, Parser)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Convert {
        #[arg(long)]
        aat: PathBuf,
        #[arg(long)]
        mapping: PathBuf,
        #[arg(long)]
        parser_ir_out: PathBuf,
        #[arg(long)]
        divergence_out: PathBuf,
        #[arg(long)]
        abc_root: Option<PathBuf>,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args.command {
        Command::Convert { aat, mapping, parser_ir_out, divergence_out, abc_root } => {
            let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
            let abc_root = abc_root.unwrap_or_else(|| repo_root.join("../abc"));
            let aat = ab_aat_to_parser_ir::schema::read_json(&aat)?;
            let mapping = MappingDocument::from_path(&mapping)?;
            let schemas = SchemaSet::load(&repo_root, &abc_root)?;
            let output = convert(ConversionRequest {
                aat,
                mapping,
                schemas,
                options: ConversionOptions::default(),
            })?;
            std::fs::write(parser_ir_out, serde_json::to_string_pretty(&output.parser_ir)? + "\n")?;
            std::fs::write(divergence_out, serde_json::to_string_pretty(&output.divergence_bundle)? + "\n")?;
        }
    }
    Ok(())
}
```

Add dev dependency:

```toml
[dev-dependencies]
tempfile = { workspace = true }
```

- [ ] **Step 4: Add sandbox-pure smoke script**

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/../abc}"
out_dir="${TMPDIR:-/tmp}/ab-validator-aat-to-parser-ir-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir"

aat="$out_dir/input.aat.json"
cat > "$aat" <<'JSON'
{
  "version": 1,
  "work_id": "smoke",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:4444444444444444444444444444444444444444444444444444444444444444",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "吾輩" },
        { "kind": "ruby", "base": "猫", "reading": "ねこ", "direction": "right" }
      ]
    }
  ]
}
JSON

cargo_args=()
if [ -n "${CARGO_ARGS:-}" ]; then
  # shellcheck disable=SC2206
  cargo_args=(${CARGO_ARGS})
fi

"${CARGO:-cargo}" "${cargo_args[@]}" run --package ab-aat-to-parser-ir -- convert \
  --aat "$aat" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --parser-ir-out "$out_dir/parser-ir.json" \
  --divergence-out "$out_dir/divergence.json" \
  --abc-root "$abc_root"

jq -e '.schema_id == "https://w3id.org/abc/schemas/parser-ir.schema.json"' "$out_dir/parser-ir.json"
jq -e '.mapping.mapping_version == "0.1.1"' "$out_dir/divergence.json"
jq -e 'all(.records[]; .rule_id != null and .message != null and .count >= 1)' "$out_dir/divergence.json"

python3 - "$abc_root/schemas/parser-ir.schema.json" \
  "$abc_root/schemas/aat-parser-ir-divergence.schema.json" \
  "$repo_root/data/aat-parser-ir-divergence-bundle-v1.schema.json" \
  "$out_dir/parser-ir.json" \
  "$out_dir/divergence.json" <<'PY'
import json
import sys

import jsonschema

parser_schema_path, record_schema_path, bundle_schema_path, parser_ir_path, bundle_path = sys.argv[1:]
with open(parser_schema_path, encoding="utf-8") as handle:
    parser_schema = json.load(handle)
with open(record_schema_path, encoding="utf-8") as handle:
    record_schema = json.load(handle)
with open(bundle_schema_path, encoding="utf-8") as handle:
    bundle_schema = json.load(handle)
with open(parser_ir_path, encoding="utf-8") as handle:
    parser_ir = json.load(handle)
with open(bundle_path, encoding="utf-8") as handle:
    bundle = json.load(handle)

jsonschema.validate(parser_ir, parser_schema)
jsonschema.validate(bundle, bundle_schema)
for record in bundle["records"]:
    jsonschema.validate(record, record_schema)
PY
```

- [ ] **Step 5: Wire flake package, app, and check**

Add this explicit ABC schema input inside the existing `inputs` attrset. Do not let a pure derivation rely on the checkout's parent directory:

```nix
abc-src = {
  url = "path:../abc";
  flake = false;
};
```

Add `abc-src` to the existing `outputs` argument set:

```nix
outputs =
  {
    self,
    nixpkgs,
    abc-src,
  }:
```

Add a package and smoke check in the existing `let` block. The check must use vendored Cargo dependencies, `TMPDIR`, and `AB_ABC_ROOT=${abc-src}`:

Also add `AB_ABC_ROOT = "${abc-src}";` to the existing `abValidator` and `workspaceCheck` derivations so `nix flake check` can run the new Rust tests without reading a sibling checkout outside the store.

```nix
abAatToParserIr =
  if hasCargoManifest && hasCargoLock then
    rustPlatform.buildRustPackage {
      pname = "ab-aat-to-parser-ir";
      version = "0.1.0";

      src = source;
      cargoDeps = abCargoDeps;
      cargoBuildFlags = [
        "--package"
        "ab-aat-to-parser-ir"
      ];
      doCheck = false;
    }
  else
    pkgs.writeShellApplication {
      name = "ab-aat-to-parser-ir";
      text = "echo 'Rust workspace not scaffolded' >&2; exit 1";
    };

abAatToParserIrCheck =
  pkgs.runCommand "ab-aat-to-parser-ir-smoke-check"
    {
      nativeBuildInputs = [
        rustToolchain
        pkgs.pkg-config
        pkgs.jq
        pythonWithAatSchemaDeps
        pkgs.zstd
      ];
      buildInputs = [
        pkgs.pdfium-binaries
      ]
      ++ lib.optionals pkgs.stdenv.isDarwin [
        pkgs.libiconv
        pkgs.darwin.apple_sdk.frameworks.Security
        pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
      ];
    }
    ''
      work_dir="$(mktemp -d)"
      cp -R "${source}" "$work_dir/source"
      chmod -R +w "$work_dir/source"
      cd "$work_dir/source"

      export TMPDIR="$work_dir/tmp"
      mkdir -p "$TMPDIR"
      export CARGO_HOME="$TMPDIR/cargo-home"
      export CARGO_NET_OFFLINE=true
      export AB_ABC_ROOT="${abc-src}"
      export AB_AOZORA_RS_GAIJI_MENKUTEN_PATH="${aozoraRsGaijiMenkuten}"
      export AB_AOZORA_RS_GAIJI_CHUKI_PDF="${aozoraRsGaijiChukiPdf}"
      export AB_AOZORA_RS_GAIJI_PDFIUM_DIR="${pkgs.pdfium-binaries}/lib"
      export CARGO_ARGS="--offline --config source.crates-io.replace-with=vendored-sources --config source.vendored-sources.directory=${abCargoDeps}"

      bash tests/aat-to-parser-ir-cli-smoke.sh
      touch "$out"
    '';
```

Add package/app/check entries:


```nix
packages = {
  ab-aat-to-parser-ir = abAatToParserIr;
};

apps.ab-aat-to-parser-ir = flake-utils.lib.mkApp {
  drv = abAatToParserIr;
};

checks = {
  aat-to-parser-ir-smoke = abAatToParserIrCheck;
};
```

- [ ] **Step 6: Run verification**

Run:

```bash
cargo fmt --manifest-path Cargo.toml --all
cargo test -p ab-aat-to-parser-ir -- --nocapture
bash tests/aat-to-parser-ir-cli-smoke.sh
nix build .#checks.x86_64-linux.aat-to-parser-ir-smoke --print-build-logs
nix flake check --print-build-logs
```

Expected: all commands exit 0. The final command must be full `nix flake check`, not `--no-build`.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aat-to-parser-ir tests/aat-to-parser-ir-cli-smoke.sh Cargo.toml flake.nix
git commit -m "feat: add aat to parser-ir cli"
```

## Self-Review Notes

- Route: this remains an upstream protocol/trust-boundary plan until Tasks 2, 5, and 6 prove folding conformance, `children[]` traversal, and real measured fixtures. Do not treat `convert(request)` as an accepted deep seam before those gates pass.
- Spec coverage: the plan covers schema hash preflight, ABC-owned divergence records, bundle schema, measured warigaki `UNSUPPORTED`, stale gaiji pointer rejection, span synthesis ambiguity, direct `ruby.direction`, depth-faithful traversal, and sandbox-pure smoke verification.
- Manual mapping risk: emitted production divergence rule ids come from `MappingIndex`; implementation code may name categories and pointers but must not name rule ids. Applicability omissions are not solved by the guard, so fixtures assert specific measured rules such as A-18 and U-09.
- Integration scope: checked-in smoke validates text/ruby flow, and checked-in real measured fixtures validate at least one aozora-rs and one aozora2html input without `/db`. Full-corpus conversion and ABC manifest publication remain downstream work after the CLI exists.
- Incubation note: parser-IR `derived_from` remains intentionally unset until the generated mapping artifact says provenance is projected rather than lost.
