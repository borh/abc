# ab-aat-to-parser-ir Crate Design

Date: 2026-07-03
Status: proposed

## Problem

`ab-validator` now owns a generated AAT-to-parser-IR mapping artifact at
`data/aat-to-parser-ir-mapping-v1.json`. The post-measurement gate records
`CLI_READY_WITH_LOWER_BOUND_CAVEAT`: aozora-rs has zero measured
`UNSUPPORTED` files, and the current aozora2html run has no clean
adapter-obligation gaps for warigaki/kunten.

The next crate, `ab-aat-to-parser-ir`, should turn that evidence into an
operator-facing conversion tool without reopening the adapter-boundary
decision. Its job is not to invent a new mapping policy. Its job is to apply
the measured policy, emit ABC parser-IR, and preserve every non-representable
construct in a deterministic divergence sidecar.

## Classification

Type badge: Protocol Design + Deepen.

Evidence:

- Observed: AAT JSON is the normative adapter contract
  (`data/aat-schema.json`, `docs/aat-contract.md`).
- Observed: ABC parser-IR is a separate JSON schema at
  `../abc/schemas/parser-ir.schema.json`, with a flat `nodes[]` model.
- Observed: The generated mapping has 25 measured folded rule buckets and no
  `UNSUPPORTED` bucket for the aozora-rs corpus.
- Observed: aozora2html can emit warigaki, and parser-IR has no warigaki node.
- Inferred: A generic interpreter for `transform_rule_descriptions` would be a
  fake seam; those descriptions are evidence, not executable rules.
- Assumed: v1 consumers need schema-valid parser-IR plus machine-readable
  divergence evidence more than they need a fully generic mapping DSL.
- Disconfirming evidence: if ABC changes the mapping schema to include
  executable transform expressions, the recommendation should be revisited.

Hickey hazard check:

- Braided concern risk: traversal, target projection, schema identity, and
  divergence policy must not be spread across callers.
- State/time/identity risk: output must be deterministic; no wall-clock fields
  in parser-IR or sidecar.
- Protocol seam risk: sidecar shape is a new contract and needs its own local
  schema/test gate.
- Trust seam risk: mapping and target schema hashes must be checked before any
  conversion output is trusted.
- Behavior-preservation risk: the disposable Python probe is evidence only.
  Rust implementation must be tested at the crate interface, not by line-by-line
  port assertions.

## Non-Goals

- Do not make `ab-ir` the adapter or parser-IR contract.
- Do not hand-copy the old 27-rule synthesized table.
- Do not start manifest identity or ABC compatibility-registry hardening here.
- Do not add AAT v2 vocabulary.
- Do not repair aozora2html timeout/protocol/parse-incomplete buckets in this
  crate.

## Design Alternatives

### Alternative A: Direct Rust Port of the Python Probe

This is fastest, but shallow. It copies behavior into Rust without giving
callers a stable interface or guarding drift between the code and
`data/aat-to-parser-ir-mapping-v1.json`.

Reject as the main design. Useful only as implementation prior art.

### Alternative B: Generic Mapping Interpreter

This treats `transform_rule_descriptions` as if it were executable. It would
need a DSL for traversal, construction, loss policy, span handling, and
sidecar behavior. The current ABC schema does not define such a DSL.

Reject for v1. It is more design surface than measured evidence supports.

### Alternative C: Measured Policy Engine With Mapping Guard

Implement the AAT traversal and parser-IR construction as Rust code, but guard
it with the generated mapping artifact:

- validate mapping schema hash and target parser-IR schema hash up front,
- emit only divergence categories/rule buckets represented by the mapping,
- fail when implementation emits an unmeasured bucket unless explicitly run in
  an exploratory mode,
- make parser-IR and divergence sidecar the only output contract.

Accept. This is the deepest module: one small conversion interface hides nested
AAT traversal, flat parser-IR construction, schema identity, and sidecar policy.

## Module Interface

The crate should expose one library seam and a thin CLI wrapper.

```rust
pub struct ConversionRequest {
    pub aat: serde_json::Value,
    pub mapping: MappingDocument,
    pub target: TargetSchemaIdentity,
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
    pub divergence: DivergenceSidecar,
}

pub fn convert(request: ConversionRequest) -> anyhow::Result<ConversionOutput>;
```

This is the test surface. Internal helper modules may exist, but callers should
not need to know traversal, text projection, loss taxonomy, span conversion, or
schema validation details.

## CLI Shape

V1 should start with a single-document command:

```bash
ab-aat-to-parser-ir convert \
  --aat path/to/input.aat.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out path/to/parser-ir.json \
  --divergence-out path/to/divergence.json \
  --abc-root ../abc
```

Defaults:

- validate AAT input against `data/aat-schema.json`,
- validate mapping against `../abc/schemas/aat-parser-ir-mapping.schema.json`,
- validate parser-IR output against `../abc/schemas/parser-ir.schema.json`,
- refuse unmeasured divergence buckets,
- write deterministic pretty JSON.

Batch conversion can be a later thin wrapper over the same library seam. Do not
make batch orchestration part of the core interface.

## Parser-IR Output Policy

The output must validate against the ABC parser-IR schema named by the mapping
artifact.

Top-level fields:

- `schema_id`: `https://w3id.org/abc/schemas/parser-ir.schema.json`
- `schema_hash`: mapping `target_parser_ir_schema_hash`
- `source.work_content_hash`: AAT `meta.source_hash`
- `source.encoding`: `utf-8`/`utf-8-bom` -> `UTF-8`,
  `windows-31j`/`windows-31j-lossy` -> `Shift_JIS`
- `source.normalization`: invented as `source`
- `source.source_path`: `null` in v1
- `warnings`: AAT warnings projected to parser-IR diagnostics with invented
  `severity = warning` and `code = AAT_WARNING`
- `errors`: empty array

Important crux:

The current generated mapping records `meta.adapter` and
`meta.adapter_version` as `LOSS`, even though the current ABC parser-IR schema
has optional `derived_from`. To avoid silently changing the measured mapping,
v1 should keep the parser-IR document aligned with
`data/aat-to-parser-ir-mapping-v1.json` and put adapter/mapping provenance in
the divergence sidecar. If ABC wants `derived_from` populated, regenerate the
mapping artifact first and make that a measured policy change.

## Node Projection Policy

The Rust conversion policy should match the generated measured rules:

- AAT `text` -> parser-IR `text`.
- AAT `ruby` -> parser-IR `ruby`, including direct `direction` projection.
- AAT `ruby.scope` -> invented `explicit`.
- AAT `gaiji.resolved` string/null -> parser-IR `gaiji.resolved` boolean, with
  an `AMBIGUITY` sidecar entry.
- AAT `gaiji.description` -> parser-IR `gaiji.raw_marker`, with an `INVENTION`
  sidecar entry because the original source marker is not present.
- AAT `style` -> parser-IR `emphasis`, with an `AMBIGUITY` sidecar entry.
- AAT headings -> parser-IR `heading`, with non-text inline structure flattened
  and recorded.
- AAT paragraph/block containers -> no parser-IR container node; emit contained
  nodes in document order and record `STRUCTURAL`.
- AAT `jisage_block` -> parser-IR `indentation` with invented default depth.
- AAT `quote_block` -> parser-IR `quote` with ambiguous marker details.
- AAT `figure` -> parser-IR `image`, recording filename/path and caption/size
  losses.
- AAT `accent` -> parser-IR `emphasis`, recording accent semantics as
  ambiguous/lossy.
- AAT `raw` -> dropped with `UNSUPPORTED`.
- AAT `warigaki` -> drop the warigaki node structure with `UNSUPPORTED`, but
  preserve child visible text when possible by projecting upper/lower children.

Kunten policy:

- Kunten expressed as `style_type = kaeriten` or `style_type = okurigana`
  follows the same `style -> emphasis` rule and records `AMBIGUITY`.
- Any `x-*` extension properties on projected nodes are not parser-IR fields.
  If present, they should be recorded in the sidecar under the same divergence
  entry rather than silently ignored.

## Span Policy

AAT spans are decoded-UTF8 byte offsets named `byte_start`/`byte_end` with
`line_start`/`line_end`. Parser-IR spans are also decoded-UTF8 byte offsets
named `start`/`end`, with optional `line`, `column`, and `coordinate_system`.

V1 mapping:

- `start = byte_start`
- `end = byte_end`
- `line = line_start`
- `column = null`
- `coordinate_system = decoded_utf8`

Missing AAT spans should use zero-width spans at the latest known offset only
when required to keep parser-IR schema-valid, and must record an `INVENTION` or
`AMBIGUITY` sidecar entry.

## Divergence Sidecar Contract

The sidecar is a local ab-validator contract in v1. It should get a schema such
as `data/aat-parser-ir-divergence.schema.json` during implementation.

Proposed shape:

```json
{
  "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-v1.json",
  "schema_version": "0.1.0",
  "work_id": "000000_00000",
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.1.0",
    "mapping_schema_hash": "sha256:..."
  },
  "target": {
    "parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
    "parser_ir_schema_hash": "sha256:..."
  },
  "aat": {
    "version": 1,
    "adapter": "aozora2html",
    "adapter_version": "aozora2html-adapter 0.1.0",
    "source_hash": "sha256:...",
    "parse_complete": true
  },
  "summary": {
    "LOSS": 0,
    "INVENTION": 0,
    "AMBIGUITY": 0,
    "UNSUPPORTED": 0,
    "STRUCTURAL": 0
  },
  "entries": [
    {
      "rule_id": "U-01",
      "category": "UNSUPPORTED",
      "action": "drop-sidecar",
      "aat_pointer": "blocks[3].content[1].warigaki",
      "parser_ir_pointer": null,
      "message": "parser-IR has no warigaki node; upper/lower children projected as visible text",
      "span": { "start": 42, "end": 64, "line": 3, "column": null, "coordinate_system": "decoded_utf8" },
      "node_kind": "warigaki",
      "value_preview": "..."
    }
  ]
}
```

Rules:

- Every `LOSS`, `AMBIGUITY`, `UNSUPPORTED`, and `STRUCTURAL` entry records a
  sidecar entry because the mapping taxonomy says `records_sidecar = true`.
- `INVENTION` entries are omitted from `entries` by default but counted in
  `summary`, because the mapping taxonomy says `records_sidecar = false`.
- The implementation should keep enough internal detail to enable an
  `--include-inventions` debug flag later, but not expose that flag in v1 unless
  needed by tests.
- `rule_id` must come from the folded mapping bucket whenever available.
- If no rule matches, default behavior is `Refuse`.

## Mapping Guard

The conversion engine should not trust the mapping file merely because it
parses.

Preflight checks:

- mapping validates against ABC `aat-parser-ir-mapping.schema.json`,
- mapping `mapping_schema_hash` equals the computed canonical hash of that
  schema,
- mapping `target_parser_ir_schema_hash` equals the computed canonical hash of
  ABC `parser-ir.schema.json`,
- mapping `source_aat_version == 1`,
- loss taxonomy includes all five categories and their expected default
  sidecar behavior,
- no conversion output is written if any preflight check fails.

Runtime checks:

- every emitted divergence bucket must match one folded mapping rule,
- every emitted parser-IR document must validate against ABC parser-IR schema,
- critical unsupported constructs default to `drop-sidecar` rather than
  `drop-silent`.

## Proposed File Layout

```text
crates/ab-aat-to-parser-ir/
  Cargo.toml
  src/lib.rs
  src/main.rs
  src/mapping.rs
  src/convert.rs
  src/divergence.rs
  src/schema.rs
  tests/integration.rs
```

Responsibilities:

- `lib.rs`: exports the small conversion interface.
- `main.rs`: parses CLI args, loads files, calls `convert`, writes outputs.
- `mapping.rs`: typed mapping document, rule lookup, taxonomy validation.
- `convert.rs`: AAT traversal and parser-IR construction.
- `divergence.rs`: sidecar model, folded bucket lookup, summaries.
- `schema.rs`: JSON schema loading, canonical hash calculation, validation.
- `tests/integration.rs`: interface-level tests only.

## Required Tests

Interface-level tests:

- text/ruby/gaiji fixture produces schema-valid parser-IR.
- `ruby.direction` projects directly and does not produce a divergence entry.
- `style` maps to `emphasis` and records `AMBIGUITY`.
- `windows-31j-lossy` maps to `Shift_JIS` and records `AMBIGUITY`.
- warigaki structure is absent from parser-IR, child visible text is preserved
  when possible, and the sidecar has an `UNSUPPORTED` entry.
- kunten style emits parser-IR `emphasis` plus sidecar `AMBIGUITY`.
- A mapping file with the wrong target parser-IR hash refuses before writing
  outputs.
- An unmeasured divergence bucket refuses by default.
- Sidecar JSON validates against local sidecar schema.

Smoke tests:

- `tests/aat-to-parser-ir-cli-smoke.sh` converts small checked-in fixtures.
- The smoke test validates parser-IR against ABC schema and sidecar against the
  local sidecar schema.
- It asserts no old 27-rule table is referenced.

## Next Implementation Plan Scope

The implementation plan should build this in small slices:

1. Crate scaffold and mapping/schema preflight.
2. Sidecar schema and sidecar model.
3. Minimal text/ruby/gaiji conversion.
4. Style, heading, block flattening, warnings, and source encoding.
5. Warigaki/kunten critical sidecar behavior.
6. CLI smoke and final verification.

Do not bundle batch conversion, manifest identity, compatibility registry, or
aozora2html residual runtime triage into that implementation plan.

## Incubation Notes

The one decision worth sleeping on before implementation is `derived_from`.
Parser-IR has a field that can carry adapter and mapping provenance, but the
current measured mapping says those AAT metadata fields are lost. The
conservative v1 choice is to keep parser-IR aligned with the generated mapping
and put provenance in the sidecar. A future measured mapping revision can move
some of that provenance into `derived_from`.
