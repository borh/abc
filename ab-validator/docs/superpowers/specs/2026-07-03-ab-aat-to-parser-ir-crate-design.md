# ab-aat-to-parser-ir Crate Design

Date: 2026-07-03
Status: implemented with identity compatibility hardening

## Problem

`ab-validator` now owns a generated AAT-to-parser-IR mapping artifact at
`data/aat-to-parser-ir-mapping-v1.json`. The post-measurement gate records
`CLI_READY_WITH_LOWER_BOUND_CAVEAT`: aozora-rs has zero measured
`UNSUPPORTED` files, and the current aozora2html run has no clean
adapter-obligation gaps for warigaki/kunten.

The next crate, `ab-aat-to-parser-ir`, should turn that evidence into an
operator-facing conversion tool without reopening the adapter-boundary
decision. Its job is not to invent a new mapping policy. Its job is to apply a
measured policy, emit ABC parser-IR, and preserve non-representable constructs
in a deterministic divergence sidecar.

The prior version of this spec was too optimistic. Review found three blocking
protocol issues:

- the then-current mapping artifact had no `UNSUPPORTED` rule, so default warigaki
  drop-sidecar behavior is not a measured production rule;
- the ABC divergence record schema already exists and is incompatible with the
  proposed local sidecar entry shape;
- span synthesis and several mapping pointers were not represented honestly in
  the then-current mapping artifact.

Those protocol issues were resolved by the `0.1.1` mapping protocol correction
artifact. The `0.2.0` mapping added measured producer-identity projection into
parser-IR `derived_from`; the current `0.2.1` mapping extends that measured
surface with aozora-epub3 `tcy`, `keigakomi_block`, and `yokogumi_block`
evidence while keeping ABC as the compatibility registry owner.

## Review Corrections

Protocol correction status: `data/aat-to-parser-ir-mapping-v1.json` now carries
`mapping_version = 0.2.1`, includes measured aozora2html `UNSUPPORTED`
warigaki evidence, includes measured aozora-epub3 `tcy` and block-container
evidence, projects `version`, `meta.adapter`, and
`meta.adapter_version` into parser-IR `derived_from`, uses
`abc-legacy-json-c14n-v0`, and validates against the local AAT pointer
contract.

Resolved blockers:

- B1: the corrected mapping includes measured `UNSUPPORTED` rules from the
  current aozora2html corpus, including warigaki. A production conversion may
  emit measured unsupported records. Unmeasured unsupported buckets still use
  `UnmeasuredDivergencePolicy::Refuse` by default.
- B3: `../abc/schemas/aat-parser-ir-divergence.schema.json` already exists and
  owns the per-record divergence shape. A local same-named schema would create a
  protocol collision.
- S1/S2: missing AAT spans are common, parser-IR requires spans, and the
  corrected mapping records missing span synthesis as `AMBIGUITY`, not
  `INVENTION`. The Python generator advances synthesized offsets by projected
  UTF-8 byte length and records the approximation.
- S3: invalid gaiji pointers are corrected: `gaiji.raw_marker` is sourced from
  `gaiji.description`, and `gaiji.unicode` is recorded as a derived absence
  with no AAT pointer.
- S4: `meta.metrics` and `meta.semantic_summary` are mapped as `LOSS` but the
  proposed sidecar did not preserve them structurally.

Partial correction:

- B2: the shipped mapping hashes do match the current generator's canonical
  form: JSON parsed, sorted by key, compact separators, then `/` escaped as
  `\/` before SHA-256. Raw bytes, ordinary sorted compact JSON, and indented
  JSON do not match. The defect was that this algorithm was implicit. This spec
  now names it as `abc-legacy-json-c14n-v0`.

## Classification

Type badge: Protocol Design first, Deepen second.

Evidence:

- Observed: AAT JSON is the normative adapter contract
  (`data/aat-schema.json`, `docs/aat-contract.md`).
- Observed: ABC parser-IR is a separate JSON schema at
  `../abc/schemas/parser-ir.schema.json`, with a flat `nodes[]` model and
  required spans on every node.
- Observed: ABC owns a per-entry divergence record schema at
  `../abc/schemas/aat-parser-ir-divergence.schema.json`.
- Observed: The corrected generated mapping has 130 measured folded rule
  buckets across aozora-rs, current aozora2html evidence, and current
  aozora-epub3 evidence. The aozora-rs-only gate still has zero `UNSUPPORTED`
  files.
- Observed: aozora2html can emit warigaki, and parser-IR has no warigaki node.
- Observed: production AAT spans are mostly absent (`docs/aat-span-audit.md`).
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
- Protocol seam risk: sidecar ownership, span synthesis, and unmeasured
  unsupported constructs are protocol decisions, not implementation details.
- Trust seam risk: mapping and target schema hashes must be computed by a named
  canonicalization algorithm before any conversion output is trusted.
- Behavior-preservation risk: the disposable Python probe is evidence only.
  Rust implementation must be tested at the crate interface, not by line-by-line
  port assertions.

Deepening acceptance verdict:

The interface shape `convert(request) -> ConversionOutput` is still a good deep
module seam. The protocol gates below are now explicit enough to start the Rust
crate implementation against the corrected mapping artifact.

## Non-Goals

- Do not make `ab-ir` the adapter or parser-IR contract.
- Do not hand-copy the old 27-rule synthesized table.
- Do not create a compatibility registry in ab-validator; ABC owns registry
  admission and exact adapter-version matching.
- Do not add AAT v2 vocabulary.
- Do not repair aozora2html timeout/protocol/parse-incomplete buckets in this
  crate.
- Do not treat exploratory unsupported handling as production mapping policy.

## Required Protocol Gates

These gates must be completed before the implementation plan for
`crates/ab-aat-to-parser-ir`.

### Gate 1: Pin Schema Hash Canonicalization

The current mapping artifact uses this algorithm:

1. parse JSON;
2. serialize with UTF-8, sorted object keys, compact separators `,` and `:`;
3. escape every `/` as `\/`;
4. SHA-256 the resulting bytes and prefix with `sha256:`.

Call this `abc-legacy-json-c14n-v0`.

Implementation requirements:

- document `abc-legacy-json-c14n-v0` in the mapping generator report;
- add a checked command or smoke test that prints the hashes for
  `../abc/schemas/aat-parser-ir-mapping.schema.json` and
  `../abc/schemas/parser-ir.schema.json`;
- keep using the current hashes only if that command reproduces
  `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
  and
  `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`.

Do not implement raw-byte hashing for this mapping artifact unless the mapping
is regenerated with raw-byte hashes.

### Gate 2: Resolve Divergence Sidecar Ownership

ABC owns the per-entry record schema:
`../abc/schemas/aat-parser-ir-divergence.schema.json`.

That schema requires:

- `rule_id`
- `category`
- `message`
- `count`
- `first_path`

and allows only:

- `aat_pointer`
- `parser_ir_pointer`
- `source_value`
- `target_value`

The ab-validator sidecar must therefore be a bundle schema with a different
name, for example `data/aat-parser-ir-divergence-bundle-v1.schema.json`.

Bundle rules:

- `records[]` items must validate against ABC's per-entry divergence record
  schema without extra properties.
- Any per-work metadata lives outside `records[]`.
- `action`, `span`, `node_kind`, and `value_preview` are not per-record fields
  unless ABC extends the record schema.
- `count` is per work, per rule bucket.
- `first_path` is the first occurrence path in the input AAT document.
- The bundle may contain a separate `preserved_aat_meta` object for structured
  AAT metadata that parser-IR cannot carry, including `metrics` and
  `semantic_summary`.

Proposed bundle shape:

```json
{
  "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
  "schema_version": "0.1.0",
  "work_id": "000000_00000",
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.2.1",
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
  "preserved_aat_meta": {
    "metrics": null,
    "semantic_summary": null
  },
  "summary": {
    "LOSS": 0,
    "INVENTION": 0,
    "AMBIGUITY": 0,
    "UNSUPPORTED": 0,
    "STRUCTURAL": 0
  },
  "records": [
    {
      "rule_id": "A-06",
      "category": "AMBIGUITY",
      "aat_pointer": "meta.source_hash",
      "parser_ir_pointer": "source.work_content_hash",
      "source_value": "sha256:...",
      "target_value": "sha256:...",
      "message": "AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ",
      "count": 1,
      "first_path": "meta.source_hash"
    }
  ]
}
```

### Gate 3: Mapping v1.1 Production Warigaki Policy

Resolved: the corrected mapping v1.1 includes measured `UNSUPPORTED` rules from
current aozora2html evidence, including warigaki.

Production behavior for the corrected mapping:

- `UnmeasuredDivergencePolicy::Refuse` is the default.
- AAT documents containing measured warigaki buckets may emit `UNSUPPORTED`
  divergence records using mapping artifact rule ids.
- AAT documents containing unmeasured unsupported buckets must refuse unless
  explicitly run under `UnmeasuredDivergencePolicy::RecordExploratory`.

### Gate 4: Fix Mapping Pointers

Resolved: strict mapping preflight is enabled in the generator. Non-null
`aat_pointer` values must resolve to fields in `data/aat-schema.json`; derived
absence/policy rules use null.

Resolved defects:

- `blocks[].content[].gaiji.raw_marker` now points at
  `blocks[].content[].gaiji.description`.
- `blocks[].content[].gaiji.unicode` is represented as a derived absence with
  no AAT pointer.

Implementation preflight:

- resolve every non-null `aat_pointer` against `data/aat-schema.json` using the
  folded pointer syntax;
- reject pointers that cannot be resolved.

### Gate 5: Span Synthesis Mapping Policy

Parser-IR requires `span.start` and `span.end` on every node. Production AAT
spans are mostly absent.

Resolved: the corrected mapping v1.1 records missing-span synthesis as measured
`AMBIGUITY` buckets. The generator aggregates count and first path by folded
rule so sidecars do not need one schema-level rule per node.

V1 span value policy:

- if AAT span exists: `start = byte_start`, `end = byte_end`,
  `line = line_start`, `column = null`,
  `coordinate_system = decoded_utf8`;
- if AAT span is absent: synthesize a decoded-UTF8 fallback span from the latest
  known offset to `offset + projected_visible_text_utf8_len`, and
  record/aggregate the `AMBIGUITY` span rule;
- dropping observed `line_end` is recorded as an explicit `LOSS` rule.

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

- validate mapping schema hash and target parser-IR schema hash up front using
  `abc-legacy-json-c14n-v0`;
- emit only divergence categories/rule buckets represented by the mapping in
  production mode;
- fail when implementation emits an unmeasured bucket unless explicitly run in
  exploratory mode;
- make parser-IR and divergence bundle the only output contract.

Accept only after the protocol gates above are complete. This is the deepest
module shape: one small conversion interface hides nested AAT traversal, flat
parser-IR construction, schema identity, and sidecar policy.

## Module Interface

The crate should expose one library seam and a thin CLI wrapper.

```rust
pub struct ConversionRequest {
    pub aat: serde_json::Value,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
    pub options: ConversionOptions,
}

pub struct SchemaSet {
    pub aat_schema: serde_json::Value,
    pub mapping_schema: serde_json::Value,
    pub parser_ir_schema: serde_json::Value,
    pub abc_divergence_record_schema: serde_json::Value,
    pub bundle_schema: serde_json::Value,
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
}

pub fn convert(request: ConversionRequest) -> anyhow::Result<ConversionOutput>;
```

This is the test surface. Internal helper modules may exist, but callers should
not need to know traversal, text projection, loss taxonomy, span conversion,
hash canonicalization, or schema validation details.

## CLI Shape

V1 should start with a single-document command:

```bash
ab-aat-to-parser-ir convert \
  --aat path/to/input.aat.json \
  --mapping data/aat-to-parser-ir-mapping-v1.1.json \
  --parser-ir-out path/to/parser-ir.json \
  --divergence-out path/to/divergence-bundle.json \
  --abc-root ../abc
```

Defaults:

- validate AAT input against `data/aat-schema.json`;
- validate mapping against `../abc/schemas/aat-parser-ir-mapping.schema.json`;
- validate parser-IR output against `../abc/schemas/parser-ir.schema.json`;
- validate divergence records against ABC's divergence record schema;
- validate the bundle against the local bundle schema;
- refuse unmeasured divergence buckets;
- write deterministic pretty JSON.

Batch conversion can be a later thin wrapper over the same library seam. Do not
make batch orchestration part of the core interface.

## Parser-IR Output Policy

The output must validate against the ABC parser-IR schema named by the mapping
artifact.

Top-level fields:

- `schema_id`: `https://w3id.org/abc/schemas/parser-ir.schema.json`
- `schema_hash`: mapping `target_parser_ir_schema_hash`
- `source.work_content_hash`: AAT `meta.source_hash`, with an `A-06`
  `AMBIGUITY` divergence record
- `source.encoding`: `utf-8`/`utf-8-bom` -> `UTF-8`,
  `windows-31j`/`windows-31j-lossy` -> `Shift_JIS`; lossy Shift_JIS emits an
  `A-05` `AMBIGUITY` divergence record
- `source.normalization`: invented as `source`
- `source.source_path`: `null` in v1
- `warnings`: AAT warnings projected to parser-IR diagnostics with invented
  `severity = warning` and `code = AAT_WARNING`
- `errors`: empty array

Important crux:

Mapping version `0.2.1` preserves the producer-identity behavior introduced in
`0.2.0`: `version` -> `aat_version`, `meta.adapter` -> `aat_adapter`, and
`meta.adapter_version` -> `aat_adapter_version`. The mapping document also
contributes `mapping_id`, `mapping_version`, and `mapping_schema_hash`.
`mapping_hash` is deliberately not serialized inside parser-IR; ABC combines
`derived_from` with manifest input `mapping_hash` and admits exact
adapter-version tuples in `data/aat-parser-ir-compatibility.edn`.

## Node Projection Policy

This section is illustrative, not a substitute for the mapping artifact. The
mapping rules and sidecar records are authoritative after the protocol gates
are complete.

Measured current rules include:

- AAT `text` -> parser-IR `text`.
- AAT `ruby` -> parser-IR `ruby`, including direct `direction` projection.
- AAT `ruby.scope` -> invented `explicit`.
- AAT `gaiji.resolved` string/null -> parser-IR `gaiji.resolved` boolean, with
  an `AMBIGUITY` divergence record.
- AAT `gaiji.description` -> parser-IR `gaiji.raw_marker`, with an
  `INVENTION` count because the original source marker is not present.
- AAT `style` -> parser-IR `emphasis`, with an `AMBIGUITY` divergence record.
- AAT heading level -> parser-IR heading level, with an `A-04` domain
  `AMBIGUITY` divergence record.
- AAT paragraph/heading block containers -> no parser-IR container node; emit
  contained nodes in document order and record `STRUCTURAL`.

Rules corrected by v1.1 before production:

- AAT `warigaki` -> parser-IR has no warigaki node. Under measured v1.1
  `UNSUPPORTED` rules it may `drop-sidecar` and preserve child visible text
  where possible.
- AAT spans absent -> parser-IR requires spans. v1.1 records synthesized
  decoded-UTF8 fallback spans as `AMBIGUITY`.
- AAT `gaiji.unicode` loss -> v1.1 expresses this as derived absence/policy,
  not an AAT field path.

Kunten policy:

- Kunten expressed as `style_type = kaeriten` or `style_type = okurigana`
  follows the same `style -> emphasis` rule and records `AMBIGUITY`.
- Any `x-*` extension properties on projected nodes are not parser-IR fields.
  If present, they need a measured mapping rule or production conversion must
  refuse.

## Mapping Guard

The conversion engine should not trust the mapping file merely because it
parses.

Preflight checks:

- mapping validates against ABC `aat-parser-ir-mapping.schema.json`;
- mapping `mapping_schema_hash` equals the `abc-legacy-json-c14n-v0` hash of
  that schema;
- mapping `target_parser_ir_schema_hash` equals the
  `abc-legacy-json-c14n-v0` hash of ABC `parser-ir.schema.json`;
- mapping `source_aat_version == 1`;
- loss taxonomy includes all five categories and their expected default
  sidecar behavior;
- every category present in `transform_rule_descriptions` has a taxonomy entry;
- every non-null `aat_pointer` resolves to `data/aat-schema.json` after the
  v1.1 pointer correction;
- no conversion output is written if any preflight check fails.

Runtime checks:

- every production divergence record must match one folded mapping rule;
- every parser-IR document must validate against ABC parser-IR schema;
- every divergence record must validate against ABC's divergence record schema;
- every bundle must validate against the local bundle schema;
- unmeasured unsupported constructs refuse by default.

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
- `divergence.rs`: ABC record aggregation and local bundle construction.
- `schema.rs`: JSON schema loading, `abc-legacy-json-c14n-v0`, validation.
- `tests/integration.rs`: interface-level tests only.

## Required Tests After Protocol Gates

Interface-level tests:

- text/ruby/gaiji fixture produces schema-valid parser-IR.
- `ruby.direction` projects directly and does not produce a divergence record.
- `style` maps to `emphasis` and records `AMBIGUITY`.
- `windows-31j-lossy` maps to `Shift_JIS` and records `AMBIGUITY`.
- `source.work_content_hash` projection records `A-06` `AMBIGUITY`.
- heading level projection records `A-04` `AMBIGUITY`.
- current mapping v1.1 emits measured warigaki `UNSUPPORTED`: warigaki
  structure is absent from parser-IR, child visible text is preserved when
  possible, and the bundle has a valid ABC divergence record.
- if exploratory mode is kept, warigaki exploratory output uses a synthesized
  `U-00` record and is not accepted as production evidence.
- kunten style emits parser-IR `emphasis` plus sidecar `AMBIGUITY`.
- a mapping file with the wrong target parser-IR hash refuses before writing
  outputs.
- an unmeasured divergence bucket refuses by default.
- all divergence `records[]` validate against ABC's per-entry schema.
- bundle JSON validates against the local bundle schema.

Smoke tests:

- `tests/aat-to-parser-ir-cli-smoke.sh` converts small checked-in fixtures only
  after the mapping/span/sidecar gates are resolved.
- The smoke test validates parser-IR against ABC schema, each divergence record
  against ABC schema, and the bundle against the local bundle schema.
- It asserts no old 27-rule table is referenced.

## Current Implementation

`crates/ab-aat-to-parser-ir` implements the corrected mapping artifact as a
deep module boundary: callers provide AAT JSON and a mapping document; the crate
returns schema-valid parser-IR plus a divergence bundle. The implementation
refuses unmeasured divergence buckets by default, validates parser-IR and
divergence records against the ABC schemas, and exposes a corpus audit that
publishes per-adapter compatibility candidates.

## Incubation Notes

Adapter-version matching is exact. Any new `meta.adapter_version` tuple requires
a fresh measured conversion-audit entry in ABC's compatibility registry; do not
add wildcard, prefix, or directory-label matching.
