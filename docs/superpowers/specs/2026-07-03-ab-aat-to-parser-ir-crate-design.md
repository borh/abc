# ab-aat-to-parser-ir Crate Design

Date: 2026-07-03
Status: blocked pending protocol corrections

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

- the current mapping artifact has no `UNSUPPORTED` rule, so default warigaki
  drop-sidecar behavior is not a measured production rule;
- the ABC divergence record schema already exists and is incompatible with the
  proposed local sidecar entry shape;
- span synthesis and several mapping pointers are not represented honestly in
  the current mapping artifact.

Until those protocol issues are resolved, this crate is a deepening candidate,
not an implementation-ready module.

## Review Corrections

Accepted blockers:

- B1: current mapping has no `UNSUPPORTED` rule. A production conversion using
  `UnmeasuredDivergencePolicy::Refuse` must refuse warigaki under the current
  mapping. A test that expects warigaki to emit `U-01` cannot run against
  `data/aat-to-parser-ir-mapping-v1.json`.
- B3: `../abc/schemas/aat-parser-ir-divergence.schema.json` already exists and
  owns the per-record divergence shape. A local same-named schema would create a
  protocol collision.
- S1/S2: missing AAT spans are common, parser-IR requires spans, and an
  `INVENTION` sidecar entry conflicts with the current taxonomy's
  `records_sidecar = false`. Span synthesis needs an explicit measured rule or
  a separate span policy before implementation.
- S3: two current mapping pointers do not correspond to AAT schema fields:
  `blocks[].content[].gaiji.raw_marker` and
  `blocks[].content[].gaiji.unicode`. The former should point at
  `gaiji.description`; the latter is a derived absence/policy choice and should
  not pretend to resolve to an AAT field.
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
- Observed: The generated mapping has 25 measured folded rule buckets and no
  `UNSUPPORTED` bucket for the aozora-rs corpus.
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
module seam. The crate should not be implemented until the protocol gates below
are resolved, because the current artifacts do not yet make warigaki, span
synthesis, or divergence sidecar ownership explicit enough for the risk.

## Non-Goals

- Do not make `ab-ir` the adapter or parser-IR contract.
- Do not hand-copy the old 27-rule synthesized table.
- Do not start manifest identity or ABC compatibility-registry hardening here.
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

### Gate 3: Regenerate Mapping v1.1 or Refuse Warigaki by Default

The current mapping has no `UNSUPPORTED` rule.

Production behavior for the current mapping:

- `UnmeasuredDivergencePolicy::Refuse` is the default.
- AAT documents containing warigaki must refuse under the current mapping.
- No production test should expect `U-01` unless a regenerated mapping artifact
  contains a measured `UNSUPPORTED` warigaki rule.

Two acceptable paths:

1. Regenerate a mapping v1.1 from measured inputs that include current
   aozora2html AATs, producing a real `UNSUPPORTED` warigaki rule. Then
   production conversion may use `drop-sidecar` for warigaki.
2. Keep mapping v1.0 and treat warigaki as exploratory. The only test that
   emits an `UNSUPPORTED` warigaki record must run with
   `UnmeasuredDivergencePolicy::RecordExploratory`, use a synthesized rule id
   such as `U-00`, and clearly mark the bundle as exploratory.

The implementation plan should prefer path 1 if the CLI is expected to handle
aozora2html warigaki as production input. It should prefer path 2 only for a
diagnostic prototype.

### Gate 4: Fix Mapping Pointers

Before strict mapping preflight is enabled, regenerate the mapping artifact so
that `aat_pointer` values either resolve to fields in `data/aat-schema.json` or
are explicitly null for derived absence/policy rules.

Known current defects:

- `blocks[].content[].gaiji.raw_marker` should point at
  `blocks[].content[].gaiji.description`, because AAT has no `raw_marker`.
- `blocks[].content[].gaiji.unicode` should not pretend to be an AAT field.
  AAT currently carries `resolved` as a string/null and does not separate a
  Unicode codepoint field.

Implementation preflight after v1.1:

- resolve every non-null `aat_pointer` against `data/aat-schema.json` using the
  folded pointer syntax;
- reject pointers that cannot be resolved unless the mapping schema has grown a
  way to mark them as derived.

### Gate 5: Decide Span Synthesis as Mapping Policy

Parser-IR requires `span.start` and `span.end` on every node. Production AAT
spans are mostly absent.

The current mapping does not record a span synthesis rule, so a strict
"every divergence matches one measured rule" guard cannot honestly emit
zero-width spans and also claim the mapping is complete.

Required decision before implementation:

- add a measured span rule to mapping v1.1, probably `AMBIGUITY` or
  `STRUCTURAL`, with per-work aggregation rather than one sidecar record per
  node; or
- define span synthesis as a target-construction invariant outside the
  divergence taxonomy and document why it is not a divergence.

The first option is preferred because it keeps invented target coordinates
auditable. It should aggregate count and first_path per work to avoid millions
of nearly identical sidecar entries.

V1 span value policy, once the gate is resolved:

- if AAT span exists: `start = byte_start`, `end = byte_end`,
  `line = line_start`, `column = null`,
  `coordinate_system = decoded_utf8`;
- if AAT span is absent: synthesize a zero-width decoded-UTF8 span at the latest
  known offset and record/aggregate the chosen span rule;
- dropping `line_end` should be recorded under the same span rule or an
  explicit `LOSS` rule if multi-line spans are observed.

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

The current generated mapping records `meta.adapter` and
`meta.adapter_version` as `LOSS`, even though the current ABC parser-IR schema
has optional `derived_from`. To avoid silently changing the measured mapping,
v1 should keep the parser-IR document aligned with the generated mapping and
put adapter/mapping provenance in the divergence bundle. If ABC wants
`derived_from` populated, regenerate the mapping artifact first and make that a
measured policy change.

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

Rules that require v1.1 correction before production:

- AAT `warigaki` -> parser-IR has no warigaki node. Under v1.0 this must
  refuse by default. Under a measured v1.1 `UNSUPPORTED` rule it may
  `drop-sidecar` and preserve child visible text where possible.
- AAT spans absent -> parser-IR requires spans. This needs the span policy gate.
- AAT `gaiji.unicode` loss -> current pointer is invalid; v1.1 must express
  this as derived absence/policy, not an AAT field path.

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
- current mapping v1.0 refuses warigaki under `Refuse`.
- if mapping v1.1 contains measured warigaki `UNSUPPORTED`, warigaki structure
  is absent from parser-IR, child visible text is preserved when possible, and
  the bundle has a valid ABC divergence record.
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

## Next Route

Do not write the `crates/ab-aat-to-parser-ir` implementation plan yet.

Next work should be a protocol-correction plan:

1. Pin `abc-legacy-json-c14n-v0` in code/tests and report docs.
2. Define the divergence bundle schema around ABC's existing per-entry record
   schema.
3. Regenerate mapping v1.1 or explicitly keep warigaki exploratory/refusing.
4. Fix invalid mapping pointers.
5. Add a measured span synthesis rule or document span synthesis as an
   out-of-taxonomy target construction invariant.

After those corrections, the six-slice implementation plan is:

1. Crate scaffold and mapping/schema preflight.
2. Bundle schema and divergence record aggregation.
3. Minimal text/ruby/gaiji conversion.
4. Style, heading, block flattening, warnings, source encoding, and source hash
   ambiguity.
5. Warigaki/kunten behavior according to the corrected mapping.
6. CLI smoke and final verification.

## Incubation Notes

The `derived_from` field remains a separate measured-policy question. Parser-IR
can carry adapter and mapping provenance, but the current measured mapping says
those AAT metadata fields are lost. The conservative v1 choice is to keep
parser-IR aligned with the generated mapping and put provenance in the
divergence bundle. A future measured mapping revision can move some of that
provenance into `derived_from`.
