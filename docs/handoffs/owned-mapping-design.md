# Owned AAT → Parser-IR Mapping — Design Spec & Implementation Plan

> **Skills used:** `hammock-driven-design` and `writing-plans`.
> **Probe status:** The disposable probe at
> `docs/handoffs/aat-parser-ir-mapping-probe.md` is treated as settled
> evidence. This document does **not** re-litigate Option A/B/C/D; it
> records Option C with an owned, schema-addressable mapping and hands it
> off to implementation.

## 1. Owned mapping artifact spec

### 1.1 Where the artifacts live

| Artifact | Location | Owner | Purpose |
|---|---|---|---|
| Mapping **schema** | `schemas/aat-parser-ir-mapping.schema.json` | ABC | Normative contract for what a mapping document must contain. |
| Divergence **schema** | `schemas/aat-parser-ir-divergence.schema.json` | ABC | Normative contract for the per-document loss sidecar. Aggregates per-rule (rule_id + count + first-path), not per-occurrence — the full-corpus probe (`docs/handoffs/full-corpus-probe.md` Finding I) emitted 8,323,736 entries across 17,894 works with a worst-case file producing 151,681 entries; per-occurrence output is storage noise. |
| Mapping **document v1** | `ab-validator/data/aat-to-parser-ir-mapping-v1.json` | ab-validator | Producer-owned, versioned transform-rule set. |
| Mapping **CLI/runtime** | `ab-validator/crates/ab-aat-to-parser-ir/` | ab-validator | Executable that turns AAT + mapping → parser-IR + divergence sidecar. |
| Compatibility **registry** | `data/aat-parser-ir-compatibility.edn` | ABC | Declares which `(AAT version, mapping version, parser-IR schema)` triples are acceptable. |

Rationale: ABC owns the **shape** of what it accepts (schemas + registry);
ab-validator owns the **translation** because only the producer can decide
which AAT fields are faithful and which are synthesized.

### 1.2 Mapping schema shape (`schemas/aat-parser-ir-mapping.schema.json`)

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/aat-parser-ir-mapping.schema.json",
  "title": "AAT to Parser-IR Mapping",
  "type": "object",
  "required": [
    "mapping_id",
    "mapping_version",
    "mapping_schema_hash",
    "source_aat_version",
    "target_parser_ir_schema_id",
    "target_parser_ir_schema_hash",
    "transform_rule_descriptions",
    "loss_taxonomy"
  ],
  "additionalProperties": false,
  "properties": {
    "mapping_id": { "type": "string", "format": "uri" },
    "mapping_version": { "type": "string", "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$" },
    "mapping_schema_hash": { "$ref": "#/$defs/hash" },
    "source_aat_version": { "type": "integer", "minimum": 1 },
    "target_parser_ir_schema_id": { "type": "string", "format": "uri" },
    "target_parser_ir_schema_hash": { "$ref": "#/$defs/hash" },
    "transform_rule_descriptions": {
      "type": "array",
      "items": { "$ref": "#/$defs/rule" },
      "uniqueItems": true
    },
    "loss_taxonomy": {
      "type": "object",
      "required": ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"],
      "additionalProperties": false,
      "properties": {
        "LOSS": { "$ref": "#/$defs/category" },
        "INVENTION": { "$ref": "#/$defs/category" },
        "AMBIGUITY": { "$ref": "#/$defs/category" },
        "UNSUPPORTED": { "$ref": "#/$defs/category" },
        "STRUCTURAL": { "$ref": "#/$defs/category" }
      }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "category": {
      "type": "object",
      "required": ["description", "default_action"],
      "additionalProperties": false,
      "properties": {
        "description": { "type": "string" },
        "default_action": { "enum": ["drop-silent", "drop-sidecar", "invent", "refuse"] },
        "records_sidecar": { "type": "boolean" }
      }
    },
    "rule": {
      "type": "object",
      "required": ["rule_id", "category", "aat_pointer", "parser_ir_pointer", "action", "description"],
      "additionalProperties": false,
      "properties": {
        "rule_id": { "type": "string", "pattern": "^[A-Z]+-[0-9]+$" },
        "category": { "enum": ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"] },
        "aat_pointer": { "type": ["string", "null"] },
        "parser_ir_pointer": { "type": ["string", "null"] },
        "action": { "enum": ["drop", "project", "invent", "flatten", "refuse"] },
        "description": { "type": "string" }
      }
    }
  }
}
```

### 1.3 Mapping document v1 instance (`ab-validator/data/aat-to-parser-ir-mapping-v1.json`)

Historical draft: the live instance embedded the 27 synthesized probe ledger
entries as rules. **Errata 2026-07-03:** do not hand-transcribe this table as
the production starting point. The current ABC probe now generates
`prototypes/aat-to-parser-ir-probe/mapping.generated.aozora-rs.json` from
folded, measured aozora-rs rule buckets: 25 observed rules, zero UNSUPPORTED,
live mapping-schema hash
`sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`,
and live parser-IR schema hash
`sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`.
Use the generated candidate as evidence; treat the JSON below as historical
design context until an ab-validator-owned generator lands.

```json
{
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1",
  "mapping_version": "1.0.0",
  "mapping_schema_hash": "sha256:<hash-of-abc-mapping-schema>",
  "source_aat_version": 1,
  "target_parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "target_parser_ir_schema_hash": "sha256:<hash-of-parser-ir-schema>",
  "transform_rule_descriptions": [
    { "rule_id": "S-01", "category": "STRUCTURAL", "aat_pointer": "blocks[*].kind='heading'", "parser_ir_pointer": null, "action": "flatten", "description": "Heading block container is dropped; only its inline content is emitted (probe ledger STRUCTURAL-1)." },
    { "rule_id": "S-02", "category": "STRUCTURAL", "aat_pointer": "blocks[*].kind='paragraph'", "parser_ir_pointer": null, "action": "flatten", "description": "Paragraph block container is dropped; only its inline content is emitted (probe ledger STRUCTURAL-2)." },
    { "rule_id": "S-03", "category": "STRUCTURAL", "aat_pointer": "blocks[*].kind='paragraph'", "parser_ir_pointer": null, "action": "flatten", "description": "Paragraph block container is dropped; only its inline content is emitted (probe ledger STRUCTURAL-3)." },
    { "rule_id": "S-04", "category": "STRUCTURAL", "aat_pointer": "blocks[*].kind='paragraph'", "parser_ir_pointer": null, "action": "flatten", "description": "Paragraph block container is dropped; only its inline content is emitted (probe ledger STRUCTURAL-4)." },
    { "rule_id": "A-01", "category": "AMBIGUITY", "aat_pointer": "blocks[*].heading.level", "parser_ir_pointer": "nodes[*].type='heading'.level", "action": "project", "description": "AAT heading level is 1-3; parser-IR allows 1-6. Values fit but domains differ (probe ledger AMBIGUITY-1)." },
    { "rule_id": "A-02", "category": "AMBIGUITY", "aat_pointer": "blocks[*].content[*].gaiji.resolved", "parser_ir_pointer": "nodes[*].type='gaiji'.gaiji.resolved", "action": "project", "description": "AAT gaiji.resolved is the resolved character string; parser-IR requires a boolean 'was it resolved?' (probe ledger AMBIGUITY-2)." },
    { "rule_id": "A-03", "category": "AMBIGUITY", "aat_pointer": "blocks[*].content[*].accent", "parser_ir_pointer": "nodes[*].type='emphasis'", "action": "project", "description": "AAT accent is mapped to parser-IR emphasis; accent code/name semantics are not preserved (probe ledger AMBIGUITY-3)." },
    { "rule_id": "A-04", "category": "AMBIGUITY", "aat_pointer": "meta.source_hash", "parser_ir_pointer": "source.work_content_hash", "action": "project", "description": "AAT source_hash is a hash of raw source bytes; parser-IR work_content_hash is a content hash. Identifier semantics differ (probe ledger AMBIGUITY-4)." },
    { "rule_id": "L-01", "category": "LOSS", "aat_pointer": "blocks[*].heading.style", "parser_ir_pointer": null, "action": "drop", "description": "Heading style (e.g. 'main') has no parser-IR field (probe ledger LOSS-1)." },
    { "rule_id": "L-02", "category": "LOSS", "aat_pointer": "blocks[*].content[*].ruby.direction", "parser_ir_pointer": null, "action": "drop", "description": "Superseded by ADR 0024: parser-IR now has ruby.direction; production mapping should project direction directly and omit this LOSS divergence. Original probe ledger classified it as LOSS-2." },
    { "rule_id": "L-03", "category": "LOSS", "aat_pointer": "blocks[*].content[*].gaiji.unicode", "parser_ir_pointer": null, "action": "drop", "description": "AAT does not separate unicode codepoint from resolved string; parser-IR gaiji.unicode cannot be reliably populated (probe ledger LOSS-3)." },
    { "rule_id": "L-04", "category": "LOSS", "aat_pointer": "blocks[*].content[*].accent.name", "parser_ir_pointer": null, "action": "drop", "description": "Accent name (e.g. 'circumflex') has no parser-IR field (probe ledger LOSS-4)." },
    { "rule_id": "L-05", "category": "LOSS", "aat_pointer": "blocks[*].content[*].figure.css_class", "parser_ir_pointer": null, "action": "drop", "description": "Figure css_class (e.g. 'source-note') has no parser-IR field (probe ledger LOSS-5)." },
    { "rule_id": "L-06", "category": "LOSS", "aat_pointer": "blocks[*].content[*].figure.width", "parser_ir_pointer": null, "action": "drop", "description": "Figure width has no parser-IR field (probe ledger LOSS-6)." },
    { "rule_id": "L-07", "category": "LOSS", "aat_pointer": "blocks[*].content[*].figure.height", "parser_ir_pointer": null, "action": "drop", "description": "Figure height has no parser-IR field (probe ledger LOSS-7)." },
    { "rule_id": "L-08", "category": "LOSS", "aat_pointer": "meta.adapter", "parser_ir_pointer": null, "action": "drop", "description": "Adapter identity is adapter-faithfulness metadata, not publication identity (probe ledger LOSS-8)." },
    { "rule_id": "L-09", "category": "LOSS", "aat_pointer": "meta.adapter_version", "parser_ir_pointer": null, "action": "drop", "description": "Adapter version is adapter-faithfulness metadata, not publication identity (probe ledger LOSS-9)." },
    { "rule_id": "L-10", "category": "LOSS", "aat_pointer": "meta.parse_complete", "parser_ir_pointer": null, "action": "drop", "description": "Parse-complete status is adapter-faithfulness metadata, not publication identity (probe ledger LOSS-10)." },
    { "rule_id": "L-11", "category": "LOSS", "aat_pointer": "meta.metrics", "parser_ir_pointer": null, "action": "drop", "description": "Performance/fallback metrics are adapter-fidelity metadata with no publication-IR home (full-corpus probe Finding C: present in every real file). Included in sidecar aggregation as a count." },
    { "rule_id": "L-12", "category": "LOSS", "aat_pointer": "meta.semantic_summary", "parser_ir_pointer": null, "action": "drop", "description": "semantic_summary provenance is adapter-fidelity metadata with no publication-IR home (full-corpus probe Finding C: present in every real file). Included in sidecar aggregation as a count." },
    { "rule_id": "I-01", "category": "INVENTION", "aat_pointer": null, "parser_ir_pointer": "nodes[*].type='ruby'.ruby.scope", "action": "invent", "description": "AAT has no ruby scope; default to 'explicit' (probe ledger INVENTION-1)." },
    { "rule_id": "I-02", "category": "INVENTION", "aat_pointer": "blocks[*].content[*].gaiji.description", "parser_ir_pointer": "nodes[*].type='gaiji'.gaiji.raw_marker", "action": "invent", "description": "AAT has no raw source marker; use gaiji description as raw_marker (probe ledger INVENTION-2)." },
    { "rule_id": "I-03", "category": "INVENTION", "aat_pointer": "blocks[*].content[*].accent.code", "parser_ir_pointer": "nodes[*].type='emphasis'.style", "action": "invent", "description": "Use accent code (e.g. 'CU') as free-form emphasis style (probe ledger INVENTION-3)." },
    { "rule_id": "I-04", "category": "INVENTION", "aat_pointer": "blocks[*].content[*].figure.filename", "parser_ir_pointer": "nodes[*].type='image'.src", "action": "invent", "description": "Filename is not a resolved source path; used as image.src (probe ledger INVENTION-4)." },
    { "rule_id": "I-05", "category": "INVENTION", "aat_pointer": null, "parser_ir_pointer": "source.normalization", "action": "invent", "description": "parser-IR requires normalization; AAT has none — default 'source' (probe ledger INVENTION-5)." },
    { "rule_id": "I-06", "category": "INVENTION", "aat_pointer": null, "parser_ir_pointer": "source.source_path", "action": "invent", "description": "parser-IR source_path optional; AAT has none — default null (probe ledger INVENTION-6)." },
    { "rule_id": "I-07", "category": "INVENTION", "aat_pointer": null, "parser_ir_pointer": "schema_id, schema_hash", "action": "invent", "description": "parser-IR requires ABC schema identity; AAT only has version=1 — producer hardcodes current parser-IR schema (probe ledger INVENTION-7)." },
    { "rule_id": "I-08", "category": "INVENTION", "aat_pointer": null, "parser_ir_pointer": "errors[]", "action": "invent", "description": "parser-IR requires errors[]; AAT has no errors concept — default empty (probe ledger INVENTION-8)." },
    { "rule_id": "U-01", "category": "UNSUPPORTED", "aat_pointer": "blocks[*].content[*].warigaki", "parser_ir_pointer": null, "action": "drop-sidecar", "description": "parser-IR has no warigaki node; v1 mapping records the loss rather than refusing (probe ledger UNSUPPORTED-1). Default in development = drop-sidecar + critical-severity record; release-smoke (ADR 0006) fails on any critical-severity divergence record." },
    { "rule_id": "I-09", "category": "INVENTION", "aat_pointer": "blocks[*].content[*].style.style_type", "parser_ir_pointer": "nodes[*].type='emphasis'.style", "action": "invent", "description": "AAT style node (e.g. style_type='boten') -> parser-IR emphasis; style_type preserved verbatim in emphasis.style, child content emitted (full-corpus probe Finding G: 28,492 nodes in 5,474 files)." }
  ],
  "loss_taxonomy": {
    "LOSS": { "description": "AAT carries information that parser-IR has no field for.", "default_action": "drop-sidecar", "records_sidecar": true },
    "INVENTION": { "description": "parser-IR requires a value that AAT does not supply.", "default_action": "invent", "records_sidecar": false },
    "AMBIGUITY": { "description": "Same concept exists in both schemas but with different semantics or range.", "default_action": "drop-sidecar", "records_sidecar": true },
    "UNSUPPORTED": { "description": "AAT node kind has no parser-IR representation.", "default_action": "drop-sidecar", "records_sidecar": true },
    "STRUCTURAL": { "description": "Tree-shape differences force boundary/span loss.", "default_action": "drop-sidecar", "records_sidecar": true }
  }
}
```

### 1.4 Parser-IR provenance reference (`derived_from`)

Add an **optional** top-level object to `schemas/parser-ir.schema.json` directly
after `schema_hash`:

```json
"derived_from": {
  "type": ["object", "null"],
  "additionalProperties": false,
  "required": [
    "aat_version",
    "aat_adapter",
    "mapping_id",
    "mapping_version",
    "mapping_schema_hash"
  ],
  "properties": {
    "aat_version": { "type": "integer", "minimum": 1 },
    "aat_adapter": { "type": "string" },
    "aat_adapter_version": { "type": ["string", "null"] },
    "mapping_id": { "type": "string", "format": "uri" },
    "mapping_version": { "type": "string" },
    "mapping_schema_hash": { "$ref": "#/$defs/hash" }
  }
}
```

It is optional for backwards compatibility, but **required** for any parser-IR
emitted from AAT. Example value:

```json
"derived_from": {
  "aat_version": 1,
  "aat_adapter": "aozora2html",
  "aat_adapter_version": "aozora2html-adapter 0.1.0 gem-3.0.1",
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1",
  "mapping_version": "1.0.0",
  "mapping_schema_hash": "sha256:<hash-of-abc-mapping-schema>"
}
```

### 1.5 Manifest identity input — mapping is identity-bearing

**Decision (corrected 2026-07-02 per review §1.1):** the identity dimension
is the JCS hash of the mapping **document** (the rule set), not the mapping
**schema** hash. Field name: `aat_parser_ir_mapping_hash` (document hash),
NOT `aat_parser_ir_mapping_schema_hash` (schema hash).

**Why the original draft was wrong.** It promoted `mapping_schema_hash` into
`manifest_identity_object`, but the variability ADR 0001 protects against —
"two runs with identical work/parser/parser-config but different mappings can
produce different `content.content_hash`" — lives in the mapping **document**
(the rule set: I-09, U-01 policy, ruby-scope default), not the schema. Mapping
v1.0.0 and v1.1.0 validate against the same schema and therefore share
`mapping_schema_hash`; two different transformations would yield identical
`artifact_id`s with different content hashes — exactly the reproducibility
conflict ADR 0001 exists to prevent.

**Corrected rule:**
- `manifest_identity_object` gains `aat_parser_ir_mapping_hash` =
  `sha256(JCS(mapping_document))`, nullable (null for non-AAT-derived kinds).
- `mapping_schema_hash` stays in `derived_from` **provenance only** — it is
  not identity-bearing.

Consequences:

- `schemas/manifest.schema.json` adds `"aat_parser_ir_mapping_hash"` (the
  **document** hash) to `identityObject.required` and `identityObject.properties`.
  The field name in `manifest-inputs.json` is `mapping_hash` (JCS of the
  document).
- `schemas/manifest.schema.json` adds `"mapping-divergence"` to the
  `sidecar.role` enum.
- `src/abc/tools/manifest.clj` adds the key to `identity-keys` and to
  `identity-object`.

## 2. Loss-handling policy

The probe produced 27 divergence entries across five categories. The default
policy per category is:

| Category | Count from probe | Default action | Records sidecar? | Rationale |
|---|---|---|---|---|
| **STRUCTURAL** | 4 | drop, emit sidecar | yes | Flattening AAT blocks into parser-IR `nodes[]` is intentional; the loss is part of the target model. |
| **AMBIGUITY** | 4 | map closest value, emit sidecar | yes | Both schemas have the concept, but semantics/range differ. The mapping documents the difference. |
| **LOSS** | 10 | drop, emit sidecar | yes | Information exists only in AAT; parser-IR has no home. Adapter-faithfulness-only LOSS (probe LOSS-8/9/10) is acceptable for publication identity; publication-feature LOSS (probe LOSS-1/2/3/4/5/6/7) is acceptable in v1 but must be auditable. |
| **INVENTION** | 8 | invent per documented rule | value-level: yes; schema-level: no | Filling required parser-IR fields is necessary. Value-level inventions (ruby scope, raw marker, emphasis style, image src) encode producer choices and are recorded. Schema-level inventions (schema identity, normalization default, null source_path, empty errors) are pure mapping constants. |
| **UNSUPPORTED** | 1 (warigaki) + style nodes (mapped via I-09, not UNSUPPORTED) | **drop-sidecar** + critical severity (mode-bound: dev default; release-smoke fails on critical-severity records) | yes | Full-corpus probe (`docs/handoffs/full-corpus-probe.md` Finding G/H): the mapper's `style` UNSUPPORTED fires 28,492× across 5,474 files (30.6% of the corpus) — a hard `refuse` default would halt ingestion of ~1 in 3 works. Warigaki fires 0× in real aozora-rs output (17,894 docs). Demote default to `drop-sidecar`; add STYLE→EMPHASIS rule (I-09) so `style` maps rather than refuses. |

> **POLICY NOTE (mode-binding).** Strictness is bound to release mode, not a CLI flag. Development builds default to **drop-sidecar** (so a single rare construct doesn't halt ingestion); **release-smoke** (ADR 0006) fails on any divergence record with `severity=critical`. This converts the policy from operator-discipline into a gate — there is no `--strict` flag to forget to set. The warigaki-specific note still applies: warigaki fires 0× in real aozora-rs data, so the real unmeasured risk this gate addresses is aozora2html `style` nodes (Finding G), not warigaki.

### Per-entry disposition using probe ledger IDs

| ID | AAT field/node | Category | Disposition |
|---|---|---|---|
| S-01 | blocks[0][block=heading] | STRUCTURAL | Flatten to inline children; record in `divergence.jsonl`. |
| S-02 | blocks[1][block=paragraph] | STRUCTURAL | Flatten to inline children; record in `divergence.jsonl`. |
| S-03 | blocks[2][block=paragraph] | STRUCTURAL | Flatten to inline children; record in `divergence.jsonl`. |
| S-04 | blocks[3][block=paragraph] | STRUCTURAL | Flatten to inline children; record in `divergence.jsonl`. |
| A-01 | blocks[0].heading.level | AMBIGUITY | Pass integer through; record domain mismatch. |
| A-02 | blocks[1].content[3].gaiji.resolved | AMBIGUITY | Convert non-null → `true`, null → `false`; record semantics difference. |
| A-03 | blocks[1].content[4].accent | AMBIGUITY | Map to `emphasis`; record loss of accent code/name semantics. |
| A-04 | meta.source_hash | AMBIGUITY | Copy to `source.work_content_hash`; record raw-bytes vs content-hash semantics. |
| L-01 | blocks[0].heading.style | LOSS | Drop; record. |
| L-02 | blocks[1].content[1].ruby.direction | Superseded LOSS | ADR 0024 adds parser-IR `ruby.direction`; project directly and do not record this as a LOSS divergence. |
| L-03 | blocks[1].content[3].gaiji.unicode | LOSS | Drop; record. |
| L-04 | blocks[1].content[4].accent.name | LOSS | Drop; record. |
| L-05 | blocks[2].content[0].figure.css_class | LOSS | Drop; record. |
| L-06 | blocks[2].content[0].figure.width | LOSS | Drop; record. |
| L-07 | blocks[2].content[0].figure.height | LOSS | Drop; record. |
| L-08 | meta.adapter | LOSS | Drop; record (adapter identity). |
| L-09 | meta.adapter_version | LOSS | Drop; record. |
| L-10 | meta.parse_complete | LOSS | Drop; record. |
| L-11 | meta.metrics | LOSS | Drop; record (full-corpus probe Finding C — present in every real file). |
| L-12 | meta.semantic_summary | LOSS | Drop; record (full-corpus probe Finding C — present in every real file). |
| I-01 | blocks[1].content[1].ruby.scope | INVENTION | Default `"explicit"`; record value invention. |
| I-02 | blocks[1].content[3].gaiji.raw_marker | INVENTION | Use `description`; record value invention. |
| I-03 | blocks[1].content[4].accent.code → emphasis.style | INVENTION | Use `code`; record value invention. |
| I-04 | blocks[2].content[0].figure.filename → image.src | INVENTION | Use `filename`; record value invention. |
| I-05 | source.normalization | INVENTION | Default `"source"`; no per-doc record. |
| I-06 | source.source_path | INVENTION | Default `null`; no per-doc record. |
| I-07 | schema_id/schema_hash | INVENTION | Hardcode current parser-IR schema; no per-doc record. |
| I-08 | errors[] | INVENTION | Default `[]`; no per-doc record. |
| U-01 | blocks[3].content[1].warigaki | UNSUPPORTED | **Drop with critical-severity record.** Default behavior records the loss and continues; release-smoke (ADR 0006) fails on any divergence record with `severity=critical`, so the guarantee is enforced by the release gate rather than a per-invocation flag. (Annotated per `docs/handoffs/full-corpus-probe.md`: warigaki fires 0× in real aozora-rs data; the original `refuse` default was unsafe because the same category caught `style` nodes at 30.6% of the corpus.) |
| I-09 | blocks[*].content[*].style.style_type='boten' (and other style_type values) | INVENTION (metadata) / STRUCTURAL (mapping) | Map `style` → parser-IR `emphasis`; preserve `style_type` verbatim in `emphasis.style`; emit child content in order. Recorded as a value-level invention. (Probe Finding G: 28,492 nodes in 5,474 files.) |

The divergence sidecar schema (`schemas/aat-parser-ir-divergence.schema.json`):

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/aat-parser-ir-divergence.schema.json",
  "title": "AAT to Parser-IR Divergence Record",
  "type": "object",
  "required": ["rule_id", "category", "message", "count", "first_path"],
  "additionalProperties": false,
  "properties": {
    "rule_id": { "type": "string", "pattern": "^[A-Z]+-[0-9]+$" },
    "category": { "enum": ["LOSS", "INVENTION", "AMBIGUITY", "UNSUPPORTED", "STRUCTURAL"] },
    "aat_pointer": { "type": ["string", "null"] },
    "parser_ir_pointer": { "type": ["string", "null"] },
    "source_value": { "type": ["string", "integer", "boolean", "null"] },
    "target_value": { "type": ["string", "integer", "boolean", "null"] },
    "message": { "type": "string" },
    "count": { "type": "integer", "minimum": 1, "description": "Occurrences of this rule in this work (per-rule aggregation; full-corpus probe Finding I)." },
    "first_path": { "type": ["string", "null"], "description": "AAT pointer to the first occurrence, for debugging." }
  }
}
```

## 3. Versioning & evolution

There are **three independent version axes**:

1. **AAT** — `version` integer in the AAT document. Per `aat-contract.md`,
   additive optional fields stay in the same AAT version; breaking changes
   require AAT v2.
2. **parser-IR** — URI `schema_id` + content `schema_hash`. Additive
   parser-IR changes must be optional so older emitted documents remain valid.
   Breaking changes change the schema URI/hash.
3. **Mapping** — semver `mapping_version` + document `mapping_hash`. Patch =
   wording/clarifications; minor = new transform rules for the same AAT/parser-IR;
   major = new AAT or parser-IR major target. `mapping_schema_hash` remains
   provenance for the mapping-document contract; it is not the identity-bearing
   transform dimension.

### Compatibility registry

Create `data/aat-parser-ir-compatibility.edn` owned by ABC:

```clojure
[{:aat_version 1
  :mapping_id "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1"
  :mapping_version "1.0.0"
  :mapping_hash "sha256:<hash-of-mapping-document>"
  :mapping_schema_hash "sha256:<hash-of-abc-mapping-schema>"
  :parser_ir_schema_id "https://w3id.org/abc/schemas/parser-ir.schema.json"
  :parser_ir_schema_hash "sha256:<hash-of-parser-ir-schema>"
  :compatibility "lossy"
  :notes "AAT v1 -> parser-IR v1 via mapping v1.0.0. Loss documented in mapping v1."}]
```

A compatibility entry is valid iff:

- the mapping document validates against
  `schemas/aat-parser-ir-mapping.schema.json`, its JCS document hash matches
  `mapping_hash`, and its declared `mapping_schema_hash` matches the live
  mapping-schema hash, and
- the `parser_ir_schema_hash` matches either the live parser-IR schema hash or
  a parser-IR schema hash that is itself registered as backward-compatible.

### Validation rule

`abc.tools.validate-design-bundle` replaces the strict parser-IR hash equality
check with:

```clojure
(or (= declared-parser-ir-hash live-parser-ir-hash)
    (registry/compatible? {:aat-version ...
                           :mapping-id ...
                           :mapping-version ...
                           :parser-ir-schema-hash declared-parser-ir-hash}))
```

The same check applies to the mapping schema hash.

### v1 → v2 stories

| Change | Effect on AAT | Effect on mapping | Effect on parser-IR | Effect on old manifests |
|---|---|---|---|---|
| AAT adds optional field | AAT stays v1 | Mapping may get a minor bump if it starts using the field | parser-IR unchanged | None; old mapping still valid. |
| AAT adds node kind (e.g. warigaki) | AAT v2 | Mapping v1 drops/flatten (lossy), mapping v2 supports it once parser-IR supports it | parser-IR v2 adds node | Old AAT v1 manifests unchanged. New AAT v2 bundles must use a registered mapping. |
| parser-IR adds optional field | AAT unchanged | Mapping unchanged or minor bump to populate it | parser-IR hash changes, URI may bump | Old parser-IR v1 docs still validate against v2 if optional. Registry keeps (AAT v1, mapping v1) → parser-IR v2. |
| Transform rule changes | AAT unchanged, parser-IR unchanged | Mapping version bumps | parser-IR hash unchanged | New manifests get new mapping hash; old manifests keep old artifact_id. |
| Breaking parser-IR change | AAT may bump | Mapping major bump | Schema URI/hash change | Old documents remain under old schema via registry; new documents use new schema. |

## 4. Implementation plan

> **Goal:** Make the AAT → parser-IR mapping a first-class, versioned,
> schema-addressable artifact, validate it at ABC's boundary, and keep
> `nix run .#validate-design-bundle` green.

> **Architecture:** ABC owns the mapping schema, divergence schema, and
> compatibility registry; ab-validator owns the mapping document and CLI.
> The mapping CLI consumes AAT + mapping document and emits parser-IR plus a
> divergence sidecar. ABC's materialize-import and design-bundle gate verify
> the mapping reference against the registry.

> **Tech stack:** JSON Schema Draft 2020-12, Clojure (ABC), Rust (ab-validator),
> EDN registry, Nix flake apps, `cargo`, `clojure -M:test`.

### Global constraints

- `nix run .#validate-design-bundle` must remain green after every task.
- Every schema change that adds a required field must update all checked-in
  fixtures and hardened tests.
- Cross-repo changes must not make ABC depend on ab-validator source at
  validation time; the boundary stays file-based.
- No `TBD`, `TODO`, or placeholder steps.

### File structure

**ABC repo:**
- `schemas/aat-parser-ir-mapping.schema.json` — mapping contract
- `schemas/aat-parser-ir-divergence.schema.json` — divergence sidecar contract
- `schemas/parser-ir.schema.json` — add `derived_from`
- `schemas/manifest-inputs.schema.json` — add mapping hash
- `schemas/manifest.schema.json` — add `aat_parser_ir_mapping_hash` and sidecar role
- `data/aat-parser-ir-compatibility.edn` — compatibility registry
- `src/abc/tools/aat_parser_ir_compat.clj` — registry loader/checker
- `src/abc/tools/validate_design_bundle.clj` — use registry
- `src/abc/tools/manifest.clj` — identity object update
- `src/abc/tools/materialize_import.clj` — propagate mapping hash
- `src/abc/tools/malli.clj` — update `::manifest-inputs`
- `examples/ab-validator-output/*` — refresh fixtures
- `examples/v0/example-work/*` — refresh manifests and RDF fixtures
- `fixtures/canonicalization/*` — refresh identity object and expected hash
- `test/abc/tools/*_test.clj` — update fixtures and assertions

**ab-validator repo:**
- `data/aat-to-parser-ir-mapping-v1.json` — mapping document instance
- `crates/ab-aat-to-parser-ir/Cargo.toml`
- `crates/ab-aat-to-parser-ir/src/lib.rs`
- `crates/ab-aat-to-parser-ir/src/main.rs`
- `crates/ab-aat-to-parser-ir/tests/*`
- Root `Cargo.toml` workspace members

---

### Task 1: Add mapping and divergence schemas to ABC

**Files:**
- Create: `schemas/aat-parser-ir-mapping.schema.json`
- Create: `schemas/aat-parser-ir-divergence.schema.json`
- Modify: `src/abc/tools/validate_design_bundle.clj`

**Interfaces:**
- Consumes: none
- Produces: two JSON Schemas accepted by `schema-valid!`

**Steps:**

- [ ] **Step 1:** Write `schemas/aat-parser-ir-mapping.schema.json` exactly as in §1.2.
- [ ] **Step 2:** Write `schemas/aat-parser-ir-divergence.schema.json` exactly as in §2.
- [ ] **Step 3:** In `src/abc/tools/validate_design_bundle.clj`, add both schemas to the `let` in `validate-json-schemas!` and to the validation loop:
  ```clojure
  mapping-schema (files/read-json "schemas/aat-parser-ir-mapping.schema.json")
  divergence-schema (files/read-json "schemas/aat-parser-ir-divergence.schema.json")
  ```
  and include them in the `doseq`.
- [ ] **Step 4:** Run `nix run .#validate-design-bundle`.
  Expected: passes (new schemas have no fixtures yet).

---

### Task 2: Add `derived_from` to parser-IR schema

**Files:**
- Modify: `schemas/parser-ir.schema.json`

**Interfaces:**
- Consumes: none
- Produces: parser-IR documents may optionally declare `derived_from`.

**Steps:**

- [ ] **Step 1:** Add the `derived_from` property definition from §1.4 to the root `properties` of `schemas/parser-ir.schema.json`.
- [ ] **Step 2:** Run `nix run .#validate-design-bundle`.
  Expected: passes; existing fixtures omit `derived_from` so they are still valid.

---

### Task 3: Add mapping fields to `manifest-inputs` and Malli

**Files:**
- Modify: `schemas/manifest-inputs.schema.json`
- Modify: `src/abc/tools/malli.clj`
- Modify: `examples/ab-validator-output/manifest-inputs.json`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`
- Modify: `test/abc/tools/materialize_import_test.clj`

**Interfaces:**
- Consumes: `schemas/aat-parser-ir-mapping.schema.json` (Task 1)
- Produces: `manifest-inputs.json` carries `mapping_hash`; Malli and tests enforce it.

**Steps:**

- [ ] **Step 1:** In `schemas/manifest-inputs.schema.json`, add to `required`:
  `"mapping_hash"`. Add properties:
  ```json
  "mapping_hash": { "$ref": "#/$defs/hash" },
  "aat_version": { "type": "integer", "minimum": 1 },
  "mapping_id": { "type": "string" },
  "mapping_version": { "type": "string" }
  ```
- [ ] **Step 2:** In `src/abc/tools/malli.clj`, add `"mapping_hash"` to the `::manifest-inputs` required-keys list.
- [ ] **Step 3:** Compute the live mapping document hash after the mapping document exists:
  ```bash
  clojure -e "(require 'abc.tools.manifest) (println (abc.tools.manifest/schema-hash \"ab-validator/data/aat-to-parser-ir-mapping-v1.json\"))"
  ```
- [ ] **Step 4:** Update `examples/ab-validator-output/manifest-inputs.json`:
  ```json
  "mapping_hash": "sha256:<hash-from-step-3>",
  "aat_version": 1,
  "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1",
  "mapping_version": "1.0.0"
  ```
- [ ] **Step 5:** Update `test/abc/tools/validate_design_bundle_test.clj` `complete-manifest-inputs` with the same required `mapping_hash`.
- [ ] **Step 6:** Update every `manifest-inputs.json` literal in `test/abc/tools/materialize_import_test.clj` with `"mapping_hash": "sha256:..."` using any valid hash (e.g. the one from Step 3).
- [ ] **Step 7:** Run:
  ```bash
  clojure -M:test -n abc.tools.validate-design-bundle-test
  clojure -M:test -n abc.tools.materialize-import-test
  nix run .#validate-design-bundle
  ```
  Expected: all pass.

---

### Task 4: Add mapping dimension to manifest identity object

**Files:**
- Modify: `schemas/manifest.schema.json`
- Modify: `src/abc/tools/manifest.clj`
- Modify: `fixtures/canonicalization/manifest-identity-object.json`
- Modify: `fixtures/canonicalization/manifest-identity-object.canonical.json`
- Modify: `examples/v0/example-work/manifest.json`
- Modify: `examples/v0/example-work/source.manifest.json`
- Modify: `examples/v0/example-work/failure-manifest.example.json`
- Modify: `examples/v0/example-work/manifest.ttl`
- Modify: `examples/v0/example-work/failure-manifest.example.ttl`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/manifest_to_rdf_test.clj` if assertions change

**Interfaces:**
- Consumes: `schemas/manifest.schema.json`, `src/abc/tools/manifest.clj` contracts
- Produces: `manifest_identity_object` contains `aat_parser_ir_mapping_hash`; sidecar role allows `mapping-divergence`.

**Steps:**

- [ ] **Step 1:** In `schemas/manifest.schema.json`:
  - Add `"aat_parser_ir_mapping_hash"` to `identityObject.required`.
  - Add property:
    ```json
    "aat_parser_ir_mapping_hash": { "$ref": "#/$defs/nullableHash" }
    ```
  - Add `"mapping-divergence"` to the `sidecar.role` enum.
- [ ] **Step 2:** In `src/abc/tools/manifest.clj`:
  - Add `"aat_parser_ir_mapping_hash"` to `identity-keys`.
  - In `identity-object`, resolve it from `manifest-inputs` for parser-IR kinds, otherwise `nil`:
    ```clojure
    "aat_parser_ir_mapping_hash" (get manifest-inputs "mapping_hash")
    ```
- [ ] **Step 3:** Update `fixtures/canonicalization/manifest-identity-object.json` and `.canonical.json`:
  add `"aat_parser_ir_mapping_hash": null` in sorted position and recompute the expected hash.
- [ ] **Step 4:** Update `src/abc/tools/validate_design_bundle.clj` `validate-canonicalization!` expected hash to the value from Step 3.
- [ ] **Step 5:** Update `examples/v0/example-work/source.manifest.json`,
  `examples/v0/example-work/failure-manifest.example.json`, and
  `examples/v0/example-work/manifest.json` by adding
  `"aat_parser_ir_mapping_hash": null` (or the real mapping hash if the
  artifact is AAT-derived) and recomputing each `artifact_id`.
- [ ] **Step 6:** Regenerate the Turtle fixtures:
  ```bash
  nix run .#manifest-to-rdf -- examples/v0/example-work/manifest.json -o examples/v0/example-work/manifest.ttl
  nix run .#manifest-to-rdf -- examples/v0/example-work/failure-manifest.example.json -o examples/v0/example-work/failure-manifest.example.ttl
  ```
- [ ] **Step 7:** Run:
  ```bash
  clojure -M:test -n abc.tools.manifest-to-rdf-test
  nix run .#validate-design-bundle
  ```
  Expected: all pass.

---

### Task 5: Materialize-import validates and propagates the mapping reference

**Files:**
- Modify: `src/abc/tools/materialize_import.clj`
- Modify: `test/abc/tools/materialize_import_test.clj`

**Interfaces:**
- Consumes: `mapping_hash` from `manifest-inputs.json`
- Produces: parser-IR manifest includes mapping identity and sidecars divergence file if present.

**Steps:**

- [ ] **Step 1:** In `src/abc/tools/materialize_import.clj`, update `parser-ir-manifest`:
  - Pass `:aat_parser_ir_mapping_hash` via `identity-object`.
  - Add `(get manifest-inputs "mapping_hash")` to `:used`.
  - If `input-dir/divergence.jsonl` exists, add a sidecar:
    ```clojure
    {"role" "mapping-divergence"
     "hash" (str "sha256:" (files/sha256-file (io/file input-dir "divergence.jsonl")))
     "media_type" "application/jsonl"
     "path_hint" "divergence.jsonl"}
    ```
- [ ] **Step 2:** In `warnings-manifest`, set mapping hash to `nil`.
- [ ] **Step 3:** Update `test/abc/tools/materialize_import_test.clj` to assert the new identity key is present and `used` includes the mapping hash.
- [ ] **Step 4:** Run:
  ```bash
  clojure -M:test -n abc.tools.materialize-import-test
  nix run .#validate-design-bundle
  ```
  Expected: all pass.

---

### Task 6: Compatibility registry and design-bundle validation

**Files:**
- Create: `data/aat-parser-ir-compatibility.edn`
- Create: `src/abc/tools/aat_parser_ir_compat.clj`
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: mapping schema, parser-IR schema, registry
- Produces: `compat/compatibility-errors` used by `validate-ab-validator-output!`

**Steps:**

- [ ] **Step 1:** Create `src/abc/tools/aat_parser_ir_compat.clj`:
  ```clojure
  (ns abc.tools.aat-parser-ir-compat
    (:require [abc.tools.files :as files]
              [abc.tools.schema :as schema]
              [clojure.edn :as edn]))

  (def registry-path "data/aat-parser-ir-compatibility.edn")

  (defn load-registry []
    (-> registry-path files/path slurp edn/read-string))

  (defn compatible? [registry {:keys [aat_version mapping_id mapping_version mapping_hash parser_ir_schema_hash mapping_schema_hash]}]
    ...)
  ```
  Implement `compatible?` to return `true` only when an entry matches all
  supplied keys, the `mapping_hash` matches the mapping document JCS hash, and
  the `mapping_schema_hash` matches the live mapping-schema hash.

- [ ] **Step 2:** Create `data/aat-parser-ir-compatibility.edn` with the entry
  from §3, inserting the live hashes:
  - mapping document hash from Task 3 Step 3
  - mapping-schema hash from `schemas/aat-parser-ir-mapping.schema.json`
  - parser-IR schema hash: compute with
    ```bash
    clojure -e "(require 'abc.tools.manifest) (println (abc.tools.manifest/schema-hash \"schemas/parser-ir.schema.json\"))"
    ```
- [ ] **Step 3:** In `src/abc/tools/validate_design_bundle.clj`:
  - Add `(require '[abc.tools.aat-parser-ir-compat :as compat])`.
  - Replace `schema-hash-errors` strict parser-IR check with a registry-aware
    check:
    ```clojure
    (defn compatibility-errors [manifest-inputs parser-ir]
      (let [aat-version (get-in parser-ir ["derived_from" "aat_version"])
            mapping-id (get-in parser-ir ["derived_from" "mapping_id"])
            mapping-version (get-in parser-ir ["derived_from" "mapping_version"])
            mapping-hash (get manifest-inputs "mapping_hash")
            parser-ir-hash (get parser-ir "schema_hash")
            mapping-schema-hash (get-in parser-ir ["derived_from" "mapping_schema_hash"])]
        (when-not (compat/compatible? (compat/load-registry) {...})
          [(str "No registered compatibility rule for ...")])))
    ```
  - Ensure `validate-ab-validator-output!` calls `compatibility-errors`.
- [ ] **Step 4:** Add tests in `test/abc/tools/validate_design_bundle_test.clj` for
  a compatible triple and an incompatible triple.
- [ ] **Step 5:** Run:
  ```bash
  clojure -M:test -n abc.tools.validate-design-bundle-test
  nix run .#validate-design-bundle
  ```
  Expected: all pass.

---

### Task 7: ab-validator mapping document and CLI

**Files (ab-validator repo):**
- Create: `data/aat-to-parser-ir-mapping-v1.json`
- Create: `crates/ab-aat-to-parser-ir/Cargo.toml`
- Create: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Create: `crates/ab-aat-to-parser-ir/src/main.rs`
- Create: `crates/ab-aat-to-parser-ir/tests/mapping_tests.rs`
- Modify: `Cargo.toml` workspace members

**Interfaces:**
- Consumes: AAT JSON, mapping JSON
- Produces: `parser-ir.json`, `divergence.jsonl`

**Steps:**

- [ ] **Step 1:** Create `data/aat-to-parser-ir-mapping-v1.json` from a
  generated rule-bucket candidate, not by hand-copying the historical §1.3
  table. ABC's current probe candidate is
  `prototypes/aat-to-parser-ir-probe/mapping.generated.aozora-rs.json`; an
  ab-validator implementation should regenerate the equivalent from its mapper
  rules and measured corpus evidence.
  Compute and fill:
  - `mapping_schema_hash` = JCS hash of ABC's `schemas/aat-parser-ir-mapping.schema.json`.
  - `target_parser_ir_schema_hash` = JCS hash of ABC's `schemas/parser-ir.schema.json`.
- [ ] **Step 2:** Add `crates/ab-aat-to-parser-ir` to the workspace `members` in
  `Cargo.toml`.
- [ ] **Step 3:** Create `crates/ab-aat-to-parser-ir/Cargo.toml`:
  ```toml
  [package]
  name = "ab-aat-to-parser-ir"
  version = "1.0.0"
  edition = "2021"

  [dependencies]
  serde = { version = "1", features = ["derive"] }
  serde_json = "1"
  clap = { version = "4", features = ["derive"] }
  ```
- [ ] **Step 4:** Implement `src/lib.rs` with:
  - `struct Mapper { mapping: MappingDocument }`
  - `Mapper::map_aat(aat: &Value) -> Result<MappedOutput, MappingError>`
  - `MappedOutput { parser_ir: Value, divergence: Vec<Divergence> }`
  - For each rule ID from §2, branch on the AAT node and apply the action.
- [ ] **Step 5:** Implement `src/main.rs` with CLI:
  ```bash
  ab-aat-to-parser-ir map \
    --aat aat.json \
    --mapping data/aat-to-parser-ir-mapping-v1.json \
    --output-dir bundle/
  ```
  - **Default mode** (the only behavior the CLI has): a UNSUPPORTED construct (e.g. `U-01` warigaki) is dropped and emits a critical-severity divergence record; the CLI exits 0 and continues. There is no `--strict` flag to toggle.
    **Correction (full-corpus probe Finding H):** strictness is mode-bound, not a CLI flag. The default behavior drops the construct and emits a critical-severity divergence record, because `style` UNSUPPORTED fires on 30.6% of real works (Finding G) and warigaki fires 0× — a default that refuses would halt ingestion of ~1 in 3 works. The hard-guarantee behavior — fail on any critical-severity divergence record — is enforced by **release-smoke** (ADR 0006), not by a per-invocation `--strict` flag operators must remember to set.
- [ ] **Step 6:** Add tests in `tests/mapping_tests.rs` that:
  - Map the probe's `prototypes/aat-to-parser-ir-probe/aat-sample.json` and
    assert the output validates against ABC's `schemas/parser-ir.schema.json`
    and `schemas/aat-parser-ir-divergence.schema.json`.
  - Assert `warigaki` (and any UNSUPPORTED construct) records a critical-severity divergence in default mode and continues; assert the release-smoke gate (ADR 0006) fails on any critical-severity divergence record. There is no `--strict` flag — strictness is bound to release mode, not a CLI option.
  - Assert `style` nodes map to `emphasis` (I-09) and produce **zero UNSUPPORTED entries** on a real aozora-rs AAT. The 2026-07-03 source-encoding policy probe maps `meta.source_encoding=windows-31j-lossy` to parser-IR `Shift_JIS` with an AMBIGUITY sidecar entry, so the measured aozora-rs corpus now has zero UNSUPPORTED records.
- [ ] **Step 7:** Run:
  ```bash
  cargo test -p ab-aat-to-parser-ir
  cargo run -p ab-aat-to-parser-ir -- map \
    --aat prototypes/aat-to-parser-ir-probe/aat-sample.json \
    --mapping data/aat-to-parser-ir-mapping-v1.json \
    --output-dir /tmp/aat-map-out
  ```
  Expected: tests pass; command produces schema-valid parser-IR and divergence sidecar.

---

### Task 8: Refresh ABC boundary fixtures

**Files:**
- Modify: `examples/ab-validator-output/parser-ir.json`
- Modify: `examples/ab-validator-output/manifest-inputs.json` (already touched in Task 3)
- Optional create: `examples/ab-validator-output/divergence.jsonl`

**Interfaces:**
- Consumes: mapping metadata
- Produces: checked-in fixture demonstrates the new contract.

**Steps:**

- [ ] **Step 1:** Update `examples/ab-validator-output/parser-ir.json` to include
  a `derived_from` block matching the mapping declared in
  `manifest-inputs.json` (use the same hashes/versions).
- [ ] **Step 2:** If a divergence sidecar is added, create
  `examples/ab-validator-output/divergence.jsonl` with records for the
  loss/ambiguity/structural entries present in the fixture and add it to the
  parser-IR manifest sidecars via materialize-import in Task 5.
- [ ] **Step 3:** Run:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: both pass.

---

### Task 9: Record the decision in an ADR

**Files:**
- Create: `docs/adr/0023-aat-parser-ir-owned-mapping.md`

**Steps:**

- [ ] **Step 1:** Write ADR summarizing:
  - Probe verdict and why Option C was selected.
  - Mapping ownership (ab-validator) vs schema ownership (ABC).
  - Loss-handling policy by category.
  - Three-axis versioning and registry design.
  - Deferred decisions (see §5).
- [ ] **Step 2:** Run final gates:
  ```bash
  nix run .#validate-design-bundle
  nix flake check
  ```
  Expected: both pass.

## 5. Explicitly deferred decisions

The following are **not** decided by this design and must be resolved in later
ADRs or TEI vocabulary work:

1. **Whether `warigaki` should be added to parser-IR.**
   This is a TEI / publication-IR vocabulary decision, not a mapping decision.
   The v1 mapping **records** warigaki as a loss by default (drop-sidecar +
   critical-severity record); release-smoke (ADR 0006) fails on any
   critical-severity divergence record. Once parser-IR supports it, a new
   mapping version and registry
   entry can be added. (Annotated per `docs/handoffs/full-corpus-probe.md`:
   warigaki fires 0× in 17,894 real aozora-rs AAT documents; the original
   `refuse` default was demoted because the same UNSUPPORTED category caught
   `style` nodes at 30.6% of the corpus.)

1b. **Whether `kunten` (訓点 — 返り点 / 送り仮名 / 再読文字) should be added to
   AAT v1 and parser-IR.** Per `docs/handoffs/aozora-manual-integration-audit.md`
   Gap B: the manual's `annotation/kunten.html` documents 24 kunten markers
   and the **real corpus emits them** (`［＃（ツ）］`, `［＃（フ）］`, `［＃二］`,
   `［＃レ］` — the kanji/kana 返り点 notation for classical Chinese reading
   order). AAT v1 enumerates 20 node kinds and **none represents kunten**, so
   real classical-text works currently have no AAT v1 home for these markers.
   This is the one construct family (of 8 in the manual) that is documented,
   present in real data, and entirely absent from the AAT v1 schema. Like
   warigaki, adding it is a vocabulary decision deferred to a later AAT v2 /
   parser-IR ADR; the v1 mapping records the loss rather than inventing it.
   Frequency is low (classical-text subset) but non-zero — surfacing it here
   prevents v1 from silently losing the construct.

2. **Whether AAT `meta.metrics`, `meta.semantic_summary`, or `x-provenance`
   should flow into parser-IR or manifest provenance.**
   They are adapter-faithfulness metadata and are currently dropped as LOSS.
   Promoting any of them to publication identity requires its own ADR.

3. **Span coordinate conversion semantics.**
   The probe used `start=byte_start`, `end=byte_end`, `line=line_start`,
   `column=null`. A precise cross-schema span contract is deferred to a span-mapping ADR.

4. **Governance location of the mapping schema in the long term.**
   This design keeps the schema in ABC. A future shared `ab-schemas` repository
   (Option B) is not ruled out, but is deferred until cross-repo release
   cadence justifies the overhead.

5. **Backward-compatibility policy for old parser-IR schema hashes.**
   The registry can express compatibility rules; the policy for how long old
   parser-IR schemas remain accepted is operational and deferred.

---

*Spec produced: 2026-07-02. Built on probe evidence at `docs/handoffs/aat-parser-ir-mapping-probe.md`.*

---

## Errata applied 2026-07-02 (full-corpus probe corrections)

The original spec was written from the synthesized 27-entry probe
(`aat-parser-ir-mapping-probe.md`). Running the mapper over the full real
aozora-rs corpus (17,894 documents, `docs/handoffs/full-corpus-probe.md`)
refuted three assumptions and confirmed two. Applied corrections:

1. **UNSUPPORTED default policy demoted** (§2 table, U-01 disposition, Task 7
   Step 5, §5 deferred-decisions): `refuse` → `drop-sidecar` + critical
   severity; strictness now bound to release mode (release-smoke fails on any
   critical-severity divergence record) instead of an opt-in `--strict` flag.
   Rationale: the mapper's
   `style` UNSUPPORTED fires on 28,492 nodes across 5,474 files (30.6% of the
   corpus) — a `refuse` default would halt ~1 in 3 works. Warigaki (the
   original U-01 target) fires 0× in real data.

2. **STYLE→EMPHASIS rule added** (§1.3 mapping document, §2 disposition
   table): the new rule I-09 maps `style` (`style_type` e.g. `boten`) to
   parser-IR `emphasis`, preserving `style_type` verbatim. Replaces the
   mapper's prior "UNSUPPORTED: no first-class node" classification, which was
   a mapper limitation, not a schema limitation (Finding G).

3. **Divergence sidecar aggregates per-rule** (§1.1, divergence schema in §2):
   added `count` and `first_path` fields; required. Rationale: per-occurrence
   output emitted 8,323,736 entries corpus-wide with a worst-case of 151,681
   entries in one file (Finding I).

4. **LOSS-11 and LOSS-12 added** (§1.3, §2 disposition): `meta.metrics` and
   `meta.semantic_summary`, present in every real file, were missing from the
   synthesized 27-entry ledger (Finding C).

5. **Task 7 Step 6 regression assertion strengthened**: must assert zero
   UNSUPPORTED on a real aozora-rs AAT after I-09 and the source-encoding
   policy update, not just the synthesized sample. Follow-up measurement on
   2026-07-03 maps `meta.source_encoding=windows-31j-lossy` to parser-IR
   `Shift_JIS` with an AMBIGUITY sidecar entry, leaving zero measured
   aozora-rs UNSUPPORTED records.

Deferred, unchanged: warigaki-in-parser-IR vocabulary decision; R3
null-dimension (covered by canonicalization fixture, not a solver).

---

## Errata 2026-07-02 (review §1.1: mapping identity hash was wrong)

External review identified a correctness bug in §1.5: the original promoted
`mapping_schema_hash` into `manifest_identity_object`, but the variability it
guards (two mappings → different content) lives in the mapping **document**
(rule set), not the schema. Two mapping versions validating against the same
schema would share `mapping_schema_hash` → identical `artifact_id` + different
content → silent reproducibility conflict (the exact failure ADR 0001 prevents).

Corrected: identity dimension is now `aat_parser_ir_mapping_hash`
(document JCS hash); `mapping_schema_hash` demoted to `derived_from` provenance
only. Field name in `manifest-inputs.json` is `mapping_hash`.

This is the kind of bug the existing ADR 0001 SMT check does NOT catch (it
verifies rule consistency, not field-selection correctness) — flagging that
SMT gates aren't a substitute for design review of *which* hash is
identity-bearing, only for *whether* the chosen invariant is self-consistent.

---

## Errata 2026-07-02 (review §1.2: `--strict` CLI flag replaced with dev/release mode-binding)

External review §1.2 proposed replacing the owned-mapping spec's `--strict`
CLI flag for UNSUPPORTED handling with a mode-binding: default **drop-sidecar**
in development, but **release-smoke** (ADR 0006) fails on any
critical-severity divergence record. This converts the policy from
operator-discipline into a gate — there is no `--strict` flag to forget to
set. Applied to the U-01 disposition, the §2 loss-handling policy, Task 7
Step 5 (the ab-validator mapping crate default CLI behavior), and the §5
deferred-decisions warigaki annotation. The warigaki-specific note is
preserved: warigaki fires 0× in real aozora-rs data, so the real unmeasured
risk the gate guards is aozora2html `style` nodes (Finding G), not warigaki.

---

## Errata 2026-07-03 (ADR 0024: `ruby.direction` promoted into parser-IR)

ADR 0024 adds optional parser-IR `ruby.direction`, so the original probe rule
L-02 is no longer a production LOSS. Implementers should project
`blocks[*].content[*].ruby.direction` to `nodes[*].type='ruby'.ruby.direction`
and omit L-02 from the divergence sidecar. The historical L-02 row remains in
the synthesized probe ledger only to explain why this parser-IR addition was
prioritized.
