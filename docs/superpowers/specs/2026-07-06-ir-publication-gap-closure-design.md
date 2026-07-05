# IR Publication Gap Closure Design

Status: Proposed for review
Date: 2026-07-06
Owner boundary: ab-validator owns measured gap classification and evidence reports. ABC owns TEI profile policy, the custom preservation contract, renderer behavior, and schema admission.

## Problem

The current IR publication coverage report proves the right end-state shape but
still reports `IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS`.

Current measured report:

- Report: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Scope: 17,894 source-authority works, five parser evidence lanes
  (`aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, `aozora`)
- Unsupported gaps: 165
- Owner counts:
  - `custom_schema`: 104
  - `policy`: 41
  - `parser_ir_schema`: 15
  - `aat_to_parser_ir_converter`: 5

Those 165 gaps are not 165 independent design problems. They collapse into ten
families:

| Family | Gap rows | Current owner/category shape | Measured occurrence sum |
|---|---:|---|---:|
| span coordinates | 14 | `policy` / `AMBIGUITY` | 32,304,997 |
| style/rendition | 14 | `policy` / `AMBIGUITY`, `custom_schema` / `LOSS` | 820,964 |
| accent | 12 | `policy` / `AMBIGUITY` and `INVENTION` | 6,636 |
| source identity | 5 | `policy` / `AMBIGUITY` and `INVENTION` | 356,683 |
| provenance metrics | 3 | `custom_schema` / `LOSS` | 139,773 |
| figure metadata | 83 | `custom_schema` / `LOSS` | 25,433 |
| gaiji unresolved reason | 4 | `custom_schema` / `LOSS` | 22,445 |
| heading/jisage structure | 28 | policy/custom/converter mix | 472,923 |
| font size / tcy | 13 | custom/schema mix | 54,043 |
| keigakomi / yokogumi | 8 | custom/schema mix | 517 |

The next step is therefore not another broad parser benchmark. It is a closure
contract that decides which gap families are TEI policy projections, which are
ABC custom-preservation facts, and which require Parser-IR or converter changes.

## Decision

Use a classification-first closure sequence.

Every current unsupported gap family must move into exactly one closure lane:

1. `tei_policy_projection`
   - TEI P5 can carry the publication fact, but ABC must declare the convention
     in the TEI profile or rendering policy.
2. `tei_plus_abc_extension`
   - TEI carries the human-facing publication form; exact source/IR identity is
     preserved through the ABC custom contract.
3. `custom_sidecar`
   - The fact is not TEI publication content and belongs only in the ABC custom
     preservation contract.
4. `parser_ir_schema_delta`
   - Parser-IR lacks a first-class representation needed to preserve the fact.
5. `aat_to_parser_ir_converter_delta`
   - Parser-IR can represent the fact, but the converter is not emitting it.

The coverage report may not claim completion while any family remains
`unsupported_gap`. It may, however, stop reporting a family as unsupported once
the family has a named lane, named owner, and an explicit admission gate.

## Closure Buckets

### 1. Span Coordinates

Current shape:

- 14 unsupported rows
- `policy` / `AMBIGUITY`
- Very high prevalence: 32,304,997 measured occurrences

Decision:

- Class: `custom_sidecar`
- Target: ABC preservation contract
- Rationale: Parser-IR spans are necessary for validation, traceability, and
  diagnostics, but span coordinates are not TEI publication content. TEI may
  receive anchors where ABC chooses, but full span identity belongs in the
  custom preservation contract.

Admission gate:

- ABC custom contract defines span-record shape or states that spans are
  preserved through existing IR pointer records.
- ab-validator coverage classifier maps all `*.span` divergence rows to
  `custom_sidecar`.

### 2. Style / Rendition

Current shape:

- 14 unsupported rows
- Style rows split between `policy` / `AMBIGUITY` and `custom_schema` / `LOSS`

Decision:

- Class: `tei_policy_projection`
- Target: TEI `@rend` / `@rendition`, mostly on `hi`, `p`, `div`, or `head`
- Exact source style value preservation: `custom_sidecar` when TEI `@rend`
  cannot preserve the source marker unambiguously.

Admission gate:

- ABC TEI profile names the allowed rendition vocabulary for Aozora style
  constructs.
- Coverage classifier maps known style rows to `tei_policy_projection` plus
  `custom_sidecar` for exact source marker preservation when needed.

### 3. Accent

Current shape:

- 12 unsupported rows
- 6 ambiguity rows for accent structure
- 6 invention rows for accent code

Decision:

- Class: `tei_plus_abc_extension`
- Target: TEI `hi @rend` for publication display; ABC sidecar for exact accent
  code and source-marker identity.

Admission gate:

- ABC profile declares the accent rendition vocabulary.
- ABC custom contract preserves the original accent code.
- Coverage classifier maps `accent` rows to `tei_plus_abc_extension` and
  `accent.code` rows to `custom_sidecar`.

### 4. Source Identity

Current shape:

- 5 unsupported rows
- Includes `meta.source_encoding`, `meta.source_hash`,
  `source.normalization`, `source.source_path`, and `schema_id/schema_hash`

Decision:

- Class: `custom_sidecar`
- Target: ABC sidecar and manifest identity.

Admission gate:

- ABC custom contract includes source identity and normalization fields.
- Manifest linkage records the parser-IR schema and source identity hashes.

### 5. Provenance Metrics

Current shape:

- 3 unsupported rows
- `meta.metrics`, `meta.parse_complete`, `meta.semantic_summary`

Decision:

- Class: `custom_sidecar`
- Target: ABC sidecar.

Admission gate:

- ABC custom contract includes a `producer_metrics` or equivalent provenance
  section.
- `parse_complete` is either preserved as a sidecar fact or superseded by a
  stronger validation/admission verdict.

### 6. Figure Metadata

Current shape:

- 83 unsupported rows
- Mostly deep-path `figure`, `figure.css_class`, `figure.width`, and
  `figure.height`

Decision:

- `figure.width` / `figure.height`: `tei_policy_projection`
  - Target: TEI `graphic @width` / `@height` when values are dimensional and
    publication-safe.
- `figure.css_class`: `tei_plus_abc_extension`
  - Target: TEI `@rend` when representable; ABC sidecar preserves exact source
    class.
- Whole `figure` rows at impossible/deep nested paths: split by provenance.
  - If source-derived and recoverable: `tei_policy_projection`
  - If parser recovery artifact: `custom_sidecar`

Admission gate:

- Coverage classifier recognizes figure subfields rather than treating deep
  path depth as a distinct unsupported construct.
- ABC custom contract records exact CSS/source class when TEI `@rend` is not
  exact.

### 7. Gaiji Unresolved Reason

Current shape:

- 4 unsupported rows
- `gaiji.unresolved_reason`

Decision:

- Class: `custom_sidecar`
- Target: ABC sidecar.
- Rationale: TEI `g` / `charDecl` can carry the visible gaiji reference, but
  parser failure reason is producer evidence, not publication text.

Admission gate:

- ABC custom contract has a record kind for gaiji resolution diagnostics.
- Coverage classifier maps `gaiji.unresolved_reason` to `custom_sidecar`.

### 8. Heading / Jisage Structure

Current shape:

- 28 unsupported rows
- Includes heading style loss, heading structural rows, and jisage structural
  rows.

Decision:

- Heading structure: `tei_policy_projection`
  - Target: TEI `div/head`, with heading level policy.
- Heading style: `tei_policy_projection` plus `custom_sidecar` for exact source
  style when needed.
- Jisage: `tei_policy_projection`
  - Target: paragraph or division `@rend` indentation policy.
- Converter-owned structural rows: `aat_to_parser_ir_converter_delta` until
  converter emits paragraph/layout/heading facts consistently.

Admission gate:

- Existing `paragraph.layout` and heading-level policy are applied to source
  rows currently reported as unsupported.
- Converter emits Parser-IR paragraph/layout rows for jisage and heading
  structure where the schema can already represent them.

### 9. Font Size / TCY

Current shape:

- 13 unsupported rows
- `font_size` and `tcy`

Decision:

- `font_size`: `tei_policy_projection`
  - Target: TEI `hi @rend` / CSS-style `font-size:*` vocabulary.
- `tcy`: `tei_plus_abc_extension`
  - Target: TEI `hi @rend="text-combine-upright"` or equivalent declared
    profile value; ABC sidecar preserves the original Aozora TCY marker.

Admission gate:

- Parser-IR schema has either a generic inline rendition node/field or explicit
  support for `font_size` and `tcy`.
- ABC TEI profile declares concrete rendering values.
- Custom contract preserves original marker identity for `tcy`.

### 10. Keigakomi / Yokogumi

Current shape:

- 8 unsupported rows
- Includes inline and block-level `keigakomi` / `yokogumi`

Decision:

- Class: `tei_plus_abc_extension`
- Target:
  - `keigakomi_block` -> TEI `div type="keigakomi" rend="bordered"` unless
    content is truly self-contained, in which case ABC may use
    `floatingText`.
  - `yokogumi_block` -> TEI `div type="yokogumi" rend="horizontal"`.
  - Inline keigakomi/yokogumi -> TEI `seg` or `hi` with declared `@rend`.
- ABC sidecar preserves source marker identity and boundary evidence.

Admission gate:

- Parser-IR schema distinguishes inline vs block layout containers, or provides
  a generic layout-scope construct that can represent both.
- ABC profile names the TEI vocabulary.

## Report Changes Required In ab-validator

The publication coverage classifier should stop using path depth as the primary
semantic identity for publication coverage. The mapping artifact may still
retain depth-specific rule IDs, but the coverage report should fold unsupported
rules into closure families before assigning publication classes.

Required classifier additions:

- Add a `closure_family` field for unsupported-derived rows.
- Add a `closure_lane` field with one of:
  - `tei_policy_projection`
  - `tei_plus_abc_extension`
  - `custom_sidecar`
  - `parser_ir_schema_delta`
  - `aat_to_parser_ir_converter_delta`
- Add `admission_gate` text for every remaining non-complete family.
- Treat the following as coverage folds:
  - any pointer ending in `.span` -> `span_coordinates`
  - any pointer ending in `.style` -> `style_rendition`
  - any pointer containing `.accent` -> `accent`
  - source/meta identity pointers -> `source_identity`
  - producer metrics pointers -> `provenance_metrics`
  - figure subfields and deep figure paths -> `figure_metadata`
  - `gaiji.unresolved_reason` -> `gaiji_unresolved_reason`
  - heading and jisage rows -> `heading_jisage_structure`
  - `font_size` and `tcy` rows -> `font_tcy`
  - `keigakomi` and `yokogumi` rows -> `keigakomi_yokogumi`

After this classifier update, the report should distinguish:

- `classified_but_not_admitted` families: known target, missing ABC/schema work.
- `true_unsupported_gap` families: no target selected.

The completion verdict remains blocked until both are empty or admitted by the
appropriate owner. The purpose of this spec is to avoid pretending that
classification debt is the same as representability failure.

## ABC Work Required

ABC-side follow-up should define:

- Custom preservation schema/sidecar shape.
- TEI profile rendition vocabulary:
  - warigaki
  - accent
  - font-size
  - tcy
  - keigakomi
  - yokogumi
  - jisage
  - figure metadata
- Manifest linkage between TEI, custom preservation record, Parser-IR schema,
  source identity, and mapping identity.
- Renderer policy for:
  - `ruby type="furigana"`
  - inline warigaki notes
  - figure dimensions/classes
  - line/page/span coordinate handling

## Non-Goals

- Do not claim TEI-EAJ equivalence for constructs absent from the TEI-EAJ
  corpus.
- Do not force parser provenance, mapping identity, or diagnostics into TEI body
  text.
- Do not use the custom sidecar as an excuse to skip faithful TEI P5 mappings.
- Do not weaken the metadata-free plaintext policy.
- Do not remove depth-specific rule IDs from the mapping artifact; only fold
  them for publication coverage reporting.

## Acceptance Criteria

This design is ready for implementation when:

1. The ten current unsupported families are all represented in the coverage
   classifier.
2. The report exposes family-level closure lanes and admission gates.
3. No row remains `true_unsupported_gap` unless it falls outside the ten known
   families.
4. The top-level verdict still blocks completion while ABC custom contract,
   Parser-IR schema deltas, or converter deltas are missing.
5. The plaintext policy remains unchanged:
   `exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance`.

## Self-Review

- Placeholder scan: no TBD/TODO placeholders.
- Internal consistency: TEI remains primary publication XML; custom sidecar
  preserves exact non-TEI facts; plaintext remains metadata-free.
- Scope check: this spec covers classification and closure lanes only. It does
  not implement ABC's custom contract or Parser-IR schema changes.
- Ambiguity check: closure lanes and admission gates are explicit for every
  current unsupported family.
