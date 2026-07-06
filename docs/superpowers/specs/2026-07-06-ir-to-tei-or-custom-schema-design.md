# IR To TEI Or Custom Schema Publication Contract

Status: Accepted; implemented as the current IR publication coverage gate
Date: 2026-07-06
Owner boundary: ABC owns the publication contract, TEI profile, custom extension/schema, plaintext policy, and renderer behavior. ab-validator owns measured source-authority, parser/adapter evidence, AAT-to-parser-IR mapping evidence, and generated-output coverage reports.

## Problem

The previous working label, "Full TEI 2/3 generation via IR," is too narrow.
It usefully pushed paragraph, source-note, ruby, and TEI-EAJ comparison work,
but it is not the exact end state.

The real goal is:

> Complete TEI mapping from Aozora Bunko markup through parser-IR. Every
> observed Parser-IR construct and source-derived Aozora markup family must be
> publication-accounted-for. Use TEI P5 where TEI is a faithful target. Use an
> explicit ABC custom schema, extension, or sidecar where TEI is not exact.
> Plaintext remains metadata-free visible text.

Current evidence shows why this distinction matters:

- ABC can materialize the TEI-EAJ workset through parser-IR and TEI with zero
  materialization failures.
- The source-authority gate over the Aozora corpus has no unallowlisted unknown
  source markers.
- TEI-EAJ comparison remains useful calibration evidence, but TEI-EAJ does not
  cover several Aozora constructs: warigaki, keigakomi, yokogumi, page breaks,
  accent marks, raw-source preservation, and ABC provenance semantics.
- TEI P5 can faithfully express many constructs, but some IR facts are
  ABC-specific identity, provenance, recovery, or source-authority facts that
  should not be forced into misleading TEI elements.

This spec changes the headline gate from "does this look like TEI Level 3?" to
"is every IR node and source-derived construct mapped, preserved, or explicitly
blocked by a named gap?"

## Evidence Ledger

| Claim | Evidence | Consequence |
|---|---|---|
| TEI-EAJ is comparison evidence, not source authority. | Profile-aware Level 3 admission spec and generated matrix reports distinguish source authority from TEI-EAJ rows. | Do not use TEI-EAJ level labels as the final success definition. |
| Parser-IR already carries core publication nodes. | `../abc/schemas/parser-ir.schema.json` includes text, ruby, gaiji, editor-note, emphasis, heading, indentation, page-break, line-break, image, caption, quote, source-note, and paragraph rows. | The next gate can be coverage/accounting, not only schema creation. |
| ABC plaintext intentionally strips metadata. | `../abc/src/abc/tools/parser_ir_plaintext.clj` renders ruby as base text, omits editor notes, separates source notes, and ignores layout metadata. | Plaintext must stay separate from TEI/custom metadata preservation. |
| ABC TEI rendering already has paragraph-aware and source-note-aware paths. | `../abc/src/abc/tools/parser_ir_tei.clj` branches on `paragraphs[]`, routes `source-note` to front/body/back, and renders paragraph layout as `@rend`. | The custom contract should extend this renderer, not replace it. |
| TEI P5 is sufficient for many Aozora constructs. | `/tmp/tei-node-coverage-gaps-report.md` maps ruby, gaiji, page/line breaks, paragraphs, headings, notes, figures, quotes, `hi`, `div`, and layout `@rend` to TEI P5. | Keep TEI P5 as the primary publication surface. |
| TEI P5 alone is not an exact account for every IR fact. | The same report identifies raw-source recovery, warigaki substructure, some layout semantics, parser provenance, source pointers, and mapping/divergence evidence as special cases. | Add an ABC-owned custom extension/schema or sidecar rather than overloading TEI. |

## Decision

Adopt an IR publication coverage contract with three coordinated outputs:

1. **TEI P5 publication XML**
   - The human-facing scholarly/publication projection.
   - Uses standard TEI P5 elements where faithful.
   - May use ABC-declared TEI profile conventions such as controlled `@rend`
     values and `encodingDesc` policy declarations.

2. **ABC custom preservation contract**
   - A machine-facing schema for IR facts that TEI does not carry exactly.
   - May be represented as an adjacent JSON sidecar, a namespaced XML extension,
     or both after ABC chooses the serialization form.
   - Must be versioned, schema-validated, content-addressable, and referenced
     from the TEI/manifest layer.

3. **Plaintext projection**
   - Visible body text only.
   - No ruby readings, layout metadata, source notes, provenance, parser
     warnings, or custom metadata unless the source explicitly makes that text
     body content.

The admission claim becomes:

> For a declared parser/adapter/version/corpus scope, every observed Parser-IR
> node kind and source-derived construct is classified as TEI, TEI-policy,
> custom-preserved, plaintext-only, or unsupported with a named blocker.

## Mapping Classes

Every Parser-IR node kind, node field, paragraph field, warning/error class, and
source-authority construct must be assigned exactly one primary publication
class.

### `tei_exact`

The IR fact maps directly to TEI P5 without loss of semantics.

Examples:

- `text` -> text node
- `ruby` -> `ruby type="furigana"` when classified as ordinary Aozora reading
- `gaiji` -> `g` with `charDecl`
- `page-break` -> `pb`
- `line-break` -> `lb`
- `image` -> `figure/graphic`
- body paragraphs -> `p`
- headings -> `head`

### `tei_policy_projection`

The IR fact maps to TEI P5, but ABC must declare the convention in the TEI
profile or `encodingDesc`.

Examples:

- `source-note` placement -> `front`, `body`, or `back` note routing
- paragraph layout -> `p @rend`
- emphasis and inline container style -> `hi @rend` or `@rendition`
- quote/caption policy where source structure does not fully determine the
  semantic target
- raw source represented in TEI as `seg type="raw-source"` when ABC decides
  that is a publication-safe recovery path

### `tei_plus_abc_extension`

The primary output is TEI P5, but exact preservation requires an ABC extension
or declared profile vocabulary.

Examples:

- warigaki rendered as `note place="inline" rend="warigaki"` or `rend="割注"`
  while preserving upper/lower line identity in the ABC sidecar if TEI text
  alone would lose it
- accent marks rendered as `hi @rend` while preserving the original Aozora/JIS
  accent code in the ABC sidecar
- `tcy`, `yokogumi`, and keigakomi when rendered through TEI visual vocabulary
  but retaining source-marker identity separately

### `custom_sidecar`

The fact is not a TEI publication fact and should be preserved outside the TEI
tree.

Examples:

- parser/adapter identity and version
- AAT-to-parser-IR mapping identity and hash
- divergence records
- source pointers and source-authority row IDs
- exact recovery details for parser-derived raw nodes
- confidence, evidence owner, and admission blocker classifications

### `plaintext_only`

The fact affects visible text but has no metadata representation in plaintext.

Examples:

- ruby base text in plaintext
- gaiji visible replacement text in plaintext
- line/page break visible spacing policy

This class must not be used to justify dropping metadata from TEI or the custom
contract.

### `unsupported_gap`

The fact is observed but not yet represented by TEI, policy projection, or the
custom contract.

Rules:

- Every `unsupported_gap` must have an owner: parser/adapter, parser-IR schema,
  AAT-to-parser-IR converter, ABC TEI renderer, ABC custom schema, or policy.
- Every `unsupported_gap` must carry measured prevalence when available.
- A report with any `unsupported_gap` cannot claim full IR publication
  coverage.

## Initial Coverage Targets

The initial mapping table is derived from the current Parser-IR schema and the
TEI node coverage gap report.

| Construct | Initial class | Target |
|---|---|---|
| `text` | `tei_exact` | TEI text and plaintext text. |
| `ruby` | `tei_exact` | `ruby type="furigana"` by default; plaintext emits base only. |
| `gaiji` | `tei_exact` with schema delta | `g` + `charDecl`; align `gaiji.resolved` semantics so resolved string is not reduced to boolean. |
| `editor-note` | `tei_policy_projection` | TEI `note`; plaintext omits. |
| `emphasis` | `tei_policy_projection` | `hi @rend`; inline children preserve nested ruby/style. |
| `heading` | `tei_exact` | `head`; profile-specific sectioning remains policy. |
| `indentation` and paragraph `layout` | `tei_policy_projection` | `p @rend` for paragraph layout; structural `div` when the source marks a block boundary. |
| `page-break` | `tei_exact` | `pb`; plaintext spacing only. |
| `line-break` | `tei_exact` | `lb`; lineated-text policy remains separate from prose admission. |
| `image` | `tei_exact` | `figure/graphic`, with `figDesc` or caption when available. |
| `caption` | `tei_policy_projection` | Figure `head` when associated; otherwise typed `div`/`seg` policy. |
| `quote` | `tei_policy_projection` | `quote` or `cit` depending on source attribution evidence. |
| `source-note` | `tei_policy_projection` | Placement-driven TEI front/body/back routing; plaintext body-only rule preserved. |
| `warigaki` | `tei_plus_abc_extension` | TEI inline note with declared rend; preserve split-line identity explicitly. |
| `raw-source` / recovery | `tei_policy_projection` or `custom_sidecar` | Preserve source-derived raw content; parser-derived raw recovery remains separately classified. |
| accent marks | `tei_plus_abc_extension` | TEI `hi @rend` plus original accent code preservation. |
| mapping/divergence/provenance | `custom_sidecar` | Versioned ABC schema, manifest-linked. |

## Custom Contract Requirements

ABC should define a versioned preservation contract before claiming full IR
publication coverage. The contract may be a JSON sidecar first; a TEI namespace
extension can follow if ABC wants inline XML preservation.

Required sidecar fields:

- `schema_id`
- `schema_version`
- `parser_ir_schema_id`
- `parser_ir_schema_hash`
- `tei_profile_id`
- `tei_profile_hash`
- `source`
- `producer`
- `mapping`
- `coverage`
- `records`

Each record must include:

- `record_id`
- `ir_pointer`
- `tei_pointer` or `null`
- `class`
- `construct`
- `source_pointer` or `null`
- `source_inventory_row` or `null`
- `message`
- `count`
- `first_path`

The sidecar is not a dumping ground for renderer leftovers. It is the owned
contract for exact facts that do not belong in TEI or plaintext.

## Report Contract

ab-validator should produce a machine-readable coverage report with:

- `schema_version`
- `scope`
- `source_authority_gate`
- `parser_evidence_coverage`
- `parser_ir_schema`
- `tei_profile`
- `custom_contract`
- `node_coverage`
- `field_coverage`
- `source_construct_coverage`
- `unsupported_gaps`
- `tei_eaj_calibration`
- `plaintext_policy`

Required verdicts:

- `IR_PUBLICATION_COVERAGE_COMPLETE`
- `IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS`
- `IR_PUBLICATION_COVERAGE_BLOCKED_CUSTOM_CONTRACT_MISSING`
- `IR_PUBLICATION_COVERAGE_BLOCKED_INCOMPLETE_PARSER_EVIDENCE`
- `IR_PUBLICATION_COVERAGE_BLOCKED_SOURCE_AUTHORITY`

The report must include all five active parser inputs where evidence is
available:

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`

Missing parser evidence is an evidence gap, not an implicit pass.

## TEI-EAJ Role

TEI-EAJ remains important, but its role is calibration:

- It checks whether ABC TEI resembles an external Japanese TEI practice where
  comparable material exists.
- It reveals profile-specific policy lanes: plain prose, lineated text, drama,
  notes, front/back matter, and Level 4 enrichment.
- It must not be treated as source authority for Aozora constructs that TEI-EAJ
  does not encode.
- It must not require semantic enrichment that Aozora Bunko markup cannot
  provide without human or model interpretation.

TEI-EAJ comparison reports should therefore feed `tei_eaj_calibration`, not the
headline full-coverage verdict.

## Plaintext Policy

Plaintext remains a separate projection with a stricter rule:

> Plaintext emits only visible body text. It does not include ruby readings,
> source notes routed to front/back, layout metadata, custom sidecar records,
> parser warnings, or provenance.

This rule prevents the full-publication goal from contaminating plaintext with
metadata needed only by TEI/custom consumers.

## Non-Goals

- Do not claim TEI Level 4 or Level 5 semantic enrichment from Aozora markup
  alone.
- Do not infer people, places, speakers, roles, or speech acts unless source
  markup or an explicit enrichment layer provides that fact.
- Do not make TEI-EAJ paragraph parity the universal gate.
- Do not use custom schema as an excuse to skip faithful TEI P5 mappings.
- Do not put ruby readings or metadata into plaintext.

## Current Implementation State

The first implementation of this contract is measured in:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`

Current measured verdict:

- `IR_PUBLICATION_COVERAGE_COMPLETE`
- `FIVE_PARSER_EVIDENCE_COMPLETE` for `aozora2html`, `aozora-epub3`,
  `aozora-rs`, `aozora2`, and `aozora`
- `SOURCE_AUTHORITY_GATE_PASS` over 17,894 works
- `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- `classified_but_not_admitted.count == 0`
- `true_unsupported_gaps.count == 0`

The report can still list raw unsupported-derived rows because the mapping
artifact preserves depth-specific divergence identities. Completion is judged by
the closure buckets after those rows are folded into TEI/profile/custom
families.

## Sequencing

1. **Keep source-authority evidence current**
   - Rerun the source-authority representability gate when the corpus, scanner,
     parser adapters, or notation-spec comparison changes.
   - Treat new unallowlisted source markers as mapping debt until reviewed.

2. **Keep coverage reporting synchronized**
   - Regenerate the IR publication coverage report whenever ABC schema/profile
     hashes, mapping hashes, or parser evidence changes.
   - Fail on any newly observed Aozora markup family without a TEI/profile/custom
     class.

3. **High-leverage schema/rendering deltas still worth tracking**
   - `gaiji.resolved` semantic alignment.
   - warigaki node or sidecar preservation path.
   - raw-source recovery path.
   - accent code measurement and rendition vocabulary.
   - quote/caption pass-through measurement if still blocked in any adapter.

4. **Five-parser evidence maintenance**
   - Keep `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`
     current where feasible.
   - Report parser-specific unsupported gaps separately from IR/schema gaps.

5. **Admission maintenance**
   - Claim full IR publication coverage only when all observed facts are in
     `tei_exact`, `tei_policy_projection`, `tei_plus_abc_extension`,
     `custom_sidecar`, or `plaintext_only`, and `unsupported_gap` is empty.

## Self-Review

- Placeholder scan: no TBD/TODO placeholders.
- Internal consistency: TEI remains primary publication XML; custom schema
  preserves non-TEI facts; plaintext remains metadata-free.
- Scope check: this is a publication contract design, not an implementation
  plan and not a parser-selection decision.
- Ambiguity check: TEI-EAJ is explicitly calibration evidence, not source
  authority or the headline goal.
