# Aozora Publication Completion Next Steps Design

Status: Draft for review
Date: 2026-07-06
Owner boundary: ab-validator owns source measurement, parser evidence,
adapter diagnostics, and admission reports. ABC owns TEI profile, publication
renderer behavior, preservation schemas, design-bundle validation, and final
publication policy.

## Problem

The current ab-validator gates are green for the current measured contracts, but
that does not mean the whole Aozora Bunko markup to publication mapping project
is finished.

The completed work proves:

- source authority is classified with zero unreviewed source-region rows;
- five parser lanes are visible in parser evidence;
- AAT-to-parser-IR conversion audit runs across the five parser lanes;
- parser-IR publication coverage has no current unsupported node/fact class;
- the full 285-row five-parser TEI-EAJ matrix materializes into internally
  consistent parser-IR, TEI, plaintext, preservation, manifest, and
  source-region bundles.

The remaining work is different: durable publication admission. We need to show
that every observed Aozora source construct has a stable publication
disposition:

- TEI P5 where TEI carries the fact faithfully;
- TEI plus ABC namespace/profile projection where TEI needs source-exact audit
  attributes;
- ABC custom sidecar where TEI should not carry the fact;
- diagnostic preservation for genuinely malformed source residues;
- explicit unsupported blocker only when the source construct cannot yet be
  represented.

TEI-EAJ is calibration evidence, not the goal. TEI-EAJ levels do not align
exactly with Aozora Bunko markup mapping, and TEI-EAJ Level 4 enrichment often
requires editorial or semantic knowledge outside source markup.

## Evidence Ledger

Current source-region report:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `works_scanned: 17894`
- `gate_status: SOURCE_AUTHORITY_GATE_PASS`
- `source_region_coverage.body_typed_occurrences: 4570071`
- `source_region_coverage.body_raw_preserved_occurrences: 46382`
- `source_region_coverage.source_apparatus_occurrences: 13920`
- `source_region_coverage.front_matter_occurrences: 14627`
- `source_region_coverage.back_matter_occurrences: 90274`
- `source_region_coverage.body_end_boundary_occurrences: 243`
- `source_region_coverage.terminal_provenance_occurrences: 609`
- `source_region_coverage.colophon_metadata_occurrences: 89416`
- `source_region_coverage.letter_address_origin_occurrences: 6`
- `source_region_coverage.malformed_source_occurrences: 16`
- `source_region_coverage.unsupported_body_markup_occurrences: 0`
- `source_region_coverage.unknown_region_occurrences: 0`
- `source_region_coverage.unknown_unreviewed_occurrences: 0`

Current publication coverage report:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- `verdict: IR_PUBLICATION_COVERAGE_COMPLETE`
- source-region contract confirmed against ABC schema/policy snapshots;
- custom preservation contract confirmed;
- TEI profile contract confirmed;
- publication bundle contract confirmed;
- bundle evidence now cites the full matrix validation with `rows_validated:
  285` and `rows_failed: 0`.

Current full-matrix TEI-EAJ calibration:

- `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`
- `rows_attempted: 285`
- `tei_eaj_rows_attempted: 57`
- `materialization_failed: 0`
- `rows_skipped: 5`
- `tei_eaj_rows_attempted` counts TEI-EAJ source rows. `rows_attempted`
  expands those rows across the five parser lanes.
- paragraph origin buckets:
  - `adapter_over_segmented: 127`
  - `adapter_collapsed: 67`
  - `aligned: 60`
  - `adapter_under_segmented: 14`
  - `converter_paragraph_mismatch: 8`
  - `source_note_back_routing: 4`
  - `page_break_projection: 3`
  - `adapter_raw_only: 2`
- body text relation buckets:
  - `different: 271`
  - `equal: 7`
  - `generated_contains_tei_eaj: 6`
  - `tei_eaj_contains_generated: 1`
- TEI-EAJ profile rows include `lineated_text`, `plain_prose`, `drama`,
  `notes`, `front_back_matter`, `verse`, and `lv4_enrichment`.

Current five-parser AAT-to-parser-IR conversion audit:

- `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`
- `files_attempted: 89169`
- `files_succeeded: 89169`
- `files_failed: 0`
- `parser_ir_nodes: 33359611`
- `divergence_records: 1032872`
- adapters present: `aozora`, `aozora-epub3`, `aozora-rs`, `aozora2`,
  `aozora2html`.

Current source-reference reconciliation:

- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
- `verdict: SOURCE_REFERENCE_RECONCILIATION_COMPLETE`
- `observed_without_syntax_row: 0`
- `p4suta_feature_unmapped: 0`

Current performance report:

- `docs/superpowers/reports/2026-07-04-parser-performance-measurement.md`
- Comparison budgets are explicit.
- DNF is useful signal.
- `aozora2html` is comparator evidence, not an optimization target.
- Parser performance should be measured across all parser lanes when parser
  changes affect evidence.

## Design Direction

The next phase should not start with another parser rewrite or another TEI-EAJ
parity run. It should start with an admission ledger that reports, in one
machine-readable place, which measured Aozora facts are already admitted and
which next action owns the remaining evidence or policy work.

The project should use four gates:

1. **Source authority gate**
   - Every source-observed Aozora occurrence is typed, raw-preserved,
     source-apparatus, front/back, diagnostic, or explicitly unsupported.
   - Unknown and unreviewed rows block completion.

2. **Publication representation gate**
   - Every admitted source or parser-IR fact has one target lane:
     `tei_exact`, `tei_policy_projection`, `tei_plus_abc_extension`,
     `custom_sidecar`, `diagnostic`, `body_visible_text_only`, or
     `unsupported_gap`.
   - `body_visible_text_only` is a disposition for ordinary visible body text,
     not permission for front/back/source apparatus to appear in plaintext.

3. **Cross-artifact bundle gate**
   - Parser-IR, TEI, plaintext, preservation, source-region evidence, and
     manifests agree.
   - This gate is currently green for 285 matrix rows.
   - This gate is independent of the publication representation gate. It can
     be green while the next-work/admission ledger remains open.

4. **Calibration/fidelity gate**
   - TEI-EAJ comparison and five-parser evidence are used to find adapter,
     parser-IR, and policy gaps.
   - TEI-EAJ editorial enrichment is explicitly out of scope unless the source
     markup contains the fact.

## Candidate Approaches Considered

### Approach A: Build a new Rust parser now

This is attractive because no existing parser is complete enough to be the final
publication authority. It is premature as the next step because we would still
need the same admission ledger, TEI/custom policy, and source-region
disposition to know whether the new parser is complete.

Use this approach later, after the parser acceptance criteria are derived from
the source inventory and TEI/custom mapping dossiers.

### Approach B: Continue chasing TEI-EAJ parity rows

This finds real adapter and policy bugs, but it entangles the project goal with
TEI-EAJ editorial choices. It can push the team toward requiring parser output
for people, places, speakers, roles, and speech acts that are not fully present
in Aozora source markup.

Keep TEI-EAJ as calibration, not the admission authority.

### Approach C: Build an admission-ledger first, then close slices by owner

This is the recommended approach.

It keeps source authority separate from parser evidence, separates TEI P5
mapping from ABC extension/custom sidecar facts, and gives each follow-up a
measured blocker to close. It also gives a new Rust parser a concrete future
acceptance target instead of a vague "better parser" mandate.

## Required Next Workstreams

### 1. Publication Admission Next-Work Ledger

Create a report that joins the current source-region, IR coverage,
TEI-EAJ matrix, full-corpus conversion, source-reference, and performance
reports.

The report should answer:

- Which gates are already complete?
- Which facts are admitted by TEI, ABC extension, custom sidecar, diagnostic, or
  plaintext policy?
- Which remaining items are source-region disposition work, text-policy work,
  adapter fidelity work, parser-IR schema work, ABC renderer/profile work, or
  future-parser acceptance work?
- Which evidence is calibration-only?

This report becomes the single next-step dashboard for the overall goal.

### 2. Source-Region Disposition And Front/Back Policy

The source-region counters are measured and ABC has an initial policy snapshot.
The next step is sample-backed disposition evidence for:

- notation legends;
- notation placeholders;
- body-end boundaries;
- terminal provenance;
- colophon metadata;
- malformed source diagnostics;
- letter address/origin rows such as `宛先` and `発信地`.

The source-region policy file admits `letter_address_origin` as a source class
with TEI target `teiHeader/profileDesc/correspDesc`. Current reports measure
those rows with `source_region_coverage.letter_address_origin_occurrences: 6`.
Treat them as admitted source-region policy rows, not missing evidence.

The output must distinguish source facts from publication decisions:

- ab-validator says region and apparatus kind;
- ABC decides TEI header/front/back/custom sidecar placement;
- plaintext remains visible body text only.

### 3. Text-Policy Calibration

The TEI-EAJ matrix has 271 rows with `body_base_text_relation == "different"`.
That bucket is too coarse. Split it into measured subcauses:

- ruby reading/base differences;
- parenthetical source-note policy;
- iteration mark normalization;
- front/back metadata inclusion/exclusion;
- body-visible layout text such as TCY;
- adapter text loss;
- TEI-EAJ editorial differences outside Aozora source markup.

Only the source-markup-backed subcauses should become blockers for the Aozora
publication mapping goal.

### 4. Adapter Fidelity And Workset Triage

Parser evidence remains supporting evidence. Adapter work should target
publication accounting distortions, not generic parser improvement.

Priority diagnostics:

- paragraph over-segmentation and collapse;
- converter paragraph mismatch;
- adapter raw-only rows;
- source-note/back-matter routing;
- quote, caption, and layout marker association;
- raw-source recovery where adapter output is contaminated by coordinate-space
  mismatch.

Each adapter-fidelity issue should produce a workset and a measured before/after
report. Performance budgets stay bounded; DNF is recorded, not hidden.

### 5. TEI P5 Mapping Dossiers

Create one mapping dossier per Aozora feature family. Each dossier should cite
TEI P5 where relevant from the flake-backed `.#tei-p5-reference` tree, the
local source inventory, current parser-IR representation, ABC policy/sidecar
target, and plaintext projection.

Initial families:

- ruby;
- gaiji;
- page and line breaks;
- paragraphs and headings;
- indentation and layout spans;
- emphasis and style;
- images, figures, and captions;
- warigaki;
- kunten/okurigana/kaeriten;
- source apparatus and front/back matter;
- malformed source diagnostics.

### 6. Parser Acceptance Criteria For The Eventual Rust Parser

The eventual comprehensive parser should be accepted against measured
publication requirements, not parser consensus.

Acceptance criteria should include:

- source inventory coverage for all observed source rows;
- parser-IR emission for every TEI/custom-mapped feature family;
- zero unreviewed source-region rows;
- zero unsupported body markup rows;
- bundle validation over representative and full scopes;
- bounded performance budgets with DNF reporting;
- explicit non-goal for editorial/semantic enrichment not in source markup.

## Completion Definition

The overall Aozora publication mapping goal is complete when all of the
following are true for the chosen measured corpus scope:

- source authority passes with zero unsupported body markup, zero unknown
  region rows, and zero unreviewed rows;
- every observed source family has a disposition in TEI P5, TEI+ABC extension,
  custom sidecar, diagnostic preservation, plaintext-only, or explicit
  unsupported blocker;
- all explicit unsupported blockers are zero or accepted as out-of-scope by a
  reviewed decision record;
- parser-IR publication coverage is complete against synced ABC schemas and
  profile hashes;
- full-scope bundle validation passes for parser-IR, TEI, plaintext,
  preservation, manifests, and source-region evidence;
- TEI-EAJ calibration gaps are either closed, classified as adapter/policy work,
  or marked editorial/enrichment out of scope;
- five-parser evidence and performance reports are current enough to explain
  adapter choices and guide the eventual parser;
- plaintext contains no ruby readings, source apparatus, source notes, layout
  metadata, custom records, warnings, or provenance.

## Non-Goals

- Do not make TEI-EAJ Level 2/3/4 labels the success metric.
- Do not require Aozora source markup to supply TEI Level 4 editorial semantics
  such as `persName`, `placeName`, `roleName`, or speech-act structure.
- Do not optimize comparator parsers beyond what is needed to produce bounded
  evidence.
- Do not make parser consensus override source inventory.
- Do not push all difficult facts into a custom sidecar without schema,
  pointers, hashes, and validation.

## Risks And Checks

| Risk | Control |
|---|---|
| Green bundle evidence is mistaken for complete publication admission. | The next-work ledger separates contract-green, source-region policy, text-policy, adapter fidelity, and calibration-only states. |
| TEI-EAJ editorial enrichment becomes parser-required. | Mapping dossiers explicitly classify source-backed vs editorial/enrichment facts. |
| New parser work starts before target is stable. | Parser acceptance criteria are derived after admission ledger and mapping dossiers. |
| ABC and ab-validator drift on schema/profile hashes. | Every report cites path/hash and the admission ledger checks synced snapshots. |
| Plaintext leaks metadata. | Bundle validation keeps body-only projection, with visible layout text included and layout metadata omitted. |

## Self-Review

- Placeholder scan: no placeholder sections are present.
- Internal consistency: source authority, parser evidence, TEI/custom mapping,
  and TEI-EAJ calibration are separate gates.
- Scope check: the spec is a next-phase design; implementation must be split
  into focused plans.
- Ambiguity check: TEI-EAJ is calibration evidence, not the success definition.
