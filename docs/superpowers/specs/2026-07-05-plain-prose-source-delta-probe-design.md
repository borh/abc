# Plain Prose Source Delta Probe Design

Status: approved concept, awaiting written-spec review
Date: 2026-07-05

## Problem

The current Level 3 TEI admission report proves two useful facts:

- Source authority is clean: the source inventory scanned 17,894 works and has zero unallowlisted unknown markers.
- Parser-IR infrastructure is ready: the current AAT to Parser-IR mapping, conversion, and generated TEI matrix can run without infrastructure blockers.

It also shows that plain-prose Level 3 admission is still blocked. The current plain-prose lane has 81 rows, 0 passing rows, and the verdict
`LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY`.

That verdict is correct but too coarse for the next engineering step. It does not tell us whether a row is blocked because:

- an adapter failed to preserve paragraph structure,
- Parser-IR projection lost structure that existed in AAT,
- ABC rendering mishandled correct Parser-IR,
- TEI-EAJ uses editorial segmentation that should become policy,
- ruby metadata is being compared as plaintext,
- source attribution belongs in typed metadata rather than body text, or
- a required parser has no materialized evidence.

The immediate need is a source-authoritative delta probe that separates those causes.

## Decision

Build a diagnostic probe for the plain-prose TEI-EAJ comparison lane. The probe produces a JSON summary and Markdown report that classify every plain-prose row by evidence source and blocker owner.

The probe does not admit Level 3. It produces the evidence needed to choose the next implementation slice.

The required parser evidence set is exactly five adapters:

```json
[
  "aozora2html",
  "aozora-epub3",
  "aozora-rs",
  "aozora2",
  "aozora"
]
```

A report that lacks rows for any required adapter must say so explicitly. It must not present itself as a five-parser conclusion.

## Current Evidence Baseline

The committed matrix-generation recipe already passes all five adapter AAT roots to the structural expansion stage:

- `aozora-rs`
- `aozora2`
- `aozora2html`
- `aozora-epub3`
- `aozora`

The current generated matrix summary is not five-parser complete. Its row counts are:

- `aozora2html`: 57 rows
- `aozora-epub3`: 57 rows
- `aozora-rs`: 57 rows
- `aozora2`: 2 rows
- `aozora`: 0 rows

Therefore the current admission report is valid as a partial diagnostic result, but not as a five-parser evidence claim.

## Scope

In scope:

- Classify plain-prose Level 3 comparison rows from the TEI-EAJ generated matrix.
- Require five-parser coverage as an evidence condition.
- Distinguish adapter, Parser-IR, ABC renderer, policy, and evidence owners.
- Treat source inventory as authority for Aozora source markup.
- Treat parser and adapter outputs as supporting evidence.
- Preserve the plaintext rule: plaintext contains body base text only. Ruby, notes, source attribution, and other metadata are not plaintext.

Out of scope:

- Selecting the final parser.
- Changing Parser-IR schema.
- Implementing the full Aozora Bunko to TEI P5 mapping.
- Claiming TEI levels 4 or 5 semantic enrichment from source markup alone.
- Treating TEI-EAJ editorial enrichment such as people, places, roles, or speech annotation as parser compatibility evidence.

This design supports the broader goal that all Aozora Bunko markup should be mappable to legal TEI P5 constructs. It does not claim that Aozora Bunko markup can supply semantic annotation that requires human or model interpretation.

## Inputs

The implementation should consume existing artifacts first, rather than rerunning parsers inside the diagnostic probe.

Required inputs:

- `docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json`
- `docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`
- `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `data/aat-to-parser-ir-mapping-v1.json`

The probe may also read per-work AAT and generated Parser-IR artifacts when a richer explanation is available locally. Those reads are optional diagnostics, not a condition for the fixture smoke test.

## Output Contract

The JSON summary must use this top-level shape:

```json
{
  "schema_version": "plain-prose-source-delta-probe-v1",
  "required_parsers": [
    "aozora2html",
    "aozora-epub3",
    "aozora-rs",
    "aozora2",
    "aozora"
  ],
  "parser_evidence_coverage": {
    "verdict": "FIVE_PARSER_EVIDENCE_INCOMPLETE",
    "observed_rows_by_parser": {},
    "missing_parsers": [],
    "rows_missing_required_parser_evidence": 0
  },
  "source_authority_gate": {},
  "mapping": {},
  "plain_prose_scope": {
    "rows_total": 0,
    "works_total": 0
  },
  "classification_counts": {},
  "blocking_owners": [],
  "rows": []
}
```

The `mapping` object must include at least:

- `mapping_id`
- `mapping_version`
- `mapping_hash`
- `mapping_schema_hash`
- `target_parser_ir_schema_hash`
- `generated_mapping_rules`

The report must fail rather than emit an empty `mapping` object when those fields cannot be read from the admission summary or mapping artifact.

Each row entry must include:

- `work_id`
- `tei_eaj_file`
- `adapter`
- `paragraph_origin_bucket`
- `body_text_match_bucket`
- `classifications`
- `blocking_owners`
- `evidence`

`classifications` and `blocking_owners` are arrays. A row can have multiple independent blockers; the probe must not collapse them into a single precedence-ordered verdict.

## Classifications

### `missing_parser_evidence`

The row, work, or report lacks evidence for at least one required parser. This is an evidence blocker and prevents five-parser claims.

### `adapter_paragraph_bug`

The generated matrix reports `adapter_over_segmented` or `adapter_collapsed`, and there is no stronger evidence that TEI-EAJ editorial segmentation is the intended explanation.

Owner: `adapter`.

### `parser_ir_projection_bug`

The adapter AAT contains typed paragraph or source-note evidence, but the converted Parser-IR does not preserve it in paragraphs or source-note nodes.

Owner: `parser_ir`.

### `abc_renderer_bug`

Parser-IR contains the correct paragraph ranges or source-note nodes, but ABC-generated TEI or plaintext places them incorrectly.

Owner: `abc_renderer`.

### `source_note_metadata_excluded`

The difference is attributable to source attribution or source-note material that should be represented as metadata or back matter, not body plaintext.

Owner: `policy` unless Parser-IR or ABC rendering evidence shows a projection or renderer bug.

### `ruby_metadata_not_plaintext`

The row only matches on a ruby-expanded diagnostic surface. Plaintext must not include ruby readings or other metadata, so this is not a plaintext pass.

Owner: `policy` unless ABC body-base text is actually including ruby metadata.

### `source_text_policy_required`

The row has a text mismatch after excluding metadata, and the mismatch is not explained by a more specific class.

Owner: `policy`.

### `tei_eaj_editorial_segmentation`

The source and parser evidence agree, but TEI-EAJ chooses a different paragraph segmentation for editorial reasons. This needs a profile policy before admission.

Owner: `policy`.

### `evidence_gap`

The row cannot be classified because materialized AAT, Parser-IR, generated TEI, source inventory, or TEI-EAJ data is absent.

Owner: `evidence`.

## Coverage And Gate Semantics

The probe has two modes:

- Default mode: fail the smoke or full report if the required parser list is not exactly the five labels named in this spec.
- Exploratory mode: allow missing parser evidence but set `parser_evidence_coverage.verdict` to `FIVE_PARSER_EVIDENCE_INCOMPLETE`.

The committed full report should use exploratory mode until `aozora2` and `aozora` have complete generated matrix coverage. Its headline must make the incomplete parser evidence visible.

The report can only use `FIVE_PARSER_EVIDENCE_COMPLETE` when every in-scope plain-prose TEI-EAJ row has evidence for all five required parsers, or when a missing row is represented by an explicit evidence-gap record.

## Testing Requirements

Add a fixture smoke test that does not read `/db` and does not use the network. The fixture must include:

- all five parser labels,
- one row with missing parser evidence,
- one row where ruby-expanded text matches but plaintext does not,
- one row where source-note material should be excluded from body plaintext,
- one row with adapter paragraph over-segmentation,
- one row with aligned paragraphs but a text-policy blocker.

The smoke must assert:

- `required_parsers | length == 5`,
- `required_parsers` equals the five labels in this spec,
- `mapping.mapping_id` is non-null,
- missing parser evidence is reported,
- ruby-expanded equality is not counted as a plaintext pass,
- source-note metadata is classified separately from generic text mismatch,
- rows can carry multiple classifications and multiple owners.

## Risks

The biggest risk is treating the current partial matrix as a complete parser comparison. The five-parser coverage field is designed to prevent that.

The second risk is turning policy differences into adapter bugs. The report must preserve multiple labels per row so that adapter and policy work can proceed independently.

The third risk is comparing metadata as plaintext. The probe must preserve the rule that plaintext excludes ruby and other typed metadata.

## Implementation Shape

The likely implementation is a small Python report generator under `reports/parser-ir/`, a shell fixture smoke under `tests/`, and a `just` target plus flake check. The implementation plan should decide exact filenames and task boundaries.

No Rust schema or converter changes are required for the probe itself.

## Self-Review

- Unresolved-token scan: no open-ended sections remain.
- Internal consistency: the required parser set is five labels everywhere.
- Scope check: this is one diagnostic probe, not a parser selection or TEI P5 mapping implementation.
- Ambiguity check: plaintext, missing-parser evidence, and multi-owner classification semantics are explicit.
