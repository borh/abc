# Source Region Coverage ABC Integration Handoff

Date: 2026-07-06
Source repo: ab-validator
Audience: ABC implementor

Update: ABC commit `95ace31 feat(parser-ir): validate source-region coverage`
implements the initial source-region schema, policy, manifest sidecar role, and
design-bundle validation. ab-validator has synced the schema/policy snapshots
and now requires
`source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`
in the IR publication coverage report.

## Purpose

ab-validator now separates source-body markup, front/back matter, source
apparatus, and genuinely malformed source residues. ABC should consume this as
the source-authoritative input for publication placement and preservation
policy.

This is not a TEI-EAJ level gate. The goal is complete Aozora Bunko markup
publication accounting: TEI P5 where faithful, TEI plus ABC extension or custom
sidecar where TEI is not exact, diagnostics for genuinely invalid source
residue, and plaintext as visible body text only.

## Current Evidence

Current report:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`

Canonical vocabulary:

- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`

Current measured state:

- `schema_version: "aozora-source-region-coverage-v1"`
- `gate_status: "SOURCE_AUTHORITY_GATE_PASS"`
- `works_scanned: 17894`
- `unknown_markers_total: 14886`
- `unallowlisted_unknown_markers_total: 0`
- `body_typed_occurrences: 4570071`
- `body_raw_preserved_occurrences: 46382`
- `source_apparatus_occurrences: 13920`
- `front_matter_occurrences: 14627`
- `back_matter_occurrences: 243`
- `malformed_source_occurrences: 16`
- `unsupported_body_markup_occurrences: 0`
- `unknown_region_occurrences: 0`
- `unknown_unreviewed_occurrences: 0`

The old counters remain for compatibility:

- `typed_occurrences: 4570071`
- `raw_preserved_occurrences: 46382`
- `out_of_body_occurrences: 950`
- `malformed_noise_occurrences: 13936`
- `unsupported_occurrences: 0`
- `needs_research_occurrences: 0`

Required compatibility invariant for the current migration window:

- `malformed_noise_occurrences == source_apparatus_occurrences + malformed_source_occurrences`
- `unsupported_occurrences == unsupported_body_markup_occurrences`
- legacy fields remain readable until ABC migrates to `source_region_coverage`

The `out_of_body_occurrences` field is legacy evidence for allowlisted
non-body markers, not the terminal front/back accounting model. In the current
report it is the sum of notation-placeholder rows (`465 + 242`) and the
body-end-boundary row (`243`). ABC should use `source_region_coverage` for new
policy decisions, and should not infer separate terminal-provenance or colophon
prevalence from `out_of_body_occurrences`.

## Source Region Contract

ab-validator-owned source facts:

- `document_region`
  - `front_matter`
  - `body`
  - `back_matter`
  - `unknown_region`
- `apparatus_kind`
  - `notation_legend`
  - `notation_placeholder`
  - `body_end_boundary`
  - `terminal_provenance`
  - `colophon_metadata`
  - `malformed_source`

ABC-owned publication facts:

- TEI target, if any
- custom-sidecar target, if any
- parser-IR node or preservation record shape
- renderer placement policy
- plaintext omission policy

The owner boundary is intentional: ab-validator measures where and what the
source occurrence is; ABC decides how that source fact is represented in TEI,
custom preservation, headers, front/back matter, or diagnostics.

## Classes Requiring ABC Disposition

| Source class | Evidence | ABC disposition needed |
|---|---:|---|
| `notation_legend` | 13,920 `CommandFullwidth` `［＃］` occurrences | Decide TEI `encodingDesc`/`editorialDecl` vs custom preservation record. |
| `notation_placeholder` | 465 `［＃…］` and 242 `［＃（…）］` occurrences | Decide whether placeholders are TEI documentation, preservation records, or both. |
| `body_end_boundary` | 243 `［＃本文終わり］` occurrences | Treat as source region boundary evidence; do not render as body text. |
| `terminal_provenance` / `colophon_metadata` | Not separately measured in the current source-region aggregate. The current `back_matter_occurrences: 243` is exhausted by the `body_end_boundary` row. | Add ABC fixtures/policy and, if needed, a later ab-validator measurement split before claiming separate prevalence. Decide TEI back/header/source-note/custom placement. |
| `malformed_source` | 16 total residues | Preserve as diagnostics; do not count as unsupported Aozora syntax. |

The 16 malformed-source residues are:

- `MalformedCommand` raw `［＃`: 8
- `MalformedAccentNotation` raw `〔`: 6
- `MalformedGaiji` raw `※［＃`: 1
- `MalformedImplicitRuby` raw `《`: 1

## ABC Validation Asks

ABC should add or update validation so the design bundle can prove:

1. The source-region report schema version is accepted:
   `aozora-source-region-coverage-v1`.
2. `SOURCE_AUTHORITY_GATE_PASS` is required for full Aozora markup publication
   admission.
3. `unsupported_body_markup_occurrences == 0`.
4. `unknown_region_occurrences == 0`.
5. `unknown_unreviewed_occurrences == 0`.
6. Each source-apparatus class has an admitted disposition:
   TEI, TEI plus ABC extension, custom sidecar, diagnostic, or explicit
   unsupported blocker.
7. Plaintext remains visible body text only; ruby readings, source apparatus,
   front/back matter, layout metadata, custom records, warnings, and provenance
   are excluded.
8. TEI and custom preservation outputs can be joined back to source evidence:
   every ABC preservation record that claims source-region provenance must point
   to either a source-region class, source-inventory row, or source pointer.

## Cross-Artifact Contract

The full publication bundle should validate these relationships:

| Artifact | Must carry | Must agree with |
|---|---|---|
| Parser-IR | parser-IR schema id/hash, nodes, paragraphs, source notes, warnings/errors | mapping and preservation sidecar |
| TEI XML | TEI profile id/hash and any `abc:*` attributes admitted by ABC | preservation records and plaintext policy |
| Preservation sidecar | ABC schema id/hash, record ids, IR pointers, TEI pointers, source pointers/classes | parser-IR, TEI, source-region report |
| Source-region report | source-authority gate, region counts, apparatus counts, unknown counters | design-bundle admission gate |
| Plaintext | visible body text only | parser-IR body text projection |

The sidecar should remain the authoritative machine ledger for exact facts that
TEI does not carry. Inline TEI `abc:*` attributes, if present, are projections
that must resolve to sidecar records rather than independent facts.

## Suggested ABC Work Order

1. Accept `aozora-source-region-coverage-v1` in the design-bundle validator.
2. Add a fixture with source apparatus in front matter, a body-end boundary, and
   terminal provenance/back matter.
3. Decide the ABC disposition table for the source classes above, with a
   separate measurement/fixture path for terminal provenance and colophon
   metadata because current source-region counters do not isolate them.
4. Add preservation records or TEI profile rules for each admitted disposition.
5. Add cross-file validation for parser-IR, TEI, preservation sidecar,
   source-region report, and plaintext.
6. Regenerate ABC reports and send ab-validator:
   - commit hash
   - schema/profile hashes
   - fixture paths
   - validation command output

## ab-validator Follow-Up After ABC Lands

Implemented for ABC commit `95ace31`:

- Synced ABC artifacts into `data/abc-schemas/`:
  - `schemas/source-region-coverage.schema.json`
  - `schemas/manifest.schema.json`
  - `data/source-region-publication-policy-v0.json`
- Regenerated IR publication coverage:
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  - `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- Asserted the full goal still has:
   - `IR_PUBLICATION_COVERAGE_COMPLETE`
   - `SOURCE_AUTHORITY_GATE_PASS`
   - five parser evidence present
   - zero true unsupported gaps
   - zero unreviewed source-region rows

Remaining follow-up:

1. Split `terminal_provenance` and `colophon_metadata` measurement; current
   `back_matter_occurrences: 243` is still exhausted by body-end-boundary
   evidence.
2. Add bundle validation that joins source-region evidence to parser-IR, TEI,
   preservation records, manifests, and plaintext.
