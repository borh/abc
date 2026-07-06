# Source Region Coverage Handoff From ab-validator

Date: 2026-07-06
Source repo: `/home/bor/Projects/ab-validator`
Source commit: `b93a1c6 docs: plan aozora publication contract integration`

## Purpose

ab-validator now reports source-region coverage as the source-authoritative
input for Aozora Bunko publication accounting. ABC should consume this contract
when deciding TEI header/front/back placement, preservation sidecar records,
diagnostics, and plaintext exclusion.

This is not a TEI-EAJ level gate. The shared goal is complete Aozora Bunko
markup publication mapping: TEI P5 where faithful, ABC TEI/profile extension
or custom sidecar where TEI is not exact, diagnostics for genuinely invalid
source residue, and plaintext as visible body text only.

## ab-validator Artifacts

Handoff and roadmap:

- `docs/handoffs/source-region-coverage-abc-integration.md`
- `docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md`

Canonical contract:

- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`

Reports:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

## Current Measurement

Source authority:

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

Legacy compatibility counters remain present in ab-validator:

- `typed_occurrences: 4570071`
- `raw_preserved_occurrences: 46382`
- `out_of_body_occurrences: 950`
- `malformed_noise_occurrences: 13936`
- `unsupported_occurrences: 0`
- `needs_research_occurrences: 0`

Important invariants:

- `malformed_noise_occurrences == source_apparatus_occurrences + malformed_source_occurrences`
- `unsupported_occurrences == unsupported_body_markup_occurrences`
- `out_of_body_occurrences` is a legacy non-body marker counter, not the final
  front/back accounting model.

Current publication coverage:

- `IR_PUBLICATION_COVERAGE_COMPLETE`
- five parser evidence present:
  `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, `aozora`
- `classified_but_not_admitted.count == 0`
- `true_unsupported_gaps.count == 0`

## Source Classes Needing ABC Policy

| Source class | Current evidence | ABC decision needed |
|---|---:|---|
| `notation_legend` | 13,920 `CommandFullwidth` `［＃］` occurrences | TEI `encodingDesc`/`editorialDecl`, custom preservation record, or both. |
| `notation_placeholder` | 465 `［＃…］` and 242 `［＃（…）］` occurrences | TEI documentation, preservation records, or both. |
| `body_end_boundary` | 243 `［＃本文終わり］` occurrences | Source region boundary evidence; do not render as body text. |
| `terminal_provenance` / `colophon_metadata` | Not separately measured in current ab-validator aggregate. Current `back_matter_occurrences: 243` is exhausted by `body_end_boundary`. | Add fixtures/policy and request a later measurement split before making separate prevalence claims. Decide TEI back/header/source-note/custom placement. |
| `malformed_source` | 16 total residues | Preserve as diagnostics; do not count as unsupported Aozora syntax. |

Malformed-source residue split:

- `MalformedCommand` raw `［＃`: 8
- `MalformedAccentNotation` raw `〔`: 6
- `MalformedGaiji` raw `※［＃`: 1
- `MalformedImplicitRuby` raw `《`: 1

## Requested ABC Work

1. Accept `aozora-source-region-coverage-v1` in design-bundle validation.
2. Require `SOURCE_AUTHORITY_GATE_PASS` for full Aozora markup publication
   admission.
3. Require:
   - `unsupported_body_markup_occurrences == 0`
   - `unknown_region_occurrences == 0`
   - `unknown_unreviewed_occurrences == 0`
4. Add a fixture with:
   - source apparatus in front matter
   - a body-end boundary
   - terminal provenance or colophon/back matter
   - body-visible text
   - plaintext expected output with metadata omitted
5. Decide and encode disposition policy for each source class:
   TEI, TEI plus ABC extension, custom sidecar, diagnostic, or explicit
   unsupported blocker.
6. Add cross-artifact validation for parser-IR, TEI XML, preservation sidecar,
   source-region evidence, and plaintext.
7. Ensure TEI `abc:*` projections, if emitted, resolve to preservation sidecar
   records rather than becoming independent facts.

## Cross-Artifact Validation Target

The design bundle should prove these relationships:

| Artifact | Required agreement |
|---|---|
| Parser-IR | validates against parser-IR schema id/hash and agrees with preservation sidecar pointers |
| TEI XML | validates against ABC TEI profile and any admitted Schematron rules |
| Preservation sidecar | validates against ABC schema and links parser-IR, TEI, and source evidence |
| Source-region report | supplies source-authority gate, region counts, apparatus counts, and unknown counters |
| Plaintext | contains visible body text only |

Plaintext must not include ruby readings, source apparatus, source notes routed
to front/back, layout metadata, custom records, warnings, or provenance.

## Response Needed Back To ab-validator

After ABC lands the policy/validation work, send ab-validator:

- ABC commit hash
- schema/profile hashes
- fixture paths
- validation command output
- any new source-apparatus disposition vocabulary
- whether terminal provenance and colophon metadata need a new ab-validator
  measurement split

ab-validator will then sync ABC schema/profile snapshots, regenerate:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

Then assert that the full mapping goal still reports complete coverage.
