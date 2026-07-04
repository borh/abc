# TEI-EAJ Aozora Comparison Source

Status: Active handoff
Date: 2026-07-04

## Source

ABC treats `TEI-EAJ/jp_guidelines` as the authoritative Japanese TEI markup
style reference, and `TEI-EAJ/aozora_tei` as the canonical external comparison
corpus for Aozora-derived TEI examples.

Style authority:

- Wiki: `https://github.com/TEI-EAJ/jp_guidelines/wiki`
- Use this when making markup-style decisions for Japanese TEI.
- Cite the relevant guideline page in ADRs, ODD comments, handoffs, and paper
  notes when a decision depends on Japanese TEI convention rather than only on
  ABC pipeline mechanics.

Comparison corpus:

- Repository: `https://github.com/TEI-EAJ/aozora_tei`
- Pinned flake input: `tei-eaj-aozora-tei`
- Current pinned revision: `77a675fc2771936f9544505d922d4cd45075338c`
- Local pinned source path: `nix run .#tei-eaj-aozora-tei-source`
- Regenerate all comparison reports:
  `nix run .#tei-eaj-aozora-reports`
- Regenerate the focused Melos comparison report:
  `nix run .#tei-eaj-aozora-melos-report`
- Regenerate the all-work coverage/comparison report:
  `nix run .#tei-eaj-aozora-all-work-report`
- Current reports:
  - `docs/handoffs/tei-eaj-aozora-all-work-comparison-report.md`
  - `docs/handoffs/tei-eaj-aozora-melos-comparison-report.md`
- Melos comparison files:
  - `data/complete/tei_lib_lv4/1567_tei.xml`
  - `data/complete/tei_lib_lv4/1567_header_updated.xml`

The comparison source is pinned through Nix instead of vendored into this
repository. The `tei-eaj-aozora-comparison-source` flake check verifies that the
pinned checkout still contains the Melos Level 4 files. The
`tei-eaj-aozora-report-generation` flake check runs both report modes against
the pinned checkout using a temporary ABC counterpart fixture, so the report
tooling is automatically exercised even though `paper/` is intentionally
gitignored.

The pinned checkout currently contains 62 XML files overall. The all-work
report enumerates all 62 complete, draft, and etc XML files. In the current
local paper workset it discovers ABC TEI counterparts for work IDs `127` and
`1567`; only `1567` appears in TEI-EAJ, so the literal comparison still covers
the two Level 4 Melos files above. The other 60 rows are materialization
coverage backlog, not text mismatches. No draft Melos TEI file exists in the
current pinned revision. Draft material is still included in the all-work
coverage table.

## Level Vocabulary

ABC documentation and planning should use the TEI-EAJ `aozora_tei` Level 2-5
vocabulary when describing TEI depth.

| Level | TEI-EAJ framing | ABC planning interpretation |
|---|---|---|
| Level 2 | Close to Aozora text depth | Source-preserving transcription with core Aozora markup preserved. |
| Level 3 | Basic structural units such as paragraphs, tanka, and haiku | Parser-IR must preserve paragraph and source-note structure before ABC can claim Level 3 publication TEI. |
| Level 4 | Deeper convenience markup such as named entities | Curated enrichment: names, places, roles, references, and speech attribution. Not a parser compatibility requirement by default. |
| Level 5 | Scholarly edition or specialized corpus markup | Out of scope for generated ABC publication TEI unless a separate editorial/enrichment project is declared. |

## Current ABC Position

ABC's current parser-IR-derived TEI is between Level 2 and Level 3:

- It preserves body text and native TEI ruby well enough for publication
  validation.
- It does not yet preserve Melos paragraph segmentation.
- It currently emits the final Melos source attribution as an unmapped
  structure, which prevents a credible Level 3 claim.
- It does not attempt Level 4 entity or speech enrichment.

This is a contract issue, not only a renderer issue. Paragraphs and source
notes must be represented in parser-IR by the producer side before ABC can
render them without guessing.

## Comparison Policy

Use `aozora_tei` as:

- a citable prior-art comparison for the paper;
- a source of target examples for paragraph/source-note and enriched TEI
  discussions;
- a test-corpus candidate for future comparison probes;
- the default external corpus enumerated by the all-work report.

Do not use `aozora_tei` as:

- a runtime dependency for `nix run .#validate-design-bundle`;
- an authority that overrides ABC's content-addressed source and manifest
  identity;
- a reason to fold editorial Level 4 or Level 5 enrichment into parser-IR.

## Immediate Follow-Up

1. Update parser-IR schema planning so Level 3 completeness means paragraphs
   and source notes are producer-preserved values.
2. Generate ABC publication TEI for the remaining TEI-EAJ work IDs where the
   source work is available in the parser-IR workset; then rerun
   `nix run .#tei-eaj-aozora-reports` to convert missing-counterpart rows into
   concrete text/structure comparisons.
3. Add an `ab-validator` probe against Melos comparing ABC parser-IR structural
   output to the TEI-EAJ Level 4 Melos files at the paragraph/source-note level.
4. Keep Level 4 entity and speech attribution as a separate enrichment track
   with its own validation rules and declared editorial status.
