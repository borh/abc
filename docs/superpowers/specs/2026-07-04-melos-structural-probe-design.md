# Melos Structural Probe Design

Date: 2026-07-04
Status: accepted for implementation

## Problem

ABC now compares against the full pinned TEI-EAJ `aozora_tei` corpus, not only
the prior Melos spot check. The current local overlap still reduces to Melos
work `1567`, but the comparison exposes a Level 3 claim gap:

- ABC publication TEI currently has one body paragraph for Melos; TEI-EAJ has
  19 or 22 paragraphs depending on the TEI-EAJ file.
- ABC keeps the final source attribution
  `（古伝説と、シルレルの詩から。）` in body base text; TEI-EAJ excludes it from
  body base text.
- Parser-IR has no paragraph container node and no source-attribution/source-note
  node, so the current AAT-to-parser-IR conversion can only flatten paragraph
  content and record structural divergence.

This is not a parser consensus question. Parser outputs are supporting evidence;
the immediate question is what the existing adapter AATs preserve and what
parser-IR can represent without unmapped residuals.

## Goal

Add a measurement-only Melos structural probe that emits parser-IR evidence for:

1. paragraph segmentation preserved in AAT;
2. final source-attribution/source-note candidate classification;
3. whether parser-IR can represent those structures without residual divergence.

## Non-Goals

- Do not change the parser-IR schema.
- Do not add paragraph or source-attribution nodes in this slice.
- Do not perform TEI-EAJ XML comparison in ab-validator; ABC owns that report.
- Do not fold TEI-EAJ Level 4 enrichment (`persName`, `placeName`, `said`, etc.)
  into parser compatibility.
- Do not treat agreement among four parsers as source-authority proof.

## Inputs

The probe accepts one or more labelled AAT JSON files. Local operator runs should
use the current Melos AAT files when available:

- `scratch/morph-full-corpus/aats/aozora-rs-adapter/000035_1567-32ff5a089d67.json`
- `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/aat/aozora2html-adapter/000035_1567-32ff5a089d67.json`
- `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter/000035_1567-32ff5a089d67.json`

CI and flake checks must use synthetic fixture AAT only and must not read `/db`.

## Output Model

For each labelled input, the probe records:

- AAT metadata: `work_id`, `adapter`, `adapter_version`, `parse_complete`.
- AAT structural evidence: total block count, block-kind counts, paragraph block
  count, final block kind, normalized final visible text, final attribution
  candidate boolean, and AAT hints such as `unmapped-div` or adapter warnings.
- Parser-IR evidence: conversion success/failure, node-kind counts, whether a
  paragraph node exists, whether a source-attribution/source-note node exists.
- Divergence evidence: category counts and paragraph `STRUCTURAL` records from
  the measured mapping artifact.
- Verdict booleans: `paragraphs_represented_in_parser_ir`,
  `source_attribution_represented_in_parser_ir`, and `residual_free`.

The Markdown report is a human-facing summary. The JSON summary is the
machine-readable artifact for follow-up gates.

## Classification Rules

Paragraph representability:

- `paragraphs_represented_in_parser_ir = true` only if parser-IR contains an
  explicit paragraph-like node.
- The current schema has no such node, so a successful conversion with paragraph
  AAT blocks should produce `false` plus paragraph `STRUCTURAL` records.

Final source-attribution candidate:

- Normalize visible text by removing ASCII and Japanese whitespace.
- Candidate is true when the final non-empty block visible text starts with
  `（`, ends with `）`, and contains `から`.
- A candidate is represented in parser-IR only if parser-IR contains a
  source-attribution/source-note/editor-note style node for that text. A plain
  `text` or `emphasis` node is preservation of visible text, not classification.

Residual-free verdict:

- `residual_free = false` when paragraph blocks exist but parser-IR has no
  paragraph node, or a final attribution candidate exists but parser-IR has no
  source-attribution node, or conversion fails.
- This intentionally measures the Level 3 gap. It is not a failure of the
  adapters if their AAT preserved the source structure.

## Follow-Up

After this Melos probe is stable, expand the same source-authority workflow to
the 55 TEI-EAJ rows where ABC currently lacks materialized counterparts. ABC
now exports that workset at
`../abc/docs/handoffs/tei-eaj-aozora-workset-export.json` and regenerates it
with `nix run .#tei-eaj-aozora-workset-json`. That JSON, not the Markdown
report tables, should be the machine-readable input for the expansion.

ABC ADR 0002 also keeps the evidence boundary explicit: conversion
compatibility evidence is citable now, but parser selection remains open until
parser-candidate reports are cited by logical path/hash with exact identity and
caveats. The ab-validator expansion should therefore produce structural
evidence, not parser-selection claims.
