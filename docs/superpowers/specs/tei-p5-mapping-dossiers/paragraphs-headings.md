# Paragraphs And Headings

Status: adapter-fidelity-needed

## Source Inventory

Paragraph and heading structure is observed through parser evidence and
TEI-EAJ calibration, not as a single source-marker counter.

## Parser-IR Representation

Parser-IR carries a flat node list plus paragraph ranges and heading nodes.

## TEI P5 Target

Use TEI paragraphs and heading structures. Current local TEI citations:
`../abc/references/TEI/P5/Source/Specs/p.xml`,
`../abc/references/TEI/P5/Source/Specs/head.xml`, and
`../abc/references/TEI/P5/Source/Specs/div.xml`.

## ABC Extension Or Sidecar

Paragraph node ranges and source-derived classification are preserved through
the ABC publication preservation contract when needed for audit.

## Plaintext Projection

Plaintext preserves visible body text in paragraph order but excludes heading
metadata beyond visible text.

## Current Evidence

Adapter worksets are in
`docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json`.
The TEI-EAJ matrix is
`docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`.

## Open Decisions

Next action: inspect each adapter-fidelity workset and decide whether the
blocker is adapter parser behavior, converter paragraph construction, or ABC
rendering policy.
