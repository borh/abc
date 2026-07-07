# Kunten

Status: schema-needed

## Source Inventory

Kunten includes kaeriten and okurigana-style kanbun annotation constructs.

## Parser-IR Representation

Evidence status: not yet measured in this dossier. Current conversion reports
carry parser evidence, but this dossier must still isolate kunten-specific rows.

## TEI P5 Target

TEI target selection needs a final policy decision. Candidate flake-backed TEI
citations to evaluate are
`$TEI_P5_ROOT/Source/Specs/ruby.xml`,
`$TEI_P5_ROOT/Source/Specs/gloss.xml`,
`$TEI_P5_ROOT/Source/Specs/note.xml`,
`$TEI_P5_ROOT/Source/Specs/seg.xml`, and
`$TEI_P5_ROOT/Source/Specs/span.xml`.

## ABC Extension Or Sidecar

If TEI cannot carry source-exact kunten placement, preserve the source fact in
ABC extension attributes or sidecar records.

## Plaintext Projection

Plaintext excludes kunten metadata.

## Current Evidence

Current source and conversion reports are
`docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
and `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`.

## Open Decisions

Next action: build a kunten-specific measured row set and then decide TEI,
ABC-extension, or sidecar target lane.
