# Emphasis And Style

Status: admitted

## Source Inventory

Emphasis and style markers are source-observed body markup.

## Parser-IR Representation

Parser-IR carries emphasis nodes and style projection evidence.

## TEI P5 Target

Use TEI highlighting and rendition policy. Current flake-backed TEI citations:
`$TEI_P5_ROOT/Source/Specs/hi.xml`,
`$TEI_P5_ROOT/Source/Specs/rendition.xml`, and
`$TEI_P5_ROOT/Source/Specs/att.global.rendition.xml`.

## ABC Extension Or Sidecar

ABC source-marker attributes may be needed when source marker and TEI rendition
do not name the same thing.

## Plaintext Projection

Plaintext includes only visible body text and excludes emphasis metadata.

## Current Evidence

Style coverage is included in
`docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`.

## Open Decisions

Next action: keep source-marker audit attributes synchronized with sidecar
records when ABC namespace policy changes.
