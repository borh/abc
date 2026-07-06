# Warigaki

Status: schema-needed

## Source Inventory

Warigaki is source-observed Aozora body markup and must not be treated as plain
body text.

## Parser-IR Representation

Parser-IR currently represents warigaki through measured conversion evidence
and extension policy rather than a final dedicated publication model.

## TEI P5 Target

Warigaki likely maps to TEI note or inline annotation policy. TEI references
should cite `../abc/references/TEI/P5/Source/Specs` before final admission.

## ABC Extension Or Sidecar

ABC extension or sidecar records preserve source-exact warigaki structure when
TEI projection loses the split-line source fact.

## Plaintext Projection

Plaintext includes visible body text only and excludes warigaki metadata.

## Current Evidence

Current conversion and coverage reports:
`docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`
and `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`.

## Open Decisions

Next action: decide the Parser-IR and TEI/custom target model for warigaki
after source and adapter evidence are reconciled.
