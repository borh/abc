# Ruby

Status: admitted

## Source Inventory

Ruby is source-observed as Aozora ruby notation and counted by the source
authority reports.

## Parser-IR Representation

Parser-IR carries ruby base, reading, and direction fields. Current conversion
evidence is in `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`.

## TEI P5 Target

Use TEI ruby structures. TEI reference claims should cite
`../abc/references/TEI/P5/Source/Specs` or the matching Guidelines file when
the renderer policy is changed.

## ABC Extension Or Sidecar

No ABC extension is needed for ordinary ruby content. Direction policy remains
auditable through mapping evidence when projection differs from source shape.

## Plaintext Projection

Plaintext includes only visible body base text. Ruby readings are excluded.

## Current Evidence

`docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
classifies ruby as TEI-exact coverage.

## Open Decisions

No open v1 admission decision. Next action: keep ruby direction covered by the
mapping and publication coverage smokes during schema rotations.
