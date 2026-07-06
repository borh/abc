# Malformed Source Diagnostics

Status: diagnostic-only

## Source Inventory

Malformed source residues are the small remainder after source apparatus is
split out from legacy malformed-noise counts.

## Parser-IR Representation

Parser-IR need not turn malformed source into normal publication nodes. It must
preserve enough diagnostic evidence for audit.

## TEI P5 Target

Malformed source is not a TEI publication fact by default.

## ABC Extension Or Sidecar

Use ABC diagnostic sidecar records with source pointer, marker preview, count,
and owner for follow-up.

## Plaintext Projection

Plaintext omits malformed-source metadata.

## Current Evidence

`docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
reports `source_region_coverage.malformed_source_occurrences: 16`.

## Open Decisions

Next action: keep the diagnostic-only lane green and add samples if the
malformed-source count changes in a future corpus run.
