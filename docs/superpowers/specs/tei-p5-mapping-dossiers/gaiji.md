# Gaiji

Status: admitted

## Source Inventory

Gaiji markers are source-observed and typed or raw-preserved by source
authority.

## Parser-IR Representation

Parser-IR carries gaiji raw marker, reference, unicode, and resolved status when
available.

## TEI P5 Target

Use TEI gaiji and character declaration policy. TEI references should cite
`../abc/references/TEI/P5/Source/Guidelines` and
`../abc/references/TEI/P5/Source/Specs` for the concrete element claims.

## ABC Extension Or Sidecar

Unresolved reason and source-exact marker evidence may remain in the ABC
preservation sidecar when TEI cannot carry the complete source fact.

## Plaintext Projection

Plaintext includes the visible resolved glyph or safe visible substitute only.
Raw marker provenance is excluded.

## Current Evidence

Current coverage is reported in
`docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` and
source disposition evidence in
`docs/superpowers/reports/2026-07-06-source-region-disposition-samples.summary.json`.

## Open Decisions

Next action: pin unresolved-gaiji sidecar rows to a preservation record class
when the custom sidecar policy is expanded.
