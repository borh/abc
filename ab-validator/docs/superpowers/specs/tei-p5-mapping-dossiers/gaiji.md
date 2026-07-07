# Gaiji

Status: admitted

## Source Inventory

Gaiji markers are source-observed and typed or raw-preserved by source
authority.

## Parser-IR Representation

Parser-IR carries gaiji raw marker, reference, unicode, and resolved status when
available.

## TEI P5 Target

Use TEI gaiji and character declaration policy. Current flake-backed TEI citations:
`$TEI_P5_ROOT/Source/Guidelines/en/WD-NonStandardCharacters.xml`,
`$TEI_P5_ROOT/Source/Specs/char.xml`,
`$TEI_P5_ROOT/Source/Specs/g.xml`, and
`$TEI_P5_ROOT/Source/Specs/glyph.xml`.

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
