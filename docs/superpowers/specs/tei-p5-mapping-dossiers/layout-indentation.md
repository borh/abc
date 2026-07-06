# Layout And Indentation

Status: policy-needed

## Source Inventory

Indentation, jisage, yokogumi, keigakomi, and related layout markers are
source-observed layout facts.

## Parser-IR Representation

Parser-IR carries indentation and layout-span structures for the measured
coverage rows.

## TEI P5 Target

Use TEI rendition and layout policy where TEI can carry publication-facing
layout. TEI references should cite `../abc/references/TEI/P5/Source/Specs`.

## ABC Extension Or Sidecar

Source-exact layout kind and value may require ABC namespaced attributes or a
sidecar record when TEI rendition would lose the source marker.

## Plaintext Projection

Plaintext excludes layout metadata.

## Current Evidence

Coverage and remaining policy rows are tracked in
`docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`.

## Open Decisions

Next action: split each layout family into TEI-native, TEI-plus-ABC-extension,
or sidecar-only target lanes.
