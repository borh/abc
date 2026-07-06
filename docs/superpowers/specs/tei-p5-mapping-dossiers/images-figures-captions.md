# Images Figures And Captions

Status: policy-needed

## Source Inventory

Image, figure, and caption constructs are source-observed or parser-derived
publication facts.

## Parser-IR Representation

Parser-IR carries image and caption nodes where adapters expose them.

## TEI P5 Target

Use TEI figure and graphic policy. TEI references should cite
`../abc/references/TEI/P5/Source/Specs`.

## ABC Extension Or Sidecar

Figure metadata that is not safely represented in TEI should be preserved in
the ABC custom sidecar with source pointers.

## Plaintext Projection

Plaintext excludes figure metadata and includes caption visible text only when
the caption belongs to body text.

## Current Evidence

Figure metadata coverage is included in
`docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`.

## Open Decisions

Next action: decide which figure metadata fields are TEI attributes, ABC
attributes, or sidecar-only facts.
