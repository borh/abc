# Page And Line Breaks

Status: policy-needed

## Source Inventory

Page and line break markers are source-observed layout facts.

## Parser-IR Representation

Parser-IR has page-break and line-break nodes. Paragraph comparison evidence
also reports `page_break_projection` rows.

## TEI P5 Target

Use TEI page and line break elements where the source marker is a publication
break. TEI references should cite `../abc/references/TEI/P5/Source/Specs`.

## ABC Extension Or Sidecar

ABC sidecar records retain source pointer and policy reason when a break is
projected away from body paragraph comparison.

## Plaintext Projection

Plaintext excludes page and line break metadata except for body text newlines
needed by the visible-text format.

## Current Evidence

`docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json`
excludes `page_break_projection` from adapter-fidelity blockers.

## Open Decisions

Next action: define when a source break becomes TEI structure versus sidecar
metadata in the publication coverage policy.
