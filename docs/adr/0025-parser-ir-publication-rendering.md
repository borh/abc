# ADR 0025: Parser-IR Publication Rendering

Status: Accepted
Date: 2026-07-03
Supersedes: none
Depends on: ADR 0007, ADR 0023

## Context

`../ab-validator` is the producer-side home for parser execution, candidate
comparison, AAT extraction, and parser-IR export. ABC consumes the exported
parser-IR as a downstream publication boundary and must not rebuild that
producer logic locally.

The publication layer also has a different job from artifact materialization.
Renderers should transform values into publication content, while file layout,
manifest assembly, and other materialization concerns stay separate.

## Decision

ABC renders publication artifacts from parser-IR, not from Aozora source text.

`../ab-validator` owns parser comparison, parser implementation, AAT
extraction, and parser-IR export.

ABC renderers are pure value transforms.

File materialization and manifest construction are separate from renderers.

The current flat parser-IR cannot reconstruct original paragraph boundaries
with full fidelity. The first TEI body renderer therefore emits a valid linear
transcription and records structural limitations instead of inventing missing
layout detail.

Renderer coverage is schema-derived and fails closed when parser-IR adds node
types.

## Consequences

ABC can validate and publish parser-IR-derived artifacts without executing
parser candidates locally.

The publication pipeline remains sensitive to parser-IR schema change: new
node types require renderer coverage updates before ABC can publish them.

Because renderers are pure transforms, materialization and manifest assembly
can evolve independently from the TEI body renderer logic.

