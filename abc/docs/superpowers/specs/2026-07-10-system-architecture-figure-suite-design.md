# System Architecture Figure Suite Design

Date: 2026-07-10
Status: Approved for implementation planning

## Purpose

Replace the two overloaded presentation diagrams with a coordinated academic
figure suite whose visual relationships match the executable Soranoha
architecture. The figures may depend on concepts introduced by earlier figures
in the sequence; they do not need to repeat the whole system independently.

The current reproducibility figure is difficult to read because ten fan-out and
fan-in edges are compressed into overlapping trunks. More importantly, it uses
the same arrow syntax for three different claims:

- a coordinate is a member of `manifest_identity_object`;
- `ArtifactID` is computed from that object; and
- one pipeline stage depends on another.

Those claims need different visual forms. Better routing alone would preserve a
semantic error: the `Sources and evidence` aggregate does not produce schemas,
policies, configurations, or format specifications. The current manifest-to-
views arrow is also too strong as a production claim. TEI, tokenized, analysis,
and annotation stages have non-manifest inputs in the canonical stage graph.

## Design Principles

1. Give each figure one semantic job.
2. Derive semantic nodes and edges from executable architecture contracts.
3. Keep presentation metadata incapable of inventing topology.
4. Use containment for membership or ownership and arrows for directional
   relationships.
5. Label directional relationships whose meaning is not ordinary dataflow.
6. Keep the current black background, palette, typography, and ownership/role
   color system.
7. Preserve all fifteen current identity-coordinate names as visible evidence.
8. Treat checked-in DOT and SVG as generated evidence, never authoring sources.
9. Model the parser producer as replaceable implementation while keeping the
   Parser-IR publication boundary explicit.
10. Fail closed when architecture, ADR attribution, schemas, rendering, or
    committed artifacts drift.

## Scope

This work produces five coordinated 1920 by 1080 SVG figures:

1. system boundaries and authority;
2. publication dataflow;
3. reproducible artifact identity;
4. scholarly derivation dependencies; and
5. architecture governance.

It also strengthens the presentation model so that semantic projection and
layout are separate. Ranks, ports, ordering constraints, line wrapping,
bundled visual routes, and cluster placement are layout. Stages, ownership,
identity coordinates, and dependency edges are semantic architecture.

## Canonical Sources

The figure suite is a projection of existing or strengthened contracts:

- `docs/architecture-stages.edn` owns stage IDs, the stage DAG, stage inputs,
  manifest-coordinate ADR attribution, and the new producer/contract/authority
  classifications.
- `schemas/manifest.schema.json` owns the required key set of
  `manifest_identity_object`.
- `docs/architecture-presentation.edn` owns presentation-only labels, family
  grouping, captions, ordering, and figure metadata. It cannot declare
  semantic edges.
- `docs/adr/*.md`, parsed by the shared ADR parser, own lifecycle and decision
  backing.
- A small executable governance contract owns the governance figure's declared
  files, namespaces or Vars, registry entries, renderer, and Nix gates.

The generator must not infer architecture heuristically from arbitrary source
code. Explicit contracts are reviewable and stable; validators check that
their references resolve to the implementation and documentation they claim.

### Stage Responsibility Vocabulary

A single `:owner` field would conflate distinct responsibilities. Stages must
be able to declare:

- `:source-authority`: authority for the underlying source facts, when
  applicable;
- `:producer`: component that materializes the stage; and
- `:contract-owner`: component that owns the accepted interchange or
  publication contract.

These fields use closed vocabularies. Parser-IR is the important cross-boundary
case: conversion evidence is produced on the validator side while the accepted
publication contract is owned by ABC. Current or replaceable producer details
also receive an explicit implementation classification rather than being
deduced from names or prose.

## Visual Relationship Grammar

The same grammar applies across all figures:

- containment means membership or ownership;
- a solid arrow means an actual data or dependency flow;
- a labelled computation arrow means a deterministic transformation;
- a dashed boundary or arrow means current or replaceable implementation
  detail;
- a labelled non-computation arrow states its verb, such as `identifies`,
  `recorded in`, or `anchors provenance for`.

No dense figure may use an unlabeled arrow for an ambiguous relationship.
Layout-only junctions may route edges, but they are not semantic nodes and must
not appear in the projected node or edge set.

## Figure 1: Soranoha System Boundaries

### Purpose

Introduce source authority, production responsibility, contract ownership, and
the stable handoff between parser evidence and publication.

### Content

The primary sequence is:

```text
Aozora source authority
        |
        v
Parser and evidence production -- ab-validator
        |
        v  Parser-IR handoff
Acceptance and publication contracts -- ABC
        |
        v
Scholarly publication and analysis
```

Parser-IR sits at the component boundary. The figure distinguishes its
producer from its contract owner. The current parser/AAT path may appear as a
dashed subordinate detail, so a future parser consolidation changes the
producer projection without weakening the stable publication boundary.

### Derivation

Nodes are canonical stages grouped by their explicit responsibility fields.
Handoffs are derived from canonical stage dependencies that cross producer or
contract-owner boundaries. Presentation metadata may choose aggregation and
order, but it cannot add a boundary crossing.

## Figure 2: Soranoha Publication Dataflow

### Purpose

Show the accepted stage DAG without identity-coordinate internals.

### Content

The projection includes every canonical stage and every `:inputs` edge:

```text
Aozora snapshot --> AAT evidence --> Parser-IR --+--> TEI
       |                                         +--> Tokenized text --> Analysis
       +--> Metadata ----------------+            |                 +--> Annotation
                                      +--> Manifest +--> RDF
                                                   +--> IIIF
```

The rendered graph must also show the Parser-IR and tokenized inputs to
annotation. The sketch is explanatory, not an alternate edge list.

### Derivation

Nodes come from `:stages`; edges are exactly the reverse projection of each
stage's `:inputs`. Dashed styling is driven by explicit implementation
classification. There is no hand-maintained presentation topology.

## Figure 3: Reproducible Artifact Identity

### Purpose

Explain the ArtifactID contract precisely while keeping the coordinate set
independently checkable.

### Content

```text
Versioned identity contract
+-- manifest_identity_object -------------------+
| Source identity                    3 names     |
| Parsing identity                   4 names     |
| Publication contracts              2 names     |
| Linguistic analysis                5 names     |
| Output format                      1 name      |
+-----------------------------------------------+
                    | RFC 8785 JCS + SHA-256
                    v
                ArtifactID
                    | identifies / recorded in
                    v
            Validated manifest
```

All fifteen current coordinate names remain visible and unique. Coordinate
families are contained fields, not processing stages, so there are no arrows
between family boxes and no generic `Sources and evidence` node.

### Derivation

The coordinate key set comes from the manifest schema. Family and display
labels come from presentation metadata whose key set must equal the schema key
set. ADR attribution comes from the per-coordinate mapping in
`architecture-stages.edn`. The formula is fixed by the accepted manifest
identity contract.

## Figure 4: Scholarly Derivation Dependencies

### Purpose

Expose the multi-input structure hidden by the former aggregate
manifest-to-views arrow.

### Content

The output-stage projection includes these canonical relations:

```text
Parser-IR ------+--> TEI
                +--> Tokenized text --> Analysis
                |          +---------> Annotation
                +--------------------> Annotation

Manifest -------+--> TEI
                +--> RDF
                +--> IIIF
                +--> Tokenized text
```

This figure does not claim that the manifest alone materializes every view.
It shows direct dependencies, while transitive consequences remain derivable
from the stage graph.

### Derivation

Select output and contract stages from the canonical DAG, include their direct
inputs, and project every resulting direct edge. The figure query defines the
selection rule, not an independent edge list.

## Figure 5: Architecture Governance

### Purpose

Show why the other architecture figures are auditable derived views rather
than manually maintained illustrations.

### Content

```text
ADRs ----------------+
Schemas -------------+--> validated architecture model
Architecture EDN ----+               |
                                     v
                            figure projections
                                     |
                                     v
                         deterministic DOT / SVG
                                     |
                                     v
                           tests + Nix drift gates
```

The detailed projection names the shared ADR parser, architecture loaders and
validators, presentation registry, renderer, checked-in artifacts, and root
migration/Nix gates.

### Derivation

The governance graph comes from an executable contract whose node references
must resolve as files, namespaces or Vars, registry entries, or Nix check
attributes. Its relations describe actual consumption, projection, rendering,
and enforcement. A missing implementation reference or gate is an error, not
a silently omitted node.

## Projection Architecture

The presentation layer consumes a validated architecture model and exposes
pure figure queries. Each query returns semantic nodes and edges with canonical
backing. A separate layout layer adds clusters, ranks, ports, wrapping, and
routing hints.

The generator enforces that:

- a layout transform cannot create or delete semantic nodes or edges;
- presentation metadata cannot declare semantic topology;
- every projected node and edge retains its originating stage, coordinate,
  ADR, schema, or governance-contract reference; and
- layout-only junctions are marked and excluded from semantic topology tests.

This separation permits professional routing changes without treating port or
bundling changes as architecture changes.

## Semantic Validation

Generation fails when any of the following holds:

- stage IDs are duplicated;
- an input endpoint is missing;
- the stage graph contains a cycle;
- responsibility or implementation values fall outside their closed enums;
- the publication or derivative edge set differs from its canonical query;
- Parser-IR loses its explicit producer/contract-owner boundary;
- identity keys differ between the schema, coordinate metadata, and ADR
  attribution map;
- a coordinate is duplicated, unlabeled, invisible, or unattributed;
- an ADR, schema, stage, file, namespace, Var, registry entry, or Nix check
  reference does not resolve;
- a semantic arrow lacks a declared relationship type; or
- presentation metadata attempts to add topology.

Errors identify the figure and originating contract entry so that drift is
actionable.

## Rendering and Visual Validation

Every figure uses the established black 1920 by 1080 canvas, embedded Noto Sans
CJK font, palette, role colors, typography, and safe margins. Titles and
descriptions remain accessible and accurate.

Automated checks require:

- a valid SVG namespace and exact 1920 by 1080 dimensions;
- nonempty, accurate `<title>` and `<desc>` elements;
- approved colors and graph-body text-size floors;
- labels contained within their owning boxes;
- no node-box overlap or clipping;
- no edge crossing an unrelated node;
- zero crossings between unrelated semantic-edge centerlines; shared endpoints
  and explicitly declared layout junctions are excluded;
- arrowheads outside text and attached clearly to intended endpoints;
- all fifteen identity labels visible exactly once;
- successful pinned-librsvg rasterization to a nonblank 1920 by 1080 PNG; and
- byte-identical output from two isolated generation runs.

The geometric validator must consume Graphviz geometry or normalized SVG
geometry rather than infer correctness from string presence. Automated checks
are followed by human review at projected presentation size for hierarchy,
balance, and immediate endpoint legibility.

## Drift Enforcement

All DOT and SVG files are generated into temporary directories before atomic
replacement. The drift gate:

1. validates canonical contracts and ADR backing;
2. builds the semantic projections;
3. validates layout preservation of topology;
4. generates every figure twice and compares bytes;
5. compares generated files with committed artifacts;
6. rasterizes and checks each SVG; and
7. runs the geometry and accessibility validators.

All five figures participate in the root migration gate. A topology change,
coordinate change, parser-boundary change, missing governance link, stale
artifact, or rendering regression therefore fails before publication.

## Parser Evolution

The figure suite does not freeze the current parser implementation. The
canonical stage model records the current producer path and its replaceable
classification. Parser consolidation updates that model once. Boundary,
pipeline, and derivative projections then regenerate or fail loudly. The
identity figure changes only when the accepted identity contract changes.

## Migration

Implementation proceeds in reviewable stages:

1. characterize the current stage, identity, and governance contracts;
2. add responsibility and implementation classifications with validation;
3. separate semantic projection from layout metadata;
4. implement and test the five pure figure queries;
5. add geometry and suite-level drift checks;
6. generate and visually review all five figures;
7. update architecture documentation links and captions; and
8. remove superseded two-figure assumptions only after the new suite passes
   the root gate.

The committed figure paths are:

- `figures/soranoha-system-boundaries.svg`;
- `figures/soranoha-publication-pipeline.svg`;
- `figures/soranoha-reproducibility-architecture.svg`;
- `figures/soranoha-scholarly-derivation-dependencies.svg`; and
- `figures/soranoha-architecture-governance.svg`.

The two established paths retain their names while their subjects are narrowed
as specified above. No checked-in figure is removed until every reference has
been migrated.

## Non-goals

- Inferring architecture from a general-purpose source-code call graph.
- Making every figure independently self-contained.
- Encoding presentation ranks or ports as semantic architecture.
- Promoting or changing ADR lifecycle status as a side effect.
- Changing the manifest identity coordinate set.
- Freezing the current parser implementation.
- Replacing the detailed Mermaid architecture diagram used for engineering
  documentation.

## Acceptance Criteria

The design is complete when:

1. five coordinated, accessible 1920 by 1080 SVGs are generated and linked;
2. their semantic nodes and edges are canonical projections rather than
   hand-maintained presentation topology;
3. the identity figure shows exactly fifteen visible, unique coordinate labels
   and the accepted JCS/SHA-256 formula;
4. the dataflow and derivative figures equal the canonical stage DAG queries;
5. the boundary figure distinguishes source authority, producer, and contract
   ownership, including Parser-IR's cross-boundary role;
6. the governance figure's implementation and gate references all resolve;
7. the shared visual grammar and existing palette are consistent across the
   suite;
8. no figure contains node overlap, clipping, unintended crossings, obscured
   endpoints, or ambiguous arrows;
9. two isolated runs produce byte-identical DOT and SVG output;
10. committed artifacts pass drift, raster, geometry, accessibility, Clojure,
    Nix, and root migration gates; and
11. a parser-topology fixture proves that an implementation change causes the
    affected projections to change or fail while leaving identity unchanged
    unless its coordinate contract changes.
