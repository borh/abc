# Academic Presentation Diagrams Design

Date: 2026-07-10
Status: Approved for implementation planning

## Purpose

Soranoha already generates drift-checked Mermaid views from validated ADR and
architecture data. Those views are appropriate for repository documentation,
but they do not provide the hierarchy, typography, layout control, or portable
vector output required for an academic presentation.

This design adds two clean, professional SVG figures for a mixed
digital-humanities audience:

1. **Soranoha Reproducibility Architecture**
2. **Soranoha Publication Pipeline**

The figures are generated derived views. They do not become competing sources
of architectural truth and cannot affect manifest identity or ArtifactID
calculation.

## Audience and Medium

The immediate use is a 16:9 academic presentation with a black background. The
audience combines digital-humanities scholars and technically experienced
academic participants. A viewer should understand the principal argument from
the slide without knowing repository names, Clojure namespaces, schema paths,
or ADR conventions.

The canonical presentation artifacts are deterministic SVG files. They retain
real text for accessibility and later editing; text is not converted to paths.
The SVGs embed their black canvas and font resources so that their appearance
does not depend on the presentation machine.

## Goals

- Communicate the reproducibility model and publication pipeline as two
  complementary arguments.
- Preserve exact traceability from every displayed concept and relationship to
  validated repository facts.
- Remain legible at normal slide scale without zooming.
- Produce byte-stable, self-contained SVG under pinned build inputs.
- Keep Mermaid as the lightweight repository-documentation renderer.
- Permit the current parser implementation to simplify without redesigning the
  stable publication-pipeline figure.

## Non-Goals

- Replacing the current Mermaid diagrams or diagram registry.
- Hand-authoring SVG geometry.
- Creating a complete component, deployment, or class diagram.
- Showing every ADR, schema, executable, or identity coordinate as an equal
  visual element.
- Presenting the planned parser simplification as completed architecture.
- Adding animation or presentation-software-specific transitions.
- Feeding presentation metadata or generated figures into artifact identity.

## Design Principles

### Semantic authority remains outside the figures

The live sources remain:

- `docs/architecture-stages.edn` for canonical stage topology;
- `schemas/manifest.schema.json` for required identity coordinates;
- the manifest identity ownership contract in
  `docs/architecture-stages.edn`;
- `docs/architecture.md` for the accepted prose contract; and
- validated ADR relations for decision ownership and citations.

Presentation metadata may abbreviate labels, group canonical concepts, choose
emphasis, and supply layout hints. It may not define a new stage, identity
coordinate, dependency, or lifecycle fact.

### Stable contract over transient implementation

The main publication pipeline presents the stable scholarly boundary:

```text
Aozora source -> validated parser process -> Parser-IR -> manifest -> scholarly outputs
```

A subordinate dashed inset expands the current producer implementation through
AAT evidence and the mapping/compatibility boundary. The inset is labeled
**Current producer implementation**. Its displayed path must resolve against
the live stage graph. When the new parser removes this intermediate structure,
the inset can be updated or deleted without changing the main visual argument.

### Presentation is a checked projection

Every presentation node declares one or more canonical facts that back it.
Every displayed connector must be backed by either a canonical edge or a
reachable canonical path. Aggregation is allowed; invented behavior is not.

## Visual System

Both figures use one restrained theme:

- exact 1920 by 1080 SVG view box;
- explicit `#000000` background;
- a 96-pixel minimum safe margin around the graph;
- near-white primary text and muted-gray secondary text;
- cyan for identity and reproducibility;
- amber for evidence and validation gates;
- restrained green for scholarly publication outputs;
- consistent rounded boxes, thin rules, and simple directional connectors;
- shape, label, or line-style reinforcement for every color distinction; and
- no gradients, shadows, decorative icons, or ornamental effects.

The initial palette is deliberately small:

| Role | Color |
| --- | --- |
| Canvas | `#000000` |
| Primary text | `#F5F7FA` |
| Secondary text | `#A7B0BE` |
| Identity | `#48CAE4` |
| Evidence and validation | `#F2B84B` |
| Publication outputs | `#7BC47F` |

Typography uses pinned Noto Sans CJK JP Regular and Bold fonts. A subset
containing the glyphs used by both figures is embedded in each SVG as a data
resource. Graphviz computes layout using the same pinned font inputs. At the
1920 by 1080 view box, titles are at least 52 pixels, primary labels 30 pixels,
secondary labels 22 pixels, and citations 16 pixels. Connectors and enclosing
rules are at least 2 pixels wide.

Visual hierarchy comes from position, scale, grouping, and whitespace. Main
labels use disciplinary language. ADR numbers, repository paths, and schema
identifiers appear only as quiet provenance citations or footer material.

## Figure 1: Soranoha Reproducibility Architecture

### Argument

Reproducibility depends on the joint identity of scholarly sources,
computational evidence, contracts, profiles, and recipes. A validated manifest
binds those inputs to durable derived views without confusing ArtifactID with
the byte hash of a materialized file.

### Composition

The figure reads from left to right in four zones.

1. **Sources and evidence** groups the corpus snapshot, bibliographic and
   person metadata, parser and mapping evidence, schemas and profiles, and
   analysis recipes.
2. **Identity contract** is the visual center. The fifteen live manifest
   identity coordinates are grouped into source, parsing, publication,
   linguistic-analysis, and output-format families. The figure includes the
   formula `ArtifactID = SHA-256(JCS(manifest_identity_object))`, accompanied
   by the plain-language label **canonical identity contract**.
3. **Validated manifest** connects identity, provenance, validation status,
   and materialized content while keeping ArtifactID distinct from content
   hash.
4. **Scholarly views** fans out to TEI and visible text, RDF/PROV-O and Linked
   Art, tokenized and analytical data, and IIIF applicability.

A secondary annotation explains invalidation: changing an identity-bearing
input produces a new ArtifactID and rebuilds only dependent layers. It does not
attempt to draw every invalidation edge.

The five coordinate families are primary visual units. Individual coordinate
names appear as smaller items inside those families and do not compete with the
main reading path.

### Data derivation

The identity-coordinate key set comes directly from the live manifest schema.
Coordinate ownership and stage membership come from the checked architecture
contract. Presentation metadata must provide exactly one visual family and one
audience label for every required coordinate; missing and extra keys fail.

## Figure 2: Soranoha Publication Pipeline

### Argument

Soranoha turns an authoritative source corpus into traceable scholarly
publication and analysis artifacts through explicit producer evidence,
publication contracts, validation gates, and identity-bearing manifests.

### Composition

The figure uses three horizontal lanes.

1. **Sources and parser evidence** contains the Aozora snapshot, metadata, and
   validated parser process.
2. **ABC contracts and materialization** contains the Parser-IR boundary,
   compatibility and schema gates, manifest identity, provenance, and
   materialization.
3. **Scholarly publication and analysis** contains TEI and visible text,
   RDF/PROV-O and Linked Art, tokenization and annotation, analytical datasets,
   and IIIF applicability where relevant.

The dominant reading path is:

```text
Source -> validated parser process -> Parser-IR -> manifest -> scholarly outputs
```

A thin cyan identity/provenance spine continues across the pipeline. Amber gate
markers distinguish evidence admission, schema validation, and publication
profile validation. Ownership boundaries use the plain labels `ab-validator`
and `ABC`, accompanied by short explanations rather than repository-centric
jargon.

### Current parser inset

A small dashed inset attached to the validated parser process shows the
current AAT and mapping/compatibility implementation. It remains visually
subordinate to the main path. The inset configuration names the exact
canonical stages and path that back it; generation fails when that path no
longer exists. This forces an honest update when the new parser simplifies the
producer boundary.

## Presentation Metadata

A single versioned EDN document supplies presentation-only attributes for both
figures. Its responsibilities are limited to:

- audience-facing labels and optional short subtitles;
- identity-coordinate visual families;
- mappings from aggregate presentation nodes to canonical stage IDs;
- figure membership and emphasis;
- group and lane labels;
- citation selection from canonical stage and coordinate ownership; and
- constrained layout hints such as preferred rank and order.

Validation rules are:

- the coordinate metadata key set equals the manifest schema's required
  identity-coordinate set;
- the stage metadata key set equals the canonical architecture stage set;
- every aggregate node has a non-empty canonical backing set;
- every backing stage exists and every citation belongs to the canonical ADR
  set associated with that stage, coordinate, or aggregate node;
- every presentation edge resolves to a canonical edge or reachable path;
- the current parser inset resolves to its declared live path; and
- layout hints cannot add semantic nodes or edges.

Thus presentation metadata can become stale, but it cannot drift silently.

## Rendering Architecture

The existing graph value remains the semantic rendering boundary. Publication
builders add optional presentation fields such as groups, subtitles,
citations, semantic roles, emphasis, and constrained layout hints. The Mermaid
renderer ignores fields it does not use.

A Graphviz renderer converts the enriched graph value into deterministic DOT.
The effect-owning presentation driver then:

1. loads and validates all canonical sources;
2. validates presentation metadata and canonical backing paths;
3. builds both graph values in memory;
4. emits deterministically ordered DOT;
5. invokes a Nix-pinned Graphviz `dot` renderer using a fixed locale and pinned
   font input;
6. normalizes non-semantic Graphviz metadata and identifiers;
7. wraps the rendered graph in the exact 1920 by 1080 canvas;
8. injects the black background, embedded font subset, `<title>`, and `<desc>`;
9. validates the completed SVGs; and
10. replaces each committed artifact atomically only after both figures pass.

The generated, inspectable artifacts are:

- `docs/figures/soranoha-reproducibility-architecture.dot`
- `docs/figures/soranoha-reproducibility-architecture.svg`
- `docs/figures/soranoha-publication-pipeline.dot`
- `docs/figures/soranoha-publication-pipeline.svg`

The CLI is separate from the repository Mermaid command, for example:

```sh
clojure -M:abc/presentation-diagrams
clojure -M:abc/presentation-diagrams --check
```

The existing `clojure -M:abc/diagrams` behavior remains unchanged.

## Failure Model

Generation fails before writing when:

- ADR or canonical architecture validation fails;
- presentation metadata has missing, extra, or unresolved keys;
- a summarized edge lacks canonical reachability;
- the current parser inset lacks its declared live implementation path;
- Graphviz exits unsuccessfully;
- SVG parsing or normalization fails;
- an SVG uses an unapproved color, undersized text, external resource, or
  incorrect canvas; or
- accessibility metadata is absent.

Errors name the figure, offending key or source, and regeneration command. A
temporary output directory owns intermediate files. Replacement of committed
artifacts begins only after the complete set validates, and each file
replacement is atomic. An interruption between replacements can temporarily
leave a mixed set; `--check` detects that state deterministically and reports
every stale artifact.

## Verification

Automated tests cover:

- pure projections for both figures;
- total coordinate and stage presentation metadata;
- canonical backing of aggregate nodes;
- edge and path reachability;
- failure of a stale current-parser inset;
- deterministic DOT generation;
- deterministic normalized SVG generation under pinned inputs;
- exact canvas, background, palette, minimum font sizes, title, description,
  embedded font, and absence of external resources;
- committed DOT and SVG drift;
- compatibility with the existing Mermaid renderer and registry; and
- a regression proving presentation metadata and SVGs do not participate in
  `manifest_identity_object` or ArtifactID generation.

The root flake exposes a presentation-diagram drift check. The normal migration
validation includes that check once the committed figures land.

## Manual Proof Review

The first rendered proofs receive explicit human review at actual slide size.
Each figure must satisfy all of the following before acceptance:

- readable at 100 percent without zooming;
- clear entry point and left-to-right reading order;
- no connector crossing through a label;
- no more than approximately eight primary conceptual units;
- comprehensible without repository-specific knowledge;
- balanced whitespace and consistent alignment;
- consistent terminology, geometry, citation treatment, and palette across
  both figures; and
- accurate representation of current versus planned parser architecture.

Automated validation prevents structural drift and common rendering defects;
it does not replace this visual-quality review.

## Rollout

Implementation should proceed in reviewable increments:

1. presentation metadata schema and validation;
2. pure figure projections and canonical reachability checks;
3. deterministic DOT renderer;
4. pinned Graphviz and font-subsetting environment;
5. SVG normalization, accessibility, and structural checks;
6. the two initial generated proofs;
7. visual review and constrained style adjustments;
8. committed artifact drift gate and root validation wiring; and
9. architecture documentation links and authoring instructions.

Style adjustments found during proof review must change theme tokens or
presentation metadata, not hand-edit generated DOT or SVG.

## Acceptance Criteria

The work is complete when:

- both approved figures exist as self-contained, deterministic SVG on a black
  16:9 canvas;
- both figures pass the manual proof checklist at presentation scale;
- every displayed semantic node and connector has validated canonical backing;
- the current parser implementation is accurate but visually subordinate to
  the stable publication contract;
- schema, architecture, or parser-boundary drift produces an actionable
  validation failure;
- the presentation drift check runs through the repository's normal validation
  entry point;
- existing Mermaid diagrams and checks remain green; and
- presentation artifacts remain excluded from manifest identity.
