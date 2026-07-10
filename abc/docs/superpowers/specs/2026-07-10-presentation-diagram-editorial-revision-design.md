# Academic Presentation Diagram Editorial Revision

**Date:** 2026-07-10

## Purpose

Revise the two academic presentation diagrams for faster reading on a black
presentation canvas while preserving their established architecture and color
system. The revision removes unsupported or redundant wording, shortens
surrounding prose, enlarges the remaining labels, and simplifies visual routing.

The fifteen manifest identity coordinates remain visible because they are the
evidence for the identity contract and let a reader independently verify its
extent.

## Scope

The revision applies to:

- `soranoha-reproducibility-architecture.{dot,svg}`
- `soranoha-publication-pipeline.{dot,svg}`
- their source projections, renderer constraints, and regression tests

The semantic architecture is fixed. “Unchanged topology” means the same
semantic node set and semantic edge set in each figure. Ports, rank and ordering
constraints, invisible layout edges, and bundled visual routes may change
without constituting topology changes.

## Editorial Contract

### Reproducibility architecture

- Rename the identity cluster from **15-coordinate identity contract** to
  **Versioned identity contract**.
- Preserve exactly fifteen visible and unique coordinate labels across the five
  identity-family boxes: fourteen coordinate bullets plus the single
  **Output format** family label.
- Show **Output format** once. Its family heading serves as the visible label for
  the single output-format coordinate; the duplicate bullet is suppressed only
  when rendering a single-coordinate family whose coordinate label equals its
  heading. The `output_format_spec_hash` coordinate remains in projection data,
  canonical backing, ArtifactID backing, and semantic edges.
- Replace the footer with exactly:
  **Identity-bearing inputs determine ArtifactID.**
- Remove every form of the unsupported selective-rebuild claim, including
  “rebuilds only dependent layers.”
- Shorten prose in the source/evidence, ArtifactID, manifest, and scholarly-view
  boxes without removing their conceptual distinctions.

### Publication pipeline

- Retain the stable source → validated parser process → Parser-IR → manifest →
  scholarly outputs spine.
- Retain the dashed current-producer AAT inset as subordinate implementation
  detail.
- Shorten subtitles and lists to presentation phrases rather than sentences.
- Preserve the distinction between the stable Parser-IR contract and the current
  producer implementation so future parser consolidation remains accurately
  represented.

## Visual Contract

The existing black canvas and ownership palette remain unchanged:

- text: off-white
- secondary structure: gray
- identity and ABC-owned contract elements: cyan
- evidence and producer-owned process: amber
- scholarly outputs: green

The renderer fits the complete Graphviz body into a fixed 1728×745 region with
one uniform transform and rejects any fit scale below 1. The current
reproducibility figure is already close to that floor. Editorial trimming and
more efficient routing are therefore preconditions for raising the body-text
floors: the revised content must first leave enough raw-graph headroom for the
larger labels while keeping the fit scale at or above 1. The pinned drift gate is
the proof that both constraints coexist.

After trimming, primary graph-body labels increase from 30 to at least 34 source
points. Coordinate, cluster, and other non-bold graph-body labels increase from
22 to at least 24 source points. `graph-node-problems` raises its bold and
non-bold source-size floors to 34 and 24 respectively. Because the outer graph
scale and every descendant scale are constrained to be at least 1, those source
sizes are already conservative lower bounds on effective rendered size; no
separate source-times-scale validator is added.

The fixed slide chrome is outside that graph-body rule and remains unchanged:
title 52 px, subtitle 22 px, and footer/citation 16 px. Layout changes should
reduce crossings and visual fan-out by using stable ordering, explicit ports, or
shared visual routing while preserving every semantic edge.

Neither figure may contain node overlap, clipped text or shapes, or unintended
edge/label collisions at 1920×1080. The diagrams remain readable as complete
slides rather than requiring a detail view.

## Accessibility

Each SVG retains a namespaced `<title>` and `<desc>`. The title remains the
approved concise figure title. The description must be updated when shortened
box prose changes the figure’s stated emphasis, and must accurately summarize
the visible architecture without repeating removed claims.

Text remains live SVG text with the embedded Noto Sans CJK JP subset. No
external font, stylesheet, image, or resource dependency is introduced.

## Source and Generation

Projection data remains the source of figure wording and semantic structure.
DOT and SVG files remain generated artifacts. The pinned Nix application
continues to render and check both figures atomically and byte-deterministically.
Editorial changes alter the font-subset glyph set and therefore legitimately
change the committed WOFF2 payload and SVG bytes exactly once, on this revision.
Byte reproducibility means that repeated pinned generation after this revision
produces those new bytes identically; it does not mean byte invariance across
the editorial edit.

The editorial change must not alter manifest identity, canonical architecture
stage data, ADR ownership, or the established palette. Presentation metadata
remains a derived view and stays outside the manifest identity object.

## Validation and Acceptance

Automated tests and generated-artifact checks must enforce:

1. Exactly fifteen visible, unique coordinate labels while retaining all fifteen
   coordinate records and backing references: fourteen coordinate bullets plus
   the **Output format** family heading standing for
   `output_format_spec_hash`. Projection tests continue to count fifteen
   coordinates; rendering tests verify the heading-equals-single-coordinate
   suppression rule and the resulting fifteen visible coordinate labels.
2. The same semantic nodes and semantic edges as the pre-revision figures.
3. The same palette and ownership-color assignments.
4. The exact cluster heading **Versioned identity contract**.
5. **Output format** appearing exactly once in visible reproducibility-figure
   text; accessibility metadata is evaluated for accuracy rather than counted as
   a visible label.
6. The exact footer **Identity-bearing inputs determine ArtifactID.**
7. No residual selective-rebuild claim.
8. `graph-node-problems` body-text floors of 34 for bold labels and 24 for
   non-bold labels, with fixed title/subtitle/footer chrome remaining 52/22/16.
   Both figures must still render with outer and descendant scale factors at or
   above 1.
9. No node overlap, clipping, or unintended edge/label collision in original-size
   1920×1080 raster proofs.
10. Byte-reproducible 1920×1080 SVGs with accurate accessible `<title>` and
    `<desc>` elements.

Focused projection tests compare semantic node and edge identities independently
from layout-only attributes. SVG validation checks graph-body source-size floors,
the scale-at-least-1 invariant, canvas, namespace, resources, and accessibility.
The pinned drift gate regenerates and rasterizes both committed figures.
Original-size raster inspection remains the final visual proof for spatial
collisions that structural validation cannot reliably infer.

## Non-goals

- Changing the underlying system architecture or ownership boundaries
- Hiding, aggregating, or moving the fifteen coordinates to a companion figure
- Changing the palette or adopting a new visual style
- Promoting the current AAT path into the stable publication contract
- Adding presentation-only fields to manifest identity
