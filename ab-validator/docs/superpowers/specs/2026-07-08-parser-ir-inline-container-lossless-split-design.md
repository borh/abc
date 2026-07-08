# Parser-IR Inline-Container Lossless Split Design

**Date:** 2026-07-08
**Status:** B1–B4 implemented; U2 + U3 resolved. B1/B2 corpus-validated
(999/1000 on calib-triage-1000, only the accepted `gaiji` residual). B3 adds
sentence/orthographic preservation provenance (Issue 5); B4 retires the dead
sentence splitter (Issue 4). See the Phase B validation section of the audit
report.
**Owner:** ab-validator
**Revises:** decisions D4/D5 and the "Node-boundary invariant" of
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`. That design
made every node with `inline_children` atomic for sentence projection. Corpus
evidence shows that blocks the majority of the ~9.7% projection failures; this
design makes `emphasis` and `layout-span` splittable-by-recursion while keeping
truly-atomic boundaries a hard failure.
**Evidence:** `ab-validator/docs/reports/2026-07-08-corpus-scale-sentence-projection-audit.md`.

## Problem

At corpus scale ~9.7% of works fail parser-IR sentence projection. The failures
are **not one cause** — they are three distinct causes that this design must not
conflate:

1. **Inline-container atomic-boundary bail (dominant; `emphasis` 54, `layout-span`
   10 in calib-triage-1000):** a sentence terminal falls inside an `emphasis` /
   `layout-span` carrying `inline_children`; `split_node_at_boundaries`
   (`sentences.rs`) hard-fails because the node is atomic. **B1 fixes this.**
2. **Whitespace-only paragraph → no sentence spans (part of the 32 "other"):** a
   body paragraph whose *visible* text is whitespace-only (or empty) while its
   byte span is non-empty. `ab_plaintext::sentence_split` returns zero spans for
   whitespace-only input, so `assert_sentence_span_tiling` bails with
   `body paragraph has no sentence spans for non-empty byte span`. **This is a
   whitespace-handling bug, NOT the container problem — B2 fixes it, B1 does not.**
3. **Off-by-N tiling (part of the 32 "other"):** a trailing whitespace/newline
   text node after content is not tiled into any sentence span, so the last
   sentence ends before `paragraph_end`. **B2 fixes this.**

### Reproducers (verified)

- Cause 2: converting a body paragraph whose only content is `"　\n"` (full-width
  space + newline) fails with exactly
  `body paragraph has no sentence spans for non-empty byte span`. A paragraph with
  non-whitespace, terminal-free text (`"これは終端なし"`) instead yields exactly one
  sentence — confirming `sentence_split` emits a span for any non-whitespace run
  and the no-span bail is specific to whitespace-only visible text.
- Cause 1: the shared fixture `atomic-boundary-emphasis-input.aat.json` (emphasis
  with mixed `inline_children`, interior terminal) fails with
  `sentence boundary falls inside atomic node emphasis at byte 6`.

Note: `001091`'s actual failing paragraph is block 109 — content `[text, figure]`
with visible text just `"\n"` (whitespace-only) — not a container at all. So the
no-span case is genuinely whitespace-only visible content. (The earlier draft
misattributed cause 2 to a `font_size` container; that was wrong.)

Schema caveat feeding B-D5: `layoutSpanNode` requires `text` unconditionally, but
`emphasisNode` requires only `anyOf: [text, inline_children]` — so a schema-valid
`emphasis` may carry `inline_children` **without** `text`. `convert.rs` populates
`text` for emphasis today, but projection must not rely on it: it must derive
visible text from `inline_children` (see the invariant below).

## Scope and residuals (bounded by evidence — corrected)

Only three parser-IR node types carry `inline_children`: `emphasis`,
`layout-span`, `heading`. `heading` is not sentence-wrapped body prose (already
special-cased). So **B1 clears the dominant `emphasis`/`layout-span`
atomic-boundary failures** — it does **not** claim to clear the entire class.

Known residuals after B1:

- **gaiji atomic-boundary (calib-triage-1000: 1 work):** a `gaiji` node whose
  visible text (`gaiji/unicode` or `raw_marker`) contains an interior terminal.
  B-D2 keeps `gaiji` atomic, so this remains a hard failure. **Accepted as a rare
  hard-fail (B-D7):** a `gaiji` is a single glyph, so there is no lossless split;
  the work simply does not publish until edited. The audit must keep classifying
  it separately so it cannot mask new regressions.
- **whitespace-only no-span and off-by-N:** cleared by B2, not B1.

Because residuals remain, **B4 (fallback retirement / Issue 4) is conditional on
the measured residual rate**, not on B1 landing.

## Options considered

| # | Approach | Verdict — why |
|---|---|---|
| 0 | **Status quo (hard-fail)** | Baseline. ~9.7% of the corpus unpublishable; Issue 4 stays blocked. Rejected as end state. |
| A | **Recursive sibling split (RECOMMENDED)** | Extend `split_node_at_boundaries`: a container straddling boundaries becomes N+1 siblings of the same type/style, each with its text sub-slice and its partition of `inline_children`; recurse for nested containers; still fail if a boundary lands inside an atomic child. Localizes boundary logic where it lives, preserves the node-boundary invariant (whole sibling nodes; ABC never slices), lossless (visible text, emphasis coverage, tokenizer input unchanged), keeps D5 honesty. Cost: recursion over `inline_children`; TEI gains adjacent `<hi>` siblings. |
| B | **Pre-split at node construction** (`convert.rs`) | Rejected: inverts the spec's ownership ordering and spreads boundary logic into `inline_children_nodes`. More invasive, worse separation. |
| C | **Soft boundary** (`node_range` includes a straddling container) | Rejected: violates D4/D5, falsifies segmentation evidence. |

**Recommendation: Option A**, scoped to `emphasis` + `layout-span`.

### Why Option A is lossless

Splitting an emphasis at a real sentence boundary does not change visible text,
which characters are emphasized (both siblings carry the same `style`, together
covering the same run), or tokenizer input (flattened visible text). It changes
only parser-IR node structure and TEI element grouping — a faithful
representation, not the silent snapping D5 prohibits.

## Visible-text source of truth (invariant)

There must be one authoritative visible-text projection for a container, used both
to locate sentence boundaries and to build split siblings.

- **Authoritative projection:** for a container with `inline_children`, visible
  text is `concat(child visible text)`, where per-child visible text is:
  `text`→`text`; `ruby`→`ruby.base`; `gaiji`→`unicode`|`raw_marker`;
  `line-break`→`"\n"`; `editor-note`→`""`; nested `emphasis`/`layout-span`→ recurse.
- **B1 computes visible text from children** (not the flat `text`) when splitting.
  The flat `text` is a consistency check, not the source: when `text` is present
  it MUST equal the child projection (else fail with a diagnostic); when absent
  (a schema-valid `emphasis` may omit `text`), the child projection is
  authoritative. Note this also implies fixing `parser_ir_node_visible_text_ref`,
  which today reads only the flat `text` and would under-project an emphasis that
  has `inline_children` but no `text`.

## B1 — recursive partition contract (precise)

Sentence boundaries are byte offsets in the paragraph's visible text. A container
occupies a contiguous visible range `[c_start, c_end)`; its children tile that
range. Let `B` be the set of sentence boundaries with `c_start < b < c_end`.

- `|B| = 0` → container unchanged.
- `|B| = k > 0` → container becomes **k+1 sibling nodes** of the same
  `type`/`style`/`layout`. Each sibling's `inline_children` = the children in its
  visible sub-range, `text` = their concatenated visible text, `span` = union of
  their source byte spans.

Per-child handling at a boundary `b`:

| Child kind | Rule |
|---|---|
| `text` | If `b` strictly inside → split into two `text` children at `b`. If at an edge → clean cut. |
| `emphasis`, `layout-span` | If `b` strictly inside → **recurse**. If at an edge → clean cut. |
| `ruby`, `gaiji` | If `b` strictly inside its visible span → **fail** with the atomic-node diagnostic (child type + byte offset). At an edge → clean cut. |
| `editor-note` | Zero visible width (`""`). Cannot contain `b` strictly. Ownership handled below. |
| `line-break` | Visible `"\n"` (width 1, not a terminal). `b` can only fall at its edges → clean cut; never split. |

Zero-width / edge ownership (deterministic): a zero-visible-width child
(`editor-note`) or an atomic child boundary lying **exactly** on `b` is assigned
to the **left** (preceding) sibling — matching the existing top-level rule in
`node_belongs_to_sentence` (a zero-width node at `p` is claimed by the sentence
whose range ends at `p`, because the cursor advances through it while resolving
that earlier sentence). B1 MUST reuse this rule so top-level and nested ownership
agree.

Child source spans are kept in absolute byte coordinates, so the orthographic
join and ruby-base logic are unaffected by the split.

## U1 — TEI/renderer legality (RESOLVED, favorable)

- `<s>` uses `macro.phraseSeq` in `abc/schemas/tei-profile.rng`; `<hi>` is
  phrase-level. **Repeated adjacent `<hi>` inside `<s>` is valid.** The only `<s>`
  constraint is no-nested-`<s>` (schematron rule 41), which does not apply.
- The ABC renderer is per-node (`render-emphasis-node` / `render-layout-span-node`
  render each node's `inline_children` slice or flat `text` to `[:hi {:rend …}]`).
  Two siblings render as two `<hi>` with no renderer change.
- Preservation: two nodes deterministically yield two style-projection-records; no
  existing materialization test asserts a hard record count for a split fixture,
  so no breakage.

## Decomposition

- **B1 — Recursive inline-container split** (`sentences.rs`). Clears the dominant
  `emphasis`/`layout-span` atomic-boundary failures. Implements the partition
  contract and the visible-text invariant above.
- **B2 — Whitespace handling** (`sentences.rs`, independent). Two parts, both
  rooted in whitespace-only visible runs:
  - trailing whitespace/newline text node absorbed into the last sentence span
    (fixes off-by-N);
  - a body paragraph with whitespace-only visible text but non-empty byte span
    yields no sentence rows without erroring (fixes the no-span bail — decide:
    emit no rows and relax the tiling assert for whitespace-only paragraphs, or
    emit a single whitespace-covering row). Reproducer: `"　\n"` paragraph.
- **B3 — Preservation provenance (Issue 5). DONE.** Two new preservation-sidecar
  record constructs (schema `0.2.0`→`0.3.0`): `sentence_segmentation` (splitter
  identity + that node/paragraph ranges were rewritten for `<s>` emission — this
  subsumes container-split provenance, closing U3) and `orthographic_annotation`
  (one per detector annotation: detector id, kind, source byte range, and the
  sentence ids it tags). Both `custom_sidecar` class; no new coverage class.
- **B4 — Retire the sentence fallback (Issue 4). DONE.** Deleted the dead
  `abc.text` namespace (its `split-japanese-sentence` was the last legacy splitter,
  referenced only by its own test). The publication gate already rejects non-empty
  body paragraphs without sentence rows, so no renderer change was needed; the
  renderer's non-`<s>` path stays for legitimately non-wrappable content (headings)
  and zero-span paragraphs. Full-adapter-scale re-audit still worth running before
  declaring corpus-wide readiness.

## Open unknowns (incubate)

- **U2 (measure):** RESOLVED. Re-audit of `calib-triage-1000` after B1+B2 →
  999/1000 succeed; the only failure is the accepted `gaiji` hard-fail (B-D7); no
  new classes. Full-adapter-scale re-audit still worth running as part of B4.
- **U3 (decide):** RESOLVED. No bespoke per-split record; instead a single
  `sentence_segmentation` preservation record documents that node/paragraph ranges
  were rewritten for `<s>` emission by the named splitter — which covers container
  splits without marking individual siblings. Detector provenance is carried by
  the `orthographic_annotation` records (B3, B-D8).

## Non-goals

- Splitting `heading`, `ruby`, `gaiji`, `line-break`, or `editor-note`.
- Any change to ABC's TEI renderer or the tei-profile (U1 shows none needed).
- Ruby-driven tokenization or ortho-normalized analyzer input (issues 2/3).

## Decisions

| # | Decision | Rationale |
|---|---|---|
| B-D1 | Make `emphasis` + `layout-span` with `inline_children` splittable by recursion | Clears the dominant atomic-boundary failures with a bounded surface (only 3 types carry inline_children; heading excluded). |
| B-D2 | Boundary strictly inside an atomic child that carries visible text (`ruby`/`gaiji`) still hard-fails; zero-/single-width children (`editor-note`/`line-break`) resolve by clean-cut/ownership per the partition table | Preserves D5 honesty; leaves a measured gaiji residual (U2), not a silent snap. |
| B-D3 | Localize the split in `sentences.rs`, not `convert.rs` | Keeps the spec's ownership ordering and confines boundary logic. |
| B-D4 | No ABC renderer/profile change | U1: `<s>` allows repeated `<hi>`; renderer is per-node. |
| B-D5 | Container visible text is derived from children; `text` must equal that projection | One source of truth; guards the schema gap where `text`/`inline_children` could disagree. |
| B-D6 | Zero-width / edge children owned by the left sibling | Deterministic partition; matches existing top-level ownership. |
| B-D7 | Accept the `gaiji` atomic-boundary as a rare hard-fail | A single glyph has no lossless split; the work does not publish until edited. Audit classifies it separately so it never masks new regressions. |
| B-D8 | Provenance via `sentence_segmentation` + `orthographic_annotation` preservation records, not per-split markers | Segmentation record covers container splits at the aggregate; detector records carry ortho tag/index/detector linkage. Avoids a Rust converter change and per-node markers. |
