# Corpus per-construct adapter fidelity: classifier + why the matrix is not auto-regenerated (2026-07-08)

Follow-up to `2026-07-08-adapter-preservation-live-verification.md`. Goal was to
regenerate the stale per-adapter `aat_fidelity` cells in
`data/aozora-syntax-coverage.toml` from **corpus** evidence (native adapters over
all 17,886 works) rather than single canonical fixtures.

## What was run

Full-corpus AAT for both native adapters (aozora2: 17,862 works; aozora-rs:
17,886), then a node-content classifier
(`reports/aat-fidelity/corpus-adapter-fidelity-classifier.py`): per construct, a
predicate over AAT `blocks` node kinds/fields counts faithful typed nodes
(numerator); `ab-source-inventory` occurrences are the denominator; rate →
preserved/lossy/dropped, coupling-consistent with parser `recognition`.
Result data: `2026-07-08-corpus-adapter-fidelity.summary.json`.

## Result: trustworthy for a handful, nonsense for the rest — matrix NOT rewritten

The classifier is only sound where numerator and denominator count the **same
construct at the same granularity**. That holds for a few high-level constructs
and fails for sub-constructs:

**Trustworthy (rate ≤ ~1, 1:1 aligned):**

| construct | aozora2 | aozora-rs | reading |
|---|--:|--:|---|
| ruby.basic | 0.87 | 0.99 | preserved by both |
| decoration.boten | 0.97 | 0.95 | preserved by both |
| figure.image_inline | 0.91 | 0.91 | preserved by both |
| gaiji.jis_code | 0.82 | 0.86 | a2 preserved; rs synthesised (no jis_code field) |
| heading.basic | 0.92 | **0.24** | real aozora-rs weakness |
| decoration.bousen | 0.95 | **0.18** | real aozora-rs weakness |
| layout.tcy | 0.89 | **0.00** | aozora-rs emits no tcy node |

**Nonsense (rate ≫ 1 — granularity/representation mismatch):**
`gaiji.unicode_codepoint` rs 15.96×, `gaiji.un_embed` 20–34×,
`indentation.jisage_oneline` a2 237×, `iteration.kunoji` rs 5.3×.

**Root cause:** the two native adapters represent sub-constructs with divergent,
lossy field sets. aozora-rs stores gaiji resolution in `resolved` and **omits the
`jis_code` field**, so its JIS gaiji are indistinguishable from unicode gaiji;
the source denominator is the narrow sub-construct while the node predicate
matches the broad node class. Cross-adapter per-sub-construct fidelity is
therefore **not recoverable** from current adapter output.

## Conclusion / what a reliable regeneration would require

The corpus **confirms** (consistent with the fixture oracle) that the natives
preserve the main constructs at scale — reinforcing that the matrix's
`dropped`/`lossy` cells for those are stale. But a trustworthy per-construct
matrix regeneration needs **matched per-construct detection on both sides**:
either (a) per-construct source detectors and per-construct node classifiers at
the same granularity, or (b) uniform per-construct `x-aozora-syntax-id` /
`semantic_summary` tagging in the native adapters (aozora2 emits none; aozora-rs
tags only ~5 constructs, 92.5% `source_fallback`). Neither exists today.
Rewriting the cells from the current signal would inject nonsense; the cells are
left as-is, and the staleness stands documented (see the companion report).

Data: `2026-07-08-corpus-adapter-fidelity.summary.json`; corpus AAT staged at
`/db/ab-validator/fidelity-corpus/` (regenerable; safe to clear).
