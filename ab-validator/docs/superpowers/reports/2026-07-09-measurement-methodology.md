# Measurement methodology: putting the parsers on one axis (§6)

**Date:** 2026-07-09
**Addresses:** `2026-07-08-aozora-parser-comparison-study.md` §5 threat #2
(methodological non-uniformity — "parsers are measured three ways"); open-work item §6
of `2026-07-09-parser-comparison-followups-handoff.md`.

**Thesis.** The study uses two measurement *layers* with different jobs. The one the
**verdict rests on is already a single uniform axis**; the "three methods" caveat
applies only to the other layer (curated-vector breadth), which is corroboration, not
the decision. This note makes that explicit so threat #2 does not read as undermining
the recommendation.

## The two layers

### Layer A — conformance breadth (§4.1–4.6): per-parser, three native methods

Purpose: how many of the **curated 127 vectors** (weighted toward edge cases) each
parser handles. Production AAT adapters omit spans and vary in completeness, so a single
AAT-serialization comparison is apples-to-oranges (§3.2). Each parser is therefore
measured at its own native granularity:

| parser | native method | why |
| --- | --- | --- |
| aozora / aozora-pipeline | own `inspect` projection (exact) | only parser exposing inspect |
| aozora-core (aozora2) | AAT + divergence attribution over its `Node` tree | separates mapper loss from parser gap |
| aozora-rs-core | `--mode retokenized` token dump → spec kinds | adapter is fallback-dominant; dump bypasses it |
| aozora2html / aozora-epub3 | AAT only | context/reference; not Rust fork candidates |

This layer is **deliberately non-uniform** — each method is the fairest available view of
that parser — and its numbers are read *with the method attached*. It is not a single
scale, and was never intended as the decision axis.

### Layer B — corpus coverage / mass (§4.7–4.9): one method, all five parsers

Purpose: what fraction of the markup that **actually occurs** in the ~17,886-work corpus,
frequency-weighted, each parser can represent. This is the **uniform single-scale axis**,
and it is the axis the recommendation is stated on. All five parsers are measured
*identically*:

- same instrument (`normalized-corpus-coverage.py`) over each parser's full-corpus AAT;
- **fair union recognition signatures** — each construct's signature is the union of
  every adapter's actual encoding, derived from a full-corpus vocabulary audit, so each
  parser is credited for its own encoding with no cross-contamination (the encodings are
  disjoint: ab-aozora uses markers, aozora2html uses HTML-flavoured `style_type`s);
- 1-per-occurrence counting; rates capped at 1.0 in the weighted sum;
- shared source-authority denominators, now verified **corpus-invariant** (§1).

Result (§1, fully pinned): aozora 0.957 > aozora-rs 0.938 > aozora-epub3 0.926 >
aozora2 0.855 > aozora2html 0.743 — one number per parser, one scale.

§4.8 (fidelity/robustness split) further decomposes Layer B on **two independent
denominators** (best-attested and reference) and shows they give the **same ranking** —
a built-in single-scale robustness check. §4.9 (parity) reuses the same signatures.

## Why the verdict is not a "three-methods" artifact

The recommendation ("base the fork on aozora-pipeline") is stated on Layer B, the uniform
axis, where aozora leads on one scale. Layer A **corroborates** it (aozora leads
conformance breadth, 22/25 must) but does not define it. Four independent safeguards
anchor cross-parser comparability despite Layer A's per-parser methods:

1. **AAT validation oracle (§3.4).** `ab-aozora` (the reference parser through the AAT
   path) reproduces `aozora`-inspect on 115/122 vectors, 0 must-fails — so the AAT
   projection that Layer B uses for *every* parser is a faithful stand-in for the exact
   inspect view, not a lossy second-class measurement.
2. **Fair union signatures (§4.7).** The one method credits each parser's own encoding;
   no parser is penalised for not emitting another's node shape.
3. **Two-denominator agreement (§4.8).** Best-attested and reference denominators — two
   different definitions of "truth" — rank the parsers identically.
4. **Corpus-invariant denominators (§1) + controlled re-pin (§7).** The shared scale does
   not move with corpus snapshot or parser revision beyond a bounded (≤0.05) amount.

## Would a single-scale *normalization* add anything?

A further step — projecting Layer A's three native methods onto one normalized 0–1
conformance score — is possible but low-value and potentially misleading: it would
average away the very distinction (parser gap vs mapper loss vs fallback) that makes each
native method fair. The honest single scale already exists (Layer B); Layer A is best
left as three method-attached views feeding the divergence attribution (§3.3). The
recommendation therefore stands on one uniform axis without a synthetic composite.

**Bottom line for §5 threat #2:** the non-uniformity is real but confined to the
breadth-corroboration layer; the decision axis (corpus coverage) is a single uniform
scale, cross-checked four ways.
