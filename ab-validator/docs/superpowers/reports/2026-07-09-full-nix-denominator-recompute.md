# Full-nix-corpus denominator recompute (§1)

**Date:** 2026-07-09
**Extends:** `2026-07-08-aozora-parser-comparison-study.md` §4.7/§4.8/§4.9 and §7
(reproducibility); open-work item §1 of `2026-07-09-parser-comparison-followups-handoff.md`.
**Goal.** The study's coverage numerators are measured on the pinned nix corpus
(repin `1a4f864` dump, via the run-set), but the **denominators** were source-authority
`occ` counts from the original *local* extraction. Recompute the denominators on the
**pinned** corpus so numerator and denominator share one corpus, then re-run the three
coverage scripts and confirm the verdict.

## Method

1. `ab-source-inventory` (matrix `data/aozora-syntax-coverage.toml`) over the pinned
   corpus (`aozorabunko@0e9ea3e`, resolved via nix) + full index (17,886 works) →
   per-construct source-authority `occurrences`.
   Output: `2026-07-09-source-authority-inventory.pinned.json`.
2. `rebuild-fidelity-summary.py` — new summary with pinned denominators; aozora2 /
   aozora-rs numerators recomputed from the run-set AAT dirs (same predicates as
   `corpus-adapter-fidelity-classifier.py`). Output:
   `2026-07-09-corpus-adapter-fidelity.summary.json`.
3. Re-ran `normalized-corpus-coverage.py` (§4.7), `parity-support-audit.py` (§4.9),
   `fidelity-robustness-split.py` (§4.8) against the run-set + new summary
   (`run-coverage-report.sh`, run-set as sole dump authority). Outputs:
   `2026-07-09-{normalized-corpus-coverage,parity-support-audit,fidelity-robustness-split}.json`.

## Headline: denominators are corpus-invariant

The source-authority `occ` denominators are **effectively identical** between the local
extraction (17,894 works) and the pinned nix corpus (17,886 works):

| | constructs | change |
| --- | --- | --- |
| identical | 17 of 21 | 0 |
| `gaiji.marker` | 62,355 → 62,351 | −4 (−0.006%) |
| `gaiji.un_embed` | 144 → 141 | −3 (−2.1%, tiny mass) |
| `accent.diacritic` | 0 → 0 | — |

`ruby` (90% of mass), `jisage`, `boten`, `heading`, `tcy`, `chitsuki`, `burasage`,
`yokogumi`, `jizume` — all **byte-identical**. The 8-work count difference (local
17,894 vs pinned 17,886) accounts for the two small gaiji moves.

**This retires the study §7 "corpus caveat."** The feared "phantom gaiji −0.11 /
jisage −0.14 drops" were a **numerator** artifact (the naive nix re-measure before the
`ab-index` symlink fix), *not* a denominator drift. At the source-authority level the
denominators do not meaningfully depend on the corpus snapshot.

## Verdict unchanged (fully-pinned re-run)

**§4.7 frequency-weighted coverage** — ranking identical:

| adapter | committed (old num + local denom) | fully-pinned (repin num + pinned denom) |
| --- | ---: | ---: |
| **aozora** | 0.969 | **0.957** |
| aozora-rs | 0.938 | 0.938 |
| aozora-epub3 | 0.926 | 0.926 |
| aozora2 | 0.855 | 0.855 |
| aozora2html | 0.743 | 0.743 |

Only aozora moves (−0.012). Because the denominators are corpus-invariant (<0.001
weighted effect), this delta is the **numerator** — the repin `1a4f864` dump vs the
prior `5df2cfa5` dump — exactly the parser effect the §7 controlled experiment already
bounded at ≤0.05. aozora remains clearly #1.

**§4.8 fidelity/robustness split** — robustness completion rates **identical** (dump-only);
fidelity ordering preserved (aozora2html > aozora2 > aozora > epub3 > aozora-rs under
both best-attested and reference denominators); reconciliation passes. The ~0.011
shifts are the repin dump (FRS uses best-attested/reference denominators derived from
the dumps, not the source denominators, so §1's denominator change is a no-op here).

**§4.9 parity** — every headline finding holds: aozora drops `jizume` + `yokogumi`;
`chitsuki` dropped by 3/5; `burasage` by 2; `bold/italic` + `warichu` universal.

## Resolved: the §2 denominator gaps

- **keigakomi now has a denominator (717)** and is scored in the fidelity summary:
  aozora2 0.319 (lossy), aozora-rs 0.279 (lossy) — both partial. It moves out of the
  classifier's `skipped` set. (`parity-support-audit.py` keeps its own fixed matrix
  scope and still lists it under `unscoreable`; adding the row there is a trivial
  optional follow-up now that a denominator exists.)
- **yokogumi (3,690) / jizume (3,239) confirmed corpus-invariant.** The §2 "yokogumi
  denominator is ~20× the observed block-starts" concern is explained, not a defect:
  the source-authority matrix pattern counts **all** marker forms (start + end +
  inline `の`-form + compounds), whereas the §2 grep counted only `［＃ここから…］`
  starts. Same for keigakomi (717 vs 200 starts).

## Follow-ups surfaced

- A **reproducibility hole** in dump selection (stale `AB_*_AAT_DIR` profile env vars
  silently override the run-set) was found while running this — see
  `2026-07-09-fidelity-workflow-integration.md`. All numbers here were produced with
  the run-set as sole authority (overrides neutralized).
- The study §7 corpus caveat can be tightened to "denominators verified corpus-invariant
  (§1); the only cross-snapshot movement is the parser dump, bounded by the controlled
  experiment."
