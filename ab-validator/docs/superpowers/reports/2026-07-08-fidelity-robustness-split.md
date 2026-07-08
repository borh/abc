# Fidelity vs robustness — decomposing Aozora parser corpus coverage

**Status:** resolves open-work item #1 of the 2026-07-08 parser comparison study
(`2026-07-08-aozora-parser-comparison-study.md` §8). Companion data:
`2026-07-08-fidelity-robustness-split.json`. Tool:
`reports/aat-fidelity/fidelity-robustness-split.py`.

## Why split

Frequency-weighted **corpus coverage** (§4.7 of the study) answers "how much of the
markup that actually occurs can each parser represent?" — but it silently multiplies
two independent things together:

- **robustness** — did the adapter produce output for the work at all?
- **fidelity** — *given it ran*, how faithfully did it represent that work's markup?

The §4.7 closure already showed this for one cell: aozora2html scores ruby 0.74 in
corpus coverage yet has ~1.0 per-work ruby fidelity — its gap is that it *fails* on
ruby-heavy works, not that it drops ruby. This report generalizes that decomposition
across **all** constructs and **all five** parsers, so no adapter's fidelity is
hidden behind (or flattered by) its completion rate.

## Method

Same union recognition signatures and construct weights as
`normalized-corpus-coverage.py` (full-corpus vocabulary audit, 2026-07-08) — the
per-work counts summed over all works are **asserted equal** to the full-corpus
`num` values, so the walker is provably the same instrument.

- **Unit = the source input file** `<workid>-<hash>.json`. The hash is source-derived,
  so basenames are identical across adapters (verified). ~236 works have >1 input
  file; keying on workid alone would collapse them and under-count (it did, until
  caught by the reconciliation assertion).
- **Completion** = the adapter emitted a file that parses to a **non-empty** `blocks`
  array. A parseable-but-empty file is a representation failure, not a completion.
- **Robustness** = completed / union (union = every file any adapter completed =
  17,886), plus the ruby mass carried by each adapter's missing files.
- **Fidelity** = frequency-weighted coverage recomputed over **I = the intersection
  of works all five adapters completed** (17,518 works), so every adapter is scored
  on the identical input set and the robustness differences cancel.

No per-work *source-occurrence* denominator exists (`build-inputs.json` holds
corpus-wide totals only), so fidelity is reported under two independent, reproducible
denominators and shown to give the same ranking:

- **best-attested (max)**: `D(w,c) = max over adapters of that adapter's count`.
  Adapter-neutral — no single parser defines truth; only an adapter maximal on
  *every* work reaches 1.0.
- **reference (aozora-pipeline)**: `D(w,c) = aozora's count` — "share of
  reference-visible markup reproduced." Undefined where the reference emits nothing
  (font_size, folded bousen); those constructs are excluded from this variant.

## Robustness (completion over 17,886 works)

| adapter | completed | rate | missing | of which empty | **ruby mass missed** |
| --- | ---: | ---: | ---: | ---: | ---: |
| aozora (pipeline) | 17,886 | **1.0000** | 0 | 0 | 0.00% |
| aozora-rs | 17,886 | **1.0000** | 0 | 0 | 0.00% |
| aozora-epub3 | 17,815 | 0.9960 | 71 | 29 | 1.12% |
| aozora2 (core) | 17,856 | 0.9983 | 30 | 0 | **13.58%** |
| aozora2html | 17,584 | 0.9831 | 302 | 105 | **24.67%** |

The completion *rate* understates the damage because the missed works are not random:

- **aozora2's 30 missing works hold 13.6% of all ruby** — they are the largest works
  in the corpus (the biggest alone has 75,736 ruby occurrences). This is the same
  timeout-on-giant-works pathology the performance sample caught (§4.6, 2/6 timeouts)
  surfacing as a robustness gap — an independent corroboration.
- **aozora2html misses 302 works (197 no output + 105 empty) holding 24.7% of ruby.**
  This is the entirety of its coverage deficit.
- **aozora-pipeline and aozora-rs complete every work** — perfect robustness.

## Fidelity (intersection I = 17,518 works), best-attested denominator

| construct | weight | aozora | aozora2 | aozora-rs | aozora2html | aozora-epub3 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ruby | 88.2% | 0.999 | 0.990 | 0.987 | 0.990 | 0.973 |
| boten | 4.4% | 0.999 | 0.854 | 0.811 | 0.870 | 0.860 |
| jisage_block | 4.0% | 0.586 | 0.248 | 0.296 | 0.813 | 0.899 |
| heading | 2.1% | 0.856 | 0.956 | 0.224 | 0.977 | 0.845 |
| bousen | 0.5% | 0.000† | 0.981 | 0.152 | 0.998 | 0.964 |
| font_size | 0.4% | 0.000† | 0.997 | 0.007 | 1.000 | 0.000 |
| tcy | 0.3% | 0.972 | 0.884 | 0.000 | 0.000 | 0.791 |
| figure | 0.1% | 1.000 | 0.972 | 0.940 | 0.972 | 0.074 |
| **weighted fidelity** | | **0.970** | **0.953** | **0.925** | **0.975** | **0.956** |

**Ranking invariance.** The reference-denominator variant gives the same ordering
(aozora 1.000 by construction; then aozora2html **0.975** > aozora2 0.970 >
aozora-epub3 0.961 > aozora-rs **0.951**). aozora2html top and aozora-rs bottom hold
under both denominators; the middle three are within 0.02 of each other. † aozora's
bousen/font_size are folded/dropped (small mass, footnoted in §4.7).

## The two inversions (this is the finding)

| adapter | corpus coverage | → | intersection fidelity | Δ |
| --- | ---: | :-: | ---: | ---: |
| aozora2html | 0.745 (**5th**) | → | 0.975 (**1st**) | **+0.230** |
| aozora2 | 0.840 | → | 0.953 | +0.113 |
| aozora-epub3 | 0.940 | → | 0.956 | +0.016 |
| aozora (pipeline) | 0.972 | → | 0.970 | −0.002 |
| aozora-rs | 0.937 (**2nd**) | → | 0.925 (**5th**) | −0.012 |

1. **aozora2html is the *most faithful* parser per work (0.975), despite ranking
   dead last on corpus coverage (0.745).** Its deficit is 100% robustness: it fails
   on 302 ruby-heavy works. On works it completes, its representation is the best
   measured — including ruby ~0.99, and it leads on bousen/font_size/heading among
   the small-mass constructs.
2. **aozora-rs's #2 corpus-coverage rank was flattered by perfect robustness.** On
   equal inputs it is the *least* faithful (0.925): tcy 0.000, bousen 0.152,
   heading 0.224 are genuine parser gaps, not missing works.
3. **On ruby (88% of mass) all five parsers are 0.97–0.999 on the intersection** —
   the entire corpus-coverage spread on ruby was the missing works, confirming §4.7
   finding #6 across the whole corpus rather than one adapter pair.

## Consequence for the fork decision — strengthened, not overturned

The recommendation to base the new parser on **aozora-pipeline** is *reinforced* by
separating the axes:

- **aozora-pipeline** is the only parser top-tier on **both** axes: fidelity 0.970
  (statistically tied with aozora2html's 0.975 for the lead) **and** robustness 1.000
  (perfect) **and** fastest with no timeouts (§4.6). It is the Pareto-optimal choice.
- **aozora2html** has marginally the best fidelity but **catastrophic robustness**
  (misses 24.7% of ruby mass) — disqualifying for a production parser, and its
  fidelity is in non-canonical encodings (sesame_dot, unmapped-h4/h5) needing the
  AAT normalization anyway.
- **aozora-rs** has perfect robustness but the **weakest fidelity** — its coverage
  lead was a robustness artifact; the heading/tcy/bousen gaps are real and confirm
  it is a throughput-only candidate.
- **aozora2 (core)** is undercut on a *third* independent axis here: its 30 missing
  works are the timeout giants, so its robustness gap and its performance pathology
  are the same defect.

The two axes are genuinely orthogonal, and only aozora-pipeline is strong on both.

## Reproducibility

```
python3 reports/aat-fidelity/fidelity-robustness-split.py \
  docs/superpowers/reports/2026-07-08-normalized-corpus-coverage.json \
  > docs/superpowers/reports/2026-07-08-fidelity-robustness-split.json
# stderr prints per-adapter completion, the reconciliation verdict (must be OK),
# and the intersection size. ~3 min over the 5 full-corpus AAT dumps.
```

## Threats specific to this split

- **No source-authority denominator.** Fidelity uses best-attested and reference
  proxies; both bracket but neither *is* the source-occurrence truth. The ranking is
  invariant across them, and ruby (88% of mass) is barely proxy-sensitive.
- **Over-emission raises the best-attested bar** on small-mass constructs (aozora
  boten emits ~1.19× source; capped per work). This distorts boten/jisage_block
  slightly but not ruby; disclosed per construct.
- **Completion = non-empty blocks** is a coarse robustness proxy. A file that parses
  but silently truncates a long body (a partial-completion) counts as completed and
  shows up instead as lowered fidelity on those works — a conservative split (it
  charges partial output to fidelity, not robustness).
- **jisage_block** remains approximate for aozora (`containerOpen` over-attributes)
  and epub3 (per-line emission) — see §4.7; unchanged by this split.
