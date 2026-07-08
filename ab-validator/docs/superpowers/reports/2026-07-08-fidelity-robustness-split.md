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

## Signature refinements applied here (and back-propagated to §4.7)

Auditing the full-corpus vocabulary for constructs that were *not* normalized or
correctly mapped surfaced three fixes, applied to the shared signatures so §4.7 and
§4.8 stay identical (the reconciliation assertion enforces it):

1. **Indentation harmonized.** aozora2 uses `jisage_line` as a *primary* indentation
   encoding (65,749 nodes; in a 400-work sample, more works are `jisage_line`-only
   than `jisage_block`-only) — the same 字下げ construct the others emit as
   `jisage_block`. Folding `jisage_line` into the jisage signature lifts aozora2's
   corpus indentation from **0.38 → ~1.0**; no other adapter loses (none used
   `jisage_line`). This was the one materially distorted row.
2. **gaiji added** (62,355 source occ). Previously excluded entirely; it is
   boten-scale mass. Adding it reveals **aozora-epub3 emits zero gaiji** — a real gap
   that was invisible in the old coverage number.
3. **`midashi` heading** encoding added to the heading signature (aozora2 emits 122
   headings this way). Negligible but correct.

## Method

Same union recognition signatures and construct weights as
`normalized-corpus-coverage.py` (full-corpus vocabulary audit, 2026-07-08, plus the
three refinements above) — the per-work counts summed over all works are **asserted
equal** to the full-corpus `num` values, so the walker is provably the same
instrument and the two scripts cannot silently drift.

- **Unit = the source input file** `<workid>-<hash>.json`. The hash is source-derived,
  so basenames are identical across adapters (verified). ~236 works have >1 input
  file; keying on workid alone would collapse them and under-count (it did, until
  caught by the reconciliation assertion).
- **Completion** = the adapter emitted a file that parses to a **non-empty** `blocks`
  array. A parseable-but-empty file is a representation failure, not a completion.
- **Robustness** = completed / union (union = every file any adapter completed =
  17,886), plus the ruby mass carried by each adapter's missing files.
- **Fidelity** = frequency-weighted coverage recomputed over **I = the intersection
  of works all five parsers completed** (17,518 works), scoring every parser on the
  identical input set so robustness differences cancel.

No per-work *source-occurrence* denominator exists (`build-inputs.json` holds
corpus-wide totals only), so fidelity is reported under two reproducible denominators:

- **reference (aozora-pipeline)** — *primary*. `D(w,c) = aozora's count`; "share of
  reference-visible markup each adapter reproduces." aozora is the recommended base
  and a per-occurrence-faithful emitter for the dominant constructs, and this
  denominator is immune to *other* adapters' over-emission. It cannot rank aozora
  itself (1.0 by construction), and it inherits aozora's `containerOpen`
  over-attribution on the indentation row.
- **best-attested (max)** — *cross-check*. `D(w,c) = max over adapters`.
  Adapter-neutral and the only denominator that ranks aozora too, but a single
  over-emitter inflates the bar: aozora-rs emits gaiji at ~1.8× source and per-line
  adapters inflate jisage, so the **gaiji and indentation rows are unreliable here**
  — read those two from §4.7 (source-anchored) or the reference column.

The two denominators **bracket** the truth: tight where node counts are commensurable
(ruby, boten, heading, tcy, figure — 93% of mass), wide only on gaiji/indentation.

## Robustness (completion over 17,886 works)

| adapter | completed | rate | missing | of which empty | **ruby mass missed** |
| --- | ---: | ---: | ---: | ---: | ---: |
| aozora (pipeline) | 17,886 | **1.0000** | 0 | 0 | 0.00% |
| aozora-rs | 17,886 | **1.0000** | 0 | 0 | 0.00% |
| aozora-epub3 | 17,815 | 0.9960 | 71 | 29 | 1.12% |
| aozora2 (core) | 17,856 | 0.9983 | 30 | 0 | **13.58%** |
| aozora2html | 17,584 | 0.9831 | 302 | 105 | **24.67%** |

The completion *rate* understates the damage because the missed works are the *largest*:

- **aozora2's 30 missing works hold 13.6% of all ruby** — they are the biggest works
  in the corpus (the largest alone has 75,736 ruby occurrences). This is the same
  timeout-on-giants pathology the performance sample caught (§4.6, 2/6 timeouts)
  surfacing as a robustness gap — an independent corroboration.
- **aozora2html misses 302 works (197 no output + 105 empty) holding 24.7% of ruby.**
  This is the entirety of its coverage deficit.
- **aozora-pipeline and aozora-rs complete every work** — perfect robustness.

## Fidelity (intersection I = 17,518 works)

Weighted, both denominators:

| denominator | aozora | aozora2 | aozora-rs | aozora2html | aozora-epub3 |
| --- | ---: | ---: | ---: | ---: | ---: |
| **reference (primary)** | 1.000‡ | 0.973 | **0.950** | **0.974** | 0.952 |
| best-attested (cross-check) | 0.953 | 0.955 | **0.925** | **0.960** | 0.930 |

‡ aozora = 1.0 by construction under its own denominator. Both agree: **aozora2html
highest, aozora-rs lowest**; the middle cluster is within 0.02.

Per-construct (best-attested; gaiji/indentation rows granularity-noisy — see method):

| construct | weight | aozora | aozora2 | aozora-rs | aozora2html | aozora-epub3 |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| ruby | 85.8% | 0.999 | 0.990 | 0.987 | 0.990 | 0.973 |
| boten | 4.3% | 0.999 | 0.854 | 0.811 | 0.870 | 0.860 |
| jisage (harmonized) | 3.9% | 0.586 | 0.690 | 0.296 | 0.812 | 0.899 |
| gaiji ✦ | 2.7% | 0.357 | 0.379 | 0.924 | 0.414 | 0.000 |
| heading | 2.0% | 0.856 | 0.958 | 0.224 | 0.977 | 0.845 |
| bousen | 0.5% | 0.000† | 0.981 | 0.152 | 0.998 | 0.964 |
| font_size | 0.4% | 0.000† | 0.997 | 0.007 | 1.000 | 0.000 |
| tcy | 0.3% | 0.972 | 0.884 | 0.000 | 0.000 | 0.791 |
| figure | 0.1% | 1.000 | 0.972 | 0.940 | 0.972 | 0.074 |

**On ruby (86% of mass) all five sit at 0.97–0.999** — the entire §4.7 ruby spread
was missing works, not lossiness. ✦ gaiji best-attested is corrupted by aozora-rs
over-emission (1.8×); its true absolute rates are §4.7's aozora 0.77 / aozora2 0.82 /
aozora-rs ~1.0(cap) / aozora2html 0.57 / **epub3 0.00**. † aozora bousen/font_size are
folded/dropped.

## The two inversions (this is the finding)

| adapter | corpus coverage | → | intersection fidelity (ref) | Δ |
| --- | ---: | :-: | ---: | ---: |
| aozora2html | 0.743 (**5th**) | → | 0.974 (**1st**) | **+0.231** |
| aozora2 | 0.855 | → | 0.973 | +0.118 |
| aozora-epub3 | 0.926 | → | 0.952 | +0.026 |
| aozora (pipeline) | 0.969 | → | 1.000‡ | — |
| aozora-rs | 0.938 (**2nd**) | → | 0.950 (**5th**) | +0.012 |

1. **aozora2html is the *most faithful* parser per work (0.974), despite ranking
   dead last on corpus coverage (0.743).** Its deficit is 100% robustness: it fails
   on 302 ruby-heavy works. On works it completes its representation is the best
   measured — ruby ~0.99, and it leads bousen/font_size/heading among small-mass
   constructs.
2. **aozora-rs's #2 corpus-coverage rank was flattered by perfect robustness.** On
   equal inputs it is the *least* faithful (0.950 ref / 0.925 best-attested): tcy
   0.000, bousen 0.152, heading 0.224 are genuine parser gaps, not missing works.
3. **On ruby (86% of mass) all five parsers are 0.97–0.999 on the intersection** —
   confirming §4.7 finding #6 across the whole corpus, not one adapter pair.

## Consequence for the fork decision — strengthened, not overturned

The recommendation to base the new parser on **aozora-pipeline** is *reinforced* by
separating the axes:

- **aozora-pipeline** is the only parser top-tier on **both** axes: fidelity ~0.97
  (tied with aozora2html for the lead) **and** robustness 1.000 (perfect) **and**
  fastest with no timeouts (§4.6). It is the Pareto-optimal choice.
- **aozora2html** has marginally the best fidelity but **catastrophic robustness**
  (misses 24.7% of ruby mass) — disqualifying for a production parser, and its
  fidelity is in non-canonical encodings (sesame_dot, unmapped-h4/h5) needing the
  AAT normalization anyway.
- **aozora-rs** has perfect robustness but the **weakest fidelity** — its coverage
  lead was a robustness artifact; the heading/tcy/bousen gaps are real and confirm
  it is a throughput-only candidate.
- **aozora2 (core)** is undercut on a *third* independent axis: its 30 missing works
  are the timeout giants, so its robustness gap and its performance pathology are the
  same defect.

The two axes are genuinely orthogonal, and only aozora-pipeline is strong on both.

## Reproducibility

```
# regenerate §4.7 first (defines the reconciliation baseline), then this split:
python3 reports/aat-fidelity/normalized-corpus-coverage.py \
  docs/superpowers/reports/2026-07-08-corpus-adapter-fidelity.summary.json \
  > docs/superpowers/reports/2026-07-08-normalized-corpus-coverage.json
python3 reports/aat-fidelity/fidelity-robustness-split.py \
  docs/superpowers/reports/2026-07-08-normalized-corpus-coverage.json \
  > docs/superpowers/reports/2026-07-08-fidelity-robustness-split.json
# stderr prints per-adapter completion, the reconciliation verdict (must be OK),
# and the intersection size. ~2.5 min each over the 5 full-corpus AAT dumps.
```

## Threats specific to this split

- **No source-authority denominator.** Fidelity uses reference and best-attested
  proxies; both bracket but neither *is* the source-occurrence truth. The ranking is
  invariant across them, and ruby (86% of mass) is barely proxy-sensitive.
- **Granularity-incommensurable constructs.** Node counts are not comparable across
  adapters for **gaiji** (aozora-rs over-emits ~1.8×) and **indentation** (aozora2/
  epub3 emit per-line; aozora `containerOpen` over-attributes). For those two rows the
  §4.7 source-anchored, corpus-capped coverage is authoritative; §4.8's per-work
  proxies are noisy there (disclosed per construct) but each is <4% of weight.
- **Completion = non-empty blocks** is a coarse robustness proxy. A file that parses
  but silently truncates a long body counts as completed and shows up instead as
  lowered fidelity on those works — a conservative split (partial output is charged
  to fidelity, not robustness).
- **Untracked constructs remain** (chitsuki/burasage, bold/italic, kaeriten, warichu,
  accent, keigakomi, yokogumi). Adding each fairly needs its own vocabulary audit
  (aozora emits chitsuki as `alignEnd`, bold as `lineBold`, kaeriten as a marker —
  all folded); they total a few percent of mass and are the next completeness step.
