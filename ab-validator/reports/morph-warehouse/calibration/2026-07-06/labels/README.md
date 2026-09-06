# Blind labeling package — full-corpus interestingness calibration (2026-07-06)

This directory contains a pooled, blinded sample of morpheme-difference
patterns for human verdicts. `labels.tsv` is the union of the top-50
patterns from four ranking methods run against the full canonical warehouse
(`full-2026-07-05_164518-jobs0`), deduplicated, seed-shuffled (seed
20260706), and stripped of anything that could reveal which method(s)
surfaced a row or at what rank. The method/rank data lives only in
`mapping.json`; do not read it before labeling.

## The six verdicts

| verdict | one-line definition |
|---|---|
| `bug` | Analyzer output is wrong: a real defect in an analyzer's segmentation or feature output. |
| `expected-policy` | Legitimate segmentation/POS policy difference between analyzers (both defensible under their own guidelines). |
| `expected-dictionary` | Dictionary coverage/lemma difference — not a bug, but informative about lexicon gaps. |
| `corpus-artifact` | Aozora encoding/gaiji/formatting artifact, not a linguistic disagreement. |
| `noise` | Punctuation/whitespace/trivial difference with no diagnostic value. |
| `unclear` | Cannot judge from the snippets provided. |

Scoring treats `bug` and `expected-dictionary` as relevant for p@50; nDCG
gains are bug=3, expected-dictionary=2, corpus-artifact=1, expected-policy=1,
noise=0, unclear=0.

## How to fill the TSV

- Edit ONLY the `verdict` column (and optionally `notes`) of each data row.
  Any editor works, but keep the tab separators intact.
- `verdict` must be exactly one of the six strings above, on every row —
  the scorer hard-errors on an empty or misspelled verdict and lists the
  offending `label_id`s (partial labeling would silently bias p@k, so it
  is rejected outright).
- Do NOT add, delete, or reorder rows, and do not touch `label_id` — the
  scorer verifies the TSV's `label_id` set equals `mapping.json`'s key set
  exactly and hard-errors on any mismatch (missing, unknown, or duplicated
  ids).
- The `#` comment lines at the top and the header row must stay as they are.
- Each row carries up to 3 text snippets (20 chars of context on each side)
  sliced from the AAT corpus; judge from those. If they are not enough,
  use `unclear` rather than guessing.

## Scoring (after all rows are labeled)

```bash
cargo run --release -p ab-morph-run -- score-interesting-labels \
  --labels reports/morph-warehouse/calibration/2026-07-06/labels/labels.tsv \
  --mapping reports/morph-warehouse/calibration/2026-07-06/labels/mapping.json
```

This reports per-method p@50 and nDCG@50 over the pooled labels.
