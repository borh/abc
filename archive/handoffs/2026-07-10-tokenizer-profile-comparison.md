# Tokenizer-profile comparison: unidic-novel vs qkana vs kindai-bungo (2026-07-10)

First tokenizer-profile comparison over the annotation join statistics,
following the workflow-backed re-run
(`docs/handoffs/2026-07-10-annotation-join-stats-rerun.md`). Three runs of
`soranoha annotation-join-stats-run`, identical except for
`tokenizer_dict` — same repin AAT dump, stride-44 sample (407 works,
188,292 ruby / 1,086 gaiji annotations), same converter and mapping, same
pinned `vibrato-tokenize` — so every rate delta isolates the dictionary.
All three runs passed with schema-valid workflow-run records
(`soranoha validate-workflow`: valid).

Committed plans: `data/annotation-join-stats-plans/aozora-repin-1a4f864-stride44-{unidic-novel,qkana,kindai-bungo}.json`
Outputs: `/db/soranoha/annotation-join-stats/aozora-repin-1a4f864-stride44-{unidic-novel,qkana,kindai-bungo}-2026-07-10/`

## Corpus-wide ruby classification rates

| class | unidic-novel | qkana | kindai-bungo |
|---|---|---|---|
| aligned-single | 58.00% | 57.80% | 57.68% |
| aligned-multi | 8.82% | 8.83% | 8.88% |
| stem-prefix | 31.24% | 31.26% | 31.40% |
| conflict | 1.94% | 2.11% | 2.04% |

## Findings

1. **Ruby-base alignment is robust to profile choice.** The three
   dictionaries stay within 0.32pp of each other on every class; conflict
   sits at 1.9–2.1% for all. The morph-warehouse comparison found
   orthography to be the dominant dictionary-disagreement axis
   (`reports/` farspark, 2026-07); that disagreement evidently moves
   token *labels* far more than token *boundaries at ruby bases*. For
   ADR 0028 purposes, join-classification rates are not a discriminator
   between these profiles.
2. **The differences are real but small and local.** 119/407 works
   (qkana) and 163/407 (kindai-bungo) shift at least one annotation's
   classification vs unidic-novel, but almost all shifts are ±1–3
   annotations per work.
3. **Where profiles do disagree, it is compound segmentation.** The
   largest outlier: `001869_59100` gains +13 conflicts (+5.9pp) under
   kindai-bungo, which segments the recurring gang name 黒襟飾組 as
   黒襟|飾|組 while unidic-novel keeps 襟飾 whole — rubies on 襟飾 then
   straddle a token boundary. Same pattern for 一瞬間 → 一|瞬間 and
   のろくさく. This is the phenomenon a tokenizer-profile *selection*
   would have to weigh, and the per-work deltas locate exactly the works
   where it matters.
4. **gaiji classifications are essentially profile-independent**
   (conflict 327–331 of 1,086 across the three runs), consistent with
   gaiji conflicts being a rendering-interplay question, not a
   tokenization one.

## Operational note

The two comparison runs executed in parallel from the same committed-plan
template with only `tokenizer_dict` changed — the marginal cost of an
additional profile is one CLI invocation plus ~1 GB of dictionary cache
under `AB_VIBRATO_CACHE_DIR`.

## Follow-ups

- Work-level sampling and the gaiji-conflict pass carry over unchanged
  from the re-run handoff.
- If a tokenizer profile becomes a request-set-level choice (ADR 0028 /
  analysis identity), the per-work conflict deltas here are the seed
  evidence set; no corpus-wide re-measurement is needed until the sampled
  works change.
