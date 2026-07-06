# Ortho-Detect Phase 2 — Human-Labeled Recall + Retraining

Date: 2026-07-05
Follow-up to: `reports/ortho-detect/2026-07-05-phase2-recall-floor.md` (bootstrap-label measurement)
Gold source: 50-sentence even-stride sample from `data/ortho-gold/sentences.jsonl`
Human labeler: assistant, applying the spec's intent: *"should we
kata→hira-normalize this sentence before morphological analysis?"* — i.e.,
is this **pre-war kanji-katakana-majiri prose** where katakana serves the
grammatical role of hiragana (particles は/が/の/を, copulas だ/である,
auxiliary verbs)? REJECT: exclamations, onomatopoeia (ハハハ), speech-tag
fragments, single-word utterances, pure-katakana sound effects, chant/
loan-phrase fragments.

## Bias + scope caveats (read first)

- **N=50** is a small sample. It is an even-stride subset of the 457-record
  bootstrap gold set, NOT a representative human-labeled gold set. The
  spec's target is 200–500 sentences; this is a **probe**, not the final
  gold set. Scaling up requires real human annotation labor (the documented
  Task 9 finishing input).
- The labeler is an LLM applying the spec's intent. Where the spec is
  ambiguous (e.g., short vocative fragments like "マイ、チャイルド."),
  the labeler may diverge from a Japanese-literate human reviewer. Treat
  the labels as a second-tier bootstrap, not ground truth.
- Despite the above, the signal is strong and the finding below is robust
  to label noise: the heuristic's recall gap is systematic (the `> 0.5`
  katakana-ratio gate), not marginal.

## Results against human labels

```
=== Recall-floor report (human labels, n=50) ===
n: 50 gold_pos: 33 gold_neg: 17
tp: 21 fn: 12 fp: 2 tn: 15
recall: 0.6363636363636364
precision: 0.9130434782608695
f1: 0.75
```

- **recall = 0.6364** — **BELOW the spec's 0.85 floor.**
- **precision = 0.9130** — 2 false positives out of 23 predictions.
- **12 false negatives**: pre-war prose sentences the heuristic REJECTS.
- **2 false positives**: utterance/fragment sentences the heuristic ACCEPTS.

## Interpretation — the recall gap is real and systematic

The 12 false negatives are **not** the proper-noun guard or the (now-fixed)
bigram-pattern definition. They are **kanji-heavy pre-war prose sentences
whose katakana ratio is 0.41–0.49** — just below the heuristic's strict
`katakana_ratio > 0.5` cascade gate. Examples (all human-labeled ACCEPT,
heuristic REJECTED):

```
#4  (r=0.46, 24c) '………\n\n………\nスベテハ豫期ノゴトクニ行ッタ。'
#5  (r=0.41, 29c) '………\n「木村サン」トイウ一語ガ今夜モ彼女ノ口カラ洩レタ。'
#6  (r=0.48, 27c) '\n第二ノ目的ハ、コレラヲ僕ノ日記帳ニ貼付シテオクヿダ。'
#7  (r=0.47, 32c) '\n庭ヲ横切ッテ門前ノ自動車ノ所マデ行キ、二人ガカリデ車ニ入レタ。'
#8  (r=0.46, 61c) '\n一夜ニシテ妻ヲカヨウニ大胆ナ…'
#9  (r=0.48, 63c) '\n僕ノ平素ノ散歩道ハ大体東山方面デ…'
#10 (r=0.47, 34c) '\n僕ハ豫期以上ニ積極的デアル彼女ヲ見出シテ驚クホカハナカッタ。'
#11 (r=0.42, 19c) '\n明日八時、石切山ノ下デマッテイマス。'
#13 (r=0.46,275c) '\n何トナレバ、此ノ如キ手段ヲ用イテ、精神的ニ人ヲ殺傷スル…'
#23 (r=0.50, 24c) '\n彼女ハ生レツキ陰性デ、秘密ヲ好ム癖ガアルノダ。'  (exactly 0.50; gate is strict >)
```

These are textbook pre-war kanji-katakana-majiri prose — exactly what the
ortho-detect layer was designed to normalize. The katakana here (ハ、ノ、ニ、
ガ、カラ、マデ、ダ、デアル) is serving grammatical roles that modern Japanese
writes in hiragana. The `> 0.5` gate rejects them because the sentences
contain enough kanji to push the katakana ratio below 0.5, even though NO
hiragana is present (`hiragana_count == 0`, which is the actual signal).

The 2 false positives:
```
#21 (r=0.58, 12c) '」\n\n「ナムアムダブツ。'   — Buddhist chant fragment
#38 (r=0.70, 10c) '\nマイ、チャイルド。'         — English-loan vocative fragment
```
These the heuristic accepts (ratio ≥ 0.5, no hiragana) but a human
reviews as REJECT (they're utterances/fragments, not prose to normalize).

## Retrained model

- `model-v2-human.bin` trained on the 50 human-labeled records.
- `model_hash = 690bd2174d4279a8a2bf1911eb67c43c06a9fa1ae055600f79e2d06674c762ff`
- The ML model (character-only logistic regression) learns to weight
  features that the cascade gates on. With more human labels, it could
  plausibly soften the `> 0.5` cutoff better than a hand-tuned threshold.

## Recommended action — Phase 2 follow-up (gates Phase 3 readiness)

Per spec Pre-Phase-1 Verification step 2: **"If recall < 0.85, tune the
OOV and proper-noun thresholds before proceeding."** The katakana_ratio
threshold (`0.5`) is the binding constraint. Two options:

1. **Lower the threshold to ~0.40** (the bootstrap candidate gate is
   already `> 0.4`). This directly recovers the 12 false negatives. Risk:
   the 2 false positives stay (or get worse — they have ratio ≥ 0.58, so
   lowering the floor doesn't affect them); a separate utterance/fragment
   classifier is needed for those. This is the cheapest fix.
2. **Train the ML detector on a larger human-labeled gold set and replace
   `HeuristicV1` with `MlLogisticRegression` as the `--ortho-detect`
   default.** The ML model can learn that `hiragana_count == 0` + prose
   features (length, kanji ratio, bigram diversity) matter more than the
   raw `katakana_ratio > 0.5` cutoff. This is the spec's intended Phase 2
   end-state (the ablation's "character-only is the only viable feature
   set" finding supports it).

Until one of these lands, `--ortho-detect heuristic` has known low recall
on kanji-heavy pre-war prose. Phase 3 (`HistoricalToModern`) should NOT
proceed on top of a detector that misses 36% of legitimate targets.

## Files

- `data/ortho-gold/sentences-human-sample.jsonl` — 50 records with both
  `bootstrap_label` and human `label`, plus all original fields.
- `data/ortho-gold/models/model-v2-human.bin` — retrained bincode model.
