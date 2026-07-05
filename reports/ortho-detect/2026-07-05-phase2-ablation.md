# Ortho-Detect ML Ablation (Phase 2)

Date: 2026-07-05
Gold set: `data/ortho-gold/sentences.jsonl` (n=457, accept=228, reject=229)

## Ablation: full-feature vs character-only

**v1 reality (Branch B):** TokenFeatures (oov_count, oov_ratio,
proper_noun_char_ratio) are dead — Vibrato's `LexType::Unknown` never
fires on katakana prose because Unidic-CWJ has dictionary entries for
katakana particles/copulas (see `reports/ortho-detect/2026-07-05-sudachi-baseline.md`).
Therefore **character-only IS the only viable feature set in v1.** The
Vibrato-coupling (`OrthoTokenizer` trait + `ortho_compat.rs`) is a no-op
for ML purposes in v1 and can be DELETED if the ML detector becomes the
default.

**Train-set accuracy (small data, no hold-out split):** 0.9059

## Bias caveat (read before trusting these numbers)

This accuracy is against **bootstrap labels** (the Python
`is_katakana_sentence` heuristic labeling itself, via
aozora-corpus-generator — see `scripts/ortho-gold/bootstrap_label.py`).
It measures whether the linear model can reproduce the heuristic's
character cascade, NOT real-world detection quality. A perfectly-trained
model on these labels can at best tie the heuristic; it cannot exceed it
on the labeled distribution. Real evaluation requires the
human-annotated gold set (Task 9 — the documented finishing input).

## Recommended action

- Delete `OrthoTokenizer` trait + `ortho_compat.rs` + double-dict-load
+ `oov_*` config fields (investigation report items C1, C3, C4).
- Keep `MlLogisticRegression` character-only as the canonical detector.
- Re-run this ablation when TokenFeatures become live (requires exposing
Vibrato `LexType::Unknown` through the trait).
