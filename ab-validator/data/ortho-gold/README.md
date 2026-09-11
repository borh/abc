# Ortho-Detect Gold Data

Evaluation datasets and models for the `ab-ortho-detect` orthographic
normalization layer (katakana→hiragana for pre-war Japanese prose).

## Dataset provenance

| File | Description |
|---|---|
| `candidates.jsonl` (1068 records) | Full candidate pool extracted from 46 Aozora fiction works (9 author-prefixes). Each record has `work_id`, `sentence`, `katakana_ratio`, `hiragana_count`, `total_chars`, and a bootstrap `label` from the Python `aozora-corpus-generator` heuristic. |
| `sample-300-unlabeled.jsonl` (300 records) | Corpus-representative sample drawn from the candidate pool by `scripts/ortho-gold/sample_300.py`. Each record has a `label` field currently set to the bootstrap value; overwrite it with your own `"accept"` or `"reject"`. The original is preserved in `bootstrap_label`. |
| `sentences-llm-300.jsonl` (300 records) | **LLM-labeled evaluation set** (single-annotator, NOT human ground truth). Labeled by an LLM applying the spec's intent rubric. 10% spot-check agreement 0.933. Used for the heuristic tuning and ML cross-validation. |
| `sentences.jsonl` (457 records) | Bootstrap-labeled set. 58% Tanizaki, inherited from an unstratified candidate pool. Superseded by `sentences-llm-300.jsonl` for evaluation; retained for historical comparison. |
| `sentences-human-sample.jsonl` (50 records) | Human-annotated probe. First human-labeled subset; used to discover the 0.636 recall floor. |
| `models/model-v1.bin` | ML model trained on the bootstrap labels (457 records). Superseded by `model-gold-300.bin`. |
| `models/model-v2-human.bin` | ML model trained on the 50-sentence human probe. Small-data quality; not for production use. |
| `models/model-gold-300.bin` | ML model trained on the 300 LLM labels. `model_hash: b5c460c7f13cf4698aea6dcd0e9e6358aadadeeb748f56d7aff776cf6666b978`. 5-fold CV mean recall 0.959. |

## Labeling rubric (for human reviewers)

Three labels separate two orthogonal concerns: (1) is the katakana usage
historical orthography? (2) would converting katakana→hiragana improve
tokenization, regardless of the historical question?

### `accept`: Historical kanji-katakana-majiri prose

Katakana serves the grammatical role of hiragana (particles は/が/の/を,
okurigana, copulas だ/である, auxiliary verbs タ/テ/ナイ). The sentence is
running narrative or dialogue with grammatical structure. The presence of
kanji does NOT disqualify (kanji+katakana-majiri is the canonical form);
what matters is that the katakana is doing grammatical work. Normalizing
would improve tokenization, AND the usage is a historical-orthography fact
worth recording.

### `normalize`: Emphatic, stylistic, or robot-speech katakana

Katakana is used for emphasis, robot/alien speech, diary-entry delineation
(e.g. in Tanizaki's *Kagi*), or other stylistic effect; not historical
orthography. However, converting to hiragana WOULD improve tokenization
(e.g. Vibrato fails to segment `ボクヲミタコト` and `イイナサイ` as single
unknown nouns, but `ぼくをみたこと` and `いいなさい` would segment correctly).

**Record as `normalize`, not `accept`.** The normalization should happen
before tokenization, but the katakana usage is NOT a historical-orthography
fact; it is a separate phenomenon that should not be conflated with
historical kana use in annotation.

### `reject`: No normalization benefit

(a) exclamations/interjections (`「アハハ」`, `「ヨイショ」`, `「ハテナ」`);
(b) onomatopoeia (`ウハハハハ`, `フフフ`, `アッハッハ`);
(c) single-word utterances or fragments (`「スウプ。」`, `「マイ、チャイルド。」`);
(d) Buddhist chants or foreign-loan vocative fragments with no grammatical
structure (`「ナムアムダブツ」`); (e) sentences where the katakana is
lexical (proper nouns, foreign words) rather than grammatical;
(f) emphatic exclamations where conversion wouldn't meaningfully improve
tokenization (`シマッタ！`, `オロカ！`).

### Edge-case guidance

- If the sentence has ≥ 15 chars, `hiragana_count == 0`, and reads as
  continuous narrative → prefer `accept` unless it's clearly robot/alien
  speech or emphatic styling.
- If the sentence is emphatic/robot/stylistic katakana with grammatical
  structure that Vibrato can't segment → prefer `normalize`.
- If < 12 chars or starts with `「` and contains no verb → prefer `reject`.
- Document genuinely uncertain calls by setting `label_notes` to a one-line
  reason.

## How to produce a fresh human-labeled set

1. **Sample** (optional; `sample-300-unlabeled.jsonl` is ready to label):

   ```bash
   cd ab-validator
   python scripts/ortho-gold/sample_300.py
   # Produces data/ortho-gold/sample-300-unlabeled.jsonl
   ```

   The sampler is corpus-representative (proportional to candidate-pool
   author shares, per-author minimum of 4). The seed is fixed (20260705)
   for reproducibility.

2. **Label**: Open `data/ortho-gold/sample-300-unlabeled.jsonl` and overwrite
   the `label` field in each record with your verdict: `"accept"`, `"normalize"`,
   or `"reject"` (see rubric above). The original bootstrap value is already
   saved in `bootstrap_label`; do not change that field. Optionally add
   `label_notes` for edge cases. Set `labeler` to your name. When done, save as
   `data/ortho-gold/sentences-human-REVIEWER.jsonl`.

3. **Measure recall**:

   ```bash
   export AB_VIBRATO_DICT=$(pwd)/dictionary/compiled/unidic-cwj-202512.dic.zst
   # Heuristic:
   cargo run -p ab-ortho-detect --example detect_sentences \
     < data/ortho-gold/sentences-human-REVIEWER.jsonl \
     > /tmp/gold-human-heur.jsonl
   bb scripts/ortho-gold/recall_floor.clj /tmp/gold-human-heur.jsonl
   # ML (if model is retrained on the human set):
   cargo run -p ab-ortho-detect --example detect_ml -- \
     data/ortho-gold/models/model-human.bin \
     < data/ortho-gold/sentences-human-REVIEWER.jsonl \
     > /tmp/gold-human-ml.jsonl
   bb scripts/ortho-gold/recall_floor.clj /tmp/gold-human-ml.jsonl
   ```

The trainer and cross-validator that produced `models/` were retired with the
research layer; the archived repository holds them.

## Historical evaluation (2026-07-05)

The 300 LLM labels yielded heuristic recall 0.939, precision 0.930, and F1
0.935. Five-fold cross-validation of logistic regression yielded mean recall
0.959, precision 0.967, and F1 0.963. These are measurements against a single
LLM annotator, not independently human-verified generalization estimates.
