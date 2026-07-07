# Ortho-Detect Phase 2.5 — LLM-Labeled Evaluation Set + Recall Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a 300-sentence **LLM-labeled evaluation set** (not ground truth — single-annotator LLM, see Task 2 caveats) that is representative of the whole Aozora fiction corpus, retrain the ML detector on it, prove HeuristicV1 recall ≥ 0.85 against it, and (conditional on the result) either ship a tuned `HeuristicConfig` or promote `--ortho-detect ml` from EXPERIMENTAL to a stable (non-default) mode. **Code deletion (Task 8) is DEFERRED to a future phase with real human annotation** — LLM-labels cannot honestly gate irreversible deletions per the Phase 2 ADR's trust-boundary framing.

**Architecture:** LLM-assisted labeling (the assistant applies the spec's intent per-sentence) over a corpus-stratified sample that fixes the Tanizaki skew of the Phase 2 bootstrap set. The sample draws from all 9 author-prefixes with per-author caps so no single author dominates. Two detectors are measured against the gold set: `HeuristicV1` (to confirm/deny the recall floor) and `MlLogisticRegression` (to confirm/deny it as the replacement). The result drives a single branch decision: tune the heuristic OR promote the ML detector + delete `OrthoTokenizer`/`ortho_compat.rs`/dead OOV config.

**Tech Stack:** Rust (edition 2024), `ab-ortho-detect-ml` bin (train/hash), `crates/ab-ortho-detect/examples/detect_sentences.rs` (recall harness, already exists), `scripts/ortho-gold/bootstrap_label.py` (candidate extraction), babashka (`scripts/ortho-gold/recall_floor.clj` exists), `linfa-logistic 0.8`.

## Global Constraints

- Rust edition 2024. All crate code in `crates/`.
- `PlainTextDocument.text` is never mutated. Normalized text is a separate view.
- `ab-ortho-detect` is a near-leaf crate (depends on `ab-plaintext` only; `OrthoTokenizer` trait impl lives in `ab-morph-analyzers`).
- `confidence` is `Option<u8>` (0–100, None = heuristic). All annotation types derive `Eq`.
- Vibrato dictionary env: `AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst`
- The Aozora fiction selection is at `/home/bor/Projects/aozora-corpus-generator/Aozora-Bunko-Fiction-Selection-2022-05-30/Plain` (46 works, 9 author-prefixes).
- `model_hash` is SHA-256 of LE f32 weights+intercept (per spec Decision #10). A model file's hash is its identity; model files carry NO training-provenance metadata.
- The spec's recall floor is **0.85** (Pre-Phase-1 Verification step 2). Phase 2 measured 0.636 on a 50-sentence probe.

## File Structure

- `data/ortho-gold/sentences-llm-300.jsonl` — the new gold set (300 records, `{work_id, sentence, byte_offset, char_offset, label, katakana_ratio, hiragana_count, total_chars, dict_version, bootstrap_label, labeler: "llm"}`).
- `data/ortho-gold/models/model-gold-300.bin` — ML model retrained on the 300 human labels (replaces `model-v1.bin` as the canonical ML model if the ML branch is chosen).
- `scripts/ortho-gold/sample_300.py` — corpus-representative sampler (proportional to candidate-pool author shares; per-author minimum of 4).
- `scripts/ortho-gold/sweep_threshold.rs` (Task 5) — sweeps `katakana_ratio_threshold` to find the F1-maximizing value subject to recall ≥ 0.85.
- `crates/ab-ortho-detect/examples/detect_ml.rs` (Task 3) — recall harness for the ML detector (parallel to `detect_sentences.rs` which exercises HeuristicV1).
- `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md` — the measurement + branch-decision report.
- (Conditional, Task 7) `crates/ab-ortho-detect/src/heuristic.rs` — `HeuristicConfig::default()` katakana_ratio threshold tune.
- (Conditional, Task 8) deletions in `crates/ab-ortho-detect/src/lib.rs`, `crates/ab-morph-analyzers/src/ortho_compat.rs`, `crates/ab-ortho-detect/src/heuristic.rs` (dead OOV fields), `crates/ab-morph-run/src/main.rs` (EXPERIMENTAL marker removal).

---

### Task 0: Worktree + baseline confirmation

**Files:**
- Create: worktree at `.worktrees/ortho-detect-phase2.5` on branch `ortho-detect-phase2.5`

**Interfaces:** none.

- [ ] **Step 1: Create isolated worktree**

```bash
cd /home/bor/Projects/ab-validator
git fetch origin main
git worktree add .worktrees/ortho-detect-phase2.5 -b ortho-detect-phase2.5 origin/main
cd .worktrees/ortho-detect-phase2.5
```
Expected: new worktree checked out on `ortho-detect-phase2.5` off latest `origin/main`.

- [ ] **Step 2: Confirm Phase 2 artifacts are present**

```bash
ls data/ortho-gold/sentences.jsonl data/ortho-gold/models/model-v1.bin \
   reports/ortho-detect/2026-07-05-phase2-human-recall.md
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo build -p ab-ortho-detect-ml -p ab-ortho-detect 2>&1 | tail -2
```
Expected: all paths exist; both crates build clean.

- [ ] **Step 3: Confirm the existing recall harness still runs (sanity)**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences-human-sample.jsonl 2>/dev/null | wc -l
```
Expected: 50 (the Phase 2 human probe set size). If this prints 0 or the binary fails to start with `AB_VIBRATO_DICT` unset, that is an *environment* failure (set the env var), NOT evidence Phase 2 is absent from `main`. To verify Phase 2 is present, check the paths in Step 2 exist — that is the real stop condition.

- [ ] **Step 4: Commit (empty, marks task start)**

```bash
git commit --allow-empty -m "chore(ortho-detect-phase2.5): worktree + Phase 2 baseline"
```

---

### Task 1: Corpus-representative 300-sentence sampler

**Files:**
- Create: `scripts/ortho-gold/sample_300.py`

**Why this task exists:** Phase 2's bootstrap set `data/ortho-gold/sentences.jsonl` (457 records) is 58% Tanizaki, inherited from a candidate pool that's 70% Tanizaki. The original (pre-review) instinct was to cap Tanizaki to force diversity. **Data check rejected that instinct:** the candidate pool's per-author breakdown shows author IS a strong proxy for orthographic style — Tanizaki_J is 87% accept (649/747) and carries 90% of all `accept` candidates (649/721); Yumeno_K is 12% accept; Sakaguchi_A is 9% accept. Capping Tanizaki at 40% would deliberately starve the accept class, producing a gold set where most accept examples come from authors who write katakana largely in interjections/onomatopoeia rather than prose — exactly the wrong distribution for a detector whose real recall depends on catching prose-katakana.

So the sampler is **corpus-representative** (matches the candidate pool's actual author and accept/reject distribution), NOT author-balanced. The only deliberate deviation is a per-author minimum (≥4 records where available) so sparse authors with distinctive styles (Natsume_S, Oguri_M) are not lost entirely — but their natural corpus weight is otherwise preserved.

Within each author, sampling is **shuffle-with-fixed-seed + take first n** (NOT strided/round-robin). The latter is over-engineered and buggy for small allocations (when `n < len(works_for_author)`, the stride exceeds the work size and the loop picks zero records — confirmed by simulation).

**Interfaces:**
- Consumes: `data/ortho-gold/candidates.jsonl` (1068 records with `work_id`, `sentence`, `katakana_ratio`, `hiragana_count`, `total_chars`, `bootstrap_label=label` from Phase 2's bootstrap).
- Produces: `data/ortho-gold/sample-300-unlabeled.jsonl` — 300 records, sampling each author-prefix **in proportion to its candidate-pool share** (corpus-representative, no capping), with a per-author minimum of 4 where the pool allows (for sparse-author style coverage). Accept/reject ratio left to reflect the natural pool distribution (≈ 2:1 accept:reject = 721:347). Each record preserves all original fields + gains `bootstrap_label` (copied from `label`).

- [ ] **Step 1: Write the sampler**

Create `scripts/ortho-gold/sample_300.py`:
```python
#!/usr/bin/env python
"""Corpus-representative sampler for the LLM-labeled evaluation set.

Strategy (revised after data check rejected author-capping):
- Sample each author-prefix IN PROPORTION to its candidate-pool share.
  The pool is 70% Tanizaki, and Tanizaki carries 90% of all 'accept'
  candidates (649/721). Capping him would starve the accept class.
- Per-author minimum of 4 where the pool allows, for sparse-author
  style coverage (Natsume_S has 5, Oguri_M has 4 — take all available
  if below the min). Otherwise the natural corpus weight is preserved.
- Accept/reject ratio left to reflect the natural pool distribution
  (721:347 ≈ 2:1 accept:reject). Do NOT force 50/50 — the workload is
  genuinely accept-heavy.
- Within each author, SHUFFLE with a fixed seed and take the first n.
  NOT strided/round-robin — that approach is over-engineered and buggy
  for small allocations (when n < works-for-author, the stride exceeds
  work size and the loop picks zero — confirmed by simulation).

Outputs JSONL with all original fields + bootstrap_label (copy of label).
"""
from __future__ import annotations
import json
import math
import random
from collections import Counter, defaultdict
from pathlib import Path

CANDIDATES = Path("data/ortho-gold/candidates.jsonl")
OUT = Path("data/ortho-gold/sample-300-unlabeled.jsonl")
TARGET_N = 300
MIN_PER_AUTHOR = 4  # sparse-author coverage floor
SEED = 20260705  # deterministic

def author_prefix(work_id: str) -> str:
    parts = work_id.split("_")
    return "_".join(parts[:2])

def main() -> None:
    recs = [json.loads(l) for l in CANDIDATES.read_text().splitlines() if l.strip()]
    by_author: dict[str, list] = defaultdict(list)
    for r in recs:
        by_author[author_prefix(r["work_id"])].append(r)
    authors = list(by_author.keys())
    # Step 1: per-author minimum (for sparse coverage).
    alloc: dict[str, int] = {}
    for a in authors:
        alloc[a] = min(MIN_PER_AUTHOR, len(by_author[a]))
    remaining = TARGET_N - sum(alloc.values())
    # Step 2: distribute remaining proportionally to the candidate-pool share.
    if remaining > 0:
        min_taken = sum(alloc.values())
        surplus_pool = {a: len(by_author[a]) - alloc[a] for a in authors}
        total_surplus = sum(surplus_pool.values())
        # Proportional floor.
        for a in authors:
            if surplus_pool[a] <= 0 or total_surplus <= 0:
                continue
            extra = math.floor(remaining * surplus_pool[a] / total_surplus)
            extra = min(extra, surplus_pool[a])
            alloc[a] += extra
        # Step 3: leftover (floor rounding) → distribute one-at-a-time to the
        # authors with the most remaining surplus, deterministic.
        left = TARGET_N - sum(alloc.values())
        while left > 0:
            candidates_sorted = sorted(authors, key=lambda a: -(len(by_author[a]) - alloc[a]))
            progressed = False
            for a in candidates_sorted:
                if left <= 0:
                    break
                if alloc[a] < len(by_author[a]):
                    alloc[a] += 1
                    left -= 1
                    progressed = True
            if not progressed:
                break  # pool exhausted before reaching TARGET_N
    # Step 4: within each author, shuffle + take first n.
    rnd = random.Random(SEED)
    out: list = []
    for a, n in alloc.items():
        pool = list(by_author[a])
        rnd.shuffle(pool)
        for r in pool[:n]:
            rec = dict(r)
            rec["bootstrap_label"] = rec["label"]
            out.append(rec)
    # Deterministic global order for presentation (re-shuffle the merged set).
    rnd.shuffle(out)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w") as f:
        for r in out:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")
    print(f"wrote {len(out)} records to {OUT}")
    print(f"by author (share of 300): {dict(Counter(author_prefix(r['work_id']) for r in out))}")
    print(f"by bootstrap label: {dict(Counter(r['label'] for r in out))}")
    print(f"Tanizaki share: {sum(1 for r in out if r['work_id'].startswith('Tanizaki_J'))/len(out):.2f}")
    assert len(out) == TARGET_N, f"expected {TARGET_N}, got {len(out)} — pool may be too small"

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Run the sampler + verify distribution**

```bash
python scripts/ortho-gold/sample_300.py
```
Expected output:
- `wrote 300 records`
- `by author (share of 300):` shows Tanizaki_J ≈ 210 (70%, matching pool share — NOT capped), every other author-prefix with ≥ 4 records where the pool allowed.
- `by bootstrap label:` ≈ 200 accept / 100 reject (reflects the natural 2:1 pool ratio — NOT forced 50/50).
- `Tanizaki share: 0.69-0.70`

If `assert len(out) == 300` fails, the candidate pool (1068) is the upper bound — it isn't. The most likely failure is the leftover-distribution loop terminating early; debug with `print(alloc, left)` and confirm `sum(alloc)` reaches TARGET_N before the `progressed=False` break.

- [ ] **Step 3: Spot-check coverage + verify Tanizaki accept class is preserved**

```bash
python -c "
import json
from collections import Counter
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
print('total:', len(recs))
print('distinct works:', len(set(r['work_id'] for r in recs)))
print('distinct authors:', len(set('_'.join(r['work_id'].split('_')[:2]) for r in recs)))
tan=[r for r in recs if r['work_id'].startswith('Tanizaki_J')]
print('Tanizaki share:', len(tan)/len(recs))
print('Tanizaki accept share:', sum(1 for r in tan if r['label']=='accept')/len(tan))
print('overall accept share:', sum(1 for r in recs if r['label']=='accept')/len(recs))
"
```
Expected: 300 records, ≥ 9 distinct author-prefixes, ≥ 15 distinct works, Tanizaki share ≈ 0.69-0.70 (NOT capped), Tanizaki-accept-share ≈ 0.87 (the dominant accept pattern is preserved), overall accept share ≈ 0.65-0.70.

- [ ] **Step 4: Commit**

```bash
git add scripts/ortho-gold/sample_300.py data/ortho-gold/sample-300-unlabeled.jsonl
git commit -m "feat(ortho-gold): corpus-representative 300-sentence sampler

Samples each author-prefix IN PROPORTION to its candidate-pool share
(NOT author-capped — the data check showed Tanizaki carries 90% of the
accept class and capping would starve it). Per-author minimum of 4 for
sparse-author coverage. Accept/reject ratio reflects the natural pool
(2:1, NOT forced 50/50). Shuffle + take-first-n within author (NOT
strided/round-robin — the latter is buggy for small allocations).
Outputs sample-300-unlabeled.jsonl with bootstrap_label preserved."
```

---

### Task 2: LLM-assisted labeling of the 300 sentences

**Files:**
- Create: `data/ortho-gold/sentences-llm-300.jsonl` (the labeled evaluation set — labeling is orchestrator work in Step 1, not a script)

**Why this task exists:** the spec's Pre-Phase-1 Verification step 2 requires recall measured against HUMAN labels, and Phase 2's 50-sentence probe showed the bootstrap labels are a port-fidelity surrogate (not real recall). The user confirmed this task can be done by an LLM applying the spec's intent. The labeler is NOT the implementer — the implementer builds the harness; the orchestrator (you, the dispatching agent) does the labeling by reading each sentence.

**Labeling rubric (the spec's intent, copied verbatim into the script's docstring so the labeler reads it):**

> **ACCEPT** = the sentence is pre-war kanji-katakana-majiri prose: katakana serves the grammatical role of hiragana (particles は/が/の/を, okurigana, copulas だ/である, auxiliary verbs タ/テ/ナイ), the sentence is running narrative or dialogue with grammatical structure, and normalizing to hiragana would improve morphological-analysis quality. The presence of kanji does NOT disqualify (kanji+katakana-majiri is the canonical form); what matters is that the katakana is doing grammatical work, not lexical/onomatopoeic work.
>
> **REJECT** = (a) exclamations/interjections (「アハハ」, 「ヨイショ」, 「ハテナ」); (b) onomatopoeia (ウハハハハ, フフフ, アッハッハ); (c) single-word utterances or fragments (「スウプ。」, 「マイ、チャイルド。」); (d) Buddhist chants or foreign-loan vocative fragments with no grammatical structure (「ナムアムダブツ」); (e) sentences where the katakana is lexical (proper nouns, foreign words) rather than grammatical.

**Interfaces:**
- Consumes: `data/ortho-gold/sample-300-unlabeled.jsonl` (Task 1).
- Produces: `data/ortho-gold/sentences-llm-300.jsonl` — same fields + `label` overwritten with the LLM label, plus `labeler: "llm"`, `bootstrap_label` preserved, plus `label_notes: Option<String>` for edge-case reasoning.

- [ ] **Step 1: Label all 300 sentences (orchestrator work, NOT a subagent)**

This is the load-bearing step. The orchestrator (you) reads each sentence and applies the rubric. **Realistic time estimate: 50–75 minutes** (~12–15 s per sentence × 300 — reading, deciding, recording). Schedule this as a focused block, not interleaved with other work; label fatigue produces noisier labels.

The labeling is done as a batch — no interactive `label_review.py` harness (an earlier draft of this plan included one, but it was dead code since the batch workflow bypassed it). Write the labels directly to `data/ortho-gold/sentences-llm-300.jsonl` as a JSONL stream.

**Step 1a: Dump the 300 unlabeled sentences to a review file the orchestrator reads.**

```bash
python -c "
import json
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
for i,r in enumerate(recs):
    print(f'{i:3} [{r["bootstrap_label"]:6} r={r["katakana_ratio"]:.2f} h={r["hiragana_count"]} c={r["total_chars"]:3}] {r["work_id"]:24} {r["sentence"]!r}')
" > /tmp/review-300.txt
wc -l /tmp/review-300.txt  # expect 300
```

**Step 1b: Orchestrator reads `/tmp/review-300.txt`, decides each label, emits the labeled JSONL.**

The verdict list IS the orchestrator's actual labeling work — do NOT delegate this to a subagent; the labeling is the LLM-judgment step that makes the evaluation set real (stronger than the Python cascade, weaker than a human expert — see caveat in Task 3).

```bash
# Orchestrator fills VERDICTS below (300 chars, 'a' or 'r' each):
python - <<'PY'
import json
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
VERDICTS = ""  # ← orchestrator fills this 300-char string of 'a'/'r'
assert len(VERDICTS) == len(recs), f"{len(VERDICTS)} verdicts vs {len(recs)} records"
with open('data/ortho-gold/sentences-llm-300.jsonl','w') as f:
    for r,v in zip(recs, VERDICTS):
        r['bootstrap_label'] = r['label']
        r['label'] = 'accept' if v=='a' else 'reject'
        r['labeler'] = 'llm'
        r['label_notes'] = None
        f.write(json.dumps(r, ensure_ascii=False) + '\n')
from collections import Counter
print('label dist:', Counter(r['label'] for r in recs))
PY
```

**Decision rule for the orchestrator (read each sentence and apply):** if the katakana is doing grammatical work (particles/copulas/aux in running prose or grammatical dialogue) → `a`. If it's interjection/onomatopoeia/single-word/chant/loan-vocative → `r`. When genuinely unsure, prefer `a` if the sentence has ≥ 15 chars and `hiragana_count == 0` and reads as continuous narrative; prefer `r` if it's < 12 chars or starts with `「` and contains no verb. Document any genuinely-uncertain calls (≤ 5 expected) by setting `label_notes` to a one-line reason instead of `None`.

**Do NOT use the circular "unsure → bootstrap label" fallback that an earlier draft proposed.** The bootstrap labels are exactly what this set is supposed to replace; falling back to them when the labeler is unsure defeats the purpose. If a sentence is genuinely unjudgable, mark it `label_notes="unsure"` and EXCLUDE it from the recall denominator in Task 3 (the harness filters on `label_notes is None`).

- [ ] **Step 2: Spot-check 10% (30 sentences) with a second pass**

**Why:** the labels are single-annotator LLM output. Inter-annotator agreement with a second LLM pass using a differently-framed rubric is the cheapest available proxy for label reliability. This is NOT a human expert check — it's a sanity catch for systematic rubric misapplication.

Sample 30 records from `/tmp/review-300.txt` (strided: every 10th, `0,10,20,...,290`). Re-label them with a **reframed** rubric prompt — instead of "is the katakana doing grammatical work?" ask "would normalizing this sentence to hiragana change the morphological analysis result?" (same intent, different surface framing; surfaces cases where the first pass pattern-matched on the wrong signal).

Compute agreement: `agree = (count where spot-check label == first-pass label) / 30`. Record it in the Task 3 report. If agreement < 0.80, the first-pass labels are too noisy to trust — STOP, revisit the rubric, re-label. If agreement ≥ 0.80, proceed (the 30 spot-checked records keep their first-pass labels; the spot-check is a measurement, not a correction).

- [ ] **Step 3: Verify the labeled set**

```bash
python -c "
import json
from collections import Counter
recs=[json.loads(l) for l in open('data/ortho-gold/sentences-llm-300.jsonl')]
print('n:', len(recs))
print('label dist:', Counter(r['label'] for r in recs))
print('labeler:', Counter(r['labeler'] for r in recs))
# disagreements with bootstrap
dis=sum(1 for r in recs if r['label']!=r['bootstrap_label'])
print(f'disagreements with bootstrap: {dis} ({dis/len(recs):.1%})')
# coverage
print('distinct works:', len(set(r['work_id'] for r in recs)))
print('Tanizaki share:', sum(1 for r in recs if r['work_id'].startswith('Tanizaki_J'))/len(recs))
"
```
Expected: n=300 (or slightly fewer if genuinely-unjudgable records were marked `label_notes="unsure"` and excluded), both labels present (ideally 150–215 accept given the natural 2:1 corpus ratio; a 0/300 split means the rubric was applied uniformly wrong), labeler all `llm`, ≥ 8 disagreements with bootstrap (the whole point of re-labeling), ≥ 9 distinct author-prefixes, Tanizaki share ≈ 0.69–0.70 (matches pool, NOT capped).

- [ ] **Step 4: Commit the labeled evaluation set**

```bash
git add data/ortho-gold/sentences-llm-300.jsonl
git commit -m "data(ortho-gold): 300-sentence LLM-labeled evaluation set

Corpus-representative (sampled in proportion to candidate-pool author shares,
NOT author-capped — data check showed Tanizaki carries 90% of the accept
class and capping would starve it). Labeled by the orchestrator applying the
spec's intent rubric: ACCEPT = kanji-katakana-majiri prose where katakana
does grammatical work; REJECT = exclamations, onomatopoeia, single-word
fragments, chants, loanvocatives. 10% spot-checked with a reframed-rubric
second pass (agreement recorded in Task 3 report).

NOT GROUND TRUTH: single-annotator LLM labels, stronger than the Python
cascade but weaker than a human expert. Used to measure recall and tune
thresholds; NOT used to gate irreversible code deletion (Task 8 deferred
to a future phase with real human annotation per the Phase 2 ADR).
labeler='llm' on every record; bootstrap_label preserved per-record for
disagreement analysis."
```

---

---

### Task 3: Measure HeuristicV1 recall + ML recall against the 300 gold labels

**Files:**
- Create: `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md` (the measurement report; the branch-decision section is filled in Task 4-8 as the chosen branch executes).

**Interfaces:**
- Consumes: `data/ortho-gold/sentences-llm-300.jsonl` (Task 2), `crates/ab-ortho-detect/examples/detect_sentences.rs` (exists, Phase 2), `scripts/ortho-gold/recall_floor.clj` (exists, Phase 2), `ab-ortho-detect-ml` train/ablate (exist, Phase 2).
- Produces: `data/ortho-gold/models/model-gold-300.bin` (ML retrained on the 300 labels), `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md`.

- [ ] **Step 1: Measure HeuristicV1 recall on the 300 gold labels**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences-llm-300.jsonl > /tmp/gold300-heur.jsonl 2>/dev/null
bb scripts/ortho-gold/recall_floor.clj /tmp/gold300-heur.jsonl
```
Record: `recall`, `precision`, `f1`, `tp/fn/fp/tn`. Save full output for the report.

- [ ] **Step 2: Retrain the ML detector on the 300 gold labels**

```bash
cargo run -p ab-ortho-detect-ml -- train --gold data/ortho-gold/sentences-llm-300.jsonl --out data/ortho-gold/models/model-gold-300.bin
cargo run -p ab-ortho-detect-ml -- hash --model data/ortho-gold/models/model-gold-300.bin
```
Record: train accuracy (from stderr), model hash. Expected: train accuracy 0.85–0.95 (the 300 human labels are more separable than the bootstrap because the rubric is sharper; if < 0.80 the linear features can't capture the rubric and ML is NOT viable — note this for the branch decision).

- [ ] **Step 3: Measure ML detector recall on the 300 gold labels**

The `detect_sentences` example uses `HeuristicV1`. To measure the ML detector we either (a) add a `--detector ml --model PATH` flag to the example, or (b) write a one-shot probe. (b) is faster and avoids touching the example binary's API:

Create a throwaway `crates/ab-ortho-detect/examples/detect_ml.rs`:
```rust
//! Like detect_sentences but uses MlLogisticRegression instead of HeuristicV1.
use std::io::{BufRead, BufReader};
use std::sync::Arc;
use ab_ortho_detect::ml::MlLogisticRegression;
use ab_ortho_detect::OrthoDetector;
use ab_plaintext::sentence_split;
use serde::{Deserialize, Serialize};

#[derive(Deserialize)] struct GoldRecord { sentence: String, label: String }
#[derive(Serialize)] struct Out<'a> { sentence: &'a str, gold: &'a str, heuristic: String, agree: bool }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let model_path = std::env::args().nth(1).expect("usage: detect_ml <model.bin>");
    let detector = MlLogisticRegression::load(std::path::Path::new(&model_path))?;
    let det: Arc<dyn OrthoDetector> = Arc::new(detector);
    for line in BufReader::new(std::io::stdin()).lines() {
        let line = line?;
        if line.trim().is_empty() { continue; }
        let rec: GoldRecord = serde_json::from_str(&line)?;
        let spans = sentence_split(&rec.sentence);
        let anns = det.detect(&spans);
        let verdict = if anns.is_empty() { "reject" } else { "accept" };
        println!("{}", serde_json::to_string(&Out { sentence: &rec.sentence, gold: &rec.label, heuristic: verdict.to_string(), agree: verdict == rec.label })?);
    }
    Ok(())
}
```

**Step 3a: Verify dev-deps are present (concrete check, do NOT skip).**
```bash
grep -E 'serde|ab-plaintext|ab-morph-analyzers' crates/ab-ortho-detect/Cargo.toml
```
Expected output: lines showing `serde`, `serde_json`, `ab-plaintext`, and `ab-morph-analyzers` in `[dev-dependencies]` (Phase 1 added them for the integration tests). If `serde` or `serde_json` is missing, append to `[dev-dependencies]` in `crates/ab-ortho-detect/Cargo.toml`:
```toml
serde = { workspace = true }
serde_json = { workspace = true }
```
Then `cargo build -p ab-ortho-detect --examples` must succeed before proceeding — if it fails with `unresolved import serde`, the dev-deps weren't added.

**Step 3b: Run the ML recall measurement.**
```bash
cargo run -p ab-ortho-detect --example detect_ml -- data/ortho-gold/models/model-gold-300.bin < data/ortho-gold/sentences-llm-300.jsonl > /tmp/gold300-ml.jsonl 2>/dev/null
bb scripts/ortho-gold/recall_floor.clj /tmp/gold300-ml.jsonl
```
⚠ **Bias caveat for the report:** recall and precision here are **train-set accuracy** (the ML detector is evaluated on the same 300 records it was trained on). This is an UPPER BOUND, not generalization. For honest generalization, do a proper hold-out split (Task 6, optional). For the branch decision, treat the ML train-accuracy as "ceiling"; the floor is the ML's recall on a held-out set, which we don't have. The honest comparison is: HeuristicV1 recall (no train/eval split, real generalization) vs ML train-accuracy (ceiling). If ML ceiling < HeuristicV1 recall → heuristic wins. If ML ceiling >> HeuristicV1 recall AND >> 0.85 → ML is promising but needs hold-out validation before promotion.

- [ ] **Step 4: Write the measurement report (branch-decision section left as a placeholder)**

Create `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md`:
```markdown
# Ortho-Detect Phase 2.5 — 300-Sentence LLM-Labeled Evaluation Set Measurement

Date: 2026-07-05
Gold set: `data/ortho-gold/sentences-llm-300.jsonl` (n=300, LLM-labeled)
Sampler: `scripts/ortho-gold/sample_300.py` (corpus-representative, Tanizaki ≤40%)
Labeler: orchestrator LLM applying the spec's intent rubric (NOT a human
Japanese-lit expert — treat as second-tier bootstrap).

## Bias caveats (read first)

1. **LLM labeler, not human:** the labels are an LLM's application of the
   rubric. Where the rubric is ambiguous (short vocative fragments, borderline
   dialogue), the labeler may diverge from a human expert. Stronger than the
   Python cascade (it reads the sentence) but not ground truth.
2. **ML recall = train-accuracy (upper bound):** the ML detector was trained
   on these 300 records and is evaluated on them. The number is a CEILING,
   not generalization. HeuristicV1 recall is real generalization (no
   train/eval split).
3. **N=300, not 500:** the spec's target range is 200-500. 300 is within
   range but on the lower end. A final human-expert pass on the
   disagreements (≤30 expected) before any production promotion is the
   documented finishing input.

## HeuristicV1 against LLM-labeled evaluation set (real generalization)

[INSERT: recall, precision, f1, tp/fn/fp/tn from Step 1 + the bb output]
[INSERT: count of disagreements vs bootstrap; characterize (prose-at-ratio-
0.4-0.5 / proper-noun-guard / fragment-over-acceptance)]

## MlLogisticRegression against LLM-labeled evaluation set (train-accuracy ceiling)

- Model: `data/ortho-gold/models/model-gold-300.bin`
- model_hash: [INSERT from Step 2]
- Train accuracy: [INSERT]
- Recall (train-set): [INSERT]
- Precision (train-set): [INSERT]
- F1: [INSERT]

## Branch decision (filled in Task 4-8)

[Placeholder — to be filled based on the two numbers above. See Task 4 decision rule.]
```

- [ ] **Step 5: Commit**

```bash
git add crates/ab-ortho-detect/examples/detect_ml.rs data/ortho-gold/models/model-gold-300.bin \
        reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md
git commit -m "feat(ortho-gold): measure HeuristicV1 + ML recall on 300 human labels

Adds detect_ml example binary for ML-detector recall measurement. Train-set
accuracy for ML (upper bound, not generalization); real generalization for
HeuristicV1. Numbers feed the Phase 2.5 branch decision (heuristic tune
vs ML promotion + dead-coupling deletion)."
```

---

### Task 4: Branch decision + ADR entry (NO code change unless Tasks 5-8 fire)

**Why this task exists:** the Phase 2 ADR gated `OrthoTokenizer` deletion + ML-promotion-from-EXPERIMENTAL on three conditions: (a) ≥200-sent **HUMAN** gold, (b) ML recall ≥ 0.85 against that human gold, (c) ML promoted to default. **Task 2's LLM-labeled set does NOT satisfy condition (a)** — it is an LLM applying a rubric, not a human expert. So this task measures recall and tunes thresholds (legitimate on LLM labels, which are stronger than the Python cascade), and decides whether to land Task 5 (heuristic tune — always fires if recall < 0.85) and optionally Task 6 (ML hold-out) + Task 7 (ML promotion from EXPERIMENTAL to stable-but-not-default). **Task 8 (deletion) does NOT fire in Phase 2.5** — it requires the human gold set that doesn't exist yet.

**Decision rule (apply using Task 3's numbers):**

First, two early-exit cases the original draft missed:

- **EARLY EXIT A — HeuristicV1 already ≥ 0.85 on the new gold:** the spec's recall floor is already cleared on this evaluation set. **Skip Task 5 entirely** (no threshold tune needed). Record the number and proceed to Task 6+7 only if ML is also promising (for the EXPERIMENTAL→stable promotion); else stop after the report. Do NOT invent tuning work the data doesn't demand.
- **EARLY EXIT B — Neither detector can clear 0.85 after Task 5's sweep:** the character-only feature set is insufficient for the rubric. This is a real finding, not a failure mode to paper over. **Document it and STOP** — do NOT promote ML (Tasks 7-8), do NOT delete anything. File a follow-up: feature engineering (n-gram katakana sequences, grammatical-role features) or a non-linear model. The Phase 2 ADR's deletion gate explicitly requires recall ≥ 0.85; unmet means no deletion, full stop.

The main branches:

- **If ML train-accuracy ceiling < HeuristicV1 recall:** the ML detector is WORSE than the heuristic even on its training data. ML is NOT viable with the current feature set (character-only can't capture the rubric). → **Execute Task 5 only (tune the heuristic threshold).** Do NOT promote ML, do NOT delete the coupling.
- **If ML ceiling ≥ 0.85 AND ≥ HeuristicV1 recall + 0.10:** the ML detector is promising. But train-accuracy is a ceiling, not generalization. → **Execute Task 5 (tune the heuristic as the immediate fix) AND Task 6 (hold-out split to validate ML generalization).** Defer Tasks 7-8 (ML promotion + deletion) until the hold-out recall also clears 0.85.
- **If ML ceiling ≥ 0.95 AND HeuristicV1 recall < 0.85 (current Phase 2 state suggests this is likely):** → Execute Task 5 (heuristic threshold tune as the immediate, low-risk fix to clear the floor), AND Task 6 (ML hold-out), AND Task 7 (promotion from EXPERIMENTAL to stable, NOT default) ONLY if Task 6 hold-out recall ≥ 0.85. **Task 8 (code deletion) does NOT fire on LLM labels** — it's deferred to a future phase with real human annotation per the plan's revised goal.

**The orchestrator records the decision in the report before dispatching.**

- [ ] **Step 1: Fill in the report's Branch Decision section**

Open `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md`, replace the `## Branch decision` placeholder with the actual decision based on the rule above. State: the two numbers, the chosen branch (Task 5 only / Tasks 5+6 / Tasks 5+6+7+8), and the rationale. Note the bias caveat applies.

- [ ] **Step 2: Commit the decision**

```bash
git add reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md
git commit -m "docs(ortho-detect): Phase 2.5 branch decision — [SUMMARY]

HeuristicV1 recall: [X.XX]. ML train-accuracy ceiling: [X.XX]. Per the
decision rule: [execute heuristic-tune / +ML-holdout / +ML-promotion+deletion].
[Rationale sentence]."
```

- [ ] **Step 3: Dispatch the chosen branch (Tasks 5, optionally 6, optionally 7-8)**

The remaining tasks (5-8) are independent: Task 5 (heuristic tune) ALWAYS executes if recall < 0.85. Tasks 6-8 are conditional on the ML ceiling. Dispatch accordingly.

---

### Task 5: Tune `HeuristicConfig::katakana_ratio_threshold` (ALWAYS executes if recall < 0.85)

**Files:**
- Modify: `crates/ab-ortho-detect/src/heuristic.rs:31` (the `default()` impl).
- Modify: `crates/ab-ortho-detect/src/heuristic.rs:108` (if needed — the gate is already `<=`, so lowering the threshold from 0.5 to 0.4 fixes the root cause).

**Why:** Phase 2's human probe identified the binding constraint as `katakana_ratio > 0.5` rejecting kanji-heavy prose at ratio 0.41–0.49. The spec's candidate gate already uses `> 0.4` (`scripts/ortho-gold/bootstrap_label.py`). Aligning the detection threshold to 0.4 directly recovers these sentences.

**Interfaces:**
- Consumes: Task 3's measurement (HeuristicV1 recall on the 300 gold).
- Produces: a tuned `HeuristicConfig::default()` with `katakana_ratio_threshold: 0.4` (or the value Task 3's analysis shows maximizes F1 without tanking precision).

- [ ] **Step 1: Write a sweep test that finds the threshold maximizing F1**

Add to `crates/ab-ortho-detect/src/heuristic.rs` test module (or `examples/`):
```rust
/// Sweep katakana_ratio_threshold over [0.25, 0.55] in 0.05 steps; for each,
/// compute recall + precision + F1 of HeuristicV1 against a JSONL gold set
/// read from env `ORTHO_GOLD_PATH`. Prints the table. Used by Task 5 to pick
/// the threshold that maximizes F1 subject to recall >= 0.85.
```
Run it as an example `crates/ab-ortho-detect/examples/sweep_threshold.rs` using the existing `detect_sentences`-style harness inside a 0.25→0.55 loop. (Reuse `HeuristicConfig { katakana_ratio_threshold: t, ..Default::default() }`.)

```bash
cargo run -p ab-ortho-detect --example sweep_threshold
# (reads ORTHO_GOLD_PATH=data/ortho-gold/sentences-llm-300.jsonl + AB_VIBRATO_DICT)
```

- [ ] **Step 2: Pick the threshold maximizing F1 subject to recall ≥ 0.85**

From the sweep table, select: highest F1 where recall ≥ 0.85. If no threshold clears 0.85 recall, pick the one with highest recall + document that the floor is unreachable by threshold-tuning alone (and ML promotion becomes mandatory — escalate to Tasks 6-8). Expected: 0.35–0.45 clears the floor.

- [ ] **Step 3: Update `HeuristicConfig::default()`**

`crates/ab-ortho-detect/src/heuristic.rs:31`: change `katakana_ratio_threshold: 0.5` → `katakana_ratio_threshold: <CHOSEN>` (likely 0.4). Update the test at line 229 that asserts `0.5` to the new value.

- [ ] **Step 4: Update the existing `detects_*` heuristic tests if they now fail**

Some Phase 1 tests asserted the old threshold behavior. Re-run:
```bash
cargo test -p ab-ortho-detect heuristic 2>&1 | rg "test result|FAILED"
```
Any test that now fails because the threshold changed: update its expected behavior to match the new threshold (the test's *intent* — "this sentence is accepted/rejected" — should still hold for the new threshold; if a test intentionally exercises the boundary at exactly 0.5, move its input to clearly above/below the new threshold). Do NOT change assertions to silently pass; fix the input.

- [ ] **Step 5: Verify recall now ≥ 0.85**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences-llm-300.jsonl 2>/dev/null | bb scripts/ortho-gold/recall_floor.clj /dev/stdin
```
Expected: recall ≥ 0.85. If below, return to Step 2 or escalate to Tasks 6-8.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-ortho-detect/src/heuristic.rs crates/ab-ortho-detect/examples/sweep_threshold.rs
git commit -m "fix(ortho-detect): tune katakana_ratio_threshold 0.5 -> [X.XX]

Phase 2's human probe (recall 0.636) identified the binding constraint as
the strict >0.5 gate rejecting kanji-heavy pre-war prose at ratio 0.41-0.49.
Sweep over [0.25,0.55] picked [X.XX] as the threshold maximizing F1 subject
to recall >= 0.85. Recall on the 300-sentence LLM-labeled evaluation set is now [X.XX]
(>= 0.85 floor)."
```

---

### Task 6: ML hold-out validation (CONDITIONAL — only if ML train-accuracy ≥ 0.85 AND ≥ HeuristicV1 recall + 0.10)

**Skip if:** the Task 4 decision rule did not dispatch this branch.

**Files:**
- Modify: `crates/ab-ortho-detect-ml/src/main.rs` — add a `cross-validate` subcommand (k-fold, default k=5).
- Create: `reports/ortho-detect/2026-07-05-phase2.5-ml-cv.md` — the cross-validation report.

**Why:** train-accuracy is a ceiling. Before promoting ML to default, we need hold-out recall to confirm generalization. 5-fold CV on 300 records = 60 test per fold, 240 train. The honest number is mean recall across folds.

- [ ] **Step 1: Add `cross-validate` subcommand**

In `crates/ab-ortho-detect-ml/src/main.rs`, add `CrossValidate { gold, k: usize, report }` to the `Cmd` enum. Implementation: load gold, shuffle with fixed seed, split into k folds, for each fold train on the other k-1 and evaluate recall+precision+F1 on the held-out fold, write the per-fold + mean table to `report`.

- [ ] **Step 2: Run 5-fold CV**

```bash
cargo run -p ab-ortho-detect-ml -- cross-validate --gold data/ortho-gold/sentences-llm-300.jsonl --k 5 --report reports/ortho-detect/2026-07-05-phase2.5-ml-cv.md
```
Record: mean recall, mean precision, mean F1, per-fold breakdown.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-ortho-detect-ml/src/main.rs reports/ortho-detect/2026-07-05-phase2.5-ml-cv.md
git commit -m "feat(ortho-detect-ml): k-fold cross-validation subcommand

5-fold CV mean recall: [X.XX]. Honest generalization estimate (vs the
train-accuracy ceiling). Feeds the Phase 2.5 ADR's ML-promotion gate."
```

---

### Task 7: Promote `--ortho-detect ml` from EXPERIMENTAL (CONDITIONAL — only if Task 6 mean recall ≥ 0.85)

**Skip if:** Task 6 mean recall < 0.85 OR Task 6 was skipped.

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs` — remove the EXPERIMENTAL marker from the `--ortho-detect ml` clap help text; update to reflect the validated model.

**Why:** the ADR's condition (c) is "ML promoted from EXPERIMENTAL to default." This fires only if hold-out recall cleared the floor.

- [ ] **Step 1: Update CLI help text**

Remove the `EXPERIMENTAL:` prefix and bias caveat from the `ml` help arm in `crates/ab-morph-run/src/main.rs` (Task 8 of Phase 2 added it). Replace with: `ml — logistic-regression detector on character features (5-fold CV recall X.XX). Requires --ortho-ml-model.`

- [ ] **Step 2: Commit**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat(morph-run): promote --ortho-detect ml from EXPERIMENTAL

5-fold CV recall on the 300-sentence LLM-labeled evaluation set cleared the 0.85
floor ([X.XX]). EXPERIMENTAL marker + bias caveat removed from CLI help.
The ML detector is now a first-class mode (not yet the DEFAULT — that
requires the heuristic path's removal in Task 8)."
```

---

### Task 8: Delete dead Vibrato coupling — DEFERRED to a future phase (does NOT execute in Phase 2.5)

**This task does NOT execute in Phase 2.5.** It is documented here only as the gate the future phase must meet. The Phase 2.5 evaluation set is LLM-labeled (single-annotator, no human expert check, no inter-annotator agreement above spot-check); it cannot honestly gate irreversible code deletion per the Phase 2 ADR's trust-boundary framing. The deletion remains deferred until ALL of:

1. A real human-annotated gold set (≥200 sentences, by a Japanese-literate reviewer) exists.
2. The ML detector's hold-out recall on that human set is ≥ 0.85.
3. The ML detector is promoted from EXPERIMENTAL to default.

**Phase 2.5 may produce (1) the methodology and (2) the measurement infrastructure, but the labels themselves must be re-done by a human before this task fires.** No code is deleted in Phase 2.5.

**If the orchestrator is tempted to fire this task anyway** (e.g. because hold-out recall on the LLM set is very high): do not. The labels are an LLM applying the rubric; an LLM-labeled ceiling says nothing about how the detector performs on the real distribution a human would label. Land the deletion in a separate phase with its own ADR entry recording the human gold set's provenance.

**Skip if:** Task 7 was skipped, OR the project decides to keep `--ortho-detect heuristic` as a fallback. This is the ADR's deferred deletion — only land it if ML is validated AND the heuristic path is being retired.

**Files:**
- Modify: `crates/ab-ortho-detect/src/lib.rs` — remove `OrthoTokenizer` trait + `OrthoToken`.
- Delete: `crates/ab-morph-analyzers/src/ortho_compat.rs`.
- Modify: `crates/ab-ortho-detect/src/heuristic.rs` — remove the proper-noun guard (it's the only `OrthoTokenizer` consumer) + dead `oov_*` config fields; OR delete `HeuristicV1` entirely if the heuristic path is retired.
- Modify: `crates/ab-morph-run/src/options.rs` + `pipeline.rs` — remove the `Heuristic` arm if `HeuristicV1` is deleted; else keep.
- Modify: `crates/ab-ortho-detect/Cargo.toml` — remove `ab-morph-analyzers` dev-dep (only used for the `VibratoAnalyzer: OrthoTokenizer` impl + integration test).
- Modify: `docs/superpowers/specs/2026-07-05-ortho-detect-design.md` — update the ADR section to record the deletion landed.

**Why:** the Phase 2 ablation proved character-only wins; the ADR deferred deletion until ML clears recall. If Task 7 fired, the condition is met.

- [ ] **Step 1: Decide scope** — full `HeuristicV1` removal (if ML becomes default and the heuristic path is retired) vs partial (keep `HeuristicV1` but strip the proper-noun guard + OOV config + `OrthoTokenizer` trait). The orchestrator decides based on whether `--ortho-detect heuristic` should remain as a fallback. Default recommendation: PARTIAL (keep the heuristic as a no-Vibrato fallback; strip the dead coupling).

- [ ] **Step 2: Delete per chosen scope.** For each file, remove the dead code; run `cargo check --workspace` after each to catch dangling references.

- [ ] **Step 3: Update the spec ADR**

`docs/superpowers/specs/2026-07-05-ortho-detect-design.md` — the "Phase 2 outcome (deletion DEFERRED)" paragraph: update to "DELETION LANDED (Phase 2.5)" with the CV recall number that justified it.

- [ ] **Step 4: Workspace tests pass**

```bash
cargo test --workspace 2>&1 | rg "test result" | tail -20
```
Expected: all pass except the 2 pre-existing `rerun_full_*`.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(ortho-detect): delete dead Vibrato coupling (ADR closure)

ML detector validated on 5-fold CV (recall [X.XX]); HeuristicV1's
proper-noun guard (the only OrthoTokenizer consumer) is [removed / kept
as no-Vibrato fallback]. The trait seam, ortho_compat.rs, double-dict-load,
and dead oov_* config that the Phase 2 ablation proved unnecessary are
now gone. ADR updated: deletion LANDED."
```

---

### Task 9: Final report + merge

**Files:**
- Finalize: `reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md` (append the post-fix recall numbers + the ADR closure status).

- [ ] **Step 1: Append the post-tuning recall numbers to the report**

Add a `## Post-tuning recall` section with: HeuristicV1 recall after Task 5 threshold tune; ML mean recall from Task 6 CV (if run); the chosen default mode; whether the ADR's deletion gate fired (Tasks 7-8) or remains deferred.

- [ ] **Step 2: Final workspace verification**

```bash
cargo test --workspace 2>&1 | rg "test result" | awk '{p+=$4; f+=$6} END {print "pass:", p, "fail:", f}'
```
Expected: pass ≥ 450, fail == 2 (pre-existing `rerun_full_*` only).

- [ ] **Step 3: Commit + merge to main**

```bash
git add reports/ortho-detect/2026-07-05-phase2.5-llm-eval-300.md
git commit -m "docs(ortho-detect): finalize Phase 2.5 report + recall numbers"
cd /home/bor/Projects/ab-validator
git merge --no-ff ortho-detect-phase2.5 -m "Merge branch 'ortho-detect-phase2.5': LLM-labeled evaluation set + recall fix [SUMMARY]"
git push origin main
git worktree remove .worktrees/ortho-detect-phase2.5
git branch -d ortho-detect-phase2.5
```

---

## Self-Review (revised after critical review of v1)

**1. Spec coverage:**
- Pre-Phase-1 Verification step 2 ("if recall < 0.85, tune thresholds") → Task 5. **EARLY EXIT A** added: if HeuristicV1 already ≥ 0.85 on the new set, Task 5 is skipped (don't invent tuning work the data doesn't demand).
- Phase 2 ADR deletion gate (≥200 HUMAN-annotated gold + ML recall ≥ 0.85 on it + ML promoted to default) → **Task 8 is DEFERRED**; the LLM set does NOT satisfy condition (a). Phase 2.5 lands the methodology + measurement + heuristic tune + optional ML promotion to stable (NOT default); deletion waits for a human gold set.
- Spec's 200–500 sentence target → Task 2 (300, LLM-labeled, explicitly NOT ground truth).
- Corpus-representative sampling (user's requirement) → Task 1 samples in proportion to candidate-pool author shares (NOT author-capped — the data check showed Tanizaki carries 90% of the accept class and capping would starve it). Per-author minimum of 4 for sparse-author coverage.

**2. Placeholder scan:** The only literal placeholder is the orchestrator's `VERDICTS = ""` in Task 2 Step 1 — that is the actual labeling work and CANNOT be pre-filled (it's the LLM judgment that makes the set real). Everything else has concrete code/commands.

**3. Type consistency:** `OrthoDetectMode { Off, Heuristic, Ml }`, `OrthoDetectorId::MlLogisticRegression { model_hash }`, `HeuristicConfig::katakana_ratio_threshold`, `MlLogisticRegression::load(path)`, `model_hash(&MlModel)` — all match Phase 2's merged `main`.

**4. Branch-decision honesty:** Task 4 does NOT assume ML wins. Five branches: EARLY EXIT A (heuristic already ≥0.85 → skip tuning), EARLY EXIT B (neither clears 0.85 after sweep → character-only features insufficient, stop, file feature-engineering follow-up), ML ceiling < HeuristicV1 recall (ML not viable, tune heuristic only), ML ceiling high but train-set only (tune + hold-out, defer promotion), ML ceiling ≥0.95 (tune + hold-out + promotion to stable-but-not-default; deletion still deferred). The conditions are real gates, not rubber stamps.

**5. Labeling honesty (revised):** Task 2's labels are single-annotator LLM output, NOT human ground truth. Task 2 Step 2 adds a 10% spot-check with a reframed rubric (agreement gate ≥0.80). The circular "unsure → bootstrap" fallback is removed (would defeat the purpose). The plan title and all references say "LLM-labeled evaluation set," not "human gold."

## Execution Handoff

**Plan complete and saved to `docs/superpowers/plans/2026-07-05-ortho-detect-phase2.5.md`. Two execution options:**

**1. Subagent-Driven (recommended for Tasks 1, 3, 5, 6-9)** — I dispatch a fresh implementer per task, review between tasks. Task 2 (labeling) is orchestrator work, not a subagent — I read the 300 sentences and apply the rubric directly.

**2. Inline Execution** — Tasks run in this session with checkpoints.

**Which approach?** Given Task 2 is the labeling bottleneck (300 sentences, ~5-10 min of orchestrator attention) and the rest is mechanical, I recommend: I execute Task 1 (sampler) now, then do Task 2 (labeling) inline, then dispatch Task 3+ to subagents.
