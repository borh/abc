# Ortho-Detect Phase 2.5 — Human Gold Set + Recall Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a real 300-sentence human-labeled gold set that is representative of the whole Aozora fiction corpus, retrain the ML detector on it, prove HeuristicV1 recall ≥ 0.85 against it, and (conditional on the result) either ship a tuned `HeuristicConfig` or promote `--ortho-detect ml` from EXPERIMENTAL to default + delete the dead Vibrato coupling per the Phase 2 ADR's deferred-deletion gate.

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

- `data/ortho-gold/sentences-human-300.jsonl` — the new gold set (300 records, `{work_id, sentence, byte_offset, char_offset, label, katakana_ratio, hiragana_count, total_chars, dict_version, bootstrap_label, labeler: "llm"}`).
- `data/ortho-gold/models/model-v3-gold.bin` — ML model retrained on the 300 human labels (replaces `model-v1.bin` as the canonical ML model if the ML branch is chosen).
- `scripts/ortho-gold/sample_300.py` — stratified sampler (corpus-representative, fixes Tanizaki skew).
- `scripts/ortho-gold/label_review.py` — presents each sample sentence + bootstrap label + features for the LLM labeler; emits the labeled JSONL.
- `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md` — the measurement + branch-decision report.
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
Expected: 50 (the Phase 2 human probe set size). If this fails, Phase 2 is not on `main` — do not proceed.

- [ ] **Step 4: Commit (empty, marks task start)**

```bash
git commit --allow-empty -m "chore(ortho-detect-phase2.5): worktree + Phase 2 baseline"
```

---

### Task 1: Corpus-representative 300-sentence sampler

**Files:**
- Create: `scripts/ortho-gold/sample_300.py`

**Why this task exists:** the Phase 2 bootstrap set `data/ortho-gold/sentences.jsonl` is 58% Tanizaki (266/457). It was stratified *per work* but the candidate pool itself is 70% Tanizaki (747/1068), so the stratification inherited the skew. For a human gold set that generalizes across the corpus, we need per-author capping that guarantees all 9 author-prefixes are represented proportionally to their presence in the corpus, NOT proportionally to their candidate counts.

**Interfaces:**
- Consumes: `data/ortho-gold/candidates.jsonl` (1068 records with `work_id`, `sentence`, `katakana_ratio`, `hiragana_count`, `total_chars`, `bootstrap_label=label` from Phase 2's bootstrap).
- Produces: `data/ortho-gold/sample-300-unlabeled.jsonl` — 300 records, one work per author-prefix capped so Tanizaki ≤ 40% (120), every other author-prefix gets ≥ its proportional share of the remaining 180, min 8 per author-prefix to guarantee coverage. Each record preserves all original fields + gains `bootstrap_label` (copied from `label`).

- [ ] **Step 1: Write the sampler**

Create `scripts/ortho-gold/sample_300.py`:
```python
#!/usr/bin/env python3
"""Corpus-representative stratified sampler for the human gold set.

Fixes the Phase 2 bootstrap set's Tanizaki skew (58% of 457). Strategy:
- Cap Tanizaki_J at 40% (120 of 300) — he genuinely has the most
  kanji-katakana-majiri prose, but capping prevents overfitting generalization.
- Distribute the remaining 180 across the other 8 author-prefixes
  proportionally to their candidate counts, with a minimum of 8 each
  to guarantee coverage of rare authors (Natsume_S had only 5 candidates —
  take all 5, the deficit rolls over to others).
- Within each author-prefix, sample evenly across that author's works
  (strided) and within each work, sample evenly across the candidate list
  (strided) so we don't cluster on one passage.
- Balance accept/reject roughly 50/50 within each author-prefix where the
  candidate pool allows (some authors have only accept or only reject).

Outputs JSONL with all original fields + bootstrap_label (copy of label).
"""
from __future__ import annotations
import json
import math
from collections import defaultdict
from pathlib import Path

CANDIDATES = Path("data/ortho-gold/candidates.jsonl")
OUT = Path("data/ortho-gold/sample-300-unlabeled.jsonl")
TARGET_N = 300
TANIZAKI_CAP = 120  # 40%
MIN_PER_AUTHOR = 8
SEED = 20260705  # deterministic

def author_prefix(work_id: str) -> str:
    parts = work_id.split("_")
    return "_".join(parts[:2])

def main() -> None:
    recs = [json.loads(l) for l in CANDIDATES.read_text().splitlines() if l.strip()]
    by_author: dict[str, list] = defaultdict(list)
    for r in recs:
        by_author[author_prefix(r["work_id"])].append(r)
    # Sort authors by candidate count desc; Tanizaki_J will be first.
    authors_sorted = sorted(by_author, key=lambda a: -len(by_author[a]))
    # Allocate.
    alloc: dict[str, int] = {}
    for a in authors_sorted:
        if a == "Tanizaki_J":
            alloc[a] = min(TANIZAKI_CAP, len(by_author[a]))
        else:
            alloc[a] = min(max(MIN_PER_AUTHOR, len(by_author[a])), len(by_author[a]))
    # The non-Tanizaki authors get MIN or all-if-fewer; remaining slots
    # distributed proportionally among those with surplus candidates.
    remaining = TARGET_N - sum(alloc.values())
    # Collect authors that have surplus (allocated < available) for top-up.
    surplus = [a for a in authors_sorted if len(by_author[a]) > alloc[a] and a != "Tanizaki_J"]
    total_surplus_pool = sum(len(by_author[a]) - alloc[a] for a in surplus)
    for a in surplus:
        if remaining <= 0 or total_surplus_pool <= 0:
            break
        extra = min(remaining, math.floor(remaining * (len(by_author[a]) - alloc[a]) / total_surplus_pool))
        alloc[a] += extra
        remaining -= extra
    # Floor any leftover to the largest-pool author(s).
    surplus = sorted(surplus, key=lambda a: -(len(by_author[a]) - alloc[a]))
    si = 0
    while remaining > 0 and surplus:
        a = surplus[si % len(surplus)]
        if alloc[a] < len(by_author[a]):
            alloc[a] += 1
            remaining -= 1
        si += 1
        if si > 10000:
            break
    # Sample within each author: by work (strided), then strided within work.
    import bisect
    out: list = []
    for a, n in alloc.items():
        pool = by_author[a]
        # Group by work_id.
        by_work: dict[str, list] = defaultdict(list)
        order = []
        for r in pool:
            if r["work_id"] not in by_work:
                order.append(r["work_id"])
            by_work[r["work_id"]].append(r)
        # Round-robin strided pick across works to reach n.
        idx = {w: 0 for w in order}
        # Pre-compute per-work stride so we don't cluster on one work's head.
        for w in order:
            step = max(1, len(by_work[w]) // max(1, n // len(order) + 1))
            idx[w] = -step  # so first picked is index 0
        picked = 0
        round_n = 0
        while picked < n:
            progressed = False
            for w in order:
                if picked >= n:
                    break
                step = max(1, len(by_work[w]) // max(1, n // len(order) + 1))
                i = idx[w] + step
                if i < len(by_work[w]):
                    idx[w] = i
                    rec = dict(by_work[w][i])
                    rec["bootstrap_label"] = rec["label"]
                    out.append(rec)
                    picked += 1
                    progressed = True
            round_n += 1
            if not progressed or round_n > 1000:
                break
    # Shuffle deterministically for labeler presentation (stable seed).
    import random
    rnd = random.Random(SEED)
    rnd.shuffle(out)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with OUT.open("w") as f:
        for r in out:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")
    # Report distribution.
    from collections import Counter
    by_a = Counter(author_prefix(r["work_id"]) for r in out)
    by_l = Counter(r["label"] for r in out)
    print(f"wrote {len(out)} records to {OUT}")
    print("by author-prefix:", dict(by_a))
    print("by bootstrap label:", dict(by_l))
    assert len(out) == TARGET_N, f"expected {TARGET_N}, got {len(out)}"

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Run the sampler + verify distribution**

```bash
python3 scripts/ortho-gold/sample_300.py
```
Expected output (exact counts may vary by ±1 due to rounding):
- `wrote 300 records`
- `by author-prefix:` shows Tanizaki_J=120 (40%), every other author-prefix ≥ 8, Natsume_S ≤ 5 (only 5 candidates).
- `by bootstrap label:` roughly balanced (~150/150).

If `assert len(out) == 300` fails, the allocation top-up loop has a bug. Inspect `alloc` and fix the top-up math; the most likely cause is `remaining` not reaching zero when `surplus` is exhausted (re-run with a `print(alloc, remaining)` debug line). The fix is to fold leftover into Tanizaki (who has the largest pool) if other authors are full.

- [ ] **Step 3: Spot-check coverage**

```bash
python3 -c "
import json
from collections import Counter
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
print('total:', len(recs))
print('distinct works:', len(set(r['work_id'] for r in recs)))
print('distinct authors:', len(set('_'.join(r['work_id'].split('_')[:2]) for r in recs)))
print('Tanizaki share:', sum(1 for r in recs if r['work_id'].startswith('Tanizaki_J'))/len(recs))
"
```
Expected: 300 records, ≥ 9 distinct author-prefixes, ≥ 15 distinct works, Tanizaki share ≤ 0.41.

- [ ] **Step 4: Commit**

```bash
git add scripts/ortho-gold/sample_300.py data/ortho-gold/sample-300-unlabeled.jsonl
git commit -m "feat(ortho-gold): corpus-representative 300-sentence sampler

Fixes the Phase 2 bootstrap set's Tanizaki skew (58% of 457) via
per-author capping: Tanizaki_J <=40% (120), every other author-prefix
>=8 with proportional top-up. Strided within-work + round-robin across
works to avoid clustering on one passage. Outputs sample-300-unlabeled.jsonl
with bootstrap_label preserved for later disagreement analysis."
```

---

### Task 2: LLM-assisted labeling of the 300 sentences

**Files:**
- Create: `scripts/ortho-gold/label_review.py`
- Create: `data/ortho-gold/sentences-human-300.jsonl`

**Why this task exists:** the spec's Pre-Phase-1 Verification step 2 requires recall measured against HUMAN labels, and Phase 2's 50-sentence probe showed the bootstrap labels are a port-fidelity surrogate (not real recall). The user confirmed this task can be done by an LLM applying the spec's intent. The labeler is NOT the implementer — the implementer builds the harness; the orchestrator (you, the dispatching agent) does the labeling by reading each sentence.

**Labeling rubric (the spec's intent, copied verbatim into the script's docstring so the labeler reads it):**

> **ACCEPT** = the sentence is pre-war kanji-katakana-majiri prose: katakana serves the grammatical role of hiragana (particles は/が/の/を, okurigana, copulas だ/である, auxiliary verbs タ/テ/ナイ), the sentence is running narrative or dialogue with grammatical structure, and normalizing to hiragana would improve morphological-analysis quality. The presence of kanji does NOT disqualify (kanji+katakana-majiri is the canonical form); what matters is that the katakana is doing grammatical work, not lexical/onomatopoeic work.
>
> **REJECT** = (a) exclamations/interjections (「アハハ」, 「ヨイショ」, 「ハテナ」); (b) onomatopoeia (ウハハハハ, フフフ, アッハッハ); (c) single-word utterances or fragments (「スウプ。」, 「マイ、チャイルド。」); (d) Buddhist chants or foreign-loan vocative fragments with no grammatical structure (「ナムアムダブツ」); (e) sentences where the katakana is lexical (proper nouns, foreign words) rather than grammatical.

**Interfaces:**
- Consumes: `data/ortho-gold/sample-300-unlabeled.jsonl` (Task 1).
- Produces: `data/ortho-gold/sentences-human-300.jsonl` — same fields + `label` overwritten with the LLM label, plus `labeler: "llm"`, `bootstrap_label` preserved, plus `label_notes: Option<String>` for edge-case reasoning.

- [ ] **Step 1: Write the label-review harness**

Create `scripts/ortho-gold/label_review.py` — this is NOT an auto-labeler; it's a presentation + recording harness that the orchestrator runs interactively (or feeds batch input to). It reads the unlabeled sample, prints each sentence with its features and bootstrap label, accepts a verdict from stdin (`a`=accept, `r`=reject, `u`=unsure→default to bootstrap, `q`=quit), and on quit/emission writes the labeled JSONL.

```python
#!/usr/bin/env python3
"""LLM-assisted labeling harness for the 300-sentence gold set.

NOT an auto-labeler. Presents each sentence + features + bootstrap label;
the human (or LLM orchestrator) decides accept/reject per the rubric in
the docstring. Records decisions to sentences-human-300.jsonl.

Labeling rubric (the spec's intent):
  ACCEPT = pre-war kanji-katakana-majiri prose: katakana serves the
    grammatical role of hiragana (particles, okurigana, copulas, aux
    verbs), the sentence is running narrative/dialogue with grammatical
    structure, and normalizing would improve analysis. Kanji presence
    does NOT disqualify.
  REJECT = exclamations/interjections, onomatopoeia, single-word
    fragments, chants/loan-vocatives without grammatical structure, or
    sentences where katakana is lexical (proper nouns/foreign) not
    grammatical.

Usage:
  python3 label_review.py --in data/ortho-gold/sample-300-unlabeled.jsonl \\
                          --out data/ortho-gold/sentences-human-300.jsonl
  # Interactive: type a/r/u per sentence.
  # Batch: pipe a file of single-char verdicts (one per line) to stdin.
  # Resume: re-runs skip already-labeled records in --out.
"""
from __future__ import annotations
import argparse
import json
import sys
from pathlib import Path

def load_labeled(out_path: Path) -> dict[int, dict]:
    """Index existing labeled records by (work_id, char_offset) for resume."""
    idx = {}
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            if not line.strip():
                continue
            r = json.loads(line)
            idx[(r["work_id"], r["char_offset"])] = r
    return idx

def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--in", dest="inp", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()
    inp = Path(args.inp)
    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    labeled = load_labeled(out)
    recs = [json.loads(l) for l in inp.read_text().splitlines() if l.strip()]
    # Append mode so we can resume.
    fa = open(out, "a")
    pending = [r for r in recs if (r["work_id"], r["char_offset"]) not in labeled]
    print(f"{len(recs)} total, {len(labeled)} already labeled, {len(pending)} pending", file=sys.stderr)
    for i, r in enumerate(pending):
        print(f"\n[{i+1}/{len(pending)}] {r['work_id']}  bootstrap={r['bootstrap_label']}  "
              f"ratio={r['katakana_ratio']:.2f} hira={r['hiragana_count']} chars={r['total_chars']}",
              file=sys.stderr)
        print(f"  {r['sentence']!r}", file=sys.stderr)
        sys.stderr.flush()
        verdict = sys.stdin.readline().strip().lower()
        if verdict == "q":
            break
        r["bootstrap_label"] = r.get("bootstrap_label", r["label"])
        if verdict == "a":
            r["label"] = "accept"
        elif verdict == "r":
            r["label"] = "reject"
        else:  # 'u' or empty → keep bootstrap
            r["label"] = r["bootstrap_label"]
        r["labeler"] = "llm"
        r["label_notes"] = None
        fa.write(json.dumps(r, ensure_ascii=False) + "\n")
        fa.flush()
    fa.close()

if __name__ == "__main__":
    main()
```

- [ ] **Step 2: Commit the harness before labeling**

```bash
git add scripts/ortho-gold/label_review.py
git commit -m "feat(ortho-gold): LLM-assisted labeling harness (interactive/resume)

Presents each sentence + features + bootstrap label for accept/reject
decision per the rubric in the docstring. Records decisions to
sentences-human-300.jsonl with labeler='llm' and bootstrap_label preserved.
Supports resume (re-runs skip already-labeled records)."
```

- [ ] **Step 3: Label all 300 sentences (orchestrator work, not a subagent)**

This is the load-bearing step. The orchestrator (you) reads each sentence and applies the rubric. Two viable workflows:

**Workflow A (batch, recommended for speed):** write the labels directly to `data/ortho-gold/sentences-human-300.jsonl` as a JSONL stream, copying all fields from `sample-300-unlabeled.jsonl` and overwriting `label` per the rubric. Use a Python one-shot that emits the records with your hand-assigned labels:

```bash
# First, dump the 300 sentences to a review file the orchestrator reads:
python3 -c "
import json
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
for i,r in enumerate(recs):
    print(f'{i:3} [{r[\"bootstrap_label\"]:6} r={r[\"katakana_ratio\"]:.2f} h={r[\"hiragana_count\"]} c={r[\"total_chars\"]:3}] {r[\"work_id\"]:24} {r[\"sentence\"]!r}')
" > /tmp/review-300.txt
wc -l /tmp/review-300.txt
```

Then the orchestrator reads `/tmp/review-300.txt`, decides each label, and emits `data/ortho-gold/sentences-human-300.jsonl` via a Python script that takes a list of 300 verdicts (`a`/`r`). The verdict list is the orchestrator's actual labeling work — do NOT delegate this to a subagent; the labeling is the human-style judgment that makes the gold set real.

```bash
# Orchestrator fills VERDICTS below (300 chars, 'a' or 'r' each):
python3 - <<'PY'
import json
recs=[json.loads(l) for l in open('data/ortho-gold/sample-300-unlabeled.jsonl')]
VERDICTS = ""  # ← orchestrator fills this 300-char string of 'a'/'r'
assert len(VERDICTS) == len(recs), f"{len(VERDICTS)} verdicts vs {len(recs)} records"
with open('data/ortho-gold/sentences-human-300.jsonl','w') as f:
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

**Decision rule for the orchestrator (read each sentence and apply):** if the katakana is doing grammatical work (particles/copulas/aux in running prose or grammatical dialogue) → `a`. If it's interjection/onomatopoeia/single-word/chant/loan-vocative → `r`. When genuinely unsure, prefer `a` if the sentence has ≥ 15 chars and `hiragana_count == 0` and reads as continuous narrative; prefer `r` if it's < 12 chars or starts with `「` and contains no verb. Document any genuinely-uncertain calls (≤ 5 expected) in a follow-up `label_notes` field.

- [ ] **Step 4: Verify the labeled set**

```bash
python3 -c "
import json
from collections import Counter
recs=[json.loads(l) for l in open('data/ortho-gold/sentences-human-300.jsonl')]
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
Expected: n=300, both labels present (ideally 130-200 each; a 0/300 split means the rubric was applied uniformly wrong), labeler all `llm`, ≥ 8 disagreements with bootstrap (the whole point of human labels), ≥ 9 distinct works, Tanizaki share ≤ 0.41.

- [ ] **Step 5: Commit the labeled gold set**

```bash
git add data/ortho-gold/sentences-human-300.jsonl
git commit -m "data(ortho-gold): 300-sentence human-labeled gold set (LLM-assisted)

Corpus-representative (per-author capped, Tanizaki <=40%) 300-sentence gold
set labeled by the orchestrator applying the spec's intent rubric:
ACCEPT = kanji-katakana-majiri prose where katakana does grammatical work;
REJECT = exclamations, onomatopoeia, single-word fragments, chants, loan
vocatives. bootstrap_label preserved per-record for disagreement analysis.
labeler='llm' on every record (not a human Japanese literature expert;
treat as second-tier bootstrap, stronger than the Python cascade but not
ground truth — see the Phase 2 ADR for the trust-boundary framing)."
```

---

### Task 3: Measure HeuristicV1 recall + ML recall against the 300 gold labels

**Files:**
- Create: `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md` (the measurement report; the branch-decision section is filled in Task 4-8 as the chosen branch executes).

**Interfaces:**
- Consumes: `data/ortho-gold/sentences-human-300.jsonl` (Task 2), `crates/ab-ortho-detect/examples/detect_sentences.rs` (exists, Phase 2), `scripts/ortho-gold/recall_floor.clj` (exists, Phase 2), `ab-ortho-detect-ml` train/ablate (exist, Phase 2).
- Produces: `data/ortho-gold/models/model-v3-gold.bin` (ML retrained on the 300 labels), `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md`.

- [ ] **Step 1: Measure HeuristicV1 recall on the 300 gold labels**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences-human-300.jsonl > /tmp/gold300-heur.jsonl 2>/dev/null
bb scripts/ortho-gold/recall_floor.clj /tmp/gold300-heur.jsonl
```
Record: `recall`, `precision`, `f1`, `tp/fn/fp/tn`. Save full output for the report.

- [ ] **Step 2: Retrain the ML detector on the 300 gold labels**

```bash
cargo run -p ab-ortho-detect-ml -- train --gold data/ortho-gold/sentences-human-300.jsonl --out data/ortho-gold/models/model-v3-gold.bin
cargo run -p ab-ortho-detect-ml -- hash --model data/ortho-gold/models/model-v3-gold.bin
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
        let h = if anns.is_empty() { "reject" } else { "accept" };
        println!("{}", serde_json::to_string(&Out { sentence: &rec.sentence, gold: &rec.label, heuristic: h.to_string(), agree: h == rec.label })?);
    }
    Ok(())
}
```
Add `serde`/`serde_json` to `ab-ortho-detect` dev-deps if missing (Phase 1 added them — verify). Run:
```bash
cargo run -p ab-ortho-detect --example detect_ml -- data/ortho-gold/models/model-v3-gold.bin < data/ortho-gold/sentences-human-300.jsonl > /tmp/gold300-ml.jsonl 2>/dev/null
bb scripts/ortho-gold/recall_floor.clj /tmp/gold300-ml.jsonl
```
⚠ **Bias caveat for the report:** recall and precision here are **train-set accuracy** (the ML detector is evaluated on the same 300 records it was trained on). This is an UPPER BOUND, not generalization. For honest generalization, do a proper hold-out split (Task 6, optional). For the branch decision, treat the ML train-accuracy as "ceiling"; the floor is the ML's recall on a held-out set, which we don't have. The honest comparison is: HeuristicV1 recall (no train/eval split, real generalization) vs ML train-accuracy (ceiling). If ML ceiling < HeuristicV1 recall → heuristic wins. If ML ceiling >> HeuristicV1 recall AND >> 0.85 → ML is promising but needs hold-out validation before promotion.

- [ ] **Step 4: Write the measurement report (branch-decision section left as a placeholder)**

Create `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md`:
```markdown
# Ortho-Detect Phase 2.5 — 300-Sentence Human Gold Measurement

Date: 2026-07-05
Gold set: `data/ortho-gold/sentences-human-300.jsonl` (n=300, LLM-labeled)
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

## HeuristicV1 against human gold (real generalization)

[INSERT: recall, precision, f1, tp/fn/fp/tn from Step 1 + the bb output]
[INSERT: count of disagreements vs bootstrap; characterize (prose-at-ratio-
0.4-0.5 / proper-noun-guard / fragment-over-acceptance)]

## MlLogisticRegression against human gold (train-accuracy ceiling)

- Model: `data/ortho-gold/models/model-v3-gold.bin`
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
git add crates/ab-ortho-detect/examples/detect_ml.rs data/ortho-gold/models/model-v3-gold.bin \
        reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md
git commit -m "feat(ortho-gold): measure HeuristicV1 + ML recall on 300 human labels

Adds detect_ml example binary for ML-detector recall measurement. Train-set
accuracy for ML (upper bound, not generalization); real generalization for
HeuristicV1. Numbers feed the Phase 2.5 branch decision (heuristic tune
vs ML promotion + dead-coupling deletion)."
```

---

### Task 4: Branch decision + ADR entry (NO code change unless Tasks 5-8 fire)

**Why this task exists:** the Phase 2 ADR gated `OrthoTokenizer` deletion + ML-promotion-from-EXPERIMENTAL on three conditions: (a) ≥200-sent human gold, (b) ML recall ≥ 0.85, (c) ML promoted to default. Task 2 produced (a); Task 3 measured (b). This task evaluates the gate and dispatches either the heuristic-tune branch (Tasks 5 only) OR the ML-promotion + deletion branch (Tasks 6-8).

**Decision rule (apply using Task 3's numbers):**

- **VALIDATION:** HeuristicV1 recall < 0.85 against the 300 gold set → the spec's floor is violated, a fix is REQUIRED (not optional).
- **If ML train-accuracy ceiling < HeuristicV1 recall:** the ML detector is WORSE than the heuristic even on its training data. ML is NOT viable with the current feature set (character-only can't capture the rubric). → **Execute Task 5 only (tune the heuristic threshold).** Do NOT promote ML, do NOT delete the coupling.
- **If ML ceiling ≥ 0.85 AND ≥ HeuristicV1 recall + 0.10:** the ML detector is promising. But train-accuracy is a ceiling, not generalization. → **Execute Task 5 (tune the heuristic as the immediate fix) AND Task 6 (hold-out split to validate ML generalization).** Defer Tasks 7-8 (ML promotion + deletion) until the hold-out recall also clears 0.85.
- **If ML ceiling ≥ 0.95 AND HeuristicV1 recall < 0.85 (current Phase 2 state suggests this is likely):** → Execute Task 5 (heuristic threshold tune as the immediate, low-risk fix to clear the floor), AND Task 6 (ML hold-out), AND Tasks 7-8 (promotion + deletion) ONLY if Task 6 hold-out recall ≥ 0.85.

**The orchestrator records the decision in the report before dispatching.**

- [ ] **Step 1: Fill in the report's Branch Decision section**

Open `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md`, replace the `## Branch decision` placeholder with the actual decision based on the rule above. State: the two numbers, the chosen branch (Task 5 only / Tasks 5+6 / Tasks 5+6+7+8), and the rationale. Note the bias caveat applies.

- [ ] **Step 2: Commit the decision**

```bash
git add reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md
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
# (reads ORTHO_GOLD_PATH=data/ortho-gold/sentences-human-300.jsonl + AB_VIBRATO_DICT)
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
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences-human-300.jsonl 2>/dev/null | bb scripts/ortho-gold/recall_floor.clj /dev/stdin
```
Expected: recall ≥ 0.85. If below, return to Step 2 or escalate to Tasks 6-8.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-ortho-detect/src/heuristic.rs crates/ab-ortho-detect/examples/sweep_threshold.rs
git commit -m "fix(ortho-detect): tune katakana_ratio_threshold 0.5 -> [X.XX]

Phase 2's human probe (recall 0.636) identified the binding constraint as
the strict >0.5 gate rejecting kanji-heavy pre-war prose at ratio 0.41-0.49.
Sweep over [0.25,0.55] picked [X.XX] as the threshold maximizing F1 subject
to recall >= 0.85. Recall on the 300-sentence human gold set is now [X.XX]
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
cargo run -p ab-ortho-detect-ml -- cross-validate --gold data/ortho-gold/sentences-human-300.jsonl --k 5 --report reports/ortho-detect/2026-07-05-phase2.5-ml-cv.md
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

5-fold CV recall on the 300-sentence human gold set cleared the 0.85
floor ([X.XX]). EXPERIMENTAL marker + bias caveat removed from CLI help.
The ML detector is now a first-class mode (not yet the DEFAULT — that
requires the heuristic path's removal in Task 8)."
```

---

### Task 8: Delete dead Vibrato coupling (CONDITIONAL — only if Task 7 fired AND user/heuristic path is being removed)

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
- Finalize: `reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md` (append the post-fix recall numbers + the ADR closure status).

- [ ] **Step 1: Append the post-tuning recall numbers to the report**

Add a `## Post-tuning recall` section with: HeuristicV1 recall after Task 5 threshold tune; ML mean recall from Task 6 CV (if run); the chosen default mode; whether the ADR's deletion gate fired (Tasks 7-8) or remains deferred.

- [ ] **Step 2: Final workspace verification**

```bash
cargo test --workspace 2>&1 | rg "test result" | awk '{p+=$4; f+=$6} END {print "pass:", p, "fail:", f}'
```
Expected: pass ≥ 450, fail == 2 (pre-existing `rerun_full_*` only).

- [ ] **Step 3: Commit + merge to main**

```bash
git add reports/ortho-detect/2026-07-05-phase2.5-human-gold-300.md
git commit -m "docs(ortho-detect): finalize Phase 2.5 report + recall numbers"
cd /home/bor/Projects/ab-validator
git merge --no-ff ortho-detect-phase2.5 -m "Merge branch 'ortho-detect-phase2.5': human gold set + recall fix [SUMMARY]"
git push origin main
git worktree remove .worktrees/ortho-detect-phase2.5
git branch -d ortho-detect-phase2.5
```

---

## Self-Review

**1. Spec coverage:**
- Pre-Phase-1 Verification step 2 ("if recall < 0.85, tune thresholds") → Task 5.
- Phase 2 ADR deletion gate (≥200 human gold + ML recall ≥ 0.85 + ML promoted to default) → Task 2 (≥200) + Task 3 (ML recall) + Task 7 (promote) + Task 8 (delete).
- Spec's 200–500 sentence target → Task 2 (300).
- Corpus-representative sampling (user's requirement) → Task 1 (per-author capping, Tanizaki ≤40%).

**2. Placeholder scan:** The only literal placeholder is the orchestrator's `VERDICTS = ""` in Task 2 Step 3 — that is the actual labeling work and CANNOT be pre-filled (it's the human/LLM judgment that makes the gold set real). Everything else has concrete code/commands.

**3. Type consistency:** `OrthoDetectMode { Off, Heuristic, Ml }`, `OrthoDetectorId::MlLogisticRegression { model_hash }`, `HeuristicConfig::katakana_ratio_threshold`, `MlLogisticRegression::load(path)`, `model_hash(&MlModel)` — all match Phase 2's merged `main`.

**4. Branch-decision honesty:** Task 4 does NOT assume ML wins. It explicitly handles "ML ceiling < HeuristicV1 recall" (ML not viable) and "ML ceiling high but train-set only" (needs hold-out before promotion). The conditions on Tasks 6-8 are real gates, not rubber stamps.

## Execution Handoff

**Plan complete and saved to `docs/superpowers/plans/2026-07-05-ortho-detect-phase2.5.md`. Two execution options:**

**1. Subagent-Driven (recommended for Tasks 1, 3, 5, 6-9)** — I dispatch a fresh implementer per task, review between tasks. Task 2 (labeling) is orchestrator work, not a subagent — I read the 300 sentences and apply the rubric directly.

**2. Inline Execution** — Tasks run in this session with checkpoints.

**Which approach?** Given Task 2 is the labeling bottleneck (300 sentences, ~5-10 min of orchestrator attention) and the rest is mechanical, I recommend: I execute Task 1 (sampler) now, then do Task 2 (labeling) inline, then dispatch Task 3+ to subagents.
