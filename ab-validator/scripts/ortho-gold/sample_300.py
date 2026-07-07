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
    recs = [json.loads(line) for line in CANDIDATES.read_text().splitlines() if line.strip()]
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
    print(
        f"Tanizaki share: {sum(1 for r in out if r['work_id'].startswith('Tanizaki_J')) / len(out):.2f}"
    )
    assert len(out) == TARGET_N, f"expected {TARGET_N}, got {len(out)} — pool may be too small"


if __name__ == "__main__":
    main()
