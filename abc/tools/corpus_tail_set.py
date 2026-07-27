#!/usr/bin/env python3
"""Propose natural-tail selections by rank. NOT governed membership.

Reads a `tools/corpus_inventory.py` inventory and emits the archives that are
extreme on each notation dimension. Selection is deterministic: given the same
inventory the same archives are chosen, with ties broken on archive path so the
order is total.

Why rank rather than a random sample: a sample characterises the middle of the
distribution, and the envelope is set by the tail. Why several dimensions rather
than size alone: measurement showed the extremes lie on largely independent axes
-- only 5 of 30 slots overlapped -- so a "largest N archives" selection misses
them. The longest-line archive in the pinned snapshot is 57 KB, far from the
largest, and parses in 33 ms.

TWO PROJECTIONS, NOT ONE UNION
------------------------------
The dimensions do not all bear on the same thing. `bytes`, `ruby`, and `lines`
were measured to drive parse latency and peak memory; `max_line`, `gaiji`, and
`annot_non_gaiji` were not -- the `max_line` extremes are the *fastest* archives
measured. Emitting one undifferentiated union would let a correctness-motivated
archive be read as resource-envelope evidence. So two projections are emitted
and the caller decides which tier consumes which.

This proposes. It attaches no expectation, threshold, or authority to any
archive, and it inherits its input's non-authoritative admission approximation:
governed membership must be projected from the authoritative source-bundle
census and from publication's own source-selection projection. See
`docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.

Usage: python3 tools/corpus_tail_set.py <inventory.json> <top-k> [output.json]
"""

from __future__ import annotations

import json
import sys

# (metric, projection, rationale). `projection` decides which output list an
# archive lands in, and therefore which tier may cite it as evidence.
DIMENSIONS: tuple[tuple[str, str, str], ...] = (
    ("bytes", "resource", "largest source: parse throughput and peak memory"),
    ("ruby", "resource", "most ruby markers: inline span emission volume"),
    ("lines", "resource", "most lines: per-line overhead accumulation"),
    ("max_line", "correctness", "longest single line: line-oriented buffer behaviour"),
    ("gaiji", "correctness", "most gaiji markers: external-character resolution"),
    ("annot_non_gaiji", "correctness", "most non-gaiji annotations: editor-note handling"),
)

PROJECTIONS = ("resource", "correctness")


def rank(works: list[dict[str, object]], metric: str, top_k: int) -> list[dict[str, object]]:
    return sorted(works, key=lambda w: (-int(w[metric]), str(w["archive"])))[:top_k]


def project(works: list[dict[str, object]], top_k: int, projection: str) -> dict[str, list[str]]:
    """Archives extreme on any dimension belonging to one projection."""
    hits: dict[str, list[str]] = {}
    for metric, belongs, _why in DIMENSIONS:
        if belongs != projection:
            continue
        for work in rank(works, metric, top_k):
            hits.setdefault(str(work["archive"]), []).append(metric)
    return dict(sorted(hits.items()))


def main(inventory_path: str, top_k: int, out_path: str | None) -> int:
    with open(inventory_path, encoding="utf-8") as handle:
        inventory = json.load(handle)
    works = inventory["works"]
    if inventory.get("authoritative"):
        print(
            "input claims authority; this tool expects the exploratory inventory", file=sys.stderr
        )
        return 1

    by_metric = {m: rank(works, m, top_k) for m, _p, _w in DIMENSIONS}
    for metric, belongs, why in DIMENSIONS:
        print(f"\n== top {top_k} by {metric} [{belongs}] -- {why}")
        for work in by_metric[metric]:
            print(
                f"   {work['archive']:52s} {metric}={int(work[metric]):>9,}"
                f"  bytes={int(work['bytes']):>9,}"
            )

    sizes = {str(w["archive"]): int(w["bytes"]) for w in works}
    payload: dict[str, object] = {
        "measurement_construction": "abc-corpus-tail-proposal-v1",
        "authoritative": False,
        "top_k": top_k,
        "selection_basis": [
            {"metric": m, "projection": p, "rationale": w} for m, p, w in DIMENSIONS
        ],
        # Named for what it observed, not for a classification it cannot make:
        # "stock zipfile could not open this" is NOT "ABC recovers it", "it is in
        # publication selection", or "these four share a failure class". The
        # source-bundle summary distinguishes damaged paths, declared/actual size
        # mismatch, admitted, and rejected-unreadable; only the authoritative
        # projection may classify an archive as recovered, rejected, or
        # tier-eligible. Emitted sorted, since the inventory sorts its skipped list.
        "stock_zipfile_skipped_archives": [e["archive"] for e in inventory["skipped"]],
    }
    for projection in PROJECTIONS:
        hits = project(works, top_k, projection)
        total = sum(sizes[a] for a in hits)
        multi = {a: m for a, m in hits.items() if len(m) > 1}
        print(f"\n== {projection} projection: {len(hits)} archives, {total / 1e6:.1f} MB")
        print(f"   extreme on more than one {projection} metric : {len(multi)}")
        for archive, metrics in multi.items():
            print(f"     {archive}  {metrics}")
        payload[f"{projection}_archives"] = [
            {"archive": a, "extreme_on": m} for a, m in hits.items()
        ]

    overlap = set(project(works, top_k, "resource")) & set(project(works, top_k, "correctness"))
    print(f"\n== archives in BOTH projections: {len(overlap)}")
    for archive in sorted(overlap):
        print(f"     {archive}")
    print(f"\n== stock-zipfile-skipped archives: {len(payload['stock_zipfile_skipped_archives'])}")
    for archive in payload["stock_zipfile_skipped_archives"]:  # type: ignore[union-attr]
        print(f"     {archive}")

    if out_path:
        with open(out_path, "w", encoding="utf-8") as handle:
            json.dump(payload, handle, ensure_ascii=False, indent=1)
    return 0


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4):
        print(__doc__, file=sys.stderr)
        raise SystemExit(2)
    raise SystemExit(
        main(sys.argv[1], int(sys.argv[2]), sys.argv[3] if len(sys.argv) == 4 else None)
    )
