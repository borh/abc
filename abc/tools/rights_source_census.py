#!/usr/bin/env python3
"""Census of the Aozora catalog's rights-bearing fields.

Emits every number quoted in
`docs/evidence/external/aozora-rights-source-contract.md`, so that document's
Observed Facts can be re-derived rather than trusted. Reads the catalog ZIP
straight out of a pinned aozorabunko checkout: the census is a function of the
snapshot commit and nothing else, and it depends on no ingested corpus.

Every measurement here is lexical -- which cell values appear and how they
co-occur. Nothing in the output carries a legal or operational reading of a
value; those are the evidence document's Open Questions. This reports and
always exits zero. It is a census, not a gate: the invariants it surfaces
become enforceable only once a policy exists for them to defend.

Usage: python3 tools/rights_source_census.py <aozorabunko-root>
"""

from __future__ import annotations

import collections
import csv
import hashlib
import io
import re
import sys
import zipfile
from urllib.parse import urlparse

ZIP_RELATIVE = "index_pages/list_person_all_extended_utf8.zip"

# Aozora's own server. Split out because `guide/kijyunn.html` in the same
# pinned checkout makes where a file sits, and whether its card carries a
# permission notice, the operative distinction for a protected work.
AOZORA_HOST = "www.aozora.gr.jp"

# A reporting parameter with no authority, not a legal constant and not a
# threshold this census claims to have located. It is the latest parseable
# death year observed among `なし` persons in the pinned snapshot. The catalog
# holds no 1968 or 1969 deaths at all, so the data is equally consistent with a
# threshold anywhere in 1967-1969. What legal threshold is available to compare
# against is the evidence document's Q1; whether any such threshold produced or
# governs these flags is Q6.
OBSERVED_NASHI_YEAR_FRONTIER = 1967

# 没年月日 is a Japanese-catalog date field, not an ISO one: it carries 3- and
# 4-digit CE years, partial precision, `不詳`, and 前N / 紀元前N世紀 BCE prose.
# Anchoring on a 3-or-more-digit run keeps 陳寿 (297) and 沈約 (513) parseable,
# which a \d{4} anchor would silently drop into the "no death year" bucket and
# so overstate how much of the catalog resists corroboration.
YEAR_PREFIX = re.compile(r"^(-?\d{3,4})")

Row = dict[str, str]


def death_year(value: str) -> int | None:
    """The CE year in a 没年月日 cell, or None when it carries no usable year."""
    match = YEAR_PREFIX.match(value.strip())
    return int(match.group(1)) if match else None


def is_translated(work_rows: list[Row]) -> bool:
    """Whether a work carries either catalog signal of translation.

    Either is sufficient: a `翻訳者` contributor row, or a non-empty 原題. Both
    are read because neither alone covers the other -- F6 counts 742 なし works
    with a translator against 262 carrying an original title. What follows from
    a work being translated is Q4 and Q5, not something this predicate decides.
    """
    return any(row["役割フラグ"] == "翻訳者" for row in work_rows) or bool(
        work_rows[0]["原題"].strip()
    )


def load(root: str) -> tuple[list[Row], str, str]:
    zip_path = f"{root}/{ZIP_RELATIVE}"
    with open(zip_path, "rb") as handle:
        raw = handle.read()
    with zipfile.ZipFile(io.BytesIO(raw)) as archive:
        csv_bytes = archive.read(archive.namelist()[0])
    rows = list(csv.DictReader(io.StringIO(csv_bytes.decode("utf-8-sig"))))
    return (
        rows,
        f"sha256:{hashlib.sha256(raw).hexdigest()}",
        f"sha256:{hashlib.sha256(csv_bytes).hexdigest()}",
    )


def main(root: str) -> int:
    rows, zip_hash, csv_hash = load(root)

    by_work: dict[str, list[Row]] = collections.defaultdict(list)
    for row in rows:
        by_work[row["作品ID"]].append(row)
    persons: dict[str, Row] = {}
    for row in rows:
        persons.setdefault(row["人物ID"], row)

    print("== snapshot binding ==")
    print(f"  archive              {ZIP_RELATIVE}")
    print(f"  archive sha256       {zip_hash}")
    print(f"  csv sha256           {csv_hash}")
    print(f"  rows (work x person) {len(rows)}")
    print(f"  distinct works       {len(by_work)}")
    print(f"  distinct persons     {len(persons)}")

    print("\n== F0 denormalization consistency ==")
    # Several measurements below collapse the denormalized table by taking one
    # representative row per work or per person, which is only sound while every
    # repeated cell agrees across a key's rows. Reported rather than assumed: a
    # conflicting cell would make those counts a function of row order instead
    # of of the data. It does not touch the measurements that consume every row
    # -- role counts, the contributor-flag side of F2, translator detection.
    #
    # A reported conflict does not change the exit status, which is always zero
    # (this is a census, not a gate); it invalidates the representative-row
    # measurements, which must then not be read as facts about the snapshot.
    for key, columns in (
        ("作品ID", ("作品著作権フラグ", "原題", "テキストファイルURL", "作品名")),
        ("人物ID", ("人物著作権フラグ", "没年月日", "姓", "名")),
    ):
        grouped: dict[str, list[Row]] = collections.defaultdict(list)
        for row in rows:
            grouped[row[key]].append(row)
        for column in columns:
            conflicting = [k for k, rs in grouped.items() if len({r[column] for r in rs}) > 1]
            suffix = f"  e.g. {conflicting[:3]}" if conflicting else ""
            print(f"  {key} / {column}: {len(conflicting)} conflicting{suffix}")

    print("\n== F1 flag distribution ==")
    works = collections.Counter(r[0]["作品著作権フラグ"] for r in by_work.values())
    people = collections.Counter(p["人物著作権フラグ"] for p in persons.values())
    print(f"  works   なし={works['なし']} あり={works['あり']}")
    print(f"  persons なし={people['なし']} あり={people['あり']}")
    roles = collections.Counter(r["役割フラグ"] for r in rows)
    print("  roles   " + " ".join(f"{k}={v}" for k, v in roles.most_common()))

    print("\n== F2 work flag == disjunction over contributor person flags ==")
    disagreements = [
        work_id
        for work_id, work_rows in by_work.items()
        if work_rows[0]["作品著作権フラグ"]
        != ("あり" if any(r["人物著作権フラグ"] == "あり" for r in work_rows) else "なし")
    ]
    print(f"  works checked {len(by_work)}  disagreements {len(disagreements)}")

    print("\n== F3 observed death-year frontier among なし records ==")
    for flag in ("なし", "あり"):
        group = [p for p in persons.values() if p["人物著作権フラグ"] == flag]
        years = [y for y in (death_year(p["没年月日"]) for p in group) if y is not None]
        print(
            f"  {flag}: persons={len(group):5d} with-year={len(years):5d} "
            f"min={min(years)} max={max(years)} no-year={len(group) - len(years)}"
        )
    print("  persons by death year across the frontier (note the empty 1968-1969):")
    for year in range(1965, 1971):
        counts = collections.Counter(
            p["人物著作権フラグ"] for p in persons.values() if death_year(p["没年月日"]) == year
        )
        print(f"    {year}: なし={counts['なし']:3d} あり={counts['あり']:3d}")

    print("\n== F4 あり persons with a parseable year at or below the frontier ==")
    exceptions = sorted(
        (
            p
            for p in persons.values()
            if p["人物著作権フラグ"] == "あり"
            and (death_year(p["没年月日"]) or 9999) <= OBSERVED_NASHI_YEAR_FRONTIER
        ),
        key=lambda p: p["没年月日"],
    )
    for person in exceptions:
        titles = "; ".join(
            f"{w[0]['作品名']} (原題 {w[0]['原題']})"
            for w in by_work.values()
            if any(r["人物ID"] == person["人物ID"] for r in w)
        )
        print(
            f"  {person['没年月日']:12} {person['姓']}{person['名']} "
            f"role={person['役割フラグ']}  {titles}"
        )

    print("\n== F5 text-file URL presence by flag ==")
    ari = [w for w in by_work.values() if w[0]["作品著作権フラグ"] == "あり"]
    nashi = [w for w in by_work.values() if w[0]["作品著作権フラグ"] == "なし"]
    for label, group in (("なし", nashi), ("あり", ari)):
        with_text = sum(1 for w in group if w[0]["テキストファイルURL"].strip())
        print(f"  {label}: works={len(group):5d} with テキストファイルURL={with_text:5d}")

    print("\n== F5b text-file URL host by flag ==")
    # Aozora's own handling rules key on where a file sits and on per-card
    # permission notices, so host is a rights-relevant lexical property that
    # mere URL presence does not capture.
    for label, group in (("なし", nashi), ("あり", ari)):
        hosts = collections.Counter(
            urlparse(w[0]["テキストファイルURL"].strip()).netloc
            for w in group
            if w[0]["テキストファイルURL"].strip()
        )
        total = sum(hosts.values())
        own = hosts.pop(AOZORA_HOST, 0)
        print(f"  {label}: with URL={total:5d}  {AOZORA_HOST}={own:5d}")
        print(f"    elsewhere={total - own} across {len(hosts)} hosts")
        for host, count in hosts.most_common(5):
            print(f"      {count:4d}  {host}")

    print("\n== F6 translation signals by flag ==")
    for label, group in (("なし", nashi), ("あり", ari)):
        translators = sum(1 for w in group if any(r["役割フラグ"] == "翻訳者" for r in w))
        titles = sum(1 for w in group if w[0]["原題"].strip())
        print(
            f"  {label}: works={len(group):5d} with 翻訳者={translators:5d} with 原題={titles:5d}"
        )

    print("\n== F7 なし persons with no CE year parseable by this census ==")
    unusable = [
        p
        for p in persons.values()
        if p["人物著作権フラグ"] == "なし" and death_year(p["没年月日"]) is None
    ]
    print(f"  count {len(unusable)}")
    for person in sorted(unusable, key=lambda p: p["人物ID"]):
        print(f"    {person['姓']}{person['名']} 没年月日={person['没年月日']!r}")

    print("\n== coverage of the flag-versus-threshold comparison ==")
    # Stated as coverage rather than as a bare agreement count: a person with no
    # parseable year yields neither agreement nor disagreement, so folding the
    # 179 of them into a "holds except for four" claim would report an
    # uncorroborated record as a corroborated one.
    parseable = [p for p in persons.values() if death_year(p["没年月日"]) is not None]
    disagreeing = [
        p
        for p in parseable
        if (death_year(p["没年月日"]) <= OBSERVED_NASHI_YEAR_FRONTIER)
        != (p["人物著作権フラグ"] == "なし")
    ]
    print(f"  persons in the comparison's domain (parseable CE year) : {len(parseable)}")
    print(
        f"    agreeing with the frontier                           : {len(parseable) - len(disagreeing)}"
    )
    print(f"    disagreeing (the F4 exception set)                   : {len(disagreeing)}")
    print(
        "  persons outside the domain, neither agreeing nor disagreeing : "
        f"{len(persons) - len(parseable)}"
    )

    print("\n== non-normative census partition (work level, no outcome attached) ==")
    # Deliberately unlabelled. Which partition may carry which rights outcome is
    # Q6-Q8 in the evidence document; naming the rows for their hoped-for
    # outcomes here would smuggle the undecided answer into the measurement.
    buckets: collections.Counter[str] = collections.Counter()
    for work_rows in by_work.values():
        if work_rows[0]["作品著作権フラグ"] == "あり":
            buckets["ari"] += 1
            continue
        years = [death_year(r["没年月日"]) for r in work_rows]
        date_checkable = all(y is not None and y <= OBSERVED_NASHI_YEAR_FRONTIER for y in years)
        translated = is_translated(work_rows)
        if date_checkable and not translated:
            buckets["nashi-date-checkable-untranslated"] += 1
        elif date_checkable:
            buckets["nashi-translation-signal"] += 1
        elif translated:
            buckets["nashi-both"] += 1
        else:
            buckets["nashi-unparseable-date"] += 1
    residual = (
        buckets["nashi-translation-signal"]
        + buckets["nashi-unparseable-date"]
        + buckets["nashi-both"]
    )
    print(
        f"  なし, date-checkable, no translation signal : {buckets['nashi-date-checkable-untranslated']}"
    )
    print(f"  なし, not date-checkable or translation-signalled : {residual}")
    print(
        f"    translation signal, dates otherwise checkable   : {buckets['nashi-translation-signal']}"
    )
    print(
        f"    unparseable date only                           : {buckets['nashi-unparseable-date']}"
    )
    print(f"    both                                            : {buckets['nashi-both']}")
    print(f"  あり                                        : {buckets['ari']}")
    print(f"  total                                       : {sum(buckets.values())}")
    return 0


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        raise SystemExit(2)
    raise SystemExit(main(sys.argv[1]))
