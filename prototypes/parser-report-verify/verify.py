#!/usr/bin/env python3
"""Disposable verifier: cross-check PARSER_REPORT.md §0 feature taxonomy
against the two canonical Aozora sources (chuki_tag.txt + annotation/*.html).

Does NOT modify PARSER_REPORT.md. Read-only on references/.

Verdict convention (disjoint partition of marker-bearing features):
  VERIFIED      - marker found in BOTH canonical sources
  CHUKI-ONLY    - marker found only in chuki_tag.txt
  MANUAL-ONLY   - marker found only in annotation/*.html
  UNVERIFIED    - marker in neither canonical source (likely report error)
  NO-MARKER     - feature row carries no explicit ［＃...］ example to check

The trust statistic uses the union "found in at least one canonical source"
(= VERIFIED + CHUKI-ONLY + MANUAL-ONLY) to honour the task's literal wording.
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.abspath(__file__))
# Resolve project paths relative to the script location (prototypes/.. ).
PROJECT = os.path.abspath(os.path.join(ROOT, "..", ".."))
REPORT = os.path.join(PROJECT, "references", "PARSER_REPORT.md")
CHUKI = os.path.join(
    PROJECT, "references", "parsers", "AozoraEpub3-JDK21", "chuki_tag.txt"
)
ANNOT_DIR = os.path.join(PROJECT, "references", "aozorabunko", "annotation")

MARKER_RE = re.compile(r"［＃([^］]*)］")
# Full/short width digit runs and the report's Ｎ placeholder -> single "N".
DIGIT_RE = re.compile(r"[0-9０-９]+")


def normalize(s):
    s = s.strip()
    s = DIGIT_RE.sub("N", s)
    s = s.replace("Ｎ", "N")  # fullwidth Latin capital N (U+FF2E)
    return s


def strip_html(text):
    return re.sub(r"<[^>]*>", "", text)


# --------------------------------------------------------------------------- #
# Source 1: chuki_tag.txt first column
# --------------------------------------------------------------------------- #
def load_chuki_set():
    entries = set()
    with open(CHUKI, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\r\n")
            if not line or line.startswith("#"):
                continue
            name = line.split("\t")[0].strip()
            if name:
                entries.add(normalize(name))
    return entries


# --------------------------------------------------------------------------- #
# Source 2: annotation/*.html demonstrated markers (tags stripped first)
# --------------------------------------------------------------------------- #
def load_manual_set():
    entries = set()
    if not os.path.isdir(ANNOT_DIR):
        return entries
    for fn in sorted(os.listdir(ANNOT_DIR)):
        if not fn.endswith(".html"):
            continue
        with open(os.path.join(ANNOT_DIR, fn), encoding="utf-8") as f:
            text = strip_html(f.read())
        for inner in MARKER_RE.findall(text):
            n = normalize(inner)
            if n:
                entries.add(n)
    return entries


# --------------------------------------------------------------------------- #
# Parse PARSER_REPORT.md §0 feature rows
# --------------------------------------------------------------------------- #
def parse_section0():
    with open(REPORT, encoding="utf-8") as f:
        lines = f.readlines()

    start = end = None
    for i, ln in enumerate(lines):
        if start is None and ln.startswith("## 0."):
            start = i
        elif start is not None and ln.startswith("## 1."):
            end = i
            break
    if start is None:
        sys.exit("FATAL: could not locate '## 0.' section in PARSER_REPORT.md")
    if end is None:
        end = len(lines)

    section = lines[start:end]
    features = []  # list of dicts: id, name, family, markers[]
    family = "(unknown)"

    for ln in section:
        s = ln.rstrip("\n")
        if s.startswith("### "):
            family = s[4:].strip()
            continue
        if not s.startswith("|"):
            continue
        cells = [c.strip() for c in s.strip().strip("|").split("|")]
        # Separator / header rows.
        if all(re.fullmatch(r":?-{2,}:?", c.replace(" ", "")) for c in cells):
            continue
        if not cells:
            continue
        feat_id = cells[0].strip()
        if not re.match(r"^[A-Z][0-9]+$", feat_id):
            continue  # header row like "ID"
        name = cells[1].strip() if len(cells) > 1 else ""
        row_text = " ".join(cells)
        markers = MARKER_RE.findall(row_text)
        features.append(
            {
                "id": feat_id,
                "name": name,
                "family": family,
                "markers": markers,  # raw inner texts
            }
        )
    return features


# --------------------------------------------------------------------------- #
# Membership check with bidirectional substring matching (after normalization).
# Records match direction so we can label CHUKI-ONLY / MANUAL-ONLY.
# --------------------------------------------------------------------------- #
def matches(marker_norm, source_set):
    if not marker_norm:
        return False
    if marker_norm in source_set:
        return True
    for entry in source_set:
        if marker_norm in entry or entry in marker_norm:
            return True
    return False


def main():
    chuki_set = load_chuki_set()
    manual_set = load_manual_set()
    features = parse_section0()

    print(f"# PARSER_REPORT §0 verification\n")
    print(f"Canonical sources loaded:")
    print(f"  chuki_tag.txt:        {len(chuki_set)} distinct normalized markers")
    print(f"  annotation/*.html:    {len(manual_set)} distinct normalized markers")
    print(f"  PARSER_REPORT §0:     {len(features)} feature rows parsed\n")

    results = []
    counts = {
        "VERIFIED": 0,
        "CHUKI-ONLY": 0,
        "MANUAL-ONLY": 0,
        "UNVERIFIED": 0,
        "NO-MARKER": 0,
    }
    for feat in features:
        if not feat["markers"]:
            verdict = "NO-MARKER"
        else:
            in_chuki = any(matches(normalize(m), chuki_set) for m in feat["markers"])
            in_manual = any(matches(normalize(m), manual_set) for m in feat["markers"])
            if in_chuki and in_manual:
                verdict = "VERIFIED"
            elif in_chuki:
                verdict = "CHUKI-ONLY"
            elif in_manual:
                verdict = "MANUAL-ONLY"
            else:
                verdict = "UNVERIFIED"
        counts[verdict] += 1
        results.append((feat, verdict))

    print("## Per-feature verdicts\n")
    print("| ID | Family | Feature | Verdict | Markers (normalized) |")
    print("|----|--------|---------|---------|------------------------|")
    for feat, verdict in results:
        norms = [normalize(m) for m in feat["markers"]]
        shown = ", ".join(norms) if norms else "—"
        print(f"| {feat['id']} | {feat['family']} | {feat['name']} | {verdict} | {shown} |")

    print("\n## Summary counts (disjoint 5-way partition)\n")
    total = len(results)
    for k in ["VERIFIED", "CHUKI-ONLY", "MANUAL-ONLY", "UNVERIFIED", "NO-MARKER"]:
        print(f"  {k:14s}: {counts[k]:3d}")
    print(f"  {'TOTAL':14s}: {total:3d}")

    marker_bearing = total - counts["NO-MARKER"]
    corroborated = counts["VERIFIED"] + counts["CHUKI-ONLY"] + counts["MANUAL-ONLY"]
    pct = (100.0 * corroborated / marker_bearing) if marker_bearing else 0.0
    print(
        f"\nMarkers verified against >=1 canonical source: "
        f"{corroborated}/{marker_bearing} ({pct:.1f}%) of feature rows that carry a marker."
    )

    print("\n## UNVERIFIED features (claim not corroborated by either source)\n")
    unver = [(f, v) for (f, v) in results if v == "UNVERIFIED"]
    if not unver:
        print("  (none)")
    for f, _ in unver:
        norms = [normalize(m) for m in f["markers"]]
        print(f"  {f['id']}  {f['name']}  -> {norms}")

    # ---- Coverage gaps: canonical markers absent from PARSER_REPORT §0 ----
    report_cores = set()
    for feat, _ in results:
        for m in feat["markers"]:
            n = normalize(m)
            if n:
                report_cores.add(n)

    def covered_by_report(marker_norm):
        if marker_norm in report_cores:
            return True
        for rc in report_cores:
            if marker_norm in rc or rc in marker_norm:
                return True
        return False

    print("\n## Coverage gaps: canonical markers NOT represented in PARSER_REPORT §0\n")
    chuki_gaps = sorted(m for m in chuki_set if not covered_by_report(m))
    manual_gaps = sorted(m for m in manual_set if not covered_by_report(m))
    print(f"  chuki_tag-only gaps:  {len(chuki_gaps)} markers")
    print(f"  manual-only gaps:     {len(manual_gaps)} markers")

    GAP_CAP = 60
    print(f"\n  --- chuki_tag markers absent from §0 (showing up to {GAP_CAP}) ---")
    for m in chuki_gaps[:GAP_CAP]:
        print(f"    {m}")
    if len(chuki_gaps) > GAP_CAP:
        print(f"    ... ({len(chuki_gaps) - GAP_CAP} more omitted)")

    print(f"\n  --- annotation/*.html markers absent from §0 (showing up to {GAP_CAP}) ---")
    for m in manual_gaps[:GAP_CAP]:
        print(f"    {m}")
    if len(manual_gaps) > GAP_CAP:
        print(f"    ... ({len(manual_gaps) - GAP_CAP} more omitted)")

    # Exit clean.
    return 0


if __name__ == "__main__":
    sys.exit(main())
