#!/usr/bin/env python
"""Author a SECOND, independent conformance instrument: a small seed of vectors
derived from the OFFICIAL 青空文庫 annotation documentation (aozora.gr.jp/annotation,
CC-BY 4.0) rather than the third-party P4suta suite. Purpose: cross-check whether
the P4suta-based parser findings replicate on independently-sourced cases, reducing
reliance on a single instrument.

Scope: a SEED (not a complete instrument). Cases + construct identification are
derived from the official docs; the expected `kind` labels reuse the shared
measurement vocabulary so the existing harness can score them. Node spans are
NOMINAL (whole source) — the cross-parser comparison uses the kind sequence (AAT
adapters emit no spans), which is what these vectors exercise.

Emits vector.json files compatible with run-aozora-notation-spec.py.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

# (name, feature, level, source, expected_kinds, spec_section_url, note)
# Sources use official syntax; several are verbatim official examples.
SEED = [
    ("official_ruby_explicit", "ruby", "must",
     "｜青空《あおぞら》を歩く。\n", ["ruby"],
     "https://www.aozora.gr.jp/annotation/",
     "[provenance:official] explicit ruby base marker ｜ + 《reading》"),
    ("official_ruby_implicit", "ruby", "must",
     "青空《あおぞら》\n", ["ruby"],
     "https://www.aozora.gr.jp/annotation/",
     "[provenance:official] implicit ruby 《reading》 after kanji run"),
    ("official_bouten", "bouten", "must",
     "腹がへっても［＃「腹がへっても」に傍点］\n", ["bouten"],
     "https://www.aozora.gr.jp/annotation/emphasis.html",
     "[provenance:official] verbatim example; 傍点 -> <em class=sesame_dot>"),
    ("official_bousen", "bousen", "should",
     "この形は傍線［＃「傍線」に傍線］と書いてください。\n", ["emphasis"],
     "https://www.aozora.gr.jp/annotation/emphasis.html",
     "[provenance:official] verbatim example; 傍線 -> <em class=underline_solid>; spec has no bousen kind -> emphasis"),
    ("official_bold", "emphasis", "should",
     "重要［＃「重要」は太字］。\n", ["emphasis"],
     "https://www.aozora.gr.jp/annotation/emphasis.html",
     "[provenance:official] 太字 forward-reference form ［＃「○○」は太字］"),
    ("official_italic", "emphasis", "should",
     "strong［＃「strong」は斜体］\n", ["emphasis"],
     "https://www.aozora.gr.jp/annotation/emphasis.html",
     "[provenance:official] 斜体 forward-reference form ［＃「○○」は斜体］"),
    ("official_gaiji_jisx0213", "gaiji", "must",
     "※［＃「てへん＋劣」、第3水準1-84-77］\n", ["gaiji"],
     "https://www.aozora.gr.jp/annotation/external_character.html",
     "[provenance:official] verbatim example; JIS X 0213 plane3 row84 cell77"),
    ("official_heading_large", "heading", "should",
     "上　先生と私［＃「上　先生と私」は大見出し］\n", ["heading"],
     "https://www.aozora.gr.jp/annotation/heading.html",
     "[provenance:official] verbatim example; 大見出し (large heading)"),
    ("official_pagebreak_kaicho", "break", "should",
     "［＃改丁］\n", ["pageBreak"],
     "https://www.aozora.gr.jp/annotation/layout_1.html",
     "[provenance:official] 改丁 -> next left page (major section break)"),
    ("official_pagebreak_kaipage", "break", "should",
     "［＃改ページ］\n", ["pageBreak"],
     "https://www.aozora.gr.jp/annotation/layout_1.html",
     "[provenance:official] 改ページ -> next page (medium break)"),
    ("official_jisage_ageage", "layout", "should",
     "［＃地から１字上げ］（明治七年三月出版）\n", ["indent"],
     "https://www.aozora.gr.jp/annotation/layout_1.html",
     "[provenance:official] verbatim example; 地から１字上げ (raise 1 char from baseline)"),
]


def main():
    out_dir = Path(sys.argv[1])
    out_dir.mkdir(parents=True, exist_ok=True)
    for name, feature, level, source, kinds, url, note in SEED:
        byte_len = len(source.rstrip("\n").encode("utf-8"))
        # NOMINAL spans (whole construct); AAT comparison is kind-sequence.
        nodes = [{"kind": k, "span": {"start": 0, "end": byte_len}} for k in kinds]
        vector = {
            "name": name,
            "meta": {
                "feature": feature,
                "level": level,
                "spec_section": url,
                "note": note,
            },
            "source": source,
            "expected": {"nodes": nodes},
        }
        vdir = out_dir / name
        vdir.mkdir(parents=True, exist_ok=True)
        (vdir / "vector.json").write_text(
            json.dumps(vector, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
        )
    print(f"wrote {len(SEED)} official-docs seed vectors to {out_dir}")


if __name__ == "__main__":
    main()
