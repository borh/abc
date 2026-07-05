#!/usr/bin/env python3
"""Bootstrap-label candidate katakana sentences from one Aozora work.

Faithful Branch-B port of aozora-corpus-generator's is_katakana_sentence
(aozora.py:311). Branch B = OOV guard dropped (Vibrato's LexType::Unknown
never fires on katakana prose because Unidic-CWJ has dictionary entries
for katakana particles/copulas — see reports/ortho-detect/2026-07-05-sudachi-baseline.md).
So oov_count=0, proper_noun_chars=0; those branches never trigger; only
the character cascade decides. This re-implements the cascade faithfully
WITHOUT requiring a MeCab install or the sibling repo's Token type.

This is a SEED, not ground truth: a model trained on these labels can
at best reproduce the heuristic. Real human annotation is Task 9.

Emits JSONL to stdout, one record per candidate sentence.
"""
from __future__ import annotations
import argparse
import json
import re
from collections import defaultdict
from pathlib import Path

TERMINALS = set("。！？!?")
DICT_VERSION = "unidic-cwj-202512"

def code_frequencies(text: str) -> tuple[dict, set]:
    """Port of aozora.py code_frequencies: returns (counts, unigram_types)."""
    cmap = {"katakana": 0, "hiragana": 0, "kanji": 0, "other": 0}
    unigram_types: set[str] = set()
    for ch in text:
        unigram_types.add(ch)
        if "\u30A1" <= ch <= "\u30FA" or "\uFF65" <= ch <= "\uFF9F":
            cmap["katakana"] += 1
        elif "\u3041" <= ch <= "\u3096":
            cmap["hiragana"] += 1
        elif "\u4E00" <= ch <= "\u9FFF" or "\u3400" <= ch <= "\u4DBF":
            cmap["kanji"] += 1
        else:
            cmap["other"] += 1
    return cmap, unigram_types

def is_katakana_sentence_branch_b(text: str) -> bool:
    """Faithful Branch-B port (oov_count=0, proper_noun_chars=0)."""
    cmap, unigram_types = code_frequencies(text)
    bigram_types: dict[str, int] = defaultdict(int)
    for bigram in map(lambda a, b: "{}{}".format(a, b), text, text[1:]):
        bigram_types[bigram] += 1
    total_chars = cmap["katakana"] + cmap["hiragana"] + cmap["kanji"] + cmap["other"]
    if total_chars == 0:
        return False
    katakana_ratio = cmap["katakana"] / total_chars
    if not (cmap["hiragana"] == 0 and katakana_ratio > 0.5):
        return False
    # Branch B: oov_count = 0, so the proper-noun branch (oov_count==0 AND ratio>0.3) never fires.
    # oov_ratio = 0/len(tokens) — but we stub tokens, so skip (0 > 0.2 is False).
    # Character cascade:
    if (len(text) < 8
        or (len(text) < 10 and re.search(r"ッ?.?[？！]」$", text[-3:]))
        or (len(text) < 100 and len(unigram_types) / len(text) < 0.5)
        or (max(bigram_types.values()) / len(text) > 0.5 if bigram_types else False)
        or len(re.findall(r"(.)\1+", text)) / len(text) > 0.1
        or len(re.findall(r"(..)ッ?\1", text)) / len(text) > 0.1):
        return False
    return True

def sentence_split(text: str):
    """Port of ab-plaintext::sentence_split. Returns [(sentence, byte_offset, char_offset)]."""
    spans = []
    char_start = 0
    i = 0
    n = len(text)
    while i < n:
        if text[i] in TERMINALS:
            j = i + 1
            while j < n and text[j] in TERMINALS:
                j += 1
            slice_text = text[char_start:j]
            byte_offset = len(text[:char_start].encode("utf-8"))
            if slice_text.strip():
                spans.append((slice_text, byte_offset, char_start))
            char_start = j
            i = j
        else:
            i += 1
    if char_start < n:
        slice_text = text[char_start:]
        byte_offset = len(text[:char_start].encode("utf-8"))
        if slice_text.strip():
            spans.append((slice_text, byte_offset, char_start))
    return spans

def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--work-id", required=True)
    ap.add_argument("--text-file", required=True)
    args = ap.parse_args()
    text = Path(args.text_file).read_text(encoding="utf-8")
    for sentence, byte_off, char_off in sentence_split(text):
        total = len(sentence)
        if total == 0:
            continue
        kata = sum(1 for c in sentence if "\u30A1" <= c <= "\u30FA" or "\uFF65" <= c <= "\uFF9F")
        hira = sum(1 for c in sentence if "\u3041" <= c <= "\u3096")
        ratio = kata / total
        if not (ratio > 0.4 and hira == 0):
            continue
        label = "accept" if is_katakana_sentence_branch_b(sentence) else "reject"
        rec = {
            "work_id": args.work_id,
            "sentence": sentence,
            "byte_offset": byte_off,
            "char_offset": char_off,
            "label": label,
            "katakana_ratio": ratio,
            "hiragana_count": hira,
            "total_chars": total,
            "dict_version": DICT_VERSION,
        }
        print(json.dumps(rec, ensure_ascii=False))

if __name__ == "__main__":
    main()
