#!/usr/bin/env python
"""Bootstrap-label candidate katakana sentences from one Aozora work.

Character-pattern labels omit token-level OOV and proper-noun guards.
They need no tokenizer or dictionary. A model trained on these labels measures
agreement with this heuristic; independent human annotation is required to
measure detection quality.

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
        if "\u30a1" <= ch <= "\u30fa" or "\uff65" <= ch <= "\uff9f":
            cmap["katakana"] += 1
        elif "\u3041" <= ch <= "\u3096":
            cmap["hiragana"] += 1
        elif "\u4e00" <= ch <= "\u9fff" or "\u3400" <= ch <= "\u4dbf":
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
    # oov_ratio = 0/len(tokens); tokens are stubbed here, so skip (0 > 0.2 is False).
    # Character cascade:
    if (
        len(text) < 8
        or (len(text) < 10 and re.search(r"ッ?.?[？！]」$", text[-3:]))
        or (len(text) < 100 and len(unigram_types) / len(text) < 0.5)
        or (max(bigram_types.values()) / len(text) > 0.5 if bigram_types else False)
        or len(re.findall(r"(.)\1+", text)) / len(text) > 0.1
        or len(re.findall(r"(..)ッ?\1", text)) / len(text) > 0.1
    ):
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
        kata = sum(1 for c in sentence if "\u30a1" <= c <= "\u30fa" or "\uff65" <= c <= "\uff9f")
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
