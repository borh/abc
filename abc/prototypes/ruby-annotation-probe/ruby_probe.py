#!/usr/bin/env python3
"""THROWAWAY PROBE — do not productionize.

Question: does dropping ruby annotations from the tokenization input view
(parser-ir-plaintext-body-v1) lose segmentation/reading information, and how
much? Measured over a deterministic sample of Aozora ruby-bearing works:

  (a) token/ruby-span boundary alignment (does a UniDic token straddle a ruby
      span edge? does a ruby span cover multiple whole tokens?)
  (b) reading agreement: author ruby vs UniDic kana reading, on spans whose
      edges align with token boundaries.

Approximates the plaintext view by stripping Aozora markup directly (the real
renderer keeps ruby base text, resolved gaiji, drops annotations) — good
enough for a probe, not identity-faithful.
"""

import csv
import json
import re
import subprocess
import sys
import zipfile
from pathlib import Path

CORPUS = Path("/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus/cards")
SAMPLE_N = 80
KANA_FIELD = 20  # unidic 'kana' column (katakana reading of surface)
DICTS = {
    "unidic-novel": "/nix/store/ihib719s18m8hrl45sagii5awnc0lv2d-unidic-novel-2512/share/mecab/dic/unidic-novel",
    "unidic-cwj": "/nix/store/i1dsm4d2mnicvvkasg6pbmdphnn4kk32-unidic-cwj-2512/share/mecab/dic/unidic-cwj",
}

KANJI_EXTRA = set("々〆〻ヶ仝〇")


def char_class(c):
    o = ord(c)
    if 0x4E00 <= o <= 0x9FFF or 0x3400 <= o <= 0x4DBF or 0xF900 <= o <= 0xFAFF or c in KANJI_EXTRA:
        return "kanji"
    if 0x30A1 <= o <= 0x30FA or c == "ー":
        return "katakana"
    if c.isascii() and c.isalpha() or 0xFF21 <= o <= 0xFF5A:
        return "latin"
    return None  # hiragana etc.: implicit ruby base not allowed without ｜


def hira_to_kata(s):
    out = []
    for c in s:
        o = ord(c)
        if 0x3041 <= o <= 0x3096 or o in (0x309D, 0x309E):
            out.append(chr(o + 0x60))
        else:
            out.append(c)
    return "".join(out)


LOOSE = str.maketrans({"ヂ": "ジ", "ヅ": "ズ", "ヰ": "イ", "ヱ": "エ", "ヲ": "オ"})

SMALL_DEMOTE = str.maketrans({"ャ": "ヤ", "ュ": "ユ", "ョ": "ヨ", "ッ": "ツ"})
MEDIAL_H = {"ハ": "ワ", "ヒ": "イ", "フ": "ウ", "ヘ": "エ", "ホ": "オ"}
A_TO_O = str.maketrans("アカサタナハマヤラワガザダバパ", "オコソトノホモヨロヲゴゾドボポ")
E_TO_IYO = {
    "エ": "ヨ",
    "ケ": "キヨ",
    "セ": "シヨ",
    "テ": "チヨ",
    "ネ": "ニヨ",
    "ヘ": "ヒヨ",
    "メ": "ミヨ",
    "レ": "リヨ",
    "ゲ": "ギヨ",
    "ゼ": "ジヨ",
    "デ": "ジヨ",
    "ベ": "ビヨ",
    "ペ": "ピヨ",
}


def hist_kana_normalize(s):
    """Crude historical→modern kana comparison key (katakana input).
    Handles kunojiten repeats, small-kana promotion differences, medial h-row,
    -au→-ou, -eu→-yoU, kwa/gwa. Approximate by design (probe only)."""
    while "／＼" in s or "／″＼" in s:
        mark = "／″＼" if "／″＼" in s else "／＼"
        i = s.index(mark)
        s = s[:i] + s[:i] + s[i + len(mark) :]
    s = s.translate(LOOSE).translate(SMALL_DEMOTE)
    s = s.replace("クヮ", "カ").replace("グヮ", "ガ")
    out = []
    for i, c in enumerate(s):
        if i > 0 and c in MEDIAL_H:
            out.append(MEDIAL_H[c])
        else:
            out.append(c)
    s = "".join(out)
    s = s.replace("ヤウ", "ヨウ")
    out = []
    i = 0
    while i < len(s):
        c = s[i]
        nxt = s[i + 1] if i + 1 < len(s) else ""
        if nxt == "ウ" and c in E_TO_IYO:
            out.append(E_TO_IYO[c])
        elif nxt == "ウ" and c != c.translate(A_TO_O):
            out.append(c.translate(A_TO_O))
        else:
            out.append(c)
        i += 1
    return "".join(out)


def extract_body(raw):
    text = raw.replace("\r\n", "\n").replace("\r", "\n")
    lines = text.split("\n")
    # header: metadata block delimited by ---- lines near the top
    delim = [i for i, line in enumerate(lines[:80]) if re.fullmatch(r"-{10,}", line.strip())]
    start = delim[-1] + 1 if len(delim) >= 2 else 0
    # footer: from 底本： (or blank-line-then-底本)
    end = len(lines)
    for i in range(start, len(lines)):
        if lines[i].lstrip().startswith(("底本：", "底本:")):
            end = i
            break
    return "\n".join(lines[start:end])


def parse_aozora(body):
    """Return (plaintext, ruby_spans, counters). Spans are [start,end) in
    plaintext unicode-scalar offsets, matching token-index-v1 span semantics."""
    out = []  # chars
    rubies = []
    pending_bar = None  # output offset where ｜ base started
    counters = {"gaiji_markers": 0, "orphan_ruby": 0, "annotations": 0}
    i, n = 0, len(body)
    while i < n:
        c = body[i]
        if c == "［" and body[i : i + 2] == "［＃":
            j = body.find("］", i)
            if j == -1:
                out.append(c)
                i += 1
                continue
            counters["annotations"] += 1
            i = j + 1
            continue
        if c == "※":
            counters["gaiji_markers"] += 1
            out.append(c)
            i += 1
            continue
        if c == "｜":
            pending_bar = len(out)
            i += 1
            continue
        if c == "《":
            j = body.find("》", i)
            if j == -1:
                out.append(c)
                i += 1
                continue
            reading = body[i + 1 : j]
            if pending_bar is not None:
                start = pending_bar
                pending_bar = None
            else:
                # implicit base: maximal same-class run at end of out
                k = len(out)
                cls = char_class(out[k - 1]) if k else None
                if cls is None:
                    counters["orphan_ruby"] += 1
                    i = j + 1
                    continue
                while k > 0 and char_class(out[k - 1]) == cls:
                    k -= 1
                start = k
            end = len(out)
            if end > start:
                rubies.append(
                    {
                        "start": start,
                        "end": end,
                        "base": "".join(out[start:end]),
                        "reading": reading,
                    }
                )
            else:
                counters["orphan_ruby"] += 1
            i = j + 1
            continue
        out.append(c)
        i += 1
    return "".join(out), rubies, counters


def tokenize(text, dicdir):
    """Return list of (start, end, surface, kana_or_None) in text offsets."""
    proc = subprocess.run(
        ["mecab", "-d", dicdir, "-b", "1048576"],
        input=text,
        capture_output=True,
        text=True,
        check=True,
    )
    tokens = []
    pos = 0
    misaligned = 0
    for line in proc.stdout.split("\n"):
        if line == "EOS" or line == "":
            continue
        surface, _, feat = line.partition("\t")
        if not surface:
            continue
        if text[pos : pos + len(surface)] != surface:
            idx = text.find(surface, pos, pos + 200)
            if idx == -1 or not all(c in " 　\t\n" for c in text[pos:idx]):
                misaligned += 1
                continue
            pos = idx
        fields = next(csv.reader([feat]))
        kana = (
            fields[KANA_FIELD] if len(fields) > KANA_FIELD and fields[KANA_FIELD] != "*" else None
        )
        pron = fields[9] if len(fields) > 9 and fields[9] != "*" else None
        tokens.append((pos, pos + len(surface), surface, kana, pron))
        pos += len(surface)
    return tokens, misaligned


def analyze_work(path, dicdir):
    with zipfile.ZipFile(path) as z:
        member = next(m for m in z.namelist() if m.lower().endswith(".txt"))
        raw = z.read(member).decode("cp932", errors="replace")
    body = extract_body(raw)
    plain, rubies, counters = parse_aozora(body)
    if not rubies:
        return None
    tokens, misaligned = tokenize(plain, dicdir)
    # Token lookup uses the monotonic cursor below.
    stats = {
        "rubies": 0,
        "aligned_single": 0,
        "aligned_multi": 0,
        "straddle": 0,
        "straddle_end_only": 0,
        "straddle_start": 0,
        "read_cmp": 0,
        "read_eq": 0,
        "read_eq_loose": 0,
        "prefix_cmp": 0,
        "prefix_eq": 0,
        "no_kana": 0,
        "misaligned_tokens": misaligned,
    }
    disagreements = []
    tok_i = 0
    for r in rubies:
        s, e = r["start"], r["end"]
        # advance token cursor
        while tok_i < len(tokens) and tokens[tok_i][1] <= s:
            tok_i += 1
        cover = []
        j = tok_i
        while j < len(tokens) and tokens[j][0] < e:
            cover.append(tokens[j])
            j += 1
        if not cover:
            continue
        stats["rubies"] += 1
        start_ok = cover[0][0] == s
        end_ok = cover[-1][1] == e
        ruby_kata = hira_to_kata(r["reading"])
        if start_ok and end_ok:
            key = "aligned_single" if len(cover) == 1 else "aligned_multi"
            stats[key] += 1
            kanas = [t[3] for t in cover]
            prons = [t[4] for t in cover]
            if any(k is None for k in kanas):
                stats["no_kana"] += 1
            else:
                got = "".join(kanas)
                got_pron = "".join(p for p in prons if p) if all(prons) else None
                stats["read_cmp"] += 1
                rl = ruby_kata.translate(LOOSE)
                rh = hist_kana_normalize(ruby_kata)
                if got == ruby_kata:
                    stats["read_eq"] += 1
                elif got.translate(LOOSE) == rl or (got_pron and got_pron.translate(LOOSE) == rl):
                    stats["read_eq_loose"] += 1
                elif hist_kana_normalize(got) == rh or (
                    got_pron and hist_kana_normalize(got_pron) == rh
                ):
                    stats["read_eq_histkana"] = stats.get("read_eq_histkana", 0) + 1
                else:
                    if len(disagreements) < 8:
                        disagreements.append({"base": r["base"], "ruby": ruby_kata, "unidic": got})
        else:
            stats["straddle"] += 1
            if start_ok and not end_ok:
                stats["straddle_end_only"] += 1
                # stem-ruby check: ruby reading a prefix of the straddling token's kana?
                last = cover[-1]
                if len(cover) == 1 and last[3]:
                    stats["prefix_cmp"] += 1
                    inner = [t[3] for t in cover[:-1]]
                    if all(inner):
                        got = "".join(inner) + last[3]
                        if got.startswith(ruby_kata) or got.translate(LOOSE).startswith(
                            ruby_kata.translate(LOOSE)
                        ):
                            stats["prefix_eq"] += 1
            if not start_ok:
                stats["straddle_start"] += 1
    return {
        "path": str(path),
        "chars": len(plain),
        "tokens": len(tokens),
        "counters": counters,
        "stats": stats,
        "disagreements": disagreements,
    }


def main():
    zips = sorted(CORPUS.glob("*/files/*_ruby_*.zip"))
    stride = max(1, len(zips) // SAMPLE_N)
    sample = zips[::stride][:SAMPLE_N]
    out = {"corpus_ruby_zips": len(zips), "sample": len(sample), "dicts": {}}
    all_works = {}
    for dic_name, dicdir in DICTS.items():
        agg = {}
        works = []
        for p in sample:
            try:
                r = analyze_work(p, dicdir)
            except Exception as ex:
                print(f"SKIP {p.name}: {ex}", file=sys.stderr)
                continue
            if r is None:
                continue
            works.append(r)
            for k, v in r["stats"].items():
                agg[k] = agg.get(k, 0) + v
        out["dicts"][dic_name] = {"works": len(works), "aggregate": agg}
        all_works[dic_name] = works
    print(json.dumps(out, ensure_ascii=False, indent=2))
    ex = [d for w in all_works["unidic-novel"] for d in w["disagreements"]][:40]
    print(json.dumps(ex, ensure_ascii=False))
    Path(sys.argv[1] if len(sys.argv) > 1 else "ruby_probe_works.json").write_text(
        json.dumps(all_works, ensure_ascii=False, indent=1)
    )


if __name__ == "__main__":
    main()
