# kunten-source-excerpt.notes.md

Companion notes for `kunten-source-excerpt.txt`. This pair of fixture files
is a **regression fixture for a known schema gap**: it proves that real
Aozora corpus works emit kunten (訓点) notation that AAT v1 has no node kind
to represent.

## Source work

| field                  | value                                                          |
| ---------------------- | -------------------------------------------------------------- |
| work_id (Aozora book)  | `4644`                                                         |
| card path              | `cards/000250/card1338.html`                                   |
| txt archive            | `cards/000250/files/4644_ruby_15596.zip`                        |
| txt member             | `hoo_kansho07.txt` (SHIFT_JIS)                                  |
| title                  | 放翁鑑賞 その七 ――放翁詩話三十章―― ( commentary on Lu You )    |
| author                 | 河上肇                                                         |
| excerpt regions        | source lines 17–19 (notation header) and 100–101 (a couplet)   |

The work is a kanbun-kundoku (漢文訓読) commentary — exactly the genre that
requires kunten. It was selected by scanning all 1,227 card directories in
the local Aozora snapshot (cards/*/files/*_ruby_*.zip and *_txt_*.zip) for
the Aozora 返り点 / 送り仮名 marker forms documented at
`/home/bor/Dependencies/aozorabunko/annotation/kunten.html`. Work `4644`
is the richest hit: it emits *every* marker sub-variety (see below).

## Kunten markers present in the excerpt

The excerpt's couplet (excerpt B) is dense — two source lines carry all of
the following markers:

### 返り点 (reading-order points) — ordered-numeric / ordinal forms

| marker       | meaning (per Aozora manual, `annotation/kunten.html`) |
| ------------ | ------------------------------------------------------ |
| `［＃レ］`   | レ点 — read the preceding char, then go back           |
| `［＃一］`   | 一点 — ordinal 返り点 (read 1st)                        |
| `［＃二］`   | 二点 — ordinal 返り点 (read 2nd)                        |
| `［＃上］`   | 上点 — ordinal 返り点 (upper)                           |
| `［＃中］`   | 中点 — ordinal 返り点 (middle)                          |
| `［＃下］`   | 下点 — ordinal 返り点 (lower)                           |

Excerpt A (the notation header) additionally documents the `［＃…］：返り点` form itself with the example `四月熟［＃二］黄梅［＃一］`.

### 送り仮名 / 再読文字 (okurigana / re-read markers) — parenthesised kana forms

| marker          | meaning                                                    |
| --------------- | ---------------------------------------------------------- |
| `［＃（ノ）］`  | 送り仮名 「の」 particle inflection                        |
| `［＃（ス）］`  | 送り仮名 「す」 inflection                                  |
| `［＃（レント）］` | 送り仮名 / conjugation ending 「れんと」 (incl. re-read) |

Across the full source file `hoo_kansho07.txt` the work additionally emits
`［＃（ツ）］`, `［＃（フ）］`, `［＃（ヒ）］`, `［＃（レ）］`, `［＃三］`,
`［＃天／地／人］`-style markers — the complete set of 24 documented at the
manual page. The excerpt captures a representative, dense subset; the full
file can be re-extracted from the card path above if a wider sample is
needed.

## Policy note (read this before treating this as a test)

> **AAT v1 has no kunten node kind.** See
> `docs/handoffs/aozora-manual-integration-audit.md` → **Gap B** (kunten
> construct unrepresented in the 20 AAT v1 node kinds), and
> `docs/handoffs/owned-mapping-design.md` → §5, **deferred decision 1b**
> (a dedicated kunten node is deferred to an AAT v2 ADR; the v1 schema is
> frozen). Renaming or extending `data/aat-schema.json` to add a kunten
> kind is explicitly **out of scope** for the v1 window.
>
> This fixture exists to **track the gap**, not to close it. If/when an
> adapter (e.g. the `aozora-rs` adapter) is run over this excerpt, the
> output **MUST record the kunten construct as lost** — i.e. emit a
> divergence / dropped-construct entry naming the marker and its source
> span — and **MUST NOT silently drop** `［＃レ］`, `［＃二］`,
> `［＃（ノ）］`, etc.
>
> Until AAT v2 adds a kunten node kind, this fixture must not become an
> adapter-fidelity assertion expecting a kunten AAT node. It is now executable
> only as a detector regression: the corpus feature index and coverage
> detectors must recognize the real compact spellings so prevalence
> measurement cannot silently false-zero again. When v2 lands the kunten node,
> upgrade this fixture to an executable adapter-fidelity assertion (input:
> this excerpt; expected: a kunten node projecting each marker above with the
> correct `kind`, surface form, and source span).

## How to reproduce the excerpt

```sh
unzip -p cards/000250/files/4644_ruby_15596.zip "hoo_kansho07.txt" \
  | iconv -f SHIFT_JIS -t UTF-8 \
  | sed -n '17,19p;99,102p'
```

(Note the historical gotcha that bit an earlier extraction attempt: pass
encoding as `-t UTF-8` — a *separate* `//IGNORE` argument is treated by
`iconv` as an input *filename* and silently yields an empty conversion,
making it look as if no kunten markers exist in the corpus. They do.)
