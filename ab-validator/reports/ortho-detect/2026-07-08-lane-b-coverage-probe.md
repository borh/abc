# Lane B coverage probe — historical→modern kana on real old-kana aozora text

**Date:** 2026-07-08
**Purpose:** Settle the Lane B (M2) design's open sub-questions with data before
locking the rule set — the "probe coverage first" decision from
`2026-07-08-ortho-phase3-lane-b-surface-normalizer-design.md`.
**Method:** Tokenized 6 classic old-kana aozora works under
`vibrato:unidic-kindai-bungo-202512` and, per all-kana token, compared the
written surface to its modern reading (`pron`, katakana) to flag genuinely
historical spellings, classifying each by transformation type. `pron` is the
oracle for the target sound. Disposable probe (not committed).
**Corpus sample:** Nagai Kafū 江戸芸術論, Tanizaki 盲目物語, Mori Ōgai ファウスト,
Watsuji 日本精神, Saitō 万葉集, Okamoto 奇動昔語 — from the aozora full plain-text
export (`aozora-corpus-generator/AozoraBunko-Text-2021-07-28/Plain/`). Old-kana
works exist at scale: the catalog (`list_person_all_extended_utf8.csv`) has
**3,955 新字旧仮名 + 1,917 旧字旧仮名 = ~5,872 old-kana works**.

## Headline numbers

| metric | count | of all-kana |
|---|---|---|
| total tokens | 697,426 | |
| all-kana tokens | 348,814 | 100% |
| **genuinely historical-kana tokens** | **11,059** | **3.2%** |
| standalone は/へ/を 助詞 (must NOT change) | 39,618 | — |

Genuinely historical spellings are ~3.2% of all-kana tokens (~1.6% of all
tokens) — a bounded but real workload. The dominant confound is **particles**:
the naive "surface ≠ reading" test first flagged 50,677 tokens, but **~40,000
were は/を/へ read わ/お/え by normal MODERN particle rules** — not historical, and
must be protected. Excluding particles by POS (`助詞`) drops the count to 11,059.
**This is the single most important finding: particle protection via POS is
mandatory, and it is exactly what the `kindai-bungo` oracle (M2) provides.**

## Transformation classes (particles excluded)

| class | count | share | rule | needs `pron`? |
|---|---|---|---|---|
| 3 — digraph / long vowel | 9,044 | 81.8% | au/iu/eu→ō/yū/yō (やう→よう); katakana loan イ/ウ→ー | **yes** (target sound) |
| 2 — medial h-row | 1,147 | 10.4% | は/ひ/ふ/へ/ほ → わ/い/う/え/お, word-medial (いふ→いう, なほ→なお) | no |
| (4) — yotsugana | part of 773 | — | づ/ぢ → ず/じ (いづれ→いずれ, まづ→まず) | no |
| (4) — gemination | part of 773 | — | つ → っ before て/た (よつて→よって, あつた→あった) | no |
| 1 — obsolete kana | 95 | 0.9% | ゐ/ゑ → い/え (ゆゑ→ゆえ, くれなゐ→くれない) | no |
| 4 — other | 773 | 7.0% | (yotsugana + gemination + residual, above) | mixed |

Real samples: `ゐる→イル`, `ゆゑ→ユエ`, `くれなゐ→クレナイ`; `なほ→ナオ`, `いへ→イエ`,
`いふ→イウ`, `かへつて→カエッテ`; `やう→ヨー`, `いづれ→イズレ`, `まづ→マズ`, `よつ→ヨッ`,
`あつ→アッ`; loanwords `アカデミイ→アカデミー`, `ゴンクウル→ゴンクール`.

## What this confirms for M2

1. **Particle protection via POS is non-negotiable** — without it, ~80% of the
   "historical" signal is false positives (particles). `kindai-bungo`'s 助詞
   tagging is the guard. This is the decisive reason M2 (segmentation+POS oracle)
   beats M1 (rules on unreliable contemporary segmentation).
2. **The digraph/long-vowel class dominates (82%) and needs the reading.** やう has
   `pron=ヨー`; a rules-only converter cannot know やう→よう without the phonetic
   target. `pron` supplies it — M2's second oracle. The residual rule is
   spelling the long vowel (`ヨー`→`よう`, not `よー`), an お-row `+ー→+う` mapping.
3. **The regular classes are simple, deterministic swaps** (~18% total): obsolete
   ゐゑ, yotsugana づぢ, medial h-row, つ→っ gemination. Context-free or
   position-based; a small checked-in rule table covers them.

## Recommended Lane B rule set (data-grounded)

Apply per token from the `kindai-bungo` stream, kanji kept, only kana rewritten:

1. **Guard:** if `pos1 == 助詞` and surface ∈ {は, へ, を}, leave unchanged.
2. **Context-free swaps:** ゐ→い, ゑ→え (+ katakana ヰヱ); づ→ず, ぢ→じ.
3. **Gemination:** つ→っ when followed by て/た (or the token is a 促音便 renyō).
4. **Medial h-row:** non-initial は/ひ/ふ/へ/ほ → わ/い/う/え/お.
5. **Digraph long vowel (pron-guided):** where `pron` shows a long vowel (ー) that
   the historical spelling wrote as a vowel-kana sequence (やう, かう, えう, ふ-tail),
   rewrite to the modern long-vowel spelling using the `pron` target
   (ヨー→よう, コー→こう, キョー→きょう).
6. **Katakana loanword long vowels:** イ/ウ → ー where `pron` shows ー (アカデミイ→アカデミー).

Emit one token-granular `OrthoAnnotation` per changed token (U3: clean remap;
cross-boundary → honest `ortho_remap_crosses_boundary` error row). Identity binds
the `kindai-bungo` `archive_hash` + this rule table's hash (U3 I2-D17).

## Open items the probe did not settle

- **True accuracy** needs parallel 旧仮名/新仮名 editions as ground truth (not in
  this sample). The probe measures *distribution and rule-tractability*, not a
  precision/recall number. A follow-up could align works that exist in both
  orthographies.
- **Rule 5 spelling edge cases** (えう→よう vs えう→えう, かう→こう vs かう→かう for
  distinct lemmas) — the `pron` target disambiguates most; irregular exceptions
  (rule 4/5 interactions) are the long tail to handle or route to the honest
  error path.
- **Coverage of the 7% "other"** beyond yotsugana/gemination — small; inspect the
  residual when implementing.
