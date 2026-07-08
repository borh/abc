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
Watsuji 日本精神, Saitō 万葉集, Okamoto 奇動昔語 — old-kana plain-text works.

**Catalog (corrected to the canonical source).** The authoritative catalog is
the aozorabunko repo's own zipped `index_pages/list_person_all_extended_utf8.zip`,
available offline via the `aozorabunko-corpus` nix output
(`${aozorabunko-src}/index_pages/…`). **19,471 works; old-kana = 4,855 新字旧仮名 +
2,374 旧字旧仮名 ≈ 7,229.** (An earlier draft used a stale local
`aozora-corpus-generator` copy — 15,405 works — whose counts were lower but
close; the numbers below are token-level and unaffected.)

**Ground truth exists — 244 parallel editions.** Grouping the canonical catalog by
(person, title) across orthography buckets finds **244 works published in BOTH an
old-kana and a 新字新仮名 edition** (e.g. Akutagawa 羅生門/河童/藪の中). **Every pair's
text is present offline** in the `aozorabunko-corpus` nix store output
(`cards/<person>/files/<workid>_ruby_<n>.zip`, 290 MB total). This resolves the
"no ground truth" open item below: a future harness can modernize the old-kana
text and diff against the human-modernized new-kana edition for a real
precision/recall number.

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

- **True accuracy** needs parallel 旧仮名/新仮名 editions as ground truth. **These
  exist — 244 parallel works, all texts offline in the `aozorabunko-corpus` nix
  output** (see Catalog above). The probe here measures *distribution and
  rule-tractability*; the parallel set is the next step for a real precision/recall
  number (modernize old edition → diff vs. new edition). Caveat: the two editions
  can differ in more than orthography (底本, editorial choices), so alignment must
  be token/segment-level, not whole-document diff.
- **Rule 5 spelling edge cases** (えう→よう vs えう→えう, かう→こう vs かう→かう for
  distinct lemmas) — the `pron` target disambiguates most; irregular exceptions
  (rule 4/5 interactions) are the long tail to handle or route to the honest
  error path.
- **Coverage of the 7% "other"** beyond yotsugana/gemination — small; inspect the
  residual when implementing.

## Ground-truth accuracy — 25 parallel editions (2026-07-08)

Using 25 of the 244 parallel 旧仮名/新仮名 pairs (Akutagawa 河童/藪の中/歯車/杜子春, …),
plain honbun extracted from both editions. Metric: tokenize the NEW
(human-modernized) edition under `kindai-bungo` into a modern-vocabulary set;
modernize each historical token of the OLD edition; count how many land on a form
the human editor actually used (membership — robust to the structural edits that
make positional alignment noisy). 10,444 historical old tokens.

| modernizer | in modern vocab |
|---|---|
| baseline (surface unchanged) | 14.8% |
| draft: surface-rewrite rules + `pron` only for long vowels | 64.6% |
| **`pron`-reconstruction for ALL all-kana historical tokens** | **94.8%** (regular 95.9%, digraph 92.7%) |

**Key refinement to the M2 design:** the primary mechanism is
**`pron`-reconstruction** — render the token's modern reading (`pron`) as
hiragana, spell long vowels per 現代仮名遣い — not the class-by-class surface
rewrites. `pron` already carries sokuon (ッ→っ), yōon (ャ→ゃ), ゐ→い, etc.; the
hand rules were doing *less* than reading `pron`. Surface-rewrite rules remain
only as a fallback for **kanji-mixed okurigana** (where `pron` covers the whole
token including the kanji reading, so it can't be used verbatim).

**Residual ~5%, two causes:**
1. **False misses from segmentation differences** — e.g. ちゃんと, こりゃ, せい are
   *correct* modernizations that simply aren't isolated tokens in the new
   edition's vocab (the editions segment/word differently). True accuracy is
   higher than 94.8%; a token-aligned metric would recover these.
2. **Genuine exceptions** (small): 現代仮名遣い **retains** づ/ぢ in some words
   (続く→つづく, though `pron`=ツズク says zu); え-row long vowels spelled ええ not
   えい (ねえ). These need a short exception table on top of `pron`-reconstruction.

**Conclusion:** M2 with `pron`-reconstruction reaches ~95% (likely higher, given
false misses) on real ground truth — the mechanism is validated. The shipped
normalizer should: (1) `pron`-reconstruct all-kana tokens; (2) rewrite only the
kana runs of kanji-mixed tokens by rule; (3) carry a small 現代仮名遣い exception
table (づ/ぢ-retention, え-row 長音); (4) keep the POS particle guard.

## Note — canonical metadata provenance (for Lane A selection)

Lane A selects old-kana works by `orthographic_style`, which comes from the aozora
catalog's 文字遣い種別 column. ABC already has a **canonical, code-driven ingester**
(`abc/src/abc/tools/aozora_csv.clj` + `aozora_ingest.clj` + `metadata_record.clj`,
flake app `aozora-ingest`) that reads the catalog ZIP and emits schema-validated,
JCS-hashed `works/<id>.json` + `persons/<id>.json` — the exact layout the Rust
`import-aozora-metadata` importer consumes. The remaining gap is a **Nix seam**:
the flake input `aozorabunko-src` (which carries the canonical
`index_pages/list_person_all_extended_utf8.zip`) is not wired into the ingester,
so the catalog ZIP is supplied manually today. Closing it = a derivation/app that
runs `aozora-ingest --all --zip ${aozorabunko-src}/index_pages/…`. That would make
"catalog → metadata-records → `aozora_works.parquet` → Lane A eligibility"
reproducible from pinned inputs.
