# TEI-EAJ Structural Gap Analysis

Date: 2026-07-04

## Verdict

`PARSER_IR_LEVEL3_REPRESENTABLE_WITH_ADAPTER_FIDELITY_GAPS`

After mapping TEI-EAJ file identifiers to Aozora corpus IDs, the whole
TEI-EAJ/aozora_tei comparison has no row-level parser-IR gap:

| Metric | Value |
|---|---:|
| TEI-EAJ rows | 62 |
| rows with AAT evidence | 57 |
| parser-IR gap rows | 0 |
| source-attribution gap rows | 0 |
| adapter gap rows | 18 |
| evidence gap rows | 5 |

The two former `15099` evidence gaps were false negatives. TEI-EAJ identifies
the work as `15099`, while the measured Aozora corpora materialize the same
source as `000879_104`. The structural expansion now builds a conservative
alias map from TEI-EAJ filenames such as `104_15099.xml`, so both `15099` rows
receive AAT evidence from:

- `aozora-rs:15099` -> AAT `work_id = "000879_104"`,
- `aozora2html:15099` -> AAT `work_id = "000879_104"`,
- `aozora-epub3:15099` -> AAT `work_id = "000879_104"`.

## Remaining Evidence Gaps

The remaining 5 evidence gaps are TEI-EAJ rows with no Aozora work ID in ABC's
workset export. These are not parser-IR representability conclusions.

| title | TEI-EAJ file | level/state | TEI p |
|---|---|---|---:|
| 源氏物語 第1冊 | `data/draft/tei_lib_lv2/01.xml` | Level 2 draft | 1 |
| 源氏物語 第2冊 | `data/draft/tei_lib_lv2/02.xml` | Level 2 draft | 1 |
| Title | `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml` | Level 2 draft | 2 |
| 勢理客村湧川親雲上勤職書 | `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml` | etc | 4 |
| 校異源氏物語・きりつぼ | `data/etc/校異源氏物語_header更新版.xml` | etc | 1 |

These rows should be classified by ABC as either out-of-scope for Aozora parser
compatibility, or supplied with an explicit source-material mapping. They should
not block parser-IR Level 3 admission for Aozora-derived evidence.

## Adapter Gaps

The 18 adapter gaps are collapse-to-one cases: TEI-EAJ has more than one body
paragraph, but at least one measured adapter AAT has zero or one paragraph
block.

| adapter | collapsed rows |
|---|---:|
| aozora-rs | 16 |
| aozora2 | 2 |
| aozora2html | 0 |
| aozora-epub3 | 0 |

Flagged rows:

| work_id | title | TEI-EAJ file | TEI p | collapsed adapter(s) | other AAT paragraph counts |
|---|---|---|---:|---|---|
| 1126 | 三つの宝 | `data/complete/tei_lib_lv3/1126_tei.xml` | 169 | aozora-rs:1 | aozora2html:30, aozora-epub3:154 |
| 2509 | 天災と国防 | `data/complete/tei_lib_lv3/2509_tei.xml` | 28 | aozora-rs:1 | aozora2html:28, aozora-epub3:28 |
| 4464 | 秋田街道 | `data/complete/tei_lib_lv3/4464_tei.xml` | 17 | aozora-rs:1 | aozora2html:26, aozora-epub3:26 |
| 50502 | 北海道に就いての印象 | `data/complete/tei_lib_lv3/50502_tei.xml` | 8 | aozora-rs:1 | aozora2html:8, aozora-epub3:8 |
| 51307 | みだれ髪 | `data/complete/tei_lib_lv3/51307_tei.xml` | 7 | aozora-rs:1 | aozora2html:412, aozora-epub3:400 |
| 55783 | 夢 | `data/complete/tei_lib_lv3/55783_tei.xml` | 126 | aozora-rs:1 | aozora2html:16, aozora-epub3:127 |
| 15099 | 長崎小品 | `data/complete/tei_lib_lv4/104_15099.xml` | 57 | aozora-rs:1 | aozora2html:62, aozora-epub3:62 |
| 1567 | 走れメロス | `data/complete/tei_lib_lv4/1567_header_updated.xml` | 22 | aozora2:1 | aozora-rs:79, aozora2html:75, aozora-epub3:75 |
| 1567 | 走れメロス | `data/complete/tei_lib_lv4/1567_tei.xml` | 19 | aozora2:1 | aozora-rs:79, aozora2html:75, aozora-epub3:75 |
| 7928 | 旅人（一幕） | `data/complete/tei_lib_lv4/7928_tei.xml` | 110 | aozora-rs:1 | aozora2html:61, aozora-epub3:148 |
| 1126 | 三つの宝 | `data/draft/tei_lib_lv3/1126_tei.xml` | 155 | aozora-rs:1 | aozora2html:30, aozora-epub3:154 |
| 1576 | 新ハムレット | `data/draft/tei_lib_lv3/1576_tei.xml` | 496 | aozora-rs:1 | aozora2html:467, aozora-epub3:529 |
| 52208 | 帝大聖書研究会終講の辞 | `data/draft/tei_lib_lv3/52208_tei.xml` | 18 | aozora-rs:1 | aozora2html:27, aozora-epub3:27 |
| 1805 | 安重根 | `data/draft/tei_lib_lv4/1805_tei.xml` | 29 | aozora-rs:1 | aozora2html:450, aozora-epub3:880 |
| 4244 | 獄中への手紙 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | 24 | aozora-rs:1 | aozora2html:74, aozora-epub3:74 |
| 4244 | 獄中への手紙 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | 13 | aozora-rs:1 | aozora2html:74, aozora-epub3:74 |
| 4244 | 獄中への手紙 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | 8 | aozora-rs:1 | aozora2html:74, aozora-epub3:74 |
| 46453 | 春 | `data/draft/tei_lib_lv4/46453_tei.xml` | 89 | aozora-rs:1 | aozora2html:74, aozora-epub3:145 |

## Paragraph Count Blindspot

The current `adapter_gap` flag is intentionally narrow: it detects collapsed
paragraph evidence, not paragraph-boundary accuracy. On the 34 evidence-backed
rows with more than one TEI-EAJ paragraph, the best available adapter count is:

| best adapter delta vs. TEI-EAJ p count | rows |
|---|---:|
| exact | 6 |
| 1 paragraph off | 4 |
| 2-5 paragraphs off | 5 |
| 6-10 paragraphs off | 4 |
| more than 10 paragraphs off | 15 |

Adapter-level paragraph count profile over those 34 rows:

| adapter | exact | <=5 off | >=10 off | collapsed |
|---|---:|---:|---:|---:|
| aozora-rs | 0 | 4 | 24 | 16 |
| aozora2html | 5 | 13 | 19 | 0 |
| aozora-epub3 | 6 | 14 | 18 | 0 |
| aozora2 | 0 | 0 | 2 | 2 |

This means parser-IR can now represent Level 3 paragraph/source-note structure
when AAT supplies it, but the current adapter evidence is not strong enough to
claim paragraph-boundary fidelity against TEI-EAJ. That is an adapter/parser
candidate evaluation problem, not a parser-IR schema problem.

## Next Steps

1. Keep the TEI-EAJ ID aliasing in ab-validator, but ask ABC to export explicit
   source aliases where possible. The local filename heuristic is conservative
   and useful, but ABC is the better owner for durable workset identity.
2. Extend the TEI-EAJ expansion summary with paragraph-count delta buckets
   (`best_delta`, per-adapter delta, and exact/near/far counts), so future runs
   do not hide non-collapsed paragraph disagreement behind `adapter_gap=false`.
3. Treat the 16 aozora-rs collapse rows and 2 aozora2 collapse rows as adapter
   fidelity gaps. They do not block parser-IR Level 3 representability, but they
   are useful parser-candidate comparison evidence.
4. Ask ABC to classify the 5 no-work-ID TEI-EAJ rows as out-of-scope or provide
   source mappings before using them in parser compatibility gates.
