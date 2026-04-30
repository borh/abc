# Morph corpus difference triage

Date: 2026-04-30

Input artifacts:

- `scratch/perf-interned-full-corpus/out/analyses.jsonl.zst`
- `scratch/perf-interned-full-corpus/out/comparisons.jsonl.zst`
- `scratch/perf-interned-full-corpus/out/examples.jsonl.zst`
- `scratch/perf-interned-full-corpus/out/nway.jsonl.zst`

Analyzers:

- `vibrato:unidic-cwj-202512`
- `sudachi-a`
- `sudachi-c`

Corpus run summary:

- Inputs: `17,894`
- Errors: `0`
- Analyses: `53,682`
- Pairwise compact comparisons: `53,682`
- Examples: `536,718`
- N-way rows: `17,894`

Important interpretation note:

- `summarize-compact` and `summarize-nway` row stats are exact over the compact artifacts.
- `summarize-differences` and `summarize-nway-patterns` operate over bounded example rows, not all regions. Treat `examples` counts as triage evidence, not exact corpus frequencies.
- Feature/POS reports are still noisy around whitespace and region-scope comparisons. Rows such as `pos1 名詞=>... ; 空白=>...` often indicate region alignment/coverage scope rather than a clean POS disagreement on the same lexical token.

## Commands run

```bash
target/release/ab-morph-run summarize-differences \
    --examples scratch/perf-interned-full-corpus/out/examples.jsonl.zst \
    --kind segmentation \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-segmentation.txt

target/release/ab-morph-run summarize-differences \
    --examples scratch/perf-interned-full-corpus/out/examples.jsonl.zst \
    --kind segmentation \
    --filter lexical-only \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-segmentation-lexical.txt

target/release/ab-morph-run summarize-differences \
    --examples scratch/perf-interned-full-corpus/out/examples.jsonl.zst \
    --kind feature \
    --filter lexical-only \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-feature-lexical.txt

target/release/ab-morph-run summarize-differences \
    --examples scratch/perf-interned-full-corpus/out/examples.jsonl.zst \
    --kind feature \
    --feature-key pos1 \
    --filter lexical-only \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-pos1-lexical.txt

target/release/ab-morph-run summarize-compact \
    --comparisons scratch/perf-interned-full-corpus/out/comparisons.jsonl.zst \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/worst-pairwise.txt

target/release/ab-morph-run summarize-nway \
    --nway scratch/perf-interned-full-corpus/out/nway.jsonl.zst \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/worst-nway.txt

target/release/ab-morph-run summarize-nway-patterns \
    --nway scratch/perf-interned-full-corpus/out/nway.jsonl.zst \
    --kind segmentation \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-nway-segmentation-patterns.txt

target/release/ab-morph-run summarize-nway-patterns \
    --nway scratch/perf-interned-full-corpus/out/nway.jsonl.zst \
    --kind feature \
    --feature-key pos1 \
    --script-category japanese \
    --limit 30 \
    > scratch/perf-interned-full-corpus/out/top-nway-pos1-japanese-patterns.txt
```

## Pairwise segmentation patterns

Top bounded-example patterns are overwhelmingly lexical segmentation differences, not whitespace.

| analyzers | examples | pattern |
|---|---:|---|
| vibrato vs sudachi-a | 2,908 | `あつた` vs `あつ + た` |
| vibrato vs sudachi-c | 2,447 | `二十` vs `二 + 十` |
| vibrato vs sudachi-c | 2,385 | `あつた` vs `あつ + た` |
| vibrato vs sudachi-a | 2,110 | `いつ + も` vs `いつも` |
| vibrato vs sudachi-c | 1,951 | `いつ + も` vs `いつも` |
| vibrato vs sudachi-a | 1,611 | `な + つ` vs `なつ` |
| vibrato vs sudachi-a | 1,531 | `つ + て` vs `つて` |
| vibrato vs sudachi-a | 1,452 | `一 + 度` vs `一度` |
| sudachi-a vs sudachi-c | 1,186 | `二三` vs `二 + 三` |
| sudachi-a vs sudachi-c | 1,127 | `二十` vs `二 + 十` |

Main segmentation themes:

- Historical kana/orthography: `あつた`, `つて`, `なつ`, `行つ`.
- Number expressions: `二十`, `三十`, `十二`, `十一`, `二三`.
- Lexicalized adverbs/particles: `いつも`, `一度`, `ては`, `そのまま`.
- Kinship/pronoun compounds: `私たち`, `母さん`, `お母さん`.

Actionable interpretation:

- `sudachi-c` often merges larger compounds and proper-name-like spans.
- `sudachi-a` is closer to vibrato on some compounds, but differs strongly on old orthography and some lexicalized expressions.
- Vibrato/UniDic often splits forms that Sudachi treats as one lexical unit, especially old kana forms and short compounds.

## N-way segmentation patterns

Top recurring N-way patterns show where two analyzers agree and one diverges.

| examples | pattern |
|---:|---|
| 170 | `sudachi-a + vibrato: [岸田, 國士] ; sudachi-c: [岸田國士]` |
| 110 | `sudachi-c + vibrato: [十, 二] ; sudachi-a: [十二]` |
| 91 | `sudachi-c + vibrato: [十, 一] ; sudachi-a: [十一]` |
| 60 | `sudachi-a + vibrato: [私, たち] ; sudachi-c: [私たち]` |
| 58 | `sudachi-c: [二, 十] ; sudachi-a + vibrato: [二十]` |
| 55 | `sudachi-a + vibrato: [豊島, 与志雄] ; sudachi-c: [豊島与志雄]` |
| 52 | `vibrato: [いつ, も] ; sudachi-a + sudachi-c: [いつも]` |
| 40 | `sudachi-a + vibrato: [探偵, 小説] ; sudachi-c: [探偵小説]` |
| 34 | `sudachi-a + vibrato: [登場, 人物] ; sudachi-c: [登場人物]` |
| 21 | `vibrato: [お, 母, さん] ; sudachi-a: [お, 母さん] ; sudachi-c: [お母さん]` |

N-way takeaway:

- `sudachi-c` is the compound/proper-name merger in many recurring patterns.
- `sudachi-a` sometimes merges number strings that both vibrato and `sudachi-c` split, e.g. `十二`, `十一`, `十三`, `十五`, `十四`.
- Vibrato is the outlier for some historical forms and lexicalized compounds where both Sudachi modes agree, e.g. `いつも`, `つて`, `なつ`, `就いて`.

## Feature and POS patterns

Feature reports are useful, but less clean than segmentation reports because many top rows mix lexical spans with whitespace/coverage behavior.

Top feature-key patterns from bounded examples:

| analyzers | examples | feature transition |
|---|---:|---|
| sudachi-a vs sudachi-c | 14,779 | `dictionary_form`: value vs empty |
| vibrato vs sudachi-c | 7,433 | `dictionary_form`: value vs empty |
| sudachi-a vs sudachi-c | 6,525 | `pos1`: `名詞` vs `空白` |
| vibrato vs sudachi-a | 5,495 | `goshu`: `和` vs empty |
| sudachi-a vs sudachi-c | 5,411 | `pos2`: `普通名詞` vs empty |
| vibrato vs sudachi-c | 5,259 | `goshu`: `和` vs empty |
| sudachi-a vs sudachi-c | 4,440 | `pos1`: `助詞` vs `空白` |
| sudachi-a vs sudachi-c | 4,435 | `pos3`: `一般` vs empty |
| vibrato vs sudachi-a | 3,066 | `type`: `体` vs empty |
| sudachi-a vs sudachi-c | 3,036 | `pos2`: `格助詞` vs empty |

Cleaner low-frequency POS disagreements appear much lower in the list:

| analyzers | examples | transition |
|---|---:|---|
| vibrato vs sudachi-a | 20 | `pos1`: `動詞` vs `名詞` |
| vibrato vs sudachi-a | 18 | `pos1`: `名詞` vs `動詞` |
| vibrato vs sudachi-a | 13 | `pos1`: `接尾辞` vs `名詞` |

Feature/POS takeaway:

- Most top feature rows are not semantic POS disagreements. They are schema/feature coverage differences or whitespace-alignment artifacts.
- `dictionary_form`, `normalized_form`, and `reading_form` empty-vs-present rows mostly indicate feature availability differences across adapters/modes.
- `goshu`, `type`, `atype`, and `acon_type` rows mostly reflect UniDic/Vibrato feature richness versus Sudachi feature shape.
- For true POS disagreement triage, the current summary command needs a stricter filter: exclude rows where either feature value is `空白`, and ideally restrict to exact one-to-one lexical token evidence.

## Worst sources by pairwise summary

`summary-compact` sorted by boundary F1 surfaces a mix of whitespace-only edge cases, Latin/code-heavy texts, and genuinely hard Japanese texts.

Notable rows:

| source_id | script category | worst boundary F1 | lexical segmentation regions | lexical feature-diff regions |
|---|---|---:|---:|---:|
| `000293_48490-bc2210d727db` | whitespace | null | 0 | 0 |
| `000293_60442-9301b1e3a1e3` | whitespace | null | 0 | 0 |
| `001597_53838-9776537f9066` | mixed | 0.5957 | 42 | 209 |
| `000081_43733-4063e7c46297` | japanese | 0.7297 | 140 | 2,864 |
| `000159_857-469c33b33d4a` | latin-code | 0.8144 | 570 | 1,865 |
| `000121_60573-539eaab5ecb1` | mixed | 0.8155 | 1,356 | 3,737 |
| `JISTABLE-a3b6bf10cde1` | mixed | 0.8836 | 82,080 | 435,543 |
| `001529_44909-233abede3f7e` | mixed | 0.9003 | 4,883 | 62,531 |

Interpretation:

- `null` boundary F1 rows are short/degenerate whitespace inputs, not meaningful analyzer failures.
- `JISTABLE` is an outlier and should usually be excluded from literary-text conclusions.
- The worst normal Japanese/mixed rows are useful targets for manual inspection or full reruns.

## Worst sources by N-way exact stats

Top N-way rows by segmentation/feature disagreement volume are mostly large Japanese works.

| source_id | regions | agreement regions | feature-disagreement regions | segmentation-disagreement regions | variable boundaries |
|---|---:|---:|---:|---:|---:|
| `JISTABLE-a3b6bf10cde1` | 222,079 | 0 | 216,008 | 41,042 | 59,643 |
| `001529_50685-dd3b2fe4e5bf` | 427,355 | 8 | 423,614 | 32,334 | 39,088 |
| `000129_2084-1731cdb61106` | 349,870 | 3 | 348,514 | 22,199 | 27,863 |
| `000055_365-ae629349b4c7` | 311,276 | 12 | 310,521 | 19,501 | 25,702 |
| `001111_42789-1e2257edc5d5` | 352,876 | 33 | 351,651 | 18,055 | 21,927 |
| `001099_46996-10fe9133d385` | 244,442 | 408 | 241,747 | 18,008 | 25,042 |
| `000311_2012-1bae69ee8181` | 518,793 | 128 | 517,858 | 15,723 | 17,353 |
| `001562_56146-66c41ed10b8f` | 470,219 | 15 | 469,799 | 12,824 | 15,009 |

Interpretation:

- Feature-disagreement counts are extremely high because the analyzers expose different feature schemas. These counts should not be treated as equivalent to POS disagreement volume.
- Segmentation-disagreement and variable-boundary counts are better primary ranking metrics for tokenizer behavior.
- `JISTABLE` remains an outlier and should be separated from normal corpus-level conclusions.

## Noise vs actionable disagreement

Likely noise or low-value for tokenizer quality:

- Whitespace-only sources and null boundary F1 rows.
- Feature rows where one side is empty because that analyzer does not expose the same feature key.
- POS rows involving `空白`, especially when the source span is otherwise Japanese.
- `JISTABLE` and Latin/code-heavy rows when drawing conclusions about literary Japanese text.

Actionable segmentation disagreement:

- Old orthography: `あつた`, `つて`, `なつ`, `行つ`, `就て`.
- Numeric expressions: `二十`, `三十`, `十二`, `十一`, `二三`, `二十五`.
- Compound/proper noun behavior: author names, `探偵小説`, `登場人物`, `東京市`.
- Lexicalized short expressions: `いつも`, `一度`, `そのまま`, `ては`.

Actionable reporting improvements:

1. Add a feature-report filter that excludes `空白` feature values independently of source script category.
2. Add a one-to-one lexical-only feature report mode so POS differences are not mixed with segmentation/coverage regions.
3. Add exact aggregation for N-way pattern counts instead of relying only on bounded example rows.
4. Add a default exclusion or separate bucket for known non-literary outliers such as `JISTABLE`.
