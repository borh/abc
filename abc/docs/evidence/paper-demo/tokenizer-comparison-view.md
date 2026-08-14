# Tokenizer Comparison View

This is the secondary paper artifact for tokenization. The main demo should stay
focused on parser evidence, compatibility admission, and TEI output. Tokenizer
results are strongest when shown as parallel analytical artifacts over the same
identified text.

## Current Evidence

For citations, use logical component paths. The current `../ab-validator/...`
or `/db/ab-validator/...` values are local locators for rerunning queries before
the monorepo migration.

| Evidence | Corpus/Input | Analyzers | Paper-useful signal |
|---|---|---|---|
| `ab-validator/docs/superpowers/reports/2026-04-29-morph-full-corpus.md` | 17,894 AAT JSON files from the aozora-rs adapter path | `vibrato:unidic-cwj-202512`, `sudachi-c` | No analyzer failures, no comparison failures, no coverage mismatch regions. Boundary-F1 median `0.972972972972973`, minimum `0.6884927066450566`, total segmentation regions `7,358,000`. |
| `ab-validator/benchmarks/baselines/morph-2026-07-03.md` | 17,689 AAT JSON files from the aozora2html full-corpus path | `vibrato`, `sudachi-a`, `sudachi-c` | Warehouse-mode run with 265,335 analysis rows, 4,177,696,300 morpheme rows, and 1,776,018,166 N-way region rows. This is scale evidence, not the presentation table itself. |
| `ab-validator/docs/morph-corpus-workflow.md` | Canonical workflow over checked AAT JSON | `vibrato`, `sudachi-a`, `sudachi-c` | Warehouse mode is the canonical comprehensive artifact; summary commands can extract bounded region or pattern examples without JSONL intermediates. |

The April report already gives presentation candidates for disagreement-heavy
texts. Worst boundary-F1 examples include `000081_43733`, `000136_45052`,
`000081_53446`, `001675_54805`, and `001172_44928`. These are analyzer
segmentation differences, not adapter coverage failures.

The local expanded warehouse has both the earlier disagreement-heavy candidate
and the "Run, Melos!" presentation work available:

| Run | Source ID | Regions | Segmentation disagreements | Feature disagreements |
|---|---|---:|---:|---:|
| `compare-tokenizers-expanded-full-2026-05-05` | `000081_43733-4063e7c46297` | 1,097 | 74 | 1,060 |
| `compare-tokenizers-expanded-full-2026-05-05` | `000035_1567-32ff5a089d67` | 6,499 | 164 | 6,467 |

## Demo Cut

Use tokenization as a compact comparison layer after the parser/TEI evidence.
The same identified AAT source can support multiple tokenizer-specific
analytical artifacts, and the differences are research-relevant rather than
pipeline noise.

Show only this two-row table in the main demo. Keep the warehouse counts above
as backup credibility evidence.

| Work | Region | Phrase | Main contrast |
|---|---:|---|---|
| `000081_43733` | 15 | `求むる` | Sudachi keeps the archaic verb as one 動詞 token; UniDic-backed Vibrato splits it into `求 | む | る` with differing POS assignments. |
| `000035_1567` 走れメロス | 208 | `竹馬の友` | Sudachi C treats the idiom as one named-noun-like unit, while Sudachi A/B and Vibrato keep `竹馬 | の | 友`. This connects the tokenizer artifact to the "Run, Melos!" TEI demo. |

Identity caption for slides or paper notes:

> Tokenizer outputs are not one authoritative tokenized corpus. They are
> parallel analytical artifacts over the same source identity, distinguished by
> analyzer build, dictionary/profile, normalization policy, source snapshot,
> and parser/output identity.

## Filled Presentation Regions

### Older-Language Example

Selected text and context:

| Field | Value |
|---|---|
| Text ID | `000081_43733` |
| AAT source | `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000081_43733-4063e7c46297.json` |
| Region | `15` |
| Character span | `24..27` |
| Source phrase | `その地点を求むるならば` |
| Compared surface | `求むる` |

Segmentation and POS view:

| Analyzer | Dictionary/profile | Segmentation | POS observation |
|---|---|---|---|
| `sudachi-a` | Sudachi split mode A | `求むる` | `求むる` = 動詞 |
| `sudachi-b` | Sudachi split mode B | `求むる` | `求むる` = 動詞 |
| `sudachi-c` | Sudachi split mode C | `求むる` | `求むる` = 動詞 |
| `vibrato:unidic-csj-202512` | UniDic CSJ | `求 \| む \| る` | `求` = 名詞; `む` = 助動詞; `る` = 助動詞 |
| `vibrato:unidic-cwj-202512` | UniDic CWJ | `求 \| む \| る` | `求` = 名詞; `む` = 名詞; `る` = 助動詞 |

### "Run, Melos!" Example

Selected text and context:

| Field | Value |
|---|---|
| Text ID | `000035_1567` |
| AAT source | `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000035_1567-32ff5a089d67.json` |
| Region | `208` |
| Character span | `309..313` |
| Source phrase | `メロスには竹馬の友があった。` |
| Compared surface | `竹馬の友` |

Segmentation and POS view:

| Analyzer | Dictionary/profile | Segmentation | POS observation |
|---|---|---|---|
| `sudachi-a` | Sudachi split mode A | `竹馬 \| の \| 友` | `竹馬` = 名詞/普通名詞/一般; `の` = 助詞/格助詞; `友` = 名詞/普通名詞/一般 |
| `sudachi-b` | Sudachi split mode B | `竹馬 \| の \| 友` | `竹馬` = 名詞/普通名詞/一般; `の` = 助詞/格助詞; `友` = 名詞/普通名詞/一般 |
| `sudachi-c` | Sudachi split mode C | `竹馬の友` | `竹馬の友` = 名詞/固有名詞/一般 |
| `vibrato:unidic-csj-202512` | UniDic CSJ | `竹馬 \| の \| 友` | `竹馬` = 名詞/普通名詞/一般; `の` = 助詞/格助詞; `友` = 名詞/普通名詞/一般 |
| `vibrato:unidic-cwj-202512` | UniDic CWJ | `竹馬 \| の \| 友` | `竹馬` = 名詞/普通名詞/一般; `の` = 助詞/格助詞; `友` = 名詞/普通名詞/一般 |

The important claim is comparative, not authoritative: tokenizer-specific
outputs are separate, citable artifacts tied to source identity, analyzer build,
dictionary/profile, normalization policy, and upstream corpus snapshot.

For this paper, do not make the tokenizer table carry the whole provenance
argument. Let the parser/TEI trace establish source and compatibility identity,
then use this table to show why tokenizer choice remains visible as its own
analysis coordinate.

## Extraction Path

For a final slide or live demo, prefer bounded warehouse queries instead of a
large corpus table. The current warehouse locator is
`/db/ab-validator/morph-warehouse/runs/compare-tokenizers-expanded-full-2026-05-05`;
after the monorepo migration this remains a generated analytical artifact, not
canonical identity.

Older-language example:

```bash
duckdb -csv \
  -c ".read /db/ab-validator/morph-warehouse/runs/compare-tokenizers-expanded-full-2026-05-05/views.sql" \
  -c "SELECT DISTINCT r.source_id, r.text_id, r.region_index,
             r.char_start, r.char_end, a.analyzer_id,
             array_to_string(a.surfaces, ' | ') AS surfaces
      FROM warehouse_nway_regions r
      JOIN warehouse_nway_region_analyzers a
        USING (run_id, source_id, text_id, region_index)
      WHERE r.text_id='000081_43733'
        AND r.region_index=15
      ORDER BY a.analyzer_id;"
```

"Run, Melos!" example:

```bash
duckdb -csv \
  -c ".read /db/ab-validator/morph-warehouse/runs/compare-tokenizers-expanded-full-2026-05-05/views.sql" \
  -c "SELECT DISTINCT r.source_id, r.text_id, r.region_index,
             r.char_start, r.char_end, a.analyzer_id,
             array_to_string(a.surfaces, ' | ') AS surfaces
      FROM warehouse_nway_regions r
      JOIN warehouse_nway_region_analyzers a
        USING (run_id, source_id, text_id, region_index)
      WHERE r.text_id='000035_1567'
        AND r.region_index=208
      ORDER BY a.analyzer_id;"
```

The POS values come from joining the region analyzer morpheme ranges to
`morphemes.parquet` and `morpheme_features.parquet` for `feature_key LIKE
'pos%'`, then de-duplicating repeated feature values.

## Paper Positioning

Tokenization should appear after parser and TEI evidence:

1. parser disagreement defines what text and annotations are being compared,
2. compatibility gates identify which parser/mapping evidence is admissible,
3. plaintext and TEI provide publication-facing views,
4. tokenizer outputs become parallel analytical views over the same identified
   text.
