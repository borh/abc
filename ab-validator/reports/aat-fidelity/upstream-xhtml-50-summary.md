# Upstream XHTML 50-Work Observation

Generated with:

```sh
reports/aat-fidelity/build-upstream-xhtml-manifest.py \
  --aozora-root references/aozorabunko \
  --card-url references/aozorabunko/cards/001764/card55990.html \
  --card-url references/aozorabunko/cards/001185/card45210.html \
  --card-url references/aozorabunko/cards/001574/card52529.html \
  --card-url references/aozorabunko/cards/001054/card18371.html \
  --card-url references/aozorabunko/cards/001492/card51194.html \
  --person-url references/aozorabunko/index_pages/person148.html \
  --person-url references/aozorabunko/index_pages/person879.html \
  --person-url references/aozorabunko/index_pages/person35.html \
  --person-url references/aozorabunko/index_pages/person81.html \
  --person-url references/aozorabunko/index_pages/person50.html \
  --person-url references/aozorabunko/index_pages/person153.html \
  --max-cards 180 \
  --sample-size 50 \
  --classify-source \
  --out-manifest /db/ab-validator/aat-fidelity/upstream-xhtml-50-local/manifest.tsv \
  --out-metadata /db/ab-validator/aat-fidelity/upstream-xhtml-50-local/metadata.csv
```

Artifacts are stored under `/db/ab-validator/aat-fidelity/upstream-xhtml-50-local`.
Rows were loaded into
`/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb` with
`report_id = 'upstream-50-local'`. This run uses only local files from
`references/aozorabunko`; it does not download upstream corpus data.

## Result

| Metric | Count |
| --- | ---: |
| Observations | 50 |
| Raw XHTML equal | 9 |
| Normalized `main_text` equal | 46 |
| Rendered-body proxy eligible | 46 |
| Normalized `main_text` mismatch | 3 |
| Local XHTML parse error | 1 |

Status breakdown:

| Status | Rows |
| --- | ---: |
| `raw_equal` | 9 |
| `main_text_equal` | 37 |
| `main_text_mismatch` | 3 |
| `local_parse_error` | 1 |

Proxy basis:

| Proxy basis | Rows |
| --- | ---: |
| `raw_xhtml_equal` | 9 |
| `normalized_main_text_equal` | 37 |
| `not_eligible` | 4 |

Feature coverage in the selected sample:

| Feature tag | Rows |
| --- | ---: |
| `ruby` | 47 |
| `inline_annotation` | 41 |
| `layout` | 39 |
| `gaiji` | 26 |
| `heading` | 20 |
| `media` | 5 |

Rows needing follow-up, with first-diff context from the DuckDB observation:

| Case | Status | First diff | Upstream context | Local context | Interpretation |
| --- | --- | ---: | --- | --- | --- |
| `001185_45210` | `local_parse_error` | 0 | `訳者序一九〇九年、レオン・ワルラスの七十五歳の齢` | empty | Local `aozora2html` aborts with `NoMethodError` in `push_block_tag` after accent-syntax warnings. Exclude from proxy evidence until the upstream renderer path is understood. |
| `000148_2575` | `main_text_mismatch` | 2754 | `※（原）［＃「漱」の「欠」に代えて「攵」、309-` | `※［＃「漱」の「欠」に代えて「攵」、309-15］` | Local and upstream differ in how an editorial gaiji/source note is rendered. |
| `001492_51194` | `main_text_mismatch` | 256745 | `AntoineLouis,1723-92` | `AntoineLouis,1723-1792` | Local and upstream differ in date/range expansion in visible text. |
| `001764_55990` | `main_text_mismatch` | 29632 | `※（ざる）［＃「竹かんむり／瓜」、U+7B1F、` | `笟（ざる）を二つ下げている人が` | Local resolves a gaiji to Unicode where upstream text keeps the marker plus reading. |

## Interpretation

This supports local `aozora2html` XHTML as a useful rendered-body proxy for most
works in the sample, but not as a blanket substitute for upstream XHTML. The
proxy is strongest for the 46 rows with equal normalized `main_text`. The 4
non-equal rows should be excluded or separately flagged when using local XHTML
as corroborating evidence.

Source-derived AAT enrichment remains a separate layer. It may recover source
annotation structure that XHTML has flattened or rendered as display text, but
such nodes must be marked with `x-provenance = "source-derived"` and should not
be counted as direct upstream-XHTML faithfulness.
