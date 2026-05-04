# Upstream XHTML 50-Work Observation

Generated with:

```sh
reports/aat-fidelity/build-upstream-xhtml-manifest.py \
  --card-url https://www.aozora.gr.jp/cards/001764/card55990.html \
  --card-url https://www.aozora.gr.jp/cards/001185/card45210.html \
  --card-url https://www.aozora.gr.jp/cards/001574/card52529.html \
  --card-url https://www.aozora.gr.jp/cards/001054/card18371.html \
  --card-url https://www.aozora.gr.jp/cards/001492/card51194.html \
  --person-url https://www.aozora.gr.jp/index_pages/person148.html \
  --person-url https://www.aozora.gr.jp/index_pages/person879.html \
  --person-url https://www.aozora.gr.jp/index_pages/person35.html \
  --person-url https://www.aozora.gr.jp/index_pages/person81.html \
  --person-url https://www.aozora.gr.jp/index_pages/person50.html \
  --person-url https://www.aozora.gr.jp/index_pages/person153.html \
  --max-cards 180 \
  --sample-size 50 \
  --classify-source
```

Artifacts are stored under `/db/ab-validator/aat-fidelity/upstream-xhtml-50`.
Rows were loaded into
`/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb` with
`report_id = 'upstream-50'`.

## Result

| Metric | Count |
| --- | ---: |
| Observations | 50 |
| Raw XHTML equal | 9 |
| Normalized `main_text` equal | 45 |
| Normalized `main_text` mismatch | 4 |
| Local XHTML parse error | 1 |

Status breakdown:

| Status | Rows |
| --- | ---: |
| `raw_equal` | 9 |
| `main_text_equal` | 36 |
| `main_text_mismatch` | 4 |
| `local_parse_error` | 1 |

Feature coverage in the selected sample:

| Feature tag | Rows |
| --- | ---: |
| `ruby` | 47 |
| `inline_annotation` | 41 |
| `layout` | 39 |
| `gaiji` | 26 |
| `heading` | 20 |
| `media` | 5 |

Rows needing follow-up:

| Case | Status | Tags | Upstream chars | Local chars |
| --- | --- | --- | ---: | ---: |
| `001185_45210` | `local_parse_error` | ruby; inline_annotation; heading; layout; media | 87355 | 0 |
| `000148_2575` | `main_text_mismatch` | gaiji; inline_annotation; layout | 7930 | 7940 |
| `000148_2674` | `main_text_mismatch` | ruby | 3630 | 1871 |
| `001492_51194` | `main_text_mismatch` | ruby; inline_annotation; heading; layout; media | 274676 | 274740 |
| `001764_55990` | `main_text_mismatch` | ruby; gaiji; inline_annotation; heading; layout; media | 485703 | 485247 |

## Interpretation

This supports local `aozora2html` XHTML as a useful rendered-body proxy for most
works in the sample, but not as a blanket substitute for upstream XHTML. The
proxy is strongest for the 45 rows with equal normalized `main_text`. The 5
non-equal rows should be excluded or separately flagged when using local XHTML
as corroborating evidence.

Source-derived AAT enrichment remains a separate layer. It may recover source
annotation structure that XHTML has flattened or rendered as display text, but
such nodes must be marked with `x-provenance = "source-derived"` and should not
be counted as direct upstream-XHTML faithfulness.
