# Upstream XHTML Sample Summary

Manifest: `reports/aat-fidelity/upstream-xhtml-sample.tsv`

DuckDB: `/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb`

Report id: `upstream-sample`

## Totals

| total | raw_equal | main_text_equal |
| ---: | ---: | ---: |
| 5 | 4 | 5 |

## Status Counts

| comparison_status | rows |
| --- | ---: |
| raw_equal | 4 |
| main_text_equal | 1 |

## Cases

| case_id | comparison_status |
| --- | --- |
| `ginga-tetsudo` | `main_text_equal` |
| `hashire-melos` | `raw_equal` |
| `kokoro` | `raw_equal` |
| `rashomon` | `raw_equal` |
| `wagahai-neko` | `raw_equal` |

## Interpretation

For this sample, local `aozora2html` output is a good proxy for upstream Aozora
XHTML visible `main_text`: every pair has equal normalized `main_text`. It is
not always a byte-for-byte proxy: `ginga-tetsudo` differs at the raw XHTML layer.

This supports treating `aozora2html` as an indirect rendered-body adapter for
visible text comparisons, while keeping source-level structure and markup
reconstruction claims separate.
