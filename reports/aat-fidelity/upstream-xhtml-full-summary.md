# Upstream XHTML Full-Corpus Summary

This report compares Aozora upstream XHTML against local `aozora2html` XHTML
generated from the local `references/aozorabunko` mirror. It does not download
corpus data from the network.

## Reproducible Run

Run the full corpus refresh with:

```bash
reports/aat-fidelity/run-upstream-xhtml-full.sh \
  --aozora-root references/aozorabunko \
  --out-dir /db/ab-validator/aat-fidelity/upstream-xhtml-full \
  --db /db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb \
  --report-id upstream-xhtml-full \
  --jobs "$(nproc)" \
  --triage-limit 100
```

The script writes intermediate and generated files under `/db`, including:

- `card-urls.txt`
- `manifest.tsv`
- `manifest.valid.tsv`
- `manifest.invalid.tsv`
- `metadata.csv`
- `generation-failures.tsv`
- `observations/*.local.xhtml`
- `observations/summary.csv`
- `observations/status-summary.csv`
- `triage-report/index.md`

Use `--force` only when local XHTML should be regenerated. Without `--force`,
existing non-empty local XHTML files are reused and the DuckDB load/reporting
steps are refreshed.

## Current Status

The current `report_id = 'upstream-xhtml-full'` has 17,601 loaded observations.

```csv
comparison_status,row_count
both_missing_main_text,1
local_adapter_error,58
main_text_equal,8910
main_text_mismatch,2179
raw_equal,6321
upstream_missing_main_text,132
```

Rendered-body proxy evidence is intentionally narrow:

```csv
comparison_status,rendered_body_proxy_eligible,proxy_basis,row_count
both_missing_main_text,false,not_eligible,1
local_adapter_error,false,not_eligible,58
main_text_equal,true,normalized_main_text_equal,8910
main_text_mismatch,false,not_eligible,2179
raw_equal,true,raw_xhtml_equal,6321
upstream_missing_main_text,false,not_eligible,132
```

## Main Mismatch Families

The 2,179 `main_text_mismatch` rows are dominated by two families:

- `text_content` where local output often contains title/author/front-matter or
  continues past an upstream body boundary.
- `aozora_note_marker` where source notes, correction notes, warichu markers,
  figure markers, or layout markers survive on one side but not the other.

Top shape counts:

```csv
length_direction,length_delta_bucket,first_diff_family,row_count
local_longer,1-10,text_content,664
local_longer,11-100,text_content,562
upstream_longer,11-100,aozora_note_marker,559
upstream_longer,101-1000,aozora_note_marker,163
local_longer,11-100,aozora_note_marker,72
```

Feature-tag hotspots:

```csv
feature_tag,comparison_status,row_count
inline_annotation,main_text_mismatch,1003
layout,main_text_mismatch,934
ruby,main_text_mismatch,911
gaiji,main_text_mismatch,844
heading,main_text_mismatch,554
warichu,main_text_mismatch,145
media,main_text_mismatch,90
```

## Local Adapter Errors

The 58 `local_adapter_error` rows are adapter-abort JSON payloads from the
wrapped Ruby `aozora2html` gem, not malformed local XHTML. The largest class is
block-stack imbalance in source files:

```csv
family,count
jisage_close_without_open,37
ruby_no_method_error,10
other_parser_abort,11
```

These rows should remain separate from XHTML comparison failures. They are
useful evidence about local `aozora2html` coverage, but they are not evidence
that upstream XHTML disagrees with local rendered XHTML.

## Oracle Policy

Use upstream XHTML evidence only for rendered-output claims:

- `raw_equal` and `main_text_equal` support treating local `aozora2html` output
  as a rendered-body proxy for that work.
- `main_text_mismatch` requires family-specific triage before being used as
  oracle evidence.
- `local_adapter_error`, parse errors, and missing-main-text statuses do not
  support rendered-body proxy claims.

Source-level AAT oracle failures for `aozora2html` should continue to be
reported as source-level divergence unless the rendered XHTML contains enough
structure to support the claim.
