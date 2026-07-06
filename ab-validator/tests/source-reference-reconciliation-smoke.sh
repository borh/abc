#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/smoke-env.sh"
repo_root="$AB_VALIDATOR_ROOT"
tmp="$(smoke_tmp_dir ab-source-reference-reconciliation)"
trap 'smoke_cleanup "$tmp"' EXIT

cat > "$tmp/syntax.toml" <<'TOML'
[[syntax]]
id = "ruby.basic"
reference_sources = ["references/aozorabunko/rules/kijyunn.html"]
source_patterns = ['《[^》]+》']

[syntax.representability]
source_inventory_row = "ruby.basic"
status = "typed"

[[syntax]]
id = "fraction.basic"
reference_sources = ["references/aozorabunko/rules/comments.html"]
source_patterns = ['分数']

[syntax.representability]
source_inventory_row = "fraction.basic"
status = "needs_research"

[[syntax]]
id = "comparison.only"
reference_sources = ["references/other/spec.md"]
source_patterns = ['x']

[syntax.representability]
source_inventory_row = "comparison.only"
status = "needs_research"
TOML

cat > "$tmp/source-summary.json" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "rows": {
    "ruby.basic": {
      "works_with_marker": 2,
      "occurrences": 5,
      "sample_works": ["000001_1", "000002_2"]
    },
    "source.only": {
      "works_with_marker": 1,
      "occurrences": 3,
      "sample_works": ["000003_3"]
    },
    "fraction.basic": {
      "works_with_marker": 0,
      "occurrences": 0,
      "sample_works": []
    }
  }
}
JSON

cat > "$tmp/notation-summary.json" <<'JSON'
{
  "schema_version": 1,
  "rows": [
    {"vector": "ruby_explicit", "feature": "ruby", "level": "must", "adapter": "fake", "status": "pass", "failures": [], "warnings": []},
    {"vector": "fraction_simple", "feature": "fraction", "level": "should", "adapter": "fake", "status": "warning", "failures": [], "warnings": []},
    {"vector": "future_shape", "feature": "future_feature", "level": "may", "adapter": "fake", "status": "warning", "failures": [], "warnings": []}
  ]
}
JSON

mkdir -p "$tmp/manual"
cat > "$tmp/manual/kijyunn.html" <<'HTML'
<html><body>青空文庫注記例</body></html>
HTML

python3 "$repo_root/reports/source-references/reconcile-aozora-notation.py" \
  --syntax-coverage "$tmp/syntax.toml" \
  --source-summary "$tmp/source-summary.json" \
  --notation-summary "$tmp/notation-summary.json" \
  --manual-root "$tmp/manual" \
  --summary-json "$tmp/summary.json" \
  --report-md "$tmp/report.md"

jq -e '.schema_version == "aozora-source-reference-reconciliation-v1"' "$tmp/summary.json" >/dev/null
jq -e '.verdict == "SOURCE_REFERENCE_RECONCILIATION_REVIEW_REQUIRED"' "$tmp/summary.json" >/dev/null
jq -e '.totals.official_syntax_rows == 2' "$tmp/summary.json" >/dev/null
jq -e '.totals.source_inventory_rows == 3' "$tmp/summary.json" >/dev/null
jq -e '.totals.p4suta_features == 3' "$tmp/summary.json" >/dev/null
jq -e '.documented_observed[] | select(.syntax_id == "ruby.basic" and .occurrences == 5)' "$tmp/summary.json" >/dev/null
jq -e '.documented_unobserved[] | select(.syntax_id == "fraction.basic")' "$tmp/summary.json" >/dev/null
jq -e '.observed_without_syntax_row[] | select(.source_inventory_row == "source.only" and .occurrences == 3)' "$tmp/summary.json" >/dev/null
jq -e '.p4suta_feature_mapped[] | select(.feature == "ruby" and (.source_inventory_rows | index("ruby.basic")))' "$tmp/summary.json" >/dev/null
jq -e '.p4suta_feature_mapped[] | select(.feature == "fraction" and (.source_inventory_rows | index("fraction.basic")))' "$tmp/summary.json" >/dev/null
jq -e '.p4suta_feature_unmapped[] | select(.feature == "future_feature")' "$tmp/summary.json" >/dev/null
rg -n 'Source Reference Reconciliation' "$tmp/report.md" >/dev/null
rg -n 'source.only' "$tmp/report.md" >/dev/null

echo "source reference reconciliation smoke ok"
