#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-measurement-audit-smoke"
base="$out/base"
retry="$out/retry"
other="$out/other"
audit_out="$out/audit"

rm -rf "$out"
mkdir -p "$base/check-reports/fixture-adapter" "$base/aat/fixture-adapter" \
  "$retry/check-reports/fixture-adapter" "$retry/aat/fixture-adapter" \
  "$other/check-reports/fixture-adapter" "$other/aat/fixture-adapter" \
  "$audit_out"

duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

cat > "$base/metadata.json" <<'JSON'
{
  "report_id": "smoke-base",
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0"
}
JSON

cat > "$retry/metadata.json" <<'JSON'
{
  "report_id": "smoke-retry",
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0"
}
JSON

cat > "$other/metadata.json" <<'JSON'
{
  "report_id": "smoke-other-failure-only",
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0"
}
JSON

cat > "$base/index.json" <<'JSON'
{
  "version": 1,
  "corpus_root": "fixture",
  "corpus_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "generated_at": "2026-07-03T00:00:00Z",
  "works_count": 7,
  "works": [],
  "by_feature": {
    "warigaki": ["work-war-missing", "work-war-ok"],
    "kaeriten": ["work-kun-ok", "work-kun-overlap"],
    "okurigana": [
      "work-kun-ok",
      "work-kun-overlap",
      "work-retry-kun",
      "work-kun-ruby",
      "work-kun-semantic-only"
    ]
  }
}
JSON

cp "$base/index.json" "$retry/index.json"

cat > "$other/index.json" <<'JSON'
{
  "version": 1,
  "corpus_root": "fixture",
  "corpus_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "generated_at": "2026-07-03T00:00:00Z",
  "works_count": 1,
  "works": [],
  "by_feature": {
    "warigaki": ["work-war-other-failure"],
    "kaeriten": [],
    "okurigana": []
  }
}
JSON

write_report() {
  local root="$1"
  local work_id="$2"
  local results="$3"
  cat > "$root/check-reports/fixture-adapter/$work_id.json" <<JSON
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "$work_id",
  "results": $results
}
JSON
}

pass_results='{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"}
}'
parse_incomplete_results='{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": false, "message": "Adapter reported parse_complete=false", "confidence": "strict"}
}'
timeout_results='{
  "adapter_timeout": {"pass": false, "message": "adapter timed out", "confidence": "strict"}
}'
other_failure_results='{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"},
  "visible_text_body_order": {"pass": false, "message": "AAT visible text projection is not accounted for in source order", "confidence": "strict"}
}'

write_report "$base" work-war-ok "$pass_results"
write_report "$base" work-war-missing "$pass_results"
write_report "$base" work-kun-ok "$pass_results"
write_report "$base" work-kun-overlap "$parse_incomplete_results"
write_report "$base" work-retry-kun "$timeout_results"
write_report "$base" work-kun-ruby "$pass_results"
write_report "$base" work-kun-semantic-only "$pass_results"

write_report "$retry" work-retry-kun "$pass_results"
write_report "$other" work-war-other-failure "$other_failure_results"

cat > "$base/aat/fixture-adapter/work-war-ok.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-war-ok",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "warigaki",
          "upper": [{"kind": "text", "value": "上"}],
          "lower": [{"kind": "text", "value": "下"}]
        }
      ]
    }
  ],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true
  }
}
JSON

cat > "$base/aat/fixture-adapter/work-war-missing.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-war-missing",
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "plain"}]}],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true
  }
}
JSON

cat > "$base/aat/fixture-adapter/work-kun-ok.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-kun-ok",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "style",
          "style_type": "kaeriten",
          "content": [{"kind": "text", "value": "レ"}]
        }
      ]
    }
  ],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true,
    "semantic_summary": {
      "syntax": {
        "kunten.kaeriten": [
          {"kind": "kaeriten", "provenance": "source-derived", "value": "レ"}
        ]
      }
    }
  }
}
JSON

cat > "$base/aat/fixture-adapter/work-kun-overlap.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-kun-overlap",
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "partial"}]}],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": false
  }
}
JSON

cat > "$base/aat/fixture-adapter/work-kun-ruby.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-kun-ruby",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "ruby",
          "x-annotation-type": "okurigana",
          "base_content": [{"kind": "text", "value": "送"}],
          "reading_content": [{"kind": "text", "value": "おく"}]
        }
      ]
    }
  ],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true
  }
}
JSON

cat > "$base/aat/fixture-adapter/work-kun-semantic-only.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-kun-semantic-only",
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "義"}]}],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true,
    "semantic_summary": {
      "syntax": {
        "kunten.okurigana": [
          {"kind": "okurigana", "provenance": "semantic-only", "value": "義"}
        ]
      }
    }
  }
}
JSON

cat > "$retry/aat/fixture-adapter/work-retry-kun.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-retry-kun",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "style",
          "style_type": "kaeriten",
          "content": [{"kind": "text", "value": "一"}]
        }
      ]
    }
  ],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true,
    "semantic_summary": {
      "syntax": {
        "kunten.kaeriten": [
          {"kind": "kaeriten", "provenance": "source-derived", "value": "一"}
        ]
      }
    }
  }
}
JSON

cat > "$other/aat/fixture-adapter/work-war-other-failure.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-war-other-failure",
  "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "plain"}]}],
  "meta": {
    "adapter": "fixture-adapter",
    "adapter_version": "fixture 1.0",
    "parse_complete": true
  }
}
JSON

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$base/check-reports" \
  --aat-dir "$base/aat" \
  --db "$base/fidelity.duckdb" \
  --report-id smoke-base \
  --out-dir "$base/triage"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$retry/check-reports" \
  --aat-dir "$retry/aat" \
  --db "$retry/fidelity.duckdb" \
  --report-id smoke-retry \
  --out-dir "$retry/triage"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$other/check-reports" \
  --aat-dir "$other/aat" \
  --db "$other/fidelity.duckdb" \
  --report-id smoke-other-failure-only \
  --out-dir "$other/triage"

python3 "$repo_root/reports/aat-fidelity/audit-aozora2html-measurement.py" \
  --run-dir "$base" \
  --out-md "$audit_out/audit.md" \
  --summary-json "$audit_out/audit.summary.json" \
  --worksets-dir "$audit_out/worksets"

jq -e '.source_counts.kunten == 5' "$audit_out/audit.summary.json"
jq -e '.source_counts.kunten != (.source_sets.kaeriten | length) + (.source_sets.okurigana | length)' "$audit_out/audit.summary.json"
jq -e '.bucket_counts.kunten.observed_in_aat == 3' "$audit_out/audit.summary.json"
jq -e '.bucket_counts.kunten.adapter_timeout_or_protocol_error == 1' "$audit_out/audit.summary.json"
jq -e '.bucket_counts.kunten.parse_incomplete == 1' "$audit_out/audit.summary.json"
jq -e '.buckets.kunten.observed_in_aat | index("work-kun-ruby")' "$audit_out/audit.summary.json"
jq -e '.buckets.kunten.observed_in_aat | index("work-kun-semantic-only")' "$audit_out/audit.summary.json"
jq -e '(.buckets.kunten.source_feature_without_aat_observation | index("work-kun-ruby")) == null' "$audit_out/audit.summary.json"
jq -e '(.buckets.kunten.source_feature_without_aat_observation | index("work-kun-semantic-only")) == null' "$audit_out/audit.summary.json"
jq -e '.aat_observed_counts.kunten_observations == 4' "$audit_out/audit.summary.json"
jq -e '.bucket_counts.warigaki.source_feature_without_aat_observation == 1' "$audit_out/audit.summary.json"
jq -e 'type == "array" and all(.[]; type == "string")' "$audit_out/worksets/policy-incomplete-union.json"
rg -n "source kunten works" "$audit_out/audit.md"

python3 "$repo_root/reports/aat-fidelity/audit-aozora2html-measurement.py" \
  --run-dir "$base" \
  --retry-run-dir "$retry" \
  --out-md "$audit_out/audit-retry.md" \
  --summary-json "$audit_out/audit-retry.summary.json" \
  --worksets-dir "$audit_out/retry-worksets"

jq -e '.bucket_counts.kunten.observed_in_aat == 4' "$audit_out/audit-retry.summary.json"
jq -e '.bucket_counts.kunten.adapter_timeout_or_protocol_error == 0' "$audit_out/audit-retry.summary.json"
rg -n "retry run" "$audit_out/audit-retry.md"

python3 "$repo_root/reports/aat-fidelity/audit-aozora2html-measurement.py" \
  --run-dir "$other" \
  --out-md "$audit_out/audit-other-failure-only.md" \
  --summary-json "$audit_out/audit-other-failure-only.summary.json" \
  --worksets-dir "$audit_out/other-failure-only-worksets"

jq -e '.bucket_counts.warigaki.report_failed_other_property == 1' "$audit_out/audit-other-failure-only.summary.json"
jq -e '.verdict_inputs.has_policy_relevant_residuals == true' "$audit_out/audit-other-failure-only.summary.json"
jq -e '.verdict_inputs.has_source_feature_without_aat_observation == false' "$audit_out/audit-other-failure-only.summary.json"
