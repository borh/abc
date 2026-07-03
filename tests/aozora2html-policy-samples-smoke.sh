#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-policy-samples-smoke"
run="$out/run"
retry="$out/retry"
audit="$out/audit"
samples="$out/samples"

rm -rf "$out"
mkdir -p \
  "$run/check-reports/fixture-adapter" \
  "$run/aat/fixture-adapter" \
  "$retry/check-reports/fixture-adapter" \
  "$retry/aat/fixture-adapter" \
  "$audit" \
  "$samples"

cat > "$run/metadata.json" <<'JSON'
{
  "report_id": "policy-samples-smoke",
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0"
}
JSON

cat > "$retry/metadata.json" <<'JSON'
{
  "report_id": "policy-samples-smoke-retry",
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0"
}
JSON

cat > "$run/index.json" <<'JSON'
{
  "version": 1,
  "corpus_root": "fixture",
  "corpus_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "generated_at": "2026-07-03T00:00:00Z",
  "works_count": 7,
  "works": [],
  "by_feature": {
    "warigaki": ["work-war-ok", "work-war-missing", "work-war-timeout"],
    "kaeriten": ["work-kun-ok", "work-kun-timeout", "work-kun-retry-ok"],
    "okurigana": ["work-kun-ruby"]
  }
}
JSON

write_report_to() {
  local target_run="$1"
  local work_id="$2"
  local results="$3"
  cat > "$target_run/check-reports/fixture-adapter/$work_id.json" <<JSON
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "$work_id",
  "results": $results
}
JSON
}

write_report() {
  local work_id="$1"
  local results="$2"
  write_report_to "$run" "$work_id" "$results"
}

pass_results='{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"}
}'
timeout_results='{
  "adapter_timeout": {"pass": false, "message": "adapter timed out", "confidence": "strict"}
}'

write_report work-war-ok "$pass_results"
write_report work-war-missing "$pass_results"
write_report work-war-timeout "$timeout_results"
write_report work-kun-ok "$pass_results"
write_report work-kun-timeout "$timeout_results"
write_report work-kun-retry-ok "$timeout_results"
write_report work-kun-ruby "$pass_results"
write_report work-war-ok-shadow '{
  "poison_prefix_failure": {"pass": false, "message": "must not be attributed to work-war-ok", "confidence": "strict"}
}'
write_report_to "$retry" work-war-timeout "$pass_results"
write_report_to "$retry" work-kun-timeout "$timeout_results"
write_report_to "$retry" work-kun-retry-ok "$pass_results"

cat > "$run/aat/fixture-adapter/work-war-ok.json" <<'JSON'
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

cat > "$run/aat/fixture-adapter/work-war-missing.json" <<'JSON'
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

cat > "$run/aat/fixture-adapter/work-kun-ok.json" <<'JSON'
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

cat > "$run/aat/fixture-adapter/work-kun-ruby.json" <<'JSON'
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

cat > "$retry/aat/fixture-adapter/work-war-timeout.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-war-timeout",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "warigaki",
          "upper": [{"kind": "text", "value": "再"}],
          "lower": [{"kind": "text", "value": "試"}]
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

cat > "$retry/aat/fixture-adapter/work-kun-retry-ok.json" <<'JSON'
{
  "version": 1,
  "work_id": "work-kun-retry-ok",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {
          "kind": "style",
          "style_type": "okurigana",
          "content": [{"kind": "text", "value": "リ"}]
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
        "kunten.okurigana": [
          {"kind": "okurigana", "provenance": "source-derived", "value": "リ"}
        ]
      }
    }
  }
}
JSON

duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$run/check-reports" \
  --aat-dir "$run/aat" \
  --db "$run/fidelity.duckdb" \
  --report-id policy-samples-smoke \
  --out-dir "$run/triage"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$retry/check-reports" \
  --aat-dir "$retry/aat" \
  --db "$retry/fidelity.duckdb" \
  --report-id policy-samples-smoke-retry \
  --out-dir "$retry/triage"

python3 "$repo_root/reports/aat-fidelity/audit-aozora2html-measurement.py" \
  --run-dir "$run" \
  --retry-run-dir "$retry" \
  --out-md "$audit/audit.md" \
  --summary-json "$audit/audit.summary.json" \
  --worksets-dir "$audit/worksets"

python3 "$repo_root/reports/aat-fidelity/extract-aozora2html-policy-samples.py" \
  --run-dir "$run" \
  --audit-md "$audit/audit.md" \
  --audit-summary-json "$audit/audit.summary.json" \
  --out-md "$samples/policy-samples.md" \
  --summary-json "$samples/policy-samples.summary.json" \
  --limit-per-bucket 10

rg -n "Observed warigaki" "$samples/policy-samples.md"
rg -n "Observed kunten" "$samples/policy-samples.md"
rg -n "Warigaki source feature without AAT observation" "$samples/policy-samples.md"
rg -n "Kunten source feature without AAT observation" "$samples/policy-samples.md"
rg -n "adapter_timeout_or_protocol_error" "$samples/policy-samples.md"
rg -n "work-war-ok" "$samples/policy-samples.md"
rg -n "work-war-timeout" "$samples/policy-samples.md"
rg -n "work-kun-ok" "$samples/policy-samples.md"
rg -n "work-kun-ruby" "$samples/policy-samples.md"
rg -n '"x-annotation-type": "okurigana"' "$samples/policy-samples.md"
rg -n "work-kun-retry-ok" "$samples/policy-samples.md"
rg -n "work-war-missing" "$samples/policy-samples.md"
rg -n "work-kun-timeout" "$samples/policy-samples.md"
rg -n -F "work-war-timeout | retry" "$samples/policy-samples.md"
rg -n -F "work-kun-retry-ok | retry" "$samples/policy-samples.md"
if rg -n "poison_prefix_failure" "$samples/policy-samples.md"; then
  echo "prefix-matched shadow report polluted work-war-ok sample" >&2
  exit 1
fi
jq -e '.limit_per_bucket == 10 and (.sample_counts | type == "object")' "$samples/policy-samples.summary.json"
jq -e '.retry_run_dir == "'$retry'"' "$samples/policy-samples.summary.json"
jq -e '.sample_counts.warigaki.observed_in_aat == 2' "$samples/policy-samples.summary.json"
jq -e '.sample_counts.warigaki.source_feature_without_aat_observation == 1' "$samples/policy-samples.summary.json"
jq -e '.sample_counts.kunten.observed_in_aat == 3' "$samples/policy-samples.summary.json"
jq -e '.sample_counts.kunten.adapter_timeout_or_protocol_error == 1' "$samples/policy-samples.summary.json"
