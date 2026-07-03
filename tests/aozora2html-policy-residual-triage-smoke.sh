#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-policy-residual-triage-smoke"
run="$out/run"
retry="$out/retry"
triage="$out/triage"

rm -rf "$out"
mkdir -p \
  "$run/check-reports/fixture-adapter" \
  "$run/aat/fixture-adapter" \
  "$retry/check-reports/fixture-adapter" \
  "$retry/aat/fixture-adapter" \
  "$triage/worksets"

cat > "$run/index.json" <<'JSON'
{
  "version": 1,
  "corpus_root": "fixture",
  "corpus_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
  "generated_at": "2026-07-03T00:00:00Z",
  "works_count": 5,
  "works": [
    {
      "id": "work-war-other",
      "txt_path": "cards/000001/files/war_other.zip::war_other.txt",
      "html_path": "cards/000001/files/war_other.html",
      "features": ["warigaki"],
      "feature_lines": {"warigaki": [12, 13, 14, 15, 16, 17, 18, 19, 20]}
    },
    {
      "id": "work-war-missing",
      "txt_path": "cards/000001/files/war_missing.zip::war_missing.txt",
      "html_path": "cards/000001/files/war_missing.html",
      "features": ["warigaki"],
      "feature_lines": {"warigaki": [42]}
    },
    {
      "id": "work-kun-timeout",
      "txt_path": "cards/000002/files/kun_timeout.zip::kun_timeout.txt",
      "html_path": "cards/000002/files/kun_timeout.html",
      "features": ["kaeriten"],
      "feature_lines": {"kaeriten": [7]}
    },
    {
      "id": "work-kun-parse",
      "txt_path": "cards/000002/files/kun_parse.zip::kun_parse.txt",
      "html_path": "cards/000002/files/kun_parse.html",
      "features": ["okurigana"],
      "feature_lines": {"okurigana": [8]}
    },
    {
      "id": "work-kun-missing",
      "txt_path": "cards/000002/files/kun_missing.zip::kun_missing.txt",
      "html_path": "cards/000002/files/kun_missing.html",
      "features": ["kaeriten", "okurigana"],
      "feature_lines": {"kaeriten": [9], "okurigana": [10]}
    }
  ],
  "by_feature": {
    "warigaki": ["work-war-other", "work-war-missing"],
    "kaeriten": ["work-kun-timeout", "work-kun-missing"],
    "okurigana": ["work-kun-parse", "work-kun-missing"]
  }
}
JSON

write_report() {
  local work_id="$1"
  local results="$2"
  cat > "$run/check-reports/fixture-adapter/$work_id.json" <<JSON
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "$work_id",
  "results": $results
}
JSON
}

write_report work-war-other '{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"},
  "visible_text_body_order": {"pass": false, "message": "order mismatch", "confidence": "strict"}
}'
write_report work-war-missing '{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"}
}'
write_report work-kun-timeout '{
  "adapter_timeout": {"pass": false, "message": "timeout", "confidence": "strict"}
}'
write_report work-kun-parse '{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": false, "message": "parse incomplete", "confidence": "strict"}
}'
write_report work-kun-missing '{
  "schema_valid": {"pass": true, "confidence": "strict"},
  "parse_completeness": {"pass": true, "confidence": "strict"}
}'

cat > "$retry/check-reports/fixture-adapter/work-war-missing.json" <<'JSON'
{
  "adapter": "fixture-adapter",
  "adapter_version": "fixture 1.0",
  "work_id": "work-war-missing",
  "results": {
    "schema_valid": {"pass": true, "confidence": "strict"},
    "parse_completeness": {"pass": true, "confidence": "strict"}
  }
}
JSON

cat > "$retry/aat/fixture-adapter/work-war-missing.json" <<'JSON'
{
  "work_id": "work-war-missing",
  "meta": {
    "semantic_summary": {
      "syntax": {
        "ruby.basic": [{"line": 42}]
      }
    }
  },
  "nodes": []
}
JSON

cat > "$triage/audit.summary.json" <<'JSON'
{
  "run_dir": "/fixture/run",
  "report_id": "fixture",
  "source_counts": {
    "warigaki": 2,
    "kaeriten": 2,
    "okurigana": 2,
    "kunten": 3,
    "policy_union": 5
  },
  "bucket_counts": {
    "warigaki": {
      "observed_in_aat": 0,
      "adapter_timeout_or_protocol_error": 0,
      "schema_invalid_or_no_aat": 0,
      "parse_incomplete": 0,
      "report_failed_other_property": 1,
      "source_feature_without_aat_observation": 1
    },
    "kunten": {
      "observed_in_aat": 0,
      "adapter_timeout_or_protocol_error": 1,
      "schema_invalid_or_no_aat": 0,
      "parse_incomplete": 1,
      "report_failed_other_property": 0,
      "source_feature_without_aat_observation": 1
    }
  },
  "buckets": {
    "warigaki": {
      "observed_in_aat": [],
      "adapter_timeout_or_protocol_error": [],
      "schema_invalid_or_no_aat": [],
      "parse_incomplete": [],
      "report_failed_other_property": ["work-war-other"],
      "source_feature_without_aat_observation": ["work-war-missing"]
    },
    "kunten": {
      "observed_in_aat": [],
      "adapter_timeout_or_protocol_error": ["work-kun-timeout"],
      "schema_invalid_or_no_aat": [],
      "parse_incomplete": ["work-kun-parse"],
      "report_failed_other_property": [],
      "source_feature_without_aat_observation": ["work-kun-missing"]
    }
  },
  "verdict_inputs": {
    "has_policy_relevant_residuals": true,
    "has_source_feature_without_aat_observation": true
  },
  "retry": {
    "run_dir": "__RETRY_RUN__",
    "report_id": "fixture-retry",
    "aat_observed_counts": {
      "warigaki_works": 0,
      "warigaki_nodes": 0,
      "kunten_works": 0,
      "kunten_nodes": 0,
      "kunten_semantic_observations": 0,
      "kunten_observations": 0
    }
  }
}
JSON
python3 - "$triage/audit.summary.json" "$retry" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
payload = json.loads(path.read_text())
payload["retry"]["run_dir"] = sys.argv[2]
path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n")
PY

python3 "$repo_root/reports/aat-fidelity/triage-aozora2html-policy-residuals.py" \
  --run-dir "$run" \
  --audit-summary-json "$triage/audit.summary.json" \
  --out-md "$triage/policy-residual-triage.md" \
  --summary-json "$triage/policy-residual-triage.summary.json" \
  --worksets-dir "$triage/worksets" \
  --limit-per-bucket 3

jq -e '.residual_union_count == 5' "$triage/policy-residual-triage.summary.json"
jq -e '.bucket_counts.warigaki.report_failed_other_property == 1' "$triage/policy-residual-triage.summary.json"
jq -e '.bucket_counts.kunten.source_feature_without_aat_observation == 1' "$triage/policy-residual-triage.summary.json"
jq -e '.bucket_evidence.warigaki.report_failed_other_property.failed_properties.visible_text_body_order == 1' "$triage/policy-residual-triage.summary.json"
jq -e '.bucket_evidence.warigaki.source_feature_without_aat_observation.aat_present == 1' "$triage/policy-residual-triage.summary.json"
jq -e '.bucket_evidence.warigaki.source_feature_without_aat_observation.clean_reports == 1' "$triage/policy-residual-triage.summary.json"
jq -e 'type == "array" and length == 5 and all(.[]; type == "string")' "$triage/worksets/policy-residual-union.json"
jq -e 'type == "array" and . == ["work-war-other"]' "$triage/worksets/warigaki-report_failed_other_property.json"
rg -n "work-war-other|visible_text_body_order|warigaki:12" "$triage/policy-residual-triage.md"
rg -n "warigaki:12, warigaki:13, warigaki:14, warigaki:15, warigaki:16, warigaki:17, warigaki:18, warigaki:19 \\(\\+1 more\\)" "$triage/policy-residual-triage.md"
rg -n "work-war-missing \\| retry \\| warigaki:42 \\| .*ruby.basic" "$triage/policy-residual-triage.md"
rg -n "work-kun-missing \\| baseline \\| kunten.kaeriten:9, kunten.okurigana:10" "$triage/policy-residual-triage.md"
rg -n "source_feature_without_aat_observation .*valid AAT without family observation" "$triage/policy-residual-triage.md"
rg -n "retry_run_dir: \`$retry\`" "$triage/policy-residual-triage.md"
rg -n "Next action: adapter-oracle characterization" "$triage/policy-residual-triage.md"
