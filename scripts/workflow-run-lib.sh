#!/usr/bin/env bash

workflow_utc_now() {
  date -u +%Y-%m-%dT%H:%M:%SZ
}

workflow_json_escape() {
  python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"
}

workflow_init() {
  WORKFLOW_RUN_FILE="$1"
  WORKFLOW_ID="$2"
  WORKFLOW_RUN_ID="$3"
  WORKFLOW_STARTED_AT="$(workflow_utc_now)"
  WORKFLOW_STEPS_FILE="$WORKFLOW_RUN_FILE.steps.jsonl"
  mkdir -p "$(dirname "$WORKFLOW_RUN_FILE")"
  : > "$WORKFLOW_STEPS_FILE"
}

workflow_step_pass() {
  local step_id="$1"
  local output_path="$2"
  local now
  now="$(workflow_utc_now)"
  printf '{"id":%s,"status":"passed","started_at":%s,"ended_at":%s,"duration_ms":0,"requires":[],"produces":[],"inputs":[],"outputs":[{"role":"output","path":%s}],"messages":[]}\n' \
    "$(workflow_json_escape "$step_id")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$output_path")" \
    >> "$WORKFLOW_STEPS_FILE"
}

workflow_step_fail() {
  local step_id="$1"
  local message="$2"
  local now
  now="$(workflow_utc_now)"
  printf '{"id":%s,"status":"failed","started_at":%s,"ended_at":%s,"duration_ms":0,"requires":[],"produces":[],"inputs":[],"outputs":[],"messages":[],"error":{"error_class":"shell","message":%s,"data":{}}}\n' \
    "$(workflow_json_escape "$step_id")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$message")" \
    >> "$WORKFLOW_STEPS_FILE"
}

workflow_finish() {
  local status="$1"
  local ended_at
  ended_at="$(workflow_utc_now)"
  python3 - "$WORKFLOW_RUN_FILE" "$WORKFLOW_STEPS_FILE" "$WORKFLOW_ID" "$WORKFLOW_RUN_ID" "$status" "$WORKFLOW_STARTED_AT" "$ended_at" <<'PY'
import json
import sys

run_file, steps_file, workflow_id, run_id, status, started_at, ended_at = sys.argv[1:]
steps = []
with open(steps_file, encoding="utf-8") as handle:
    for line in handle:
        line = line.strip()
        if line:
            steps.append(json.loads(line))
failed = sum(1 for step in steps if step["status"] == "failed")
passed = sum(1 for step in steps if step["status"] == "passed")
run = {
    "schema_id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
    "schema_version": "soranoha-workflow-run-v1",
    "workflow_id": workflow_id,
    "run_id": run_id,
    "status": status,
    "started_at": started_at,
    "ended_at": ended_at,
    "duration_ms": 0,
    "step_count": len(steps),
    "steps_passed": passed,
    "steps_failed": failed,
    "steps": steps,
}
with open(run_file, "w", encoding="utf-8") as handle:
    json.dump(run, handle, ensure_ascii=False, sort_keys=True, indent=2)
    handle.write("\n")
PY
}
