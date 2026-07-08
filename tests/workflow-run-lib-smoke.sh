#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# shellcheck source=/dev/null
source "$repo_root/scripts/workflow-run-lib.sh"

run_file="$tmp/workflow-run.json"
workflow_init "$run_file" "shell.fixture.v1" "fixture-run"
workflow_step_pass "first" "out/first.txt"
workflow_step_fail "second" "broken"
workflow_finish "failed"

python3 - "$run_file" <<'PY'
import json
import sys

path = sys.argv[1]
data = json.load(open(path, encoding="utf-8"))
assert data["workflow_id"] == "shell.fixture.v1"
assert data["status"] == "failed"
assert data["step_count"] == 2
assert data["steps_passed"] == 1
assert data["steps_failed"] == 1
assert data["steps"][0]["id"] == "first"
assert data["steps"][1]["id"] == "second"
assert data["steps"][1]["status"] == "failed"
assert data["steps"][1]["error"]["message"] == "broken"
PY
