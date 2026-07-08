#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/corpus/cards"

plan="$tmp/plan.json"
AB_DB_ROOT="$tmp/db" \
  bash "$repo_root/ab-validator/reports/aat-fidelity/run-aat-full.sh" \
  --adapter aozora2html \
  --corpus "$tmp/corpus" \
  --out-dir "$tmp/out" \
  --jobs 3 \
  --timeout 99s \
  --report-id smoke-report \
  --print-plan \
  > "$plan"

python - "$plan" "$repo_root" "$tmp" <<'PY'
import json
import pathlib
import sys

plan = json.loads(pathlib.Path(sys.argv[1]).read_text())
repo_root = pathlib.Path(sys.argv[2])
tmp = pathlib.Path(sys.argv[3])

assert plan["schema_version"] == 1
assert plan["workflow_id"] == "aat-full.materialize.v1"
assert plan["adapter_id"] == "aozora2html"
assert plan["report_id"] == "smoke-report"
assert plan["jobs"] == 3
assert plan["timeout"] == "99s"
assert plan["corpus"] == str((tmp / "corpus").resolve())
assert plan["out_dir"] == str(tmp / "out")
assert plan["adapter"] == str(repo_root / "ab-validator/adapters/aozora2html/aozora2html-adapter")
assert plan["index_path"] == str(tmp / "out/index.json")
assert plan["aat_dir"] == str(tmp / "out/aat")
assert plan["metadata_path"] == str(tmp / "out/metadata.json")
assert plan["workflow_run_path"] == str(tmp / "out/workflow-run.json")
PY

AB_DB_ROOT="$tmp/db" \
  bash "$repo_root/ab-validator/reports/aat-fidelity/run-aozora2html-aat-full.sh" \
  --corpus "$tmp/corpus" \
  --out-dir "$tmp/wrapper-out" \
  --jobs 2 \
  --print-plan \
  > "$tmp/wrapper-plan.json"

python - "$tmp/wrapper-plan.json" <<'PY'
import json
import pathlib
import sys

plan = json.loads(pathlib.Path(sys.argv[1]).read_text())
assert plan["adapter_id"] == "aozora2html"
assert plan["jobs"] == 2
PY

if AB_DB_ROOT="$tmp/db" \
  bash "$repo_root/ab-validator/reports/aat-fidelity/run-aat-full.sh" \
  --adapter missing \
  --corpus "$tmp/corpus" \
  --out-dir "$tmp/bad" \
  --print-plan \
  >/tmp/aat-materialization-bad.out 2>/tmp/aat-materialization-bad.err; then
  echo "unknown adapter unexpectedly succeeded" >&2
  exit 1
fi

echo "aat materialization workflow smoke ok"
