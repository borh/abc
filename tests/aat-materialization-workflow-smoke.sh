#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/corpus/cards"

# ab-aozora is the sole surviving AAT lane (the third-party comparison lanes
# are retired — ADR third-party-comparison-retirement). --adapter-bin keeps
# the plan hermetic: without it, planning resolves the pinned nix-store
# binary, which a sandboxed check cannot do.
fake_adapter="$tmp/fake-ab-aozora"
printf '#!/usr/bin/env bash\nexit 0\n' > "$fake_adapter"
chmod +x "$fake_adapter"

plan="$tmp/plan.json"
AB_DB_ROOT="$tmp/db" \
  bash "$repo_root/ab-validator/reports/aat-fidelity/run-aat-full.sh" \
  --adapter ab-aozora \
  --adapter-bin "$fake_adapter" \
  --corpus "$tmp/corpus" \
  --out-dir "$tmp/out" \
  --jobs 3 \
  --timeout 99s \
  --report-id smoke-report \
  --print-plan \
  > "$plan"

python - "$plan" "$fake_adapter" "$tmp" <<'PY'
import json
import pathlib
import sys

plan = json.loads(pathlib.Path(sys.argv[1]).read_text())
fake_adapter = sys.argv[2]
tmp = pathlib.Path(sys.argv[3])

assert plan["schema_version"] == 1
assert plan["workflow_id"] == "aat-full.materialize.v1"
assert plan["adapter_id"] == "ab-aozora"
assert plan["report_id"] == "smoke-report"
assert plan["jobs"] == 3
assert plan["timeout"] == "99s"
assert plan["corpus"] == str(tmp / "corpus")
assert plan["out_dir"] == str(tmp / "out")
# Planning defers binary resolution for the native lane: the plan names the
# adapter id, and the pinned binary (or --adapter-bin override) is resolved
# at execution time.
assert plan["adapter"] == "ab-aozora"
assert plan["index_path"] == str(tmp / "out/index.json")
assert plan["aat_dir"] == str(tmp / "out/aat")
assert plan["metadata_path"] == str(tmp / "out/metadata.json")
assert plan["workflow_run_path"] == str(tmp / "out/workflow-run.json")
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
