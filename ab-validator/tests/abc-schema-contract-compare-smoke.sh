#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/tests/lib/smoke-env.sh"
repo_root="$AB_VALIDATOR_ROOT"

work_dir="$(smoke_tmp_dir ab-validator-schema-contract-compare)"
trap 'smoke_cleanup "$work_dir"' EXIT

abc_root="$work_dir/abc"
mkdir -p "$abc_root/schemas"
cp "$repo_root"/data/abc-schemas/schemas/*.schema.json "$abc_root/schemas/"

python3 "$repo_root/scripts/schema_contracts.py" \
  --schema-dir "$abc_root/schemas" \
  --manifest "$abc_root/schemas/schema-contracts.json" \
  --path-prefix schemas \
  --write

python3 "$repo_root/scripts/compare_abc_schema_contracts.py" --abc "$abc_root"

python3 - "$abc_root/schemas/schema-contracts.json" <<'PY'
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
value = json.loads(path.read_text(encoding="utf-8"))
value["schemas"][0]["hash"] = "sha256:" + "0" * 64
path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
PY

if python3 "$repo_root/scripts/compare_abc_schema_contracts.py" --abc "$abc_root" 2>"$work_dir/drift.err"; then
  echo "expected schema contract drift failure" >&2
  exit 1
fi

rg -q "schema contract drift" "$work_dir/drift.err"
