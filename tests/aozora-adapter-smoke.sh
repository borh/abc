#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
adapter="$repo_root/adapters/aozora/target/release/aozora-adapter"
out_dir="${AB_TEST_OUT_DIR:-/tmp/ab-validator/aozora-smoke}"
mkdir -p "$out_dir"

test -x "$adapter"
"$adapter" --version | rg -n '^aozora-adapter 0\.1\.0 aozora '

printf '｜青梅《おうめ》\n' | "$adapter" --mode aat > "$out_dir/aat.json"
jq -e '.meta.adapter == "aozora" and .meta.parse_complete == true and (.blocks | length >= 1)' "$out_dir/aat.json" >/dev/null

python_jsonschema=(python3)
if ! python3 - <<'PY' >/dev/null 2>&1
import jsonschema
PY
then
  if command -v uv >/dev/null 2>&1; then
    python_jsonschema=(uv run --isolated --no-project --with 'jsonschema>=4.0' python3)
  else
    echo "python jsonschema module is required; install it or run through nix" >&2
    exit 1
  fi
fi

"${python_jsonschema[@]}" - "$repo_root/data/aat-schema.json" "$out_dir/aat.json" <<'PY'
import json
import sys
from jsonschema import Draft202012Validator

schema = json.load(open(sys.argv[1], encoding="utf-8"))
data = json.load(open(sys.argv[2], encoding="utf-8"))
Draft202012Validator(schema).validate(data)
PY

echo "aozora adapter smoke ok: $out_dir/aat.json"
