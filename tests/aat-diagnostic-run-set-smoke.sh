#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

for adapter in aozora aozora-rs aozora2html; do
  mkdir -p "$tmp/$adapter"
  cat > "$tmp/$adapter/000001_1-test.json" <<'JSON'
{"blocks":[{"kind":"ruby","text":"x"}]}
JSON
done

cat > "$tmp/run-set.json" <<JSON
{
  "schema_version": 1,
  "run_set_id": "diagnostic-smoke",
  "adapters": {
    "aozora": {"aat_dir": "$tmp/aozora"},
    "aozora-rs": {"aat_dir": "$tmp/aozora-rs"},
    "aozora2html": {"aat_dir": "$tmp/aozora2html"}
  }
}
JSON

out="$tmp/out.txt"
AB_AAT_RUN_SET="$tmp/run-set.json" \
  python "$repo_root/ab-validator/reports/aat-fidelity/ruby-coverage-per-work-diagnostic.py" \
  > "$out"

python - "$out" <<'PY'
import pathlib
import sys

text = pathlib.Path(sys.argv[1]).read_text()
for label in ["aozora", "aozora-rs", "aozora2html"]:
    expected = f"{label}: 1 works, total ruby 1"
    assert expected in text, text
assert "shared works (a2html & rs): 1" in text, text
PY

echo "aat diagnostic run-set smoke ok"
