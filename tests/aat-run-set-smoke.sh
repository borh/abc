#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

unset AB_AOZORA_AAT_DIR

# Derive fixture coordinates from the lock so input updates do not stale the
# positive case. The negative case independently checks revision mismatches.
aozora_input="aozorabunko-src"
read -r aozora_rev aozora_nar_hash < <(
  python3 - "$repo_root/flake.lock" "$aozora_input" <<'PY'
import json
import sys

lock = json.loads(open(sys.argv[1], encoding="utf-8").read())
locked = lock["nodes"][sys.argv[2]]["locked"]
print(locked["rev"], locked["narHash"])
PY
)

mkdir -p "$tmp/aozora/aat/aozora-adapter" "$tmp/override/aat/aozora-adapter"
printf '{"blocks":[]}\n' > "$tmp/aozora/aat/aozora-adapter/000001_1-test.json"
printf '{"blocks":[]}\n' > "$tmp/override/aat/aozora-adapter/000001_1-test.json"

cat > "$tmp/aozora/metadata.json" <<JSON
{
  "schema_version": 1,
  "run_id": "aozora-smoke",
  "adapter_version": "aozora-adapter 0.1.0 aozora 0.4.1",
  "aat_dir": "$tmp/aozora/aat/aozora-adapter",
  "source": {
    "flake_input": "$aozora_input",
    "owner": "aozorabunko",
    "repo": "aozorabunko",
    "rev": "$aozora_rev",
    "narHash": "$aozora_nar_hash"
  }
}
JSON

cat > "$tmp/run-set.json" <<JSON
{
  "schema_version": 1,
  "run_set_id": "smoke",
  "adapters": {
    "aozora": {
      "aat_dir": "$tmp/aozora/aat/aozora-adapter",
      "aat_dir_env": "AB_AOZORA_AAT_DIR",
      "run_descriptor": "$tmp/aozora/metadata.json",
      "expected": {
        "adapter_id": "aozora",
        "adapter_version_contains": "aozora-adapter",
        "source": {
          "flake_input": "$aozora_input",
          "rev": "$aozora_rev",
          "narHash": "$aozora_nar_hash"
        }
      }
    }
  }
}
JSON

PYTHONPATH="$repo_root/ab-validator/reports/lib" \
python3 - "$repo_root" "$tmp/run-set.json" "$tmp/override/aat/aozora-adapter" <<'PY'
import json
import os
import pathlib
import sys

from aat_runs import adapter_aat_globs, load_run_set, validate_run_set

repo_root = pathlib.Path(sys.argv[1])
run_set_path = pathlib.Path(sys.argv[2])
override_dir = pathlib.Path(sys.argv[3])

run_set = load_run_set(run_set_path)
assert adapter_aat_globs(run_set)["aozora"].endswith("/aozora/aat/aozora-adapter/*.json")
clean_errors = validate_run_set(run_set, repo_root=repo_root, require_paths=True)
assert clean_errors == [], clean_errors

# The manifest selects the dump even when an ambient directory disagrees.
os.environ["AB_AOZORA_AAT_DIR"] = str(override_dir)
assert adapter_aat_globs(run_set)["aozora"].endswith("/aozora/aat/aozora-adapter/*.json")

bad = json.loads(json.dumps(run_set))
bad["adapters"]["aozora"]["expected"]["source"]["rev"] = "deadbeef"
errors = validate_run_set(bad, repo_root=repo_root, require_paths=True)
assert any("flake.lock" in error and "deadbeef" in error for error in errors), errors
PY

PYTHONPATH="$repo_root/ab-validator/reports/lib" \
python3 "$repo_root/ab-validator/reports/aat-fidelity/validate-aat-run-set.py" \
  "$tmp/run-set.json" \
  --repo-root "$repo_root" \
  --require-paths \
  > "$tmp/validation.json"

python3 - "$tmp/validation.json" <<'PY'
import json
import sys

report = json.load(open(sys.argv[1]))
assert report["status"] == "passed", report
assert report["error_count"] == 0, report
PY

echo "aat run set smoke ok"
