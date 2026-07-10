#!/usr/bin/env bash
# Proves the --aozora-bin harness override actually controls execution AND
# recorded identity, not merely that a flag was parsed: a stub binary with a
# distinctive --version must be the one the aozora adapter subprocess spawns,
# and the run's metadata.json must record it two independent ways —
#  1. adapter_version: this comes from the REAL aozora-adapter's own
#     adapter_version(), which shells out to $AB_AOZORA_BIN --version and
#     concatenates its output. This string can only carry OVERRIDE-PROOF if
#     AB_AOZORA_BIN really reached the adapter subprocess.
#  2. aozora_bin_override: the Task-6 explicit-identity contract (path,
#     sha256, --version), recorded verbatim by run-aat-full.sh itself,
#     independent of (1).
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

stub="$tmp/stub-aozora"
cat > "$stub" <<'EOF'
#!/usr/bin/env bash
if [ "${1:-}" = "--version" ]; then
  echo "stub-aozora 9.9.9-OVERRIDE-PROOF"
  exit 0
fi
# Fixed envelope for any `inspect {nodes,diagnostics,gaiji} -` call. The data
# field must be an ARRAY (adapters/aozora/src/lib.rs Envelope<T>.data is a
# Vec<T>), not the object shape in the task brief's illustrative sketch.
cat <<'JSON'
{"schemaVersion":2,"data":[]}
JSON
EOF
chmod +x "$stub"

out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/aozora-bin-override-smoke"
corpus="$out_dir/corpus"
work_ids="$out_dir/work-ids.json"

rm -rf "$out_dir"
mkdir -p "$corpus/cards/000001/files/1_ruby"
printf '吾輩《わがはい》は猫である。\n' > "$corpus/cards/000001/files/1_ruby/test.txt"
printf '["000001_1"]\n' > "$work_ids"

# Test 1: absolute path
"$repo_root/reports/aat-fidelity/run-aozora-aat-full.sh" \
  --aozora-bin "$stub" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run-absolute" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 60s \
  --report-id aozora-bin-override-smoke-absolute \
  --force

metadata="$out_dir/run-absolute/metadata.json"
test -f "$metadata"

grep -q "OVERRIDE-PROOF" "$metadata"

python - "$metadata" "$stub" <<'PY'
import hashlib
import json
import sys

metadata_path, stub_path = sys.argv[1:]
metadata = json.loads(open(metadata_path, encoding="utf-8").read())
override = metadata.get("aozora_bin_override")
assert override is not None, "metadata.json missing aozora_bin_override"
assert override["path"] == stub_path, f"path mismatch: {override['path']} != {stub_path}"
expected_sha256 = hashlib.sha256(open(stub_path, "rb").read()).hexdigest()
assert override["sha256"] == expected_sha256, override["sha256"]
assert "OVERRIDE-PROOF" in override["version"], override["version"]
assert "OVERRIDE-PROOF" in metadata["adapter_version"], metadata["adapter_version"]
PY

# Test 2: relative path
# Change to stub's parent directory and pass relative path
stub_parent="$(dirname "$stub")"
stub_basename="$(basename "$stub")"
mkdir -p "$corpus/cards/000001/files/1_ruby"
printf '吾輩《わがはい》は猫である。\n' > "$corpus/cards/000001/files/1_ruby/test.txt"

(
  cd "$stub_parent"
  "$repo_root/reports/aat-fidelity/run-aozora-aat-full.sh" \
    --aozora-bin "./$stub_basename" \
    --corpus "$corpus" \
    --out-dir "$out_dir/run-relative" \
    --work-ids "$work_ids" \
    --jobs 1 \
    --timeout 60s \
    --report-id aozora-bin-override-smoke-relative \
    --force
)

metadata="$out_dir/run-relative/metadata.json"
test -f "$metadata"

grep -q "OVERRIDE-PROOF" "$metadata"

python - "$metadata" "$stub" <<'PY'
import hashlib
import json
import sys

metadata_path, stub_path = sys.argv[1:]
metadata = json.loads(open(metadata_path, encoding="utf-8").read())
override = metadata.get("aozora_bin_override")
assert override is not None, "metadata.json missing aozora_bin_override"
# Path must be absolute even though passed as relative
assert override["path"].startswith("/"), f"path must be absolute: {override['path']}"
assert override["path"] == stub_path, f"path mismatch: {override['path']} != {stub_path}"
expected_sha256 = hashlib.sha256(open(stub_path, "rb").read()).hexdigest()
assert override["sha256"] == expected_sha256, override["sha256"]
assert "OVERRIDE-PROOF" in override["version"], override["version"]
assert "OVERRIDE-PROOF" in metadata["adapter_version"], metadata["adapter_version"]
PY

echo "override smoke: OK"
