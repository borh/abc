#!/usr/bin/env bash
# An adapter override must control both execution and recorded generator identity.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

make_stub() {
  local path="$1"
  local proof="$2"
  cat > "$path" <<EOF
#!/usr/bin/env bash
if [ "\${1:-}" = "--version" ]; then
  echo "stub-ab-aozora $proof"
  exit 0
fi
cat > /dev/null
cat <<JSON
{"version":1,"work_id":"stdin","blocks":[],"meta":{"adapter":"ab-aozora","adapter_version":"stub-ab-aozora $proof","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}
JSON
EOF
  chmod +x "$path"
}

stub="$tmp/stub-ab-aozora"
make_stub "$stub" "9.9.9-LANE-PROOF"

out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/ab-aozora-lane-smoke"
corpus="$out_dir/corpus"
work_ids="$out_dir/work-ids.json"

rm -rf "$out_dir"
mkdir -p "$corpus/cards/000001/files/1_ruby"
printf '吾輩《わがはい》は猫である。\n' > "$corpus/cards/000001/files/1_ruby/test.txt"
printf '["000001_1"]\n' > "$work_ids"

# Test 1: absolute path
"$repo_root/reports/aat-fidelity/run-aat-full.sh" \
  --adapter ab-aozora \
  --adapter-bin "$stub" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run-absolute" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 60s \
  --report-id ab-aozora-lane-smoke-absolute \
  --force

metadata="$out_dir/run-absolute/metadata.json"
test -f "$metadata"

grep -q "LANE-PROOF" "$metadata"

# The stub really executed for the actual work (not merely for --version):
# its distinctive proof string must appear in the emitted aat/*.json.
aat_files=$(find "$out_dir/run-absolute/aat" -name '*.json')
test -n "$aat_files"
grep -rl "LANE-PROOF" "$out_dir/run-absolute/aat" >/dev/null

python - "$metadata" "$stub" <<'PY'
import hashlib
import json
import sys

metadata_path, stub_path = sys.argv[1:]
metadata = json.loads(open(metadata_path, encoding="utf-8").read())
assert metadata["adapter_id"] == "ab-aozora", metadata["adapter_id"]
override = metadata.get("adapter_bin_override")
assert override is not None, "metadata.json missing adapter_bin_override"
assert override["path"] == stub_path, f"path mismatch: {override['path']} != {stub_path}"
expected_sha256 = hashlib.sha256(open(stub_path, "rb").read()).hexdigest()
assert override["sha256"] == expected_sha256, override["sha256"]
assert "LANE-PROOF" in override["version"], override["version"]
assert "LANE-PROOF" in metadata["adapter_version"], metadata["adapter_version"]
PY

# Test 2: relative path
# Change to stub's parent directory and pass relative path
stub_parent="$(dirname "$stub")"
stub_basename="$(basename "$stub")"

(
  cd "$stub_parent"
  "$repo_root/reports/aat-fidelity/run-aat-full.sh" \
    --adapter ab-aozora \
    --adapter-bin "./$stub_basename" \
    --corpus "$corpus" \
    --out-dir "$out_dir/run-relative" \
    --work-ids "$work_ids" \
    --jobs 1 \
    --timeout 60s \
    --report-id ab-aozora-lane-smoke-relative \
    --force
)

metadata="$out_dir/run-relative/metadata.json"
test -f "$metadata"

grep -q "LANE-PROOF" "$metadata"

python - "$metadata" "$stub" <<'PY'
import hashlib
import json
import sys

metadata_path, stub_path = sys.argv[1:]
metadata = json.loads(open(metadata_path, encoding="utf-8").read())
override = metadata.get("adapter_bin_override")
assert override is not None, "metadata.json missing adapter_bin_override"
# Path must be absolute even though passed as relative
assert override["path"].startswith("/"), f"path must be absolute: {override['path']}"
assert override["path"] == stub_path, f"path mismatch: {override['path']} != {stub_path}"
expected_sha256 = hashlib.sha256(open(stub_path, "rb").read()).hexdigest()
assert override["sha256"] == expected_sha256, override["sha256"]
assert "LANE-PROOF" in override["version"], override["version"]
assert "LANE-PROOF" in metadata["adapter_version"], metadata["adapter_version"]
PY

# Test 3: a differing override binary moves recorded identity (a stale dump
# against a NEW override must never be served as fresh) — proves renderer_dir
# standing in for the override's containing directory actually participates
# in input_set_hash, not merely adapter_binary_hash.
stub2="$tmp/stub2-ab-aozora"
make_stub "$stub2" "9.9.9-LANE-PROOF-DIFFERENT"

"$repo_root/reports/aat-fidelity/run-aat-full.sh" \
  --adapter ab-aozora \
  --adapter-bin "$stub2" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run-differing" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 60s \
  --report-id ab-aozora-lane-smoke-differing \
  --force

python - "$out_dir/run-absolute/metadata.json" "$out_dir/run-differing/metadata.json" <<'PY'
import json
import sys

path_a, path_b = sys.argv[1:]
meta_a = json.loads(open(path_a, encoding="utf-8").read())
meta_b = json.loads(open(path_b, encoding="utf-8").read())
assert meta_a["input_set_hash"] != meta_b["input_set_hash"], (
    "a differing --adapter-bin override must change input_set_hash "
    f"(identity did not move): {meta_a['input_set_hash']}"
)
PY

echo "ab-aozora lane smoke: OK"
