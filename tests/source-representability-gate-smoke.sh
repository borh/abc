#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${TMPDIR:-/tmp}/ab-validator-source-representability-gate-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir/corpus/pass/cards/000001/files" "$out_dir/corpus/fail/cards/000001/files"
if [[ -n "${AB_SOURCE_INVENTORY_BIN:-}" ]]; then
  inventory_cmd=("$AB_SOURCE_INVENTORY_BIN")
else
  inventory_cmd=(cargo run -p ab-coverage --bin ab-source-inventory --)
fi

cat > "$out_dir/corpus/pass/cards/000001/files/1.txt" <<'TXT'
吾輩《わがはい》
[#fixture raw preserved]
TXT

cat > "$out_dir/pass-index.json" <<'JSON'
{
  "works": [
    {"id": "000001_1", "txt_path": "cards/000001/files/1.txt"}
  ]
}
JSON

cat > "$out_dir/allowlist.toml" <<'TOML'
[[allow]]
id = "fixture-raw-preserved"
kind = "CommandAscii"
body_pattern = "^fixture raw preserved$"
scope = "unsupported_v1"
reason = "Fixture marker exercises raw-preserved strict gate accounting."
evidence = "tests/source-representability-gate-smoke.sh"
TOML

"${inventory_cmd[@]}" \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/pass-index.json" \
  --corpus "$out_dir/corpus/pass" \
  --allowlist "$out_dir/allowlist.toml" \
  --output-json "$out_dir/pass.json" \
  --report-md "$out_dir/pass.md" \
  --strict-representability

jq -e '.representability.typed_occurrences >= 1' "$out_dir/pass.json"
jq -e '.representability.raw_preserved_occurrences >= 1' "$out_dir/pass.json"
jq -e '.allowlisted_unknown_markers_total >= 1' "$out_dir/pass.json"
jq -e '.unallowlisted_unknown_markers_total == 0' "$out_dir/pass.json"
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$out_dir/pass.json"

cat > "$out_dir/corpus/fail/cards/000001/files/1.txt" <<'TXT'
吾輩《わがはい》
[#unreviewed source marker]
TXT

cat > "$out_dir/fail-index.json" <<'JSON'
{
  "works": [
    {"id": "000001_1", "txt_path": "cards/000001/files/1.txt"}
  ]
}
JSON

if RUST_BACKTRACE=0 "${inventory_cmd[@]}" \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/fail-index.json" \
  --corpus "$out_dir/corpus/fail" \
  --allowlist "$out_dir/allowlist.toml" \
  --output-json "$out_dir/fail.json" \
  --report-md "$out_dir/fail.md" \
  --strict-representability
then
  echo "expected --strict-representability to fail on unallowlisted source marker" >&2
  exit 1
fi
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED"' "$out_dir/fail.json"

cat > "$out_dir/decode-fail-index.json" <<'JSON'
{
  "works": [
    {"id": "000001_missing", "txt_path": "cards/000001/files/missing.txt"}
  ]
}
JSON

if RUST_BACKTRACE=0 "${inventory_cmd[@]}" \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/decode-fail-index.json" \
  --corpus "$out_dir/corpus/fail" \
  --allowlist "$out_dir/allowlist.toml" \
  --output-json "$out_dir/decode-fail.json" \
  --report-md "$out_dir/decode-fail.md" \
  --strict-representability
then
  echo "expected --strict-representability to fail on source read/decode failures" >&2
  exit 1
fi
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED"' "$out_dir/decode-fail.json"
jq -e '.works_failed == 1' "$out_dir/decode-fail.json"
jq -e 'any(.strict_errors[]; test("works failed source inventory read/decode"))' "$out_dir/decode-fail.json"
