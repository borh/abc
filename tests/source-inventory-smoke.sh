#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${TMPDIR:-/tmp}/ab-validator-source-inventory-smoke"
rm -rf "$out_dir"
mkdir -p "$out_dir/corpus/cards/000001/files" "$out_dir/corpus/cards/000002/files"
if [[ -n "${AB_SOURCE_INVENTORY_BIN:-}" ]]; then
  inventory_cmd=("$AB_SOURCE_INVENTORY_BIN")
else
  inventory_cmd=(cargo run -p ab-coverage --bin ab-source-inventory --)
fi

cat > "$out_dir/corpus/cards/000001/files/1.txt" <<'TXT'
吾輩《わがはい》
※［＃「口＋世」、U+546D］
［＃ここから横組み］
TXT

cat > "$out_dir/corpus/cards/000002/files/2.txt" <<'TXT'
［＃fixture-unreviewed-marker］
TXT

cat > "$out_dir/index.json" <<'JSON'
{
  "works": [
    {"id": "000001_1", "txt_path": "cards/000001/files/1.txt"},
    {"id": "000002_2", "txt_path": "cards/000002/files/2.txt"}
  ]
}
JSON

"${inventory_cmd[@]}" \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/index.json" \
  --corpus "$out_dir/corpus" \
  --output-json "$out_dir/source-inventory.json" \
  --report-md "$out_dir/source-inventory.md" \
  --unknown-workset "$out_dir/unknown-workset.json"

jq -e '.works_scanned == 2' "$out_dir/source-inventory.json"
jq -e '.markers_total == 4' "$out_dir/source-inventory.json"
jq -e '.unknown_markers_total == 1' "$out_dir/source-inventory.json"
jq -e '.unallowlisted_unknown_markers_total == 1' "$out_dir/source-inventory.json"
jq -e '.allowlisted_unknown_markers_total == 0' "$out_dir/source-inventory.json"
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED"' "$out_dir/source-inventory.json"
jq -e '.unknown_classes_total == 1' "$out_dir/source-inventory.json"
jq -e '.unknown_classes_truncated == false' "$out_dir/source-inventory.json"
jq -e 'any(.unknown_examples[]; .kind == "CommandFullwidth" and .raw == "［＃fixture-unreviewed-marker］")' "$out_dir/source-inventory.json"
jq -e 'any(.unknown_classes[]; .kind == "CommandFullwidth" and .raw == "［＃fixture-unreviewed-marker］" and .unallowlisted_occurrences == 1)' "$out_dir/source-inventory.json"
jq -e '.rows["ruby.basic"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '.rows["gaiji.marker"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '.rows["layout.yokogumi"].occurrences >= 1' "$out_dir/source-inventory.json"
jq -e '. == ["000002_2"]' "$out_dir/unknown-workset.json"
rg -n "SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED|Unknown Source Marker Classes|Showing 1 report rows of 1 total classes|CommandFullwidth" "$out_dir/source-inventory.md"

if RUST_BACKTRACE=0 "${inventory_cmd[@]}" \
  --matrix "$repo_root/data/aozora-syntax-coverage.toml" \
  --index "$out_dir/index.json" \
  --corpus "$out_dir/corpus" \
  --output-json "$out_dir/fail.json" \
  --report-md "$out_dir/fail.md" \
  --fail-on-unknown
then
  echo "expected --fail-on-unknown to fail" >&2
  exit 1
fi
