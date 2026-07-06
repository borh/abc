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
［＃ここで引用文終わり］
［＃ここで段組、罫囲み終わり］
［＃ここか３字下げ］
［＃ルビは「？符」に掛かる］
［＃］：入力者注　主に外字の説明や、傍点の位置の指定
［＃本文終わり］
［＃
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

[[allow]]
id = "fixture-empty-command-legend"
kind = "CommandFullwidth"
raw_pattern = "^［＃］$"
scope = "front_matter_legend"
reason = "Fixture marker exercises source-apparatus accounting for Aozora notation legend boilerplate."
evidence = "tests/source-representability-gate-smoke.sh"

[[allow]]
id = "fixture-body-end-boundary"
kind = "CommandFullwidth"
raw_pattern = "^［＃本文終わり］$"
scope = "body_end_boundary"
reason = "Fixture marker exercises back-matter boundary accounting."
evidence = "tests/source-representability-gate-smoke.sh"

[[allow]]
id = "fixture-malformed-command"
kind = "MalformedCommand"
raw_pattern = "^［＃$"
scope = "malformed_source"
reason = "Fixture marker exercises malformed-source diagnostics."
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
jq -e '.schema_version == "aozora-source-region-coverage-v1"' "$out_dir/pass.json"
jq -e '.source_region_coverage.body_typed_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.body_raw_preserved_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.source_apparatus_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.front_matter_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.back_matter_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.malformed_source_occurrences >= 1' "$out_dir/pass.json"
jq -e '.source_region_coverage.unsupported_body_markup_occurrences == 0' "$out_dir/pass.json"
jq -e '.source_region_coverage.unknown_region_occurrences == 0' "$out_dir/pass.json"
jq -e '.representability.malformed_noise_occurrences == (.source_region_coverage.source_apparatus_occurrences + .source_region_coverage.malformed_source_occurrences)' "$out_dir/pass.json"
jq -e '.representability.out_of_body_occurrences == .source_region_coverage.back_matter_occurrences' "$out_dir/pass.json"
jq -e '.representability.malformed_noise_occurrences >= 1' "$out_dir/pass.json"
jq -e '.representability.unsupported_occurrences == 0' "$out_dir/pass.json"
jq -e '.allowlisted_unknown_markers_total >= 2' "$out_dir/pass.json"
jq -e '.unallowlisted_unknown_markers_total == 0' "$out_dir/pass.json"
jq -e '.rows["structure.quote_block"].occurrences >= 1' "$out_dir/pass.json"
jq -e '.rows["layout.multicolumn"].occurrences >= 1' "$out_dir/pass.json"
jq -e '.rows["indentation.jisage_block"].occurrences >= 1' "$out_dir/pass.json"
jq -e '.rows["annotation.chuuki"].occurrences >= 1' "$out_dir/pass.json"
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' "$out_dir/pass.json"
rg -n "source-markup authority gate|Semantic TEI enrichment" "$out_dir/pass.md"

cat > "$out_dir/no-tei-projection.toml" <<'TOML'
[[syntax]]
id = "fixture.typed_no_tei"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = ["吾輩《わがはい》"]
source_patterns = ['《[^》]+》']
ir_nodes = []
aat_nodes = ["ruby"]
tei_projection = ""
plaintext_projection = "base_only"
comparison_projection = "base_only"
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed_no_tei"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
evidence = "tests/source-representability-gate-smoke.sh"
notes = "fixture row intentionally omits TEI projection"
TOML

if RUST_BACKTRACE=0 "${inventory_cmd[@]}" \
  --matrix "$out_dir/no-tei-projection.toml" \
  --index "$out_dir/pass-index.json" \
  --corpus "$out_dir/corpus/pass" \
  --allowlist "$out_dir/allowlist.toml" \
  --output-json "$out_dir/no-tei-projection.json" \
  --report-md "$out_dir/no-tei-projection.md" \
  --strict-representability
then
  echo "expected --strict-representability to fail on represented source row without TEI projection" >&2
  exit 1
fi
jq -e 'any(.strict_errors[]; test("tei_projection"))' "$out_dir/no-tei-projection.json"

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
