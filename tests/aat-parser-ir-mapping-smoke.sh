#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-smoke"
git_common_dir="$(git -C "$repo_root" rev-parse --path-format=absolute --git-common-dir)"
repo_storage_root="$(cd "$git_common_dir/.." && pwd)"
aat_dir="${AB_AOZORA_RS_AAT_DIR:-$repo_storage_root/scratch/morph-full-corpus/aats/aozora-rs-adapter}"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

rm -rf "$out_dir"
mkdir -p "$out_dir"

bash "$repo_root/tests/aat-parser-ir-schema-hash-smoke.sh"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.3 \
  --out "$out_dir/aozora-rs-only.mapping.json" \
  --summary-json "$out_dir/aozora-rs-only.summary.json" \
  --assert-zero-unsupported

jq -e '.files_scanned == 17894' "$out_dir/aozora-rs-only.summary.json"
jq -e '.files_with_unsupported == 0' "$out_dir/aozora-rs-only.summary.json"

aozora2html_dir="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
aozora_epub3_dir="${AB_AOZORA_EPUB3_AAT_DIR:-/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter}"
aozora2_dir="${AB_AOZORA2_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter}"
aozora_dir="${AB_AOZORA_AAT_DIR:-/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter}"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --aat-dir "$aozora2html_dir" \
  --aat-dir "$aozora_epub3_dir" \
  --aat-dir "$aozora2_dir" \
  --aat-dir "$aozora_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.3 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.2.3"' "$out_dir/mapping.json"
jq -e '(.aat_dirs | length) == 5' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712"' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; (.category != "STRUCTURAL") or ((.aat_pointer // "") | contains("paragraph") | not))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].content[].tcy" and .parser_ir_pointer == "emphasis(?)")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].keigakomi_block")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].yokogumi_block")' "$out_dir/mapping.json"
