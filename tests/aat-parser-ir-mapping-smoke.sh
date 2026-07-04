#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aat-parser-ir-mapping-smoke"
aat_dir="${AB_AOZORA_RS_AAT_DIR:-$repo_root/scratch/morph-full-corpus/aats/aozora-rs-adapter}"
abc_root="$repo_root/../abc"

rm -rf "$out_dir"
mkdir -p "$out_dir"

bash "$repo_root/tests/aat-parser-ir-schema-hash-smoke.sh"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.1 \
  --out "$out_dir/aozora-rs-only.mapping.json" \
  --summary-json "$out_dir/aozora-rs-only.summary.json" \
  --assert-zero-unsupported

jq -e '.files_scanned == 17894' "$out_dir/aozora-rs-only.summary.json"
jq -e '.files_with_unsupported == 0' "$out_dir/aozora-rs-only.summary.json"

aozora2html_dir="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
aozora_epub3_dir="${AB_AOZORA_EPUB3_AAT_DIR:-/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter}"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --aat-dir "$aozora2html_dir" \
  --aat-dir "$aozora_epub3_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.1 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.2.1"' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.mapping_schema_hash == "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"' "$out_dir/summary.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].content[].tcy" and .parser_ir_pointer == "emphasis(?)")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].keigakomi_block")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].yokogumi_block")' "$out_dir/mapping.json"
