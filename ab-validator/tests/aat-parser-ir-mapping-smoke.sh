#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/aat-parser-ir-mapping-smoke"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

# Dump selection comes from the fidelity run-set lock (the sole authority), NOT
# ambient AB_*_AAT_DIR env vars — same as the parser-IR audit recipes. Resolve
# once, then read each adapter's pinned dir from the lock. Skip content hashing
# (--no-verify-content); dir existence is still checked so a missing pin fails.
lock="$(mktemp)"
trap 'rm -f "$lock"' EXIT
python3 "$repo_root/reports/aat-fidelity/resolve-run-set.py" \
  --repo-root "$repo_root" --no-verify-content --out "$lock"
mapfile -t aat_dirs < <(python3 "$repo_root/reports/aat-fidelity/lock-aat-dirs.py" \
  "$lock" --order aozora-rs aozora2html aozora-epub3 aozora2 ab-aozora)
aat_dir="${aat_dirs[0]}"

rm -rf "$out_dir"
mkdir -p "$out_dir"

bash "$repo_root/tests/aat-parser-ir-schema-hash-smoke.sh"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aat_dir" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.4 \
  --out "$out_dir/aozora-rs-only.mapping.json" \
  --summary-json "$out_dir/aozora-rs-only.summary.json"

jq -e '.files_scanned == 17886' "$out_dir/aozora-rs-only.summary.json"
# The run-set-pinned aozora-rs dump's ONLY mapping-unsupported construct is
# warigaki (割書) — an intentional drop (asserted UNSUPPORTED below). Assert that
# every unsupported file is a warigaki file (and that the dump exercises it), which
# is a stronger, dump-appropriate check than the former `== 0` (the old scratch
# dump merely happened to contain no warigaki).
jq -e '.files_with_warigaki > 0 and .files_with_unsupported == .files_with_warigaki' \
  "$out_dir/aozora-rs-only.summary.json"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "${aat_dirs[0]}" \
  --aat-dir "${aat_dirs[1]}" \
  --aat-dir "${aat_dirs[2]}" \
  --aat-dir "${aat_dirs[3]}" \
  --aat-dir "${aat_dirs[4]}" \
  --abc-root "$abc_root" \
  --mapping-version 0.2.4 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.2.4"' "$out_dir/mapping.json"
jq -e '(.aat_dirs | length) == 5' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.mapping_schema_hash == "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2"' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; (.category != "STRUCTURAL") or ((.aat_pointer // "") | contains("paragraph") | not))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].tcy" or .parser_ir_pointer != "emphasis(?)")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].keigakomi_block")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].yokogumi_block")' "$out_dir/mapping.json"
