#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/aat-parser-ir-mapping-smoke"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

# The normal lane audits the pinned five-adapter corpus. Nix checks cannot
# depend on machine-local corpus paths, so their hermetic lane exercises the
# same generator contract through the policy fixture and audits both committed
# protocol artifacts.
if [[ "${AB_MAPPING_SMOKE_HERMETIC:-0}" == "1" ]]; then
  bash "$repo_root/tests/aat-parser-ir-schema-hash-smoke.sh"
  bash "$repo_root/tests/aat-parser-ir-mapping-policy-smoke.sh"
  jq -e '.mapping_version == "0.4.0"' \
    "$repo_root/data/aat-to-parser-ir-mapping-v1.json"
  jq -e '.mapping_version == "0.5.0"' \
    "$repo_root/data/aat-to-parser-ir-mapping-v2.json"
  for mapping in \
    "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
    "$repo_root/data/aat-to-parser-ir-mapping-v2.json"; do
    jq -e '.target_parser_ir_schema_hash == "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"' "$mapping"
    jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "meta.primary_text_hash" and .parser_ir_pointer == "source.primary_text_hash")' "$mapping"
    jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.primary_text_hash")' "$mapping"
    jq -e 'all(.transform_rule_descriptions[]; .parser_ir_pointer != "source.work_content_hash")' "$mapping"
    jq -e 'any(.synthetic_evidence_descriptions[]; .source == "converter_policy" and .parser_ir_pointer == "source.work_content_hash")' "$mapping"
  done
  frozen_v1="$repo_root/data/aat-to-parser-ir-mapping-v1-0.2.8.json"
  jq -e '.mapping_version == "0.2.8"' "$frozen_v1"
  python "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" "$frozen_v1" \
    | rg -F $'\tsha256:952620ced4eb22f9771e6a10c3a1d4d93de604a8c33e360311f82b6e1eafc5b7'
  frozen_v2_0_4="$repo_root/data/aat-to-parser-ir-mapping-v2-0.4.0.json"
  jq -e '.mapping_version == "0.4.0"' "$frozen_v2_0_4"
  python "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" "$frozen_v2_0_4" \
    | rg -F $'\tsha256:cf177bee98af086fe728cbc1942e4f631b26ed5bb55aedc7d91b21f467c41f30'
  exit 0
fi

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
  --mapping-version 0.4.0 \
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
  --mapping-version 0.4.0 \
  --out "$out_dir/mapping.json" \
  --summary-json "$out_dir/summary.json"

jq -e '.mapping_version == "0.4.0"' "$out_dir/mapping.json"
jq -e '(.aat_dirs | length) == 5' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "meta.adapter" and .aat_pointer != "meta.adapter_version")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "LOSS" and .aat_pointer == "meta.parse_complete")' "$out_dir/mapping.json"
jq -e '.mapping_schema_hash == "sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"' "$out_dir/summary.json"
jq -e '.target_parser_ir_schema_hash == "sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"' "$out_dir/summary.json"
jq -e 'all(.transform_rule_descriptions[]; (.category != "STRUCTURAL") or ((.aat_pointer // "") | contains("paragraph") | not))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and (.description | test("warigaki")))' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .parser_ir_pointer == "span")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .aat_pointer == "blocks[].content[].gaiji.description" and .parser_ir_pointer == "gaiji.raw_marker")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "AMBIGUITY" and .aat_pointer == "meta.source_hash" and .parser_ir_pointer == "source.primary_text_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .parser_ir_pointer != "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'any(.synthetic_evidence_descriptions[]; .source == "converter_policy" and .parser_ir_pointer == "source.work_content_hash")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].gaiji.raw_marker" and .aat_pointer != "blocks[].content[].gaiji.unicode")' "$out_dir/mapping.json"
jq -e 'all(.transform_rule_descriptions[]; .aat_pointer != "blocks[].content[].tcy" or .parser_ir_pointer != "emphasis(?)")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].keigakomi_block")' "$out_dir/mapping.json"
jq -e 'any(.transform_rule_descriptions[]; .category == "UNSUPPORTED" and .aat_pointer == "blocks[].yokogumi_block")' "$out_dir/mapping.json"
