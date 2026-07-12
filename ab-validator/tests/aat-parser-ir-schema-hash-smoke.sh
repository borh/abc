#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

out="$(python "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json" \
  "$abc_root/schemas/parser-ir.schema.json")"

printf '%s\n' "$out"

printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json	sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2"
printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/parser-ir.schema.json	sha256:43a6a6d86ca5eca062508e6cae633d19bf5248f15c5bb46153a6d8580ea916ec"
