#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"

out="$(python3 "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json" \
  "$abc_root/schemas/parser-ir.schema.json")"

printf '%s\n' "$out"

printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json	sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/parser-ir.schema.json	sha256:90c9c46c1e3048cf2559733d4ee7f3e37827756e2527548ba981f023a1232fa2"
