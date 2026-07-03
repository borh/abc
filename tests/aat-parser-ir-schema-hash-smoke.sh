#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="$repo_root/../abc"

out="$(python3 "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/c14n.py" \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json" \
  "$abc_root/schemas/parser-ir.schema.json")"

printf '%s\n' "$out"

printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/aat-parser-ir-mapping.schema.json	sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
printf '%s\n' "$out" | rg -F \
  "$abc_root/schemas/parser-ir.schema.json	sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
