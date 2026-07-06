#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -f "$repo_root/references/unused" && {
  echo "this smoke must use flake-provided reference paths, not local references/" >&2
  exit 1
}

: "${AB_REFERENCE_AOZORA:?AB_REFERENCE_AOZORA is required}"
: "${AB_REFERENCE_AOZORA_NOTATION_SPEC:?AB_REFERENCE_AOZORA_NOTATION_SPEC is required}"

test -x "$AB_REFERENCE_AOZORA/bin/aozora"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/schema/vector.schema.json"
test -d "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/vectors"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/conformance/RUNNER.md"
test -f "$AB_REFERENCE_AOZORA_NOTATION_SPEC/src/grammar/aozora.abnf"

"$AB_REFERENCE_AOZORA/bin/aozora" --version | rg -n '^aozora '
echo "reference aozora metadata smoke ok"
