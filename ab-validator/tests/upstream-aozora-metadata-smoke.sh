#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

test -f "$repo_root/local-dependencies/unused" && {
  echo "this smoke must use flake-provided upstream paths, not local dependency checkouts" >&2
  exit 1
}

: "${AB_UPSTREAM_AOZORA:?AB_UPSTREAM_AOZORA is required}"
: "${AB_UPSTREAM_AOZORA_NOTATION_SPEC:?AB_UPSTREAM_AOZORA_NOTATION_SPEC is required}"

test -x "$AB_UPSTREAM_AOZORA/bin/aozora"
test -f "$AB_UPSTREAM_AOZORA_NOTATION_SPEC/conformance/schema/vector.schema.json"
test -d "$AB_UPSTREAM_AOZORA_NOTATION_SPEC/conformance/vectors"
test -f "$AB_UPSTREAM_AOZORA_NOTATION_SPEC/conformance/RUNNER.md"
test -f "$AB_UPSTREAM_AOZORA_NOTATION_SPEC/src/grammar/aozora.abnf"

"$AB_UPSTREAM_AOZORA/bin/aozora" --version | rg -n '^aozora '
echo "upstream aozora metadata smoke ok"
