#!/usr/bin/env bash
#
# Harness smoke test for the aozora-epub3 adapter.
#
# Runs the full pipeline (AozoraEpub3 JAR + Rust mapper) on a small fixture
# and asserts the AAT adapter id, parse_complete, and non-empty blocks.
#
# Requires the mapper binary and the pinned upstream release JAR. If
# AB_AOZORAEPUB3_JAR is unset, the smoke resolves it from the local flake.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
adapter="${repo_root}/adapters/aozora-epub3/aozora-epub3-adapter"

if [[ ! -x "$adapter" ]]; then
  echo "adapter not found: $adapter" >&2
  exit 1
fi

out_dir="${AB_TEST_OUT_DIR:-/tmp/ab-validator/aozora-epub3-smoke}"
mkdir -p "$out_dir"

if [[ -z "${AB_AOZORAEPUB3_JAR:-}" ]]; then
  epub3_pkg="$(nix --option post-build-hook "" build --no-link --print-out-paths "$repo_root#reference-aozora-epub3")"
  export AB_AOZORAEPUB3_JAR="$epub3_pkg/lib/AozoraEpub3.jar"
fi

printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
  | "$adapter" --mode aat > "$out_dir/aat.json"

jq -e '.meta.adapter == "aozora-epub3"' "$out_dir/aat.json" >/dev/null
jq -e '.meta.parse_complete == true' "$out_dir/aat.json" >/dev/null
jq -e '.blocks | length >= 1' "$out_dir/aat.json" >/dev/null

echo "aozora-epub3 adapter smoke ok: $out_dir/aat.json"
