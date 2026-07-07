#!/usr/bin/env bash
set -euo pipefail

repo_root="${1:-$(git rev-parse --show-toplevel)}"
expected_version="${AB_EXPECTED_TEI_P5_VERSION:-4.11.0}"
expected_tag="P5_Release_${expected_version}"

if [ -n "${AB_TEI_P5_ROOT:-}" ]; then
  tei_root="$AB_TEI_P5_ROOT"
else
  tei_root="$(nix build --no-link --print-out-paths "$repo_root#tei-p5-reference")"
fi

actual_version="$(cat "$tei_root/VERSION")"
if [ "$actual_version" != "$expected_version" ]; then
  echo "TEI P5 reference version mismatch: expected $expected_version, got $actual_version" >&2
  exit 1
fi

grep -q "github:TEIC/TEI/${expected_tag}" "$repo_root/flake.nix"
grep -q "teiP5Version = \"${expected_version}\";" "$repo_root/abc/nix/tei-profile-artifacts.nix"
grep -q "Vault/P5/\${teiP5Version}/xml/tei/custom/schema/relaxng/tei_all.rng" "$repo_root/abc/nix/tei-profile-artifacts.nix"
grep -q "Vault/P5/\${teiP5Version}/xml/tei/odd/p5subset.xml" "$repo_root/abc/nix/tei-profile-artifacts.nix"

test -f "$tei_root/Source/Specs/ruby.xml"
test -f "$tei_root/Source/Specs/hi.xml"
test -f "$tei_root/Source/Guidelines/en/HD-Header.xml"

printf 'TEI P5 coherence passed: %s (%s)\n' "$actual_version" "$expected_tag"
