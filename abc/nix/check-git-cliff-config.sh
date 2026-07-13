#!/usr/bin/env bash
set -euo pipefail
repo="$(mktemp -d)"
trap 'rm -rf "$repo"' EXIT
cp "$1" "$repo/cliff.toml"
git -C "$repo" init -q
git -C "$repo" config user.email cliff@example.invalid
git -C "$repo" config user.name 'Git Cliff Fixture'
touch "$repo/fixture"
git -C "$repo" add fixture cliff.toml
git -C "$repo" commit -q -m 'feat: fixture'
git -C "$repo" cliff --config cliff.toml --unreleased --strip header \
  --output "$repo/changelog.md"
test -s "$repo/changelog.md"
