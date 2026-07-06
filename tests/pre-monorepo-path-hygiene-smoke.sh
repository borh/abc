#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

python3 - "$repo_root" <<'PY'
import json
import pathlib
import sys

repo_root = pathlib.Path(sys.argv[1])
summary_path = repo_root / "docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json"
summary = json.loads(summary_path.read_text(encoding="utf-8"))

bad_values: list[str] = []


def walk(value: object, key_path: tuple[str, ...] = ()) -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            walk(child, key_path + (key,))
        return
    if isinstance(value, list):
        for index, child in enumerate(value):
            walk(child, key_path + (str(index),))
        return
    if not isinstance(value, str):
        return

    field = key_path[-1] if key_path else ""
    dotted = ".".join(key_path)
    if value.startswith("../abc/references/TEI/P5/"):
        bad_values.append(f"{dotted}: legacy TEI reference {value}")
    if value.startswith("/home/bor/Projects/"):
        bad_values.append(f"{dotted}: physical checkout path {value}")
    if field == "path" and value.startswith(("../abc/", "../ab-validator/")):
        bad_values.append(f"{dotted}: sibling checkout identity path {value}")


walk(summary)
if bad_values:
    print("pre-monorepo path hygiene failed:", file=sys.stderr)
    for value in bad_values:
        print(f"- {value}", file=sys.stderr)
    raise SystemExit(1)
PY

if rg -n 'AB_ABC_ROOT:-.*\.\./abc' "$repo_root/tests"; then
  echo "active smoke tests must default through AB_WORKSPACE_ROOT/AB_ABC_ROOT, not repo_root/../abc" >&2
  exit 1
fi

echo "pre-monorepo path hygiene smoke ok"
