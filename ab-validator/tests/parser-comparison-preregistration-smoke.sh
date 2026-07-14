#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
schema="$repo_root/schemas/parser-comparison-study.schema.json"
contract="$repo_root/docs/studies/aozora-parser-comparison-preregistration.json"
hashes="$repo_root/docs/studies/aozora-parser-comparison-preregistration.sha256"

uv run --isolated --no-project --with 'jsonschema>=4.0' python - "$schema" "$contract" <<'PY'
import json
import sys
from jsonschema import Draft202012Validator

schema = json.load(open(sys.argv[1], encoding="utf-8"))
contract = json.load(open(sys.argv[2], encoding="utf-8"))
Draft202012Validator.check_schema(schema)
Draft202012Validator(schema).validate(contract)

assert {candidate["id"] for candidate in contract["candidates"]} == {
    "aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3",
    "aozora-parser.js", "ab-aozora"
}
assert contract["analysis"]["unqualified_aggregate_score"] == "prohibited"
assert contract["analysis"]["use_case_weighting"]["sensitivity_analysis_required"] is True
assert {axis["id"] for axis in contract["axes"]} == {
    "construct_coverage", "fidelity", "robustness", "diagnostics", "spans",
    "performance", "maintenance", "packaging", "license"
}
PY

(cd "$repo_root" && sha256sum --check "docs/studies/$(basename "$hashes")")
