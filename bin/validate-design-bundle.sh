#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

echo "==> Validating JSON schemas and examples"
python - <<'PY'
import json
from pathlib import Path
from jsonschema import Draft202012Validator


def load_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


manifest_schema = load_json("schemas/manifest.schema.json")
parser_ir_schema = load_json("schemas/parser-ir.schema.json")

Draft202012Validator.check_schema(manifest_schema)
Draft202012Validator.check_schema(parser_ir_schema)

manifest_validator = Draft202012Validator(manifest_schema)
parser_ir_validator = Draft202012Validator(parser_ir_schema)

manifest_paths = [
    "examples/v0/example-work/source.manifest.json",
    "examples/v0/example-work/manifest.json",
    "examples/v0/example-work/failure-manifest.example.json",
]

for path in manifest_paths:
    manifest_validator.validate(load_json(path))

parser_ir_validator.validate(load_json("examples/v0/example-work/parser-ir.json"))

try:
    manifest_validator.validate({})
except Exception:
    pass
else:
    raise SystemExit("manifest schema accepted an empty object")

print("json schema validation ok")
PY

echo "==> Checking canonicalization fixtures"
expected_identity_hash="9d49ff018a43ac2b24323276424cc325e3a5d0a22716144c8800f9fec0911f0a"
actual_identity_hash="$(sha256sum fixtures/canonicalization/manifest-identity-object.canonical.json | awk '{print $1}')"

if [[ "${actual_identity_hash}" != "${expected_identity_hash}" ]]; then
  echo "canonical identity fixture hash mismatch" >&2
  echo "expected: ${expected_identity_hash}" >&2
  echo "actual:   ${actual_identity_hash}" >&2
  exit 1
fi

array_hash_a="$(sha256sum fixtures/canonicalization/array-ordering-negative-a.json | awk '{print $1}')"
array_hash_b="$(sha256sum fixtures/canonicalization/array-ordering-negative-b.json | awk '{print $1}')"

if [[ "${array_hash_a}" == "${array_hash_b}" ]]; then
  echo "array-ordering negative fixtures produced the same digest" >&2
  exit 1
fi

echo "canonicalization fixtures ok"

echo "==> Checking XML fixtures"
xmllint --noout schemas/tei-profile.odd examples/v0/example-work/tei.xml
echo "xml fixtures ok"

echo "==> Checking git-cliff configuration"
git-cliff --config cliff.toml --unreleased --strip header --output /tmp/abc-changelog-check.md
echo "git-cliff config ok"

echo "design bundle validation ok"
