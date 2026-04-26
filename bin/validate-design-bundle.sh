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

parser_ir_paths = [
    "examples/v0/example-work/parser-ir.json",
    "examples/ab-validator-output/parser-ir.json",
]

for path in parser_ir_paths:
    parser_ir_validator.validate(load_json(path))

diagnostic_schema = {
    "$schema": "https://json-schema.org/draft/2020-12/schema",
    "$defs": parser_ir_schema["$defs"],
    "$ref": "#/$defs/diagnostic",
}
Draft202012Validator.check_schema(diagnostic_schema)
diagnostic_validator = Draft202012Validator(diagnostic_schema)

for path in [
    "examples/v0/example-work/warnings.jsonl",
    "examples/ab-validator-output/warnings.jsonl",
]:
    lines = Path(path).read_text(encoding="utf-8").splitlines()
    if not lines:
        raise SystemExit(f"{path} must contain at least one diagnostic")
    for line_number, line in enumerate(lines, start=1):
        diagnostic_validator.validate(json.loads(line))

summary_events = [
    json.loads(line)
    for line in Path("examples/ab-validator-output/run-summary.jsonl").read_text(encoding="utf-8").splitlines()
    if line.strip()
]

event_types = [event.get("event") for event in summary_events]
if event_types[:1] != ["run-start"]:
    raise SystemExit("ab-validator run summary must start with run-start")
if event_types[-1:] != ["run-complete"]:
    raise SystemExit("ab-validator run summary must end with run-complete")
if "work-result" not in event_types:
    raise SystemExit("ab-validator run summary must include a work-result event")
for event in summary_events:
    if "run_id" not in event:
        raise SystemExit(f"run summary event is missing run_id: {event}")

manifest_inputs = load_json("examples/ab-validator-output/manifest-inputs.json")
required_manifest_input_keys = {
    "producer",
    "producer_version",
    "work_id",
    "work_content_hash",
    "parser_build_hash",
    "parser_config_hash",
    "parser_ir_schema_hash",
    "warning_sidecar_hash",
    "run_summary_hash",
    "comparison_report_hash",
}
missing_manifest_input_keys = required_manifest_input_keys - manifest_inputs.keys()
if missing_manifest_input_keys:
    raise SystemExit(
        "ab-validator manifest inputs missing keys: "
        + ", ".join(sorted(missing_manifest_input_keys))
    )

hash_validator = Draft202012Validator(manifest_schema["$defs"]["hash"])
for key, value in manifest_inputs.items():
    if key.endswith("_hash"):
        hash_validator.validate(value)

comparison_report = load_json("examples/ab-validator-output/comparison-report.json")
if comparison_report.get("report_schema") != "abc.ab-validator-comparison.v0":
    raise SystemExit("ab-validator comparison report has an unexpected report_schema")
if not comparison_report.get("parser_candidates"):
    raise SystemExit("ab-validator comparison report must list parser_candidates")

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
