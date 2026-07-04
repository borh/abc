#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-$repo_root/data/abc-schemas}"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/divergence-bundle-smoke"
bundle="$out_dir/bundle.json"
record="$out_dir/record.json"

rm -rf "$out_dir"
mkdir -p "$out_dir"

cat > "$bundle" <<'JSON'
{
  "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
  "schema_version": "0.1.0",
  "work_id": "fixture",
  "mapping": {
    "mapping_id": "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe",
    "mapping_version": "0.2.3",
    "mapping_schema_hash": "sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4"
  },
  "target": {
    "parser_ir_schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
    "parser_ir_schema_hash": "sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d"
  },
  "aat": {
    "version": 1,
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parse_complete": true
  },
  "preserved_aat_meta": {
    "metrics": null,
    "semantic_summary": null
  },
  "summary": {
    "LOSS": 0,
    "INVENTION": 0,
    "AMBIGUITY": 1,
    "UNSUPPORTED": 0,
    "STRUCTURAL": 0
  },
  "records": [
    {
      "rule_id": "A-06",
      "category": "AMBIGUITY",
      "aat_pointer": "meta.source_hash",
      "parser_ir_pointer": "source.work_content_hash",
      "source_value": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      "target_value": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
      "message": "AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ",
      "count": 1,
      "first_path": "meta.source_hash"
    }
  ]
}
JSON

jq '.records[0]' "$bundle" > "$record"

uv run --isolated --no-project --with 'jsonschema>=4.0' python3 - <<PY
import json
from pathlib import Path
from jsonschema import Draft202012Validator

repo = Path("$repo_root")
abc = Path("$abc_root")
bundle = json.loads(Path("$bundle").read_text())
record = json.loads(Path("$record").read_text())

Draft202012Validator(
    json.loads((repo / "data/aat-parser-ir-divergence-bundle-v1.schema.json").read_text())
).validate(bundle)
Draft202012Validator(
    json.loads((abc / "schemas/aat-parser-ir-divergence.schema.json").read_text())
).validate(record)
PY
