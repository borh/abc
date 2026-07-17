#!/usr/bin/env bash
set -euo pipefail

ab_root="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
abc_root="${2:-$(cd "$ab_root/../abc" && pwd)}"
write_target="${3:-}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

if [[ -z "${AB_AOZORA_BIN:-}" || -z "${AB_AAT_TO_PARSER_IR_BIN:-}" ]]; then
  cargo build --manifest-path "$ab_root/Cargo.toml" \
    -p ab-aozora -p ab-aat-to-parser-ir >/dev/null
  AB_AOZORA_BIN="$ab_root/target/debug/ab-aozora"
  AB_AAT_TO_PARSER_IR_BIN="$ab_root/target/debug/ab-aat-to-parser-ir"
fi

generate() {
  local output="$1"
  local stage="$tmp/stage-$(basename "$output")"
  mkdir -p "$stage"
  local mapping="$ab_root/data/aat-to-parser-ir-mapping-v2.json"
  local policy="$abc_root/data/parser-rq-parser-ir-conformance-policy-v1.json"
  local identity_ref="sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  local plain="$ab_root/crates/ab-aozora-aat/tests/data/plain-ascii.txt"
  local diagnostic="$ab_root/crates/ab-aozora-aat/tests/data/broken-ruby-utf8.txt"

  for spec in "valid:$plain" "no-output:$diagnostic" "invalid:$plain"; do
    local work_id="${spec%%:*}"
    local source="${spec#*:}"
    "$AB_AOZORA_BIN" --mode diagnostics <"$source" >"$stage/$work_id.diagnostics.json"
    "$AB_AOZORA_BIN" --mode aat <"$source" >"$stage/$work_id.aat.json"
    if [[ "$work_id" == "no-output" ]]; then
      printf 'null\n' >"$stage/$work_id.aat.json"
    fi
    "$AB_AAT_TO_PARSER_IR_BIN" qualify \
      --aat "$stage/$work_id.aat.json" \
      --mapping "$mapping" \
      --abc-root "$abc_root" \
      --work-id "$work_id" \
      --qualification-identity-ref "$identity_ref" \
      --policy "$policy" \
      --parser-ir-out "$stage/$work_id.parser-ir.json" \
      --ledger-out "$stage/$work_id.ledger.json" \
      --record-out "$stage/$work_id.record.json"
  done

  python -c 'import json,sys; assert json.load(open(sys.argv[1]))["status"] == "no_output"' \
    "$stage/no-output.record.json"
  test ! -e "$stage/no-output.parser-ir.json"

  python - "$ab_root" "$abc_root" "$stage" "$output" <<'PY'
import hashlib
import json
import pathlib
import re
import shutil
import sys

ab_root, abc_root, stage, output = map(pathlib.Path, sys.argv[1:])
identity_ref = "sha256:" + "a" * 64
work_ids = ["valid", "invalid", "no-output"]
diag_policy = json.loads((abc_root / "data/parser-rq-diagnostic-completeness-policy-v1.json").read_text())
ir_policy = json.loads((abc_root / "data/parser-rq-parser-ir-conformance-policy-v1.json").read_text())

if output.exists():
    shutil.rmtree(output)
store = output / "store"
store.mkdir(parents=True)
manifest_members = []

def canonical(value):
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()

def reference(data):
    return {"sha256": "sha256:" + hashlib.sha256(data).hexdigest(), "bytes": len(data), "media_type": "application/json"}

def publish_bytes(data):
    ref = reference(data)
    digest = ref["sha256"][7:]
    locator = f"sha256/{digest[:2]}/{digest}.json"
    path = store / locator
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(data)
    member = {"locator": locator, "ref": ref}
    manifest_members.append(member)
    return member

def publish(value):
    return publish_bytes(canonical(value))

def fixture_parser_value(path):
    value = json.loads(path.read_text())
    derived = value.get("derived_from", {})
    version = derived.get("aat_adapter_version")
    # Campaign evidence authenticates the real build revision separately. A
    # checked-in drift fixture cannot hash the commit that contains itself.
    if isinstance(version, str):
        derived["aat_adapter_version"] = re.sub(
            r"\(git [0-9a-f]+\)$", "(git fixture-revision)", version
        )
    return value

diag_entries = []
ir_entries = []
diagnostic_count = 0
works_with_diagnostics = 0
valid_outputs = 0
invalid_outputs = 0
no_outputs = 0

for work_id in work_ids:
    raw_diag = (stage / f"{work_id}.diagnostics.json").read_bytes()
    diag_value = json.loads(raw_diag)
    assert diag_value["schemaVersion"] == 3
    emitted = len(diag_value["data"])
    diagnostic_count += emitted
    works_with_diagnostics += int(emitted > 0)
    raw_member = publish_bytes(raw_diag)
    diag_record = {
        "schema_id": "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-work.schema.json",
        "schema_version": "1.0.0",
        "work_id": work_id,
        "qualification_identity_ref": identity_ref,
        "policy_hash": diag_policy["policy_hash"],
        "attempt_disposition": "parsed",
        "raw_diagnostics": raw_member["ref"],
        "status": "complete",
        "emitted_diagnostics": emitted,
        "complete_diagnostics": emitted,
        "vacuous": emitted == 0,
    }
    diag_record_member = publish(diag_record)
    diag_entries.append({"work_id": work_id, "record": diag_record_member["ref"]})

    record = json.loads((stage / f"{work_id}.record.json").read_text())
    if work_id == "no-output":
        assert record["status"] == "no_output"
        assert not (stage / f"{work_id}.parser-ir.json").exists()
        assert not (stage / f"{work_id}.ledger.json").exists()
        no_outputs += 1
    elif work_id == "invalid":
        parser_value = fixture_parser_value(stage / f"{work_id}.parser-ir.json")
        parser_value.pop("schema_id")
        parser_member = publish(parser_value)
        ledger_member = publish({"errors": ["characterization: required schema_id is absent"]})
        record.update({
            "status": "schema_invalid",
            "parser_ir": parser_member["ref"],
            "validation_ledger": ledger_member["ref"],
            "validation_witnesses": ["characterization: required schema_id is absent"],
        })
        invalid_outputs += 1
    else:
        parser_path = stage / f"{work_id}.parser-ir.json"
        assert record["parser_ir"] == reference(parser_path.read_bytes())
        parser_member = publish(fixture_parser_value(parser_path))
        record["parser_ir"] = parser_member["ref"]
        valid_outputs += 1
    record_member = publish(record)
    ir_entries.append({"work_id": work_id, "record": record_member["ref"]})

diag_index = {
    "schema_id": "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-index.schema.json",
    "schema_version": "1.0.0",
    "qualification_identity_ref": identity_ref,
    "policy_hash": diag_policy["policy_hash"],
    "expected_work_ids": work_ids,
    "records": diag_entries,
}
ir_index = {
    "schema_id": "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-index.schema.json",
    "schema_version": "1.0.0",
    "qualification_identity_ref": identity_ref,
    "policy_hash": ir_policy["policy_hash"],
    "expected_work_ids": work_ids,
    "records": ir_entries,
}
diag_aggregate = {
    "schema_id": "https://w3id.org/abc/schemas/parser-rq-diagnostic-completeness-aggregate.schema.json",
    "schema_version": "1.0.0",
    "qualification_identity_ref": identity_ref,
    "policy_hash": diag_policy["policy_hash"],
    "status": "measured",
    "expected_works": 3,
    "work_count": 3,
    "diagnostic_completeness": 1.0,
    "diagnostic_count": diagnostic_count,
    "works_with_diagnostics": works_with_diagnostics,
    "vacuous": diagnostic_count == 0,
}
generated = valid_outputs + invalid_outputs
assert (valid_outputs, invalid_outputs, no_outputs, generated) == (1, 1, 1, 2)
parser_ratio = 0.5
ir_aggregate = {
    "schema_id": "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-aggregate.schema.json",
    "schema_version": "1.0.0",
    "qualification_identity_ref": identity_ref,
    "policy_hash": ir_policy["policy_hash"],
    "status": "measured",
    "expected_works": 3,
    "generated_outputs": generated,
    "schema_valid_outputs": valid_outputs,
    "schema_invalid_outputs": invalid_outputs,
    "no_output_works": no_outputs,
    "parser_ir_schema_validation": parser_ratio,
}
for value in (diag_index, ir_index, diag_aggregate, ir_aggregate):
    publish(value)
measurements = {
    "diagnostic_completeness": {"value": 1.0, "identity_ref": identity_ref,
                                 "details": {"diagnostic_count": diagnostic_count,
                                             "works_with_diagnostics": works_with_diagnostics,
                                             "vacuous": diagnostic_count == 0}},
    "parser_ir_schema_validation": {"value": parser_ratio,
                                     "identity_ref": identity_ref,
                                     "details": {"expected_works": 3,
                                                 "generated_outputs": generated,
                                                 "schema_valid_outputs": valid_outputs,
                                                 "schema_invalid_outputs": invalid_outputs,
                                                 "no_output_works": no_outputs}},
}
publish(measurements)
manifest = {"blobs": sorted(manifest_members, key=lambda member: member["locator"])}
(output / "manifest.json").write_bytes(canonical(manifest))
PY
}

generate "$tmp/first"
generate "$tmp/second"
diff -ru "$tmp/first" "$tmp/second"

if [[ -n "$write_target" ]]; then
  rm -rf "$write_target"
  mkdir -p "$(dirname "$write_target")"
  cp -R "$tmp/first" "$write_target"
else
  diff -ru "$abc_root/test/fixtures/parser-rq/predicate-hardening-capture" "$tmp/first"
fi
