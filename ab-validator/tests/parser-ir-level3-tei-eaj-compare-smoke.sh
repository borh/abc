#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

abc_root="$(smoke_abc_root)"
workset="${AB_TEI_EAJ_WORKSET:-"$abc_root/out/reports/tei-eaj-aozora/tei-eaj-aozora-workset-export.json"}"
default_level3_aat="${AB_AOZORA2HTML_AAT_DIR:-$repo_root/scratch/state/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}/000035_1567-32ff5a089d67.json"
aat_path="${AB_LEVEL3_AAT:-$default_level3_aat}"
tei_eaj_file="${AB_LEVEL3_TEI_EAJ_FILE:-"data/complete/tei_lib_lv4/1567_tei.xml"}"

if [[ ! -d "$abc_root" ]]; then
  echo "missing ABC repo at $abc_root; set AB_ABC_ROOT=/path/to/abc" >&2
  exit 2
fi

if [[ ! -f "$workset" ]]; then
  echo "missing TEI-EAJ workset export at $workset; run nix run .#abc-tei-eaj-aozora-workset-json or set AB_TEI_EAJ_WORKSET=/path/to/workset.json" >&2
  exit 2
fi

if [[ ! -f "$aat_path" ]]; then
  echo "missing AAT input at $aat_path; set AB_LEVEL3_AAT=/path/to/file.aat.json" >&2
  exit 2
fi

if [[ -n "${AB_LEVEL3_EAJ_COMPARE_OUT:-}" ]]; then
  out_dir="$AB_LEVEL3_EAJ_COMPARE_OUT"
  rm -rf "$out_dir"
  mkdir -p "$out_dir"
else
  out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-level3-eaj-compare.XXXXXX")"
fi

parser_ir="$out_dir/parser-ir.json"
divergence="$out_dir/divergence.json"
publication_dir="$out_dir/publication"
summary_json="$out_dir/summary.json"
report_md="$out_dir/report.md"

if [[ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]]; then
  "$AB_AAT_TO_PARSER_IR_BIN" convert \
    --aat "$aat_path" \
    --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
    --parser-ir-out "$parser_ir" \
    --divergence-out "$divergence" \
    --abc-root "$repo_root/data/abc-schemas"
else
  (cd "$repo_root" && cargo run --quiet -p ab-aat-to-parser-ir -- convert \
    --aat "$aat_path" \
    --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
    --parser-ir-out "$parser_ir" \
    --divergence-out "$divergence" \
    --abc-root "$repo_root/data/abc-schemas")
fi

(cd "$abc_root" && clojure -M:abc/materialize-publication \
  "$parser_ir" \
  examples/v0/example-work/metadata-record.json \
  examples/v0/example-persons \
  "$publication_dir" \
  --source-manifest examples/v0/example-work/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z)

python - "$parser_ir" "$publication_dir/tei.xml" "$publication_dir/tei-validation-result.json" "$workset" "$tei_eaj_file" "$summary_json" "$report_md" <<'PY'
import json
import pathlib
import sys
import xml.etree.ElementTree as ET

parser_ir_path = pathlib.Path(sys.argv[1])
generated_tei_path = pathlib.Path(sys.argv[2])
validation_path = pathlib.Path(sys.argv[3])
workset_path = pathlib.Path(sys.argv[4])
tei_eaj_file = sys.argv[5]
summary_path = pathlib.Path(sys.argv[6])
report_path = pathlib.Path(sys.argv[7])

TEI = "{http://www.tei-c.org/ns/1.0}"


def children_with_local(parent, local):
    if parent is None:
        return []
    return [node for node in parent.iter() if node.tag == f"{TEI}{local}"]


def first_descendant(root, local):
    return root.find(f".//{TEI}{local}")


def text_of(node):
    if node is None:
        return ""
    return "".join(node.itertext())


def tei_counts(path):
    root = ET.parse(path).getroot()
    body = first_descendant(root, "body")
    back = first_descendant(root, "back")
    back_source_notes = [
        note
        for note in children_with_local(back, "note")
        if note.attrib.get("type") == "source-attribution"
    ]
    return {
        "path": str(path),
        "body_p_count": len(children_with_local(body, "p")),
        "body_note_count": len(children_with_local(body, "note")),
        "back_source_note_count": len(back_source_notes),
        "body_text": text_of(body),
        "back_text": text_of(back),
    }


parser_ir = json.loads(parser_ir_path.read_text(encoding="utf-8"))
validation = json.loads(validation_path.read_text(encoding="utf-8"))
workset = json.loads(workset_path.read_text(encoding="utf-8"))
tei_root = pathlib.Path(workset["tei_eaj_source"]["root"])
tei_row = next(
    (row for row in workset["files"] if row.get("tei_eaj_file") == tei_eaj_file),
    None,
)
if tei_row is None:
    raise SystemExit(f"TEI-EAJ row not found in workset: {tei_eaj_file}")
tei_eaj_path = tei_root / tei_eaj_file
if not tei_eaj_path.exists():
    raise SystemExit(f"TEI-EAJ XML not found: {tei_eaj_path}")

generated = tei_counts(generated_tei_path)
tei_eaj = tei_counts(tei_eaj_path)
source_note_texts = [
    node.get("text", "")
    for node in parser_ir.get("nodes", [])
    if node.get("type") == "source-note"
]
non_empty_source_note_texts = [text for text in source_note_texts if text]
source_note_in_body = any(
    text in generated["body_text"] for text in non_empty_source_note_texts
)
source_note_in_back = any(
    text in generated["back_text"] for text in non_empty_source_note_texts
)

summary = {
    "work_id": tei_row.get("work_id"),
    "tei_eaj_file": tei_eaj_file,
    "materialization_status": validation.get("status"),
    "parser_ir": {
        "schema_hash": parser_ir.get("schema_hash"),
        "nodes": len(parser_ir.get("nodes", [])),
        "paragraph_count": len(parser_ir.get("paragraphs", [])),
        "source_note_count": len(source_note_texts),
    },
    "generated_tei": {
        "body_p_count": generated["body_p_count"],
        "body_note_count": generated["body_note_count"],
        "back_source_note_count": generated["back_source_note_count"],
    },
    "tei_eaj": {
        "body_p_count": tei_eaj["body_p_count"],
        "body_note_count": tei_eaj["body_note_count"],
        "back_source_note_count": tei_eaj["back_source_note_count"],
        "workset_p_count": tei_row.get("tei_eaj_p_count"),
        "workset_note_count": tei_row.get("tei_eaj_note_count"),
    },
    "deltas": {
        "body_p_count": generated["body_p_count"] - tei_eaj["body_p_count"],
        "body_note_count": generated["body_note_count"] - tei_eaj["body_note_count"],
        "back_source_note_count": generated["back_source_note_count"]
        - tei_eaj["back_source_note_count"],
    },
    "source_note": {
        "texts": source_note_texts,
        "generated_body_excludes_source_note": bool(non_empty_source_note_texts)
        and not source_note_in_body,
        "generated_back_contains_source_note": bool(non_empty_source_note_texts)
        and source_note_in_back,
    },
}
summary_path.write_text(
    json.dumps(summary, ensure_ascii=False, indent=2) + "\n",
    encoding="utf-8",
)

report = [
    "# Generated Parser-IR TEI vs TEI-EAJ Comparison Smoke",
    "",
    f"- work_id: `{summary['work_id']}`",
    f"- TEI-EAJ file: `{tei_eaj_file}`",
    f"- materialization_status: `{summary['materialization_status']}`",
    "",
    "| Surface | generated parser-IR TEI | TEI-EAJ | delta |",
    "|---|---:|---:|---:|",
    f"| body p | {summary['generated_tei']['body_p_count']} | {summary['tei_eaj']['body_p_count']} | {summary['deltas']['body_p_count']} |",
    f"| body note | {summary['generated_tei']['body_note_count']} | {summary['tei_eaj']['body_note_count']} | {summary['deltas']['body_note_count']} |",
    f"| back source-attribution note | {summary['generated_tei']['back_source_note_count']} | {summary['tei_eaj']['back_source_note_count']} | {summary['deltas']['back_source_note_count']} |",
    "",
    "This smoke is intentionally a comparison, not a Level 3 pass/fail claim. A non-zero paragraph delta is evidence for the remaining adapter/rendering-fidelity work.",
    "",
]
report_path.write_text("\n".join(report), encoding="utf-8")
PY

jq -e '.materialization_status == "passed"' "$summary_json" >/dev/null
jq -e '.parser_ir.paragraph_count >= 2' "$summary_json" >/dev/null
jq -e '.parser_ir.source_note_count >= 1' "$summary_json" >/dev/null
jq -e '.generated_tei.body_p_count >= 2' "$summary_json" >/dev/null
jq -e '.generated_tei.back_source_note_count >= 1' "$summary_json" >/dev/null
jq -e '.tei_eaj.body_p_count >= 1' "$summary_json" >/dev/null
jq -e '.source_note.generated_body_excludes_source_note == true' "$summary_json" >/dev/null
jq -e '.source_note.generated_back_contains_source_note == true' "$summary_json" >/dev/null

echo "parser-IR Level 3 TEI-EAJ comparison smoke ok: $out_dir"
