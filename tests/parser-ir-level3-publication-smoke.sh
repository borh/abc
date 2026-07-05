#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
abc_root="${AB_ABC_ROOT:-"$repo_root/../abc"}"
aat_path="${AB_LEVEL3_AAT:-"$repo_root/tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json"}"
expect_source_note="${AB_LEVEL3_EXPECT_SOURCE_NOTE:-0}"

if [[ ! -d "$abc_root" ]]; then
  echo "missing ABC repo at $abc_root; set AB_ABC_ROOT=/path/to/abc" >&2
  exit 2
fi

if [[ ! -f "$aat_path" ]]; then
  echo "missing AAT fixture at $aat_path; set AB_LEVEL3_AAT=/path/to/file.aat.json" >&2
  exit 2
fi

if [[ -n "${AB_LEVEL3_PUBLICATION_OUT:-}" ]]; then
  out_dir="$AB_LEVEL3_PUBLICATION_OUT"
  rm -rf "$out_dir"
  mkdir -p "$out_dir"
else
  out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-level3-publication.XXXXXX")"
fi

parser_ir="$out_dir/parser-ir.json"
divergence="$out_dir/divergence.json"
publication_dir="$out_dir/publication"

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

jq -e '.schema_hash == "sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913"' "$parser_ir" >/dev/null
jq -e '(.nodes | length) > 0' "$parser_ir" >/dev/null
jq -e '(.paragraphs | length) >= 2' "$parser_ir" >/dev/null
jq -e 'all(.paragraphs[]; (.node_range.start < .node_range.end) and (.span.coordinate_system == "decoded_utf8"))' "$parser_ir" >/dev/null

if [[ "$expect_source_note" == "1" ]]; then
  jq -e 'any(.nodes[]; .type == "source-note" and .placement == "back")' "$parser_ir" >/dev/null
  jq -e 'any(.paragraphs[]; .role == "source-note")' "$parser_ir" >/dev/null
fi

(cd "$abc_root" && clojure -M:abc/materialize-publication \
  "$parser_ir" \
  examples/v0/example-work/metadata-record.json \
  examples/v0/example-persons \
  "$publication_dir" \
  --source-manifest examples/v0/example-work/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z)

test -s "$publication_dir/plain.txt"
test -s "$publication_dir/tei.xml"
test -s "$publication_dir/tei.manifest.json"
test -s "$publication_dir/tei-validation-result.json"

jq -e '.status == "passed" and (.findings | length) == 0' "$publication_dir/tei-validation-result.json" >/dev/null
rg -n '<[A-Za-z0-9_-]+:body(?:\s|>)' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:p(?:\s|>)' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:ruby(?:\s|>)' "$publication_dir/tei.xml" >/dev/null

if [[ "$expect_source_note" == "1" ]]; then
  rg -n '<[A-Za-z0-9_-]+:back(?:\s|>)' "$publication_dir/tei.xml" >/dev/null
  rg -n '<[A-Za-z0-9_-]+:note[^>]+type="source-attribution"' "$publication_dir/tei.xml" >/dev/null
fi

echo "parser-IR Level 3 publication smoke ok: $out_dir"
