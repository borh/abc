#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

abc_root="$(smoke_abc_root)"
aat_path="$repo_root/tests/fixtures/aat-parser-ir/orthographic-publication.aat.json"

if [[ ! -f "${AB_VIBRATO_DICT:-}" ]]; then
  echo "AB_VIBRATO_DICT must point at a Unidic CWJ vibrato dictionary" >&2
  exit 2
fi

out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-ortho-publication.XXXXXX")"
ortho="$out_dir/ortho.json"
parser_ir="$out_dir/parser-ir.json"
divergence="$out_dir/divergence.json"
publication_dir="$out_dir/publication"
dict_path="$AB_VIBRATO_DICT"

if [[ "$dict_path" == *.zst ]]; then
  if ! command -v zstd >/dev/null 2>&1; then
    echo "zstd is required when AB_VIBRATO_DICT points to a .zst dictionary" >&2
    exit 2
  fi
  dict_copy_dir="$out_dir/vibrato"
  mkdir -p "$dict_copy_dir"
  dict_path="$dict_copy_dir/${dict_path##*/}"
  dict_path="${dict_path%.zst}"
  zstd -dc "$AB_VIBRATO_DICT" > "$dict_path"
fi

export AB_VIBRATO_DICT="$dict_path"
export XDG_CACHE_HOME="$out_dir/xdg-cache"
mkdir -p "$XDG_CACHE_HOME"

if [[ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]]; then
  converter=("$AB_AAT_TO_PARSER_IR_BIN")
else
  converter=(cargo run --quiet --manifest-path "$repo_root/Cargo.toml" -p ab-aat-to-parser-ir --)
fi

"${converter[@]}" detect-ortho-annotations \
  --aat "$aat_path" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --ortho-annotations-out "$ortho" \
  --abc-root "$abc_root"

jq -e '.detector_id == "HeuristicV1"' "$ortho" >/dev/null
jq -e '.coordinate_system == "parser_text_utf8"' "$ortho" >/dev/null
jq -e '(.annotations | length) >= 1' "$ortho" >/dev/null

"${converter[@]}" convert \
  --aat "$aat_path" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --ortho-annotations "$ortho" \
  --parser-ir-out "$parser_ir" \
  --divergence-out "$divergence" \
  --abc-root "$abc_root"

jq -e '.orthographic_annotations.detector_id == "HeuristicV1"' "$parser_ir" >/dev/null
jq -e 'any(.sentences[]; (.tags | index("orthographic-katakana")))' "$parser_ir" >/dev/null
jq -e 'all(.sentences[]; .span.coordinate_system == "parser_text_utf8")' "$parser_ir" >/dev/null

(cd "$abc_root" && clojure -M - "$parser_ir" "$publication_dir" <<'CLJ'
(require '[abc.tools.materialize-publication :as publication])
(let [[parser-ir output] *command-line-args*]
  (publication/materialize-publication!
   {:parser-ir-path parser-ir
    :metadata-record-path "examples/v0/example-work/metadata-record.json"
    :persons-dir "examples/v0/example-persons"
    :output-dir output
    :source-manifest-path "examples/v0/example-work/source.manifest.json"
    :generated-at "2026-07-07T00:00:00Z"}))
CLJ
)

jq -e '.status == "passed" and (.findings | length) == 0' "$publication_dir/tei-validation-result.json" >/dev/null
rg -n '<[A-Za-z0-9_-]+:normalization method="markup"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:s[^>]* type="orthographic-katakana"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:ruby type="furigana"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:rt>めいしょう</[A-Za-z0-9_-]+:rt>' "$publication_dir/tei.xml" >/dev/null

echo "parser-IR orthographic publication smoke ok: $out_dir"
