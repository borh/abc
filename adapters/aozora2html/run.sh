#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PARSER_DIR="$REPO_ROOT/references/parsers/aozora2html"
ADAPTER_PY="$REPO_ROOT/adapters/aozora2html/adapter.py"

PY_ENV='python3.withPackages(ps: [ ps.lxml ps.jsonschema ])'

if [[ "${1:-}" == "--version" ]]; then
  exec nix-shell -p "$PY_ENV" --run "python3 '$ADAPTER_PY' --version"
fi

mode="aat"
if [[ "${1:-}" == "--mode" && $# -ge 2 ]]; then
  mode="$2"
fi

stdin_raw="$(mktemp --suffix=.raw)"
crlf_src="$(mktemp --suffix=.txt)"
xhtml="$(mktemp --suffix=.html)"
trap 'rm -f "$stdin_raw" "$crlf_src" "$xhtml"' EXIT

cat > "$stdin_raw"

# aozora2html requires Shift_JIS input with CRLF line endings (lib/aozora2html.rb:157
# hardcodes 'rb:Shift_JIS'). Detect input encoding, transcode to Shift_JIS if
# needed, then normalize line endings.
sjis_src="$(mktemp --suffix=.sjis)"
trap 'rm -f "$stdin_raw" "$crlf_src" "$xhtml" "$sjis_src"' EXIT

if head -c 3 "$stdin_raw" | od -An -tx1 | tr -d ' \n' | grep -q '^efbbbf'; then
  tail -c +4 "$stdin_raw" | iconv -f UTF-8 -t CP932 > "$sjis_src"
elif iconv -f UTF-8 -t CP932 "$stdin_raw" > "$sjis_src" 2>/dev/null; then
  : # input was UTF-8, transcoded to Shift_JIS
else
  cp "$stdin_raw" "$sjis_src"  # assume already Shift_JIS
fi

sed -e 's/\r$//' -e 's/$/\r/' "$sjis_src" > "$crlf_src"

nix shell nixpkgs#ruby --command bash -lc \
  "cd '$PARSER_DIR' && \
   RUBYLIB='$PARSER_DIR/lib:$PARSER_DIR/vendor/zip/lib' \
   ./bin/aozora2html --error-utf8 --use-unicode '$crlf_src' '$xhtml'" \
  >&2

# adapter.py reads --source for hash/encoding metadata. Pass the original
# stdin bytes so source_hash matches what other adapters see.
exec nix-shell -p "$PY_ENV" --run \
  "python3 '$ADAPTER_PY' --source '$stdin_raw' --xhtml '$xhtml' --mode '$mode'"
