#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/parse-incomplete-classifier-smoke"
aat_dir="$out_dir/aat"
report="$out_dir/report.md"

rm -rf "$out_dir"
mkdir -p "$aat_dir"

cat > "$aat_dir/ruby_struct.json" <<'JSON'
{ "work_id": "ws", "version": 1, "meta": { "parse_complete": false, "warnings": [ {"message": "エラー(123行目): 構文エラー"} ] }, "blocks": [] }
JSON
cat > "$aat_dir/xhtml.json" <<'JSON'
{ "work_id": "wx", "version": 1, "meta": { "parse_complete": false, "warnings": [ {"message": "invalid XHTML: <br> not closed"} ] }, "blocks": [] }
JSON
cat > "$aat_dir/complete.json" <<'JSON'
{ "work_id": "wc", "version": 1, "meta": { "parse_complete": true, "warnings": [] }, "blocks": [] }
JSON

python "$repo_root/reports/aat-fidelity/aozora2html-parse-incomplete-classifier.py" \
  "$aat_dir" --report-md "$report"

grep -F '| ruby_structural | 1 |' "$report"
grep -F '| invalid_xhtml | 1 |' "$report"
grep -F '| **Total** | **2** |' "$report"
