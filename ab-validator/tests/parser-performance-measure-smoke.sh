#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

corpus="$tmp/corpus"
mkdir -p "$corpus/cards/000001/files"

python - "$corpus/cards/000001/files/1_ruby.zip" <<'PY'
import sys
import zipfile

with zipfile.ZipFile(sys.argv[1], "w") as z:
    z.writestr("sample.txt", "本文\n".encode("utf-8"))
PY

cat > "$tmp/index.json" <<'JSON'
{
  "works_count": 1,
  "works": [
    {
      "id": "000001_1",
      "txt_path": "cards/000001/files/1_ruby.zip::sample.txt"
    }
  ]
}
JSON

cat > "$tmp/work_ids.json" <<'JSON'
["000001_1"]
JSON

cat > "$tmp/fake-time" <<'SH'
#!/usr/bin/env bash
set -euo pipefail

out=""
if [[ "${1:-}" == "-v" ]]; then
  shift
fi
if [[ "${1:-}" == "-o" ]]; then
  out="$2"
  shift 2
fi

"$@"
code=$?
cat > "$out" <<'EOF'
	Command being timed: "fake"
	User time (seconds): 1.23
	System time (seconds): 0.45
	Percent of CPU this job got: 71%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:02.34
	Maximum resident set size (kbytes): 45678
	Exit status: 0
EOF
exit "$code"
SH
chmod +x "$tmp/fake-time"

cat > "$tmp/fake-adapter-a" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
cat >/dev/null
printf '{"version":1,"meta":{"adapter":"parser-a","parse_complete":true},"blocks":[]}\n'
SH
chmod +x "$tmp/fake-adapter-a"

cat > "$tmp/fake-adapter-b" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
echo "fake-adapter-b should not be used for the aozora2html pipeline" >&2
exit 44
SH
chmod +x "$tmp/fake-adapter-b"

cat > "$tmp/fake-parser" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
out="${@: -1}"
printf '<html><body>ok</body></html>\n' > "$out"
SH
chmod +x "$tmp/fake-parser"

cat > "$tmp/fake-mapper" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
printf '{"version":1,"meta":{"adapter":"aozora2html","parse_complete":true},"blocks":[]}\n'
SH
chmod +x "$tmp/fake-mapper"

out="$tmp/out"
python "$repo_root/reports/aat-fidelity/measure-parser-performance.py" \
  --index "$tmp/index.json" \
  --corpus "$corpus" \
  --out-dir "$out" \
  --work-ids "$tmp/work_ids.json" \
  --sample 1 \
  --limit-s 5 \
  --time-bin "$tmp/fake-time" \
  --adapter aozora2="$tmp/fake-adapter-a --mode aat" \
  --adapter aozora-rs="$tmp/fake-adapter-a --mode aat" \
  --adapter aozora2html="$tmp/fake-adapter-b --mode aat" \
  --adapter aozora-epub3="$tmp/fake-adapter-a --mode aat" \
  --adapter aozora="$tmp/fake-adapter-a --mode aat" \
  --aozora2html-label aozora2html \
  --aozora2html-bin "$tmp/fake-parser" \
  --mapper-bin "$tmp/fake-mapper" \
  --stage-split-sample 1

jq -e '.selected_works | length == 1' "$out/results.json"
jq -e '.adapters | length == 5' "$out/results.json"
jq -e '.adapters | map(.label) == ["aozora2", "aozora-rs", "aozora2html", "aozora-epub3", "aozora"]' "$out/results.json"
jq -e '.measurements | map(select(.stage == "full_adapter")) | length == 5' "$out/results.json"
jq -e '.measurements[] | select(.adapter == "aozora2" and .stage == "full_adapter") | .status == "ok" and .wall_s == 2.34 and .user_s == 1.23 and .sys_s == 0.45 and .max_rss_kb == 45678' "$out/results.json"
jq -e '.measurements[] | select(.adapter == "aozora" and .stage == "full_adapter") | .status == "ok"' "$out/results.json"
jq -e '.measurements[] | select(.adapter == "aozora2html" and .stage == "full_adapter") | .status == "ok"' "$out/results.json"
jq -e '.measurements[] | select(.adapter == "aozora2html" and .stage == "ruby_parser") | .status == "ok"' "$out/results.json"
jq -e '.measurements[] | select(.adapter == "aozora2html" and .stage == "rust_mapper") | .status == "ok"' "$out/results.json"
test -s "$out/summary.md"
