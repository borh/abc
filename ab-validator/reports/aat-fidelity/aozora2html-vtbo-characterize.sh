#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
run_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z"
aat_dir="$run_dir/aat/aozora2html-adapter"
reports_dir="$run_dir/check-reports"
corpus="${AB_AOZORA_CORPUS:-$repo_root/references/aozorabunko}"
out_dir="${AB_AOZORA2HTML_VTBO_OUT:-$run_dir/triage/outputs/vtbo}"
sample="${AB_AOZORA2HTML_VTBO_SAMPLE:-0}"   # 0 = all 668
mkdir -p "$out_dir"

python3 - "$run_dir" "$corpus" > "$out_dir/failing_works.tsv" <<'PY'
import json, os, sys
run_dir, corpus = sys.argv[1], sys.argv[2]

# Join work_id -> txt_path via index.json (check reports carry no source path).
index = json.load(open(os.path.join(run_dir, "index.json")))
txt_by_id = {w["id"]: w.get("txt_path", "") for w in index.get("works", [])}

reports_dir = os.path.join(run_dir, "check-reports")
for root, _, files in os.walk(reports_dir):
    for fn in files:
        if not fn.endswith(".json"):
            continue
        try:
            rep = json.load(open(os.path.join(root, fn)))
        except Exception:
            continue
        results = rep.get("results", {})
        vtbo = results.get("visible_text_body_order", {})
        if isinstance(vtbo, dict) and vtbo.get("pass") is False:
            wid = rep.get("work_id", "")
            txt_path = txt_by_id.get(wid, "")
            archive_rel, _, entry = txt_path.partition("::")
            archive = os.path.join(corpus, archive_rel) if archive_rel else ""
            print(f"{wid}\t{archive}\t{entry}")
PY

count=0
while IFS=$'\t' read -r wid archive entry; do
  [[ -z "$wid" ]] && continue
  [[ "$sample" -ne 0 && "$count" -ge "$sample" ]] && break
  # AAT files are suffixed (000081_4418-04cb6bb131bc.json); glob by work_id prefix.
  aat=$(ls "$aat_dir/$wid"*.json 2>/dev/null | head -1 || true)
  [[ -f "$aat" ]] || continue
  [[ -n "$archive" && -f "$archive" ]] || continue
  src="/tmp/vtbo_src.$$.txt"
  python3 - "$archive" "$entry" "$src" <<'PY' || continue
import zipfile, sys
archive, entry, out = sys.argv[1], sys.argv[2], sys.argv[3]
try:
    with zipfile.ZipFile(archive) as z:
        if entry:
            data = z.read(entry)
        else:
            info = max(z.infolist(), key=lambda i: i.file_size)
            data = z.read(info)
        open(out, "wb").write(data)
except Exception:
    sys.exit(1)
PY
  cargo run -p ab-check --example vtbo_locate -- --aat "$aat" --source "$src" --context 60 \
    > "$out_dir/$wid.txt" 2>/dev/null || true
  rm -f "$src"
  count=$((count + 1))
done < "$out_dir/failing_works.tsv"

echo "characterized $count works; individual outputs in $out_dir/*.txt"
