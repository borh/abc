#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
corpus="${AB_AOZORA_CORPUS:-$repo_root/references/aozorabunko}"
run_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-corpus/aozora2html-full-20260703T020301Z"
out_dir="${AB_AOZORA2HTML_TIMEOUT_TAIL_OUT:-$run_dir/triage/outputs/timeout-tail}"
sample="${AB_AOZORA2HTML_TIMEOUT_TAIL_SAMPLE:-10}"
limit_s="${AB_AOZORA2HTML_TIMEOUT_TAIL_LIMIT:-600}"
adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"
mkdir -p "$out_dir"

# Build (work_id, archive, entry, size) rows for the $sample largest timed-out works.
python3 - "$run_dir" "$corpus" "$sample" > "$out_dir/sample.tsv" <<'PY'
import json, os, sys, zipfile
run_dir, corpus, sample = sys.argv[1], sys.argv[2], int(sys.argv[3])
index = json.load(open(os.path.join(run_dir, "index.json")))
txt_by_id = {w["id"]: w.get("txt_path", "") for w in index.get("works", [])}

timeout_ids = set()
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
        at = results.get("adapter_timeout", {})
        if isinstance(at, dict) and at.get("pass") is False:
            timeout_ids.add(rep.get("work_id", ""))

rows = []
for wid in timeout_ids:
    txt_path = txt_by_id.get(wid, "")
    if not txt_path:
        continue
    archive_rel, _, entry = txt_path.partition("::")
    archive = os.path.join(corpus, archive_rel)
    try:
        with zipfile.ZipFile(archive) as z:
            info = z.getinfo(entry) if entry else max(z.infolist(), key=lambda i: i.file_size)
            size = info.file_size
    except Exception:
        size = 0
    rows.append((size, wid, archive, entry))
rows.sort(reverse=True)
for size, wid, archive, entry in rows[:sample]:
    print(f"{wid}\t{archive}\t{entry}\t{size}")
PY

results="$out_dir/results.json"
printf '[' > "$results"
first=1
while IFS=$'\t' read -r wid archive entry size; do
  [[ -z "$wid" ]] && continue
  tmp="$(mktemp --suffix=.txt)"
  python3 - "$archive" "$entry" "$tmp" <<'PY'
import zipfile, sys
archive, entry, out = sys.argv[1], sys.argv[2], sys.argv[3]
with zipfile.ZipFile(archive) as z:
    if entry:
        data = z.read(entry)
    else:
        info = max(z.infolist(), key=lambda i: i.file_size)
        data = z.read(info)
    open(out, "wb").write(data)
PY
  start=$(date +%s)
  if timeout "$limit_s" "$adapter" --mode aat < "$tmp" > "/tmp/out.$$.json" 2> "/tmp/err.$$.json"; then
    status=ok
  else
    code=$?
    if [[ $code -eq 124 ]]; then status=timeout; else status="error:$code"; fi
  fi
  end=$(date +%s)
  real=$((end - start))
  rm -f "$tmp" "/tmp/out.$$.json" "/tmp/err.$$.json"
  [[ $first -eq 0 ]] && printf ',' >> "$results"
  first=0
  printf '{"work_id":"%s","size":%s,"real_s":%s,"status":"%s"}' "$wid" "$size" "$real" "$status" >> "$results"
done < "$out_dir/sample.tsv"
printf ']' >> "$results"
echo "wrote $results"
python3 - "$results" "$out_dir" <<'PY'
import json, sys, statistics
results = json.load(open(sys.argv[1]))
rows = [r for r in results if r["status"] == "ok"]
mx = max((r["real_s"] for r in rows), default=0)
med = statistics.median([r["real_s"] for r in rows]) if rows else 0
print(f"ok={len(rows)} max_real_s={mx} median_real_s={med}")
PY
