#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

aozora_root="$("$repo_root/scripts/resolve-aozorabunko-corpus.sh")"
out_dir="${AB_AAT_FIDELITY_XHTML_FULL_OUT_DIR:-/db/ab-validator/aat-fidelity/upstream-xhtml-full}"
db_path="${AB_AAT_FIDELITY_DB:-/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb}"
report_id="${AB_AAT_FIDELITY_REPORT_ID:-upstream-xhtml-full}"
jobs="${AB_AAT_FIDELITY_XHTML_JOBS:-$(nproc)}"
max_cards=0
triage_limit=100
force=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --aozora-root)
      aozora_root="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --db)
      db_path="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    --jobs)
      jobs="$2"
      shift 2
      ;;
    --max-cards)
      max_cards="$2"
      shift 2
      ;;
    --triage-limit)
      triage_limit="$2"
      shift 2
      ;;
    --force)
      force=1
      shift
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      exit 2
      ;;
  esac
done

if [[ ! "$jobs" =~ ^[0-9]+$ || "$jobs" == "0" ]]; then
  echo "--jobs must be a positive integer" >&2
  exit 2
fi
if [[ ! "$max_cards" =~ ^[0-9]+$ ]]; then
  echo "--max-cards must be a non-negative integer" >&2
  exit 2
fi
if [[ ! "$triage_limit" =~ ^[0-9]+$ || "$triage_limit" == "0" ]]; then
  echo "--triage-limit must be a positive integer" >&2
  exit 2
fi
if [[ "$aozora_root" =~ ^https?:// ]]; then
  echo "--aozora-root must be a local Aozora corpus path" >&2
  exit 2
fi
if [[ ! -d "$aozora_root/cards" ]]; then
  printf 'missing local Aozora cards directory: %s/cards\n' "$aozora_root" >&2
  exit 2
fi

mkdir -p \
  "$out_dir" \
  "$out_dir/observations" \
  "$out_dir/observations/logs" \
  "$out_dir/observations/failures" \
  "$out_dir/sources" \
  "$(dirname "$db_path")"

card_urls="$out_dir/card-urls.txt"
manifest="$out_dir/manifest.tsv"
metadata="$out_dir/metadata.csv"
valid_manifest="$out_dir/manifest.valid.tsv"
invalid_manifest="$out_dir/manifest.invalid.tsv"
generation_failures="$out_dir/generation-failures.tsv"

find -L "$aozora_root/cards" -type f -name 'card*.html' | sort > "$card_urls.all"
if [[ "$max_cards" == "0" ]]; then
  cp "$card_urls.all" "$card_urls"
else
  head -n "$max_cards" "$card_urls.all" > "$card_urls"
fi

python "$repo_root/reports/aat-fidelity/build-upstream-xhtml-manifest.py" \
  --aozora-root "$aozora_root" \
  --card-url-file "$card_urls" \
  --sample-size 0 \
  --classify-source \
  --out-manifest "$manifest" \
  --out-metadata "$metadata"

python - "$manifest" "$valid_manifest" "$invalid_manifest" <<'PY'
import csv
import sys
from pathlib import Path
from zipfile import BadZipFile, ZipFile

manifest = Path(sys.argv[1])
valid = Path(sys.argv[2])
invalid = Path(sys.argv[3])

with manifest.open(newline="", encoding="utf-8") as f:
    rows = list(csv.DictReader(f, delimiter="\t"))

valid.parent.mkdir(parents=True, exist_ok=True)
with valid.open("w", newline="", encoding="utf-8") as valid_f, invalid.open(
    "w", newline="", encoding="utf-8"
) as invalid_f:
    valid_w = csv.DictWriter(
        valid_f, fieldnames=["case_id", "source", "upstream_xhtml"], delimiter="\t"
    )
    invalid_w = csv.DictWriter(
        invalid_f,
        fieldnames=["case_id", "source", "upstream_xhtml", "reason"],
        delimiter="\t",
    )
    valid_w.writeheader()
    invalid_w.writeheader()
    for row in rows:
        source = Path(row["source"])
        upstream = Path(row["upstream_xhtml"])
        reason = ""
        if not source.is_file():
            reason = "missing_source"
        elif not upstream.is_file():
            reason = "missing_upstream_xhtml"
        elif source.suffix.lower() == ".zip":
            try:
                with ZipFile(source) as zf:
                    if not any(name.lower().endswith(".txt") for name in zf.namelist()):
                        reason = "zip_without_txt"
            except BadZipFile:
                reason = "bad_zip"
        if reason:
            invalid_w.writerow({**row, "reason": reason})
        else:
            valid_w.writerow(row)
PY

generate_one() {
  local row="$1"
  local case_id source upstream source_txt local_xhtml log failure_file tmp_xhtml
  IFS=$'\t' read -r case_id source upstream <<< "$row"
  [[ -n "${case_id:-}" && "$case_id" != "case_id" ]] || return 0

  source_txt="$out_dir/sources/$case_id.txt"
  local_xhtml="$out_dir/observations/$case_id.local.xhtml"
  log="$out_dir/observations/logs/$case_id.stderr.log"
  failure_file="$out_dir/observations/failures/$case_id.tsv"
  tmp_xhtml="$local_xhtml.tmp"
  rm -f "$failure_file"

  if [[ "$force" == "0" && -s "$local_xhtml" ]]; then
    return 0
  fi

  if file "$source" | rg -q 'Zip archive'; then
    if ! unzip -p "$source" '*.txt' > "$source_txt" 2> "$log.extract"; then
      printf '%s\t%s\t%s\n' "$case_id" "source_extract_failed" "$(tr '\n\t' '  ' < "$log.extract")" > "$failure_file"
      rm -f "$tmp_xhtml"
      return 0
    fi
  else
    cp "$source" "$source_txt"
  fi

  if ! "$repo_root/adapters/aozora2html/aozora2html-adapter" --mode html \
    < "$source_txt" \
    > "$tmp_xhtml" \
    2> "$log"; then
    printf '%s\t%s\t%s\n' "$case_id" "local_generation_failed" "$(tr '\n\t' '  ' < "$log")" > "$failure_file"
    rm -f "$tmp_xhtml"
    return 0
  fi
  mv "$tmp_xhtml" "$local_xhtml"
}

export -f generate_one
export repo_root out_dir force

tail -n +2 "$valid_manifest" \
  | xargs -r -d '\n' -n 1 -P "$jobs" bash -c 'generate_one "$1"' _

{
  printf 'case_id\treason\tdetail\n'
  for failure in "$out_dir"/observations/failures/*.tsv; do
    [[ -e "$failure" ]] || continue
    cat "$failure"
  done
} > "$generation_failures"

duckdb_bin="${AB_DUCKDB_BIN:-${DUCKDB:-duckdb}}"
if [[ -n "$duckdb_bin" ]] && ! command -v "$duckdb_bin" >/dev/null 2>&1; then
  duckdb_bin=duckdb
fi
libstdcxx_path=""
if command -v "$duckdb_bin" >/dev/null 2>&1; then
  libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
fi
loader_env=()
if [[ -n "$libstdcxx_path" ]]; then
  loader_env=(env "LD_LIBRARY_PATH=$(dirname "$libstdcxx_path"):${LD_LIBRARY_PATH:-}")
fi

"${loader_env[@]}" uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  python - "$repo_root" "$valid_manifest" "$metadata" "$out_dir/observations" "$db_path" "$report_id" <<'PY'
import csv
import importlib.util
import sys
from pathlib import Path

repo_root = Path(sys.argv[1])
manifest = Path(sys.argv[2])
metadata = Path(sys.argv[3])
observations = Path(sys.argv[4])
db_path = Path(sys.argv[5])
report_id = sys.argv[6]

spec = importlib.util.spec_from_file_location(
    "compare_xhtml_sources",
    repo_root / "reports" / "aat-fidelity" / "compare-xhtml-sources.py",
)
module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(module)

conn = module.duckdb.connect(str(db_path))
module.create_tables(conn)

metadata_by_case = {}
with metadata.open(newline="", encoding="utf-8") as f:
    for row in csv.DictReader(f):
        metadata_by_case[row["case_id"]] = row

loaded = 0
missing_local = 0
with manifest.open(newline="", encoding="utf-8") as f:
    for row in csv.DictReader(f, delimiter="\t"):
        case_id = row["case_id"]
        local_xhtml = observations / f"{case_id}.local.xhtml"
        if not local_xhtml.is_file() or local_xhtml.stat().st_size == 0:
            missing_local += 1
            continue
        meta = metadata_by_case.get(case_id, {})
        module.load_observation_with_conn(
            conn=conn,
            report_id=report_id,
            case_id=case_id,
            upstream_xhtml=Path(row["upstream_xhtml"]),
            local_xhtml=local_xhtml,
            card_url=meta.get("card_url", ""),
            source_url=meta.get("source", row["source"]),
            upstream_url=meta.get("upstream_xhtml", row["upstream_xhtml"]),
            feature_tags=meta.get("feature_tags", ""),
            manifest_status=meta.get("status", "paired"),
        )
        loaded += 1
        if loaded % 1000 == 0:
            print(f"loaded={loaded}", flush=True)

print(f"loaded={loaded}")
print(f"missing_local={missing_local}")
PY

"$duckdb_bin" -csv -header "$db_path" \
  "select count(*) as total, sum(raw_equal)::UBIGINT as raw_equal, sum(rendered_body_proxy_eligible)::UBIGINT as rendered_body_proxy_eligible from fidelity_xhtml_observations where report_id = '$report_id'" \
  > "$out_dir/observations/summary.csv"
"$duckdb_bin" -csv -header "$db_path" \
  "select comparison_status, count(*) as rows from fidelity_xhtml_observations where report_id = '$report_id' group by comparison_status order by comparison_status" \
  > "$out_dir/observations/status-summary.csv"

"$repo_root/reports/aat-fidelity/build-xhtml-triage-report.sh" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$out_dir/triage-report" \
  --limit "$triage_limit"

"${loader_env[@]}" uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/classify-xhtml-adapter-errors.py" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$out_dir/adapter-error-report"

printf 'card_urls=%s\n' "$card_urls"
printf 'manifest=%s\n' "$manifest"
printf 'valid_manifest=%s\n' "$valid_manifest"
printf 'invalid_manifest=%s\n' "$invalid_manifest"
printf 'generation_failures=%s\n' "$generation_failures"
printf 'db_path=%s\n' "$db_path"
printf 'summary=%s\n' "$out_dir/observations/summary.csv"
printf 'status_summary=%s\n' "$out_dir/observations/status-summary.csv"
printf 'triage_report=%s\n' "$out_dir/triage-report/index.md"
printf 'adapter_error_report=%s\n' "$out_dir/adapter-error-report/index.md"
