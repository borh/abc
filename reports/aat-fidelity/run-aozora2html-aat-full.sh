#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

corpus="$repo_root/references/aozorabunko"
out_dir="${AB_AOZORA2HTML_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)}"
jobs="${AB_AOZORA2HTML_AAT_FULL_JOBS:-$(nproc)}"
timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-180s}"
report_id="${AB_AOZORA2HTML_AAT_FULL_REPORT_ID:-aozora2html-full-$(date -u +%F)}"
work_ids=""
features=""
force=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --corpus)
      corpus="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --jobs)
      jobs="$2"
      shift 2
      ;;
    --timeout)
      timeout="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    --work-ids)
      work_ids="$2"
      shift 2
      ;;
    --features)
      features="$2"
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
if [[ ! -d "$corpus/cards" ]]; then
  printf 'missing Aozora corpus cards directory: %s/cards\n' "$corpus" >&2
  exit 2
fi
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  printf 'output directory already exists: %s\n' "$out_dir" >&2
  printf 'pass --force to replace it\n' >&2
  exit 2
fi

rm -rf "$out_dir"
mkdir -p "$out_dir"

index_path="$out_dir/index.json"
reports_dir="$out_dir/check-reports"
aat_dir="$out_dir/aat"
triage_dir="$out_dir/triage"
db_path="$out_dir/fidelity.duckdb"
adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"

run_just aozora2html-rust-build

run_cargo run -p ab-index -- \
  --corpus "$corpus" \
  --patterns "$repo_root/data/feature-patterns.toml" \
  --output "$index_path"

check_args=(
  run -p ab-check --
  --index "$index_path"
  --corpus "$corpus"
  --adapter "$adapter"
  --output "$reports_dir"
  --aat-output "$aat_dir"
  --jobs "$jobs"
  --per-work-timeout "$timeout"
)
if [[ -n "$work_ids" ]]; then
  check_args+=(--work-ids "$work_ids")
fi
if [[ -n "$features" ]]; then
  check_args+=(--features "$features")
fi

run_cargo "${check_args[@]}"

duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"

python3 - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" <<'PY'
import json
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, report_id, jobs, timeout, adapter = sys.argv[1:]
repo = pathlib.Path(repo_root)
out = pathlib.Path(out_dir)

def run(args):
    return subprocess.check_output(args, cwd=repo, text=True).strip()

metadata = {
    "generated_at_utc": datetime.now(timezone.utc).isoformat(),
    "repo_head": run(["git", "rev-parse", "HEAD"]),
    "repo_status_short": run(["git", "status", "--short"]),
    "corpus": str(pathlib.Path(corpus).resolve()),
    "report_id": report_id,
    "jobs": int(jobs),
    "timeout": timeout,
    "adapter": adapter,
    "adapter_version": run([adapter, "--version"]),
    "index_path": str(out / "index.json"),
    "reports_dir": str(out / "check-reports"),
    "aat_dir": str(out / "aat"),
    "triage_dir": str(out / "triage"),
    "db_path": str(out / "fidelity.duckdb"),
}
(out / "metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
PY

printf 'aozora2html AAT run complete: %s\n' "$out_dir"
printf 'triage report: %s\n' "$triage_dir/index.md"
