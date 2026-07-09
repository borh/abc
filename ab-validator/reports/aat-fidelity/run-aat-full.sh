#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
workspace_root="$(cd "$repo_root/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"
source "$workspace_root/scripts/workflow-run-lib.sh"

adapter_id=""
corpus=""
out_dir=""
jobs=""
timeout=""
report_id=""
work_ids=""
features=""
force=0
print_plan=0

usage() {
  cat >&2 <<'EOF'
usage: run-aat-full.sh --adapter ADAPTER [--corpus DIR] [--out-dir DIR]
                       [--jobs N] [--timeout DURATION] [--report-id ID]
                       [--work-ids IDS] [--features TAGS] [--force]
                       [--print-plan]

ADAPTER: aozora | aozora2html | aozora-epub3
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --adapter)
      adapter_id="$2"
      shift 2
      ;;
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
    --print-plan)
      print_plan=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      usage
      exit 2
      ;;
  esac
done

if [[ -z "$adapter_id" ]]; then
  echo "--adapter is required" >&2
  usage
  exit 2
fi

case "$adapter_id" in
  aozora2html)
    default_out_dir="${AB_AOZORA2HTML_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA2HTML_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA2HTML_AAT_FULL_REPORT_ID:-aozora2html-full-$(date -u +%F)}"
    adapter_attr="aozora2html-adapter"
    ;;
  aozora)
    default_out_dir="${AB_AOZORA_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_AAT_FULL_REPORT_ID:-aozora-full-$(date -u +%F)}"
    adapter_attr="aozora-adapter"
    ;;
  aozora-epub3)
    default_out_dir="${AB_AOZORA_EPUB3_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-epub3-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_EPUB3_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_EPUB3_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_EPUB3_AAT_FULL_REPORT_ID:-aozora-epub3-full-$(date -u +%F)}"
    adapter_attr="aozora-epub3-adapter"
    ;;
  *)
    printf 'unknown AAT adapter: %s\n' "$adapter_id" >&2
    usage
    exit 2
    ;;
esac

out_dir="${out_dir:-$default_out_dir}"
jobs="${jobs:-$default_jobs}"
timeout="${timeout:-$default_timeout}"
report_id="${report_id:-$default_report_id}"
corpus="${corpus:-$(aat_aozorabunko_corpus)}"

# Placeholder for --print-plan: the flake attr id, not a resolved store path.
# Resolving the real binaries requires a nix build, which must NOT happen on
# the --print-plan early-exit path below (no network/build needed for a plan).
# The real resolve (overwriting this) happens right after that early-exit.
adapter="$adapter_attr"

index_path="$out_dir/index.json"
reports_dir="$out_dir/check-reports"
aat_dir="$out_dir/aat"
triage_dir="$out_dir/triage"
db_path="$out_dir/fidelity.duckdb"
metadata_path="$out_dir/metadata.json"
workflow_run_path="$out_dir/workflow-run.json"

emit_plan() {
  python - "$adapter_id" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" \
    "$adapter" "$index_path" "$reports_dir" "$aat_dir" "$triage_dir" "$db_path" \
    "$metadata_path" "$workflow_run_path" <<'PY'
import json
import pathlib
import sys

(
    adapter_id,
    corpus,
    out_dir,
    report_id,
    jobs,
    timeout,
    adapter,
    index_path,
    reports_dir,
    aat_dir,
    triage_dir,
    db_path,
    metadata_path,
    workflow_run_path,
) = sys.argv[1:]
plan = {
    "schema_version": 1,
    "workflow_id": "aat-full.materialize.v1",
    "adapter_id": adapter_id,
    "corpus": str(pathlib.Path(corpus).resolve()),
    "out_dir": out_dir,
    "report_id": report_id,
    "jobs": int(jobs),
    "timeout": timeout,
    "adapter": adapter,
    "index_path": index_path,
    "reports_dir": reports_dir,
    "aat_dir": aat_dir,
    "triage_dir": triage_dir,
    "db_path": db_path,
    "metadata_path": metadata_path,
    "workflow_run_path": workflow_run_path,
}
json.dump(plan, sys.stdout, ensure_ascii=False, sort_keys=True, indent=2)
sys.stdout.write("\n")
PY
}

if [[ "$print_plan" == "1" ]]; then
  emit_plan
  exit 0
fi

# All code through nix: resolve the adapter, ab-index, and ab-check binaries
# from the flake's store paths (never cargo/just against the live source
# tree). This runs only past the --print-plan early-exit above, so a plan
# request stays build-free. adapter_attr%-adapter strips the trailing
# "-adapter" suffix so it round-trips to the package's actual bin/ name for
# all three adapters (verified: aozora-adapter, aozora2html-adapter,
# aozora-epub3-adapter).
adapter="$(nix build ".#$adapter_attr" --no-link --print-out-paths)/bin/${adapter_attr%-adapter}-adapter"
ab_index_bin="$(nix build .#ab-index --no-link --print-out-paths)/bin/ab-index"
ab_check_bin="$(nix build .#ab-check --no-link --print-out-paths)/bin/ab-check"

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
workflow_init "$workflow_run_path" "aat-full.materialize.v1" "$adapter_id:$report_id"

run_step() {
  local step_id="$1"
  local output_path="$2"
  shift 2
  if "$@"; then
    workflow_step_pass "$step_id" "$output_path"
  else
    local status=$?
    workflow_step_fail "$step_id" "command failed with exit code $status"
    workflow_finish failed
    exit "$status"
  fi
}

if [[ "$adapter_id" == "aozora-epub3" && -z "${AB_AOZORAEPUB3_JAR:-}" ]]; then
  epub3_pkg="$(nix --option post-build-hook "" build --no-link --print-out-paths "$repo_root#upstream-parser-aozora-epub3")"
  export AB_AOZORAEPUB3_JAR="$epub3_pkg/lib/AozoraEpub3.jar"
fi

run_step build-index "$index_path" "$ab_index_bin" \
  --corpus "$corpus" \
  --patterns "$repo_root/data/feature-patterns.toml" \
  --output "$index_path"

check_args=(
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

run_step check-corpus "$reports_dir" "$ab_check_bin" "${check_args[@]}"

duckdb_bin="$(aat_duckdb_bin)"
aat_setup_duckdb_runtime "$duckdb_bin"

run_step build-triage "$triage_dir" uv run --isolated --no-project --with 'duckdb>=1.1' \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"

python - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" "$adapter_id" "$features" "$work_ids" "$ab_index_bin" "$ab_check_bin" <<'PY'
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, report_id, jobs, timeout, adapter, adapter_id, features, work_ids, ab_index_bin, ab_check_bin = sys.argv[1:]
repo = pathlib.Path(repo_root)
out = pathlib.Path(out_dir)

def run(args):
    return subprocess.check_output(args, cwd=repo, text=True).strip()

# Reproducible descriptor (Phase 2 A4a): honour SOURCE_DATE_EPOCH so a rebuild can
# stamp a fixed time, and record the dirty-tree state as a clean boolean instead of
# the noisy multi-line `git status --short` that made descriptors non-reproducible
# and buried the provenance hole (F6). The aat/ tree itself is what gets content-
# addressed; this just stops the descriptor from carrying run-specific noise.
_sde = os.environ.get("SOURCE_DATE_EPOCH")
generated_at = (
    datetime.fromtimestamp(int(_sde), timezone.utc).isoformat()
    if _sde
    else datetime.now(timezone.utc).isoformat()
)

metadata = {
    "schema_version": 3,
    "generated_at_utc": generated_at,
    "repo_head": run(["git", "rev-parse", "HEAD"]),
    "repo_dirty": bool(run(["git", "status", "--short"])),
    "corpus": str(pathlib.Path(corpus).resolve()),
    "report_id": report_id,
    "jobs": int(jobs),
    "timeout": timeout,
    "adapter_id": adapter_id,
    "adapter": adapter,
    "adapter_version": run([adapter, "--version"]),
    "ab_index": ab_index_bin,
    "ab_check": ab_check_bin,
    "index_path": str(out / "index.json"),
    "reports_dir": str(out / "check-reports"),
    "aat_dir": str(out / "aat"),
    "triage_dir": str(out / "triage"),
    "db_path": str(out / "fidelity.duckdb"),
    "workflow_run_path": str(out / "workflow-run.json"),
}

# F6: record the dump's input identity and its own output content hash, so
# staleness is decidable (a dump is stale exactly when a freshly-computed
# input_set_hash differs from the one recorded here). The corpus (cards tree),
# adapter binary, and feature-patterns are hashed by content; --jobs is excluded.
sys.path.insert(0, str(repo / "reports" / "aat-fidelity"))
import generator_identity  # noqa: E402

metadata.update(generator_identity.provenance_fields(
    aat_dir=out / "aat",
    corpus_dir=pathlib.Path(corpus) / "cards",
    adapter_version=metadata["adapter_version"],
    adapter_binary=adapter,
    ab_index_binary=ab_index_bin,
    ab_check_binary=ab_check_bin,
    feature_patterns_file=repo / "data" / "feature-patterns.toml",
    timeout=timeout or None,
    features=features or None,
    work_ids=work_ids or None,
))

(out / "metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
PY
workflow_step_pass write-metadata "$metadata_path"

workflow_finish passed

printf '%s AAT run complete: %s\n' "$adapter_id" "$out_dir"
printf 'triage report: %s\n' "$triage_dir/index.md"
