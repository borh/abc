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

# Every adapter's dump identity now fully captures the code that produces its
# output, so the active skip fires for all three. aozora is a single,
# self-contained, content-addressed nix binary (no external renderer). The
# wrapper adapters orchestrate an external renderer (Ruby aozora2html gem /
# AozoraEpub3.jar) which is now pinned by content: renderer_attr names the nix
# package whose store dir is resolved and hashed into identity (see below).
case "$adapter_id" in
  aozora2html)
    default_out_dir="${AB_AOZORA2HTML_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA2HTML_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA2HTML_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA2HTML_AAT_FULL_REPORT_ID:-aozora2html-full-$(date -u +%F)}"
    # Wrapper-pipeline adapter: ab-check invokes this bash wrapper (Ruby
    # aozora2html renderer + Rust mapper), not a self-contained binary. The
    # Rust mapper is resolved from nix past the --print-plan early-exit
    # (mapper_attr below); identity pins that nix mapper binary — the code that
    # actually changes — not the wrapper script.
    adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"
    mapper_attr="aozora2html-adapter"
    renderer_attr="upstream-parser-aozora2html"
    ;;
  aozora)
    default_out_dir="${AB_AOZORA_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_AAT_FULL_REPORT_ID:-aozora-full-$(date -u +%F)}"
    # The Rust mapper binary is resolved from nix past the --print-plan
    # early-exit; here "adapter" is just the flake attr id placeholder that
    # --print-plan reports (no build); it comes prebuilt, no mapper attr.
    # The adapter spawns the external upstream `aozora` parser (via
    # AB_AOZORA_BIN); treat it as this adapter's renderer so it is both
    # provisioned below and pinned by content in the identity, exactly like the
    # aozora2html/aozora-epub3 renderers.
    adapter="aozora-adapter"
    mapper_attr=""
    renderer_attr="upstream-parser-aozora"
    ;;
  aozora-epub3)
    default_out_dir="${AB_AOZORA_EPUB3_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-epub3-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_EPUB3_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_EPUB3_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_EPUB3_AAT_FULL_REPORT_ID:-aozora-epub3-full-$(date -u +%F)}"
    # Wrapper-pipeline adapter (AozoraEpub3.jar + Rust mapper): same rationale
    # as aozora2html above. Identity pins the nix-built Rust mapper binary
    # (mapper_attr below).
    adapter="$repo_root/adapters/aozora-epub3/aozora-epub3-adapter"
    mapper_attr="aozora-epub3-adapter"
    renderer_attr="upstream-parser-aozora-epub3"
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

# For --print-plan, "adapter" is already the path/attr set in the case block
# above: the invoked bash-wrapper path for the wrapper adapters, or the flake
# attr id for aozora (whose nix store path is resolved past the early-exit).
# Resolving the real binaries requires a nix build, which must NOT happen on
# the --print-plan early-exit path below (no network/build needed for a plan).

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

# Resolve the shared engine binaries (ab-index, ab-check) from the flake's
# store paths for ALL adapters — never cargo/just against the live source
# tree. Refs are repo-anchored ("$repo_root#...") so they resolve against the
# repo regardless of the caller's $PWD (matching the epub3-JAR line below).
# This runs only past the --print-plan early-exit above, so a plan stays
# build-free.
ab_index_bin="$(nix build "$repo_root#ab-index" --no-link --print-out-paths)/bin/ab-index"
ab_check_bin="$(nix build "$repo_root#ab-check" --no-link --print-out-paths)/bin/ab-check"
triage_python="$(nix build "$repo_root#aat-triage-python" --no-link --print-out-paths)/bin/python3"

# The aozora adapter's Rust mapper binary also comes from nix; adapter_hash_target
# pins it. The adapter additionally spawns the external upstream `aozora` parser,
# resolved + provisioned + content-pinned below as this adapter's renderer (so its
# identity is complete). The wrapper adapters keep the bash-wrapper path set in the
# case block (ab-check invokes it) and resolve their Rust mapper from nix below;
# adapter_hash_target then points at that nix mapper binary for them.
if [[ "$adapter_id" == "aozora" ]]; then
  adapter="$(nix build "$repo_root#aozora-adapter" --no-link --print-out-paths)/bin/aozora-adapter"
  adapter_hash_target="$adapter"
fi

# Wrapper adapters resolve their Rust mapper from nix (the flake packages the
# identical mapper crate). adapter_hash_target — the binary hashed into identity
# — becomes that nix store binary, and the matching wrapper override is exported
# so ab-check's wrapper execs the SAME binary (recorded == checked). Both must be
# set before the skip gate, which runs "$adapter" --version (the wrapper, which
# execs the exported mapper) and hashes adapter_hash_target.
if [[ -n "$mapper_attr" ]]; then
  mapper_bin="$(nix build "$repo_root#$mapper_attr" --no-link --print-out-paths)/bin/$mapper_attr"
  adapter_hash_target="$mapper_bin"
  if [[ "$adapter_id" == "aozora2html" ]]; then
    export AB_AOZORA2HTML_MAPPER_BIN="$mapper_bin"
  elif [[ "$adapter_id" == "aozora-epub3" ]]; then
    export AB_AOZORAEPUB3_MAPPER_BIN="$mapper_bin"
  fi
fi

# Resolve each adapter's external parser/renderer from its nix store dir so it is
# pinned by content (the Rust mapper was already resolved from nix above): the
# aozora2html Ruby gem, the AozoraEpub3.jar, or — for the aozora adapter — the
# upstream `aozora` parser it spawns. Each is exported under the env var its
# adapter reads, so ab-check runs the SAME store binary that identity pins.
renderer_dir=""
if [[ -n "$renderer_attr" ]]; then
  renderer_dir="$(nix build "$repo_root#$renderer_attr" --no-link --print-out-paths)"
fi
if [[ "$adapter_id" == "aozora2html" ]]; then
  export AB_AOZORA2HTML_BIN="$renderer_dir/bin/aozora2html"
elif [[ "$adapter_id" == "aozora-epub3" ]]; then
  export AB_AOZORAEPUB3_JAR="$renderer_dir/lib/AozoraEpub3.jar"
elif [[ "$adapter_id" == "aozora" ]]; then
  export AB_AOZORA_BIN="$renderer_dir/bin/aozora"
fi
if [[ ! "$jobs" =~ ^[0-9]+$ || "$jobs" == "0" ]]; then
  echo "--jobs must be a positive integer" >&2
  exit 2
fi
if [[ ! -d "$corpus/cards" ]]; then
  printf 'missing Aozora corpus cards directory: %s/cards\n' "$corpus" >&2
  exit 2
fi
# Compute the input identity ONCE (tree_hash over the corpus is the expensive
# part) and reuse it for both the skip gate and the recorded metadata, so the
# gate hash and the recorded hash are the SAME value by construction rather than
# two argument lists that must be kept byte-identical by hand. --version is run
# from $repo_root so the recorded adapter_version is reproducible for a
# cwd-sensitive adapter (otherwise a stale dump could never be recognised as
# fresh). identity_file is a mktemp OUTSIDE $out_dir, so it survives the
# `rm -rf "$out_dir"` below and can still be read by the metadata writer.
adapter_version="$(cd "$repo_root" && "$adapter" --version)"
identity_file="$(mktemp)"
cleanup_identity_file() { rm -f "$identity_file"; }
trap cleanup_identity_file EXIT
renderer_arg=()
if [[ -n "$renderer_dir" ]]; then
  renderer_arg=(--renderer-dir "$renderer_dir")
fi
current_hash="$(python "$repo_root/reports/aat-fidelity/generator_identity.py" \
  --emit-identity "$identity_file" \
  --corpus-dir "$corpus/cards" \
  --adapter-version "$adapter_version" \
  --adapter-binary "$adapter_hash_target" \
  --ab-index-binary "$ab_index_bin" \
  --ab-check-binary "$ab_check_bin" \
  --feature-patterns "$repo_root/data/feature-patterns.toml" \
  "${renderer_arg[@]}" \
  ${timeout:+--timeout "$timeout"} \
  ${features:+--features "$features"} \
  ${work_ids:+--work-ids "$work_ids"})"

if [[ -e "$out_dir" && "$force" != "1" ]]; then
  # A prior dump exists: skip recompute iff it is provably fresh for the current
  # inputs. current_hash was computed once above and is the same value the
  # metadata below records, so the gate and the record cannot drift.
  if python "$repo_root/reports/aat-fidelity/generator_skip.py" \
       --out-dir "$out_dir" --input-set-hash "$current_hash"; then
    printf '%s AAT dump already fresh, skipping: %s\n' "$adapter_id" "$out_dir"
    exit 0
  fi
  printf 'output directory exists (stale or unverifiable): %s\n' "$out_dir" >&2
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

run_step build-triage "$triage_dir" "$triage_python" \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"

python - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" "$adapter_id" "$features" "$work_ids" "$ab_index_bin" "$ab_check_bin" "$adapter_hash_target" "$renderer_dir" "$identity_file" "$adapter_version" <<'PY'
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, report_id, jobs, timeout, adapter, adapter_id, features, work_ids, ab_index_bin, ab_check_bin, adapter_hash_target, renderer_dir, identity_file, adapter_version = sys.argv[1:]
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
    "adapter_version": adapter_version,
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
# input_set_hash differs from the one recorded here). The input identity was
# computed ONCE up front (identity_file) and its hash was the value the skip gate
# checked; reusing it verbatim here makes the recorded input_set_hash equal the
# gated one by construction. Only output_content_hash is fresh (it hashes the
# aat/ tree this run just produced).
payload = json.loads(pathlib.Path(identity_file).read_text())
sys.path.insert(0, str(repo / "reports" / "lib"))
import aat_hash  # noqa: E402

metadata["input_set_hash"] = payload["input_set_hash"]
metadata["output_content_hash"] = aat_hash.hash_aat_dir(out / "aat")
# Full identity object (not only its derived hash), so a future audit can see
# WHICH input changed, not merely that input_set_hash moved.
metadata["input_identity"] = payload["identity_object"]

(out / "metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
PY
workflow_step_pass write-metadata "$metadata_path"

workflow_finish passed

printf '%s AAT run complete: %s\n' "$adapter_id" "$out_dir"
printf 'triage report: %s\n' "$triage_dir/index.md"
