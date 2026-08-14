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
adapter_bin_override=""

usage() {
  cat >&2 <<'EOF'
usage: run-aat-full.sh --adapter ADAPTER [--corpus DIR] [--out-dir DIR]
                       [--jobs N] [--timeout DURATION] [--report-id ID]
                       [--work-ids IDS] [--features TAGS] [--force]
                       [--print-plan] [--adapter-bin PATH]

ADAPTER: ab-aozora

--adapter-bin PATH: only valid with --adapter ab-aozora. Overrides the
  pinned nix-store `ab-aozora` adapter binary with an explicit binary: it is
  what actually runs AND what is recorded as the run's generator identity
  (hashed into input_set_hash, plus its path, sha256, and --version output
  recorded verbatim in metadata.json). Fails (exit 2) if PATH does not exist
  or `PATH --version` fails. Without it, behavior is unchanged: the pinned
  ab-aozora store binary (no inner parser, no renderer).
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
    --adapter-bin)
      adapter_bin_override="$2"
      shift 2
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

if [[ -n "$adapter_bin_override" && "$adapter_id" != "ab-aozora" ]]; then
  echo "--adapter-bin only applies to --adapter ab-aozora" >&2
  usage
  exit 2
fi

case "$adapter_id" in
  ab-aozora)
    default_out_dir="${AB_AB_AOZORA_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/ab-aozora-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AB_AOZORA_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AB_AOZORA_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AB_AOZORA_AAT_FULL_REPORT_ID:-ab-aozora-full-$(date -u +%F)}"
    # ab-aozora (Task 4's permanent native stdin->AAT binary) has NO external
    # renderer and NO separate Rust mapper: the adapter binary itself is the
    # complete generator identity. "adapter" here is a placeholder for
    # --print-plan (no build); the resolution block below (mirroring the
    # aozora branch) resolves the real binary from nix, or from an explicit
    # --adapter-bin override, past the --print-plan early-exit.
    adapter="ab-aozora"
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
# store paths — never cargo/just against the live source tree. Refs are
# repo-anchored ("$repo_root#...") so they resolve against the repo
# regardless of the caller's $PWD. This runs only past the --print-plan
# early-exit above, so a plan stays build-free.
ab_index_bin="$(nix build "$repo_root#ab-index" --no-link --print-out-paths)/bin/ab-index"
ab_check_bin="$(nix build "$repo_root#ab-check" --no-link --print-out-paths)/bin/ab-check"
triage_python="$(nix build "$repo_root#aat-triage-python" --no-link --print-out-paths)/bin/python3"

# The wrapper adapters keep the bash-wrapper path set in the case block
# (ab-check invokes it) and resolve their Rust mapper from nix below;
# adapter_hash_target then points at that nix mapper binary for them.

# ab-aozora is the permanent native stdin->AAT binary (Task 4): no separate
# Rust mapper, no external renderer — the binary itself is the complete
# generator identity. --adapter-bin overrides resolution entirely: no nix
# build, the override both executes AND is what is recorded (never silently
# diverging).
adapter_bin_override_sha256=""
adapter_bin_override_version=""
if [[ "$adapter_id" == "ab-aozora" ]]; then
  if [[ -n "$adapter_bin_override" ]]; then
    if [[ ! -x "$adapter_bin_override" ]]; then
      printf -- '--adapter-bin not found or not executable: %s\n' "$adapter_bin_override" >&2
      exit 2
    fi
    adapter_bin_override="$(cd "$(dirname "$adapter_bin_override")" && pwd)/$(basename "$adapter_bin_override")"
    if ! adapter_bin_override_version="$("$adapter_bin_override" --version)"; then
      printf -- '--adapter-bin --version failed: %s\n' "$adapter_bin_override" >&2
      exit 2
    fi
    adapter_bin_override_sha256="$(sha256sum "$adapter_bin_override" | cut -d' ' -f1)"
    adapter="$adapter_bin_override"
  else
    adapter="$(nix build "$repo_root#ab-aozora" --no-link --print-out-paths)/bin/ab-aozora"
  fi
  adapter_hash_target="$adapter"
fi

# --adapter-bin PATH overrides resolution entirely: no nix build happens; the
# override's containing directory stands in for renderer_dir below so the
# existing tree_hash-based identity/staleness plumbing picks up its content
# unchanged (a differing override binary is a differing renderer_content_hash,
# so a stale dump against a NEW override is never served as fresh).
renderer_dir=""
if [[ -n "$adapter_bin_override" ]]; then
  renderer_dir="$(dirname "$adapter_bin_override")"
else
  # nix-resolved: renderer_dir = the nix output dir (content-addressed), i.e.
  # the store path containing bin/ab-aozora.
  renderer_dir="$(dirname "$(dirname "$adapter")")"
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

python - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" "$adapter_id" "$features" "$work_ids" "$ab_index_bin" "$ab_check_bin" "$adapter_hash_target" "$renderer_dir" "$identity_file" "$adapter_version" "$adapter_bin_override" "$adapter_bin_override_sha256" "$adapter_bin_override_version" <<'PY'
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

(
    repo_root,
    corpus,
    out_dir,
    report_id,
    jobs,
    timeout,
    adapter,
    adapter_id,
    features,
    work_ids,
    ab_index_bin,
    ab_check_bin,
    adapter_hash_target,
    renderer_dir,
    identity_file,
    adapter_version,
    adapter_bin_override,
    adapter_bin_override_sha256,
    adapter_bin_override_version,
) = sys.argv[1:]
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

# Explicit --adapter-bin override identity (Task 6, ab-aozora lane): recorded
# verbatim — proof the override reached the adapter subprocess ab-check
# actually invoked, not merely that a flag was passed.
if adapter_bin_override:
    metadata["adapter_bin_override"] = {
        "path": adapter_bin_override,
        "sha256": adapter_bin_override_sha256,
        "version": adapter_bin_override_version,
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
