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
    # aozora2html renderer + Rust mapper), not a self-contained binary, so it
    # stays on its build pipeline (the flake's mapper-only package would fail
    # ab-check's `--mode aat` on raw stdin). Identity pins the Rust mapper
    # binary — the code that actually changes — not the wrapper script.
    adapter="$repo_root/adapters/aozora2html/aozora2html-adapter"
    build_step=(run_just aozora2html-rust-build)
    adapter_hash_target="$repo_root/adapters/aozora2html/target/release/aozora2html-adapter"
    renderer_attr="upstream-parser-aozora2html"
    ;;
  aozora)
    default_out_dir="${AB_AOZORA_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_AAT_FULL_REPORT_ID:-aozora-full-$(date -u +%F)}"
    # Self-contained Rust binary resolved from nix past the --print-plan
    # early-exit; here "adapter" is just the flake attr id placeholder that
    # --print-plan reports (no build). No build_step — it comes prebuilt.
    adapter="aozora-adapter"
    build_step=()
    renderer_attr=""
    ;;
  aozora-epub3)
    default_out_dir="${AB_AOZORA_EPUB3_AAT_FULL_OUT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-epub3-full-$(date -u +%Y%m%dT%H%M%SZ)}"
    default_jobs="${AB_AOZORA_EPUB3_AAT_FULL_JOBS:-$(nproc)}"
    default_timeout="${AB_AOZORA_EPUB3_AAT_FULL_TIMEOUT:-300s}"
    default_report_id="${AB_AOZORA_EPUB3_AAT_FULL_REPORT_ID:-aozora-epub3-full-$(date -u +%F)}"
    # Wrapper-pipeline adapter (AozoraEpub3.jar + Rust mapper): same rationale
    # as aozora2html above. Identity pins the Rust mapper binary.
    adapter="$repo_root/adapters/aozora-epub3/aozora-epub3-adapter"
    build_step=(run_just aozora-epub3-build)
    adapter_hash_target="$repo_root/adapters/aozora-epub3/target/release/aozora-epub3-adapter"
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

# The self-contained aozora adapter also comes from nix (its identity is
# complete). The wrapper adapters keep the bash-wrapper path set in the case
# block (ab-check invokes it) and build their Rust mapper via build_step below;
# adapter_hash_target already points at that mapper binary for them.
if [[ "$adapter_id" == "aozora" ]]; then
  adapter="$(nix build "$repo_root#aozora-adapter" --no-link --print-out-paths)/bin/aozora-adapter"
  adapter_hash_target="$adapter"
fi

# Resolve the external renderer (wrapper adapters) from its nix store dir so it
# is pinned by content, and build the Rust mapper NOW (before the skip gate) so
# its hash is available to the gate. aozora has no external renderer.
renderer_dir=""
if [[ -n "$renderer_attr" ]]; then
  renderer_dir="$(nix build "$repo_root#$renderer_attr" --no-link --print-out-paths)"
fi
if [[ "$adapter_id" == "aozora2html" ]]; then
  export AB_AOZORA2HTML_BIN="$renderer_dir/bin/aozora2html"
elif [[ "$adapter_id" == "aozora-epub3" ]]; then
  export AB_AOZORAEPUB3_JAR="$renderer_dir/lib/AozoraEpub3.jar"
fi
# Wrapper adapters build their Rust mapper up front so the gate can hash it
# (adapter_hash_target points at the mapper binary). aozora comes prebuilt from
# nix (build_step empty). This runs pre-workflow_init, so use a plain build (not
# run_step); the post-workflow build-adapter step below re-verifies incrementally.
if [[ ${#build_step[@]} -gt 0 ]]; then
  "${build_step[@]}"
fi

if [[ ! "$jobs" =~ ^[0-9]+$ || "$jobs" == "0" ]]; then
  echo "--jobs must be a positive integer" >&2
  exit 2
fi
if [[ ! -d "$corpus/cards" ]]; then
  printf 'missing Aozora corpus cards directory: %s/cards\n' "$corpus" >&2
  exit 2
fi
if [[ -e "$out_dir" && "$force" != "1" ]]; then
  # Active skip: if a prior dump at $out_dir is provably fresh for the current
  # inputs, exit 0 without recomputing. Every adapter is identity-complete now
  # (aozora is a self-contained nix binary; wrapper adapters pin their Rust
  # mapper AND their nix-packaged renderer, both resolved above), so the check
  # runs for all. The input_set_hash here MUST be byte-identical to the one
  # provenance_fields(...) writes into metadata.json below; its args mirror that
  # heredoc's provenance_fields(...) call exactly (corpus/cards, adapter_hash_target
  # as --adapter-binary, the adapter --version run from $repo_root, ab-index/ab-check
  # bins, feature patterns, the renderer dir, and the same optional
  # timeout/features/work-ids).
  #
  # --version is run from $repo_root so it is byte-identical to the metadata
  # heredoc's run([adapter, "--version"], cwd=repo) — otherwise a cwd-sensitive
  # adapter would make the recorded hash unreproducible here (silently never skip).
  renderer_arg=()
  if [[ -n "$renderer_dir" ]]; then
    renderer_arg=(--renderer-dir "$renderer_dir")
  fi
  current_hash="$(python "$repo_root/reports/aat-fidelity/generator_identity.py" \
    --corpus-dir "$corpus/cards" \
    --adapter-version "$(cd "$repo_root" && "$adapter" --version)" \
    --adapter-binary "$adapter_hash_target" \
    --ab-index-binary "$ab_index_bin" \
    --ab-check-binary "$ab_check_bin" \
    --feature-patterns "$repo_root/data/feature-patterns.toml" \
    "${renderer_arg[@]}" \
    ${timeout:+--timeout "$timeout"} \
    ${features:+--features "$features"} \
    ${work_ids:+--work-ids "$work_ids"})"
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

# Wrapper adapters build their Rust mapper here (build_step is non-empty);
# aozora comes prebuilt from nix (build_step is empty) so it runs no build.
if [[ ${#build_step[@]} -gt 0 ]]; then
  run_step build-adapter "$adapter" "${build_step[@]}"
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

run_step build-triage "$triage_dir" "$triage_python" \
  "$repo_root/reports/aat-fidelity/build-aat-batch-triage.py" \
  --reports-dir "$reports_dir" \
  --aat-dir "$aat_dir" \
  --db "$db_path" \
  --report-id "$report_id" \
  --out-dir "$triage_dir"

python - "$repo_root" "$corpus" "$out_dir" "$report_id" "$jobs" "$timeout" "$adapter" "$adapter_id" "$features" "$work_ids" "$ab_index_bin" "$ab_check_bin" "$adapter_hash_target" "$renderer_dir" <<'PY'
import json
import os
import pathlib
import subprocess
import sys
from datetime import datetime, timezone

repo_root, corpus, out_dir, report_id, jobs, timeout, adapter, adapter_id, features, work_ids, ab_index_bin, ab_check_bin, adapter_hash_target, renderer_dir = sys.argv[1:]
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

# Shared kwargs, passed identically to provenance_fields (the hash) and
# identity_fields (the object that hash is over) — so the recorded
# input_identity object and the recorded input_set_hash correspond exactly.
identity_kwargs = dict(
    corpus_dir=pathlib.Path(corpus) / "cards",
    adapter_version=metadata["adapter_version"],
    # Identity pins the code that actually changes: for wrapper adapters this is
    # the Rust mapper binary (not the invoked bash wrapper); for aozora it is the
    # nix binary itself (== adapter). "adapter"/"adapter_version" above still
    # describe the invoked path.
    adapter_binary=adapter_hash_target,
    ab_index_binary=ab_index_bin,
    ab_check_binary=ab_check_bin,
    feature_patterns_file=repo / "data" / "feature-patterns.toml",
    renderer_dir=(renderer_dir or None),
    timeout=timeout or None,
    features=features or None,
    work_ids=work_ids or None,
)
metadata.update(generator_identity.provenance_fields(aat_dir=out / "aat", **identity_kwargs))
# Full identity object (not only its derived hash), so a future audit can see
# WHICH input changed, not merely that input_set_hash moved.
metadata["input_identity"] = generator_identity.identity_fields(**identity_kwargs)

(out / "metadata.json").write_text(json.dumps(metadata, indent=2, ensure_ascii=False) + "\n")
PY
workflow_step_pass write-metadata "$metadata_path"

workflow_finish passed

printf '%s AAT run complete: %s\n' "$adapter_id" "$out_dir"
printf 'triage report: %s\n' "$triage_dir/index.md"
