#!/usr/bin/env bash
# Shared local runtime paths for Soranoha tools.
#
# Source this file from direnv, shell smokes, or ad-hoc report scripts. Explicit
# environment variables win over machine config files; config files win over
# repo-local scratch defaults.

_soranoha_runtime_env_pid="${BASHPID:-$$}"
if [[ "${SORANOHA_RUNTIME_ENV_LOADED_PID:-}" == "$_soranoha_runtime_env_pid" ]]; then
  return 0 2>/dev/null || exit 0
fi
SORANOHA_RUNTIME_ENV_LOADED=1
SORANOHA_RUNTIME_ENV_LOADED_PID="$_soranoha_runtime_env_pid"
export -n SORANOHA_RUNTIME_ENV_LOADED SORANOHA_RUNTIME_ENV_LOADED_PID 2>/dev/null || true

_soranoha_script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_soranoha_script_root="$(cd "$_soranoha_script_dir/.." && pwd)"

export SORANOHA_WORKSPACE_ROOT="${SORANOHA_WORKSPACE_ROOT:-$_soranoha_script_root}"

_soranoha_config_candidates=()
if [[ -n "${SORANOHA_CONFIG:-}" ]]; then
  _soranoha_config_candidates+=("$SORANOHA_CONFIG")
else
  _soranoha_config_candidates+=(
    "$SORANOHA_WORKSPACE_ROOT/.soranoha/env"
    "${XDG_CONFIG_HOME:-$HOME/.config}/soranoha/env"
  )
fi

_soranoha_vars=(
  SORANOHA_STATE_ROOT
  SORANOHA_REPORT_DIR
  AB_DB_ROOT
  AB_MORPH_WAREHOUSE_DIR
  AB_MORPH_WAREHOUSE_AAT_DIR
  AB_AAT_RUN_SET
  AB_AOZORA2HTML_AAT_DIR
  AB_TEI_EAJ_WORKSET
  AB_RESEARCH_OUTPUT_ROOT
  AB_RESEARCH_REPORT_DIR
  AB_TEI_P5_ROOT
)

_soranoha_explicit_file="$(mktemp)"
for _soranoha_var in "${_soranoha_vars[@]}"; do
  if [[ -v "$_soranoha_var" ]]; then
    printf '%s=%q\n' "$_soranoha_var" "${!_soranoha_var}" >> "$_soranoha_explicit_file"
  fi
done

for _soranoha_config in "${_soranoha_config_candidates[@]}"; do
  if [[ -f "$_soranoha_config" ]]; then
    # shellcheck source=/dev/null
    source "$_soranoha_config"
    break
  fi
done

if [[ -s "$_soranoha_explicit_file" ]]; then
  # Restore values that were present before sourcing config.
  # shellcheck source=/dev/null
  source "$_soranoha_explicit_file"
fi
rm -f "$_soranoha_explicit_file"

export SORANOHA_STATE_ROOT="${SORANOHA_STATE_ROOT:-$SORANOHA_WORKSPACE_ROOT/scratch/state}"
export SORANOHA_REPORT_DIR="${SORANOHA_REPORT_DIR:-$SORANOHA_WORKSPACE_ROOT/out/reports}"

export AB_DB_ROOT="${AB_DB_ROOT:-$SORANOHA_WORKSPACE_ROOT/ab-validator/scratch/state}"
export AB_MORPH_WAREHOUSE_DIR="${AB_MORPH_WAREHOUSE_DIR:-$AB_DB_ROOT/morph-warehouse}"
export AB_MORPH_WAREHOUSE_AAT_DIR="${AB_MORPH_WAREHOUSE_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-aat/aozora2html-adapter}"
export AB_AAT_RUN_SET="${AB_AAT_RUN_SET:-$SORANOHA_WORKSPACE_ROOT/ab-validator/reports/aat-fidelity/run-sets/current.json}"
# The resolved AAT run-set lock selects fidelity dumps.
# AB_AOZORA2HTML_AAT_DIR below supplies only retained single-file research fixtures.
export AB_AOZORA2HTML_AAT_DIR="${AB_AOZORA2HTML_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"

export AB_RESEARCH_OUTPUT_ROOT="${AB_RESEARCH_OUTPUT_ROOT:-$SORANOHA_WORKSPACE_ROOT/ab-validator/research/out}"
export AB_RESEARCH_REPORT_DIR="${AB_RESEARCH_REPORT_DIR:-$AB_RESEARCH_OUTPUT_ROOT/reports}"
export AB_TEI_EAJ_WORKSET="${AB_TEI_EAJ_WORKSET:-$AB_RESEARCH_REPORT_DIR/tei-eaj-aozora/tei-eaj-aozora-workset-export.json}"

soranoha_runtime_summary() {
  cat <<EOF
SORANOHA_WORKSPACE_ROOT=$SORANOHA_WORKSPACE_ROOT
SORANOHA_STATE_ROOT=$SORANOHA_STATE_ROOT
AB_DB_ROOT=$AB_DB_ROOT
AB_AAT_RUN_SET=$AB_AAT_RUN_SET
AB_RESEARCH_OUTPUT_ROOT=$AB_RESEARCH_OUTPUT_ROOT
AB_RESEARCH_REPORT_DIR=$AB_RESEARCH_REPORT_DIR
SORANOHA_REPORT_DIR=$SORANOHA_REPORT_DIR
EOF
}
