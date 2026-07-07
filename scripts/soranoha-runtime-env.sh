#!/usr/bin/env bash
# Shared local runtime paths for Soranoha tools.
#
# Source this file from direnv, shell smokes, or ad-hoc report scripts. Explicit
# environment variables win over machine config files; config files win over
# repo-local scratch defaults.

if [[ -n "${SORANOHA_RUNTIME_ENV_LOADED:-}" ]]; then
  return 0 2>/dev/null || exit 0
fi
export SORANOHA_RUNTIME_ENV_LOADED=1

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
  AB_AOZORA_RS_AAT_DIR
  AB_AOZORA2_AAT_DIR
  AB_AOZORA2HTML_AAT_DIR
  AB_AOZORA_EPUB3_AAT_DIR
  AB_AOZORA_AAT_DIR
  AB_TEI_EAJ_WORKSET
  ABC_OUTPUT_ROOT
  ABC_REPORT_DIR
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
export AB_AOZORA_RS_AAT_DIR="${AB_AOZORA_RS_AAT_DIR:-$SORANOHA_WORKSPACE_ROOT/scratch/morph-full-corpus/aats/aozora-rs-adapter}"
export AB_AOZORA2_AAT_DIR="${AB_AOZORA2_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter}"
export AB_AOZORA2HTML_AAT_DIR="${AB_AOZORA2HTML_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
export AB_AOZORA_EPUB3_AAT_DIR="${AB_AOZORA_EPUB3_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter}"
export AB_AOZORA_AAT_DIR="${AB_AOZORA_AAT_DIR:-$AB_DB_ROOT/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter}"

export ABC_OUTPUT_ROOT="${ABC_OUTPUT_ROOT:-$SORANOHA_WORKSPACE_ROOT/abc/out}"
export ABC_REPORT_DIR="${ABC_REPORT_DIR:-$ABC_OUTPUT_ROOT/reports}"
export AB_TEI_EAJ_WORKSET="${AB_TEI_EAJ_WORKSET:-$ABC_REPORT_DIR/tei-eaj-aozora/tei-eaj-aozora-workset-export.json}"

soranoha_runtime_summary() {
  cat <<EOF
SORANOHA_WORKSPACE_ROOT=$SORANOHA_WORKSPACE_ROOT
SORANOHA_STATE_ROOT=$SORANOHA_STATE_ROOT
AB_DB_ROOT=$AB_DB_ROOT
ABC_OUTPUT_ROOT=$ABC_OUTPUT_ROOT
ABC_REPORT_DIR=$ABC_REPORT_DIR
SORANOHA_REPORT_DIR=$SORANOHA_REPORT_DIR
EOF
}
