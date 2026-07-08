#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
runtime_env="$repo_root/scripts/soranoha-runtime-env.sh"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

unset SORANOHA_RUNTIME_ENV_LOADED
unset SORANOHA_RUNTIME_ENV_LOADED_PID
unset SORANOHA_CONFIG
unset SORANOHA_STATE_ROOT
unset SORANOHA_REPORT_DIR
unset AB_DB_ROOT
unset AB_MORPH_WAREHOUSE_DIR
unset AB_MORPH_WAREHOUSE_AAT_DIR
unset AB_AOZORA_RS_AAT_DIR
unset AB_AOZORA2_AAT_DIR
unset AB_AOZORA2HTML_AAT_DIR
unset AB_AOZORA_EPUB3_AAT_DIR
unset AB_AOZORA_AAT_DIR
unset AB_AAT_RUN_SET
unset AB_TEI_EAJ_WORKSET
unset ABC_OUTPUT_ROOT
unset ABC_REPORT_DIR
unset AB_TEI_P5_ROOT

mkdir -p "$tmp/workspace"

cat > "$tmp/machine.env" <<'ENV'
export SORANOHA_STATE_ROOT=/machine/state
export AB_DB_ROOT=/config/ab-validator
export ABC_OUTPUT_ROOT=/config/abc
ENV

AB_DB_ROOT=/explicit/ab \
SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/machine.env" \
bash -c '
  set -euo pipefail
  source "$1"
  test "$AB_DB_ROOT" = "/explicit/ab"
  test "$ABC_OUTPUT_ROOT" = "/config/abc"
  test "$ABC_REPORT_DIR" = "/config/abc/reports"
' bash "$runtime_env"

SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/machine.env" \
bash -c '
  set -euo pipefail
  source "$1"
  test "$SORANOHA_STATE_ROOT" = "/machine/state"
  test "$AB_DB_ROOT" = "/config/ab-validator"
  test "$AB_MORPH_WAREHOUSE_DIR" = "/config/ab-validator/morph-warehouse"
  test "$AB_AAT_RUN_SET" = "$SORANOHA_WORKSPACE_ROOT/ab-validator/reports/aat-fidelity/run-sets/current.json"
  test "$AB_AOZORA2HTML_AAT_DIR" = "/config/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter"
  test "$ABC_REPORT_DIR" = "/config/abc/reports"
  test "$AB_TEI_EAJ_WORKSET" = "/config/abc/reports/tei-eaj-aozora/tei-eaj-aozora-workset-export.json"
' bash "$runtime_env"

SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/missing.env" \
bash -c '
  set -euo pipefail
  source "$1"
  case "$AB_DB_ROOT" in
    "$SORANOHA_WORKSPACE_ROOT"/ab-validator/scratch/state) ;;
    *) echo "unexpected AB_DB_ROOT=$AB_DB_ROOT" >&2; exit 1 ;;
  esac
  case "$ABC_OUTPUT_ROOT" in
    "$SORANOHA_WORKSPACE_ROOT"/abc/out) ;;
    *) echo "unexpected ABC_OUTPUT_ROOT=$ABC_OUTPUT_ROOT" >&2; exit 1 ;;
  esac
  case "$AB_DB_ROOT $ABC_OUTPUT_ROOT $ABC_REPORT_DIR" in
    */db/*) echo "repo-local defaults must not point at /db: $AB_DB_ROOT $ABC_OUTPUT_ROOT $ABC_REPORT_DIR" >&2; exit 1 ;;
  esac
' bash "$runtime_env"

echo "runtime config smoke ok"
