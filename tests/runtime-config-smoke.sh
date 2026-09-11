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
unset AB_TEI_EAJ_WORKSET
unset AB_TEI_P5_ROOT

mkdir -p "$tmp/workspace"

cat > "$tmp/machine.env" <<'ENV'
export SORANOHA_STATE_ROOT=/machine/state
export SORANOHA_REPORT_DIR=/config/reports
ENV

SORANOHA_REPORT_DIR=/explicit/reports \
SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/machine.env" \
bash -c '
  set -euo pipefail
  source "$1"
  test "$SORANOHA_REPORT_DIR" = "/explicit/reports"
  test "$SORANOHA_STATE_ROOT" = "/machine/state"
  test "$AB_TEI_EAJ_WORKSET" = "/explicit/reports/tei-eaj-aozora/tei-eaj-aozora-workset-export.json"
' bash "$runtime_env"

SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/machine.env" \
bash -c '
  set -euo pipefail
  source "$1"
  test "$SORANOHA_STATE_ROOT" = "/machine/state"
  test "$SORANOHA_REPORT_DIR" = "/config/reports"
  test "$AB_TEI_EAJ_WORKSET" = "/config/reports/tei-eaj-aozora/tei-eaj-aozora-workset-export.json"
' bash "$runtime_env"

SORANOHA_WORKSPACE_ROOT="$tmp/workspace" \
SORANOHA_CONFIG="$tmp/missing.env" \
bash -c '
  set -euo pipefail
  source "$1"
  case "$SORANOHA_STATE_ROOT" in
    "$SORANOHA_WORKSPACE_ROOT"/scratch/state) ;;
    *) echo "unexpected SORANOHA_STATE_ROOT=$SORANOHA_STATE_ROOT" >&2; exit 1 ;;
  esac
  case "$SORANOHA_REPORT_DIR" in
    "$SORANOHA_WORKSPACE_ROOT"/out/reports) ;;
    *) echo "unexpected SORANOHA_REPORT_DIR=$SORANOHA_REPORT_DIR" >&2; exit 1 ;;
  esac
  case "$SORANOHA_STATE_ROOT $SORANOHA_REPORT_DIR" in
    */db/*) echo "repo-local defaults must not point at /db: $SORANOHA_STATE_ROOT $SORANOHA_REPORT_DIR" >&2; exit 1 ;;
  esac
' bash "$runtime_env"

echo "runtime config smoke ok"
