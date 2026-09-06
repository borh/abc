#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workflow="${1:?usage: run-tei-eaj-probe-workflow.sh WORKFLOW [ARGS...]}"
shift

case "$workflow" in
  alignment-probe) app="tei-eaj-aozora-alignment-probe" ;;
  reports-with-probes) app="tei-eaj-aozora-reports-with-probes" ;;
  *)
    echo "unknown TEI-EAJ probe workflow: $workflow" >&2
    exit 64
    ;;
esac

cd "$repo_root"
probe_root="$(nix build ./ab-validator#ab-aat-to-parser-ir --no-link --print-out-paths)"
export ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN="$probe_root/bin/ab-aat-to-parser-ir"
exec nix run "./ab-validator#$app" -- "$@"
