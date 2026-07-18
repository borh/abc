#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
exec python3 "$repo_root/abc/tools/parser_rq_campaign_orchestrator.py" "$@"
