#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

case_id=""
source_path=""
upstream_xhtml=""
out_dir="${AB_AAT_FIDELITY_XHTML_OUT_DIR:-/db/ab-validator/aat-fidelity/xhtml-observations}"
db_path="${AB_AAT_FIDELITY_DB:-/db/ab-validator/aat-fidelity/cross-adapter/fidelity.duckdb}"
report_id="${AB_AAT_FIDELITY_REPORT_ID:-cross-adapter}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --case-id)
      case_id="$2"
      shift 2
      ;;
    --source)
      source_path="$2"
      shift 2
      ;;
    --upstream-xhtml)
      upstream_xhtml="$2"
      shift 2
      ;;
    --out-dir)
      out_dir="$2"
      shift 2
      ;;
    --db)
      db_path="$2"
      shift 2
      ;;
    --report-id)
      report_id="$2"
      shift 2
      ;;
    *)
      printf 'unknown argument: %s\n' "$1" >&2
      exit 2
      ;;
  esac
done

if [[ -z "$case_id" || -z "$source_path" || -z "$upstream_xhtml" ]]; then
  cat >&2 <<'EOF'
usage: compare-aozora-upstream-xhtml.sh \
  --case-id CASE \
  --source source.txt \
  --upstream-xhtml upstream.xhtml \
  [--out-dir /db/...] \
  [--db /db/.../fidelity.duckdb] \
  [--report-id cross-adapter]
EOF
  exit 2
fi

mkdir -p "$out_dir" "$(dirname "$db_path")"
local_xhtml="$out_dir/$case_id.local.xhtml"

"$repo_root/adapters/aozora2html/aozora2html-adapter" --mode html \
  < "$source_path" \
  > "$local_xhtml"

duckdb_bin="${DUCKDB:-duckdb}"
if [[ -x /etc/profiles/per-user/bor/bin/duckdb ]]; then
  duckdb_bin=/etc/profiles/per-user/bor/bin/duckdb
fi
libstdcxx_path=""
if command -v "$duckdb_bin" >/dev/null 2>&1; then
  libstdcxx_path="$(ldd "$duckdb_bin" | awk '/libstdc\+\+/{print $3; exit}')"
fi
loader_env=()
if [[ -n "$libstdcxx_path" ]]; then
  loader_env=(env "LD_LIBRARY_PATH=$(dirname "$libstdcxx_path"):${LD_LIBRARY_PATH:-}")
fi

"${loader_env[@]}" uv run --isolated --no-project \
  --with 'duckdb>=1.1' \
  --with 'lxml>=5' \
  "$repo_root/reports/aat-fidelity/compare-xhtml-sources.py" \
  --db "$db_path" \
  --report-id "$report_id" \
  --case-id "$case_id" \
  --upstream-xhtml "$upstream_xhtml" \
  --local-xhtml "$local_xhtml"

printf 'local_xhtml=%s\n' "$local_xhtml"
printf 'db_path=%s\n' "$db_path"
