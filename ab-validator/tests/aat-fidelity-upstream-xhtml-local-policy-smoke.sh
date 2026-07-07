#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/upstream-xhtml-local-policy-smoke"
manifest="$out_dir/manifest.tsv"

rm -rf "$out_dir"
mkdir -p "$out_dir"

cat > "$manifest" <<'TSV'
case_id	source	upstream_xhtml
remote.disallowed	https://example.test/source.zip	https://example.test/upstream.xhtml
TSV

if "$repo_root/reports/aat-fidelity/run-upstream-xhtml-observations.sh" \
  --manifest "$manifest" \
  --out-dir "$out_dir/observations" \
  --db "$out_dir/fidelity.duckdb" \
  --report-id local-policy > "$out_dir/stdout.txt" 2> "$out_dir/stderr.txt"; then
  echo "runner unexpectedly accepted remote manifest inputs" >&2
  exit 1
fi

rg -n 'remote inputs are not allowed' "$out_dir/stderr.txt"

echo "aat fidelity upstream xhtml local policy smoke ok: $out_dir"
