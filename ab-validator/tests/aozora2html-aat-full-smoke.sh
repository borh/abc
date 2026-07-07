#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="${AB_DB_ROOT:-$repo_root/scratch/state}/aat-fidelity/aozora2html-aat-full-smoke"
corpus="$out_dir/corpus"
work_ids="$out_dir/work-ids.json"

rm -rf "$out_dir"
mkdir -p "$corpus/cards/000250/files"

python - "$repo_root" "$corpus" <<'PY'
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile
import sys

repo = Path(sys.argv[1])
corpus = Path(sys.argv[2])
fixture = repo / "tests/fixtures/kunten-source-excerpt.txt"
zip_path = corpus / "cards/000250/files/4644_ruby_15596.zip"
with ZipFile(zip_path, "w", ZIP_DEFLATED) as zf:
    zf.writestr("hoo_kansho07.txt", fixture.read_bytes())
PY

printf '["000250_4644"]\n' > "$work_ids"

"$repo_root/reports/aat-fidelity/run-aozora2html-aat-full.sh" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 180s \
  --report-id aozora2html-aat-full-smoke \
  --force

test -f "$out_dir/run/index.json"
test -f "$out_dir/run/metadata.json"
test -f "$out_dir/run/triage/index.md"
rg -n "aozora2html-aat-full-smoke" "$out_dir/run/triage/index.md"
find "$out_dir/run/check-reports" -name '*.json' | rg -n '.'
find "$out_dir/run/aat" -name '*.json' | rg -n '.'
