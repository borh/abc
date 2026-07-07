#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/aat-fidelity-env.sh"

out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora-epub3-aat-full-smoke"
corpus="$out_dir/corpus"
work_ids="$out_dir/work-ids.json"

rm -rf "$out_dir"
mkdir -p "$corpus/cards/000001/files"

python - "$corpus/cards/000001/files/1_ruby.zip" <<'PY'
import sys
from zipfile import ZIP_DEFLATED, ZipFile

source = """テスト作品
テスト著者

-------------------------------------------------------
凡例
-------------------------------------------------------

吾輩《わがはい》は猫である。

底本：テスト出版
"""

with ZipFile(sys.argv[1], "w", ZIP_DEFLATED) as zf:
    zf.writestr("sample.txt", source.encode("utf-8"))
PY

printf '["000001_1"]\n' > "$work_ids"

"$repo_root/reports/aat-fidelity/run-aozora-epub3-aat-full.sh" \
  --corpus "$corpus" \
  --out-dir "$out_dir/run" \
  --work-ids "$work_ids" \
  --jobs 1 \
  --timeout 60s \
  --report-id aozora-epub3-aat-full-smoke \
  --force

test -f "$out_dir/run/index.json"
test -f "$out_dir/run/metadata.json"
test -f "$out_dir/run/triage/index.md"
rg -n "aozora-epub3-aat-full-smoke" "$out_dir/run/triage/index.md"
find "$out_dir/run/check-reports" -name '*.json' | rg -n '.'
find "$out_dir/run/aat" -name '*.json' | rg -n '.'
jq -e '.adapter_version | startswith("aozora-epub3-adapter ")' "$out_dir/run/metadata.json"
