#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix develop "${repo_root}#aozora2html" --command python3 - "${repo_root}" <<'PY'
import json
import sys
from pathlib import Path
try:
  import tomllib as tomli
except ImportError:
  import tomli
import jsonschema

root = Path(sys.argv[1])
for stem in ['aat-oracle-cases', 'aat-upstream-observations']:
    schema = json.loads((root / 'data' / f'{stem}.schema.json').read_text())
    data = tomli.loads((root / 'data' / f'{stem}.toml').read_text())
    jsonschema.validate(data, schema)
    print(f'validated {stem}')
PY
