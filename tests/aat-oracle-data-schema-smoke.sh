#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix-shell -p 'python3.withPackages(ps: [ ps.jsonschema ps.tomli ])' --run "
python3 - <<'PY'
import json
from pathlib import Path
import tomli
import jsonschema

root = Path('$repo_root')
for stem in ['aat-oracle-cases', 'aat-upstream-observations']:
    schema = json.loads((root / 'data' / f'{stem}.schema.json').read_text())
    data = tomli.loads((root / 'data' / f'{stem}.toml').read_text())
    jsonschema.validate(data, schema)
    print(f'validated {stem}')
PY
"
