#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

nix develop "${repo_root}#aozora2html" --command bash -lc "
python3 - <<'PY'
import json
from pathlib import Path
try:
  import tomllib as tomli
except ImportError:
  import tomli
import jsonschema

root = Path('$repo_root')
schema = json.loads((root / 'data' / 'adapter-fidelity-notes.schema.json').read_text())
notes = tomli.loads((root / 'data' / 'adapter-fidelity-notes.toml').read_text())
jsonschema.validate(notes, schema)
assert isinstance(notes.get('note'), list)
print(f\"validated {len(notes['note'])} adapter fidelity notes\")
PY
"
