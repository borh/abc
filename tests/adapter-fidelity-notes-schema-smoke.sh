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
schema = json.loads((root / 'data' / 'adapter-fidelity-notes.schema.json').read_text())
notes = tomli.loads((root / 'data' / 'adapter-fidelity-notes.toml').read_text())
jsonschema.validate(notes, schema)
assert isinstance(notes.get('note'), list)
print(f"validated {len(notes['note'])} adapter fidelity notes")
PY
