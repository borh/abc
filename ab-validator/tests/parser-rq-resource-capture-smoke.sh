#!/usr/bin/env bash
set -euo pipefail

repo="${1:-$(cd "$(dirname "$0")/.." && pwd)}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

python - "$repo" "$tmp/first.json" <<'PY'
import importlib.util, json, pathlib, sys
root, output = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
path = root / "reports/parser-ir/parser-rq-resource-capture.py"
spec = importlib.util.spec_from_file_location("capture", path)
module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
policy = {"work_ids": ["a", "b"]}
value = module.capture_index(policy, lambda work: {"work_id": work, "status": "measured", "peak_cgroup_memory_bytes": 1024})
output.write_text(json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n")
PY
python - "$repo" "$tmp/second.json" <<'PY'
import importlib.util, json, pathlib, sys
root, output = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])
path = root / "reports/parser-ir/parser-rq-resource-capture.py"
spec = importlib.util.spec_from_file_location("capture", path)
module = importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
policy = {"work_ids": ["a", "b"]}
value = module.capture_index(policy, lambda work: {"work_id": work, "status": "measured", "peak_cgroup_memory_bytes": 1024})
output.write_text(json.dumps(value, sort_keys=True, separators=(",", ":")) + "\n")
PY
cmp "$tmp/first.json" "$tmp/second.json"
