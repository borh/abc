#!/usr/bin/env python3
import json
import shutil
import sys
from pathlib import Path


def main() -> int:
    if len(sys.argv) != 4:
        print(
            "usage: sync-schema-mirror.py <schema-contracts.json> <abc-schema-dir> <mirror-dir>",
            file=sys.stderr,
        )
        return 2

    manifest_path = Path(sys.argv[1])
    abc_schema_dir = Path(sys.argv[2])
    mirror_dir = Path(sys.argv[3])

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    schema_names = sorted(Path(row["path"]).name for row in manifest["schemas"])

    mirror_dir.mkdir(parents=True, exist_ok=True)
    for name in schema_names:
        shutil.copyfile(abc_schema_dir / name, mirror_dir / name)

    expected = set(schema_names)
    for path in mirror_dir.glob("*.schema.json"):
        if path.name not in expected:
            path.unlink()

    print(f"synced {len(schema_names)} schema mirror files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
