#!/usr/bin/env python3
"""Semantic-JSON parity between two AAT dumps.

Gate definition (Phase 1 Gate A): equality of parsed JSON documents after
normalizing EXACTLY one pointer — /meta/adapter_version — which embeds the
producing binary's identity (upstream store path vs fork shim) and is the
only sanctioned difference. This is semantic JSON equality, not byte
equality: key order and float formatting differences would be invisible,
which is acceptable because both sides are serde_json output.

Exit 0 = parity; 1 = divergence; 2 = usage/reference error.
"""
import json
import pathlib
import sys


def normalize(doc):
    meta = doc.get("meta")
    if isinstance(meta, dict):
        meta = dict(meta)
        meta.pop("adapter_version", None)
        doc = dict(doc)
        doc["meta"] = meta
    return doc


def load_dir(d: pathlib.Path) -> dict:
    files = {p.name: p for p in sorted(d.glob("*.json"))}
    if not files:
        print(f"ERROR: no *.json under {d}", file=sys.stderr)
        raise SystemExit(2)
    return files


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__, file=sys.stderr)
        return 2
    a_files = load_dir(pathlib.Path(sys.argv[1]))
    b_files = load_dir(pathlib.Path(sys.argv[2]))
    missing = sorted(set(a_files) ^ set(b_files))
    diverged = []
    for name in sorted(set(a_files) & set(b_files)):
        a = normalize(json.loads(a_files[name].read_text()))
        b = normalize(json.loads(b_files[name].read_text()))
        if a != b:
            diverged.append(name)
    print(json.dumps({
        "compared": len(set(a_files) & set(b_files)),
        "missing_count": len(missing),
        "missing_sample": missing[:20],
        "diverged_count": len(diverged),
        "diverged_sample": diverged[:20],
    }, indent=2))
    return 0 if not missing and not diverged else 1


if __name__ == "__main__":
    raise SystemExit(main())
