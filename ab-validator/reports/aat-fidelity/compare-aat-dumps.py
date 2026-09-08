#!/usr/bin/env python3
"""Parity between two AAT dumps (directories of per-work *.json).

Two instruments over the same allowlist (the identity pointers
/meta/adapter and /meta/adapter_version):

  semantic  parsed-JSON equality after REMOVING the two pointers. Key order
            and number formatting are invisible. Localization diagnostic.
  bytes     (--bytes) raw-byte equality after SUBSTITUTING each pointer's
            exact serialized occurrence with a fixed placeholder. The
            occurrence must appear exactly once per document (fail closed:
            exit 2) so drift can never hide inside the substitution.

Without --bytes the exit code reflects semantic parity; with --bytes it reflects byte parity, and the semantic result is
still computed and reported for localization.

Exit 0 = parity; 1 = divergence; 2 = usage/reference error.
"""

import argparse
import json
import pathlib
import sys

POINTERS = ("adapter", "adapter_version")
PLACEHOLDER = "__AB_PARITY_IDENTITY__"


def normalize(doc):
    meta = doc.get("meta")
    if isinstance(meta, dict):
        meta = dict(meta)
        for key in POINTERS:
            meta.pop(key, None)
        doc = dict(doc)
        doc["meta"] = meta
    return doc


def substitute_identity(raw: bytes, path: pathlib.Path) -> bytes:
    doc = json.loads(raw)
    meta = doc.get("meta")
    if not isinstance(meta, dict):
        print(f"ERROR: {path}: no /meta object", file=sys.stderr)
        raise SystemExit(2)
    for key in POINTERS:
        if key not in meta:
            print(f"ERROR: {path}: missing /meta/{key}", file=sys.stderr)
            raise SystemExit(2)
        value = json.dumps(meta[key], ensure_ascii=False).encode("utf-8")
        # serde_json emits compact (`"k":v`); tolerate a single space after
        # the colon in case a producer pretty-prints. Total must be exactly 1.
        needles = [b'"%s":%s' % (key.encode(), value), b'"%s": %s' % (key.encode(), value)]
        counts = [raw.count(n) for n in needles]
        if sum(counts) != 1:
            print(
                f"ERROR: {path}: expected exactly 1 serialized occurrence "
                f"of /meta/{key}, found {sum(counts)}",
                file=sys.stderr,
            )
            raise SystemExit(2)
        needle = needles[0] if counts[0] else needles[1]
        raw = raw.replace(needle, b'"%s":"%s"' % (key.encode(), PLACEHOLDER.encode()), 1)
    return raw


def load_dir(d: pathlib.Path) -> dict:
    files = {p.name: p for p in sorted(d.glob("*.json"))}
    if not files:
        print(f"ERROR: no *.json under {d}", file=sys.stderr)
        raise SystemExit(2)
    return files


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("dir_a")
    ap.add_argument("dir_b")
    ap.add_argument("--bytes", action="store_true")
    args = ap.parse_args()
    a_files = load_dir(pathlib.Path(args.dir_a))
    b_files = load_dir(pathlib.Path(args.dir_b))
    missing = sorted(set(a_files) ^ set(b_files))
    shared = sorted(set(a_files) & set(b_files))
    sem_diverged, byte_diverged = [], []
    for name in shared:
        raw_a = a_files[name].read_bytes()
        raw_b = b_files[name].read_bytes()
        if normalize(json.loads(raw_a)) != normalize(json.loads(raw_b)):
            sem_diverged.append(name)
        if args.bytes and substitute_identity(raw_a, a_files[name]) != substitute_identity(
            raw_b, b_files[name]
        ):
            byte_diverged.append(name)
    summary = {
        "compared": len(shared),
        "missing_count": len(missing),
        "missing_sample": missing[:20],
        "semantic": {
            "diverged_count": len(sem_diverged),
            "diverged_sample": sem_diverged[:20],
        },
    }
    if args.bytes:
        summary["bytes"] = {
            "diverged_count": len(byte_diverged),
            "diverged_sample": byte_diverged[:20],
        }
    print(json.dumps(summary, indent=2))
    gate_diverged = byte_diverged if args.bytes else sem_diverged
    return 0 if not missing and not gate_diverged else 1


if __name__ == "__main__":
    raise SystemExit(main())
