#!/usr/bin/env python3
"""Fail-closed derivation of the perf-workset flat-file corpus.

`data/perf-workset.json` pins each work's `source_sha256` and, in
`_provenance`, the zip archive (`archive_relpath`, relative to the
resolved aozorabunko corpus root) and the entry inside it
(`zip_entry`) that the flat file was originally extracted from.
`run-perf-workset.py` only ever reads flat files off disk (no zip
handling) and fails closed on any hash mismatch, but it cannot
*materialize* those flat files on a fresh host — that's this script's
job.

For each work: open `<corpus-root>/<_provenance.archive_relpath>` as a
zip, extract `_provenance.zip_entry`, write it to
`<out-dir>/<basename of corpus_relpath>`, then re-hash the bytes just
written and verify they equal `source_sha256`. Any archive/entry
missing, or any hash mismatch, is a fail-closed error (exit 2) — this
is a *derivation* of the pinned corpus, not just a materialization, so
it must be at least as strict as the runner it feeds.

Usage:
  extract-perf-workset-corpus.py --workset data/perf-workset.json \
      --corpus-root <resolved corpus> --out-dir scratch/perf-workset-corpus
"""
import argparse
import hashlib
import json
import pathlib
import sys
import zipfile


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--workset", required=True)
    ap.add_argument("--corpus-root", required=True)
    ap.add_argument("--out-dir", required=True)
    args = ap.parse_args()

    ws = json.loads(pathlib.Path(args.workset).read_text())
    corpus_root = pathlib.Path(args.corpus_root)
    out_dir = pathlib.Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    works = ws["works"]
    ok = 0
    for work in works:
        work_id = work["work_id"]
        prov = work.get("_provenance")
        if not prov:
            print(f"FAIL-CLOSED: {work_id} has no _provenance block in {args.workset}",
                  file=sys.stderr)
            return 2

        archive_path = corpus_root / prov["archive_relpath"]
        zip_entry = prov["zip_entry"]
        if not archive_path.is_file():
            print(f"FAIL-CLOSED: {work_id} archive missing at {archive_path}",
                  file=sys.stderr)
            return 2

        try:
            with zipfile.ZipFile(archive_path) as zf:
                data = zf.read(zip_entry)
        except KeyError:
            print(f"FAIL-CLOSED: {work_id} entry {zip_entry!r} not found in {archive_path}",
                  file=sys.stderr)
            return 2
        except zipfile.BadZipFile as exc:
            print(f"FAIL-CLOSED: {work_id} archive {archive_path} is not a valid zip: {exc}",
                  file=sys.stderr)
            return 2

        out_path = out_dir / pathlib.Path(work["corpus_relpath"]).name
        out_path.write_bytes(data)

        actual = sha256_bytes(out_path.read_bytes())
        expected = work["source_sha256"]
        if actual != expected:
            print(
                f"FAIL-CLOSED: {work_id} derived sha256 {actual} != pinned {expected} "
                f"(wrote {out_path} from {archive_path}::{zip_entry})",
                file=sys.stderr,
            )
            return 2

        print(f"OK: {work_id} -> {out_path} ({actual})")
        ok += 1

    print(f"Extracted and verified {ok}/{len(works)} works into {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
