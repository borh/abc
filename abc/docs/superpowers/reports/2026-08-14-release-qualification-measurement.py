#!/usr/bin/env python3
"""Disposable, deterministic measurement harness for the release-qualification
design (docs/superpowers/specs/2026-08-14-release-qualification-by-output-identity-design.md).

Answers, over the official catalog-joined population:
  Q1/Q4/Q8  cost of parser, converter, and each projected digest
  Q6        converter determinism (two full passes compared per work)
  Q7        the population itself

NOT a production instrument. In particular the projection canonicalization here
is Python `json.dumps(sort_keys=True)`, NOT `rfc8785-safe-integer-json-string-v1`
(spec E17): digests are valid evidence for cost and determinism and are NOT
approvable values.

Archive reading mirrors abc.tools.source-bundle: an archive the zip reader
rejects is retried at earlier EOCD candidates, because some shipped Aozora
archives carry a decoy EOCD after the intact archive. Using a bare zipfile
reader here silently drops such works and redefines the population.

Usage:
  ./2026-08-14-release-qualification-measurement.py --out DIR \
      [--corpus PATH] [--parser BIN] [--converter BIN] [--mapping FILE] [--jobs N]
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import os
import pathlib
import re
import subprocess
import sys
import time
import zipfile
from concurrent.futures import ProcessPoolExecutor

PROJECTIONS = {
    "aat": ("aat-behavior-v1", "meta.adapter_version"),
    "parser_ir": ("parser-ir-behavior-v1", "derived_from.aat_adapter_version"),
    "divergence": ("divergence-behavior-v1", "aat.adapter_version"),
}
CANONICALIZATION = "python-json-dumps-sorted-v0 (NOT rfc8785-safe-integer-json-string-v1)"
CARD_ZIP = re.compile(r"^cards/([0-9]{6})/files/[^/]+\.zip$")
EOCD = b"PK\x05\x06"


def sha256_file(p) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return "sha256:" + h.hexdigest()


def read_member(zf: zipfile.ZipFile, name: str):
    """(bytes, crc_mismatch). ABC's admission reader (java.util.zip, via
    abc.tools.source-bundle/inspect-zip) admits members whose stored CRC-32
    disagrees with their content — e.g. cards/001393/files/50710_ruby_36965.zip.
    Python's zipfile rejects them, so a bare read would silently shrink the
    qualified population below what publication accepts."""
    try:
        return zf.read(name), False
    except zipfile.BadZipFile as e:
        if "Bad CRC-32" not in str(e):
            raise
        with zf.open(name) as fh:
            fh._expected_crc = None  # follow the authoritative reader
            return fh.read(), True


def open_zip_with_recovery(raw: bytes):
    """(ZipFile, trimmed_bytes). Mirrors source-bundle trailing-garbage recovery."""
    try:
        return zipfile.ZipFile(io.BytesIO(raw)), 0
    except zipfile.BadZipFile:
        pass
    ends = [m.start() + 22 for m in re.finditer(re.escape(EOCD), raw)]
    for end in sorted((e for e in ends if e < len(raw)), reverse=True)[:3]:
        try:
            return zipfile.ZipFile(io.BytesIO(raw[:end])), len(raw) - end
        except zipfile.BadZipFile:
            continue
    raise zipfile.BadZipFile("unrecoverable archive")


def build_population(corpus: pathlib.Path):
    catalog_zip = corpus / "index_pages/list_person_all_extended_utf8.zip"
    cz = zipfile.ZipFile(catalog_zip)
    name = [n for n in cz.namelist() if n.endswith(".csv")][0]
    idx = {}
    for row in csv.DictReader(io.TextIOWrapper(cz.open(name), encoding="utf-8-sig")):
        url = (row.get("テキストファイルURL") or "").strip()
        if url:
            idx[url.split("/")[-1]] = row
    rows, recovered, rejected = [], 0, 0
    for p in sorted(corpus.rglob("cards/*/files/*.zip")):
        rel = str(p.relative_to(corpus))
        m = CARD_ZIP.match(rel)
        if not m or p.name not in idx:
            rejected += 1
            continue
        raw = p.read_bytes()
        zf, trimmed = open_zip_with_recovery(raw)
        if trimmed:
            recovered += 1
        members = sorted(n for n in zf.namelist() if n.lower().endswith(".txt"))
        if not members:
            continue
        cat = idx[p.name]
        data, crc_mismatch = read_member(zf, members[0])
        rows.append({
            "slug": "%s_%s_%s_%s" % (cat.get("作品ID"), cat.get("人物ID"), m.group(1), p.name[:-4]),
            "text_zip_relpath": rel,
            "primary_text_member": members[0],
            "archive_hash": "sha256:" + hashlib.sha256(raw).hexdigest(),
            "primary_text_hash": "sha256:" + hashlib.sha256(data).hexdigest(),
            "trailing_garbage_trimmed": trimmed,
            "crc_mismatch": crc_mismatch,
            "txt_member_count": len(members),
        })
    return rows, recovered, rejected, sha256_file(catalog_zip)


def _extract(args):
    corpus, out, row = args
    raw = (pathlib.Path(corpus) / row["text_zip_relpath"]).read_bytes()
    zf, _ = open_zip_with_recovery(raw)
    data, _crc = read_member(zf, row["primary_text_member"])
    (pathlib.Path(out) / "src" / (row["slug"] + ".txt")).write_bytes(data)


def _parse(args):
    parser_bin, out, slug = args
    src = pathlib.Path(out) / "src" / (slug + ".txt")
    dst = pathlib.Path(out) / "aat" / (slug + ".json")
    with open(src, "rb") as i, open(dst, "wb") as o:
        r = subprocess.run([parser_bin, "--mode", "aat"], stdin=i, stdout=o, stderr=subprocess.DEVNULL)
    return slug if r.returncode != 0 else None


def _convert(args):
    conv, mapping, out, slug, work_hash, npass = args
    o = pathlib.Path(out)
    r = subprocess.run([conv, "convert",
                        "--aat", str(o / "aat" / (slug + ".json")),
                        "--mapping", mapping,
                        "--work-content-hash", work_hash,
                        "--parser-ir-out", str(o / f"ir{npass}" / (slug + ".json")),
                        "--divergence-out", str(o / f"div{npass}" / (slug + ".json"))],
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    return slug if r.returncode != 0 else None


def _project(args):
    path, drop_path = args
    with open(path, "rb") as fh:
        doc = json.load(fh)
    parts = drop_path.split(".")
    node = doc
    for k in parts[:-1]:
        node = node.get(k) if isinstance(node, dict) else None
        if node is None:
            break
    if isinstance(node, dict):
        node.pop(parts[-1], None)
    blob = json.dumps(doc, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()
    return os.path.basename(path), hashlib.sha256(blob).hexdigest()


def aggregate(per: dict) -> str:
    top = hashlib.sha256()
    for k in sorted(per):
        top.update(k.encode()); top.update(b"\0")
        top.update(bytes.fromhex(per[k])); top.update(b"\n")
    return "sha256:" + top.hexdigest()


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--corpus", default=os.environ.get("QUAL_CORPUS", ""))
    ap.add_argument("--parser", default=os.environ.get("QUAL_PARSER", ""))
    ap.add_argument("--converter", default=os.environ.get("QUAL_CONVERTER", ""))
    ap.add_argument("--mapping", default=os.environ.get("QUAL_MAPPING", ""))
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 8)
    a = ap.parse_args()
    for req in ("corpus", "parser", "converter", "mapping"):
        if not getattr(a, req):
            print(f"--{req} is required", file=sys.stderr)
            return 2

    out = pathlib.Path(a.out)
    for d in ("src", "aat", "ir1", "div1", "ir2", "div2"):
        (out / d).mkdir(parents=True, exist_ok=True)
    corpus = pathlib.Path(a.corpus)
    timings, t0 = {}, time.time()

    rows, recovered, rejected, catalog_hash = build_population(corpus)
    timings["population_seconds"] = round(time.time() - t0, 1)
    pop_path = out / "population.jsonl"
    pop_path.write_text(
        "".join(json.dumps(r, ensure_ascii=False, sort_keys=True) + "\n" for r in rows))
    pop_hash = sha256_file(pop_path)

    with ProcessPoolExecutor(a.jobs) as ex:
        t = time.time()
        list(ex.map(_extract, [(str(corpus), str(out), r) for r in rows], chunksize=32))
        timings["extract_seconds"] = round(time.time() - t, 1)

        t = time.time()
        parse_fail = [s for s in ex.map(_parse, [(a.parser, str(out), r["slug"]) for r in rows],
                                        chunksize=32) if s]
        timings["parse_seconds"] = round(time.time() - t, 1)

        work_hashes = {}
        for r in rows:
            with open(out / "aat" / (r["slug"] + ".json"), "rb") as fh:
                work_hashes[r["slug"]] = json.load(fh)["meta"]["source_hash"]

        convert_fail = {}
        for npass in (1, 2):
            t = time.time()
            jobs = [(a.converter, a.mapping, str(out), r["slug"], work_hashes[r["slug"]], npass)
                    for r in rows]
            convert_fail[npass] = [s for s in ex.map(_convert, jobs, chunksize=32) if s]
            timings[f"convert_pass{npass}_seconds"] = round(time.time() - t, 1)

        digests, projection_timings = {}, {}
        for label, subdir in (("aat", "aat"), ("parser_ir", "ir1"), ("parser_ir_p2", "ir2"),
                              ("divergence", "div1"), ("divergence_p2", "div2")):
            key = label.replace("_p2", "")
            _, drop = PROJECTIONS[key]
            files = sorted(str(p) for p in (out / subdir).glob("*.json"))
            t = time.time()
            per = dict(ex.map(_project, [(f, drop) for f in files], chunksize=32))
            projection_timings[label] = round(time.time() - t, 1)
            digests[label] = per

    summary = {
        "schema_version": "release-qualification-measurement-v1",
        "not_approvable": True,
        "projection_canonicalization": CANONICALIZATION,
        "identities": {
            "corpus": str(corpus),
            "catalog_zip_hash": catalog_hash,
            "parser_bin": a.parser, "parser_bin_hash": sha256_file(a.parser),
            "converter_bin": a.converter, "converter_bin_hash": sha256_file(a.converter),
            "mapping": a.mapping, "mapping_hash": sha256_file(a.mapping),
            "projections": {k: v[0] for k, v in PROJECTIONS.items()},
            "harness_hash": sha256_file(__file__),
        },
        "population": {
            "qualified_works": len(rows),
            "archives_recovered_by_eocd_trim": recovered,
            "zips_rejected_not_catalog_text": rejected,
            "zips_with_multiple_txt_members": sum(1 for r in rows if r["txt_member_count"] > 1),
            "members_with_crc_mismatch": sum(1 for r in rows if r["crc_mismatch"]),
            "population_manifest": "population.jsonl",
            "population_manifest_hash": pop_hash,
        },
        "failures": {"parse": parse_fail,
                     "convert_pass1": convert_fail[1], "convert_pass2": convert_fail[2]},
        "timings_seconds": {**timings, "projection": projection_timings, "jobs": a.jobs},
        "aggregate_digests": {k: aggregate(v) for k, v in digests.items()},
        "determinism": {
            "parser_ir_differing_works":
                sum(1 for k in digests["parser_ir"] if digests["parser_ir"][k] != digests["parser_ir_p2"].get(k)),
            "divergence_differing_works":
                sum(1 for k in digests["divergence"] if digests["divergence"][k] != digests["divergence_p2"].get(k)),
        },
    }
    (out / "summary.json").write_text(json.dumps(summary, indent=2, ensure_ascii=False, sort_keys=True) + "\n")
    print(json.dumps(summary["population"] | summary["determinism"] |
                     {"aggregates": summary["aggregate_digests"]}, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
