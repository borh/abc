#!/usr/bin/env python3
"""Capture explicit publication evidence into immutable content-addressed blobs."""

from __future__ import annotations

import argparse
import hashlib
import json
import pathlib
import sys
from typing import Any, NamedTuple


class CaptureError(ValueError):
    pass


class CaptureResult(NamedTuple):
    manifest: dict[str, Any]
    index: dict[str, Any]
    manifest_bytes: bytes
    index_bytes: bytes


def canonical_bytes(value: object) -> bytes:
    return (
        json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")) + "\n"
    ).encode()


def _digest(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def put_blob(store_root: pathlib.Path, value: bytes, media_type: str) -> dict[str, Any]:
    digest = _digest(value)
    suffix = "json" if media_type == "application/json" else "bin"
    locator = pathlib.Path("sha256") / digest[:2] / f"{digest}.{suffix}"
    destination = store_root / locator
    destination.parent.mkdir(parents=True, exist_ok=True)
    try:
        with destination.open("xb") as handle:
            handle.write(value)
    except FileExistsError:
        if destination.read_bytes() != value:
            raise CaptureError(f"content-address collision at {locator}") from None
    return {
        "sha256": f"sha256:{digest}",
        "bytes": len(value),
        "media_type": media_type,
        "locator": locator.as_posix(),
    }


def validate_capture_input(value: object, corpus: dict[str, Any]) -> list[str]:
    if not isinstance(value, dict) or not isinstance(value.get("works"), list):
        return ["capture input must contain an explicit works array"]
    expected = [row["work_id"] for row in corpus["entries"]]
    actual = [row.get("work_id") for row in value["works"] if isinstance(row, dict)]
    errors = []
    if len(actual) != len(set(actual)) or set(actual) != set(expected):
        errors.append("works do not have exact pinned corpus membership")
    source_by_work = {row["work_id"]: row["source_sha256"] for row in corpus["entries"]}
    for row in value["works"]:
        if not isinstance(row, dict):
            errors.append("work row must be an object")
            continue
        if row.get("source_sha256") != source_by_work.get(row.get("work_id")):
            errors.append(f"source identity mismatch for {row.get('work_id')}")
        disposition = row.get("parser_disposition")
        if disposition == "parsed" and not isinstance(row.get("publication"), dict):
            errors.append(f"parsed work {row.get('work_id')} lacks publication evidence")
        if disposition in {"failed", "timeout"} and "publication" in row:
            errors.append(f"non-parsed work {row.get('work_id')} carries publication evidence")
        if disposition not in {"parsed", "failed", "timeout"}:
            errors.append(f"invalid parser disposition for {row.get('work_id')}")
    return errors


def capture(value: dict[str, Any], store_root: pathlib.Path) -> CaptureResult:
    corpus = value["corpus"]
    errors = validate_capture_input(value, corpus)
    if errors:
        raise CaptureError("; ".join(errors))
    authority = value["authority"]
    expected = [row["work_id"] for row in corpus["entries"]]
    input_by_work = {row["work_id"]: row for row in value["works"]}
    index_records = []
    manifest_blobs = []
    for work_id in expected:
        source = next(row for row in corpus["entries"] if row["work_id"] == work_id)
        supplied = input_by_work[work_id]
        record = {
            "schema_id": "https://w3id.org/soranoha/schemas/parser-rq-publication-work.schema.json",
            "schema_version": "1.0.0",
            "work_id": work_id,
            "source_sha256": source["source_sha256"],
            "qualification_identity_ref": authority["qualification_identity_ref"],
            "parser_disposition": supplied["parser_disposition"],
            "policy_hash": authority["policy_hash"],
            "preservation_schema_hash": authority["preservation_schema_hash"],
            "validator_semantics_hash": authority["validator_semantics_hash"],
            "census_hash": authority["census_hash"],
        }
        if supplied["parser_disposition"] == "parsed":
            record["publication"] = supplied["publication"]
        else:
            record["failure"] = supplied.get("failure", {"kind": supplied["parser_disposition"]})
        ref = put_blob(store_root, canonical_bytes(record), "application/json")
        index_records.append(
            {
                "work_id": work_id,
                "source_sha256": source["source_sha256"],
                "locator": ref["locator"],
                "ref": {key: ref[key] for key in ("sha256", "bytes", "media_type")},
            }
        )
        manifest_blobs.append(
            {
                "locator": ref["locator"],
                "ref": {key: ref[key] for key in ("sha256", "bytes", "media_type")},
            }
        )
    index = {
        "schema_id": "https://w3id.org/soranoha/schemas/parser-rq-publication-index.schema.json",
        "schema_version": "1.0.0",
        "corpus_id": corpus["corpus_id"],
        "corpus_snapshot_hash": corpus["corpus_snapshot_hash"],
        "corpus_list_hash": corpus["list_hash"],
        **authority,
        "expected_work_ids": expected,
        "records": index_records,
    }
    index_bytes = canonical_bytes(index)
    index_ref = put_blob(store_root, index_bytes, "application/json")
    manifest_blobs.append(
        {
            "locator": index_ref["locator"],
            "ref": {key: index_ref[key] for key in ("sha256", "bytes", "media_type")},
        }
    )
    manifest = {"blobs": manifest_blobs}
    return CaptureResult(manifest, index, canonical_bytes(manifest), index_bytes)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", required=True, type=pathlib.Path)
    parser.add_argument("--store", required=True, type=pathlib.Path)
    parser.add_argument("--manifest-out", required=True, type=pathlib.Path)
    parser.add_argument("--index-out", required=True, type=pathlib.Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        result = capture(json.loads(args.input.read_text()), args.store)
        args.manifest_out.write_bytes(result.manifest_bytes)
        args.index_out.write_bytes(result.index_bytes)
        return 0
    except (CaptureError, OSError, KeyError, json.JSONDecodeError) as error:
        print(error, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
