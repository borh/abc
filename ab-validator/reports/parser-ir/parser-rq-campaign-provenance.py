#!/usr/bin/env python3
"""Measure executable reproducibility and evidence-store replication.

This module emits evidence values. Release authority remains in the Clojure
campaign verifier, which authenticates the closed records before promotion.
"""

from __future__ import annotations

import hashlib
from pathlib import Path, PurePosixPath
from typing import NamedTuple


CHUNK_BYTES = 1024 * 1024
EXECUTABLE_FIELDS = (
    "name",
    "nix_output",
    "nar_hash",
    "sha256",
    "bytes",
    "adapter",
    "adapter_version",
    "parser_git_rev",
    "argv_template",
)


class LogicalBlob(NamedTuple):
    sha256: str
    bytes: int
    media_type: str
    locator: str


def sha256_bytes(payload: bytes) -> str:
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def _stream_identity(path: Path) -> tuple[str, int]:
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as stream:
        while chunk := stream.read(CHUNK_BYTES):
            digest.update(chunk)
            size += len(chunk)
    return "sha256:" + digest.hexdigest(), size


def executable_record(
    path: Path, nix_output: dict[str, object], argv: list[str]
) -> dict[str, object]:
    digest, size = _stream_identity(path)
    return {
        "name": nix_output["name"],
        "nix_output": nix_output["nix_output"],
        "nar_hash": nix_output["nar_hash"],
        "sha256": digest,
        "bytes": size,
        "adapter": nix_output["adapter"],
        "adapter_version": nix_output["adapter_version"],
        "parser_git_rev": nix_output["parser_git_rev"],
        "argv_template": list(argv),
    }


def compare_builds(first: dict[str, object], second: dict[str, object]) -> dict[str, object]:
    if first.get("output_ref") != second.get("output_ref"):
        return {"status": "unavailable", "reason": "Nix output identity differs"}
    first_executables = first.get("executables")
    second_executables = second.get("executables")
    if not isinstance(first_executables, list) or not isinstance(second_executables, list):
        return {"status": "unavailable", "reason": "executable records are absent"}
    keyed_first = {record.get("name"): record for record in first_executables}
    keyed_second = {record.get("name"): record for record in second_executables}
    if set(keyed_first) != set(keyed_second) or len(keyed_first) != len(first_executables):
        return {"status": "unavailable", "reason": "executable membership differs"}
    for name in sorted(keyed_first):
        if any(
            keyed_first[name].get(field) != keyed_second[name].get(field)
            for field in EXECUTABLE_FIELDS
        ):
            return {
                "status": "unavailable",
                "reason": f"executable coordinates differ for {name}",
            }
    return {
        "status": "reproducible",
        "output_ref": first["output_ref"],
        "executables": [keyed_first[name] for name in sorted(keyed_first)],
    }


def _safe_path(root: Path, locator: str) -> Path | None:
    logical = PurePosixPath(locator)
    if logical.is_absolute() or ".." in logical.parts or not locator:
        return None
    candidate = root.joinpath(*logical.parts)
    try:
        resolved_root = root.resolve(strict=True)
        resolved_candidate = candidate.resolve(strict=True)
    except OSError:
        return None
    if resolved_root not in resolved_candidate.parents:
        return None
    return resolved_candidate


def verify_replicas(blobs: list[LogicalBlob], roots: tuple[Path, Path]) -> dict[str, object]:
    primary, replica = roots
    try:
        primary_resolved = primary.resolve(strict=True)
        replica_resolved = replica.resolve(strict=True)
        if primary_resolved == replica_resolved:
            return {"status": "unavailable", "reason": "replica aliases primary root"}
    except OSError:
        return {"status": "unavailable", "reason": "a replica root is offline"}
    if not blobs or len({blob.sha256 for blob in blobs}) != len(blobs):
        return {"status": "unavailable", "reason": "blob membership is empty or duplicated"}
    records: list[dict[str, object]] = []
    for blob in sorted(blobs, key=lambda value: value.sha256):
        if not blob.media_type or blob.bytes < 0:
            return {"status": "unavailable", "reason": "blob metadata is invalid"}
        primary_path = _safe_path(primary_resolved, blob.locator)
        replica_path = _safe_path(replica_resolved, blob.locator)
        if primary_path is None or replica_path is None:
            return {"status": "unavailable", "reason": "blob locator is absent or unsafe"}
        primary_hash, primary_bytes = _stream_identity(primary_path)
        replica_hash, replica_bytes = _stream_identity(replica_path)
        if (
            primary_hash != blob.sha256
            or replica_hash != blob.sha256
            or primary_bytes != blob.bytes
            or replica_bytes != blob.bytes
        ):
            return {"status": "unavailable", "reason": "replica bytes do not authenticate"}
        records.append(
            {
                "blob": blob._asdict(),
                "primary_rehash": primary_hash,
                "replica_rehash": replica_hash,
            }
        )
    return {"status": "replicated", "blobs": records}
