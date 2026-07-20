#!/usr/bin/env python3
"""Capture and authenticate the frozen parser-study diagnostic fixture lanes."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
from pathlib import Path, PurePosixPath
from typing import Any


EXPECTED_LANES = (
    ("aozora", "native"),
    ("aozora", "adapter_normalized"),
    ("aozora2", "native"),
    ("aozora2", "adapter_normalized"),
    ("aozora-rs", "native"),
    ("aozora-rs", "adapter_normalized"),
    ("aozora2html", "native"),
    ("aozora2html", "adapter_normalized"),
    ("aozora-epub3", "native"),
    ("aozora-epub3", "adapter_normalized"),
    ("ab-aozora", "native"),
)


def sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def canonical_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")) + "\n"
    ).encode()


def safe_relative(value: str) -> bool:
    path = PurePosixPath(value.replace("\\", "/"))
    return (
        bool(value)
        and not value.startswith(("/", "\\"))
        and not (len(value) >= 2 and value[0].isalpha() and value[1] == ":")
        and all(part not in ("", ".", "..") for part in path.parts)
    )


def _atomic_write(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_bytes(data)
    temporary.replace(path)


def _content_ref(root: Path, path: Path, data: bytes, media_type: str) -> dict[str, Any]:
    return {
        "sha256": sha256(data),
        "bytes": len(data),
        "media_type": media_type,
        "locator": path.relative_to(root).as_posix(),
    }


def _load_object(path: Path) -> dict[str, Any]:
    value = json.loads(path.read_bytes())
    if not isinstance(value, dict):
        raise ValueError(f"{path}: expected a JSON object")
    return value


def _file_identity(path: Path) -> dict[str, Any]:
    digest = hashlib.sha256()
    size = 0
    with path.open("rb") as handle:
        while chunk := handle.read(1024 * 1024):
            digest.update(chunk)
            size += len(chunk)
    return {"sha256": "sha256:" + digest.hexdigest(), "bytes": size}


def _validate_inputs(
    preregistration: dict[str, Any], fixture_path: Path, policy: dict[str, Any]
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    cases = preregistration.get("fixture_manifests", {}).get("diagnostics")
    if not isinstance(cases, dict) or cases.get("sha256") != _file_identity(fixture_path)["sha256"]:
        raise ValueError("diagnostic fixture does not match preregistration")
    fixture = _load_object(fixture_path)
    fixture_cases = fixture.get("cases")
    lanes = policy.get("lanes")
    if not isinstance(fixture_cases, list) or len(fixture_cases) != 4:
        raise ValueError("diagnostic fixture must contain exactly four cases")
    if not isinstance(lanes, list) or [
        (lane.get("candidate"), lane.get("measurement_mode"))
        for lane in lanes
        if isinstance(lane, dict)
    ] != list(EXPECTED_LANES):
        raise ValueError("lane policy does not contain the closed lane set in order")
    return fixture_cases, lanes


def _program_record(programs: dict[str, Any], basename: str) -> tuple[Path, dict[str, str]]:
    record = programs.get(basename)
    if not isinstance(record, dict) or set(record) != {"path", "environment"}:
        raise ValueError(f"missing or invalid program record: {basename}")
    program = Path(record["path"])
    environment = record["environment"]
    if (
        not program.is_absolute()
        or program.name != basename
        or not program.is_file()
        or not os.access(program, os.X_OK)
        or not isinstance(environment, dict)
        or not all(
            isinstance(key, str) and isinstance(value, str) for key, value in environment.items()
        )
    ):
        raise ValueError(f"invalid executable identity: {basename}")
    return program, environment


def capture(
    preregistration_path: Path,
    fixture_path: Path,
    lane_policy_path: Path,
    programs_path: Path,
    bundle_root: Path,
    manifest_out: Path,
    timeout_seconds: float = 300,
) -> dict[str, Any]:
    preregistration = _load_object(preregistration_path)
    policy = _load_object(lane_policy_path)
    programs = _load_object(programs_path)
    cases, lanes = _validate_inputs(preregistration, fixture_path, policy)

    members: list[dict[str, Any]] = []
    seen: set[tuple[str, str, str]] = set()
    for lane in lanes:
        required = {
            "candidate",
            "measurement_mode",
            "parser_revision",
            "adapter_revision",
            "program",
            "argv",
            "projector",
        }
        if not isinstance(lane, dict) or set(lane) != required:
            raise ValueError("lane fields do not match the closed contract")
        program, environment = _program_record(programs, lane["program"])
        program_bytes = program.read_bytes()
        for case in cases:
            case_id = case.get("id")
            source = case.get("source")
            key = (lane["candidate"], lane["measurement_mode"], case_id)
            if not isinstance(case_id, str) or not isinstance(source, str) or key in seen:
                raise ValueError("duplicate or invalid lane/case identity")
            seen.add(key)
            try:
                completed = subprocess.run(
                    [str(program), *lane["argv"]],
                    input=source.encode("utf-8"),
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                    timeout=timeout_seconds,
                    check=False,
                    env=environment,
                )
                stdout, stderr = completed.stdout, completed.stderr
                status = "exited"
                returncode: int | None = completed.returncode
            except subprocess.TimeoutExpired as error:
                stdout = error.stdout or b""
                stderr = error.stderr or b""
                status, returncode = "timed_out", None
            member_root = (
                bundle_root / "raw" / lane["candidate"] / lane["measurement_mode"] / case_id
            )
            stdout_path = member_root / "stdout.bin"
            stderr_path = member_root / "stderr.bin"
            _atomic_write(stdout_path, stdout)
            _atomic_write(stderr_path, stderr)
            members.append(
                {
                    "candidate": lane["candidate"],
                    "measurement_mode": lane["measurement_mode"],
                    "parser_revision": lane["parser_revision"],
                    "adapter_revision": lane["adapter_revision"],
                    "case_id": case_id,
                    "program": lane["program"],
                    "program_sha256": sha256(program_bytes),
                    "argv": lane["argv"],
                    "projector": lane["projector"],
                    "status": status,
                    "returncode": returncode,
                    "stdout": _content_ref(
                        bundle_root, stdout_path, stdout, "application/octet-stream"
                    ),
                    "stderr": _content_ref(
                        bundle_root, stderr_path, stderr, "application/octet-stream"
                    ),
                }
            )
    manifest = {
        "schema_id": "https://w3id.org/abc/schemas/parser-study-diagnostic-capture-v1",
        "schema_version": 1,
        "preregistration_ref": _file_identity(preregistration_path),
        "fixture_ref": _file_identity(fixture_path),
        "lane_policy_ref": _file_identity(lane_policy_path),
        "members": members,
    }
    _atomic_write(manifest_out, canonical_bytes(manifest))
    return manifest


def verify_members(bundle_root: Path, manifest: dict[str, Any]) -> None:
    seen: set[str] = set()
    for member in manifest.get("members", []):
        for stream in ("stdout", "stderr"):
            content = member.get(stream)
            if not isinstance(content, dict):
                raise ValueError(f"missing {stream} content reference")
            locator = content.get("locator")
            if not isinstance(locator, str) or not safe_relative(locator) or locator in seen:
                raise ValueError("unsafe or duplicate raw locator")
            seen.add(locator)
            root = bundle_root.resolve()
            path = (bundle_root / locator).resolve()
            if not path.is_relative_to(root) or not path.is_file():
                raise ValueError("missing or escaping raw member")
            identity = _file_identity(path)
            if identity["sha256"] != content.get("sha256") or identity["bytes"] != content.get(
                "bytes"
            ):
                raise ValueError(f"content mismatch: {locator}")


def verify(
    fixture_path: Path, lane_policy_path: Path, bundle_root: Path, manifest_path: Path
) -> dict[str, Any]:
    fixture = _load_object(fixture_path)
    policy = _load_object(lane_policy_path)
    manifest = _load_object(manifest_path)
    if manifest.get("fixture_ref", {}).get("sha256") != sha256(fixture_path.read_bytes()):
        raise ValueError("fixture content mismatch")
    if manifest.get("lane_policy_ref", {}).get("sha256") != sha256(lane_policy_path.read_bytes()):
        raise ValueError("lane policy content mismatch")
    expected = [
        (lane["candidate"], lane["measurement_mode"], case["id"])
        for lane in policy["lanes"]
        for case in fixture["cases"]
    ]
    observed = [
        (member["candidate"], member["measurement_mode"], member["case_id"])
        for member in manifest.get("members", [])
    ]
    if observed != expected:
        raise ValueError("capture membership or order mismatch")
    verify_members(bundle_root, manifest)
    return manifest


def main() -> None:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command", required=True)
    for command in ("capture", "verify"):
        child = subparsers.add_parser(command)
        child.add_argument("--fixture", type=Path, required=True)
        child.add_argument("--lane-policy", type=Path, required=True)
        child.add_argument("--bundle-root", type=Path, required=True)
        child.add_argument("--manifest", type=Path)
        if command == "capture":
            child.add_argument("--preregistration", type=Path, required=True)
            child.add_argument("--programs-json", type=Path, required=True)
            child.add_argument("--manifest-out", type=Path, required=True)
    args = parser.parse_args()
    if args.command == "capture":
        capture(
            args.preregistration,
            args.fixture,
            args.lane_policy,
            args.programs_json,
            args.bundle_root,
            args.manifest_out,
        )
    else:
        if args.manifest is None:
            parser.error("verify requires --manifest")
        verify(args.fixture, args.lane_policy, args.bundle_root, args.manifest)


if __name__ == "__main__":
    main()
