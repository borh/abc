#!/usr/bin/env python3
"""Resumable per-item executor for frozen parser-study inventories."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import signal
import subprocess
import time
import zipfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path, PurePosixPath
from typing import Any


def sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def safe_relative(value: str) -> bool:
    normalized = value.replace("\\", "/")
    path = PurePosixPath(normalized)
    return (
        bool(value)
        and not value.startswith(("/", "\\"))
        and not (len(value) >= 2 and value[0].isalpha() and value[1] == ":")
        and all(part not in ("", ".", "..") for part in path.parts)
    )


def contained_file(root: Path, value: str) -> Path:
    if not safe_relative(value):
        raise ValueError("unsafe relative path")
    root_resolved = root.resolve()
    path = (root / value).resolve()
    if not path.is_relative_to(root_resolved) or not path.is_file():
        raise ValueError("path escapes output root or is not a file")
    return path


def load_inventory(path: Path) -> list[dict[str, str]]:
    document = json.loads(path.read_bytes())
    items = document.get("items")
    if not isinstance(items, list):
        raise ValueError("inventory items must be an array")
    seen: set[str] = set()
    result: list[dict[str, str]] = []
    for item in items:
        if not isinstance(item, dict) or set(item) != {"id", "path", "sha256"}:
            raise ValueError("inventory item has invalid fields")
        if not all(isinstance(item[key], str) for key in item):
            raise ValueError("inventory item fields must be strings")
        if item["id"] in seen or not safe_relative(item["path"]):
            raise ValueError("duplicate id or unsafe inventory path")
        seen.add(item["id"])
        result.append(item)
    return result


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False, sort_keys=True) + "\n")
    temporary.replace(path)


def outcome_name(item_id: str) -> str:
    return hashlib.sha256(item_id.encode()).hexdigest() + ".json"


def materialize_vectors(vector_root: Path, output: Path) -> None:
    items: list[dict[str, str]] = []
    sources = output / "sources"
    for vector_path in sorted(vector_root.glob("*/vector.json")):
        vector = json.loads(vector_path.read_bytes())
        name, source = vector.get("name"), vector.get("source")
        if not isinstance(name, str) or not isinstance(source, str) or not safe_relative(name):
            raise ValueError(f"invalid frozen vector: {vector_path}")
        data = source.encode()
        relative = name + ".txt"
        destination = sources / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
        items.append({"id": name, "path": relative, "sha256": sha256(data)})
    write_json(output / "inventory.json", {"items": items})


#: Marker stamped into the smoke inventory so the artifact self-identifies as
#: non-authoritative and cannot be mistaken for the study inventory.
SMOKE_MATERIALIZER = "neutral_executor.smoke"


def materialize_index(index_path: Path, corpus: Path, output: Path, limit: int) -> None:
    """Smoke-only inventory materializer for local `--limit` runs.

    This is NOT the authoritative study-inventory materializer. It uses a
    different identity scheme than the Rust `ab-materialize-study-inventory`
    (`ab-check` bin): here each item's ``id`` is the raw work id and its ``path``
    is ``sha256(work_id)``, whereas the authoritative materializer keys items as
    ``{work_id}-{sha12}`` and produced the committed ``aozorabunko-source-snapshot``
    corpus hash. The two therefore yield different ``inventory.json`` bytes, and
    this one MUST NOT be used to reproduce or stand in for the study inventory.
    The output is stamped with ``"materializer": SMOKE_MATERIALIZER`` to make that
    non-authority explicit and checkable.
    """
    works = json.loads(index_path.read_bytes()).get("works")
    if not isinstance(works, list):
        raise ValueError("index works must be an array")
    selected = works if limit == 0 else works[:limit]
    items: list[dict[str, str]] = []
    sources = output / "sources"
    for work in selected:
        work_id, indexed_path = work.get("id"), work.get("txt_path")
        if not isinstance(work_id, str) or not isinstance(indexed_path, str):
            raise ValueError("invalid index work")
        archive_path, separator, member = indexed_path.partition("::")
        if not safe_relative(archive_path) or (separator and not safe_relative(member)):
            raise ValueError("unsafe indexed source path")
        if separator:
            with zipfile.ZipFile(corpus / archive_path) as archive:
                data = archive.read(member)
        else:
            data = (corpus / archive_path).read_bytes()
        relative = hashlib.sha256(work_id.encode()).hexdigest() + ".txt"
        destination = sources / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
        items.append({"id": work_id, "path": relative, "sha256": sha256(data)})
    write_json(output / "inventory.json", {"materializer": SMOKE_MATERIALIZER, "items": items})


def run_item(
    item: dict[str, str],
    source_root: Path,
    command: list[str],
    environment: dict[str, str],
    execution_sha256: str,
    output: Path,
    timeout: int,
) -> dict[str, str]:
    source = (source_root / item["path"]).read_bytes()
    if sha256(source) != item["sha256"]:
        raise ValueError(f"source hash mismatch: {item['id']}")
    started = time.monotonic_ns()
    process = subprocess.Popen(
        command,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=os.environ | environment,
        start_new_session=True,
    )
    try:
        stdout, stderr = process.communicate(source, timeout=timeout)
        status = "success" if process.returncode == 0 else "failure"
        exit_code: int | None = process.returncode
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
        status, exit_code = "timeout", None
    elapsed = time.monotonic_ns() - started
    raw = output / "raw"
    stdout_path = raw / (outcome_name(item["id"]) + ".stdout")
    stderr_path = raw / (outcome_name(item["id"]) + ".stderr")
    stdout_path.parent.mkdir(parents=True, exist_ok=True)
    stdout_path.write_bytes(stdout)
    stderr_path.write_bytes(stderr)
    outcome: dict[str, Any] = {
        "item_id": item["id"],
        "source_sha256": item["sha256"],
        "execution_sha256": execution_sha256,
        "status": status,
        "exit_code": exit_code,
        "elapsed_ns": elapsed,
        "stdout": {"path": stdout_path.relative_to(output).as_posix(), "sha256": sha256(stdout)},
        "stderr": {"path": stderr_path.relative_to(output).as_posix(), "sha256": sha256(stderr)},
    }
    destination = output / "outcomes" / outcome_name(item["id"])
    write_json(destination, outcome)
    return {
        "path": destination.relative_to(output).as_posix(),
        "sha256": sha256(destination.read_bytes()),
    }


def execute(
    inventory_path: Path,
    source_root: Path,
    command: list[str],
    output: Path,
    timeout: int,
    jobs: int,
    environment: dict[str, str] | None = None,
) -> None:
    items = load_inventory(inventory_path)
    environment = environment or {}
    execution_sha256 = sha256(
        json.dumps(
            {"command": command, "environment": environment},
            sort_keys=True,
            separators=(",", ":"),
        ).encode()
    )
    output.mkdir(parents=True, exist_ok=True)
    existing: dict[str, dict[str, str]] = {}
    manifest_path = output / "manifest.json"
    if manifest_path.exists():
        previous = json.loads(manifest_path.read_bytes())
        if previous.get("execution_sha256") == execution_sha256:
            for entry in previous.get("outcomes", []):
                existing[Path(entry["path"]).name] = entry
    else:
        for path in sorted((output / "outcomes").glob("*.json")):
            data = path.read_bytes()
            row = json.loads(data)
            if row.get("execution_sha256") == execution_sha256:
                existing[path.name] = {
                    "path": path.relative_to(output).as_posix(),
                    "sha256": sha256(data),
                }

    def one(item: dict[str, str]) -> dict[str, str]:
        name = outcome_name(item["id"])
        entry = existing.get(name)
        if entry:
            try:
                path = contained_file(output, entry["path"])
            except ValueError:
                path = None
            if path is not None and sha256(path.read_bytes()) == entry["sha256"]:
                return entry
        return run_item(item, source_root, command, environment, execution_sha256, output, timeout)

    with ThreadPoolExecutor(max_workers=jobs) as pool:
        outcomes = list(pool.map(one, items))
    write_json(
        manifest_path,
        {
            "inventory_sha256": sha256(inventory_path.read_bytes()),
            "execution_sha256": execution_sha256,
            "outcomes": outcomes,
        },
    )
    verify(inventory_path, output)


def verify(inventory_path: Path, output: Path) -> list[dict[str, Any]]:
    items = load_inventory(inventory_path)
    manifest = json.loads((output / "manifest.json").read_bytes())
    entries = manifest.get("outcomes")
    if not isinstance(entries, list) or len(entries) != len(items):
        raise ValueError("manifest outcomes are incomplete")
    rows: list[dict[str, Any]] = []
    for item, entry in zip(items, entries, strict=True):
        if not isinstance(entry, dict) or not safe_relative(entry.get("path", "")):
            raise ValueError("manifest outcomes contain unsafe paths")
        path = contained_file(output, entry["path"])
        data = path.read_bytes()
        if sha256(data) != entry.get("sha256"):
            raise ValueError("outcome hash mismatch")
        row = json.loads(data)
        if row.get("item_id") != item["id"] or row.get("source_sha256") != item["sha256"]:
            raise ValueError("outcome identity mismatch")
        if row.get("execution_sha256") != manifest.get("execution_sha256"):
            raise ValueError("outcome execution identity mismatch")
        for stream in ("stdout", "stderr"):
            artifact = row[stream]
            if not safe_relative(artifact["path"]):
                raise ValueError("unsafe artifact path")
            if sha256(contained_file(output, artifact["path"]).read_bytes()) != artifact["sha256"]:
                raise ValueError("artifact hash mismatch")
        rows.append(row)
    return rows


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--inventory", type=Path)
    parser.add_argument("--source-root", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timeout", type=int, default=300)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument("--verify", action="store_true")
    parser.add_argument("--materialize-vectors", type=Path)
    parser.add_argument("--materialize-index", type=Path)
    parser.add_argument("--limit", type=int, default=0)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.materialize_vectors:
        materialize_vectors(args.materialize_vectors, args.output)
    elif args.materialize_index:
        if args.source_root is None:
            parser.error("index materialization requires --source-root corpus")
        materialize_index(args.materialize_index, args.source_root, args.output, args.limit)
    elif args.verify:
        if args.inventory is None:
            parser.error("verification requires --inventory")
        verify(args.inventory, args.output)
    else:
        if args.inventory is None or args.source_root is None or not args.command:
            parser.error("execution requires --inventory, --source-root, and command")
        execute(
            args.inventory, args.source_root, args.command, args.output, args.timeout, args.jobs
        )


if __name__ == "__main__":
    main()
