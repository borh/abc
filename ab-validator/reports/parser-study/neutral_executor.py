#!/usr/bin/env python3
"""Resumable per-item executor for frozen parser-study inventories."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import time
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


def run_item(
    item: dict[str, str], source_root: Path, command: list[str], output: Path, timeout: int
) -> dict[str, str]:
    source = (source_root / item["path"]).read_bytes()
    if sha256(source) != item["sha256"]:
        raise ValueError(f"source hash mismatch: {item['id']}")
    started = time.monotonic_ns()
    try:
        process = subprocess.run(
            command, input=source, capture_output=True, timeout=timeout, check=False
        )
        status = "success" if process.returncode == 0 else "failure"
        exit_code: int | None = process.returncode
        stdout, stderr = process.stdout, process.stderr
    except subprocess.TimeoutExpired as error:
        status, exit_code = "timeout", None
        stdout = error.stdout or b""
        stderr = error.stderr or b""
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
) -> None:
    items = load_inventory(inventory_path)
    output.mkdir(parents=True, exist_ok=True)
    existing: dict[str, dict[str, str]] = {}
    manifest_path = output / "manifest.json"
    if manifest_path.exists():
        for entry in json.loads(manifest_path.read_bytes()).get("outcomes", []):
            existing[Path(entry["path"]).name] = entry

    def one(item: dict[str, str]) -> dict[str, str]:
        name = outcome_name(item["id"])
        entry = existing.get(name)
        if entry:
            path = output / entry["path"]
            if path.is_file() and sha256(path.read_bytes()) == entry["sha256"]:
                return entry
        return run_item(item, source_root, command, output, timeout)

    with ThreadPoolExecutor(max_workers=jobs) as pool:
        outcomes = list(pool.map(one, items))
    write_json(
        manifest_path,
        {"inventory_sha256": sha256(inventory_path.read_bytes()), "outcomes": outcomes},
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
        path = output / entry["path"]
        data = path.read_bytes()
        if sha256(data) != entry.get("sha256"):
            raise ValueError("outcome hash mismatch")
        row = json.loads(data)
        if row.get("item_id") != item["id"] or row.get("source_sha256") != item["sha256"]:
            raise ValueError("outcome identity mismatch")
        for stream in ("stdout", "stderr"):
            artifact = row[stream]
            if not safe_relative(artifact["path"]):
                raise ValueError("unsafe artifact path")
            if sha256((output / artifact["path"]).read_bytes()) != artifact["sha256"]:
                raise ValueError("artifact hash mismatch")
        rows.append(row)
    return rows


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--inventory", type=Path, required=True)
    parser.add_argument("--source-root", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timeout", type=int, default=300)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument("--verify", action="store_true")
    parser.add_argument("--materialize-vectors", type=Path)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if args.materialize_vectors:
        materialize_vectors(args.materialize_vectors, args.output)
    elif args.verify:
        verify(args.inventory, args.output)
    else:
        if args.source_root is None or not args.command:
            parser.error("execution requires --source-root and command")
        execute(
            args.inventory, args.source_root, args.command, args.output, args.timeout, args.jobs
        )


if __name__ == "__main__":
    main()
