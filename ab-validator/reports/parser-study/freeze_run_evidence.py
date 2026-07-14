#!/usr/bin/env python3
"""Regenerate the checked parser-run summary from external raw evidence."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
import copy
from collections import Counter
from pathlib import Path
from typing import Any


CANDIDATES = ("aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3")
MODES = ("native", "adapter_normalized")
INVENTORIES = ("official-notation-vectors", "aozorabunko-source-snapshot")
COMMAND_BINDINGS = {
    ("aozora", "native"): ("aozora", ["inspect", "nodes", "-"]),
    ("aozora", "adapter_normalized"): ("aozora-adapter", ["--mode", "aat"]),
    ("aozora2", "native"): ("aozora2", ["html", "--encoding", "utf-8"]),
    ("aozora2", "adapter_normalized"): ("aozora2-adapter", ["--mode", "aat"]),
    ("aozora-rs", "native"): ("aozora-rs-native", []),
    ("aozora-rs", "adapter_normalized"): ("aozora-rs-adapter", ["--mode", "aat"]),
    ("aozora2html", "native"): ("aozora2html-adapter", ["--mode", "html"]),
    ("aozora2html", "adapter_normalized"): (
        "aozora2html-adapter",
        ["--mode", "aat"],
    ),
    ("aozora-epub3", "native"): ("aozora-epub3-adapter", ["--mode", "html"]),
    ("aozora-epub3", "adapter_normalized"): (
        "aozora-epub3-adapter",
        ["--mode", "aat"],
    ),
}
HOST_UNAVAILABLE = {
    "status": "unavailable",
    "reason": "not_captured_immediately_before_run",
    "performance_host_comparability": "unavailable",
    "applies_to": "all_other_runs",
}


def sha256(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def canonical_bytes(value: object) -> bytes:
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()


def validate_summary(summary: dict[str, Any]) -> None:
    runs = summary.get("runs")
    if not isinstance(runs, list):
        raise ValueError("runs must be an array")
    expected = {
        (candidate, mode, inventory)
        for inventory in INVENTORIES
        for candidate in CANDIDATES
        for mode in MODES
    }
    observed = {
        (run.get("candidate"), run.get("mode"), run.get("inventory"))
        for run in runs
        if isinstance(run, dict)
    }
    if len(runs) != 20 or observed != expected:
        raise ValueError("summary must contain the exact 20-lane matrix")
    host_capture = summary.get("host_capture")
    if not isinstance(host_capture, dict) or host_capture.get("default") != HOST_UNAVAILABLE:
        raise ValueError("invalid closed host capture classification")
    contracts = summary.get("execution_contracts")
    if not isinstance(contracts, dict):
        raise ValueError("execution contracts are required")
    for run in runs:
        if not isinstance(run, dict):
            raise ValueError("run must be an object")
        contract = contracts.get(run.get("execution_sha256"))
        if not isinstance(contract, dict):
            raise ValueError("missing execution contract")
        if contract.get("candidate") != run["candidate"] or contract.get("mode") != run["mode"]:
            raise ValueError("command binding does not correspond to candidate and mode")
        if "host_capture" in run:
            raise ValueError("invalid per-run host capture")


def validate_frozen_bindings(
    preregistration_bytes: bytes,
    summary: dict[str, Any],
    inventory_paths: dict[str, Path],
) -> None:
    preregistration = json.loads(preregistration_bytes)
    if summary.get("protocol_sha256") != sha256(preregistration_bytes):
        raise ValueError("protocol hash mismatch")
    if summary.get("study_id") != preregistration.get("study_id"):
        raise ValueError("study id mismatch")
    timeout = preregistration.get("performance_protocol", {}).get("timeout_seconds")
    if summary.get("timeout_seconds") != timeout:
        raise ValueError("timeout mismatch")
    expected_candidates = {
        row["id"]: {
            "parser_revision": row["revision"],
            "adapter_revision": row["adapter_revision"],
        }
        for row in preregistration.get("candidates", [])
        if row.get("disposition") == "included"
    }
    if summary.get("candidates") != expected_candidates:
        raise ValueError("candidate revisions mismatch")
    corpora = {row["id"]: row for row in preregistration.get("corpora", [])}
    summary_inventories = summary.get("inventories", {})
    for inventory_id, path in inventory_paths.items():
        document = json.loads(path.read_bytes())
        items = document.get("items")
        checked = summary_inventories.get(inventory_id, {})
        if (
            not isinstance(items, list)
            or checked.get("inventory_sha256") != sha256(path.read_bytes())
            or checked.get("items") != len(items)
        ):
            raise ValueError("inventory hash or count mismatch")
        if checked.get("revision") != corpora[inventory_id]["revision"]:
            raise ValueError("inventory revision mismatch")


def load_neutral_executor(path: Path) -> Any:
    spec = importlib.util.spec_from_file_location("neutral_executor", path)
    if spec is None or spec.loader is None:
        raise ValueError("cannot load neutral executor")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def verify_external(summary: dict[str, Any], resolver: dict[str, Any], executor_path: Path) -> None:
    validate_summary(summary)
    neutral = load_neutral_executor(executor_path)
    inventories = resolver.get("inventories", {})
    artifacts = resolver.get("artifact_roots", {})
    executions = resolver.get("executions", {})
    for run in summary["runs"]:
        inventory_path = Path(inventories[run["inventory"]])
        output = Path(artifacts[run["artifact_root"]])
        execution = executions[run["execution_sha256"]]
        calculated_execution = sha256(
            json.dumps(execution, sort_keys=True, separators=(",", ":")).encode()
        )
        if calculated_execution != run["execution_sha256"]:
            raise ValueError("resolved command/environment execution hash mismatch")
        contract = summary["execution_contracts"][run["execution_sha256"]]
        command = execution.get("command")
        required_argv = contract["required_argv"]
        if not isinstance(command, list) or not command:
            raise ValueError("resolved command violates command binding")
        if required_argv and required_argv != command[-len(required_argv) :]:
            raise ValueError("resolved command violates command binding")
        program_index = len(command) - len(required_argv) - 1
        if os.path.basename(command[program_index]) != contract["required_program_basename"]:
            raise ValueError("resolved command violates command binding")
        materialization = resolver["materializations"][run["execution_sha256"]]
        program_path = Path(command[program_index])
        materialization_projection = {
            key: materialization[key]
            for key in (
                "candidate",
                "mode",
                "parser_revision",
                "adapter_revision",
                "program_sha256",
                "derivation_identity",
            )
        }
        candidate = summary["candidates"][run["candidate"]]
        if (
            materialization_projection["candidate"] != run["candidate"]
            or materialization_projection["mode"] != run["mode"]
            or materialization_projection["parser_revision"] != candidate["parser_revision"]
            or materialization_projection["adapter_revision"] != candidate["adapter_revision"]
            or materialization_projection["program_sha256"] != sha256(program_path.read_bytes())
            or contract["materialization_binding_sha256"]
            != sha256(canonical_bytes(materialization_projection))
        ):
            raise ValueError("materialization binding mismatch")
        manifest_path = output / "manifest.json"
        if sha256(manifest_path.read_bytes()) != run["manifest_sha256"]:
            raise ValueError("external manifest hash mismatch")
        rows = neutral.verify(inventory_path, output)
        counts = dict(sorted(Counter(row["status"] for row in rows).items()))
        if counts != run["outcomes"]:
            raise ValueError("external outcome counts mismatch")


def project_summary(
    checked: dict[str, Any],
    preregistration_bytes: bytes,
    resolver: dict[str, Any],
    executor_path: Path,
) -> dict[str, Any]:
    preregistration = json.loads(preregistration_bytes)
    projected = copy.deepcopy(checked)
    projected["study_id"] = preregistration["study_id"]
    projected["protocol_sha256"] = sha256(preregistration_bytes)
    projected["timeout_seconds"] = preregistration["performance_protocol"]["timeout_seconds"]
    projected["candidates"] = {
        row["id"]: {
            "parser_revision": row["revision"],
            "adapter_revision": row["adapter_revision"],
        }
        for row in preregistration["candidates"]
        if row["disposition"] == "included"
    }
    projected["schema_version"] = 1
    projected["host_capture"] = copy.deepcopy(resolver["host_capture"])
    projected["execution_contracts"] = {}
    corpora = {row["id"]: row for row in preregistration["corpora"]}
    inventories: dict[str, Any] = {}
    for inventory_id, path_value in resolver["inventories"].items():
        path = Path(path_value)
        items = json.loads(path.read_bytes())["items"]
        identity = resolver["inventory_identities"][inventory_id]
        row: dict[str, Any] = {
            "revision": corpora[inventory_id]["revision"],
            "inventory_sha256": sha256(path.read_bytes()),
            "items": len(items),
        }
        if "corpus_sha256" in identity:
            row["corpus_sha256"] = identity["corpus_sha256"]
        inventories[inventory_id] = row
    projected["inventories"] = inventories
    neutral = load_neutral_executor(executor_path)
    for run in projected["runs"]:
        inventory_path = Path(resolver["inventories"][run["inventory"]])
        output = Path(resolver["artifact_roots"][run["artifact_root"]])
        manifest_bytes = (output / "manifest.json").read_bytes()
        manifest = json.loads(manifest_bytes)
        run["manifest_sha256"] = sha256(manifest_bytes)
        run["execution_sha256"] = manifest["execution_sha256"]
        rows = neutral.verify(inventory_path, output)
        run["outcomes"] = dict(sorted(Counter(row["status"] for row in rows).items()))
        materialization = resolver["materializations"][run["execution_sha256"]]
        program_basename, required_argv = COMMAND_BINDINGS[
            (run["candidate"], run["mode"])
        ]
        materialization_projection = {
            key: materialization[key]
            for key in (
                "candidate",
                "mode",
                "parser_revision",
                "adapter_revision",
                "program_sha256",
                "derivation_identity",
            )
        }
        projected["execution_contracts"][run["execution_sha256"]] = {
            "candidate": run["candidate"],
            "mode": run["mode"],
            "required_program_basename": program_basename,
            "required_argv": required_argv,
            "materialization_binding_sha256": sha256(
                canonical_bytes(materialization_projection)
            ),
        }
    validate_summary(projected)
    validate_frozen_bindings(
        preregistration_bytes,
        projected,
        {key: Path(value) for key, value in resolver["inventories"].items()},
    )
    verify_external(projected, resolver, executor_path)
    return projected


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--summary", type=Path, required=True)
    parser.add_argument("--resolver", type=Path, required=True)
    parser.add_argument("--executor", type=Path, required=True)
    parser.add_argument("--preregistration", type=Path, required=True)
    parser.add_argument("--verify-checked-bytes", action="store_true")
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    raw = args.summary.read_bytes()
    checked = json.loads(raw)
    generated = canonical_bytes(
        project_summary(
            checked,
            args.preregistration.read_bytes(),
            json.loads(args.resolver.read_bytes()),
            args.executor,
        )
    )
    if args.verify_checked_bytes and generated != raw:
        raise ValueError("checked summary is not byte-identical canonical regeneration")
    if args.write:
        args.summary.write_bytes(generated)


if __name__ == "__main__":
    main()
