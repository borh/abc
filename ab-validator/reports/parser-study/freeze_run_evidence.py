#!/usr/bin/env python3
"""Regenerate the checked parser-run summary from external raw evidence."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from collections import Counter
from pathlib import Path
from typing import Any


CANDIDATES = ("aozora", "aozora2", "aozora-rs", "aozora2html", "aozora-epub3")
MODES = ("native", "adapter_normalized")
INVENTORIES = ("official-notation-vectors", "aozorabunko-source-snapshot")
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
        manifest_path = output / "manifest.json"
        if sha256(manifest_path.read_bytes()) != run["manifest_sha256"]:
            raise ValueError("external manifest hash mismatch")
        rows = neutral.verify(inventory_path, output)
        counts = dict(sorted(Counter(row["status"] for row in rows).items()))
        if counts != run["outcomes"]:
            raise ValueError("external outcome counts mismatch")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--summary", type=Path, required=True)
    parser.add_argument("--resolver", type=Path, required=True)
    parser.add_argument("--executor", type=Path, required=True)
    parser.add_argument("--verify-checked-bytes", action="store_true")
    args = parser.parse_args()
    raw = args.summary.read_bytes()
    summary = json.loads(raw)
    verify_external(summary, json.loads(args.resolver.read_bytes()), args.executor)
    generated = canonical_bytes(summary)
    if args.verify_checked_bytes and generated != raw:
        raise ValueError("checked summary is not byte-identical canonical regeneration")


if __name__ == "__main__":
    main()
