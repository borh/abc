#!/usr/bin/env python3
"""Capture shared raw diagnostics and Parser-IR qualification records.

Membership is supplied by an explicit corpus index. The producer invokes each
candidate executable once per member, publishes raw values by logical identity,
and emits closed indexes; it does not derive predicate verdicts.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import shutil
from pathlib import Path
from typing import Any


def canonical(value: object) -> bytes:
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()


def publish(store: Path, payload: bytes, media_type: str = "application/json") -> dict[str, object]:
    digest = hashlib.sha256(payload).hexdigest()
    locator = f"sha256/{digest[:2]}/{digest}.json"
    target = store / locator
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.exists() and target.read_bytes() != payload:
        raise ValueError("content-address collision")
    target.write_bytes(payload)
    return {
        "sha256": f"sha256:{digest}",
        "bytes": len(payload),
        "media_type": media_type,
        "locator": locator,
    }


def _run(argv: list[str], *, stdin: bytes | None = None) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(argv, input=stdin, capture_output=True, check=False)


def _source_below(root: Path, relative: object) -> Path:
    if not isinstance(relative, str):
        raise ValueError("corpus source path must be a string")
    candidate = Path(relative)
    if candidate.is_absolute() or ".." in candidate.parts:
        raise ValueError("corpus source path escapes its root")
    resolved_root = root.resolve(strict=True)
    resolved = (resolved_root / candidate).resolve(strict=True)
    if not resolved.is_relative_to(resolved_root):
        raise ValueError("corpus source path escapes its root")
    return resolved


def capture(
    corpus: dict[str, Any],
    *,
    source_root: Path,
    aozora: Path,
    converter: Path,
    mapping: Path,
    research_root: Path,
    identity_ref: str,
    parser_policy: Path,
    store: Path,
    output: Path,
) -> tuple[dict[str, object], dict[str, object]]:
    rows = corpus.get("entries")
    if not isinstance(rows, list) or not rows:
        raise ValueError("corpus entries must be a nonempty explicit list")
    work_ids = [row.get("work_id") for row in rows]
    if any(not isinstance(work_id, str) or not work_id for work_id in work_ids):
        raise ValueError("work IDs must be nonblank strings")
    if len(set(work_ids)) != len(work_ids):
        raise ValueError("duplicate corpus work ID")
    store.mkdir(parents=True, exist_ok=True)
    output.mkdir(parents=True, exist_ok=True)
    parser_ir_root = output / "parser-ir"
    parser_ir_root.mkdir()
    diagnostics: list[dict[str, object]] = []
    parser_records: list[dict[str, object]] = []
    for row in rows:
        work_id = row["work_id"]
        source = _source_below(source_root, row.get("source_path")).read_bytes()
        diagnostic_run = _run([str(aozora), "--mode", "diagnostics"], stdin=source)
        diagnostic_ref = publish(store, diagnostic_run.stdout)
        diagnostics.append(
            {
                "work_id": work_id,
                "exit_code": diagnostic_run.returncode,
                "raw_diagnostics": diagnostic_ref,
            }
        )
        aat_run = _run([str(aozora), "--mode", "aat"], stdin=source)
        aat_path = output / f"{work_id}.aat.json"
        aat_path.write_bytes(aat_run.stdout)
        parser_out = output / f"{work_id}.parser-ir.json"
        ledger_out = output / f"{work_id}.ledger.json"
        record_out = output / f"{work_id}.record.json"
        converter_run = _run(
            [
                str(converter),
                "qualify",
                "--aat",
                str(aat_path),
                "--mapping",
                str(mapping),
                "--research-root",
                str(research_root),
                "--work-id",
                work_id,
                "--qualification-identity-ref",
                identity_ref,
                "--policy",
                str(parser_policy),
                "--parser-ir-out",
                str(parser_out),
                "--ledger-out",
                str(ledger_out),
                "--record-out",
                str(record_out),
            ]
        )
        record_bytes = (
            record_out.read_bytes()
            if record_out.is_file()
            else canonical({"status": "protocol_error", "exit_code": converter_run.returncode})
        )
        parser_entry: dict[str, object] = {
            "work_id": work_id,
            "record": publish(store, record_bytes),
        }
        if parser_out.is_file():
            parser_entry["parser_ir"] = publish(store, parser_out.read_bytes())
        if ledger_out.is_file():
            parser_entry["validation_ledger"] = publish(store, ledger_out.read_bytes())
        parser_records.append(parser_entry)
        if parser_out.is_file():
            shutil.copyfile(parser_out, parser_ir_root / f"{work_id}.json")
    diagnostic_index = {
        "schema_version": "abc/parser-rq-predicate-hardening-raw-diagnostics/v1",
        "qualification_identity_ref": identity_ref,
        "expected_work_ids": work_ids,
        "records": diagnostics,
    }
    parser_index = {
        "schema_version": "abc/parser-rq-predicate-hardening-parser-ir/v1",
        "qualification_identity_ref": identity_ref,
        "expected_work_ids": work_ids,
        "records": parser_records,
    }
    (output / "raw-diagnostics-index.json").write_bytes(canonical(diagnostic_index))
    (output / "parser-ir-index.json").write_bytes(canonical(parser_index))
    return diagnostic_index, parser_index


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, required=True)
    parser.add_argument("--ab-aozora", type=Path, required=True)
    parser.add_argument("--converter", type=Path, required=True)
    parser.add_argument("--mapping", type=Path, required=True)
    parser.add_argument("--research-root", type=Path, required=True)
    parser.add_argument("--identity-ref", required=True)
    parser.add_argument("--parser-policy", type=Path, required=True)
    parser.add_argument("--store", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    capture(
        json.loads(args.corpus.read_text()),
        source_root=args.source_root,
        aozora=args.ab_aozora,
        converter=args.converter,
        mapping=args.mapping,
        research_root=args.research_root,
        identity_ref=args.identity_ref,
        parser_policy=args.parser_policy,
        store=args.store,
        output=args.output,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
