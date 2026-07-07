#!/usr/bin/env python
from __future__ import annotations

import argparse
import json
import shlex
import subprocess
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any


@dataclass
class Adapter:
    label: str
    mode: str
    command: list[str]


@dataclass
class Row:
    vector: str
    feature: str
    level: str
    adapter: str
    status: str
    failures: list[str]
    warnings: list[str]


def parse_adapter(spec: str) -> Adapter:
    label, sep, rest = spec.partition("=")
    if not sep or not label or not rest:
        raise SystemExit(f"--adapter must be label=mode:command, got {spec!r}")
    mode, mode_sep, command = rest.partition(":")
    if not mode_sep or mode not in {"inspect", "aat"} or not command:
        raise SystemExit(f"--adapter mode must be inspect or aat, got {spec!r}")
    return Adapter(label=label, mode=mode, command=shlex.split(command))


def load_vectors(vectors_dir: Path) -> list[dict[str, Any]]:
    vectors = []
    for path in sorted(vectors_dir.glob("*/vector.json")):
        data = json.loads(path.read_text(encoding="utf-8"))
        if data["name"] != path.parent.name:
            raise SystemExit(f"{path}: name must match directory")
        vectors.append(data)
    if not vectors:
        raise SystemExit(f"no vector.json files found under {vectors_dir}")
    return vectors


def inspect(adapter: Adapter, kind: str, source: str) -> tuple[dict[str, Any] | None, str | None]:
    proc = subprocess.run(
        adapter.command + [kind, "-"],
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        value = json.loads(proc.stdout)
    except json.JSONDecodeError as error:
        return None, f"invalid JSON: {error}"
    if value.get("schemaVersion") != 1 or not isinstance(value.get("data"), list):
        return None, "unsupported inspect envelope"
    return value, None


def compare_projection(
    adapter: Adapter,
    vector: dict[str, Any],
    projection: str,
    failures: list[str],
    warnings: list[str],
) -> None:
    expected = vector["expected"].get(projection)
    if expected is None:
        return
    if projection == "serialize":
        return
    if adapter.mode == "aat":
        warnings.append(
            f"{projection} comparison skipped: "
            f"AAT adapter does not expose aozora inspect {projection}"
        )
        return
    if projection not in {"nodes", "pairs", "diagnostics"}:
        warnings.append(f"{projection} comparison skipped: unsupported projection")
        return
    envelope, error = inspect(adapter, projection, vector["source"])
    if error:
        failures.append(f"{projection}: {error}")
        return
    actual = envelope["data"]
    if actual != expected:
        failures.append(f"{projection}: expected {expected!r}, got {actual!r}")


def evaluate(adapter: Adapter, vector: dict[str, Any]) -> Row:
    failures: list[str] = []
    warnings: list[str] = []
    level = vector["meta"]["level"]
    for projection in ["nodes", "pairs", "diagnostics", "serialize", "html"]:
        compare_projection(adapter, vector, projection, failures, warnings)

    if failures and level == "must":
        status = "fail"
    elif failures:
        warnings.extend(failures)
        failures = []
        status = "warning"
    elif warnings:
        status = "warning"
    else:
        status = "pass"

    return Row(
        vector=vector["name"],
        feature=vector["meta"]["feature"],
        level=level,
        adapter=adapter.label,
        status=status,
        failures=failures,
        warnings=warnings,
    )


def render_markdown(rows: list[Row], vectors_dir: Path) -> str:
    out = [
        "# Aozora Notation-Spec Comparison",
        "",
        f"- vectors_dir: `{vectors_dir}`",
        f"- rows: {len(rows)}",
        "",
        "| vector | feature | level | adapter | status | failures | warnings |",
        "| --- | --- | --- | --- | --- | --- | --- |",
    ]
    for row in rows:
        out.append(
            "| {} | {} | {} | {} | {} | {} | {} |".format(
                row.vector,
                row.feature,
                row.level,
                row.adapter,
                row.status,
                "<br>".join(row.failures),
                "<br>".join(row.warnings),
            )
        )
    out.append("")
    return "\n".join(out)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--vectors-dir", type=Path, required=True)
    parser.add_argument("--adapter", action="append", default=[])
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path, required=True)
    args = parser.parse_args()

    adapters = [parse_adapter(spec) for spec in args.adapter]
    if not adapters:
        raise SystemExit("at least one --adapter is required")

    vectors = load_vectors(args.vectors_dir)
    rows = [evaluate(adapter, vector) for vector in vectors for adapter in adapters]
    summary = {
        "schema_version": 1,
        "vectors_dir": str(args.vectors_dir),
        "totals": {
            "vectors": len(vectors),
            "adapters": len(adapters),
            "rows": len(rows),
            "pass": sum(1 for row in rows if row.status == "pass"),
            "warning": sum(1 for row in rows if row.status == "warning"),
            "fail": sum(1 for row in rows if row.status == "fail"),
        },
        "rows": [asdict(row) for row in rows],
    }
    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(
        json.dumps(summary, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    args.report_md.write_text(render_markdown(rows, args.vectors_dir), encoding="utf-8")


if __name__ == "__main__":
    main()
