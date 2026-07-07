#!/usr/bin/env python
"""Measure parser adapter performance on a shared Aozora workset.

The primary measurement is intentionally adapter-agnostic: every parser is
invoked as a stdin -> AAT process and wrapped with GNU time. aozora2html can
also receive a supplemental Ruby-parser/Rust-mapper stage split because its
adapter path is a shell pipeline rather than a single parser binary.
"""

from __future__ import annotations

import argparse
import json
import os
import pathlib
import re
import shlex
import statistics
import subprocess
import tempfile
import zipfile
from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class Work:
    work_id: str
    archive: pathlib.Path
    entry: str
    size: int


@dataclass(frozen=True)
class Adapter:
    label: str
    command: list[str]


def main() -> int:
    args = parse_args()
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    adapters = [parse_adapter(value) for value in args.adapter]
    if not adapters:
        raise SystemExit("at least one --adapter LABEL=COMMAND is required")

    works = select_works(
        index_path=args.index,
        corpus=args.corpus,
        work_ids_path=args.work_ids,
        sample=args.sample,
    )
    if not works:
        raise SystemExit("selected workset is empty")

    measurements: list[dict[str, Any]] = []
    for work in works:
        source = read_work_bytes(work)
        for adapter in adapters:
            if adapter.label == args.aozora2html_label:
                measurements.append(
                    measure_aozora2html_full(
                        args=args,
                        source=source,
                        work=work,
                        out_dir=out_dir,
                    )
                )
            else:
                measurements.append(
                    measure_command(
                        time_bin=args.time_bin,
                        label=adapter.label,
                        stage="full_adapter",
                        command=adapter.command,
                        stdin_bytes=source,
                        timeout_s=args.limit_s,
                        work=work,
                        out_dir=out_dir,
                    )
                )

    if args.aozora2html_label:
        selected = works[: args.stage_split_sample]
        for work in selected:
            source = read_work_bytes(work)
            measurements.extend(
                measure_aozora2html_stages(
                    args=args,
                    source=source,
                    work=work,
                    out_dir=out_dir,
                )
            )

    payload = {
        "generated_by": "reports/aat-fidelity/measure-parser-performance.py",
        "index": str(args.index),
        "corpus": str(args.corpus),
        "limit_s": args.limit_s,
        "time_bin": str(args.time_bin),
        "adapters": [{"label": adapter.label, "command": adapter.command} for adapter in adapters],
        "selected_works": [
            {
                "work_id": work.work_id,
                "archive": str(work.archive),
                "entry": work.entry,
                "size": work.size,
            }
            for work in works
        ],
        "measurements": measurements,
        "summary": summarize(measurements),
    }
    write_json(out_dir / "results.json", payload)
    write_summary_md(out_dir / "summary.md", payload)
    print(f"wrote {out_dir / 'results.json'}")
    print(f"wrote {out_dir / 'summary.md'}")
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--index", required=True, type=pathlib.Path)
    parser.add_argument("--corpus", required=True, type=pathlib.Path)
    parser.add_argument("--out-dir", required=True, type=pathlib.Path)
    parser.add_argument("--work-ids", type=pathlib.Path)
    parser.add_argument("--sample", type=int, default=20)
    parser.add_argument("--limit-s", type=int, default=900)
    parser.add_argument(
        "--time-bin",
        type=pathlib.Path,
        default=pathlib.Path("/run/current-system/sw/bin/time"),
    )
    parser.add_argument(
        "--adapter",
        action="append",
        default=[],
        help="adapter command in LABEL=COMMAND form; COMMAND is parsed with shlex",
    )
    parser.add_argument(
        "--aozora2html-label",
        help="adapter label that should receive supplemental Ruby/Rust stage split",
    )
    parser.add_argument("--aozora2html-bin", type=pathlib.Path)
    parser.add_argument(
        "--aozora2html-gem-home",
        type=pathlib.Path,
        default=pathlib.Path(
            os.environ.get(
                "AB_AOZORA2HTML_GEM_HOME",
                str(
                    pathlib.Path(os.environ.get("AB_DB_ROOT", "scratch/state"))
                    / "gems"
                    / "aozora2html-3.0.1"
                ),
            )
        ),
    )
    parser.add_argument("--mapper-bin", type=pathlib.Path)
    parser.add_argument("--stage-split-sample", type=int, default=5)
    return parser.parse_args()


def parse_adapter(value: str) -> Adapter:
    label, sep, command = value.partition("=")
    if not sep or not label.strip() or not command.strip():
        raise SystemExit(f"invalid --adapter value, expected LABEL=COMMAND: {value}")
    return Adapter(label=label.strip(), command=shlex.split(command))


def select_works(
    index_path: pathlib.Path,
    corpus: pathlib.Path,
    work_ids_path: pathlib.Path | None,
    sample: int,
) -> list[Work]:
    index = json.loads(index_path.read_text())
    selected_ids: set[str] | None = None
    if work_ids_path is not None:
        values = json.loads(work_ids_path.read_text())
        if not isinstance(values, list) or not all(isinstance(it, str) for it in values):
            raise SystemExit("--work-ids must be a JSON array of work-id strings")
        selected_ids = set(values)

    works: list[Work] = []
    for item in index.get("works", []):
        work_id = item.get("id")
        if not isinstance(work_id, str):
            continue
        if selected_ids is not None and work_id not in selected_ids:
            continue
        txt_path = item.get("txt_path", "")
        if not isinstance(txt_path, str) or not txt_path:
            continue
        archive_rel, _, entry = txt_path.partition("::")
        archive = corpus / archive_rel
        size = zip_entry_size(archive, entry)
        works.append(Work(work_id=work_id, archive=archive, entry=entry, size=size))

    works.sort(key=lambda work: (work.size, work.work_id), reverse=True)
    return works[:sample]


def zip_entry_size(archive: pathlib.Path, entry: str) -> int:
    try:
        with zipfile.ZipFile(archive) as zf:
            if entry:
                return zf.getinfo(entry).file_size
            return max(zf.infolist(), key=lambda info: info.file_size).file_size
    except Exception:
        return 0


def read_work_bytes(work: Work) -> bytes:
    with zipfile.ZipFile(work.archive) as zf:
        if work.entry:
            return zf.read(work.entry)
        info = max(zf.infolist(), key=lambda item: item.file_size)
        return zf.read(info)


def measure_command(
    *,
    time_bin: pathlib.Path,
    label: str,
    stage: str,
    command: list[str],
    stdin_bytes: bytes,
    timeout_s: int,
    work: Work,
    out_dir: pathlib.Path,
) -> dict[str, Any]:
    with tempfile.TemporaryDirectory(prefix="parser-perf-") as td:
        tmp = pathlib.Path(td)
        time_out = tmp / "time.txt"
        stdout_path = tmp / "stdout.bin"
        stderr_path = tmp / "stderr.txt"
        full_command = [
            str(time_bin),
            "-v",
            "-o",
            str(time_out),
            "timeout",
            str(timeout_s),
            *command,
        ]
        completed = subprocess.run(
            full_command,
            input=stdin_bytes,
            stdout=stdout_path.open("wb"),
            stderr=stderr_path.open("wb"),
            check=False,
        )
        metrics = parse_time_output(time_out.read_text() if time_out.exists() else "")
        status = status_from_code(completed.returncode)
        row: dict[str, Any] = {
            "adapter": label,
            "stage": stage,
            "work_id": work.work_id,
            "size": work.size,
            "status": status,
            "exit_code": completed.returncode,
            "command": command,
            **metrics,
        }
        if status != "ok":
            row["stderr_preview"] = stderr_path.read_text(errors="replace")[:500]
        persist_failure_artifacts(out_dir, row, stdout_path, stderr_path)
        return row


def status_from_code(code: int) -> str:
    if code == 0:
        return "ok"
    if code == 124:
        return "timeout"
    return f"error:{code}"


def parse_time_output(text: str) -> dict[str, Any]:
    return {
        "wall_s": parse_wall_seconds(find_value(text, "Elapsed (wall clock) time")),
        "user_s": parse_float(find_value(text, "User time (seconds)")),
        "sys_s": parse_float(find_value(text, "System time (seconds)")),
        "cpu_percent": parse_cpu_percent(find_value(text, "Percent of CPU this job got")),
        "max_rss_kb": parse_int(find_value(text, "Maximum resident set size (kbytes)")),
    }


def find_value(text: str, label: str) -> str | None:
    for line in text.splitlines():
        line = line.strip()
        if line.startswith(label) and ": " in line:
            return line.split(": ", 1)[1].strip()
    return None


def parse_float(value: str | None) -> float | None:
    if value is None:
        return None
    try:
        return float(value)
    except ValueError:
        return None


def parse_int(value: str | None) -> int | None:
    if value is None:
        return None
    try:
        return int(value)
    except ValueError:
        return None


def parse_cpu_percent(value: str | None) -> float | None:
    if value is None:
        return None
    return parse_float(value.rstrip("%"))


def parse_wall_seconds(value: str | None) -> float | None:
    if value is None:
        return None
    parts = value.split(":")
    try:
        if len(parts) == 2:
            minutes, seconds = parts
            return int(minutes) * 60 + float(seconds)
        if len(parts) == 3:
            hours, minutes, seconds = parts
            return int(hours) * 3600 + int(minutes) * 60 + float(seconds)
        return float(value)
    except ValueError:
        return None


def persist_failure_artifacts(
    out_dir: pathlib.Path,
    row: dict[str, Any],
    stdout_path: pathlib.Path,
    stderr_path: pathlib.Path,
) -> None:
    if row["status"] == "ok":
        return
    safe = re.sub(r"[^A-Za-z0-9_.-]+", "_", f"{row['adapter']}-{row['stage']}-{row['work_id']}")
    failures = out_dir / "failures"
    failures.mkdir(parents=True, exist_ok=True)
    if stdout_path.exists():
        (failures / f"{safe}.stdout").write_bytes(stdout_path.read_bytes())
    if stderr_path.exists():
        (failures / f"{safe}.stderr").write_bytes(stderr_path.read_bytes())


def measure_aozora2html_full(
    *,
    args: argparse.Namespace,
    source: bytes,
    work: Work,
    out_dir: pathlib.Path,
) -> dict[str, Any]:
    if args.aozora2html_bin is None or args.mapper_bin is None:
        raise SystemExit(
            "--aozora2html-label requires --aozora2html-bin and --mapper-bin for full-pipeline measurement"
        )

    with tempfile.TemporaryDirectory(prefix="aozora2html-full-") as td:
        tmp = pathlib.Path(td)
        stdin_raw = tmp / "stdin.raw"
        parser_src = tmp / "parser.sjis"
        crlf_src = tmp / "parser.txt"
        xhtml = tmp / "out.html"
        script = tmp / "run.sh"
        stdin_raw.write_bytes(source)
        write_aozora2html_parser_input(source, parser_src, crlf_src)
        script.write_text(
            "\n".join(
                [
                    "#!/usr/bin/env bash",
                    "set -euo pipefail",
                    f"export GEM_HOME={shlex.quote(str(args.aozora2html_gem_home))}",
                    f"export GEM_PATH={shlex.quote(str(args.aozora2html_gem_home))}",
                    f"export PATH={shlex.quote(str(args.aozora2html_gem_home / 'bin'))}:$PATH",
                    (
                        f"{shlex.quote(str(args.aozora2html_bin))} --error-utf8 --use-unicode "
                        f"{shlex.quote(str(crlf_src))} {shlex.quote(str(xhtml))}"
                    ),
                    (
                        f"{shlex.quote(str(args.mapper_bin))} --source {shlex.quote(str(stdin_raw))} "
                        f"--xhtml {shlex.quote(str(xhtml))} --mode aat"
                    ),
                    "",
                ]
            )
        )
        script.chmod(0o755)
        return measure_command(
            time_bin=args.time_bin,
            label=args.aozora2html_label,
            stage="full_adapter",
            command=[str(script)],
            stdin_bytes=b"",
            timeout_s=args.limit_s,
            work=work,
            out_dir=out_dir,
        )


def measure_aozora2html_stages(
    *,
    args: argparse.Namespace,
    source: bytes,
    work: Work,
    out_dir: pathlib.Path,
) -> list[dict[str, Any]]:
    if args.aozora2html_bin is None or args.mapper_bin is None:
        raise SystemExit(
            "--aozora2html-label requires --aozora2html-bin and --mapper-bin for stage split"
        )

    with tempfile.TemporaryDirectory(prefix="aozora2html-stage-") as td:
        tmp = pathlib.Path(td)
        stdin_raw = tmp / "stdin.raw"
        parser_src = tmp / "parser.sjis"
        crlf_src = tmp / "parser.txt"
        xhtml = tmp / "out.html"
        stdin_raw.write_bytes(source)
        write_aozora2html_parser_input(source, parser_src, crlf_src)

        ruby = measure_command(
            time_bin=args.time_bin,
            label=args.aozora2html_label,
            stage="ruby_parser",
            command=[
                "env",
                f"GEM_HOME={args.aozora2html_gem_home}",
                f"GEM_PATH={args.aozora2html_gem_home}",
                f"PATH={args.aozora2html_gem_home / 'bin'}:{os.environ.get('PATH', '')}",
                str(args.aozora2html_bin),
                "--error-utf8",
                "--use-unicode",
                str(crlf_src),
                str(xhtml),
            ],
            stdin_bytes=b"",
            timeout_s=args.limit_s,
            work=work,
            out_dir=out_dir,
        )
        rows = [ruby]
        if ruby["status"] == "ok":
            rows.append(
                measure_command(
                    time_bin=args.time_bin,
                    label=args.aozora2html_label,
                    stage="rust_mapper",
                    command=[
                        str(args.mapper_bin),
                        "--source",
                        str(stdin_raw),
                        "--xhtml",
                        str(xhtml),
                        "--mode",
                        "aat",
                    ],
                    stdin_bytes=b"",
                    timeout_s=args.limit_s,
                    work=work,
                    out_dir=out_dir,
                )
            )
        return rows


def write_aozora2html_parser_input(
    source: bytes, parser_src: pathlib.Path, crlf_src: pathlib.Path
) -> None:
    text = decode_source_for_cp932(source)
    encoded = text.encode("cp932", errors="replace")
    if b"--------------------" in encoded:
        parser_src.write_bytes(encoded)
    else:
        header = "テスト\n著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n".encode(
            "cp932"
        )
        footer = "\n底本：テスト\n".encode("cp932")
        parser_src.write_bytes(header + encoded + footer)
    normalized = b"\r\n".join(parser_src.read_bytes().splitlines()) + b"\r\n"
    crlf_src.write_bytes(normalized)


def decode_source_for_cp932(source: bytes) -> str:
    if source.startswith(b"\xef\xbb\xbf"):
        return source[3:].decode("utf-8")
    try:
        return source.decode("utf-8")
    except UnicodeDecodeError:
        return source.decode("cp932", errors="replace")


def summarize(rows: list[dict[str, Any]]) -> list[dict[str, Any]]:
    groups: dict[tuple[str, str], list[dict[str, Any]]] = {}
    for row in rows:
        groups.setdefault((row["adapter"], row["stage"]), []).append(row)

    summary: list[dict[str, Any]] = []
    for (adapter, stage), items in sorted(groups.items()):
        ok = [row for row in items if row["status"] == "ok"]
        walls = [row["wall_s"] for row in ok if isinstance(row.get("wall_s"), (int, float))]
        rss = [row["max_rss_kb"] for row in ok if isinstance(row.get("max_rss_kb"), int)]
        summary.append(
            {
                "adapter": adapter,
                "stage": stage,
                "attempted": len(items),
                "ok": len(ok),
                "timeout": sum(1 for row in items if row["status"] == "timeout"),
                "errors": sum(1 for row in items if str(row["status"]).startswith("error:")),
                "wall_s_median": median(walls),
                "wall_s_max": max(walls) if walls else None,
                "max_rss_kb_max": max(rss) if rss else None,
            }
        )
    return summary


def median(values: list[float]) -> float | None:
    if not values:
        return None
    return float(statistics.median(values))


def write_json(path: pathlib.Path, value: Any) -> None:
    path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def write_summary_md(path: pathlib.Path, payload: dict[str, Any]) -> None:
    lines = [
        "# Parser Performance Measurement",
        "",
        f"- index: `{payload['index']}`",
        f"- corpus: `{payload['corpus']}`",
        f"- timeout limit: `{payload['limit_s']}s`",
        f"- selected works: {len(payload['selected_works'])}",
        "",
        "## Summary",
        "",
        "| adapter | stage | attempted | ok | timeout | errors | median wall s | max wall s | max RSS KB |",
        "|---|---|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for row in payload["summary"]:
        lines.append(
            "| {adapter} | {stage} | {attempted} | {ok} | {timeout} | {errors} | {wall_s_median} | {wall_s_max} | {max_rss_kb_max} |".format(
                **{key: md_value(value) for key, value in row.items()}
            )
        )
    lines.extend(
        [
            "",
            "## Notes",
            "",
            "- `full_adapter` is the comparable measurement across parser adapters.",
            "- aozora2html `ruby_parser` and `rust_mapper` rows are supplemental stage diagnostics, not a separate adapter comparison axis.",
            "- These measurements are for oracle/comparison guidance; they do not imply an optimization target for aozora2html.",
            "",
        ]
    )
    path.write_text("\n".join(lines))


def md_value(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, float):
        return f"{value:.3f}"
    return str(value)


if __name__ == "__main__":
    raise SystemExit(main())
