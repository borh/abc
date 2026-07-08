#!/usr/bin/env python
import argparse
import dataclasses
import os
import pathlib
import subprocess
import sys
from collections.abc import Sequence


DEFAULT_SOURCE_REV = "77a675fc2771936f9544505d922d4cd45075338c"
DEFAULT_REPORT_SUBDIR = pathlib.Path("out/reports/tei-eaj-aozora")
DEFAULT_COMPARE_SCRIPT = pathlib.Path(__file__).resolve().with_name("tei_eaj_compare.py")


@dataclasses.dataclass(frozen=True)
class ReportContext:
    compare_script: pathlib.Path
    tei_eaj_root: pathlib.Path
    source_rev: str
    abc_melos: str
    abc_tei: Sequence[str]
    abc_tei_dir: Sequence[str]


def default_report_dir(cwd: pathlib.Path) -> pathlib.Path:
    for name in ("ABC_REPORT_DIR", "SORANOHA_REPORT_DIR"):
        value = os.environ.get(name)
        if value:
            return pathlib.Path(value)
    if (cwd / "abc").is_dir() and (cwd / "ab-validator").is_dir():
        return cwd / "abc" / DEFAULT_REPORT_SUBDIR
    return cwd / DEFAULT_REPORT_SUBDIR


def run_compare(context: ReportContext, extra: list[str], output: pathlib.Path) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    command = [
        sys.executable,
        str(context.compare_script),
        "--tei-eaj-root",
        str(context.tei_eaj_root),
        "--source-rev",
        context.source_rev,
        "--output",
        str(output),
        *extra,
    ]
    subprocess.run(command, check=True)


def abc_all_work_inputs(context: ReportContext) -> list[str]:
    extras: list[str] = []
    for spec in context.abc_tei:
        extras.extend(["--abc-tei", spec])
    dirs = context.abc_tei_dir if context.abc_tei or context.abc_tei_dir else ["paper"]
    for path in dirs:
        extras.extend(["--abc-tei-dir", path])
    return extras


def output_or_default(value: str | None, report_dir: pathlib.Path, filename: str) -> pathlib.Path:
    return pathlib.Path(value) if value else report_dir / filename


def command_melos(context: ReportContext, output: str | None) -> pathlib.Path:
    target = output_or_default(
        output,
        default_report_dir(pathlib.Path.cwd()),
        "tei-eaj-aozora-melos-comparison-report.md",
    )
    run_compare(
        context,
        [
            "--report",
            "melos",
            "--abc",
            context.abc_melos,
        ],
        target,
    )
    return target


def command_all_work(context: ReportContext, output: str | None) -> pathlib.Path:
    target = output_or_default(
        output,
        default_report_dir(pathlib.Path.cwd()),
        "tei-eaj-aozora-all-work-comparison-report.md",
    )
    run_compare(
        context,
        [
            "--report",
            "all-work",
            *abc_all_work_inputs(context),
        ],
        target,
    )
    return target


def command_workset(context: ReportContext, output: str | None) -> pathlib.Path:
    target = output_or_default(
        output,
        default_report_dir(pathlib.Path.cwd()),
        "tei-eaj-aozora-workset-export.json",
    )
    run_compare(
        context,
        [
            "--report",
            "all-work",
            "--format",
            "json",
            *abc_all_work_inputs(context),
        ],
        target,
    )
    return target


def command_all(context: ReportContext, outputs: list[str]) -> None:
    if len(outputs) not in (0, 3):
        raise SystemExit(
            "all accepts either no output paths or exactly 3: MELOS_MD ALL_WORK_MD WORKSET_JSON"
        )
    melos_output = outputs[0] if outputs else None
    all_work_output = outputs[1] if outputs else None
    workset_output = outputs[2] if outputs else None
    print(f"Updated {command_melos(context, melos_output)}")
    print(f"Updated {command_all_work(context, all_work_output)}")
    print(f"Updated {command_workset(context, workset_output)}")


def context_from_args(args: argparse.Namespace) -> ReportContext:
    return ReportContext(
        compare_script=args.compare_script,
        tei_eaj_root=args.tei_eaj_root,
        source_rev=args.source_rev,
        abc_melos=args.abc_melos,
        abc_tei=args.abc_tei,
        abc_tei_dir=args.abc_tei_dir,
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Generate ABC vs TEI-EAJ/Aozora comparison reports."
    )
    parser.add_argument("--compare-script", default=DEFAULT_COMPARE_SCRIPT, type=pathlib.Path)
    parser.add_argument("--tei-eaj-root", required=True, type=pathlib.Path)
    parser.add_argument("--source-rev", default=DEFAULT_SOURCE_REV)
    parser.add_argument("--abc-melos", default="paper/demo-melos-real/tei.xml")
    parser.add_argument(
        "--abc-tei",
        action="append",
        default=[],
        help="ABC TEI counterpart path or WORK_ID=PATH mapping",
    )
    parser.add_argument(
        "--abc-tei-dir",
        action="append",
        default=[],
        help="Directory to scan recursively for ABC TEI counterparts",
    )
    subparsers = parser.add_subparsers(dest="command")
    all_parser = subparsers.add_parser("all")
    all_parser.add_argument(
        "outputs",
        nargs="*",
        help="Optional MELOS_MD ALL_WORK_MD WORKSET_JSON output paths",
    )
    melos_parser = subparsers.add_parser("melos")
    melos_parser.add_argument("output", nargs="?")
    all_work_parser = subparsers.add_parser("all-work")
    all_work_parser.add_argument("output", nargs="?")
    workset_parser = subparsers.add_parser("workset-json")
    workset_parser.add_argument("output", nargs="?")
    args = parser.parse_args(argv)
    context = context_from_args(args)

    if args.command in (None, "all"):
        command_all(context, getattr(args, "outputs", []))
    elif args.command == "melos":
        command_melos(context, args.output)
    elif args.command == "all-work":
        command_all_work(context, args.output)
    elif args.command == "workset-json":
        command_workset(context, args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
