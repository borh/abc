#!/usr/bin/env python
import argparse
import dataclasses
import json
import os
import pathlib
import subprocess
import sys
from collections.abc import Sequence


DEFAULT_SOURCE_REV = "77a675fc2771936f9544505d922d4cd45075338c"
DEFAULT_REPORT_SUBDIR = pathlib.Path("out/reports/tei-eaj-aozora")
DEFAULT_COMPARE_SCRIPT = pathlib.Path(__file__).resolve().with_name("tei_eaj_compare.py")
DEFAULT_ALIGNMENT_PROBE_BIN = "ab-aat-to-parser-ir"


@dataclasses.dataclass(frozen=True)
class ReportContext:
    compare_script: pathlib.Path
    tei_eaj_root: pathlib.Path
    source_rev: str
    abc_melos: str | None
    abc_tei: Sequence[str]
    abc_tei_dir: Sequence[str]
    alignment_probe_bin: str = DEFAULT_ALIGNMENT_PROBE_BIN


def default_report_dir(cwd: pathlib.Path) -> pathlib.Path:
    for name in ("ABC_REPORT_DIR", "SORANOHA_REPORT_DIR"):
        value = os.environ.get(name)
        if value:
            return pathlib.Path(value)
    if (cwd / "abc").is_dir() and (cwd / "ab-validator").is_dir():
        return cwd / "abc" / DEFAULT_REPORT_SUBDIR
    return cwd / DEFAULT_REPORT_SUBDIR


def default_abc_tei_dirs(cwd: pathlib.Path) -> list[str]:
    env_value = os.environ.get("ABC_TEI_EAJ_ABC_TEI_DIRS")
    if env_value:
        return [path for path in env_value.split(os.pathsep) if path]
    candidates = [
        cwd / "target" / "soranoha" / "full-corpus-publication-basic-ja" / "artifacts",
    ]
    return [candidate.as_posix() for candidate in candidates if candidate.exists()]


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
    for path in context.abc_tei_dir:
        extras.extend(["--abc-tei-dir", path])
    return extras


def output_or_default(value: str | None, report_dir: pathlib.Path, filename: str) -> pathlib.Path:
    return pathlib.Path(value) if value else report_dir / filename


def default_alignment_probe_outputs(report_dir: pathlib.Path) -> tuple[pathlib.Path, pathlib.Path]:
    return (
        report_dir / "tei-eaj-aozora-alignment-probe.json",
        report_dir / "tei-eaj-aozora-alignment-probe.md",
    )


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
            *(["--abc", context.abc_melos] if context.abc_melos else []),
            *abc_all_work_inputs(context),
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


def alignment_probe_command(
    context: ReportContext,
    workset: pathlib.Path,
    summary_json: pathlib.Path,
    report_md: pathlib.Path,
    max_rows: int | None = None,
) -> list[str]:
    command = [
        context.alignment_probe_bin,
        "tei-eaj-alignment-probe",
        "--workset",
        str(workset),
        "--summary-json",
        str(summary_json),
        "--report-md",
        str(report_md),
    ]
    if max_rows is not None:
        command.extend(["--max-rows", str(max_rows)])
    return command


def attach_alignment_probes_to_workset(
    workset_path: pathlib.Path, alignment_probe_path: pathlib.Path
) -> int:
    workset = json.loads(workset_path.read_text(encoding="utf-8"))
    probe_report = json.loads(alignment_probe_path.read_text(encoding="utf-8"))
    probes_by_file = {
        row["tei_eaj_file"]: row["alignment_probe"]
        for row in probe_report.get("rows", [])
        if row.get("alignment_probe") is not None
    }
    attached = 0
    for row in workset.get("files", []):
        probe = probes_by_file.get(row.get("tei_eaj_file"))
        if probe is not None:
            row["alignment_probe"] = probe
            attached += 1
    workset_path.write_text(
        json.dumps(workset, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    return attached


def command_alignment_probe(
    context: ReportContext,
    workset: pathlib.Path,
    summary_json: pathlib.Path,
    report_md: pathlib.Path,
    max_rows: int | None = None,
) -> pathlib.Path:
    summary_json.parent.mkdir(parents=True, exist_ok=True)
    report_md.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        alignment_probe_command(context, workset, summary_json, report_md, max_rows),
        check=True,
    )
    attach_alignment_probes_to_workset(workset, summary_json)
    return summary_json


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


def command_all_with_probes(
    context: ReportContext, outputs: list[str], max_rows: int | None
) -> None:
    if len(outputs) not in (0, 5):
        raise SystemExit(
            "all-with-probes accepts either no output paths or exactly 5: "
            "MELOS_MD ALL_WORK_MD WORKSET_JSON ALIGNMENT_JSON ALIGNMENT_MD"
        )
    report_dir = default_report_dir(pathlib.Path.cwd())
    melos_output = outputs[0] if outputs else None
    all_work_output = outputs[1] if outputs else None
    workset_output = outputs[2] if outputs else None
    default_alignment_json, default_alignment_md = default_alignment_probe_outputs(report_dir)
    alignment_json = pathlib.Path(outputs[3]) if outputs else default_alignment_json
    alignment_md = pathlib.Path(outputs[4]) if outputs else default_alignment_md
    print(f"Updated {command_melos(context, melos_output)}")
    print(f"Updated {command_all_work(context, all_work_output)}")
    workset = command_workset(context, workset_output)
    print(f"Updated {workset}")
    print(
        f"Updated {command_alignment_probe(context, workset, alignment_json, alignment_md, max_rows)}"
    )
    print(f"Updated {alignment_md}")


def context_from_args(args: argparse.Namespace) -> ReportContext:
    return ReportContext(
        compare_script=args.compare_script,
        tei_eaj_root=args.tei_eaj_root,
        source_rev=args.source_rev,
        abc_melos=args.abc_melos,
        abc_tei=args.abc_tei,
        abc_tei_dir=args.abc_tei_dir
        if args.abc_tei or args.abc_tei_dir
        else default_abc_tei_dirs(pathlib.Path.cwd()),
        alignment_probe_bin=args.alignment_probe_bin,
    )


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Generate ABC vs TEI-EAJ/Aozora comparison reports."
    )
    parser.add_argument("--compare-script", default=DEFAULT_COMPARE_SCRIPT, type=pathlib.Path)
    parser.add_argument("--tei-eaj-root", required=True, type=pathlib.Path)
    parser.add_argument("--source-rev", default=DEFAULT_SOURCE_REV)
    parser.add_argument("--abc-melos", default=None)
    parser.add_argument(
        "--alignment-probe-bin",
        default=os.environ.get("ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN", DEFAULT_ALIGNMENT_PROBE_BIN),
    )
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
    alignment_parser = subparsers.add_parser("alignment-probe")
    alignment_parser.add_argument("workset", nargs="?")
    alignment_parser.add_argument("summary_json", nargs="?")
    alignment_parser.add_argument("report_md", nargs="?")
    alignment_parser.add_argument("--max-rows", type=int, default=None)
    all_with_probes_parser = subparsers.add_parser("all-with-probes")
    all_with_probes_parser.add_argument(
        "outputs",
        nargs="*",
        help="Optional MELOS_MD ALL_WORK_MD WORKSET_JSON ALIGNMENT_JSON ALIGNMENT_MD output paths",
    )
    all_with_probes_parser.add_argument("--max-probe-rows", type=int, default=None)
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
    elif args.command == "alignment-probe":
        report_dir = default_report_dir(pathlib.Path.cwd())
        workset = (
            pathlib.Path(args.workset)
            if args.workset
            else report_dir / "tei-eaj-aozora-workset-export.json"
        )
        summary_json = (
            pathlib.Path(args.summary_json)
            if args.summary_json
            else default_alignment_probe_outputs(report_dir)[0]
        )
        report_md = (
            pathlib.Path(args.report_md)
            if args.report_md
            else default_alignment_probe_outputs(report_dir)[1]
        )
        command_alignment_probe(context, workset, summary_json, report_md, args.max_rows)
    elif args.command == "all-with-probes":
        command_all_with_probes(context, args.outputs, args.max_probe_rows)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
