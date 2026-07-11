#!/usr/bin/env python3
"""Phase perf gate: baseline-vs-candidate wall-time on the pinned workset.

Reads data/perf-workset.json, VERIFIES each work's source_sha256 (fail
closed on mismatch), runs each lane's argv with the work's bytes on stdin
(1 warm-up + N measured runs per work), and emits a JSON report with per-work
medians/spread, machine identity, and a blocking verdict:
- BLOCK if any candidate run times out where baseline did not
- BLOCK if candidate workset median wall-time regresses > threshold_pct
- PASS otherwise (regressions under threshold are recorded, not blocking)

Each lane is an explicit argv (not a single binary path), because some lanes
cannot be expressed by a hardcoded `<bin> inspect nodes -` invocation: a
wrapper-adapter lane may need an `env VAR=x cmd args` prefix to point its
subprocess at a pinned renderer (`env` is argv[0] in that case), while the
ab-aozora lane is a single binary, `ab-aozora --mode aat`, with no subprocess
hops. A lane's --*-id-bin is the adapter executable whose existence (-x),
sha256, and verbatim --version output are recorded as that lane's identity —
independent of what argv happens to invoke, so the report's identity claim
cannot silently diverge from what actually ran.

Usage:
  run-perf-workset.py --workset data/perf-workset.json \
      --baseline-cmd "aozora2-adapter --mode aat" --baseline-id-bin PATH \
      --candidate-cmd "ab-aozora --mode aat" --candidate-id-bin PATH \
      --corpus DIR --out report.json
"""

import argparse
import hashlib
import json
import os
import pathlib
import platform
import shlex
import statistics
import subprocess
import sys
import time


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def timed_runs(argv: list, source: bytes, warmup: int, measured: int, timeout_s: float) -> dict:
    times, timeouts = [], 0
    for i in range(warmup + measured):
        start = time.monotonic()
        try:
            subprocess.run(
                argv,
                input=source,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                timeout=timeout_s,
                check=False,
            )
            elapsed = time.monotonic() - start
        except subprocess.TimeoutExpired:
            timeouts += 1
            elapsed = timeout_s
        if i >= warmup:
            times.append(elapsed)
    return {
        "median_s": round(statistics.median(times), 4),
        "stdev_s": round(statistics.pstdev(times), 4),
        "runs_s": [round(t, 4) for t in times],
        "timeouts": timeouts,
    }


def resolve_lane_identity(id_bin: str) -> dict | None:
    """Fail-closed identity resolution for a lane's --*-id-bin: -x check,
    sha256, and verbatim --version output. Returns None (after printing a
    FAIL-CLOSED message to stderr) on any failure, so main() can exit 2
    without ever recording a lane whose identity could not be verified."""
    path = pathlib.Path(id_bin)
    if not (path.is_file() and os.access(path, os.X_OK)):
        print(f"FAIL-CLOSED: id-bin not found or not executable: {id_bin}", file=sys.stderr)
        return None
    try:
        version_proc = subprocess.run([str(path), "--version"], capture_output=True, text=True)
    except OSError as exc:
        print(f"FAIL-CLOSED: id-bin --version failed to execute: {id_bin}: {exc}", file=sys.stderr)
        return None
    if version_proc.returncode != 0:
        print(f"FAIL-CLOSED: id-bin --version failed: {id_bin}", file=sys.stderr)
        return None
    return {
        "id_bin": str(path),
        "id_bin_sha256": sha256(path),
        "id_bin_version": version_proc.stdout.strip(),
    }


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--workset", required=True)
    ap.add_argument("--baseline-cmd", required=True, help="full argv string, shlex-split")
    ap.add_argument("--candidate-cmd", required=True, help="full argv string, shlex-split")
    ap.add_argument(
        "--baseline-id-bin", required=True, help="adapter executable identifying the baseline lane"
    )
    ap.add_argument(
        "--candidate-id-bin",
        required=True,
        help="adapter executable identifying the candidate lane",
    )
    ap.add_argument("--corpus", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument(
        "--runs",
        type=int,
        default=5,
        help="measured runs per work (in addition to the workset's warmup_runs); default 5",
    )
    args = ap.parse_args()

    lanes = {}
    for label, cmd, id_bin in (
        ("baseline", args.baseline_cmd, args.baseline_id_bin),
        ("candidate", args.candidate_cmd, args.candidate_id_bin),
    ):
        identity = resolve_lane_identity(id_bin)
        if identity is None:
            return 2
        lanes[label] = {"argv": shlex.split(cmd), **identity}

    ws = json.loads(pathlib.Path(args.workset).read_text())
    proto = ws["protocol"]
    corpus = pathlib.Path(args.corpus)
    report = {
        "workset_id": ws["workset_id"],
        "machine": {
            "node": platform.node(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "cpu_count": os.cpu_count(),
        },
        "bins": lanes,
        "works": [],
    }
    for work in ws["works"]:
        src_path = corpus / work["corpus_relpath"]
        try:
            actual = sha256(src_path)
        except FileNotFoundError:
            print(
                f"FAIL-CLOSED: {work['work_id']} source missing at {src_path} — "
                f"run extract-perf-workset-corpus.py first",
                file=sys.stderr,
            )
            return 2
        if actual != work["source_sha256"]:
            print(f"FAIL-CLOSED: {work['work_id']} sha256 {actual} != pinned", file=sys.stderr)
            return 2
        source = src_path.read_bytes()
        row = {"work_id": work["work_id"]}
        for label in ("baseline", "candidate"):
            row[label] = timed_runs(
                lanes[label]["argv"],
                source,
                proto["warmup_runs"],
                args.runs,
                proto["per_work_timeout_seconds"],
            )
        report["works"].append(row)

    base_med = statistics.median(w["baseline"]["median_s"] for w in report["works"])
    cand_med = statistics.median(w["candidate"]["median_s"] for w in report["works"])
    regression_pct = 100.0 * (cand_med - base_med) / base_med if base_med else 0.0
    new_timeouts = any(
        w["candidate"]["timeouts"] > w["baseline"]["timeouts"] for w in report["works"]
    )
    report["summary"] = {
        "baseline_workset_median_s": round(base_med, 4),
        "candidate_workset_median_s": round(cand_med, 4),
        "regression_pct": round(regression_pct, 2),
        "threshold_pct": proto["median_regression_block_threshold_pct"],
        "new_timeouts": new_timeouts,
        "verdict": "BLOCK"
        if new_timeouts or regression_pct > proto["median_regression_block_threshold_pct"]
        else "PASS",
    }
    pathlib.Path(args.out).write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report["summary"], indent=2))
    return 0 if report["summary"]["verdict"] == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
