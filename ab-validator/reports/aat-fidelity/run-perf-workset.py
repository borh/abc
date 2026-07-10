#!/usr/bin/env python3
"""Phase perf gate: baseline-vs-candidate wall-time on the pinned workset.

Reads data/perf-workset.json, VERIFIES each work's source_sha256 (fail
closed on mismatch), runs each binary as `<bin> inspect nodes -` with
1 warm-up + N measured runs per work, and emits a JSON report with per-work
medians/spread, machine identity, and a blocking verdict:
- BLOCK if any candidate run times out where baseline did not
- BLOCK if candidate workset median wall-time regresses > threshold_pct
- PASS otherwise (regressions under threshold are recorded, not blocking)

Usage:
  run-perf-workset.py --workset data/perf-workset.json \
      --baseline-bin PATH --candidate-bin PATH --corpus DIR --out report.json
"""

import argparse
import hashlib
import json
import pathlib
import platform
import statistics
import subprocess
import sys
import time


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def timed_runs(bin_path: str, source: bytes, warmup: int, measured: int, timeout_s: float) -> dict:
    times, timeouts = [], 0
    for i in range(warmup + measured):
        start = time.monotonic()
        try:
            subprocess.run(
                [bin_path, "inspect", "nodes", "-"],
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


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--workset", required=True)
    ap.add_argument("--baseline-bin", required=True)
    ap.add_argument("--candidate-bin", required=True)
    ap.add_argument("--corpus", required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    ws = json.loads(pathlib.Path(args.workset).read_text())
    proto = ws["protocol"]
    corpus = pathlib.Path(args.corpus)
    report = {
        "workset_id": ws["workset_id"],
        "machine": {
            "node": platform.node(),
            "machine": platform.machine(),
            "processor": platform.processor(),
            "cpu_count": __import__("os").cpu_count(),
        },
        "bins": {
            "baseline": {
                "path": args.baseline_bin,
                "version": subprocess.run(
                    [args.baseline_bin, "--version"], capture_output=True, text=True
                ).stdout.strip(),
            },
            "candidate": {
                "path": args.candidate_bin,
                "version": subprocess.run(
                    [args.candidate_bin, "--version"], capture_output=True, text=True
                ).stdout.strip(),
            },
        },
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
        for label, bin_path in (("baseline", args.baseline_bin), ("candidate", args.candidate_bin)):
            row[label] = timed_runs(
                bin_path,
                source,
                proto["warmup_runs"],
                proto["measured_runs"],
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
