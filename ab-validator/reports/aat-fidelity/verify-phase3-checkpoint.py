#!/usr/bin/env python3
"""Fidelity rotation checkpoint: nine PASS gate summaries, two
conversion audits, three candidates.

Per stage (stage0 / rotation-a / rotation-b): the three summaries carry
the expected stage + gate names, verdict PASS, ONE candidate commit and
ONE bin_sha256, and a version string matching the stage's exact pattern
and embedding the commit. Across stages: the three candidate commits
equal --c0/--c1/--c2 and are pairwise distinct.

Substantive detail checks (never just verdict): stage0 parity
compared==17886/missing 0/bytes diverged 0; rotation-a delta mode
container-rewrite, compared==17886, class totals summing to compared;
rotation-b confinement mode span-confinement, compared==17886, all works
span_confined or identical; rotation conformance must_fail==0 AND
must_skip==0; every perf new_timeouts==0. Conversion audits (--audit-a /
--audit-b): 17886 attempted == 17886 succeeded, 0 failed, mapping 0.2.8
with the recorded hash, and the audited dump's aat_adapter_version
matching the stage pattern with that stage's candidate commit.

Exit 0 + "CHECKPOINT OK" or exit 1 with the first violation."""

import argparse
import json
import re
import sys

CORPUS = 17886
MAPPING_VERSION = "0.2.8"
MAPPING_HASH = "sha256:952620ced4eb22f9771e6a10c3a1d4d93de604a8c33e360311f82b6e1eafc5b7"
STAGES = {
    "stage0": (
        ("parity", "conformance", "perf"),
        r"^ab-aozora 0\.1\.0 aat-schema 1 facade 0\.1\.0 wire-schema 2 \(git {c}\)$",
    ),
    "rotation-a": (
        ("delta", "conformance", "perf"),
        r"^ab-aozora 0\.2\.0 aat-schema 1 facade 0\.2\.0 wire-schema 3 \(git {c}\)$",
    ),
    "rotation-b": (
        ("confinement", "conformance", "perf"),
        r"^ab-aozora 0\.3\.0 aat-schema 1 facade 0\.2\.0 wire-schema 3 \(git {c}\)$",
    ),
}


def die(msg):
    print(f"CHECKPOINT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(1)


def load(path):
    try:
        return json.load(open(path))
    except (OSError, json.JSONDecodeError) as err:
        die(f"{path}: unreadable ({err})")


def check_audit(path, stage, version_pat, commit):
    doc = load(path)
    totals = doc.get("totals") or {}
    if not (
        totals.get("files_attempted") == CORPUS
        and totals.get("files_succeeded") == CORPUS
        and totals.get("files_failed") == 0
    ):
        die(f"{path}: audit totals fail minimums: {totals}")
    mapping = doc.get("mapping") or {}
    if (
        mapping.get("mapping_version") != MAPPING_VERSION
        or mapping.get("mapping_hash") != MAPPING_HASH
    ):
        die(f"{path}: audit mapping coordinate mismatch: {mapping}")
    candidates = doc.get("compatibility_candidates") or []
    if len(candidates) != 1:
        die(f"{path}: expected exactly one compatibility candidate")
    adapter_version = candidates[0].get("aat_adapter_version") or ""
    if not re.fullmatch(version_pat.format(c=commit), adapter_version):
        die(
            f"{path}: audited adapter_version {adapter_version!r} does not match "
            f"the {stage} candidate"
        )


def check_audit_gate_details(path, gate, details):
    expected_mode = {"delta": "container-rewrite", "confinement": "span-confinement"}[gate]
    if details.get("mode") != expected_mode:
        die(f"{path}: {gate} mode is {details.get('mode')!r}, expected {expected_mode!r}")
    if details.get("compared") != CORPUS:
        die(f"{path}: {gate} compared {details.get('compared')!r} != {CORPUS}")
    classes = details.get("classes") or {}
    if sum(classes.values()) != CORPUS:
        die(f"{path}: {gate} class totals {classes} do not sum to {CORPUS}")
    if (
        gate == "confinement"
        and classes.get("span_confined", 0) + classes.get("identical", 0) != CORPUS
    ):
        die(f"{path}: confinement classes must all be span_confined/identical: {classes}")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--stage0", nargs=3, required=True)
    ap.add_argument("--rotation-a", dest="rotation_a", nargs=3, required=True)
    ap.add_argument("--rotation-b", dest="rotation_b", nargs=3, required=True)
    ap.add_argument("--audit-a", required=True)
    ap.add_argument("--audit-b", required=True)
    ap.add_argument("--c0", required=True)
    ap.add_argument("--c1", required=True)
    ap.add_argument("--c2", required=True)
    args = ap.parse_args()
    expected_commits = {"stage0": args.c0, "rotation-a": args.c1, "rotation-b": args.c2}
    for sha in expected_commits.values():
        if not re.fullmatch(r"[0-9a-f]{40}", sha):
            die(f"candidate commit must be 40-hex: {sha!r}")
    if len(set(expected_commits.values())) != 3:
        die("candidate commits must be pairwise distinct")
    files = {"stage0": args.stage0, "rotation-a": args.rotation_a, "rotation-b": args.rotation_b}
    for stage, (gates, version_pat) in STAGES.items():
        commit = expected_commits[stage]
        bins, versions = set(), set()
        for path, gate in zip(files[stage], gates):
            doc = load(path)
            if doc.get("stage") != stage:
                die(f"{path}: stage is {doc.get('stage')!r}, expected {stage!r}")
            if doc.get("gate") != gate:
                die(f"{path}: gate is {doc.get('gate')!r}, expected {gate!r}")
            if doc.get("verdict") != "PASS":
                die(f"{path}: verdict is {doc.get('verdict')!r}")
            cand = doc.get("candidate") or {}
            if cand.get("commit") != commit:
                die(f"{path}: commit {cand.get('commit')!r} != {commit}")
            if not re.fullmatch(version_pat.format(c=commit), cand.get("version") or ""):
                die(f"{path}: version {cand.get('version')!r} fails the {stage} pattern")
            bins.add(cand.get("bin_sha256"))
            versions.add(cand.get("version"))
            details = doc.get("details") or {}
            if gate == "parity":
                if not (
                    details.get("compared") == CORPUS
                    and details.get("missing_count") == 0
                    and (details.get("bytes") or {}).get("diverged_count") == 0
                ):
                    die(f"{path}: parity details fail minimums: {details}")
            if gate in ("delta", "confinement"):
                check_audit_gate_details(path, gate, details)
            if gate == "conformance" and stage != "stage0":
                if details.get("must_fail") != 0 or details.get("must_skip") != 0:
                    die(f"{path}: conformance must gate not clean: {details}")
            if gate == "perf" and details.get("new_timeouts") != 0:
                die(f"{path}: perf new_timeouts != 0: {details}")
        if len(bins) != 1 or None in bins:
            die(f"{stage}: bin_sha256 not identical across gates: {bins}")
        if len(versions) != 1:
            die(f"{stage}: version strings differ: {versions}")
    check_audit(args.audit_a, "rotation-a", STAGES["rotation-a"][1], args.c1)
    check_audit(args.audit_b, "rotation-b", STAGES["rotation-b"][1], args.c2)
    print("CHECKPOINT OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
