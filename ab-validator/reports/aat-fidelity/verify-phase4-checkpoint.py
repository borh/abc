#!/usr/bin/env python3
"""Fail-closed Phase 4 checkpoint: six PASS gate summaries (c3/c4), two
conversion audits, the terminal-provenance split summary, the admission
capture plus a *live* re-run of the admission command, the activation
commit's git-level binding, the live post-activation run-set, and the
post-activation coverage re-verification.

Per stage (c3 / c4): the three summaries carry the expected stage + gate
names, verdict PASS, ONE candidate commit and ONE bin_sha256, and a version
string matching the stage's exact pattern and embedding the commit. Across
stages: --c3/--c4 are 40-hex and pairwise distinct.

Substantive detail checks (never just verdict): c3 migration
compared==17886, class totals summing to compared; c4 confinement
compared==17886, class totals summing to compared, classes all
identical/source_note_appended, and
classes.source_note_appended == split.works_with_terminal_provenance;
conformance must_fail==0 AND must_skip==0 AND differing_full==0 AND
differing_seed==0; every perf new_timeouts==0, c4 perf additionally
activation_floor_ok==true. Conversion audits (--audit-c3/--audit-c4):
17886 attempted == 17886 succeeded, 0 failed, mapping 0.3.0 with the
recorded hash, and the audited dump's aat_adapter_version matching the
stage pattern with that stage's candidate commit.

Admission: the capture file contains ":status :admitted" AND the live
--admission-cmd subprocess (run with cwd at the abc/ directory, since the
Clojure deps context lives there) re-confirms it: exit 0 and
":status :admitted" in its stdout. A stale capture cannot pass alone.

Activation commit binding (`git show --name-only --pretty=format: SHA`,
cwd at the monorepo repo-root -- derived from --run-set's own git
worktree, not the ab-validator subdirectory, since the run-set swap and
the abc/ fixture updates are siblings under ONE monorepo root): the
changed-path set must be a superset of ACTIVATION_REQUIRED; every other
changed path must start with an ACTIVATION_ALLOWED_PREFIXES entry (this
admits the abc/examples/ab-validator-output/ fixture refresh and the one
controller-ruled fixture-coupled test-literal fix in
abc/test/abc/tools/materialize_import_test.clj -- both are Task 20
ceremony-documented, not retirement deletions or stray drift). The
run-set blob at SHA^ (the parent) must carry `aozora` and must NOT carry
`ab-aozora`; the run-set blob at SHA (the activation commit itself) must
carry `ab-aozora` (with expected.adapter_version_contains == --c4) and
must NOT carry `aozora`, with exactly five adapters. The live --run-set
file on disk is cross-checked against that same child shape, so a
post-activation edit to the working tree cannot silently diverge from
what was actually committed.

Coverage: source_authority_gate reads SOURCE_AUTHORITY_GATE_PASS with
unallowlisted_unknown_markers_total == 0 (transitively proving the three
required zero counters -- unsupported_body_markup_occurrences,
unknown_region_occurrences, unknown_unreviewed_occurrences -- which by
construction of that upstream frozen source-authority summary must all be
0 for the gate to read PASS; ab-validator/docs/superpowers/reports/
2026-07-12-phase4-postactivation-coverage.md records the three raw
counters checked directly against that frozen input too), and
parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE. Because
that verdict's own lane list is derived from the frozen five-way TEI-EAJ
matrix and still names the legacy `aozora` lane rather than `ab-aozora`
(the matrix predates the rename; REQUIRED_PARSERS in
reports/parser-ir/publication-coverage.py is a frozen tuple), `ab-aozora`
presence is instead asserted against the live --run-set file's adapter
keys -- the authoritative post-activation lane list.

Exit 0 + "CHECKPOINT OK" or exit 1 with the first violation."""

import argparse
import json
import re
import shlex
import subprocess
import sys
from pathlib import Path

CORPUS = 17886
MAPPING_VERSION = "0.3.0"
MAPPING_HASH = "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"
STAGES = {
    "c3": (
        ("migration", "conformance", "perf"),
        r"^ab-aozora 0\.4\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git {c}\)$",
    ),
    "c4": (
        ("confinement", "conformance", "perf"),
        r"^ab-aozora 0\.5\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git {c}\)$",
    ),
}
CONTENT_GATE_MODE = {"migration": "v2-migration", "confinement": "source-note-append"}
ACTIVATION_REQUIRED = {
    "ab-validator/reports/aat-fidelity/run-sets/current.json",
    "ab-validator/reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json",
    "ab-validator/docs/handoffs/ir-publication-coverage-contract.md",
    "ab-validator/justfile",
}
ACTIVATION_ALLOWED_PREFIXES = (
    "abc/examples/ab-validator-output/",
    # Controller-ruled fixture-coupled addition, folded into the activation
    # commit so the tree stays self-consistent: this test's pinned
    # mapping-hash literal was stale against the ab-validator-output
    # fixtures refreshed above, not an unrelated stray change.
    "abc/test/abc/tools/materialize_import_test.clj",
)


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


def check_content_gate_details(path, gate, details, split_doc):
    expected_mode = CONTENT_GATE_MODE[gate]
    if details.get("mode") != expected_mode:
        die(f"{path}: {gate} mode is {details.get('mode')!r}, expected {expected_mode!r}")
    if details.get("compared") != CORPUS:
        die(f"{path}: {gate} compared {details.get('compared')!r} != {CORPUS}")
    classes = details.get("classes") or {}
    if sum(classes.values()) != CORPUS:
        die(f"{path}: {gate} class totals {classes} do not sum to {CORPUS}")
    if gate == "confinement":
        if classes.get("identical", 0) + classes.get("source_note_appended", 0) != CORPUS:
            die(
                f"{path}: confinement classes must all be identical/source_note_appended: {classes}"
            )
        split_count = split_doc.get("works_with_terminal_provenance")
        if classes.get("source_note_appended") != split_count:
            die(
                f"{path}: confinement source_note_appended {classes.get('source_note_appended')!r} "
                f"!= split.works_with_terminal_provenance {split_count!r}"
            )
        if details.get("split_works_with_terminal_provenance") != split_count:
            die(
                f"{path}: confinement split_works_with_terminal_provenance "
                f"{details.get('split_works_with_terminal_provenance')!r} != split doc {split_count!r}"
            )


def check_conformance_gate_details(path, details):
    if (
        details.get("must_fail") != 0
        or details.get("must_skip") != 0
        or details.get("differing_full") != 0
        or details.get("differing_seed") != 0
    ):
        die(f"{path}: conformance gate not clean: {details}")


def check_perf_gate_details(path, stage, details):
    if details.get("new_timeouts") != 0:
        die(f"{path}: perf new_timeouts != 0: {details}")
    if stage == "c4" and details.get("activation_floor_ok") is not True:
        die(
            f"{path}: c4 perf activation_floor_ok is not true: {details.get('activation_floor_ok')!r}"
        )


def check_stage(stage, files, commit, version_pat, gates, split_doc):
    bins, versions = set(), set()
    for path, gate in zip(files, gates):
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
        if gate in ("migration", "confinement"):
            check_content_gate_details(path, gate, details, split_doc)
        elif gate == "conformance":
            check_conformance_gate_details(path, details)
        elif gate == "perf":
            check_perf_gate_details(path, stage, details)
    if len(bins) != 1 or None in bins:
        die(f"{stage}: bin_sha256 not identical across gates: {bins}")
    if len(versions) != 1:
        die(f"{stage}: version strings differ: {versions}")


def check_admission(capture_path, admission_cmd, abc_cwd):
    capture_text = Path(capture_path).read_text()
    if ":status :admitted" not in capture_text:
        die(f"{capture_path}: admission capture does not carry :status :admitted")
    try:
        argv = shlex.split(admission_cmd)
        result = subprocess.run(argv, capture_output=True, text=True, cwd=abc_cwd)
    except OSError as err:
        die(f"admission re-run failed to launch: {err}")
    if result.returncode != 0:
        die(f"admission re-run exited {result.returncode}: {result.stderr.strip()}")
    if ":status :admitted" not in (result.stdout or ""):
        die(f"admission re-run stdout lacks :status :admitted: {result.stdout!r}")


def repo_root_for(run_set_path):
    try:
        out = subprocess.run(
            ["git", "-C", str(Path(run_set_path).resolve().parent), "rev-parse", "--show-toplevel"],
            capture_output=True,
            text=True,
            check=True,
        )
    except (OSError, subprocess.CalledProcessError) as err:
        die(f"could not resolve repo root from --run-set {run_set_path}: {err}")
    return Path(out.stdout.strip())


def git_show(repo_root, rev):
    result = subprocess.run(
        ["git", "show", rev],
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    if result.returncode != 0:
        die(f"git show {rev} failed: {result.stderr.strip()}")
    return result.stdout


def git_show_names(repo_root, sha):
    result = subprocess.run(
        ["git", "show", "--name-only", "--pretty=format:", sha],
        capture_output=True,
        text=True,
        cwd=repo_root,
    )
    if result.returncode != 0:
        die(f"git show --name-only {sha} failed: {result.stderr.strip()}")
    return [line for line in result.stdout.splitlines() if line.strip()]


def parse_run_set_blob(text, label):
    try:
        return json.loads(text)
    except json.JSONDecodeError as err:
        die(f"{label}: run-set blob does not parse: {err}")


def assert_child_run_set_shape(doc, c4, label):
    adapters = doc.get("adapters") or {}
    if "aozora" in adapters:
        die(f"{label}: legacy `aozora` adapter still present in the child run-set")
    if "ab-aozora" not in adapters:
        die(f"{label}: `ab-aozora` adapter missing from the child run-set")
    contains = (adapters["ab-aozora"].get("expected") or {}).get("adapter_version_contains")
    if contains != c4:
        die(f"{label}: ab-aozora expected.adapter_version_contains {contains!r} != --c4 {c4!r}")
    if len(adapters) != 5:
        die(
            f"{label}: expected exactly five adapters in the child run-set, found {len(adapters)}: {sorted(adapters)}"
        )


def check_activation(sha, c4, run_set_path):
    repo_root = repo_root_for(run_set_path)
    changed = set(git_show_names(repo_root, sha))
    missing_required = ACTIVATION_REQUIRED - changed
    if missing_required:
        die(f"activation commit {sha}: missing required changed paths: {sorted(missing_required)}")
    stray = sorted(
        path
        for path in changed - ACTIVATION_REQUIRED
        if not any(
            path.startswith(prefix) or path == prefix for prefix in ACTIVATION_ALLOWED_PREFIXES
        )
    )
    if stray:
        die(
            f"activation commit {sha}: unexplained changed paths outside ACTIVATION_ALLOWED_PREFIXES: {stray}"
        )

    parent_text = git_show(
        repo_root, f"{sha}^:ab-validator/reports/aat-fidelity/run-sets/current.json"
    )
    parent_doc = parse_run_set_blob(parent_text, f"{sha}^:current.json")
    parent_adapters = parent_doc.get("adapters") or {}
    if "aozora" not in parent_adapters:
        die(f"{sha}^:current.json: legacy `aozora` adapter missing from the parent run-set")
    if "ab-aozora" in parent_adapters:
        die(f"{sha}^:current.json: `ab-aozora` adapter already present in the parent run-set")

    child_text = git_show(
        repo_root, f"{sha}:ab-validator/reports/aat-fidelity/run-sets/current.json"
    )
    child_doc = parse_run_set_blob(child_text, f"{sha}:current.json")
    assert_child_run_set_shape(child_doc, c4, f"{sha}:current.json")

    live_doc = load(run_set_path)
    assert_child_run_set_shape(live_doc, c4, str(run_set_path))
    return live_doc


def check_coverage(coverage_path):
    doc = load(coverage_path)
    gate = doc.get("source_authority_gate") or {}
    if gate.get("gate_status") != "SOURCE_AUTHORITY_GATE_PASS":
        die(
            f"{coverage_path}: source_authority_gate.gate_status is not SOURCE_AUTHORITY_GATE_PASS: {gate}"
        )
    if gate.get("unallowlisted_unknown_markers_total") != 0:
        die(
            f"{coverage_path}: source_authority_gate.unallowlisted_unknown_markers_total != 0: {gate}"
        )
    evidence = doc.get("parser_evidence_coverage") or {}
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        die(
            f"{coverage_path}: parser_evidence_coverage.verdict is not FIVE_PARSER_EVIDENCE_COMPLETE: {evidence}"
        )


def check_run_set_lane_list(live_run_set_doc, run_set_path):
    adapters = live_run_set_doc.get("adapters") or {}
    if "ab-aozora" not in adapters:
        die(f"{run_set_path}: ab-aozora missing from the live run-set lane list")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--c3-gates", dest="c3_gates", nargs=3, required=True)
    ap.add_argument("--c4-gates", dest="c4_gates", nargs=3, required=True)
    ap.add_argument("--audit-c3", required=True)
    ap.add_argument("--audit-c4", required=True)
    ap.add_argument("--split", required=True)
    ap.add_argument("--admission-capture", required=True)
    ap.add_argument("--admission-cmd", required=True)
    ap.add_argument("--activation-commit", required=True)
    ap.add_argument("--run-set", required=True)
    ap.add_argument("--coverage", required=True)
    ap.add_argument("--c3", required=True)
    ap.add_argument("--c4", required=True)
    args = ap.parse_args()

    for name, sha in (("--c3", args.c3), ("--c4", args.c4)):
        if not re.fullmatch(r"[0-9a-f]{40}", sha):
            die(f"{name} candidate commit must be 40-hex: {sha!r}")
    if args.c3 == args.c4:
        die("candidate commits --c3/--c4 must be distinct")

    split_doc = load(args.split)

    files = {"c3": args.c3_gates, "c4": args.c4_gates}
    commits = {"c3": args.c3, "c4": args.c4}
    for stage, (gates, version_pat) in STAGES.items():
        check_stage(stage, files[stage], commits[stage], version_pat, gates, split_doc)

    check_audit(args.audit_c3, "c3", STAGES["c3"][1], args.c3)
    check_audit(args.audit_c4, "c4", STAGES["c4"][1], args.c4)

    repo_root = repo_root_for(args.run_set)
    abc_cwd = repo_root / "abc"
    check_admission(args.admission_capture, args.admission_cmd, abc_cwd)

    live_run_set_doc = check_activation(args.activation_commit, args.c4, args.run_set)
    check_run_set_lane_list(live_run_set_doc, args.run_set)

    check_coverage(args.coverage)

    print("CHECKPOINT OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
