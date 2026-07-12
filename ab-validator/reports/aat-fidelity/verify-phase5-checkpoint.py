#!/usr/bin/env python3
"""Fail-closed Phase 5 checkpoint: the single C5 candidate's three gate
summaries (delta + conformance + perf), the bare-toggle-adoption delta-audit
summary (Task 2), the real-shape conversion audit summary (Task 9/6), the
frozen-vs-live mapping-generation binding (Task 6), the admission capture
plus a *live* re-run of the admission command, the atomic repoint commit's
git-level binding (Task 10), and the post-repoint coverage re-verification
(Task 7).

Per stage (c5, the only candidate stage this phase): the three summaries
carry the expected stage + gate names, verdict PASS, ONE candidate commit
and ONE bin_sha256, and a version string matching the stage's exact pattern
and embedding the commit.

Bare-toggle-adoption audit (--audit, Task 2's independently-derived summary
over the C4 baseline / C5 candidate dumps -- never re-derived here, only
bound): verdict PASS, mode "bare-toggle-adoption", compared==17886,
details.adopted_yokogumi_pairs==1552, details.adopted_keigakomi_pairs==25,
details.declined_markers==14, details.declined_by_reason=={orphan_open:0,
orphan_close:0, reopen_rollback:14, interleave:0}. These are the
PARSER-VISIBLE corpus totals (plan amendment 3 / Task 9 reconciliation):
the gate's universe is standalone raw marker nodes in the C4 AAT dump,
not source text, and 70 source-text markers (1582/25/24, the
placement report's unchanged source-text universe) never surface as
standalone raw nodes -- see
docs/superpowers/reports/2026-07-12-bare-toggle-placement-attribution.md's
Revision 4 section.

Conversion audit (--conversion, the REAL summary shape emitted by the
aat-to-parser-ir full-audit tooling -- copy paths from the frozen Phase 4
C4 conversion-audit fixture, never an invented simplified shape):
17886 attempted == 17886 succeeded, 0 failed, mapping matching
--mapping-version / --mapping-hash exactly.

Mapping generation (--mapping-file / --mapping-version / --mapping-hash /
--frozen-mapping): the registry/converter hash is the CANONICAL document
hash -- abc_legacy_json_hash (crates/ab-aat-to-parser-ir/src/mapping.rs:
63-68), whose Python mirror is reports/lib/legacy_json_c14n.py
(canonical_json -> sha256 -> "sha256:" + hex, reports/lib/hashing.py's
sha256_hex). The canonical hash of --mapping-file's parsed JSON must equal
--mapping-hash and its mapping_version must equal --mapping-version; the
canonical hash of --frozen-mapping's parsed JSON must equal the frozen
0.3.0 hash and its mapping_version must be "0.3.0".

Admission: the capture file contains ":status :admitted" AND the live
--admission-cmd subprocess (run with cwd at the abc/ directory, since the
Clojure deps context lives there) re-confirms it: exit 0 and
":status :admitted" in its stdout. A stale capture cannot pass alone.

Repoint commit binding (`git show --name-only --pretty=format: SHA`, cwd at
the monorepo repo-root -- derived from --run-set's own git worktree, not
the ab-validator subdirectory): the changed-path set must be EXACTLY
{"ab-validator/reports/aat-fidelity/run-sets/current.json"} (Task 10
commits only that file; retention notes and docs go in other commits). The
run-set blob at SHA^ (the parent) must carry an `ab-aozora` entry whose
`adapter_version_contains` contains the C4 commit; the run-set blob at SHA
(the repoint commit itself) must carry `ab-aozora` with
`adapter_version_contains == --c5` and exactly five adapters. The live
--run-set file on disk is cross-checked against that same child shape, so a
post-repoint edit to the working tree cannot silently diverge from what was
actually committed.

Coverage: the four Contract 5 verdicts (source_region_contract,
custom_contract, tei_profile_contract, top-level) plus
parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE, plus
source_authority_gate reading SOURCE_AUTHORITY_GATE_PASS with
unallowlisted_unknown_markers_total == 0 (transitively proving the three
required zero counters -- unsupported_body_markup_occurrences,
unknown_region_occurrences, unknown_unreviewed_occurrences -- must all be 0
for the gate to read PASS; copied from check_coverage in
verify-phase4-checkpoint.py:331).

Exit 0 + "CHECKPOINT OK" or exit 1 with the first violation."""

import argparse
import json
import re
import shlex
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "lib"))
from hashing import sha256_hex  # noqa: E402
from legacy_json_c14n import canonical_json  # noqa: E402

CORPUS = 17886
STAGE = "c5"
GATES = ("delta", "conformance", "perf")
VERSION_PATTERN = r"^ab-aozora 0\.6\.0 aat-schema 2 facade 0\.3\.0 wire-schema 3 \(git {c}\)$"

AUDIT_MODE = "bare-toggle-adoption"
# Parser-visible universe (plan amendment 3 / Task 9 reconciliation): the
# gate's universe is standalone raw marker nodes in the C4 AAT dump, not
# source text. The source-text universe (1582/25/24) remains true of the
# source text but is not what the parser-visible corpus contains -- see
# docs/superpowers/reports/2026-07-12-bare-toggle-placement-attribution.md
# Revision 4.
AUDIT_ADOPTED_YOKOGUMI = 1552
AUDIT_ADOPTED_KEIGAKOMI = 25
AUDIT_DECLINED_MARKERS = 14
AUDIT_DECLINED_BY_REASON = {
    "orphan_open": 0,
    "orphan_close": 0,
    "reopen_rollback": 14,
    "interleave": 0,
}

FROZEN_MAPPING_VERSION = "0.3.0"
FROZEN_MAPPING_HASH = "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40"

REPOINT_PATH = "ab-validator/reports/aat-fidelity/run-sets/current.json"
C4_COMMIT = "27772b1b75c9ceeb0b724095bbbb47f774f3a275"


def die(msg):
    print(f"CHECKPOINT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(1)


def load(path):
    try:
        return json.load(open(path))
    except (OSError, json.JSONDecodeError) as err:
        die(f"{path}: unreadable ({err})")


def document_hash(value):
    return sha256_hex(canonical_json(value))


def check_stage(files, commit, gates=GATES, version_pat=VERSION_PATTERN, stage=STAGE):
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
    if len(bins) != 1 or None in bins:
        die(f"{stage}: bin_sha256 not identical across gates: {bins}")
    if len(versions) != 1:
        die(f"{stage}: version strings differ: {versions}")


def check_audit(path):
    doc = load(path)
    if doc.get("verdict") != "PASS":
        die(f"{path}: audit verdict is {doc.get('verdict')!r}")
    if doc.get("mode") != AUDIT_MODE:
        die(f"{path}: audit mode is {doc.get('mode')!r}, expected {AUDIT_MODE!r}")
    if doc.get("compared") != CORPUS:
        die(f"{path}: audit compared {doc.get('compared')!r} != {CORPUS}")
    details = doc.get("details") or {}
    if details.get("adopted_yokogumi_pairs") != AUDIT_ADOPTED_YOKOGUMI:
        die(
            f"{path}: adopted_yokogumi_pairs {details.get('adopted_yokogumi_pairs')!r} "
            f"!= {AUDIT_ADOPTED_YOKOGUMI}"
        )
    if details.get("adopted_keigakomi_pairs") != AUDIT_ADOPTED_KEIGAKOMI:
        die(
            f"{path}: adopted_keigakomi_pairs {details.get('adopted_keigakomi_pairs')!r} "
            f"!= {AUDIT_ADOPTED_KEIGAKOMI}"
        )
    if details.get("declined_markers") != AUDIT_DECLINED_MARKERS:
        die(
            f"{path}: declined_markers {details.get('declined_markers')!r} != {AUDIT_DECLINED_MARKERS}"
        )
    if details.get("declined_by_reason") != AUDIT_DECLINED_BY_REASON:
        die(
            f"{path}: declined_by_reason {details.get('declined_by_reason')!r} "
            f"!= {AUDIT_DECLINED_BY_REASON}"
        )


def check_conversion(path, mapping_version, mapping_hash):
    doc = load(path)
    totals = doc.get("totals") or {}
    if not (
        totals.get("files_attempted") == CORPUS
        and totals.get("files_succeeded") == CORPUS
        and totals.get("files_failed") == 0
    ):
        die(f"{path}: conversion totals fail minimums: {totals}")
    mapping = doc.get("mapping") or {}
    if (
        mapping.get("mapping_version") != mapping_version
        or mapping.get("mapping_hash") != mapping_hash
    ):
        die(f"{path}: conversion mapping coordinate mismatch: {mapping}")


def check_mapping_generation(mapping_file, mapping_version, mapping_hash, frozen_mapping):
    live_doc = load(mapping_file)
    if live_doc.get("mapping_version") != mapping_version:
        die(
            f"{mapping_file}: mapping_version {live_doc.get('mapping_version')!r} "
            f"!= --mapping-version {mapping_version!r}"
        )
    live_hash = document_hash(live_doc)
    if live_hash != mapping_hash:
        die(f"{mapping_file}: canonical hash {live_hash} != --mapping-hash {mapping_hash}")

    frozen_doc = load(frozen_mapping)
    if frozen_doc.get("mapping_version") != FROZEN_MAPPING_VERSION:
        die(
            f"{frozen_mapping}: mapping_version {frozen_doc.get('mapping_version')!r} "
            f"!= {FROZEN_MAPPING_VERSION!r}"
        )
    frozen_hash = document_hash(frozen_doc)
    if frozen_hash != FROZEN_MAPPING_HASH:
        die(f"{frozen_mapping}: canonical hash {frozen_hash} != frozen {FROZEN_MAPPING_HASH}")


# check_admission / repo_root_for / git_show / git_show_names copied
# verbatim from verify-phase4-checkpoint.py (the frozen Phase 4 instrument)
# -- never refactor that file to share these helpers.


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


def assert_repoint_run_set_shape(doc, c5, label):
    adapters = doc.get("adapters") or {}
    if "ab-aozora" not in adapters:
        die(f"{label}: `ab-aozora` adapter missing from the run-set")
    contains = (adapters["ab-aozora"].get("expected") or {}).get("adapter_version_contains")
    if contains != c5:
        die(f"{label}: ab-aozora expected.adapter_version_contains {contains!r} != --c5 {c5!r}")
    if len(adapters) != 5:
        die(
            f"{label}: expected exactly five adapters in the run-set, found {len(adapters)}: "
            f"{sorted(adapters)}"
        )


def check_repoint(sha, c5, run_set_path):
    repo_root = repo_root_for(run_set_path)
    changed = set(git_show_names(repo_root, sha))
    if changed != {REPOINT_PATH}:
        die(
            f"repoint commit {sha}: changed paths must be exactly {{{REPOINT_PATH!r}}}: {sorted(changed)}"
        )

    parent_text = git_show(repo_root, f"{sha}^:{REPOINT_PATH}")
    parent_doc = parse_run_set_blob(parent_text, f"{sha}^:current.json")
    parent_adapters = parent_doc.get("adapters") or {}
    parent_ab_aozora = parent_adapters.get("ab-aozora") or {}
    parent_contains = (parent_ab_aozora.get("expected") or {}).get("adapter_version_contains") or ""
    if C4_COMMIT not in parent_contains:
        die(
            f"{sha}^:current.json: ab-aozora adapter_version_contains {parent_contains!r} "
            f"does not carry the C4 commit {C4_COMMIT}"
        )

    child_text = git_show(repo_root, f"{sha}:{REPOINT_PATH}")
    child_doc = parse_run_set_blob(child_text, f"{sha}:current.json")
    assert_repoint_run_set_shape(child_doc, c5, f"{sha}:current.json")

    live_doc = load(run_set_path)
    assert_repoint_run_set_shape(live_doc, c5, str(run_set_path))


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
    region = doc.get("source_region_contract") or {}
    if region.get("verdict") != "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
        die(
            f"{coverage_path}: source_region_contract.verdict is not CONFIRMED_BY_ABC_INTEGRATION: {region}"
        )
    custom = doc.get("custom_contract") or {}
    if custom.get("verdict") != "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
        die(
            f"{coverage_path}: custom_contract.verdict is not CONFIRMED_BY_ABC_INTEGRATION: {custom}"
        )
    tei = doc.get("tei_profile_contract") or {}
    if tei.get("verdict") != "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION":
        die(
            f"{coverage_path}: tei_profile_contract.verdict is not CONFIRMED_BY_ABC_INTEGRATION: {tei}"
        )
    if doc.get("verdict") != "IR_PUBLICATION_COVERAGE_COMPLETE":
        die(
            f"{coverage_path}: top-level verdict is not IR_PUBLICATION_COVERAGE_COMPLETE: {doc.get('verdict')!r}"
        )
    evidence = doc.get("parser_evidence_coverage") or {}
    if evidence.get("verdict") != "FIVE_PARSER_EVIDENCE_COMPLETE":
        die(
            f"{coverage_path}: parser_evidence_coverage.verdict is not FIVE_PARSER_EVIDENCE_COMPLETE: {evidence}"
        )


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--c5-gates", dest="c5_gates", nargs=3, required=True)
    ap.add_argument("--audit", required=True)
    ap.add_argument("--conversion", required=True)
    ap.add_argument("--admission-capture", required=True)
    ap.add_argument("--admission-cmd", required=True)
    ap.add_argument("--repoint-commit", required=True)
    ap.add_argument("--run-set", required=True)
    ap.add_argument("--coverage", required=True)
    ap.add_argument("--c5", required=True)
    ap.add_argument("--mapping-file", required=True)
    ap.add_argument("--mapping-version", required=True)
    ap.add_argument("--mapping-hash", required=True)
    ap.add_argument("--frozen-mapping", required=True)
    args = ap.parse_args()

    if not re.fullmatch(r"[0-9a-f]{40}", args.c5):
        die(f"--c5 candidate commit must be 40-hex: {args.c5!r}")

    check_stage(args.c5_gates, args.c5)
    check_audit(args.audit)
    check_conversion(args.conversion, args.mapping_version, args.mapping_hash)
    check_mapping_generation(
        args.mapping_file, args.mapping_version, args.mapping_hash, args.frozen_mapping
    )

    repo_root = repo_root_for(args.run_set)
    abc_cwd = repo_root / "abc"
    check_admission(args.admission_capture, args.admission_cmd, abc_cwd)

    check_repoint(args.repoint_commit, args.c5, args.run_set)

    check_coverage(args.coverage)

    print("CHECKPOINT OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
