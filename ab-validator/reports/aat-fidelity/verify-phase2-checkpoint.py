#!/usr/bin/env python3
"""Fail-closed Phase 2 checkpoint: three PASS gate summaries, one candidate.

Reads the three gate summary JSONs (Gate evidence schema in the Phase 2
plan) and verifies, dying on the first violation:
- every file parses and carries gate/candidate/verdict/details fields
- gate names are absorption-parity / perf / conformance-echo respectively
- all verdicts are exactly "PASS"
- all three candidate.commit equal --candidate-commit (full 40-hex)
- all three candidate.version strings are identical AND embed the commit
  (proves AB_AOZORA_GIT_REV reached every gate build)
- parity and perf attest the same bin_sha256 (same hinoki binary)
- parity: compared > 0, missing_count == 0, bytes_diverged_count == 0
- perf: new_timeouts == 0
- echo: vectors_compared > 0, differing_count == 0

Exit 0 = checkpoint holds; 1 = any violation (message on stderr)."""

import argparse
import json
import re
import sys


def die(msg):
    print(f"CHECKPOINT FAIL: {msg}", file=sys.stderr)
    raise SystemExit(1)


def field(doc, dotted, source):
    node = doc
    for part in dotted.split("."):
        if not isinstance(node, dict) or part not in node:
            die(f"{source}: missing field {dotted}")
        node = node[part]
    return node


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("parity")
    ap.add_argument("perf")
    ap.add_argument("echo")
    ap.add_argument("--candidate-commit", required=True)
    args = ap.parse_args()
    if not re.fullmatch(r"[0-9a-f]{40}", args.candidate_commit):
        die("--candidate-commit must be a full 40-hex sha")
    expected = {"parity": "absorption-parity", "perf": "perf",
                "echo": "conformance-echo"}
    docs, versions, commits = {}, set(), set()
    for name in ("parity", "perf", "echo"):
        path = getattr(args, name)
        try:
            doc = json.load(open(path))
        except (OSError, json.JSONDecodeError) as err:
            die(f"{path}: unreadable ({err})")
        docs[name] = (path, doc)
        if field(doc, "gate", path) != expected[name]:
            die(f"{path}: gate is not {expected[name]!r}")
        if field(doc, "verdict", path) != "PASS":
            die(f"{path}: verdict is not PASS")
        commits.add(field(doc, "candidate.commit", path))
        versions.add(field(doc, "candidate.version", path))
        if not field(doc, "candidate.bin_sha256", path):
            die(f"{path}: empty candidate.bin_sha256")
    if commits != {args.candidate_commit}:
        die(f"candidate commits disagree/mismatch: {sorted(commits)}")
    if len(versions) != 1:
        die(f"--version strings disagree: {sorted(versions)}")
    if args.candidate_commit not in next(iter(versions)):
        die("--version does not embed the candidate commit "
            "(AB_AOZORA_GIT_REV not injected?)")
    p_path, p = docs["parity"]
    q_path, q = docs["perf"]
    e_path, e = docs["echo"]
    if field(p, "candidate.bin_sha256", p_path) != field(
        q, "candidate.bin_sha256", q_path
    ):
        die("parity and perf attest different binaries")
    if field(p, "details.compared", p_path) <= 0:
        die("parity: compared not > 0")
    if field(p, "details.missing_count", p_path) != 0:
        die("parity: missing_count != 0")
    if field(p, "details.bytes_diverged_count", p_path) != 0:
        die("parity: bytes_diverged_count != 0")
    if field(q, "details.new_timeouts", q_path) != 0:
        die("perf: new_timeouts != 0")
    if field(e, "details.vectors_compared", e_path) <= 0:
        die("echo: vectors_compared not > 0")
    if field(e, "details.differing_count", e_path) != 0:
        die("echo: differing_count != 0")
    print(f"CHECKPOINT OK: three PASS gates attest {args.candidate_commit}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
