#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

mapfile -t active_files < <(
  find \
    flake.nix justfile config scripts tests \
    nix soranoha/src ab-validator/research/src ab-validator/research/tools \
    ab-validator/flake.nix ab-validator/justfile ab-validator/adapters \
    ab-validator/benchmarks ab-validator/crates ab-validator/reports \
    ab-validator/scripts \
    -type f \( \
      -name '*.clj' -o \
      -name '*.nix' -o \
      -name '*.py' -o \
      -name '*.rs' -o \
      -name '*.sh' -o \
      -name 'Cargo.toml' -o \
      -name 'justfile' -o \
      -name '*.toml' \
    \) \
    -not -path '*/target/*' \
    -not -path '*/__pycache__/*' \
    -not -name 'monorepo-path-hygiene-smoke.sh' \
    -not -name 'monorepo-active-path-hygiene-smoke.sh' \
    -print | sort
)

if [[ "${#active_files[@]}" -eq 0 ]]; then
  echo "no active files found for path hygiene check" >&2
  exit 1
fi

classifier="$(mktemp)"
trap 'rm -f "$classifier"' EXIT
cat >"$classifier" <<'PY'
"""Lexically reject active paths that escape this monorepo into a sibling checkout."""

from __future__ import annotations

import argparse
import posixpath
import re
import sys
from pathlib import Path, PurePosixPath

TARGETS = {"soranoha", "ab-validator"}
ABSOLUTE_PATH_EXCEPTIONS = {
    (PurePosixPath("tests/runtime-config-smoke.sh"), "/config/research/reports"),
    (
        PurePosixPath("tests/runtime-config-smoke.sh"),
        "/config/ab-validator/morph-warehouse",
    ),
    (
        PurePosixPath("tests/runtime-config-smoke.sh"),
        "/config/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter",
    ),
    (
        PurePosixPath("tests/runtime-config-smoke.sh"),
        "/config/research/reports/tei-eaj-aozora/tei-eaj-aozora-workset-export.json",
    ),
    (PurePosixPath("tests/runtime-config-smoke.sh"), "/ab-validator/scratch/state"),
    (PurePosixPath("tests/runtime-config-smoke.sh"), "/ab-validator/research/out"),
    (
        PurePosixPath("ab-validator/reports/lib/tests/test_aat_runs.py"),
        "/db/ab-validator/aat-corpus/pinned/aat/aozora-adapter",
    ),
    (
        PurePosixPath("ab-validator/justfile"),
        "/ab-validator/scratch/state",
    ),
}
APPROVED_ROOTS = {
    "$repo_root",
    "$src",
    "$work_dir",
    "$AB_WORKSPACE_ROOT",
    "$SORANOHA_WORKSPACE_ROOT",
}
TOKEN = re.compile(r"[^\s\"'`]+")
TRIM = "()[],:;{}"


def token_suffixes(token: str) -> list[str]:
    """Expose assignment/option value suffixes without interpreting expressions."""
    cleaned = token.strip(TRIM)
    if "://" in cleaned:
        return [cleaned]
    suffixes = [cleaned]
    for index, character in enumerate(cleaned):
        if character in "=:":
            suffix = cleaned[index + 1 :].strip(TRIM).lstrip("-")
            if suffix:
                suffixes.append(suffix)
    return suffixes


def candidate_tokens(text: str) -> list[str]:
    """Return path-shaped lexical tokens without evaluating source-language syntax."""
    candidates = []
    for token in TOKEN.findall(text):
        for suffix in token_suffixes(token):
            if (
                "/" in suffix
                and any(f"{target}/" in suffix for target in TARGETS)
                and (".." in PurePosixPath(suffix).parts or suffix.startswith("/"))
            ):
                candidates.append(suffix)
    return candidates


def lexical_base(repo: PurePosixPath, source: PurePosixPath) -> PurePosixPath:
    """Select the language-defined base for static relative paths."""
    if source.suffix in {".rs", ".nix"} or source.name == "Cargo.toml":
        return repo / source.parent
    return repo


def forbidden_candidate(repo: PurePosixPath, source: PurePosixPath, raw: str) -> bool:
    """Classify one token conservatively, without shell or environment expansion."""
    candidate = raw.strip(TRIM)
    components = PurePosixPath(candidate).parts
    if not TARGETS.intersection(components):
        return False
    if "://" in candidate or candidate.startswith("{https:"):
        return False
    if any(candidate.startswith(f"{root}/") for root in APPROVED_ROOTS):
        return ".." in components
    if "$" in candidate or "{" in candidate or "}" in candidate:
        return True
    if candidate.startswith("/../") and source.suffix == ".rs":
        parts = source.parts
        if "crates" in parts:
            crate_index = parts.index("crates") + 1
            crate_root = repo.joinpath(*parts[: crate_index + 1])
            normalized = PurePosixPath(posixpath.normpath(str(crate_root) + candidate))
            try:
                relative = normalized.relative_to(repo)
            except ValueError:
                return True
            return not relative.parts or relative.parts[0] not in TARGETS
    if candidate.startswith("/"):
        return True
    normalized = PurePosixPath(posixpath.normpath(str(lexical_base(repo, source) / candidate)))
    try:
        relative = normalized.relative_to(repo)
    except ValueError:
        return True
    return not relative.parts or relative.parts[0] not in TARGETS


def self_test() -> None:
    repo = PurePosixPath("/repo")
    cases = [
        ("tests/check.sh", "../soranoha/x", True),
        ("tests/check.sh", "./../soranoha/x", True),
        ("tests/check.sh", "${ROOT}/../soranoha/x", True),
        ("tests/check.sh", "foo/../../soranoha/x", True),
        ("tests/check.sh", "foo//.././../soranoha/x", True),
        ("scripts/check.sh", "/home/user/soranoha/x", True),
        ("scripts/check.sh", "'/Users/me/ab-validator/x'", True),
        ("scripts/check.sh", "/opt/build/soranoha/x", True),
        ("tests/check.sh", "$repo_root/soranoha/x", False),
        ("tests/check.sh", "$AB_WORKSPACE_ROOT/soranoha/x", False),
        ("tests/check.sh", "$AB_WORKSPACE_ROOT/../soranoha/x", True),
        ("ab-validator/crates/c/src/lib.rs", "../../../../soranoha/data/x", False),
        ("ab-validator/crates/c/src/lib.rs", "../../../../soranoha//data/./x", False),
        ("soranoha/src/a.clj", "soranoha/data/x", False),
        ("flake.nix", "./soranoha/flake.nix", False),
        ("ab-validator/Cargo.toml", "crates/abc-helper", False),
        ("ab-validator/crates/c/tests/x.rs", "/../../../soranoha/data/x", False),
    ]
    for source, candidate, expected in cases:
        stripped = candidate.strip("'")
        if expected and stripped not in candidate_tokens(stripped):
            raise AssertionError(f"extractor skipped forbidden candidate {candidate!r}")
        actual = forbidden_candidate(repo, PurePosixPath(source), stripped)
        if actual != expected:
            raise AssertionError(f"{source}: {candidate!r}: expected {expected}, got {actual}")

    source_cases = [
        ("ROOT=../soranoha/x", True),
        ("--root=../soranoha/x", True),
        ("ROOT=/home/user/soranoha/x", True),
        ('ROOT="../soranoha/x"', True),
        ("A=B=../soranoha/x", True),
        ("ROOT:../soranoha/x", True),
        ("ROOT=${BASE:-../soranoha/x}", True),
        ("ROOT=${BASE:-${OTHER:-../soranoha/x}}", True),
        ("https://w3id.org/soranoha/schemas/x", False),
        ("schema=sha256:abc/012345", False),
        ("label=soranoha/source", False),
        ("ROOT=$repo_root/soranoha/x", False),
    ]
    source = PurePosixPath("scripts/check.sh")
    for text, expected in source_cases:
        actual = any(forbidden_candidate(repo, source, value) for value in candidate_tokens(text))
        if actual != expected:
            raise AssertionError(
                f"source text {text!r}: expected {expected}, got {actual}; "
                f"tokens={candidate_tokens(text)!r}"
            )


def scan(repo: Path, sources: list[Path]) -> int:
    repo_pure = PurePosixPath(repo.as_posix())
    findings: list[str] = []
    for source in sources:
        relative = source.relative_to(repo)
        relative_pure = PurePosixPath(relative.as_posix())
        for line_number, line in enumerate(source.read_text(errors="replace").splitlines(), 1):
            for candidate in candidate_tokens(line):
                if (relative_pure, candidate) in ABSOLUTE_PATH_EXCEPTIONS:
                    continue
                if forbidden_candidate(repo_pure, relative_pure, candidate):
                    findings.append(f"{relative}:{line_number}:{candidate}")
    if findings:
        print("\n".join(findings))
        return 1
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--repo", type=Path)
    parser.add_argument("sources", nargs="*", type=Path)
    args = parser.parse_args()
    if args.self_test:
        self_test()
    if args.repo is not None:
        return scan(args.repo.resolve(), [source.resolve() for source in args.sources])
    return 0


if __name__ == "__main__":
    sys.exit(main())
PY

python3 "$classifier" --self-test

if ! python3 "$classifier" \
  --repo "$repo_root" "${active_files[@]}"; then
  echo "active code must not escape into a sibling soranoha/ab-validator checkout" >&2
  exit 1
fi

if rg -n --fixed-strings \
  -e "references/parsers/" \
  -e "soranoha/references/TEI/P5" \
  "${active_files[@]}"; then
  echo "active code must not depend on sibling checkout or untracked references/ paths" >&2
  exit 1
fi

echo "monorepo active path hygiene smoke ok"
