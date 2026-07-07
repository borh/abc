#!/usr/bin/env python
"""Check that the monorepo import matches the split repositories.

This is a migration-time guard. It intentionally compares against sibling
checkout paths, because its job is to prove the current cutover state before
the split repositories stop being the primary integration surface.
"""

from __future__ import annotations

import argparse
import filecmp
import subprocess
import sys
from pathlib import Path


AB_VALIDATOR_SCHEMA_COPIES = {
    "data/abc-schemas/schemas/aat-parser-ir-divergence.schema.json",
    "data/abc-schemas/schemas/aat-parser-ir-mapping.schema.json",
    "data/abc-schemas/schemas/manifest.schema.json",
    "data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json",
    "data/abc-schemas/schemas/parser-ir.schema.json",
    "data/abc-schemas/schemas/source-region-coverage.schema.json",
}

AB_VALIDATOR_MONOREPO_ADDITIONS = {
    "data/abc-schemas/schemas",
}

AB_VALIDATOR_INTENTIONAL_CONTENT_DIFFS = {
    "data/abc-schemas/README.md",
}


def git_files(root: Path) -> list[str]:
    output = subprocess.check_output(
        ["git", "-C", str(root), "ls-files"],
        text=True,
    )
    return sorted(line for line in output.splitlines() if line)


def compare_common_files(
    source_root: Path,
    component_root: Path,
    common_files: set[str],
    intentional_content_diffs: set[str],
) -> list[str]:
    diffs: list[str] = []
    for rel in sorted(common_files - intentional_content_diffs):
        source = source_root / rel
        component = component_root / rel
        if source.is_symlink() or component.is_symlink():
            if (
                not source.is_symlink()
                or not component.is_symlink()
                or source.readlink() != component.readlink()
            ):
                diffs.append(rel)
            continue
        if not filecmp.cmp(source, component, shallow=False):
            diffs.append(rel)
    return diffs


def audit_exact_component(name: str, source_root: Path, component_root: Path) -> bool:
    source_files = set(git_files(source_root))
    component_files = {
        rel.removeprefix(f"{name}/")
        for rel in git_files(component_root.parent)
        if rel.startswith(f"{name}/")
    }

    missing = sorted(source_files - component_files)
    extra = sorted(component_files - source_files)
    content_diffs = compare_common_files(
        source_root,
        component_root,
        source_files & component_files,
        set(),
    )

    print(f"{name}: {len(source_files)} tracked source files")
    if not missing and not extra and not content_diffs:
        print(f"{name}: exact tracked-file parity ok")
        return True

    if missing:
        print(f"{name}: missing from monorepo: {missing}", file=sys.stderr)
    if extra:
        print(f"{name}: extra in monorepo: {extra}", file=sys.stderr)
    if content_diffs:
        print(f"{name}: content differs: {content_diffs}", file=sys.stderr)
    return False


def audit_ab_validator(source_root: Path, component_root: Path, abc_component: Path) -> bool:
    source_files = set(git_files(source_root))
    component_files = {
        rel.removeprefix("ab-validator/")
        for rel in git_files(component_root.parent)
        if rel.startswith("ab-validator/")
    }

    expected_source_files = source_files - AB_VALIDATOR_SCHEMA_COPIES
    expected_component_files = expected_source_files | AB_VALIDATOR_MONOREPO_ADDITIONS

    missing = sorted(expected_component_files - component_files)
    extra = sorted(component_files - expected_component_files)
    content_diffs = compare_common_files(
        source_root,
        component_root,
        expected_source_files & component_files,
        AB_VALIDATOR_INTENTIONAL_CONTENT_DIFFS,
    )

    schema_link = component_root / "data/abc-schemas/schemas"
    expected_target = (abc_component / "schemas").resolve()
    link_ok = schema_link.is_symlink() and schema_link.resolve() == expected_target

    readme = component_root / "data/abc-schemas/README.md"
    readme_ok = (
        readme.exists()
        and "symlink to `../../../abc/schemas`" in readme.read_text(encoding="utf-8")
    )

    print(f"ab-validator: {len(source_files)} tracked source files")
    print(
        "ab-validator: intentional schema delta: "
        f"{len(AB_VALIDATOR_SCHEMA_COPIES)} vendored schema files -> schemas symlink"
    )

    ok = True
    if missing:
        print(f"ab-validator: missing from monorepo: {missing}", file=sys.stderr)
        ok = False
    if extra:
        print(f"ab-validator: unexpected extra in monorepo: {extra}", file=sys.stderr)
        ok = False
    if content_diffs:
        print(f"ab-validator: unexpected content differs: {content_diffs}", file=sys.stderr)
        ok = False
    if not link_ok:
        print(
            "ab-validator: data/abc-schemas/schemas must be a symlink to abc/schemas",
            file=sys.stderr,
        )
        ok = False
    if not readme_ok:
        print(
            "ab-validator: data/abc-schemas/README.md must document the monorepo symlink",
            file=sys.stderr,
        )
        ok = False

    if ok:
        print("ab-validator: tracked-file parity ok with documented schema symlink delta")
    return ok


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    repo_root = Path(__file__).resolve().parents[1]
    parser.add_argument("--repo-root", type=Path, default=repo_root)
    parser.add_argument("--abc-source", type=Path, default=repo_root.parent / "abc")
    parser.add_argument(
        "--ab-validator-source",
        type=Path,
        default=repo_root.parent / "ab-validator",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo_root = args.repo_root.resolve()
    abc_source = args.abc_source.resolve()
    ab_validator_source = args.ab_validator_source.resolve()

    ok = True
    ok &= audit_exact_component("abc", abc_source, repo_root / "abc")
    ok &= audit_ab_validator(
        ab_validator_source,
        repo_root / "ab-validator",
        repo_root / "abc",
    )
    return 0 if ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
