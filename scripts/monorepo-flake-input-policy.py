#!/usr/bin/env python
"""Check release-critical flake inputs are explicitly pinned.

The root flake.lock is the monorepo's canonical lock, but source-evidence
inputs should also carry an explicit rev or release tag in their flake URL so a
routine `nix flake update` cannot silently change parser/corpus/TEI evidence.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path


RELEASE_CRITICAL_INPUTS = {
    "aozorabunko-src",
    "mecab-dic-converter-src",
    "tei-eaj-aozora-tei",
    "tei-p5",
}

INFRASTRUCTURE_INPUTS = {
    "clj-nix",
    "crane",
    "devshell",
    "flake-parts",
    "flake-utils",
    "local-pkgs",
    "nix-fetcher-data",
    "nixpkgs-lib",
    "nixpkgs",
    "root",
    "rust-overlay",
    "systems",
}


def is_explicitly_pinned(original: dict[str, object]) -> bool:
    if original.get("type") == "path":
        return True
    return bool(original.get("rev") or original.get("ref"))


def check_lock(lock_path: Path) -> list[str]:
    data = json.loads(lock_path.read_text())
    errors: list[str] = []
    warnings: list[str] = []

    for name, node in sorted(data["nodes"].items()):
        original = node.get("original")
        if not isinstance(original, dict):
            continue

        if name in RELEASE_CRITICAL_INPUTS:
            if not is_explicitly_pinned(original):
                errors.append(
                    f"{lock_path}: release-critical input {name!r} lacks an explicit rev/ref in flake.nix"
                )
        elif name not in INFRASTRUCTURE_INPUTS and original.get("type") != "path":
            warnings.append(
                f"{lock_path}: input {name!r} is not classified as release-critical or infrastructure"
            )

    return errors + [f"warning: {message}" for message in warnings]


def comparable_node(node: dict[str, object]) -> dict[str, object] | None:
    original = node.get("original")
    locked = node.get("locked")
    if not isinstance(original, dict) or original.get("type") == "path":
        return None
    return {
        "original": original,
        "locked": locked if isinstance(locked, dict) else {},
    }


def check_component_lock_coherence(repo_root: Path) -> list[str]:
    """Ensure component compatibility locks do not drift from the root lock."""
    root_lock_path = repo_root / "flake.lock"
    root_lock = json.loads(root_lock_path.read_text())
    root_nodes = root_lock.get("nodes", {})
    errors: list[str] = []

    for component in ("ab-validator",):
        component_lock_path = repo_root / component / "flake.lock"
        if not component_lock_path.exists():
            continue
        component_lock = json.loads(component_lock_path.read_text())
        for name, component_node in sorted(component_lock.get("nodes", {}).items()):
            if name == "root" or name not in root_nodes:
                continue
            component_comparable = comparable_node(component_node)
            root_comparable = comparable_node(root_nodes[name])
            if component_comparable is None or root_comparable is None:
                continue
            if component_comparable != root_comparable:
                errors.append(f"{component_lock_path}: input {name!r} differs from root flake.lock")
    return errors


def main() -> int:
    repo_root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()
    lock_paths = [
        repo_root / "flake.lock",
        repo_root / "ab-validator" / "flake.lock",
    ]

    messages: list[str] = []
    for lock_path in lock_paths:
        if lock_path.exists():
            messages.extend(check_lock(lock_path))
    messages.extend(check_component_lock_coherence(repo_root))

    errors = [message for message in messages if not message.startswith("warning:")]
    for message in messages:
        print(message, file=sys.stderr if message in errors else sys.stdout)

    if errors:
        return 1

    print("flake input policy passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
