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
    "upstream-aozora-notation-spec-src",
    "upstream-aozora-parser-js-src",
    "upstream-aozora-rs-src",
    "upstream-aozora-src",
    "upstream-aozora2-src",
    "upstream-aozorabunko-extractor-src",
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


def main() -> int:
    repo_root = Path(sys.argv[1]) if len(sys.argv) > 1 else Path.cwd()
    lock_paths = [
        repo_root / "flake.lock",
        repo_root / "abc" / "flake.lock",
        repo_root / "ab-validator" / "flake.lock",
    ]

    messages: list[str] = []
    for lock_path in lock_paths:
        if lock_path.exists():
            messages.extend(check_lock(lock_path))

    errors = [message for message in messages if not message.startswith("warning:")]
    for message in messages:
        print(message, file=sys.stderr if message in errors else sys.stdout)

    if errors:
        return 1

    print("flake input policy passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
