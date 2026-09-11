#!/usr/bin/env python3
"""Check that every relative Markdown link in the project's documents resolves.

A link between documents is checked here rather than in the Clojure suite
because it is a property of the repository, and the suite runs against a copy
of the `soranoha` directory alone: it cannot see a licence, a dossier or a
test file that a document points at.

Two things are checked. The target file has to exist. When the target names a
section of a Markdown file, that file has to have a heading whose identifier
matches, using the same rule `soranoha.za.markdown/slug` applies when it
renders the heading, so a link that works here is a link that works on the
served page.
"""

from __future__ import annotations

import re
import sys
import unicodedata
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ROOTS = [ROOT / "docs", ROOT / "soranoha" / "docs"]
LINK = re.compile(r"\[(?:[^\]]+)\]\(([^)\s]+)\)")
HEADING = re.compile(r"^#{1,6} (.*)$")
EXTERNAL = re.compile(r"^(?:https?://|mailto:|ftp://)")


def slug(text: str) -> str:
    kept = [
        c for c in text.lower() if unicodedata.category(c)[0] in {"L", "N", "M"} or c in " _-\t"
    ]
    return re.sub(r"\s+", "-", "".join(kept).strip())


def headings(path: Path) -> set[str]:
    return {
        slug(match.group(1))
        for line in path.read_text(encoding="utf-8").splitlines()
        if (match := HEADING.match(line))
    }


def main() -> int:
    problems: list[str] = []
    documents = sorted(p for root in ROOTS for p in root.rglob("*.md"))
    for document in documents:
        for target in LINK.findall(document.read_text(encoding="utf-8")):
            if EXTERNAL.match(target):
                continue
            path, _, fragment = target.partition("#")
            here = document.relative_to(ROOT)
            resolved = (document.parent if path else document).joinpath(path).resolve()
            if not resolved.exists():
                problems.append(f"{here}: {target} does not exist")
                continue
            if fragment and resolved.suffix == ".md":
                if fragment not in headings(resolved):
                    problems.append(f"{here}: {target} names no heading")

    for problem in problems:
        print(problem, file=sys.stderr)
    print(f"checked {len(documents)} documents, {len(problems)} unresolved links")
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main())
