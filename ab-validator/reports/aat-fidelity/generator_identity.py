"""Input-set identity + provenance fields for an AAT dump (run-aat-full.sh).

Closes F6: the generator records both the dump's **input identity**
(`input_set_hash`) and its own **output content hash** (`output_content_hash`) in
`metadata.json`, so staleness is decidable — a dump is stale exactly when a
freshly-computed `input_set_hash` differs from the one it recorded.

Content-based, fail toward correctness: the corpus tree, adapter binary, and
feature-patterns file are hashed by content; output-affecting flags (timeout,
features, work-ids) are folded in; `--jobs` (parallelism only) is excluded. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Any

_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402
import tree_hash  # noqa: E402
import aat_hash  # noqa: E402
import hashing  # noqa: E402


def build_identity_object(
    *,
    corpus_dir: str | Path,
    adapter_version: str,
    adapter_binary: str | Path,
    feature_patterns_file: str | Path,
    timeout: str | None = None,
    features: str | None = None,
    work_ids: str | None = None,
) -> dict[str, Any]:
    """Assemble the content identity of an AAT dump's inputs."""
    return {
        "corpus_content_hash": tree_hash.tree_hash(corpus_dir),
        "adapter_version": adapter_version,
        "adapter_binary_hash": hashing.file_sha256(Path(adapter_binary)),
        "feature_patterns_hash": hashing.file_sha256(Path(feature_patterns_file)),
        "timeout": timeout,
        "features": features,
        "work_ids": work_ids,
    }


def generator_input_set_hash(**kwargs: Any) -> str:
    """input_set_hash of build_identity_object(**kwargs)."""
    return run_identity.input_set_hash(build_identity_object(**kwargs))


def provenance_fields(*, aat_dir: str | Path, **identity_kwargs: Any) -> dict[str, str]:
    """The two F6 fields to merge into metadata.json: the input identity and the
    dump's own output content hash (`hash_aat_dir` of the produced aat/ tree)."""
    return {
        "input_set_hash": generator_input_set_hash(**identity_kwargs),
        "output_content_hash": aat_hash.hash_aat_dir(aat_dir),
    }


def main(argv: list[str] | None = None) -> int:
    """Emit the input_set_hash for the given generator inputs (used by the
    run-aat-full.sh pre-build skip check)."""
    ap = argparse.ArgumentParser(description="Emit an AAT-dump input_set_hash.")
    ap.add_argument("--corpus-dir", required=True)
    ap.add_argument("--adapter-version", required=True)
    ap.add_argument("--adapter-binary", required=True)
    ap.add_argument("--feature-patterns", required=True)
    ap.add_argument("--timeout", default=None)
    ap.add_argument("--features", default=None)
    ap.add_argument("--work-ids", default=None)
    a = ap.parse_args(sys.argv[1:] if argv is None else argv)
    print(generator_input_set_hash(
        corpus_dir=a.corpus_dir, adapter_version=a.adapter_version,
        adapter_binary=a.adapter_binary, feature_patterns_file=a.feature_patterns,
        timeout=a.timeout, features=a.features, work_ids=a.work_ids,
    ))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
