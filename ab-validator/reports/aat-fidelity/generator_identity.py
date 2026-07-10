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
import json
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
    ab_index_binary: str | Path,
    ab_check_binary: str | Path,
    feature_patterns_file: str | Path,
    renderer_dir: str | Path | None = None,
    timeout: str | None = None,
    features: str | None = None,
    work_ids: str | None = None,
) -> dict[str, Any]:
    """Assemble the content identity of an AAT dump's inputs.

    `ab_index_binary` and `ab_check_binary` are hashed by content: both binaries
    produce the `aat/` tree (`ab-index`'s `index.json` feeds `ab-check`), so a
    logic change in either must invalidate a dump's identity — otherwise stale
    output is served as fresh.

    `renderer_dir` is the external parser/renderer an adapter invokes as a
    subprocess to produce the `aat/` output — the upstream `aozora` parser for
    the aozora adapter, the Ruby aozora2html gem for aozora2html, or
    AozoraEpub3.jar for aozora-epub3. Since it determines the output, its nix
    package dir is hashed by content (`tree_hash`), same as the corpus. It is
    `None` only when no external parser/renderer is supplied.

    `work_ids` is a *file path* (`ab-check --work-ids` is a `PathBuf` whose JSON
    content selects works), so it is hashed by CONTENT, not by its path string —
    otherwise an in-place edit of the work-ids file would leave identity unchanged
    while the selected works, and thus the output, changed. `features` is a
    genuine inline comma list, so its literal value is the identity.
    """
    return {
        "corpus_content_hash": tree_hash.tree_hash(corpus_dir),
        "adapter_version": adapter_version,
        "adapter_binary_hash": hashing.file_sha256(Path(adapter_binary)),
        "ab_index_binary_hash": hashing.file_sha256(Path(ab_index_binary)),
        "ab_check_binary_hash": hashing.file_sha256(Path(ab_check_binary)),
        "renderer_content_hash": (
            tree_hash.tree_hash(renderer_dir) if renderer_dir is not None else None
        ),
        "feature_patterns_hash": hashing.file_sha256(Path(feature_patterns_file)),
        "timeout": timeout,
        "features": features,
        "work_ids_hash": (hashing.file_sha256(Path(work_ids)) if work_ids is not None else None),
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


def identity_fields(**identity_kwargs: Any) -> dict[str, Any]:
    """The full identity object, for recording in metadata.json alongside the
    derived input_set_hash — so an audit can see WHICH input changed."""
    return build_identity_object(**identity_kwargs)


def identity_payload(**identity_kwargs: Any) -> dict[str, Any]:
    """Compute the identity object and its input_set_hash together, so a caller
    can compute identity ONCE and reuse it for both the skip check and the
    recorded metadata (instead of recomputing build_identity_object per use)."""
    obj = build_identity_object(**identity_kwargs)
    return {"input_set_hash": run_identity.input_set_hash(obj), "identity_object": obj}


def main(argv: list[str] | None = None) -> int:
    """Emit the input_set_hash for the given generator inputs (used by the
    run-aat-full.sh pre-build skip check)."""
    ap = argparse.ArgumentParser(description="Emit an AAT-dump input_set_hash.")
    ap.add_argument("--corpus-dir", required=True)
    ap.add_argument("--adapter-version", required=True)
    ap.add_argument("--adapter-binary", required=True)
    ap.add_argument("--ab-index-binary", required=True)
    ap.add_argument("--ab-check-binary", required=True)
    ap.add_argument("--feature-patterns", required=True)
    ap.add_argument("--renderer-dir", default=None)
    ap.add_argument("--timeout", default=None)
    ap.add_argument("--features", default=None)
    ap.add_argument("--work-ids", default=None)
    ap.add_argument(
        "--emit-identity",
        default=None,
        help="write {input_set_hash, identity_object} JSON to this path",
    )
    a = ap.parse_args(sys.argv[1:] if argv is None else argv)
    pay = identity_payload(
        corpus_dir=a.corpus_dir,
        adapter_version=a.adapter_version,
        adapter_binary=a.adapter_binary,
        ab_index_binary=a.ab_index_binary,
        ab_check_binary=a.ab_check_binary,
        feature_patterns_file=a.feature_patterns,
        renderer_dir=a.renderer_dir,
        timeout=a.timeout,
        features=a.features,
        work_ids=a.work_ids,
    )
    if a.emit_identity:
        Path(a.emit_identity).write_text(json.dumps(pay) + "\n", encoding="utf-8")
    print(pay["input_set_hash"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
