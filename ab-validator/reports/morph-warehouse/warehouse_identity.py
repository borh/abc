"""Input-set identity for a morph-warehouse run.

Builds the identity_object from the exact arguments a warehouse recipe passes to
`ab-morph-run analyze-aat`, then reduces it to one input_set_hash via run_identity.
Content-based and fail-toward-correctness: every knob that changes the analyzed
output is folded in; parallelism (--jobs), temp dirs, and report ids are excluded
(they change neither which works are analyzed nor the emitted bytes). Optional
knobs are represented explicitly (null when the flag is not passed) so that "flag
absent" and "flag present" are unambiguously different identities. See
docs/superpowers/specs/2026-07-09-batch-run-staleness-skip-recompute-design.md.
"""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

_LIB = Path(__file__).resolve().parents[1] / "lib"
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402
import aat_hash  # noqa: E402
import hashing  # noqa: E402


def build_identity_object(
    *,
    aat_dir: str | Path,
    engine_binary: str | Path,
    dictionaries: dict[str, str],
    analyzers: list[str],
    warehouse_profile: str,
    schema_files: list[str | Path],
    ortho_detect: str | None = None,
    works_parquet: str | Path | None = None,
) -> dict[str, Any]:
    """Assemble the content identity of a warehouse run's inputs.

    - aat_dir: the checked AAT dump directory; hashed by content (hash_aat_dir).
    - engine_binary: the built `ab-morph-run` binary that does the analysis; hashed
      by content so an engine rebuild (tokenization/nway/normalization change)
      changes identity — the analog of the AAT generator's `adapter_binary_hash`.
    - dictionaries: {name: nix-store-path}; store paths are already content ids.
    - analyzers: selected analyzer specs incl. dict variants ('vibrato:unidic-…');
      sorted for order-independence.
    - warehouse_profile: 'full' | 'triage'.
    - schema_files: warehouse schema .sql files; hashed by content.
    - ortho_detect: e.g. 'historical'; None when the flag is not passed.
    - works_parquet: eligibility slice; hashed by content, None when not passed.

    Forward-risk: these keys cover exactly the `analyze-aat` flags the current
    warehouse recipes vary. Other output-determining flags (`--output-profile`,
    `--nway*`, `--orthographic-style`, `--ortho-ml-model`, `--parquet-zstd-level`)
    sit at fixed CLI defaults today and are therefore NOT in identity. If a recipe
    ever begins varying one, add it here — otherwise identity silently under-covers
    and a changed run would be served as fresh.
    """
    return {
        "aat_content_hash": aat_hash.hash_aat_dir(aat_dir),
        "engine_binary_hash": hashing.file_sha256(Path(engine_binary)),
        "dictionaries": {name: str(path) for name, path in dictionaries.items()},
        "analyzers": sorted(analyzers),
        "warehouse_profile": warehouse_profile,
        "ortho_detect": ortho_detect,
        "works_parquet_hash": (
            hashing.file_sha256(Path(works_parquet)) if works_parquet is not None else None
        ),
        # Content hashes only, sorted — schema files are identified by their bytes,
        # not their names, so two same-basename files in different dirs cannot
        # collide.
        "schema_version": sorted(hashing.file_sha256(Path(s)) for s in schema_files),
    }


def warehouse_input_set_hash(**kwargs: Any) -> str:
    """input_set_hash of build_identity_object(**kwargs)."""
    return run_identity.input_set_hash(build_identity_object(**kwargs))
