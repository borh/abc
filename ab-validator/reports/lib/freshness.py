"""Shared fresh/stale decision rule for the batch-run skip gates.

Both skip gates (AAT dumps in reports/aat-fidelity/generator_skip.py and
morph-warehouse runs in reports/morph-warehouse/warehouse_index.py) reuse the
same rule: an existing output is FRESH iff its recorded input-set hash equals the
current inputs' hash AND its outputs still re-hash to the recorded output hash. An
absent/unreadable output is never fresh (fail toward recompute). This module holds
ONLY that rule as a pure function; each caller keeps its own metadata loading and
maps the returned verdict to its own result vocabulary.
"""

from __future__ import annotations

import enum
from typing import Callable, NamedTuple


class Verdict(enum.Enum):
    FRESH = "fresh"
    INPUT_MISMATCH = "input_mismatch"
    OUTPUT_UNREADABLE = "output_unreadable"
    OUTPUT_MISMATCH = "output_mismatch"


class Decision(NamedTuple):
    verdict: Verdict
    recorded: str | None
    actual: str | None


def classify(
    recorded_input_hash: str | None,
    current_input_hash: str,
    recorded_output_hash: str | None,
    recompute_output: Callable[[], str],
) -> Decision:
    """Decide freshness. ``recompute_output`` is called only when the input
    hashes match; a ``ValueError``/``OSError`` from it means the outputs are
    unreadable/absent (not fresh). Returns the verdict plus the recorded and
    (when computed) actual output hashes for the caller to report."""
    if recorded_input_hash != current_input_hash:
        return Decision(Verdict.INPUT_MISMATCH, recorded_output_hash, None)
    try:
        actual = recompute_output()
    except (ValueError, OSError):
        return Decision(Verdict.OUTPUT_UNREADABLE, recorded_output_hash, None)
    if actual != recorded_output_hash:
        return Decision(Verdict.OUTPUT_MISMATCH, recorded_output_hash, actual)
    return Decision(Verdict.FRESH, recorded_output_hash, actual)
