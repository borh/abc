"""Content identity of an expensive batch run's input set.

`input_set_hash(identity_object)` reduces the full set of inputs that determine a
batch run's output to a single `sha256:<hex>`, so a run can be skipped when its
inputs are unchanged and detected as stale the instant any input changes.

Identity is content-based (never mtime) and fails toward correctness: any input
that can affect the output belongs in `identity_object`; omitting one silently
narrows identity and would serve a stale output as fresh.
"""

from __future__ import annotations

import hashlib
import json
from typing import Any

# Bump when the canonical form or the identity contract changes, so prior hashes
# invalidate cleanly.
INPUT_SET_IDENTITY_VERSION = "soranoha-run-identity-v1"


def canonical_json(value: Any) -> str:
    """Deterministic JSON: sorted keys, compact separators, UTF-8, no NaN.

    Raises on a non-JSON-serializable value (fail closed: a run must never get a
    silent partial identity)."""
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    )


def input_set_hash(identity_object: dict[str, Any]) -> str:
    """Return `sha256:<hex>` over the canonical JSON of `identity_object`, with the
    identity-format version folded in authoritatively (a caller cannot override
    `identity_version`)."""
    if not isinstance(identity_object, dict):
        raise TypeError("identity_object must be a dict")
    stamped = {**identity_object, "identity_version": INPUT_SET_IDENTITY_VERSION}
    payload = canonical_json(stamped).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()
