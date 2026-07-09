"""abc-legacy-json-c14n-v0 canonical JSON -- single source for the ab-validator
reports tree.

PINNED: the returned bytes feed schema/document SHA-256 hashes that are compared
across adapters and against the abc side. Output MUST stay byte-for-byte stable;
changing it silently invalidates recorded fidelity and skip metadata. Guarded by
reports/lib/tests/test_legacy_json_c14n.py.

Algorithm:
1. serialize UTF-8 JSON with sorted object keys and compact separators;
2. escape every "/" as "\\/", including slashes inside string values.
(Callers SHA-256 the result and prefix with "sha256:".)
"""

from __future__ import annotations

import json


def canonical_json(value: object) -> str:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).replace("/", "\\/")
