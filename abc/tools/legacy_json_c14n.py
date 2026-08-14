"""abc-legacy-json-c14n-v0 canonical JSON -- single source for the abc tree.

PINNED: the returned text feeds SHA-256 content refs and schema hashes that are
compared against recorded identities on both sides of the monorepo. Output MUST
stay byte-for-byte stable; changing it silently invalidates recorded hashes.
The ab-validator tree carries its own copy (reports/lib/legacy_json_c14n.py)
because its flake checks stage that subtree alone; both are pinned to the same
golden vectors.

Algorithm:
1. serialize UTF-8 JSON with sorted object keys and compact separators,
   non-ASCII preserved (ensure_ascii=False);
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
