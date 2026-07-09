"""Characterization tests for AAT run-set resolution (fidelity Phase 1).

The run-set manifest is the SOLE authority for dump selection: a stale
`AB_*_AAT_DIR` in the ambient shell must NOT override the pinned value, even if
the adapter entry still carries a legacy `*_env` field. See
docs/superpowers/specs/2026-07-09-fidelity-run-idempotency-design.md (F1/F2).

Run: python3 -m unittest reports.lib.tests.test_aat_runs   (from ab-validator/)
  or: python3 reports/lib/tests/test_aat_runs.py
"""

from __future__ import annotations

import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import aat_runs  # noqa: E402

# The manifest deliberately carries a legacy `aat_dir_env` field so the test
# exercises the override code path directly: post-Phase-1 the resolver must
# ignore that field entirely rather than honour the named env var.
_MANIFEST = {
    "schema_version": 1,
    "run_set_id": "test",
    "adapters": {
        "aozora": {
            "aat_dir": "${AB_DB_ROOT}/aat-corpus/pinned/aat/aozora-adapter",
            "aat_dir_env": "AB_AOZORA_AAT_DIR",
            "run_descriptor": "${AB_DB_ROOT}/aat-corpus/pinned/metadata.json",
        },
    },
}

_EXPECTED = "/db/ab-validator/aat-corpus/pinned/aat/aozora-adapter"


class ManifestIsAuthoritative(unittest.TestCase):
    def setUp(self) -> None:
        self._saved = {k: os.environ.pop(k, None) for k in ("AB_AOZORA_AAT_DIR",)}
        os.environ.setdefault("AB_DB_ROOT", "/db/ab-validator")
        fd, name = tempfile.mkstemp(suffix=".json")
        os.close(fd)
        self.addCleanup(os.unlink, name)
        self._path = Path(name)
        self._path.write_text(json.dumps(_MANIFEST), encoding="utf-8")

    def tearDown(self) -> None:
        for k, v in self._saved.items():
            if v is None:
                os.environ.pop(k, None)
            else:
                os.environ[k] = v

    def _resolve(self) -> str:
        run_set = aat_runs.load_run_set(self._path)
        return aat_runs.adapter_aat_dirs(run_set)["aozora"]

    def test_clean_env_resolves_manifest_value(self) -> None:
        self.assertEqual(self._resolve(), _EXPECTED)

    def test_stale_env_does_not_override_manifest(self) -> None:
        # The F2 fix: a stale ambient AB_AOZORA_AAT_DIR must be ignored even
        # though the entry names it via aat_dir_env.
        os.environ["AB_AOZORA_AAT_DIR"] = "/tmp/STALE-WRONG-DUMP"
        self.assertEqual(self._resolve(), _EXPECTED)


if __name__ == "__main__":
    unittest.main()
