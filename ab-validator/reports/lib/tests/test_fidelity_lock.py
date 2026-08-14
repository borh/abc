"""Characterization tests for the fidelity lock (Phase 2, Move B).

The Move-B contract: replacing the compute stage's implicit `load_run_set()` +
`adapter_aat_globs()` with `resolve` -> lock -> `lock_aat_globs()` is a pure
substitution — for the same manifest the resolved globs are identical. Proving that
here means the coverage-report outputs are unchanged without re-walking /db.

Run: python3 reports/lib/tests/test_fidelity_lock.py   (from ab-validator/)
"""

from __future__ import annotations

import importlib.util
import json
import os
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import aat_hash  # noqa: E402
import aat_runs  # noqa: E402
import fidelity_lock  # noqa: E402

# resolve-run-set.py has hyphens; load it by path.
_RESOLVE_PATH = _LIB.parent / "aat-fidelity" / "resolve-run-set.py"
_spec = importlib.util.spec_from_file_location("resolve_run_set", _RESOLVE_PATH)
resolve_run_set = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(resolve_run_set)


class LockReader(unittest.TestCase):
    def _write(self, obj) -> Path:
        fd, name = tempfile.mkstemp(suffix=".json")
        os.close(fd)
        self.addCleanup(os.unlink, name)
        Path(name).write_text(json.dumps(obj), encoding="utf-8")
        return Path(name)

    def test_rejects_unknown_format(self) -> None:
        p = self._write({"lock_format": "nope", "run_set_id": "x", "adapters": {}})
        with self.assertRaises(ValueError):
            fidelity_lock.load_lock(p)

    def test_reads_dirs_globs_and_id(self) -> None:
        p = self._write(
            {
                "lock_format": "fidelity-lock/v1",
                "run_set_id": "rs",
                "adapters": {"a": {"aat_dir": "/x/a"}, "b": {"aat_dir": "/x/b"}},
            }
        )
        lock = fidelity_lock.load_lock(p)
        self.assertEqual(fidelity_lock.lock_aat_dirs(lock, order=["b"]), {"b": "/x/b"})
        self.assertEqual(fidelity_lock.lock_aat_globs(lock)["a"], "/x/a/*.json")
        self.assertEqual(fidelity_lock.lock_run_set_id(lock), "rs")

    def test_db_root_provenance(self) -> None:
        p = self._write(
            {
                "lock_format": "fidelity-lock/v1",
                "run_set_id": "rs",
                "db_root": "/mnt/bulk/ab-validator",
                "adapters": {"a": {"aat_dir": "/x/a"}},
            }
        )
        self.assertEqual(
            fidelity_lock.lock_db_root(fidelity_lock.load_lock(p)), "/mnt/bulk/ab-validator"
        )


class SubstitutionIsIdentity(unittest.TestCase):
    """resolve->lock globs == the old aat_runs globs, for the same manifest."""

    def setUp(self) -> None:
        self._saved = os.environ.pop("AB_AOZORA_AAT_DIR", None)
        self._tmp = tempfile.mkdtemp()
        self.addCleanup(lambda: __import__("shutil").rmtree(self._tmp, ignore_errors=True))
        # a real, non-empty AAT dir so resolve's verify_dirs gate passes
        aat = Path(self._tmp) / "aat" / "aozora-adapter"
        aat.mkdir(parents=True)
        (aat / "000001_1.json").write_text('{"blocks":[]}', encoding="utf-8")
        self._manifest = Path(self._tmp) / "run-set.json"
        self._manifest.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "run_set_id": "sub-test",
                    "adapters": {"aozora": {"aat_dir": str(aat)}},
                }
            ),
            encoding="utf-8",
        )

    def tearDown(self) -> None:
        if self._saved is not None:
            os.environ["AB_AOZORA_AAT_DIR"] = self._saved

    def test_lock_globs_equal_run_set_globs(self) -> None:
        run_set = aat_runs.load_run_set(self._manifest)
        old = aat_runs.adapter_aat_globs(run_set, order=["aozora"])
        lock = resolve_run_set.resolve_lock(run_set, repo_root=self._tmp, verify_dirs=True)
        new = fidelity_lock.lock_aat_globs(lock, order=["aozora"])
        self.assertEqual(old, new)


class ContentVerification(unittest.TestCase):
    """Move A: resolve verifies each dump against its pinned content_hash, fails closed."""

    def _fixture(self, pinned_hash) -> tuple:
        tmp = tempfile.mkdtemp()
        self.addCleanup(lambda: shutil.rmtree(tmp, ignore_errors=True))
        aat = Path(tmp) / "aat" / "aozora-adapter"
        aat.mkdir(parents=True)
        (aat / "000001_1.json").write_text('{"blocks":[]}', encoding="utf-8")
        expected = {} if pinned_hash is None else {"content_hash": pinned_hash}
        manifest = Path(tmp) / "run-set.json"
        manifest.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "run_set_id": "cv",
                    "adapters": {"aozora": {"aat_dir": str(aat), "expected": expected}},
                }
            ),
            encoding="utf-8",
        )
        return tmp, manifest, aat

    def test_verify_records_actual_hash_in_lock(self) -> None:
        tmp, manifest, aat = self._fixture(pinned_hash=None)  # unpinned: record, don't gate
        rs = aat_runs.load_run_set(manifest)
        lock = resolve_run_set.resolve_lock(
            rs, repo_root=tmp, verify_dirs=True, verify_content=True
        )
        self.assertEqual(lock["adapters"]["aozora"]["content_hash"], aat_hash.hash_aat_dir(aat))

    def test_fails_closed_on_hash_mismatch(self) -> None:
        tmp, manifest, _ = self._fixture(pinned_hash="sha256:deadbeef")
        rs = aat_runs.load_run_set(manifest)
        with self.assertRaises(ValueError):
            resolve_run_set.resolve_lock(rs, repo_root=tmp, verify_dirs=True, verify_content=True)

    def test_correct_pin_passes(self) -> None:
        tmp, manifest, aat = self._fixture(pinned_hash=None)
        good = aat_hash.hash_aat_dir(aat)
        json.loads(manifest.read_text())  # sanity
        manifest.write_text(
            json.dumps(
                {
                    "schema_version": 1,
                    "run_set_id": "cv",
                    "adapters": {
                        "aozora": {"aat_dir": str(aat), "expected": {"content_hash": good}}
                    },
                }
            ),
            encoding="utf-8",
        )
        rs = aat_runs.load_run_set(manifest)
        lock = resolve_run_set.resolve_lock(
            rs, repo_root=tmp, verify_dirs=True, verify_content=True
        )
        self.assertEqual(lock["adapters"]["aozora"]["content_hash"], good)


if __name__ == "__main__":
    unittest.main()
