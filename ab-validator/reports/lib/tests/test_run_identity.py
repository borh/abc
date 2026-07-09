"""Tests for the batch-run input-set identity hasher."""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

_LIB = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_LIB))

import run_identity  # noqa: E402


class InputSetHash(unittest.TestCase):
    def test_deterministic_and_prefixed(self) -> None:
        obj = {"a": 1, "corpus": "sha256:aa", "profile": "full"}
        h1 = run_identity.input_set_hash(obj)
        h2 = run_identity.input_set_hash(dict(obj))
        self.assertEqual(h1, h2)
        self.assertTrue(h1.startswith("sha256:"))
        self.assertEqual(len(h1), len("sha256:") + 64)

    def test_key_order_independent(self) -> None:
        a = run_identity.input_set_hash({"x": 1, "y": 2})
        b = run_identity.input_set_hash({"y": 2, "x": 1})
        self.assertEqual(a, b)

    def test_value_change_changes_hash(self) -> None:
        base = run_identity.input_set_hash({"corpus": "sha256:aa"})
        changed = run_identity.input_set_hash({"corpus": "sha256:bb"})
        self.assertNotEqual(base, changed)

    def test_omitting_an_input_changes_hash(self) -> None:
        # Narrowing identity must be detectable, not silent.
        full = run_identity.input_set_hash({"corpus": "sha256:aa", "dict": "sha256:bb"})
        narrowed = run_identity.input_set_hash({"corpus": "sha256:aa"})
        self.assertNotEqual(full, narrowed)

    def test_identity_version_is_folded_in(self) -> None:
        obj = {"corpus": "sha256:aa"}
        h = run_identity.input_set_hash(obj)
        # A caller-supplied identity_version must not override the module stamp.
        spoofed = run_identity.input_set_hash({**obj, "identity_version": "evil"})
        self.assertEqual(h, spoofed)

    def test_non_serializable_fails_closed(self) -> None:
        with self.assertRaises((TypeError, ValueError)):
            run_identity.input_set_hash({"bad": {1, 2, 3}})  # a set is not JSON

    def test_non_dict_fails_closed(self) -> None:
        with self.assertRaises(TypeError):
            run_identity.input_set_hash(["not", "a", "dict"])


if __name__ == "__main__":
    unittest.main()
