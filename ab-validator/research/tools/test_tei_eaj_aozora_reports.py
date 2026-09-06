#!/usr/bin/env python
import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock


TOOL_DIR = pathlib.Path(__file__).resolve().parent


def load_reports():
    spec = importlib.util.spec_from_file_location(
        "tei_eaj_aozora_reports", TOOL_DIR / "tei_eaj_aozora_reports.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


reports = load_reports()


class DefaultAbcTeiDirsTest(unittest.TestCase):
    """default_abc_tei_dirs honors only explicit ABC_TEI_EAJ_ABC_TEI_DIRS
    values; the implicit target/soranoha/.../artifacts fallback that named the
    retired snapshot producer's place was removed and must not reappear."""

    def test_explicit_env_values_are_returned_in_order(self):
        with tempfile.TemporaryDirectory() as raw:
            cwd = pathlib.Path(raw)
            value = os.pathsep.join(["/first/tei", "", "/second/tei"])
            with mock.patch.dict(os.environ, {"ABC_TEI_EAJ_ABC_TEI_DIRS": value}, clear=False):
                self.assertEqual(
                    reports.default_abc_tei_dirs(cwd),
                    ["/first/tei", "/second/tei"],
                )

    def test_absent_env_yields_no_default_directory(self):
        with tempfile.TemporaryDirectory() as raw:
            cwd = pathlib.Path(raw)
            with mock.patch.dict(os.environ, {}, clear=True):
                self.assertEqual(reports.default_abc_tei_dirs(cwd), [])

    def test_coincidental_old_target_dir_is_not_picked_up(self):
        with tempfile.TemporaryDirectory() as raw:
            cwd = pathlib.Path(raw)
            legacy = cwd / "target" / "soranoha" / "full-corpus-publication-basic-ja" / "artifacts"
            legacy.mkdir(parents=True)
            with mock.patch.dict(os.environ, {}, clear=True):
                self.assertEqual(reports.default_abc_tei_dirs(cwd), [])


if __name__ == "__main__":
    unittest.main()
