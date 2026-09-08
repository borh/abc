"""End-to-end tests for the warehouse_runner CLI (main()).

Exercises the gate path (arg parsing, identity, index, and a real
subprocess compute materializing the run dir), so no Rust batch is needed.
A marker file counts compute invocations, proving that a fresh hit skips
the command.
"""

from __future__ import annotations

import io
import json
import shutil
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

_PKG = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_PKG))

import warehouse_runner as wr  # noqa: E402
import warehouse_index as wix  # noqa: E402
import warehouse_identity as wi  # noqa: E402

_COMPUTE = (
    "import os,sys;"
    "d=sys.argv[1]; m=sys.argv[2];"
    "os.makedirs(d+'/analyses.parquet',exist_ok=True);"
    "open(d+'/runs.parquet','wb').write(b'runs-table');"
    "open(d+'/analyses.parquet/part-0.parquet','wb').write(b'part');"
    "open(m,'a').write('call\\n')"
)


class RunnerCLI(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = Path(tempfile.mkdtemp())
        self.addCleanup(lambda: shutil.rmtree(self.dir, ignore_errors=True))
        self.aat = self.dir / "aat"
        self.aat.mkdir()
        (self.aat / "000001_1-a.json").write_text('{"b":[1]}', encoding="utf-8")
        self.schema = self.dir / "schema.sql"
        self.schema.write_text("CREATE TABLE t(x);", encoding="utf-8")
        self.engine = self.dir / "ab-morph-run"
        self.engine.write_bytes(b"ENGINEv1")
        self.wh = self.dir / "warehouse"
        self.run_id = "cli-run"
        self.marker = self.dir / "compute-calls.log"
        self.run_dir = self.wh / "runs" / self.run_id

    def _argv(self, force: bool = False) -> list[str]:
        argv = [
            "--aat-dir",
            str(self.aat),
            "--engine-binary",
            str(self.engine),
            "--warehouse-dir",
            str(self.wh),
            "--run-id",
            self.run_id,
            "--warehouse-profile",
            "full",
            "--analyzer",
            "vibrato",
            "--analyzer",
            "sudachi-a",
            "--dict",
            "sudachi=/nix/store/aaa",
            "--schema-file",
            str(self.schema),
        ]
        if force:
            argv.append("--force")
        argv += ["--", sys.executable, "-c", _COMPUTE, str(self.run_dir), str(self.marker)]
        return argv

    def _run(self, force: bool = False) -> dict:
        buf = io.StringIO()
        with redirect_stdout(buf):
            rc = wr.main(self._argv(force=force))
        self.assertEqual(rc, 0)
        return json.loads(buf.getvalue())

    def _calls(self) -> int:
        return self.marker.read_text().count("call") if self.marker.exists() else 0

    def _expected_hash(self) -> str:
        return wi.warehouse_input_set_hash(
            aat_dir=self.aat,
            engine_binary=self.engine,
            dictionaries={"sudachi": "/nix/store/aaa"},
            analyzers=["vibrato", "sudachi-a"],
            warehouse_profile="full",
            schema_files=[self.schema],
        )

    def test_first_run_computes_records_and_matches_identity(self) -> None:
        res = self._run()
        self.assertEqual(res["action"], "computed")
        self.assertEqual(self._calls(), 1)
        self.assertEqual(res["input_set_hash"], self._expected_hash())
        self.assertEqual(wix.check(self.wh, self._expected_hash())["status"], "fresh")

    def test_second_run_skips_the_command(self) -> None:
        self._run()
        res = self._run()
        self.assertEqual(res["action"], "skip")
        self.assertEqual(self._calls(), 1)  # command NOT run the second time

    def test_force_reruns_the_command(self) -> None:
        self._run()
        res = self._run(force=True)
        self.assertEqual(res["action"], "computed")
        self.assertEqual(res["reason"], "forced")
        self.assertEqual(self._calls(), 2)

    def test_missing_command_after_dashdash_errors(self) -> None:
        with self.assertRaises(SystemExit):
            wr.main(
                [
                    "--aat-dir",
                    str(self.aat),
                    "--engine-binary",
                    str(self.engine),
                    "--warehouse-dir",
                    str(self.wh),
                    "--run-id",
                    self.run_id,
                    "--warehouse-profile",
                    "full",
                ]
            )

    def test_empty_command_after_dashdash_errors(self) -> None:
        with self.assertRaises(SystemExit):
            wr.main(
                [
                    "--aat-dir",
                    str(self.aat),
                    "--engine-binary",
                    str(self.engine),
                    "--warehouse-dir",
                    str(self.wh),
                    "--run-id",
                    self.run_id,
                    "--warehouse-profile",
                    "full",
                    "--",
                ]
            )

    def test_compute_failure_propagates_exit_code(self) -> None:
        argv = [
            "--aat-dir",
            str(self.aat),
            "--engine-binary",
            str(self.engine),
            "--warehouse-dir",
            str(self.wh),
            "--run-id",
            self.run_id,
            "--warehouse-profile",
            "full",
            "--schema-file",
            str(self.schema),
            "--",
            sys.executable,
            "-c",
            "import sys; sys.exit(3)",
        ]
        buf = io.StringIO()
        with redirect_stdout(buf):
            rc = wr.main(argv)
        self.assertEqual(rc, 3)  # child exit code preserved, not collapsed to 1
        self.assertFalse((self.wh / "by-input").exists())  # nothing recorded

    def test_bad_dict_pair_errors(self) -> None:
        with self.assertRaises(SystemExit):
            wr.main(
                [
                    "--aat-dir",
                    str(self.aat),
                    "--engine-binary",
                    str(self.engine),
                    "--warehouse-dir",
                    str(self.wh),
                    "--run-id",
                    self.run_id,
                    "--warehouse-profile",
                    "full",
                    "--dict",
                    "no-equals-sign",
                    "--",
                    "true",
                ]
            )


if __name__ == "__main__":
    unittest.main()
