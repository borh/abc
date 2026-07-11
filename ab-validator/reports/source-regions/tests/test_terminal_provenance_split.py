"""Tests for the terminal-provenance / colophon-metadata stateful split.

Two suites:

* `ClassifyTail` pins the exact stateful behavior of
  `reports.lib.terminal_provenance.classify_tail` -- the normative rule
  Task 14's Rust `source_note` emission transcribes case-for-case. The
  distinguishing case (a bare date line classifying differently depending
  on which head preceded it) is the whole reason this is a state machine
  and not a per-line predicate; see `test_same_shaped_line_...` below.
* `GeneratorCli` drives `terminal-provenance-split.py` end to end against
  small synthetic `cards/<id>/files/<n>_x.zip` corpora (mirroring the real
  aozorabunko layout) and pins its summary JSON shape, including the
  fail-closed `SystemExit(2)` + bounded-examples behavior on an
  unclassifiable residual.
"""

from __future__ import annotations

import importlib.util
import json
import sys
import unittest
import zipfile
from pathlib import Path

_LIB = Path(__file__).resolve().parents[2] / "lib"
sys.path.insert(0, str(_LIB))

import terminal_provenance as tp  # noqa: E402

_SCRIPT_DIR = Path(__file__).resolve().parents[1]
_SCRIPT_PATH = _SCRIPT_DIR / "terminal-provenance-split.py"
_spec = importlib.util.spec_from_file_location("terminal_provenance_split", _SCRIPT_PATH)
tps = importlib.util.module_from_spec(_spec)
sys.modules["terminal_provenance_split"] = tps
_spec.loader.exec_module(tps)


class ClassifyTail(unittest.TestCase):
    def test_provenance_head_line(self) -> None:
        out = tp.classify_tail(["底本：「日本文学全集1」集英社"])
        self.assertEqual(out, ["terminal_provenance"])

    def test_continuation_after_provenance_head_is_provenance(self) -> None:
        lines = [
            "底本：「日本文学全集1」集英社",
            "　　　1969（昭和44）年12月25日初版",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            ["terminal_provenance", "terminal_provenance"],
        )

    def test_same_shaped_line_after_colophon_head_is_colophon(self) -> None:
        """THE distinguishing case: the identically-shaped date line that was
        terminal_provenance in the previous test is colophon_metadata here,
        because it follows 入力： instead of 底本：. No per-line predicate can
        tell these apart -- only carried state can."""
        lines = [
            "入力：j.utiyama",
            "1998年7月28日公開",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            ["colophon_metadata", "colophon_metadata"],
        )

    def test_real_corpus_tail_end_to_end(self) -> None:
        """The real tail from cards/000005/files/5_ruby_21311.zip::aibiki.txt
        in the pinned aozorabunko corpus -- the exact motivating example."""
        lines = [
            "底本：「日本文学全集1　坪内逍遥・二葉亭四迷集」集英社",
            "　　　1969（昭和44）年12月25日初版",
            "入力：j.utiyama",
            "校正：八巻美恵",
            "1998年7月28日公開",
            "2006年1月6日修正",
            "青空文庫作成ファイル：",
            "このファイルは、インターネットの図書館、青空文庫で作られました。",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            [
                "terminal_provenance",
                "terminal_provenance",
                "colophon_metadata",
                "colophon_metadata",
                "colophon_metadata",
                "colophon_metadata",
                "colophon_metadata",
                "colophon_metadata",
            ],
        )

    def test_oyahon_continuation_is_provenance(self) -> None:
        lines = [
            "底本の親本：「新編 銀河鉄道の夜」新潮文庫",
            "　　　1989（平成元）年11月10日初版",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            ["terminal_provenance", "terminal_provenance"],
        )

    def test_all_named_colophon_heads(self) -> None:
        lines = [
            "入力：ある人",
            "校正：別の人",
            "青空文庫作成ファイル：",
            "※このファイルはインターネットの図書館、青空文庫で作られました。",
        ]
        self.assertEqual(tp.classify_tail(lines), ["colophon_metadata"] * 4)

    def test_blank_between_blocks_is_blank_and_preserves_state(self) -> None:
        lines = [
            "底本：「サンプル」出版社",
            "",
            "入力：誰か",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            ["terminal_provenance", "blank", "colophon_metadata"],
        )

    def test_blank_preserves_provenance_state_across_continuation(self) -> None:
        lines = [
            "底本：「サンプル」出版社",
            "",
            "　　　1999年1月1日初版",
        ]
        self.assertEqual(
            tp.classify_tail(lines),
            ["terminal_provenance", "blank", "terminal_provenance"],
        )

    def test_nonblank_before_any_head_is_unclassifiable(self) -> None:
        with self.assertRaises(tp.UnclassifiableTail) as ctx:
            tp.classify_tail(["何かの一行", "底本：「サンプル」出版社"])
        self.assertEqual(ctx.exception.line, "何かの一行")
        self.assertEqual(ctx.exception.index, 0)

    def test_find_tail_start_matches_leading_whitespace_stripped_line(self) -> None:
        lines = ["本文", "　底本：「サンプル」出版社"]
        self.assertEqual(tp.find_tail_start(lines), 1)

    def test_find_tail_start_none_when_absent(self) -> None:
        self.assertIsNone(tp.find_tail_start(["本文だけ", "もう一行"]))


def _make_zip_work(root: Path, card: str, file_prefix: str, text: str) -> None:
    files_dir = root / "cards" / card / "files"
    files_dir.mkdir(parents=True, exist_ok=True)
    zip_path = files_dir / f"{file_prefix}_ruby_1.zip"
    with zipfile.ZipFile(zip_path, "w") as zf:
        zf.writestr(f"{file_prefix}.txt", text.encode("shift_jis"))


class WorkIdFromIndexPath(unittest.TestCase):
    def test_zip_member_path(self) -> None:
        self.assertEqual(
            tps.work_id_from_index_path("cards/000148/files/799_ruby_19091.zip::799.txt"),
            "000148_799",
        )

    def test_bare_txt_path(self) -> None:
        self.assertEqual(
            tps.work_id_from_index_path("cards/000148/files/799_ruby_19091/test.txt"),
            "000148_799",
        )


class GeneratorCli(unittest.TestCase):
    def _run(self, corpus_root: Path, tmp_path: Path, jobs: int = 1) -> tuple[int, dict]:
        summary_json = tmp_path / "summary.json"
        report_md = tmp_path / "report.md"
        argv = [
            "terminal-provenance-split.py",
            "--corpus-root",
            str(corpus_root),
            "--summary-json",
            str(summary_json),
            "--report-md",
            str(report_md),
            "--jobs",
            str(jobs),
        ]
        old_argv = sys.argv
        sys.argv = argv
        try:
            exit_code = tps.main()
        finally:
            sys.argv = old_argv
        summary = (
            json.loads(summary_json.read_text(encoding="utf-8")) if summary_json.exists() else {}
        )
        return exit_code, summary

    def test_ok_corpus_reports_expected_counts(self) -> None:
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            _make_zip_work(
                root,
                "000001",
                "1",
                "本文一行目\n\n"
                "底本：「サンプル1」出版社\n"
                "　　　1999年1月1日初版\n"
                "入力：誰か\n"
                "校正：別の人\n"
                "1999年2月2日公開\n",
            )
            _make_zip_work(
                root,
                "000002",
                "2",
                "本文二行目\n\n底本：「サンプル2」出版社\n",
            )
            _make_zip_work(
                root,
                "000003",
                "3",
                "本文だけで底本行なし\n",
            )
            exit_code, summary = self._run(root, root / "out")
            self.assertEqual(exit_code, 0)
            self.assertEqual(summary["verdict"], "TERMINAL_PROVENANCE_SPLIT_OK")
            self.assertEqual(summary["works_scanned"], 3)
            self.assertEqual(summary["works_with_terminal_provenance"], 2)
            self.assertEqual(summary["works_without_tail"], 1)
            self.assertEqual(summary["works_with_colophon"], 1)
            self.assertEqual(summary["works_unclassifiable"], 0)
            # work 1: 2 provenance lines (head + continuation), 3 colophon
            # lines (入力/校正/date continuation).
            # work 2: 1 provenance line (head only).
            self.assertEqual(summary["terminal_provenance_lines"], 3)
            self.assertEqual(summary["colophon_lines"], 3)
            # head hits: work1's 底本：/入力：/校正： (3) + work2's 底本： (1).
            self.assertEqual(summary["head_line_hits"], 4)

    def test_unclassifiable_residual_exits_2_with_bounded_examples(self) -> None:
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            # `find_tail_start` always anchors the tail on a 底本： line, so
            # the tail's first line is always a recognized head -- a real
            # corpus work can never trip `UnclassifiableTail` on line 0 of
            # its own tail. `ClassifyTail.test_nonblank_before_any_head_...`
            # already pins the rule itself on a synthetic non-corpus tail;
            # here we monkeypatch `classify_tail` to force the residual path
            # and pin the GENERATOR's plumbing around it instead: fail-closed
            # exit code, verdict, and the <=20-examples bound.
            _make_zip_work(
                root,
                "000009",
                "9",
                "本文\n底本：「サンプル」出版社\n",
            )
            original = tps.classify_tail

            def boom(lines):
                # Must raise the SAME exception identity `process_entry`
                # catches (`tps.UnclassifiableTail`, bound via
                # `reports.lib.terminal_provenance`) -- not `tp
                # .UnclassifiableTail`, which this test file imports under a
                # different module name (`terminal_provenance`) and is thus
                # a distinct class object, even though same source file.
                raise tps.UnclassifiableTail(lines[0], 0)

            tps.classify_tail = boom
            try:
                exit_code, summary = self._run(root, root / "out")
            finally:
                tps.classify_tail = original
            self.assertEqual(exit_code, 2)
            self.assertEqual(
                summary["verdict"],
                "TERMINAL_PROVENANCE_SPLIT_UNCLASSIFIABLE_RESIDUAL",
            )
            self.assertEqual(summary["works_unclassifiable"], 1)
            self.assertLessEqual(len(summary["unclassifiable_examples"]), 20)
            self.assertEqual(summary["unclassifiable_examples"][0]["work_id"], "000009_9")


if __name__ == "__main__":
    unittest.main()
