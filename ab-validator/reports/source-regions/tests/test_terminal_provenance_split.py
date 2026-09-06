"""Tests for the terminal-provenance / colophon-metadata stateful split.

Two suites:

* `ClassifyTail` pins the exact stateful behavior of
  `reports.lib.terminal_provenance.classify_tail` -- the normative rule
  Rust `source_note` emission transcribes case-for-case. The
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
import io
import json
import struct
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


def _in_memory_zip_bytes(member_name: str, text: str) -> bytes:
    """A single-entry, correctly-formed zip archive's raw bytes (Shift_JIS
    member content), built entirely in memory."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        zf.writestr(member_name, text.encode("shift_jis"))
    return buf.getvalue()


def _corrupt_central_directory_crc(data: bytes) -> bytes:
    """Flip the CRC-32 field in the (sole) central-directory file header,
    leaving the local file header's own CRC -- and all compressed bytes --
    untouched. Reproduces the exact real-corpus corruption pattern found in
    `cards/001393/files/50710_ruby_36965.zip`: python's `zipfile.read()`
    validates against the CENTRAL DIRECTORY's CRC (now wrong) and raises
    `BadZipFile`, even though the entry decompresses to the correct bytes
    (verifiable against the LOCAL header's still-correct CRC)."""
    cd_sig = b"PK\x01\x02"
    idx = data.find(cd_sig)
    assert idx != -1, "fixture must contain a central directory record"
    crc_offset = idx + 16  # sig(4) + version_made(2) + version_needed(2) + flag(2)
    # + method(2) + time(2) + date(2) = 16 bytes before the crc-32 field.
    original_crc = data[crc_offset : crc_offset + 4]
    flipped = bytes(b ^ 0xFF for b in original_crc)
    assert flipped != original_crc
    return data[:crc_offset] + flipped + data[crc_offset + 4 :]


def _corrupt_eocd_signature(data: bytes) -> bytes:
    """Corrupt the End-Of-Central-Directory signature so `zipfile.ZipFile()`
    cannot locate (and thus cannot open) the central directory AT ALL --
    while the local file header and compressed data at the front of the
    archive remain fully intact. Reproduces
    `cards/001505/files/58100_txt_60357.zip`, where the real corpus's
    central directory/EOCD region is unparseable as a coherent whole but
    the wanted member's own local header is untouched."""
    eocd_sig = b"PK\x05\x06"
    idx = data.rfind(eocd_sig)
    assert idx != -1, "fixture must contain an EOCD record"
    return data[:idx] + b"\x00\x00\x00\x00" + data[idx + 4 :]


def _truncate_mid_stream(data: bytes) -> bytes:
    """Cut a valid zip off partway through its compressed data (destroying
    the central directory/EOCD entirely, same as truncation would). A
    local-header scan finds the (now genuinely incomplete) compressed
    stream but MUST fail to decompress it -- this is the stray/corrupt
    duplicate-file pattern (`cards/001030/files/4812_ruby_14383.txt` /
    `cards/000119/files/46429_ruby_26539.txt`), which the Rust corpus
    pipeline also excludes (`invalid Zip archive: No CDFH found`), and must
    stay excluded (not silently recovered) after this fix.

    Cuts at 2/3 of the LOCAL FILE HEADER's own declared `compress_size` span
    (not 2/3 of the whole file): a highly-compressible fixture payload can
    otherwise compress down so small that even a 2/3 whole-file truncation
    still leaves the complete local header + complete compressed payload
    intact (only the trailing central directory/EOCD gets cut) -- that is
    the *recoverable* Class C pattern, not this genuinely-truncated one."""
    header_end = data.find(tps.ZIP_LOCAL_HEADER_SIGNATURE) + 30
    fnlen = struct.unpack("<H", data[header_end - 4 : header_end - 2])[0]
    extralen = struct.unpack("<H", data[header_end - 2 : header_end])[0]
    csize = struct.unpack("<I", data[header_end - 30 + 18 : header_end - 30 + 22])[0]
    data_start = header_end + fnlen + extralen
    cut = data_start + (csize * 2 // 3)
    assert cut < data_start + csize, "fixture payload too small to truncate mid-stream"
    return data[:cut]


class ReadEntryTextZipDispatch(unittest.TestCase):
    """Direct tests of `read_entry_text`'s zip-vs-plain dispatch and its
    tolerance for the two real-corpus corruption patterns (corruption class
    A/B stray-file misread and Class C unreadable-to-python entries) --
    see `reports/source-regions/terminal-provenance-split.py`'s
    `read_entry_text`/`_read_zip_entry` docstrings for the full root-cause
    account."""

    def _write(self, tmp_path: Path, name: str, data: bytes) -> Path:
        path = tmp_path / name
        path.write_bytes(data)
        return path

    def test_txt_named_file_that_is_actually_a_valid_zip_is_read_as_zip(self) -> None:
        """Selection rule: dispatch must be by CONTENT (zip magic bytes),
        not by filename extension -- mirrors Rust's `is_zip_file` in
        `crates/ab-index/src/index.rs`. A `.txt`-named file holding a fully
        valid zip archive (corruption class B:
        `cards/001341/files/49658_ruby_70064.txt` /
        `cards/001585/files/53484_ruby_56576.txt`) must be unzipped and its
        REAL text returned, not the raw zip bytes mis-decoded as Shift_JIS
        garbage."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            data = _in_memory_zip_bytes("hiyori_geta.txt", "本文\n底本：「実物」出版社\n")
            entry = self._write(root, "49658_ruby_70064.txt", data)
            result = tps.read_entry_text(root, entry)
            self.assertIsNotNone(result)
            label, text = result
            self.assertEqual(label, "49658_ruby_70064.txt::hiyori_geta.txt")
            self.assertIn("底本：「実物」出版社", text)

    def test_txt_named_truncated_zip_is_excluded_not_misread(self) -> None:
        """Class A: a `.txt`-named stray/corrupt duplicate whose zip data is
        genuinely truncated (Rust: `invalid Zip archive: No CDFH found`,
        excluded entirely from `index.json`'s 17886-entry universe) must be
        EXCLUDED (`None`) here too -- never silently decoded as raw-byte
        Shift_JIS "text" (which is how this bug produced a phantom
        `no_tail` entry for `cards/001030/files/4812_ruby_14383.txt` /
        `cards/000119/files/46429_ruby_26539.txt`)."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            valid = _in_memory_zip_bytes("utukusii_mura.txt", "本文" * 4000)
            truncated = _truncate_mid_stream(valid)
            entry = self._write(root, "4812_ruby_14383.txt", truncated)
            self.assertIsNone(tps.read_entry_text(root, entry))

    def test_zip_with_bad_central_directory_crc_is_recovered_via_local_header(
        self,
    ) -> None:
        """Class B/C (`50710_ruby_36965.zip`): the central directory's CRC
        disagrees with the entry's own local header, so `zipfile.read()`
        raises `BadZipFile`. The Rust `zip` crate's `by_index_raw` +
        manual decompress (crates/ab-index/src/index.rs,
        crates/ab-check/src/check.rs) never validates this CRC and reads
        the entry fine; `read_entry_text` must match that behavior via the
        local-header-trusting fallback, recovering the correct text."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            data = _in_memory_zip_bytes("alice.txt", "本文\n底本：「本物」文庫\n")
            corrupted = _corrupt_central_directory_crc(data)
            entry = self._write(root, "50710_ruby_36965.zip", corrupted)
            # Sanity: this fixture really does trip stdlib's CRC check.
            with self.assertRaises(zipfile.BadZipFile):
                with zipfile.ZipFile(entry) as zf:
                    zf.read("alice.txt")
            result = tps.read_entry_text(root, entry)
            self.assertIsNotNone(result)
            label, text = result
            self.assertEqual(label, "50710_ruby_36965.zip::alice.txt")
            self.assertIn("底本：「本物」文庫", text)

    def test_zip_with_unparseable_central_directory_is_recovered_via_local_header_scan(
        self,
    ) -> None:
        """Class C (`58100_txt_60357.zip`): the EOCD/central directory
        cannot be parsed as a coherent whole at all, so
        `zipfile.ZipFile()` itself raises `BadZipFile` before any `ZipInfo`
        exists. `read_entry_text` must fall back to scanning the file for
        local file headers directly and recover the wanted member -- the
        SAME thing the Rust corpus pipeline actually reads for this file
        (correctly finding no `底本：` tail on the real corpus entry)."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            data = _in_memory_zip_bytes("chobihige_samuel_no_hanashi.txt", "本文のみ、底本行なし\n")
            corrupted = _corrupt_eocd_signature(data)
            with self.assertRaises(zipfile.BadZipFile):
                zipfile.ZipFile(io.BytesIO(corrupted))
            entry = self._write(root, "58100_txt_60357.zip", corrupted)
            result = tps.read_entry_text(root, entry)
            self.assertIsNotNone(result)
            label, text = result
            self.assertEqual(label, "58100_txt_60357.zip::chobihige_samuel_no_hanashi.txt")
            self.assertIn("本文のみ、底本行なし", text)

    def test_genuinely_corrupt_zip_with_no_recoverable_local_header_stays_excluded(
        self,
    ) -> None:
        """Regression guard: a `.zip`-named file that is not zip data at all
        (matches neither the CRC-mismatch nor the unparseable-CD-but-
        intact-local-header recovery path) must stay excluded, exactly
        like the real corpus's two files that are unreadable in BOTH
        readers (`cards/001154/files/chihobunkano_shinkensetsu.zip`, empty;
        `cards/001562/files/56151_ruby_60063.zip`, not a zip at all) --
        the fallback must never fabricate content."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            entry = self._write(root, "56151_ruby_60063.zip", b"\x8c\x95\x93\xef\x8f\x97" * 200)
            self.assertIsNone(tps.read_entry_text(root, entry))
            empty_entry = self._write(root, "chihobunkano_shinkensetsu.zip", b"")
            self.assertIsNone(tps.read_entry_text(root, empty_entry))

    def test_bare_non_zip_txt_file_is_still_read_as_plain_text(self) -> None:
        """Regression guard: an ordinary bare `.txt` sibling (no zip magic
        bytes at all) must still be read as plain Shift_JIS text, unchanged
        from before this fix."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            entry = self._write(
                root, "3_plain.txt", "本文\n底本：「サンプル」出版社\n".encode("shift_jis")
            )
            result = tps.read_entry_text(root, entry)
            self.assertIsNotNone(result)
            label, text = result
            self.assertEqual(label, "3_plain.txt")
            self.assertIn("底本：「サンプル」出版社", text)


class GeneratorClipEndToEndZipDispatch(unittest.TestCase):
    """End-to-end proof that the dispatch fix changes the FULL PIPELINE's
    classification, not just the unit-level reader: a `.txt`-named work
    whose real content is a valid zip with a genuine `底本：` tail must be
    counted under `works_with_terminal_provenance`, not
    `works_without_tail` (the exact Class B misclassification the corpus audit
    traced on the real corpus)."""

    def _run(self, corpus_root: Path, tmp_path: Path) -> tuple[int, dict]:
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
            "1",
        ]
        old_argv = sys.argv
        sys.argv = argv
        try:
            exit_code = tps.main()
        finally:
            sys.argv = old_argv
        summary = json.loads(summary_json.read_text(encoding="utf-8"))
        return exit_code, summary

    def test_txt_named_zip_shaped_work_counts_as_terminal_provenance(self) -> None:
        import tempfile

        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            files_dir = root / "cards" / "001341" / "files"
            files_dir.mkdir(parents=True, exist_ok=True)
            data = _in_memory_zip_bytes("hiyori_geta.txt", "本文\n底本：「日和下駄」出版社\n")
            (files_dir / "49658_ruby_70064.txt").write_bytes(data)
            exit_code, summary = self._run(root, root / "out")
            self.assertEqual(exit_code, 0)
            self.assertEqual(summary["works_scanned"], 1)
            self.assertEqual(summary["works_with_terminal_provenance"], 1)
            self.assertEqual(summary["works_without_tail"], 0)


if __name__ == "__main__":
    unittest.main()
