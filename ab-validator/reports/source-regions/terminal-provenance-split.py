#!/usr/bin/env python
"""Terminal-provenance / colophon-metadata measurement split (Task 12 instrument).

ABC policy v0.2.0 admits `terminal_provenance` and `colophon_metadata` as
source-region classes but flags both `needs_measurement_split`
(`reports/lib/source_region.py`). This generator applies the stateful
boundary rule in `reports/lib/terminal_provenance.py` (the NORMATIVE rule
-- Task 14's Rust `source_note` emission transcribes it case-for-case) to
every work in the corpus and reports how many tail lines fall on each
side of the 底本：/入力： boundary.

Corpus layout + iteration mirrors `reports/aat-fidelity/denominator-
attribution.py`'s `iter_work_texts`: the pinned `aozorabunko` corpus mirror
is a website snapshot, not a flat directory of plaintext -- each work's
Shift_JIS text lives inside a per-work zip under `cards/<id>/files/*.zip`
(plus a handful of bare `*.txt` siblings). `work_id_from_index_path`
mirrors `work_id_from_index_path` in `crates/ab-index/src/index.rs`; it
labels samples/residuals for humans but is NOT a dedup key -- ab-index's
own reference index has 236 ids shared by 2-3 entries each (ruby vs
non-ruby zip variants of the same literary work), so `works_scanned` and
friends count corpus TEXT ENTRIES (one per zip member / bare txt file),
the same unit as every other `works_scanned`/`files_scanned` figure in
this codebase (e.g. the pinned corpus's 17886-entry count).

Usage:
  terminal-provenance-split.py --corpus-root DIR --summary-json PATH \
      --report-md PATH [--jobs N]

Fail-closed: any tail line the state machine cannot classify (a non-blank
line reached before any state-setting head) is a residual. A non-empty
residual FAILS the run: the summary/report are still written (with
verdict `TERMINAL_PROVENANCE_SPLIT_UNCLASSIFIABLE_RESIDUAL`) for
post-mortem, up to 20 examples are printed to stderr, and the process
exits 2.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import pathlib
import struct
import sys
import zipfile
import zlib
from typing import Any, Callable

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from reports.lib.io import write_json
from reports.lib.terminal_provenance import (
    BOUNDARY_RULE,
    COLOPHON_METADATA_CLASS,
    TERMINAL_PROVENANCE_CLASS,
    UnclassifiableTail,
    classify_tail,
    find_tail_start,
    is_colophon_head,
    is_provenance_head,
)

SCHEMA_VERSION = "terminal-provenance-colophon-split-v1"
VERDICT_OK = "TERMINAL_PROVENANCE_SPLIT_OK"
VERDICT_FAIL = "TERMINAL_PROVENANCE_SPLIT_UNCLASSIFIABLE_RESIDUAL"
MAX_EXAMPLES = 20
MAX_SAMPLES = 10

# The 2026-07-06 source-authority representability run
# (docs/superpowers/reports/2026-07-04-source-authority-representability.md)
# recorded these MARKER OCCURRENCE counts from the ab-source-inventory Rust
# scanner's source_region_coverage block. This instrument measures a
# different unit (works, and tail lines under the stateful rule); the
# report reconciles the two.
REFERENCE_COVERAGE_DATE = "2026-07-06"
REFERENCE_TERMINAL_PROVENANCE_OCCURRENCES = 609
REFERENCE_COLOPHON_OCCURRENCES = 89416

# The Task 12 brief's head-marker skeleton (`PROVENANCE_HEADS`/`COLOPHON_HEADS`
# in reports/lib/terminal_provenance.py) is extended ONLY by corpus-scan
# residuals -- a non-empty `unclassifiable_examples` bucket after a
# full-corpus run means the skeleton is insufficient and must gain an entry,
# documented here with examples and work IDs. This list starts empty because
# the brief's skeleton, unmodified, reached a defined state before any
# non-blank tail line in every one of the 17886 pinned-corpus text entries
# (works_unclassifiable == 0 on the 2026-07-12 hinoki run) -- see the report's
# Rule Extensions section.
RULE_EXTENSIONS: list[dict[str, Any]] = []


def is_text_entry(name: str) -> bool:
    return (
        not name.startswith("__MACOSX/")
        and not name.endswith("/")
        and name.lower().endswith(".txt")
        and pathlib.Path(name).name.lower() != "readme.txt"
    )


def work_id_from_index_path(path: str) -> str:
    """Mirror `work_id_from_index_path` in `crates/ab-index/src/index.rs`.

    `cards/<card>/files/<n>_ruby_xxxx.zip::<entry>` -> `<card>_<n>`.
    Falls back to a sanitized file stem for paths outside `cards/*/files/`.
    """
    source_path = path.split("::", 1)[0]
    parts = source_path.split("/")
    if "cards" in parts:
        cards_pos = parts.index("cards")
        if len(parts) > cards_pos + 3 and parts[cards_pos + 2] == "files":
            card = parts[cards_pos + 1]
            file_dir = parts[cards_pos + 3]
            file_name = file_dir
            for suffix in (".zip", ".txt"):
                if file_name.endswith(suffix):
                    file_name = file_name[: -len(suffix)]
                    break
            file = file_name.split("_", 1)[0]
            return f"{card}_{file}"
    stem = pathlib.Path(source_path).stem or "unknown"
    return "".join(ch if (ch.isalnum() or ch in "-_") else "_" for ch in stem)


def discover_entries(corpus_root: pathlib.Path) -> list[pathlib.Path]:
    """Yield every `cards/*/files/*.zip` and bare `*.txt` sibling, sorted.

    Selection rule (Task 15's C4-gate correction): every `.zip`/`.txt` FILE
    CANDIDATE in a `cards/<id>/files/` directory is discovered here -- one
    candidate per file, same as Rust's `collect_source_files` in
    `crates/ab-index/src/index.rs` (which walks the identical `cards/*/
    files/*` layout). A card directory legitimately holding more than one
    candidate (a ruby and a non-ruby zip edition of the same work, or a
    stray/corrupt duplicate file alongside the real entry) is NOT
    deduplicated at discovery time in either implementation -- the
    dedup/exclusion happens downstream, at READ time: `read_entry_text`
    (below) decides zip-vs-plain by CONTENT (magic bytes), not extension,
    and a candidate that sniffs/declares itself a zip but cannot actually
    be opened/decompressed is EXCLUDED (`None`), exactly mirroring Rust's
    `push_zip_sources` skipping (and warning on) an unreadable zip rather
    than ever falling back to reading it as plain text. This is what makes
    the final scored-entry set match the Rust corpus run's 17886-entry
    `index.json` universe card-for-card, without discovery itself needing
    to pick a "winner" among a directory's candidates."""
    entries: list[pathlib.Path] = []
    for files_dir in sorted(corpus_root.glob("cards/*/files")):
        if not files_dir.is_dir():
            continue
        for path in sorted(files_dir.iterdir()):
            if not path.is_file():
                continue
            name_lower = path.name.lower()
            if name_lower.endswith(".zip") or (
                name_lower.endswith(".txt") and name_lower != "readme.txt"
            ):
                entries.append(path)
    return entries


# --- Zip-vs-plain entry selection/reading (Task 15's 6-entry correction) --
#
# Task 15's C4-gate confinement audit traced the split scanner's 6-entry
# disagreement with the Rust corpus pipeline to a single root cause: this
# reader used to dispatch zip-vs-plain by FILENAME EXTENSION (`.zip` only),
# never by content. The pinned aozorabunko corpus contains files named
# `*.txt` whose actual bytes are zip archives (a stray/corrupt duplicate
# alongside the real entry in the same `cards/<id>/files/` directory, or --
# in two cases -- the ONLY file for that work id, wrongly named `.txt`).
# Extension-only dispatch read those files' raw zip bytes as if they were
# Shift_JIS text, producing phantom `no_tail` misclassifications instead of
# either the genuine tail inside the zip or (for the two truly-corrupt
# stray duplicates) correct exclusion.
#
# The selection rule now mirrors `is_zip_file` in
# `crates/ab-index/src/index.rs` exactly: a file is treated as a zip
# archive when its content starts with the zip local-file-header magic
# (`PK\x03\x04`), REGARDLESS of its extension; a `.zip`-extension file is
# also always treated as a zip (matching Rust's unconditional
# `push_zip_sources` for `*.zip`, magic bytes or not). A file that sniffs
# as zip-shaped but cannot be opened/decompressed AT ALL is EXCLUDED
# (returns `None`, same as `unreadable_entries`) -- exactly like Rust's
# `collect_source_files` logging "skipping unreadable zip" and omitting the
# file from `index.json` entirely. This is what makes the scanner's final
# scored-entry set match the Rust corpus run's 17886-entry universe
# card-for-card, without needing to change what `discover_entries` walks.
#
# A second, independent defect lives inside the zip-reading itself. Both
# `ab-index`'s and `ab-check`'s `read_zip_entry_bytes` (crates/ab-index/src
# /index.rs, crates/ab-check/src/check.rs -- the actual dump-producing
# read path) fetch each entry via `ZipArchive::by_index_raw` and manually
# `read_to_end` + inflate: this NEVER cross-checks a central-directory-
# sourced CRC-32 or uncompressed-size against the entry's own local file
# header. Python's stdlib `zipfile.read()` does perform that check, and
# the pinned corpus contains (at least) two entries where the central
# directory disagrees with the local header:
#   - `cards/001393/files/50710_ruby_36965.zip`: central directory's CRC/
#     uncompressed-size fields for the one text member are simply wrong;
#     the LOCAL header's fields are correct and the data decompresses
#     cleanly under them.
#   - `cards/001505/files/58100_txt_60357.zip`: the central directory/EOCD
#     is not a coherent whole at all (`zipfile.ZipFile()` itself raises
#     `BadZipFile` before any `ZipInfo` exists), while the wanted member's
#     own local file header (at the very front of this 26MB archive) is
#     completely intact.
# `_read_zip_entry` tries the fully-validated stdlib path first (fast,
# correct for 17880-ish of the ~17886 entries) and falls back to
# `_read_zip_member_bypassing_central_directory` -- which trusts ONLY the
# wanted member's own local file header, never the central directory --
# exactly reproducing the Rust read path's leniency. This same fallback
# also naturally EXCLUDES the two genuinely-truncated stray duplicate
# files above: their local header is intact but the compressed data it
# points at is truncated mid-stream, so decompression fails and `None` is
# returned, matching Rust's own exclusion of those exact two files.

ZIP_LOCAL_HEADER_SIGNATURE = b"PK\x03\x04"
# signature(4) + version(2) + flag(2) + method(2) + mtime(2) + mdate(2)
# + crc32(4) + compress_size(4) + uncompressed_size(4) + name_len(2)
# + extra_len(2) = 30 bytes, per the PKZIP APPNOTE local file header layout.
_LOCAL_HEADER_STRUCT = "<IHHHHHIIIHH"
_LOCAL_HEADER_SIZE = struct.calcsize(_LOCAL_HEADER_STRUCT)
_STORED = 0
_DEFLATED = 8


def sniffs_as_zip(entry: pathlib.Path) -> bool:
    """Content-sniff for the zip local-file-header magic, mirroring Rust's
    `is_zip_file` in `crates/ab-index/src/index.rs` (peeks the first 4
    bytes rather than trusting the filename extension)."""
    try:
        with entry.open("rb") as fh:
            return fh.read(4) == ZIP_LOCAL_HEADER_SIGNATURE
    except OSError:
        return False


def _parse_local_header(data: bytes, offset: int) -> tuple[str, int, int, int, int] | None:
    """Parse one local file header directly from raw bytes at `offset`.

    Returns `(name, method, compress_size, uncompressed_size, data_offset)`,
    or `None` if `offset` is not a valid, in-bounds local-header position
    (including a streamed entry using a trailing data descriptor -- general
    purpose bit 3 -- whose sizes are not known up front and which this
    fallback does not attempt to support)."""
    if data[offset : offset + 4] != ZIP_LOCAL_HEADER_SIGNATURE:
        return None
    if offset + _LOCAL_HEADER_SIZE > len(data):
        return None
    (_sig, _ver, flag, method, _mtime, _mdate, _crc, csize, usize, fnlen, extralen) = struct.unpack(
        _LOCAL_HEADER_STRUCT, data[offset : offset + _LOCAL_HEADER_SIZE]
    )
    if flag & 0x8:
        return None
    name_start = offset + _LOCAL_HEADER_SIZE
    name_end = name_start + fnlen
    data_offset = name_end + extralen
    if data_offset > len(data):
        return None
    name = data[name_start:name_end].decode("cp437", errors="replace")
    return name, method, csize, usize, data_offset


def _decompress_local_entry(
    data: bytes, method: int, compress_size: int, uncompressed_size: int, data_offset: int
) -> bytes | None:
    """Decompress one entry using ONLY its own local-header-declared method
    and sizes -- never a central-directory CRC or size. Returns `None` on
    any failure (short read, corrupt/truncated deflate stream, unsupported
    method), mirroring Rust's `bail!` on an unsupported compression method
    in `read_zip_entry_bytes` -- never a guess."""
    compressed = data[data_offset : data_offset + compress_size]
    if len(compressed) < compress_size:
        return None
    if method == _STORED:
        return compressed[:uncompressed_size]
    if method == _DEFLATED:
        try:
            return zlib.decompressobj(-15).decompress(compressed, uncompressed_size)
        except zlib.error:
            return None
    return None


def _read_zip_member_bypassing_central_directory(
    path: pathlib.Path, want: Callable[[str], bool]
) -> tuple[str, bytes] | None:
    """Recover a member's bytes by scanning the file for local file headers
    directly, trusting ONLY each local header's own fields -- never the
    central directory or EOCD. Fallback of last resort; see the module-
    level comment above this function's block for the two real-corpus
    corruption patterns this recovers, and why the same logic correctly
    keeps a genuinely-truncated stray duplicate excluded (its local header
    parses, but the compressed data it points at is incomplete, so
    decompression fails and `None` propagates).

    Finds the FIRST local header (file-physical order, matching the
    "first matching text entry wins" convention `read_entry_text` and
    Rust's `zip_text_entries` already use) whose name satisfies `want`."""
    try:
        data = path.read_bytes()
    except OSError:
        return None
    offset = 0
    while True:
        offset = data.find(ZIP_LOCAL_HEADER_SIGNATURE, offset)
        if offset == -1:
            return None
        parsed = _parse_local_header(data, offset)
        if parsed is not None:
            name, method, csize, usize, data_offset = parsed
            if want(name):
                decompressed = _decompress_local_entry(data, method, csize, usize, data_offset)
                if decompressed is not None:
                    return name, decompressed
        offset += 4


def _read_zip_entry(entry: pathlib.Path) -> tuple[str, bytes] | None:
    """Read the first text member from a zip-shaped file, tolerating the
    two central-directory corruption patterns described above the way the
    Rust `zip` crate's `by_index_raw` + manual decompress already does.
    Tries the fully-validated stdlib path first; the raw local-header scan
    is only a fallback of last resort, so the overwhelming majority of
    (well-formed) corpus entries are entirely unaffected by it."""
    try:
        with zipfile.ZipFile(entry) as zf:
            member = next((n for n in zf.namelist() if is_text_entry(n)), None)
            if member is None:
                return None
            try:
                return member, zf.read(member)
            except (zipfile.BadZipFile, OSError):
                pass  # central-directory CRC/size disagrees with the local
                # header -- fall through to the local-header-trusting scan.
    except zipfile.BadZipFile:
        pass  # EOCD/central directory not parseable as a coherent whole at
        # all -- fall through to the local-header-trusting scan.
    return _read_zip_member_bypassing_central_directory(entry, is_text_entry)


def read_entry_text(corpus_root: pathlib.Path, entry: pathlib.Path) -> tuple[str, str] | None:
    """Return `(label, text)` for a corpus entry, or `None` if unreadable."""
    name_lower = entry.name.lower()
    rel = entry.relative_to(corpus_root)
    if name_lower.endswith(".zip") or sniffs_as_zip(entry):
        result = _read_zip_entry(entry)
        if result is None:
            return None
        member, data = result
        return f"{rel}::{member}", data.decode("shift_jis", errors="replace")
    return str(rel), entry.read_bytes().decode("shift_jis", errors="replace")


def process_entry(corpus_root_str: str, entry_str: str) -> dict[str, Any]:
    """Classify one corpus entry's tail. Runs in a worker process/thread."""
    corpus_root = pathlib.Path(corpus_root_str)
    entry = pathlib.Path(entry_str)
    result = read_entry_text(corpus_root, entry)
    if result is None:
        return {"status": "unreadable", "entry": str(entry.relative_to(corpus_root))}
    label, text = result
    work_id = work_id_from_index_path(label)
    # str.splitlines() treats \n, \r\n, and bare \r uniformly as boundaries
    # (matching the effect of the Rust pipeline's sanitize step, which
    # normalizes all three to \n before aozora_body_range ever runs) --
    # see crates/ab-aozora-aat/src/lib.rs line 229 and its `sanitize`
    # normalization comments.
    lines = text.splitlines()
    tail_start = find_tail_start(lines)
    if tail_start is None:
        return {"status": "no_tail", "work_id": work_id, "label": label}
    tail_lines = lines[tail_start:]
    try:
        classes = classify_tail(tail_lines)
    except UnclassifiableTail as exc:
        return {
            "status": "unclassifiable",
            "work_id": work_id,
            "label": label,
            "line": exc.line,
            "tail_relative_index": exc.index,
            "absolute_line_number": tail_start + exc.index + 1,
        }
    tp_lines = classes.count(TERMINAL_PROVENANCE_CLASS)
    colophon_lines = classes.count(COLOPHON_METADATA_CLASS)
    # Head-line vs. inherited-continuation-line split, for the reference
    # cross-check only (not part of the normative classification): the
    # 2026-07-06 reference's `colophon_metadata_occurrences` comes from a
    # per-LINE prefix scan (`is_colophon_metadata_line` in
    # crates/ab-coverage/src/bin/source_inventory.rs) that recognizes only
    # explicit head-shaped lines -- it has no notion of state, so it never
    # counts a continuation line like a bare date. Counting head hits here
    # the same way makes the two numbers comparable.
    head_hits = sum(
        1
        for line in tail_lines
        if is_provenance_head(line.strip()) or is_colophon_head(line.strip())
    )
    return {
        "status": "ok",
        "work_id": work_id,
        "label": label,
        "terminal_provenance_lines": tp_lines,
        "colophon_lines": colophon_lines,
        "head_line_hits": head_hits,
        "has_colophon": colophon_lines > 0,
    }


def run_entries(
    corpus_root: pathlib.Path, entries: list[pathlib.Path], jobs: int
) -> list[dict[str, Any]]:
    if jobs <= 1:
        return [process_entry(str(corpus_root), str(entry)) for entry in entries]
    with concurrent.futures.ProcessPoolExecutor(max_workers=jobs) as pool:
        futures = [pool.submit(process_entry, str(corpus_root), str(entry)) for entry in entries]
        return [future.result() for future in futures]


def build_summary(corpus_root: pathlib.Path, jobs: int) -> dict[str, Any]:
    entries = discover_entries(corpus_root)
    if not entries:
        raise SystemExit(f"no corpus entries found under {corpus_root}")

    results = run_entries(corpus_root, entries, jobs)

    works_scanned = 0
    works_without_tail = 0
    works_with_terminal_provenance = 0
    works_with_colophon = 0
    works_unclassifiable = 0
    terminal_provenance_lines = 0
    colophon_lines = 0
    head_line_hits = 0
    unreadable: list[str] = []
    unclassifiable: list[dict[str, Any]] = []
    provenance_samples: list[dict[str, Any]] = []
    colophon_samples: list[dict[str, Any]] = []

    for result in results:
        status = result["status"]
        if status == "unreadable":
            unreadable.append(result["entry"])
            continue
        work_id = result["work_id"]
        # NOT deduped by work_id: `work_id_from_index_path` (mirroring
        # crates/ab-index/src/index.rs) is not unique per corpus text --
        # ab-index's own reference index has 17886 entries but only 17605
        # unique ids, because a single literary work commonly has BOTH a
        # ruby and a non-ruby zip variant sharing one id. Each is an
        # independent source text with its own (possibly differently
        # classified) tail, so each is scanned and counted on its own; this
        # also keeps `works_scanned` in the same unit as every other
        # `works_scanned`/`files_scanned` figure in this codebase (a corpus
        # TEXT ENTRY count, e.g. denominator-attribution.py's
        # `files_scanned`), not a deduped work-identity count.
        works_scanned += 1
        if status == "no_tail":
            works_without_tail += 1
        elif status == "unclassifiable":
            works_unclassifiable += 1
            unclassifiable.append(result)
        else:  # "ok"
            works_with_terminal_provenance += 1
            terminal_provenance_lines += result["terminal_provenance_lines"]
            colophon_lines += result["colophon_lines"]
            head_line_hits += result["head_line_hits"]
            if len(provenance_samples) < MAX_SAMPLES:
                provenance_samples.append(
                    {
                        "work_id": work_id,
                        "label": result["label"],
                        "terminal_provenance_lines": result["terminal_provenance_lines"],
                    }
                )
            if result["has_colophon"]:
                works_with_colophon += 1
                if len(colophon_samples) < MAX_SAMPLES:
                    colophon_samples.append(
                        {
                            "work_id": work_id,
                            "label": result["label"],
                            "colophon_lines": result["colophon_lines"],
                        }
                    )

    unclassifiable.sort(key=lambda row: (row["work_id"], row["absolute_line_number"]))
    unclassifiable_examples = unclassifiable[:MAX_EXAMPLES]
    verdict = VERDICT_FAIL if unclassifiable else VERDICT_OK

    return {
        "schema_version": SCHEMA_VERSION,
        "verdict": verdict,
        "corpus_root": str(corpus_root),
        "works_scanned": works_scanned,
        "works_with_terminal_provenance": works_with_terminal_provenance,
        "works_with_colophon": works_with_colophon,
        "works_without_tail": works_without_tail,
        "works_unclassifiable": works_unclassifiable,
        "terminal_provenance_lines": terminal_provenance_lines,
        "colophon_lines": colophon_lines,
        "head_line_hits": head_line_hits,
        "unreadable_entries": sorted(unreadable),
        "unclassifiable_total": len(unclassifiable),
        "unclassifiable_examples": unclassifiable_examples,
        "provenance_samples": provenance_samples,
        "colophon_samples": colophon_samples,
        "boundary_rule": BOUNDARY_RULE,
        "rule_extensions": RULE_EXTENSIONS,
        "rule_extensions_note": (
            "empty: the brief's head-marker skeleton, unmodified, classified every "
            "tail line in this run with zero unclassifiable residual (works_unclassifiable "
            "== 0) -- no extension beyond 底本：/底本の親本： (provenance) and "
            "入力：/校正：/青空文庫作成ファイル：/※ (colophon) was needed. Lines matching "
            "other colophon-shaped prefixes the reference scanner also recognizes "
            "(親本：/初出：/校閲：/作成日：/修正：/ファイル作成：) are still classified "
            "correctly: they inherit whatever state the block already carries rather than "
            "needing their own head entry, since they never open a NEW block by themselves "
            "in the corpus."
        ),
        "reference_coverage": {
            "date": REFERENCE_COVERAGE_DATE,
            "terminal_provenance_occurrences": REFERENCE_TERMINAL_PROVENANCE_OCCURRENCES,
            "colophon_occurrences": REFERENCE_COLOPHON_OCCURRENCES,
            "reference_mechanism": (
                "terminal_provenance_occurrences counts SegmentBoundaryTerminalProvenance "
                "markers (crates/ab-source-syntax/src/lib.rs terminal_provenance_note_end): "
                "a ［＃地付き］（…） note immediately followed by a 底本： line -- a narrow "
                "structural co-occurrence, not 'has a 底本 block'. colophon_metadata_occurrences "
                "counts LINES matching a fixed ~20-prefix set including 底本：/底本の親本：/親本："
                "/初出： AND 入力：/校正：/校閲：/作成日：/修正：/ファイル作成：/青空文庫作成"
                "ファイル： together in ONE undifferentiated bucket (is_colophon_metadata_line "
                "in crates/ab-coverage/src/bin/source_inventory.rs) -- this conflation is "
                "exactly what ABC policy v0.2.0 flags needs_measurement_split and what this "
                "instrument splits. It is also a pure per-line prefix match with no state: it "
                "never counts a bare continuation line (an edition/date line, a plain name)."
            ),
            "this_run_unit": (
                "works_scanned/works_with_terminal_provenance/works_with_colophon count "
                "corpus TEXT ENTRIES (see works_scanned note above); "
                "terminal_provenance_lines/colophon_lines count ALL tail lines under the "
                "stateful rule (heads + inherited continuations); head_line_hits counts only "
                "the head-line subset (lines literally matching a provenance_heads/"
                "colophon_heads prefix) -- the comparable-in-kind figure to the reference's "
                "per-line prefix scan, modulo the head-list and prefix-set differences above."
            ),
        },
    }


def markdown_table(rows: list[list[str]]) -> list[str]:
    if not rows:
        return ["_No rows._", ""]
    out = ["| " + " | ".join(rows[0]) + " |", "|" + "|".join(["---"] * len(rows[0])) + "|"]
    for row in rows[1:]:
        out.append("| " + " | ".join(row) + " |")
    out.append("")
    return out


def render_markdown(summary: dict[str, Any]) -> str:
    lines = [
        "# Terminal-Provenance / Colophon-Metadata Measurement Split",
        "",
        f"Verdict: `{summary['verdict']}`",
        "",
        f"- corpus_root: `{summary['corpus_root']}`",
        f"- works_scanned: {summary['works_scanned']}",
        f"- works_with_terminal_provenance: {summary['works_with_terminal_provenance']}",
        f"- works_with_colophon: {summary['works_with_colophon']}",
        f"- works_without_tail: {summary['works_without_tail']}",
        f"- works_unclassifiable: {summary['works_unclassifiable']}",
        f"- terminal_provenance_lines: {summary['terminal_provenance_lines']}",
        f"- colophon_lines: {summary['colophon_lines']}",
        f"- head_line_hits: {summary['head_line_hits']}",
        f"- unreadable_entries: {len(summary['unreadable_entries'])}",
        "",
        "## Boundary Rule",
        "",
        f"- tail_start_marker: `{summary['boundary_rule']['tail_start_marker']}`",
        f"- tail_start_rule: {summary['boundary_rule']['tail_start_rule']}",
        f"- provenance_heads: {', '.join(summary['boundary_rule']['provenance_heads'])}",
        f"- colophon_heads: {', '.join(summary['boundary_rule']['colophon_heads'])}",
        "",
        "## Rule Extensions",
        "",
        f"- extensions beyond the skeleton: {len(summary['rule_extensions'])}",
        f"- {summary['rule_extensions_note']}",
        "",
    ]
    if summary["rule_extensions"]:
        lines.extend(["### Extension Details", ""])
        lines.extend(
            markdown_table(
                [["marker", "class", "reason", "example_work_id", "example_line"]]
                + [
                    [
                        str(ext.get("marker", "")),
                        str(ext.get("class", "")),
                        str(ext.get("reason", "")),
                        str(ext.get("example_work_id", "")),
                        f"`{ext.get('example_line', '')}`",
                    ]
                    for ext in summary["rule_extensions"]
                ]
            )
        )
    lines.extend(
        [
            "## Reference Coverage Cross-Check",
            "",
            f"- reference date: {summary['reference_coverage']['date']}",
            f"- reference terminal_provenance_occurrences: "
            f"{summary['reference_coverage']['terminal_provenance_occurrences']}",
            f"- reference colophon_occurrences: {summary['reference_coverage']['colophon_occurrences']}",
            f"- reference mechanism: {summary['reference_coverage']['reference_mechanism']}",
            f"- this run's unit: {summary['reference_coverage']['this_run_unit']}",
            "",
            "## Provenance Samples",
            "",
        ]
    )
    lines.extend(
        markdown_table(
            [["work_id", "label", "terminal_provenance_lines"]]
            + [
                [row["work_id"], f"`{row['label']}`", str(row["terminal_provenance_lines"])]
                for row in summary["provenance_samples"]
            ]
        )
    )
    lines.extend(["## Colophon Samples", ""])
    lines.extend(
        markdown_table(
            [["work_id", "label", "colophon_lines"]]
            + [
                [row["work_id"], f"`{row['label']}`", str(row["colophon_lines"])]
                for row in summary["colophon_samples"]
            ]
        )
    )
    if summary["unclassifiable_examples"]:
        lines.extend(["## Unclassifiable Residual (fail-closed)", ""])
        lines.extend(
            markdown_table(
                [["work_id", "label", "absolute_line_number", "line"]]
                + [
                    [
                        row["work_id"],
                        f"`{row['label']}`",
                        str(row["absolute_line_number"]),
                        f"`{row['line']}`",
                    ]
                    for row in summary["unclassifiable_examples"]
                ]
            )
        )
    if summary["unreadable_entries"]:
        lines.extend(["## Unreadable Entries", ""])
        lines.extend([f"- `{entry}`" for entry in summary["unreadable_entries"][:MAX_EXAMPLES]])
        lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--corpus-root", type=pathlib.Path, required=True)
    parser.add_argument("--summary-json", type=pathlib.Path, required=True)
    parser.add_argument("--report-md", type=pathlib.Path, required=True)
    parser.add_argument("--jobs", type=int, default=1)
    args = parser.parse_args()

    corpus_root = args.corpus_root.resolve()
    summary = build_summary(corpus_root, args.jobs)

    write_json(args.summary_json, summary)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.write_text(render_markdown(summary), encoding="utf-8")

    if summary["verdict"] != VERDICT_OK:
        examples = summary["unclassifiable_examples"]
        print(
            f"FAIL-CLOSED: {summary['unclassifiable_total']} unclassifiable tail line(s) "
            f"(showing up to {MAX_EXAMPLES}):",
            file=sys.stderr,
        )
        for example in examples:
            print(
                f"  {example['work_id']} {example['label']} "
                f"line {example['absolute_line_number']}: {example['line']!r}",
                file=sys.stderr,
            )
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
