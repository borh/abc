"""Shared corpus source-reading contract for report instruments.

One place for the discovery + classification + reading rules that the
Rust pipeline (`crates/ab-index/src/index.rs` `collect_source_files`) and
the litigated Phase 4 split scanner
(`reports/source-regions/terminal-provenance-split.py`) established, plus
the recovery paths ABC's builder added in abc commit `0bf4beed`
("soranoha: robustly read work ZIPs (SJIS charset + 7zz fallback)").

Every discovered candidate is classified into exactly one of four
classes (Phase 5 review P5-1):

  - ``work``          — readable work source; carries the decoded text,
                        label, reader path, and sha256.
  - ``non_work``      — a zip archive with no ``.txt`` member (the
                        ``_ttz``/``_etc`` auxiliary archives); excluded
                        from every work universe BY DESIGN, matching the
                        Rust pipeline.
  - ``recovered_extra`` — readable ONLY by the tolerant 7zz extraction
                        (nonzero exit tolerated when a ``.txt``
                        materializes). These are OUTSIDE the production
                        17,886-entry universe: the Rust pipeline skips
                        them ("skipping unreadable zip"), and ABC's own
                        7zz fallback throws on nonzero exit, so no
                        production consumer reads them. They are scanned
                        and reported separately so no claim silently
                        excludes readable text.
  - ``unreadable``    — no known reader (stdlib zip with windows-31j
                        member names, local-header bypass, tolerant 7zz)
                        yields any text.

Reader order for zip-shaped candidates (superset of the split scanner's):
  1. ``zipfile.ZipFile(path, metadata_encoding="windows-31j")`` — decodes
     Shift_JIS member names correctly (Python's default cp437 mangles
     them but keeps the ASCII ``.txt`` suffix; windows-31j gives the true
     names). Requires Python >= 3.11.
  2. On a per-member read error: the local-header-trusting bypass
     (imported from the split scanner — the two known central-directory
     corruption patterns).
  3. On total failure: 7zz extraction into a temp dir, tolerant of a
     nonzero exit when a ``.txt`` file materializes (class
     ``recovered_extra`` when only this path works).

Decoding: BOM-gated UTF-8, else Shift_JIS with ``errors="replace"`` —
the split scanner's rule (no fully-UTF-8 work exists in the pinned
corpus).
"""

from __future__ import annotations

import hashlib
import importlib.util
import pathlib
import shutil
import subprocess
import sys
import tempfile
import zipfile
from dataclasses import dataclass

_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

_SPLIT_SCRIPT = (
    _REPO_ROOT / "reports" / "source-regions" / "terminal-provenance-split.py"
)
_spec = importlib.util.spec_from_file_location(
    "terminal_provenance_split", _SPLIT_SCRIPT
)
assert _spec is not None and _spec.loader is not None
_split = importlib.util.module_from_spec(_spec)
sys.modules.setdefault(_spec.name, _split)
_spec.loader.exec_module(_split)

discover_entries = _split.discover_entries
is_text_entry = _split.is_text_entry
sniffs_as_zip = _split.sniffs_as_zip
work_id_from_index_path = _split.work_id_from_index_path
_read_zip_member_bypassing_central_directory = (
    _split._read_zip_member_bypassing_central_directory
)

ZIP_NAME_ENCODING = "windows-31j"


@dataclass
class Candidate:
    """One discovered candidate, classified."""

    path: pathlib.Path
    rel: str
    klass: str  # "work" | "non_work" | "recovered_extra" | "unreadable"
    label: str | None = None  # rel or rel::member for works
    work_id: str | None = None
    data: bytes | None = None
    reader: str | None = None  # "plain" | "zip_stdlib" | "zip_local_header_fallback" | "zip_7zz_tolerant"
    detail: str | None = None  # failure/exclusion detail


def decode_source(data: bytes) -> str:
    if data.startswith(b"\xef\xbb\xbf"):
        try:
            return data.decode("utf-8-sig")
        except UnicodeDecodeError:
            pass
    return data.decode("shift_jis", errors="replace")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _sevenzip_tolerant(path: pathlib.Path) -> tuple[str, bytes] | None:
    """Extract with 7zz into a temp dir; accept the first .txt that
    materializes even on a nonzero exit (more lenient than ABC's strict
    fallback — callers must classify such reads as recovered_extra)."""
    binary = shutil.which("7zz") or shutil.which("7z")
    if binary is None:
        return None
    tmp = tempfile.mkdtemp(prefix="corpus-reader-7z-")
    try:
        subprocess.run(
            [binary, "x", "-y", f"-o{tmp}", str(path)],
            capture_output=True,
            check=False,
        )
        for extracted in sorted(pathlib.Path(tmp).rglob("*.txt")):
            if extracted.is_file() and is_text_entry(extracted.name):
                return extracted.name, extracted.read_bytes()
        return None
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def classify_candidate(corpus_root: pathlib.Path, path: pathlib.Path) -> Candidate:
    rel = str(path.relative_to(corpus_root))
    zip_shaped = path.name.lower().endswith(".zip") or sniffs_as_zip(path)
    if not zip_shaped:
        data = path.read_bytes()
        return Candidate(
            path=path, rel=rel, klass="work", label=rel,
            work_id=work_id_from_index_path(rel), data=data, reader="plain",
        )
    # 1. stdlib with windows-31j member names.
    stdlib_error: str | None = None
    try:
        with zipfile.ZipFile(path, metadata_encoding=ZIP_NAME_ENCODING) as zf:
            member = next((n for n in zf.namelist() if is_text_entry(n)), None)
            if member is None:
                return Candidate(
                    path=path, rel=rel, klass="non_work",
                    detail="zip_no_text_member",
                )
            try:
                data = zf.read(member)
                return Candidate(
                    path=path, rel=rel, klass="work",
                    label=f"{rel}::{member}",
                    work_id=work_id_from_index_path(f"{rel}::{member}"),
                    data=data, reader="zip_stdlib",
                )
            except (zipfile.BadZipFile, OSError) as error:
                stdlib_error = f"{type(error).__name__}: {error}"
    except (zipfile.BadZipFile, OSError) as error:
        stdlib_error = f"{type(error).__name__}: {error}"
    # 2. local-header-trusting bypass (the two known CD-corruption shapes).
    recovered = _read_zip_member_bypassing_central_directory(path, is_text_entry)
    if recovered is not None:
        member, data = recovered
        return Candidate(
            path=path, rel=rel, klass="work",
            label=f"{rel}::{member}",
            work_id=work_id_from_index_path(f"{rel}::{member}"),
            data=data, reader="zip_local_header_fallback",
            detail=stdlib_error,
        )
    # 3. tolerant 7zz — recovers content NO production reader accepts.
    extra = _sevenzip_tolerant(path)
    if extra is not None:
        member, data = extra
        return Candidate(
            path=path, rel=rel, klass="recovered_extra",
            label=f"{rel}::{member}",
            work_id=work_id_from_index_path(f"{rel}::{member}"),
            data=data, reader="zip_7zz_tolerant",
            detail=stdlib_error,
        )
    return Candidate(
        path=path, rel=rel, klass="unreadable", detail=stdlib_error,
    )


def classify_corpus(corpus_root: pathlib.Path) -> list[Candidate]:
    """Discover and classify every candidate, in sorted order."""
    return [
        classify_candidate(corpus_root, path)
        for path in discover_entries(corpus_root)
    ]
