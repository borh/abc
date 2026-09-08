"""verify-golden-spans.py must suppress ONLY the documented CRLF projection
artifact (sanitize-stage \\r\\n -> \\n normalization on `value`, with `span`
kept over the full raw byte range; see crates/ab-aat/tests/goldens.rs
and the module docstring in verify-golden-spans.py), counted and reported as
`crlf_artifact_suppressed: N`. Any other span/value mismatch (including one
that merely happens to contain a `\\r` but isn't a pure CRLF-collapse of
`value`) must still fail the run.
"""

import json
import pathlib
import subprocess
import sys

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "verify-golden-spans.py"


def _write(tmp_path: pathlib.Path, src: str, blocks: list) -> tuple[pathlib.Path, pathlib.Path]:
    source = tmp_path / "source.txt"
    golden = tmp_path / "source.txt.expected.json"
    source.write_bytes(src.encode("utf-8"))
    golden.write_text(json.dumps({"blocks": blocks}))
    return source, golden


def _run(source: pathlib.Path, golden: pathlib.Path) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(SCRIPT), str(source), str(golden)],
        capture_output=True,
        text=True,
    )


# "abc\r\ndef\n" sanitized to "abc\ndef\n" by the pipeline: the CRLF at
# byte 3..5 collapses to a single '\n' in `value`, while `span` keeps the
# full raw 0..5 range (the documented artifact shape).
CRLF_SOURCE = "abc\r\ndef\n"
CRLF_ARTIFACT_NODE = {
    "span": {"byte_start": 0, "byte_end": 5, "line_start": 1, "line_end": 1},
    "value": "abc\n",
}
# A genuine content error at the second line: the raw slice ("def\n")
# carries no '\r' at all, so this can never be mistaken for the CRLF
# projection artifact no matter how the signature is stated.
REAL_ERROR_NODE = {
    "span": {"byte_start": 5, "byte_end": 9, "line_start": 2, "line_end": 2},
    "value": "XYZ\n",
}


def test_crlf_artifact_alone_is_suppressed_and_exits_zero(tmp_path):
    source, golden = _write(tmp_path, CRLF_SOURCE, [CRLF_ARTIFACT_NODE])
    result = _run(source, golden)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "crlf_artifact_suppressed: 1" in result.stdout
    assert "CRLF" in result.stdout
    assert "FAIL" not in result.stdout


def test_real_span_error_alone_fails(tmp_path):
    source, golden = _write(tmp_path, CRLF_SOURCE, [REAL_ERROR_NODE])
    result = _run(source, golden)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "crlf_artifact_suppressed: 0" in result.stdout
    assert "FAIL" in result.stdout


def test_real_span_error_still_fails_alongside_suppressed_crlf_artifact(tmp_path):
    source, golden = _write(tmp_path, CRLF_SOURCE, [CRLF_ARTIFACT_NODE, REAL_ERROR_NODE])
    result = _run(source, golden)
    assert result.returncode == 1, result.stdout + result.stderr
    assert "crlf_artifact_suppressed: 1" in result.stdout
    assert "FAIL" in result.stdout
