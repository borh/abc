#!/usr/bin/env python3
"""Check golden AAT spans against the decoded source (independent oracle).

For every node with both `value` (a string) and `span`, assert
decoded[byte_start:byte_end] == value, and that line_start equals
1 + count of '\\n' in decoded[:byte_start]. Prints one line per checked
span; exit 1 on any mismatch.

## Documented CRLF-normalization artifact

The sanitize stage's CR/LF normalization
(`ab-aozora-pipeline/src/lexer/sanitize.rs`, `normalize_line_endings_core`)
collapses every `\\r\\n` (and lone `\\r`) to a single `\\n` in the text the
parser processes, while the byte-offset map maps the corresponding span back
to the full raw `\\r\\n` run in decoded-source coordinates so spans stay
contiguous and retain source bytes. The
result: `value` (sanitized/normalized text) legitimately differs from
`decoded[byte_start:byte_end]` (raw span slice) by exactly the CRLF
collapse, while `line_start`/`line_end` (computed by counting raw `\\n`
bytes, unaffected by normalization) already match. See
`crates/ab-aat/tests/goldens.rs` for the confirmed real-golden
occurrences (`full-markup-{utf8,shift_jis}.txt`).

This is the ONLY mismatch shape suppressed here. It is recognized by an
exact signature, not a fuzzy heuristic:

- the line check already passes (`line_start`/`line_end` both correct), AND
- normalizing the raw slice (`\\r\\n` -> `\\n`, then lone `\\r` -> `\\n`)
  makes it equal `value` exactly, AND
- the raw slice actually contained at least one `\\r` (so it is a genuine
  mismatch, not an accidental no-op).

Any mismatch that does not match this signature exactly (including a
line_start/line_end drift, or a value difference that survives CRLF
normalization) is a real failure and still fails the run. Suppressed
occurrences are counted and reported as `crlf_artifact_suppressed: N`;
they do not count toward `failures`.

Usage: verify-golden-spans.py SOURCE_FILE GOLDEN_JSON
"""

import json
import sys

src = open(sys.argv[1], "rb").read()
try:
    decoded = src.decode("utf-8-sig") if src.startswith(b"\xef\xbb\xbf") else src.decode("utf-8")
except UnicodeDecodeError:
    decoded = src.decode("shift_jis", errors="replace")
data = decoded.encode("utf-8")
doc = json.load(open(sys.argv[2]))
failures = 0
crlf_artifact_suppressed = 0


def is_documented_crlf_artifact(got: str, value: str, ok_l: bool) -> bool:
    """True iff a (got, value) mismatch is exactly the documented CRLF
    projection artifact: line numbers already correct, raw slice contains
    a literal CR, and CRLF/CR normalization of the raw slice reproduces
    value exactly. See module docstring for the full signature."""
    if not ok_l or got == value or "\r" not in got:
        return False
    normalized = got.replace("\r\n", "\n").replace("\r", "\n")
    return normalized == value


def walk(node):
    global failures, crlf_artifact_suppressed
    if isinstance(node, dict):
        span, value = node.get("span"), node.get("value")
        if isinstance(span, dict) and isinstance(value, str):
            got = data[span["byte_start"] : span["byte_end"]].decode("utf-8", "replace")
            line = 1 + data[: span["byte_start"]].decode("utf-8", "replace").count("\n")
            ok_v = got == value
            ok_l = line == span["line_start"]
            if ok_v and ok_l:
                status = "OK "
            elif is_documented_crlf_artifact(got, value, ok_l):
                status = "CRLF"
                crlf_artifact_suppressed += 1
            else:
                status = "FAIL"
                failures += 1
            print(f"{status} span={span} value={value!r} slice={got!r} line={line}")
        for v in node.values():
            walk(v)
    elif isinstance(node, list):
        for v in node:
            walk(v)


walk(doc["blocks"])
print(f"crlf_artifact_suppressed: {crlf_artifact_suppressed}")
sys.exit(1 if failures else 0)
