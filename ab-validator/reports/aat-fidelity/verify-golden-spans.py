#!/usr/bin/env python3
"""Check golden AAT spans against the decoded source (independent oracle).

For every node with both `value` and `span`, assert
decoded[byte_start:byte_end] == value, and that line_start equals
1 + count of '\\n' in decoded[:byte_start]. Prints one line per checked
span; exit 1 on any mismatch.

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


def walk(node):
    global failures
    if isinstance(node, dict):
        span, value = node.get("span"), node.get("value")
        if isinstance(span, dict) and isinstance(value, str):
            got = data[span["byte_start"]:span["byte_end"]].decode("utf-8", "replace")
            line = 1 + data[:span["byte_start"]].decode("utf-8", "replace").count("\n")
            ok_v = got == value
            ok_l = line == span["line_start"]
            print(f"{'OK ' if ok_v and ok_l else 'FAIL'} span={span} value={value!r} slice={got!r} line={line}")
            failures += 0 if ok_v and ok_l else 1
        for v in node.values():
            walk(v)
    elif isinstance(node, list):
        for v in node:
            walk(v)


walk(doc["blocks"])
sys.exit(1 if failures else 0)
