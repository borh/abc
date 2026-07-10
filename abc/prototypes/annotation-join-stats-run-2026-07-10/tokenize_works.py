#!/usr/bin/env python3
"""Operator-side tokenizer wrapper for the 2026-07-10 annotation-join-stats
corpus run (disposable; recorded in the handoff).

Feeds every work's rendered plaintext through ONE vibrato-tokenize process
(dictionary loads once), then splits the MeCab-format output back into
per-work token files using the per-line EOS markers (vibrato emits exactly
one EOS per input line, including blank lines — probed 2026-07-10).

Output contract (abc annotation-join-stats token handoff):
  <tokens-dir>/<work-id>.tokens.jsonl — one {"surface": ...} per token line,
  in text order.
"""
import json
import subprocess
import sys
from pathlib import Path

plaintext_dir, tokens_dir, vibrato_cmd = sys.argv[1], sys.argv[2], sys.argv[3:]
plaintext_dir = Path(plaintext_dir)
tokens_dir = Path(tokens_dir)
tokens_dir.mkdir(parents=True, exist_ok=True)

works = sorted(plaintext_dir.glob("*.txt"))
line_counts = []
stdin_chunks = []
for work in works:
    text = work.read_text(encoding="utf-8")
    lines = text.split("\n")
    line_counts.append((work.stem, len(lines)))
    stdin_chunks.append("\n".join(lines))

full_input = "\n".join(stdin_chunks) + "\n"

proc = subprocess.run(
    vibrato_cmd,
    input=full_input.encode("utf-8"),
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    check=True,
)

# Parse: group token surfaces per line, delimited by EOS.
lines_tokens = []
current = []
for raw in proc.stdout.decode("utf-8").split("\n"):
    if raw == "EOS":
        lines_tokens.append(current)
        current = []
    elif raw:
        surface = raw.split("\t", 1)[0]
        current.append(surface)

expected_lines = sum(n for _, n in line_counts)
if len(lines_tokens) != expected_lines:
    sys.exit(
        f"line-count mismatch: fed {expected_lines} lines, "
        f"got {len(lines_tokens)} EOS groups"
    )

offset = 0
for work_id, n_lines in line_counts:
    surfaces = [s for line in lines_tokens[offset : offset + n_lines] for s in line]
    offset += n_lines
    out = tokens_dir / f"{work_id}.tokens.jsonl"
    with out.open("w", encoding="utf-8") as fh:
        for s in surfaces:
            fh.write(json.dumps({"surface": s}, ensure_ascii=False) + "\n")

print(f"tokenized {len(works)} works ({expected_lines} lines)")
