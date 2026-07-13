# ADR 0037: Terminal Provenance / Colophon Split

Status: Accepted
Date: 2026-07-13

## Context

During the AAT (Aozora Annotation Tree) build pipeline, the sanitize
stage strips encoding artifacts and emits a sanitized body text plus a
terminal tail. The tail contains end-of-work metadata: provenance lines
(底本, etc.) and colophon lines (入力, 校正, etc.). The original Python
reference (`reports/lib/terminal_provenance.py`) classified these for
emission as `source_note` blocks.

## Decision

The Rust adapter separates terminal text from body text at the
`DecodedSource` level via the `sanitized_tail` and `tail_offset` fields.
`tail_offset` records the byte offset of the tail within the full
sanitized text, which composes through `SpanContext.maps` for span
rebasing back to source-byte coordinates.

Tail line classification is a direct transcription of the Python
reference's `classify_tail` function. Provenance heads (底本, etc.) are
emitted as AAT `source_note` blocks with `region_class:
"terminal_provenance"`. Colophon lines (入力：, 校正：, etc.) are excluded
from AAT output. Unclassifiable lines fall back to colophon with a
warning.

## Consequences

- The split is corpus-validated: the corpus scan found zero exotic
  boundary tail lines across 17,886 works.
- Line terminators recognized by the Rust `line_ranges` function are
  `\n`, `\r\n`, and bare `\r` — a deliberate subset of Python's
  `str.splitlines` (which also breaks on `\v`, `\f`, U+2028, etc.).
  This divergence is corpus-absent.
- The split was previously documented in the now-absent
  `docs/superpowers/reports/2026-07-12-terminal-provenance-colophon-split.md`.
