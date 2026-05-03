# Adapter Fidelity Matrix

This document tracks whether each adapter is faithful to the upstream parser it
wraps. It does not define linguistic correctness. Correct Aozora gaiji
resolution and similar truth-data checks belong in an oracle layer that can be
compared against every adapter.

For AAT node semantics, result axes, selector protocol, and span coordinate
rules, see `docs/aat-contract.md`.

## Fidelity Levels

| Level | Meaning |
| --- | --- |
| Direct | Adapter uses upstream parser output directly and preserves the relevant parser events in AAT. |
| Direct incomplete | Adapter uses upstream parser output directly, but known upstream events are still dropped, flattened, or only preserved as `raw`. This is a fidelity failure for the affected structure, not a minor display limitation. |
| Indirect | Adapter is faithful to an intermediate representation, not directly to the original Aozora source markup. Source-level claims require separate oracle or source-marker checks. |

## Matrix

| Adapter | Faithfulness level | Upstream dependency | Entry point used | Preserved behavior | Known fidelity gaps / limits |
| --- | --- | --- | --- | --- | --- |
| `aozora-rs` | Direct | `aozora-rs-core` v0.6.0 plus `aozora-rs-gaiji` v0.6.0 | `AozoraDocument::parse` with adapter projection and `aozora-rs-gaiji::{parse_tag, gaiji_to_char}` for gaiji | Parser-normalized nodes, upstream gaiji resolution behavior including unresolved upstream results, ruby/gaiji nesting, fallback status in metadata. Fidelity policy: record upstream behavior as emitted; do not patch oracle-correct gaiji results inline. | Upstream `aozora-rs-gaiji` does not resolve every JIS-form gaiji, so faithful adapter output may still fail oracle correctness. |
| `aozora2` | Direct incomplete | `aozora-core` 0.7.1 from crates.io | `aozora_core::tokenize` then `aozora_core::parse` | Text, ruby with nested content, gaiji, accent, figure, warigaki, inline wrappers, notes, reconstructed block containers for jisage, keigakomi, yokogumi, caption, and heading, plus style scopes for one-line jisage, chitsuki, jizume, and burasage. | Block fidelity is still incomplete for block tcy and other unsupported boundary types, which remain raw. `--mode html` is unsupported because `aozora-core` does not expose an upstream HTML renderer. |
| `aozora2html` | Indirect | Ruby gem `aozora2html` 3.0.1 | Gem-rendered XHTML parsed by the Python mapper | XHTML-derived paragraphs, styles, ruby, visible parser output, and parser failure metadata. | Faithful to rendered XHTML, not directly to original Aozora markup. Source marker details, resolved gaiji markers that become plain text, and split structures not encoded in XHTML cannot always be recovered. |

## Current Oracle Matrix

The latest cross-adapter report is summarized in
`reports/aat-fidelity/cross-adapter-summary.md`; the source JSON for that run is
kept under `/db/ab-validator/aat-fidelity/cross-adapter/report.json`.

| Adapter | Reviewed cases | Oracle pass | Oracle fail | Interpretation |
| --- | ---: | ---: | ---: | --- |
| `aozora2` | 43 | 43 | 0 | Current reviewed-case AAT baseline. |
| `aozora-rs` | 43 | 4 | 39 | Needs more upstream observations before failures can be cleanly separated into faithful upstream behavior vs adapter projection gaps. |
| `aozora2html` | 43 | 1 | 42 | Fragment handling is fixed, but source-level oracle assertions mostly exceed what the rendered XHTML mapper currently reconstructs. |

## Follow-up Checks

- Add an oracle table for expected gaiji resolutions independent of adapter
  output.
- Add upstream observations for the remaining `aozora-rs` and `aozora2html`
  oracle failures before treating every mismatch as an adapter bug.
- Expand `aozora2` block reconstruction to remaining upstream boundary types
  such as block tcy.
- Keep version strings tied to the dependency actually loaded by each adapter.
