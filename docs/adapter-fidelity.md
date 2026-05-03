# Adapter Fidelity Matrix

This document tracks whether each adapter is faithful to the upstream parser it
wraps. It does not define linguistic correctness. Correct Aozora gaiji
resolution and similar truth-data checks belong in an oracle layer that can be
compared against every adapter.

For AAT node semantics, result axes, selector protocol, and span coordinate
rules, see `docs/aat-contract.md`.

| Adapter | Upstream dependency | Entry point used | Preserved behavior | Known limits |
| --- | --- | --- | --- | --- |
| `aozora-rs` | `aozora-rs-core` v0.6.0 plus `aozora-rs-gaiji` v0.6.0 | `AozoraDocument::parse` with adapter projection and `aozora-rs-gaiji::{parse_tag, gaiji_to_char}` for gaiji | Parser-normalized nodes, upstream gaiji resolution behavior, ruby/gaiji nesting, fallback status in metadata | Upstream `aozora-rs-gaiji` does not resolve every JIS-form gaiji; adapter must report that faithfully rather than patching it inline |
| `aozora2` | `aozora-core` 0.7.1 from crates.io | `aozora_core::tokenize` then `aozora_core::parse` | Text, ruby with nested content, gaiji, accent, figure, warigaki, inline wrappers, notes and block boundary markers as raw nodes | Current AAT builder still emits a single paragraph and does not reconstruct full block nesting from `BlockStart`/`BlockEnd`; `--mode html` is unsupported because `aozora-core` does not expose an upstream HTML renderer |
| `aozora2html` | Ruby gem `aozora2html` 3.0.1 | Gem-rendered XHTML parsed by the Python mapper | XHTML-derived paragraphs, styles, ruby, visible parser output, parser failure metadata | Mapping is faithful to rendered XHTML, not the original Aozora markup; resolved gaiji may be plain text, source marker details can be lost, and split structures not encoded in XHTML cannot be recovered |

## Follow-up Checks

- Add an oracle table for expected gaiji resolutions independent of adapter
  output.
- Expand `aozora2` fixtures when full block reconstruction becomes necessary.
- Keep version strings tied to the dependency actually loaded by each adapter.
