# AAT Span Audit

## Existing Emitters

| File | Node type | Coordinate meaning | Evidence |
| --- | --- | --- | --- |
| `crates/ab-source-syntax/src/lib.rs` | Internal `SourceEvent` spans | Decoded UTF-8 byte offsets into the Rust `&str` being scanned, plus a one-based-ish line counter as maintained by the scanner. These are not original Shift_JIS byte offsets. | `source_events` walks `txt` with `offset += ch.len_utf8()` and creates spans through `span_for(start, end, line)`. |
| `adapters/aozora-rs/src/aat.rs` | Source-supplement and fallback logic | Consumes internal `SourceEvent.span.start/end` for ordering, orphan-ruby detection, and implicit ruby-base lookup. It does not currently serialize these spans into AAT node `span` objects. | Uses `event.span.start` and `event.span.end` around source-derived gaiji/ruby repair logic; no `"span"` JSON emission in this adapter file. |
| `adapters/aozora2/src/lib.rs` | AAT nodes | No current AAT span emission found. | Repository search finds no `line_start`, `byte_start`, `byte_end`, or `"span"` node fields in this adapter. |
| `adapters/aozora2html/adapter.py` | AAT nodes | No current AAT source-span emission found. XHTML `<span>` elements are parsed as markup nodes, not AAT coordinate spans. | Search hits `span` as an XHTML element name, not AAT `span` coordinate fields. |
| `data/fixtures/aat-valid-nested.json` | Fixture AAT spans | Fixture numeric offsets; useful for schema acceptance but not evidence of production adapter coordinate semantics. | Contains `line_start`, `line_end`, `byte_start`, and `byte_end` values for schema tests. |
| `data/aat-schema.json` | AAT `span` definition | Defines the required fields inside a `span` object but does not encode coordinate-system metadata. | `$defs.span` requires `line_start`, `line_end`, `byte_start`, and `byte_end`. |
| `crates/ab-check/src/aat.rs` | AAT consumers | Reads only `span.line_start` when present for diagnostics. It does not interpret byte offsets. | `node_line` returns `span.line_start` only. |
| `crates/ab-ir/src/semantic_summary.rs` | Semantic summary spans | Separate `SourceSpan { start, end }` type, currently populated as `None` in semantic summaries. | Semantic summary construction sets `source_span: None` throughout current paths. |

## Conclusion

Current production AAT output does not have a reliable serialized `span`
emitter. The only active offset producer found is `ab-source-syntax`, and its
offsets are decoded UTF-8 byte offsets into a Rust string, not original encoded
file bytes.

The existing AAT v1 contract can continue to define `byte_start` and `byte_end`
as decoded UTF-8 offsets when a `span` object is present, but the implementation
state is best described as "mostly absent, internally decoded UTF-8 where source
events exist." There is no evidence that current production AAT contains raw
Shift_JIS byte offsets.

## Migration Decision

Do not change `data/aat-schema.json` in this audit task.

Before adding `coordinate_system`, `char_start`, `char_end`, `raw_byte_start`, or
`raw_byte_end`, first add a production adapter test that serializes a real AAT
`span` from a fixture string and computes expected offsets from that fixture
string. Once such an emitter exists, `coordinate_system = "decoded_utf8"` can be
added as an optional schema-compatible field for those spans.

Original encoded file byte offsets must use distinct fields such as
`raw_byte_start` and `raw_byte_end`; they must not reuse AAT v1 `byte_start` and
`byte_end`.
