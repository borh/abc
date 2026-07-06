# AAT Contract

The Aozora Adapter Tree (AAT) is the normalized JSON emitted by parser
adapters. AAT records adapter output and provenance. It does not by itself
assert linguistic correctness.

Adapters may add `x-*` extension fields to nodes where the schema permits them.
`x-provenance = "source-derived"` means the node was reconstructed from source
marker text that survived in parser output, not emitted as a first-class parser
event. Consumers must not count source-derived nodes as direct upstream parser
faithfulness.

`data/aat-schema.json` is the normative validation schema for AAT v1 document
shape. This document explains the contract semantics that schema validation
cannot fully express, including selector behavior, result axes, span coordinate
meaning, and projection rules.

## Versioning

AAT documents use top-level `version`. Current AAT is `version = 1`.

Oracle case files use `aat_version` because they are not AAT documents; they
are test data that targets an AAT contract version. `aat_version = 1` means the
oracle expectations are written against AAT document `version = 1`.

Schema-compatible additions to AAT v1 may add optional fields. Required field
changes or changed node semantics require AAT v2.

## Result Axes

| Axis | Question |
| --- | --- |
| Schema validity | Is this structurally valid AAT for the declared version? |
| Adapter faithfulness | Did the adapter preserve what its upstream parser emitted? |
| Oracle correctness | Does the output match independent Aozora/literary expectations? |

These axes are independent. A faithful adapter can fail oracle correctness when
upstream is incomplete.

## Oracle Correctness Provenance

Oracle cases are independent truth-data claims, not adapter consensus. Evidence
for oracle correctness lives in top-level `[[evidence]]` records in
`data/aat-oracle-cases.toml`; cases reference those records through
`evidence_ids`.

Review state is a succession of `[[case.review]]` values. The current review
status is the last review entry. This preserves review history instead of
mutating a single status field without context.

`oracle_status` reports only whether adapter output matches the oracle
assertions. It remains independent of oracle credibility. Reports carry
`oracle_review_status` and `oracle_evidence_strength` as separate fields.

Assertion-level `evidence_ids` are optional overrides. When omitted or empty,
the assertion inherits the case-level `evidence_ids`.

Retired oracle cases are not evaluated in reports by default.

Evidence strength is derived from linked evidence using strongest-wins
precedence: `unicode` evidence reports `normative`, `reference_table` reports
`reference`, and `curator_note` reports `curated`. If more than one evidence
kind is linked, the strongest linked kind wins.

`visible_text` assertions inherit the case-level `evidence_ids`; there is no
separate `visible_text_evidence_ids` field in this contract. If a visible-text
claim needs different evidence from the rest of the case, split it into a
separate oracle case.

Evidence records may contain extension fields whose names start with `x-`.
Consumers must ignore unknown `x-*` fields.

`reviewed_at` is stored as `YYYY-MM-DD` text. This contract validates the
shape; it does not require semantic calendar-date validation.

## Document Shape

An AAT document is an object with these top-level fields:

| Field | Required | Type | Meaning |
| --- | --- | --- | --- |
| `version` | yes | integer | AAT contract version. Current value is `1`. |
| `work_id` | yes | string | Work/source identifier assigned by the adapter or caller. |
| `blocks` | yes | array of block nodes | Document body. |
| `meta` | yes | object | Adapter and parse metadata. |

`meta` requires:

| Field | Type | Meaning |
| --- | --- | --- |
| `adapter` | string | Adapter id such as `aozora-rs`, `aozora2`, or `aozora2html`. |
| `adapter_version` | string | Adapter and upstream parser identity. |
| `source_encoding` | string | Decoded source encoding label. |
| `source_hash` | string | SHA-256 hash of the source bytes. |
| `parse_complete` | boolean | Whether the adapter reports a complete parser-backed projection. |
| `warnings` | array | Adapter warnings. |

## Block Nodes

| Kind | Required fields | Optional fields | Meaning |
| --- | --- | --- | --- |
| `paragraph` | `kind`, `content` | `span`, `x-*` | Inline content in reading order. |
| `heading` | `kind`, `level`, `style`, `content` | `span`, `x-*` | Block-level heading. |
| `jisage_block` | `kind`, `children` | `span`, `x-*` | Indented block container. |
| `quote_block` | `kind`, `children` | `span`, `x-*` | Quoted block container. |
| `keigakomi_block` | `kind`, `children` | `span`, `x-*` | Ruled enclosure block container. |
| `yokogumi_block` | `kind`, `children` | `span`, `x-*` | Horizontal-composition block container. |
| `caption_block` | `kind`, `children` | `span`, `x-*` | Caption block container. |

`content` arrays contain inline nodes. `children` arrays contain block nodes.
Block containers can nest through `children`; paragraphs and headings carry
inline content through `content`.

## Inline Nodes

| Kind | Required fields | Optional fields | Plaintext projection | Meaning |
| --- | --- | --- | --- | --- |
| `text` | `kind`, `value` | `span`, `x-*` | `value` | Plain text. |
| `ruby` | `kind`, `base`, `reading` | `base_content`, `reading_content`, `direction`, `span`, `x-*` | `base` | Ruby annotation. Nested source structure can be preserved in `base_content` and `reading_content`. |
| `gaiji` | `kind`, `description`, `resolved`, `unresolved_reason` | `jis_code`, `span`, `x-*` | `resolved` when non-empty, otherwise empty | Aozora gaiji marker or parser-normalized gaiji. |
| `accent` | `kind`, `code`, `name`, `resolved` | `span`, `x-*` | `resolved` when non-empty, otherwise `name` | Latin accent notation. |
| `figure` | `kind`, `filename`, `alt`, `css_class` | `width`, `height`, `caption`, `span`, `x-*` | empty | Inline figure/image. |
| `raw` | `kind`, `source` | `span`, `x-*` | adapter-defined, normally empty for oracle checks | Faithful escape hatch for unsupported parser events. |
| `style` | `kind`, `content` | `style_type`, `level`, `class_name`, `span`, `x-*` | child projection | Inline style or normalized parser scope. |
| `font_size` | `kind`, `content` | `size_type`, `level`, `span`, `x-*` | child projection | Inline font-size scope. |
| `tcy` | `kind`, `content` | `span`, `x-*` | child projection | Vertical-in-horizontal text. |
| `yokogumi` | `kind`, `content` | `span`, `x-*` | child projection | Inline horizontal composition. |
| `keigakomi` | `kind`, `content` | `span`, `x-*` | child projection | Inline ruled enclosure. |
| `caption` | `kind`, `content` | `span`, `x-*` | child projection | Inline caption. |
| `warigaki` | `kind`, `upper`, `lower` | `span`, `x-*` | upper then lower child projection | Split-line note/warigaki. |

### Source Marker Preservation

AAT v1 has two levels of source representability:

1. Typed representation: a source marker maps to a semantic AAT node such as
   `ruby`, `gaiji`, `figure`, `style`, `tcy`, `keigakomi`, or `warigaki`.
2. Raw preservation: a source marker that cannot be typed must be represented
   as `raw` with the original marker text in `source`, `x-provenance =
   "source-derived"` when recovered from source text, and a source span when the
   adapter has source bytes.

Parser agreement is not sufficient evidence for AAT representability. The
source-inventory gate over raw Aozora text is the authority.

Gaiji fields:

| Field | Type | Meaning |
| --- | --- | --- |
| `description` | string | Human-readable Aozora gaiji description. |
| `resolved` | string or null | Adapter-emitted character resolution, if any. |
| `jis_code` | string or null | Adapter-emitted JIS plane-row-cell or row-cell code, if known. |
| `unresolved_reason` | string or null | Reason the adapter did not resolve the gaiji. Null or empty means resolved/no unresolved reason. |

## Selector Protocol v1

Selectors are path-only. They have no predicates and no embedded comparisons.
Predicate checks belong in oracle assertions.

Selector paths are dot-separated object field names plus `*` array expansion.
Arbitrary explicit depth is allowed, so `blocks.*.children.*.content.*` is a
valid selector. The only recursive selector is `**`; recursive matching must be
requested explicitly.

Supported common selectors:

| Selector | Meaning |
| --- | --- |
| `blocks` | The top-level blocks array. |
| `blocks.*` | Every top-level block object. |
| `blocks.*.content` | Direct `content` arrays on top-level blocks that have `content`. |
| `blocks.*.content.*` | Direct inline children inside top-level block `content` arrays. |
| `blocks.*.children` | Direct `children` arrays on top-level block containers that have `children`. |
| `blocks.*.children.*` | Direct child blocks inside top-level block containers. |
| `**` | Every object in the AAT tree. |

Selectors select arrays or objects. Terminal scalar-property selectors such as
`blocks.*.kind` are not supported in selector protocol v1. Assertions inspect
fields on selected objects instead.

If an object field is missing, that branch is skipped. For example,
`blocks.*.content` skips `caption_block` nodes that have `children` but no
`content`. If `*` is applied to a value that exists but is not an array, the
selector is invalid and the evaluator reports an assertion failure.

## Empty String and Null

For gaiji `resolved`, `jis_code`, and `unresolved_reason`, oracle comparison
treats empty string and JSON null as equivalent only for compatibility with
existing adapter output. This equivalence is limited to those fields.

## Span Coordinate System

AAT v1 span objects require `line_start`, `line_end`, `byte_start`, and
`byte_end`.

When `coordinate_system` is absent, the default AAT v1 coordinate system is
`decoded_utf8`:

- `byte_start` and `byte_end` are UTF-8 byte offsets into the decoded source
  string used by the adapter;
- `line_start` and `line_end` are one-based decoded-source line numbers.

If an adapter emits spans with different semantics, that adapter output is not
conformant to this contract and must be called out in structured fidelity
notes. Future schema-compatible additions may make `coordinate_system =
"decoded_utf8"` explicit and may add optional `char_start`, `char_end`,
`raw_byte_start`, and `raw_byte_end` fields.

See `docs/aat-span-audit.md` for the current implementation audit. As of that
audit, production adapters generally omit serialized AAT spans; internal
source-scanner spans use decoded UTF-8 offsets.
