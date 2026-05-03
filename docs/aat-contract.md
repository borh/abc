# AAT Contract

The Aozora Adapter Tree (AAT) is the normalized JSON emitted by parser adapters.
AAT records adapter output and provenance. It does not by itself assert
linguistic correctness.

## Versioning

AAT documents use top-level `version`. Current AAT is `version = 1`.

Oracle cases use `aat_version = 1` to declare the AAT contract they target.
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

## Selector Protocol v1

Selectors are path-only. They have no predicates and no embedded comparisons.
Predicate checks belong in oracle assertions.

Supported selectors:

| Selector | Meaning |
| --- | --- |
| `blocks` | The top-level blocks array. |
| `blocks.*` | Every top-level block object. |
| `blocks.*.content` | Direct `content` arrays on top-level blocks that have `content`; blocks without `content` are skipped. |
| `blocks.*.content.*` | Direct inline children inside top-level block `content` arrays. |
| `blocks.*.children` | Direct `children` arrays on top-level block containers that have `children`; blocks without `children` are skipped. |
| `blocks.*.children.*` | Direct child blocks inside top-level block containers. |
| `**` | Every object in the AAT tree. Recursive matching must be explicit. |

Path mismatches skip missing object fields. A `*` segment only expands arrays.
If `*` is applied to a non-array, the selector is invalid and the evaluator
reports an assertion failure.

## Empty String and Null

For gaiji `resolved`, `jis_code`, and `unresolved_reason`, oracle comparison
treats empty string and JSON null as equivalent only for compatibility with
existing adapter output. This equivalence is limited to those fields.

## Span Coordinate System

AAT v1 spans require `line_start`, `line_end`, `byte_start`, and `byte_end`.
Before changing span semantics, implementations must audit current adapter span
emission.

If `coordinate_system = "decoded_utf8"` is present, then:

- `byte_start` and `byte_end` are UTF-8 byte offsets into the decoded source
  string used by the adapter;
- `line_start` and `line_end` are one-based decoded-source line numbers;
- optional `char_start` and `char_end` are Unicode scalar offsets into the
  decoded source string;
- original encoded byte offsets require separate fields such as
  `raw_byte_start` and `raw_byte_end`.

Tests must compute expected offsets from fixture strings rather than using
unexplained numeric literals.
