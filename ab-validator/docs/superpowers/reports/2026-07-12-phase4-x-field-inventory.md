# Phase 4 — repository-wide `x-*` field inventory (Task 2)

Scope: every `x-*` field emitted by the AAT adapter (`crates/ab-aozora-aat/src/lib.rs`),
repository-wide — not a line-range read. This inventory backs the AAT schema v2
rotation (Task 2) and the promotion work in Task 5.

## Raw grep output

```
$ grep -n '"x-' crates/ab-aozora-aat/src/lib.rs | grep -v '^\s*//' | grep -v 'get("x-'
496:                    "x-indent": indent,
510:                    "x-indent": indent,
590:            "x-align": "right",
591:            "x-offset": offset,
592:            "x-provenance": "source-derived"
621:            "x-indent-first": first,
622:            "x-indent-rest": rest,
623:            "x-provenance": "source-derived"
869:        "x-provenance": "source-derived",
872:        heading["x-indent"] = json!(indent);
967:                "x-provenance": "parser-derived",
968:                "x-source-marker-kind": "pageBreak",
969:                "x-break-kind": "page",
987:            "x-provenance": "source-derived",
988:            "x-source-marker-kind": "pageBreak",
989:            "x-break-kind": "page"
1007:            "x-provenance": "source-derived",
1008:            "x-source-marker-kind": "unparsed-source-gap",
1063:        "x-codepoint": gaiji.codepoint,
1109:        "x-provenance": "parser-derived",
1110:        "x-source-marker-kind": marker_kind,
```

## Classification

Every emitted `x-*` field, no third category:

| Field | Sites (lib.rs) | Classification | v2 disposition |
|---|---|---|---|
| `x-indent` | 496, 510 (jisage_block); 872 (heading) | layout | promoted → `indent` (Task 5) |
| `x-align` | 590 (chitsuki style) | layout | promoted → `align` (Task 5) |
| `x-offset` | 591 (chitsuki style) | layout | promoted → `offset_from_end` (Task 5) |
| `x-indent-first` | 621 (burasage style) | layout | promoted → `indent_first` (Task 5) |
| `x-indent-rest` | 622 (burasage style) | layout | promoted → `indent_rest` (Task 5) |
| `x-provenance` | 592, 623, 869, 967, 987, 1007, 1109 | provenance | retained verbatim |
| `x-source-marker-kind` | 968, 988, 1008, 1110 | provenance/marker | retained verbatim |
| `x-break-kind` | 969, 989 | provenance/marker (page-break metadata) | retained verbatim |
| `x-codepoint` | 1063 (gaiji) | provenance-of-resolution | retained verbatim |

**Note:** line-jisage has no emission site — standalone per-line 字下げ markers
remain `raw` nodes (`x-source-marker-kind: "indent"`); the converter's
`line-jisage` style path serves other-adapter v1 documents only. Nothing to
promote; classified, not skipped.
