# IR Publication Coverage

Verdict: `IR_PUBLICATION_COVERAGE_BLOCKED_UNSUPPORTED_GAPS`

## Node Coverage

| Class | Node types |
|---|---:|
| `tei_exact` | 7 |
| `tei_policy_projection` | 6 |

## Field Coverage

| Class | Field facts |
|---|---:|
| `tei_exact` | 4 |
| `tei_policy_projection` | 9 |
| `tei_plus_abc_extension` | 1 |
| `custom_sidecar` | 5 |

| Field | Class | Target |
|---|---|---|
| `gaiji.raw_marker` | `tei_policy_projection` | TEI g/charDecl raw-marker preservation policy |
| `gaiji.reference` | `tei_exact` | TEI g/charDecl reference linkage |
| `gaiji.unicode` | `tei_exact` | TEI g/charDecl Unicode value |
| `gaiji.resolved` | `tei_plus_abc_extension` | TEI visible glyph plus ABC resolution-status preservation |
| `ruby.base` | `tei_exact` | TEI ruby base text |
| `ruby.reading` | `tei_exact` | TEI ruby reading text |
| `ruby.direction` | `tei_policy_projection` | TEI ruby placement policy for left/right readings |
| `heading.level` | `tei_policy_projection` | TEI section/head level policy |
| `emphasis.inline_children` | `tei_policy_projection` | TEI hi content with nested inline children policy |
| `paragraph.layout` | `tei_policy_projection` | TEI p@rend paragraph layout policy |
| `paragraph.node_range` | `custom_sidecar` | ABC sidecar node-range traceability |
| `paragraph.role` | `tei_policy_projection` | TEI paragraph routing/body-vs-note role policy |
| `source-note.placement` | `tei_policy_projection` | TEI front/body/back source-note routing |
| `source-note.classification` | `custom_sidecar` | ABC sidecar source-note provenance classification |
| `caption.target` | `tei_policy_projection` | TEI caption-to-figure association policy |
| `quote.marker_type` | `tei_policy_projection` | TEI quote/cit marker interpretation policy |
| `mapping.identity` | `custom_sidecar` | ABC sidecar mapping id/version/hash linkage |
| `divergence.records` | `custom_sidecar` | ABC sidecar divergence record set |
| `source.pointer` | `custom_sidecar` | ABC sidecar source pointer linkage |

## Diagnostic Coverage

| Class | Diagnostic facts |
|---|---:|
| `custom_sidecar` | 10 |

| Diagnostic field | Class | Target |
|---|---|---|
| `warnings[]` | `custom_sidecar` | ABC sidecar warning record collection |
| `warnings[].code` | `custom_sidecar` | ABC sidecar warning code preservation |
| `warnings[].severity` | `custom_sidecar` | ABC sidecar warning severity preservation |
| `warnings[].message` | `custom_sidecar` | ABC sidecar warning message preservation |
| `warnings[].span` | `custom_sidecar` | ABC sidecar warning span preservation |
| `errors[]` | `custom_sidecar` | ABC sidecar error record collection |
| `errors[].code` | `custom_sidecar` | ABC sidecar error code preservation |
| `errors[].severity` | `custom_sidecar` | ABC sidecar error severity preservation |
| `errors[].message` | `custom_sidecar` | ABC sidecar error message preservation |
| `errors[].span` | `custom_sidecar` | ABC sidecar error span preservation |

## Source Construct Coverage

| Class | Constructs |
|---|---:|
| `tei_exact` | 3 |
| `tei_policy_projection` | 5 |
| `tei_plus_abc_extension` | 2 |
| `unsupported_gap` | 21 |

## Unsupported gaps

Count: 165

| Owner | Count |
|---|---:|
| `aat_to_parser_ir_converter` | 5 |
| `custom_schema` | 104 |
| `parser_ir_schema` | 15 |
| `policy` | 41 |

- `blocks[].children[].children[].content[].accent` owner `policy`
- `blocks[].children[].children[].content[].span` owner `policy`
- `blocks[].children[].children[].content[].style` owner `policy`
- `blocks[].children[].children[].content[].warigaki.upper[].span` owner `policy`
- `blocks[].children[].children[].span` owner `policy`
- `blocks[].children[].content[].accent` owner `policy`
- `blocks[].children[].content[].content[].accent` owner `policy`
- `blocks[].children[].content[].content[].span` owner `policy`
- `blocks[].children[].content[].content[].style` owner `policy`
- `blocks[].children[].content[].span` owner `policy`
- `blocks[].children[].content[].style` owner `policy`
- `blocks[].children[].content[].warigaki.lower[].span` owner `policy`
- `blocks[].children[].content[].warigaki.upper[].span` owner `policy`
- `blocks[].children[].content[].warigaki.upper[].style` owner `policy`
- `blocks[].children[].span` owner `policy`
- `blocks[].content[].accent` owner `policy`
- `blocks[].content[].content[].accent` owner `policy`
- `blocks[].content[].content[].span` owner `policy`
- `blocks[].content[].content[].style` owner `policy`
- `blocks[].content[].content[].warigaki.upper[].span` owner `policy`

## Plaintext Policy

`exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance`

## TEI-EAJ Calibration

TEI-EAJ rows are calibration evidence, not source authority.
