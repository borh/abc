# IR Publication Coverage

Verdict: `IR_PUBLICATION_COVERAGE_COMPLETE`

## Source Region Contract

Verdict: `SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`

Schema: `https://w3id.org/abc/schemas/source-region-coverage.schema.json` `aozora-source-region-coverage-v1`

Policy: `https://w3id.org/abc/policies/source-region-publication-v0` `0.1.0`

Manifest sidecar role present: `true`

## Node Coverage

| Class | Node types |
|---|---:|
| `tei_exact` | 7 |
| `tei_policy_projection` | 7 |

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
| `unsupported_gap` | 16 |

## Unsupported-Derived Closure Coverage

Closure-adjusted view of raw unsupported-derived mapping rows. A row is a true unsupported gap only if it has no TEI/profile/custom closure family.

Total raw unsupported-derived rows: 151

| Status | Rows |
|---|---:|
| `admitted_by_custom_contract` | 31 |
| `admitted_by_tei_profile` | 120 |
| `classified_but_not_admitted` | 0 |
| `true_unsupported_gap` | 0 |

## Raw Unsupported-Derived Mapping Rows

Raw unsupported-derived mapping rows before closure folding. Use unsupported_derived_closure_coverage or closure_gaps to decide whether any row remains a true unsupported gap.

Count: 151

| Owner | Count |
|---|---:|
| `aat_to_parser_ir_converter` | 5 |
| `custom_schema` | 97 |
| `policy` | 49 |

- `blocks[].children[].children[].content[].accent` owner `policy`
- `blocks[].children[].children[].content[].span` owner `policy`
- `blocks[].children[].children[].content[].style` owner `policy`
- `blocks[].children[].children[].content[].warigaki.upper[].span` owner `policy`
- `blocks[].children[].children[].heading.content[].span` owner `policy`
- `blocks[].children[].children[].heading.content[].style` owner `policy`
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
- `blocks[].children[].heading.content[].span` owner `policy`
- `blocks[].children[].heading.content[].style` owner `policy`
- `blocks[].children[].span` owner `policy`
- `blocks[].content[].accent` owner `policy`

## Closure Gaps

Admitted by custom contract: 31

Admitted by TEI profile: 120

Classified but not admitted: 0

| Admitted family | Count |
|---|---:|
| `gaiji_unresolved_reason` | 6 |
| `provenance_metrics` | 3 |
| `source_identity` | 5 |
| `span_coordinates` | 17 |

| TEI profile admitted family | Count |
|---|---:|
| `accent` | 14 |
| `figure_metadata` | 85 |
| `heading_jisage_structure` | 7 |
| `style_rendition` | 14 |

True unsupported gaps: 0

## Plaintext Policy

`exclude_ruby_readings_layout_source_notes_custom_records_warnings_and_provenance`

## TEI-EAJ Calibration

TEI-EAJ rows are calibration evidence, not source authority.
