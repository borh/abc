# Divergence Ledger (probe output)

Total entries: 27

- LOSS: 10
- AMBIGUITY: 4
- INVENTION: 8
- UNSUPPORTED: 1
- STRUCTURAL: 4

| AAT field/node | parser-IR target | category | note |
| --- | --- | --- | --- |
| blocks[0][block=heading] | (none) | STRUCTURAL | block container of kind 'heading' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[0].heading.level | heading.level | AMBIGUITY | AAT heading.level range 1-3 vs parser-IR 1-6; values fit but domain differs |
| blocks[0].heading.style | (none) | LOSS | heading.style='main' dropped |
| blocks[1][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[1].content[1].ruby.scope | ruby.scope | INVENTION | AAT has no scope field; defaulted to 'explicit' |
| blocks[1].content[1].ruby.direction | (none) | LOSS | direction=right dropped; parser-IR ruby has no direction |
| blocks[1].content[3].gaiji.raw_marker | gaiji.raw_marker | INVENTION | AAT has no raw source marker; used description as raw_marker |
| blocks[1].content[3].gaiji.resolved | gaiji.resolved | AMBIGUITY | AAT resolved is string (the chosen char); parser-IR resolved is boolean (was it resolved?) |
| blocks[1].content[3].gaiji.unicode | gaiji.unicode | LOSS | AAT does not separate unicode codepoint from resolved string |
| blocks[1].content[4].accent | emphasis | AMBIGUITY | accent mapped to emphasis; accent code/name semantics not preserved |
| blocks[1].content[4].accent.code | emphasis.style | INVENTION | used accent.code='CU' as free-form style string |
| blocks[1].content[4].accent.name | (none) | LOSS | accent.name='circumflex' has no parser-IR field |
| blocks[2][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[2].content[0].figure.filename | image.src | INVENTION | filename is not a resolved source path; used as src |
| blocks[2].content[0].figure.css_class | (none) | LOSS | css_class=source-note dropped; parser-IR image has no such field |
| blocks[2].content[0].figure.width | (none) | LOSS | width=400 dropped; parser-IR image has no such field |
| blocks[2].content[0].figure.height | (none) | LOSS | height=300 dropped; parser-IR image has no such field |
| blocks[3][block=paragraph] | (none) | STRUCTURAL | block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted |
| blocks[3].content[1].warigaki | (none) | UNSUPPORTED | parser-IR has no warigaki node; upper/lower flattened to text nodes, split-line structure lost |
| meta.source_hash | source.work_content_hash | AMBIGUITY | AAT hashes raw source bytes; parser-IR work_content_hash is content hash; identifier semantics differ |
| (none) | source.normalization | INVENTION | parser-IR requires normalization enum; AAT has none -> defaulted 'source' |
| (none) | source.source_path | INVENTION | parser-IR source_path optional; AAT has none -> null |
| meta.adapter | (none) | LOSS | adapter='aozora2html' dropped; parser-IR carries no producer identity / parse status |
| meta.adapter_version | (none) | LOSS | adapter_version='aozora2html-adapter 0.1.0 gem-3.0.1' dropped; parser-IR carries no producer identity / parse status |
| meta.parse_complete | (none) | LOSS | parse_complete=True dropped; parser-IR carries no producer identity / parse status |
| (top-level) | schema_id/schema_hash | INVENTION | parser-IR requires schema_id+schema_hash; AAT supplies only version=1; producer must hardcode ABC's identifier |
| (none) | errors[] | INVENTION | parser-IR requires errors[]; AAT has no errors concept -> defaulted empty |
