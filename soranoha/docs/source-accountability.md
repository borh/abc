# Source accountability and validation

`source-accountability.json` records lexical occurrences found directly in the
retained source. It binds the original bytes, decoded UTF-8 text and coverage
matrix by hash. Each occurrence retains its exact spelling, decoded byte span,
source region and recognized syntax families. Editorial legend examples remain
visible as front matter. Unrecognized markers and lossy decoding remain explicit.

This scanner runs independently of the parser. Its stage identity binds the
scanner executable and matrix; parser changes do not invalidate its evidence.
The native command is `ab-source-inventory --source source.txt --matrix matrix.toml
--output-json accountability.json`. The publication wrapper supplies
`AB_SOURCE_INVENTORY_BIN` and `AB_AOZORA_SYNTAX_MATRIX`.

Lexical recognition does not establish a correct interpretation. Interpreter
claims describe what the parser believes it understood; missing claims remain
unaccounted. Neither source preservation nor an empty problem list proves semantic
coverage. Source conformance tests exercise expected values and annotation
placement for specific examples. TEI profile validation checks the exported
XML against the profile. These are separate kinds of evidence.

The former runtime source-fidelity report duplicated a limited Aozora grammar in
Clojure. Its removal changes the assurance provided: conformance tests do not
replace source-derived comparisons for every arbitrary edition. The independent
accountability report must not be read as an equivalent per-edition fidelity
certificate or a claim that all source markup is supported.
