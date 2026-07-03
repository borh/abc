# AAT to Parser-IR Fixture Provenance

These fixtures are minimized, schema-valid AAT documents used by the sandbox-pure
`ab-aat-to-parser-ir` tests. They are intentionally small enough for flake checks
and do not require `/db`.

- `real-aozora-rs-sample.aat.json` represents the measured aozora-rs mapping
  corpus used by ABC's generated candidate:
  `/home/bor/Projects/abc/prototypes/aat-to-parser-ir-probe/mapping.generated.aozora-rs.json`.
  It exercises text, direct ruby direction projection, gaiji raw marker
  invention, gaiji resolved ambiguity, and `jis_code -> gaiji.reference` A-18.
- `real-aozora2html-sample.aat.json` is minimized from the measured
  aozora2html full run:
  `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`, with
  warigaki source-feature evidence from work `000005_53194`. It exercises
  `children[]` traversal, heading policy, windows-31j-lossy encoding, warning
  inventions, style-to-emphasis, and context-sensitive warigaki U-09.

Both fixtures target `data/aat-to-parser-ir-mapping-v1.json` mapping version
`0.1.1`.
