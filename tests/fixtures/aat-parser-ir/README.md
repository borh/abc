# AAT to Parser-IR Fixture Provenance

These fixtures are minimized, schema-valid AAT documents used by the sandbox-pure
`ab-aat-to-parser-ir` tests. They are intentionally small enough for flake checks
and do not require `/db`.

- `real-aozora-rs-sample.aat.json` is copied from the checked-in aozora-rs
  adapter output `adapters/aozora-rs/tests/fixtures/ruby_gaiji.aat.json`,
  preserving its real adapter version, source hash, metrics, and semantic
  summary. It exercises text, direct ruby direction projection, ruby
  `base_content` loss, gaiji raw marker invention, and gaiji resolved ambiguity.
- `real-aozora2html-sample.aat.json` is minimized from the measured
  aozora2html full-run AAT file
  `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter/000005_53194-ebb0cbaf64b3.json`,
  preserving real `work_id`, adapter version, source hash, source encoding, and
  one real warning. It exercises `children[]` traversal, style-to-emphasis,
  ruby projection, warning inventions, and measured warigaki U-02.

Both fixtures target `data/aat-to-parser-ir-mapping-v1.json` mapping version
`0.1.1`.
