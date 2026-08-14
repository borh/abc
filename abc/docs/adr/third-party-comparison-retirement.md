# Retire the Third-Party Parser-Comparison Machinery

Machine-readable record: `docs/adr/decisions.edn`, slug
`third-party-comparison-retirement`. This narrative adds context; no tool
parses it.

## Why now

The comparison question the adapters existed to answer is answered, by the
comparison machinery's own frozen output. The neutral study concluded that
parse-completion does not separate the project parser from the strongest
existing parsers, and that the axes that now matter — spans, diagnostics,
source accountability — are non-comparable because no external parser emits
them natively. `abc.tools.parser-evidence` had already made comparison
evidence structurally unable to admit or release-qualify a parser. All live
measurement moved to the single-parser release-qualification (parser-rq)
instruments. What remained was ~40k lines of unexecuted machinery, gates that
could only pass against frozen summaries (the five-parser completeness verdict
demanded evidence from the retired `aozora` lane, whose crate no longer
exists), and a production wiring default that no longer matched intent.

## The licence

The repository owner withdrew the standing comparison requirement on
2026-08-14, scoped: keep one comprehensive, citable, frozen comparison record
for the JADH 2026 paper. That record was landed before any deletion:

- `ab-validator/docs/studies/aozora-parser-neutral-comparison-2026-07/`
  (report, raw run manifests, per-lane diagnostics — moved out of the tooling
  tree, sha256 pins unchanged)
- `abc/docs/evidence/paper-demo/` (Rashōmon and Melos demo bundles, demo
  trace, tokenizer comparison view)
- the selection/oracle/coverage reports already under `ab-validator/docs/`

## What this does not touch

- Source-authority measurement of the corpus itself (the paper's Table 1
  machinery) — that measures Aozora Bunko, not other parsers.
- The `ab-aat-to-parser-ir` converter and the parser-rq instruments.
- Append-only registry rows naming third-party adapters
  (`abc/data/aat-parser-ir-compatibility.edn`): immutable historical data.
  The writer going away does not take the data with it.
- Published manifests and reports citing `comparison_report_hash`: the field
  becomes optional for new manifests; recorded values stay valid.

## Execution

Q13 pattern (decide → plan → delete), one kind of change per commit; each
deletion commit names this record as its licence.
