# Parser Evidence Citation Contract

Status: provisional / ADR 0002 unblocker
Date: 2026-07-04

## Purpose

ADR 0002 is still Draft because ABC has not defined how producer-side
`../ab-validator` measurements become ABC parser-selection evidence. ABC now
has strong downstream compatibility evidence, but parser selection and
parser-IR conversion compatibility are not the same decision.

This handoff defines the narrow citation contract needed before ADR 0002 can
move from "candidate criteria" to an accepted evidence policy.

## Monorepo Boundary

The planned monorepo migration removes the physical `../ab-validator` checkout
boundary, not the logical boundary between producer measurement and ABC
publication admission. ABC should therefore record evidence by logical
workspace-relative component paths such as
`ab-validator/docs/superpowers/reports/...`, with current `../ab-validator/...`
paths treated only as temporary locators.

The first machine-readable index for that policy is
`data/parser-evidence-citations.edn`. It is validated by
`nix run .#validate-design-bundle` and keeps report hashes next to their
logical component paths.

Monorepo component and naming policy is tracked in
`docs/handoffs/monorepo-component-boundaries.md`. A future Soranoha rename must
not change evidence identity semantics unless a separate rename/vocabulary ADR
also defines aliasing for historical paths, rule IDs, and vocabulary IRIs.

## Current Evidence

| Evidence | Logical path | SHA-256 | What ABC may cite |
|---|---|---|---|
| Parser-IR conversion sync | `ab-validator/docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md` | `sha256:a8f27f56ffceaacbbcde1d80d823b8e5727186790d6ebc841a45966f1db1761c` | Verdict `PARSER_IR_CONVERTER_CORPUS_CLEAN`, mapping/version/hash, admitted adapter tuples, caveats |
| Full-corpus conversion audit | `ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md` | `sha256:d03afdce19adef4c5a44d7a8caa628fa3b8cc8fc0f0599d047aed261cc9ad7a7` | 53,427 attempted, 53,427 succeeded, 0 failed; per-adapter conversion counts; divergence categories |
| ABC compatibility candidates | `ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn` | `sha256:c00481ab65239098a072112dd8c803f0203c6d5776d67864215491715a9616e6` | Exact adapter/version registry candidates for ABC admission |
| Parser performance measurement | `ab-validator/docs/superpowers/reports/2026-07-04-parser-performance-measurement.md` | `sha256:4603fc0f3f175dc2524e0ac1b4f312df020cecfda5728cc8fbfe04f0337d49cd` | Bounded parser timing evidence and DNF policy, especially largest-five comparison |
| Coverage report | `ab-validator/docs/coverage-report.md` | `sha256:f52e2347c1ba859eb9b2227812e17b3f720ff271d98b144b477ed3adfcb56c7c` | Parser recognition and AAT-fidelity feature matrix |
| Adapter fidelity matrix | `ab-validator/docs/adapter-fidelity.md` | `sha256:39bc788b8e81b4972936b6ceb435c07e9707e2030894ca1f39811eb0ddd2e819` | Direct vs indirect adapter-fidelity caveats |

## Evidence Classes

ABC should cite producer evidence in three separate classes:

1. **Compatibility evidence**: proves a measured adapter/version/mapping tuple
   can produce parser-IR that ABC admits and can publish. This is now strong
   for:
   - `aozora-epub3`, adapter version
     `aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21`;
   - `aozora-rs`, adapter version `aozora-rs-adapter 0.1.0 2b4e8d1`;
   - `aozora2html`, adapter version `aozora2html-adapter 0.1.0 gem-3.0.1`;
   - mapping version `0.2.1`;
   - mapping hash
     `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`.
2. **Parser-selection evidence**: compares parser candidates against ADR 0002
   criteria: syntax coverage, fatal failures, structured diagnostics, span
   coverage, performance, packaging, license, and reversibility. This is not
   yet an accepted ABC contract.
3. **Comparator/oracle evidence**: uses rendered XHTML, source-feature
   observations, or adapter-specific residual reports to explain parser
   disagreement. This evidence is valuable for the paper, but it should not be
   mistaken for selecting a production parser.

## Citation Rules

An ABC parser-evidence citation should record:

- report path relative to `../ab-validator`;
- logical workspace-relative path;
- report SHA-256;
- adapter name;
- exact adapter version string, with no wildcards;
- AAT version;
- mapping id, version, mapping hash, and mapping schema hash;
- target parser-IR schema id and schema hash;
- corpus label and work/file count;
- success/failure counts;
- unsupported/divergence counts when relevant;
- explicit caveats, especially indirect adapter paths and lower-bound
  residuals.

ABC should never cite only a prose verdict when a logical path, report hash,
and identity tuple are available.

## Mapping Version Transition

ABC now admits the `0.2.1` producer evidence as a new mapping identity. The
older `0.2.0` citations and registry rows remain historical admitted evidence;
they were not mutated in place.

Current paper-demo mapping identity:

- mapping version `0.2.1`;
- mapping hash
  `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`;
- three exact adapter-version conversion-audit candidates:
  `aozora-epub3`, `aozora-rs`, and `aozora2html`.

A future mapping version still needs the same refresh gate: fresh report hashes,
fresh compatibility candidates, new registry rows, parser-evidence entries, and
regenerated paper-demo manifests before ABC cites it as accepted evidence.

## Current Interpretation

The July 4 conversion evidence is enough to support ABC's parser-IR
publication boundary and the paper demo chain:

```text
ab-validator measured adapter output
  -> ABC compatibility registry admission
  -> parser-IR publication rendering
  -> plaintext / TEI artifacts with manifests and validation sidecars
```

It is not enough, by itself, to accept ADR 0002 as "parser selected." The
remaining parser-selection question is whether ABC wants:

- `aozora-rs` as the primary direct parser path, with known feature gaps made
  visible as diagnostics or divergence records;
- `aozora2html` as a rendered-output comparator/reference path, not a direct
  source-structure parser;
- another candidate path, such as `aozora2` or future Rust parser work, to
  remain in the serious-candidate set until a smoke-corpus candidate report is
  produced.

## ADR 0002 Acceptance Slice

The smallest acceptable ADR 0002 follow-up is not a new parser implementation.
It is an evidence-policy update that:

1. names this handoff as the citation contract;
2. accepts conversion-audit reports as downstream compatibility evidence;
3. requires a parser-candidate report, with report hash, for parser selection;
4. explicitly classifies `aozora2html` as an indirect rendered-output adapter
   unless a separate source-structure oracle claim is proven;
5. keeps adapter-version matching exact;
6. states that TEI generation is downstream publication rendering and does not
   select a parser by itself.

## Not Yet Settled

- Whether `aozora-rs` is accepted as the primary direct parser candidate for
  v0 publication builds.
- Whether `aozora2` needs a fresh candidate report before ADR 0002 acceptance.
- How much smoke-corpus parser evidence is enough when full-corpus conversion
  evidence is already corpus-clean.
- Whether parser candidate reports should be imported into ABC as data files,
  cited by hash only, or both.

## Next Work

1. Update ADR 0002 with this evidence-policy boundary.
2. Promote `data/parser-evidence-citations.edn` from provisional index to the
   accepted ADR 0002 citation index when the parser-selection policy is
   accepted.
3. For the paper, cite compatibility evidence and caveats, not parser
   selection finality.
