# Task 2 Report — Native/Adapter Attribution

## Status

Implemented and verified. The report IR rejects fallback-, mapper-, and
adapter-derived observations labeled as native; carries the required identity,
provenance, exact-count, missingness, and caveat fields; and rejects reports
that omit any frozen candidate × axis pair.

## TDD evidence

RED was observed with:

```text
cargo test -p ab-parser-study-report --test report_contract
error[E0432]: unresolved import `ab_parser_study_report`
```

The focused tests now cover:

- aozora-rs source-lexer fallback cannot be native;
- aozora2 mapper projection cannot be native;
- every frozen candidate × every frozen axis must be represented;
- non-comparable rows preserve explicit missingness and caveats;
- failed rows preserve explicit failure missingness without imputed counts;
- serialized complete reports validate against the result JSON Schema.

## Implementation

- Added workspace crate `ab-parser-study-report`.
- Added typed candidate, axis, measurement mode, provenance stage, status, and
  missingness enums.
- Added validated `ResultRow` construction with parser revision, adapter
  revision, corpus SHA-256, numerator, denominator, missingness, caveats, and
  provenance.
- Added `StudyReport` matrix validation for all 7 frozen candidates × all 9
  frozen axes. Multiple lanes per pair remain possible while absence of a pair
  is rejected.
- Added `schemas/parser-comparison-result.schema.json`, including conditional
  native-attribution, adapter revision, and measured/missing count rules.
- Added the focused Nix check
  `checks.x86_64-linux.parser-study-report`.

## Frozen-contract handling

Task 1 already froze `schemas/parser-comparison-study.schema.json` as the
preregistration schema. Task 2's earlier planning note proposed that same path
for result rows. Mutating it would change the approved Task 1 contract, so the
result IR uses the non-conflicting
`schemas/parser-comparison-result.schema.json`. No Task 1 artifact changed.

## Verification

```text
cargo test -p ab-parser-study-report
5 passed; 0 failed

cargo clippy -p ab-parser-study-report --all-targets -- -D warnings
pass

cargo fmt --all -- --check
pass

nix build .#checks.x86_64-linux.parser-study-report --print-build-logs
pass; 5 focused tests passed in the Nix derivation
```

The first Nix attempt failed because flake source filtering excludes untracked
files. After staging only Task 2 paths, the identical command passed. The
Harmonia post-build hook emitted environment warnings after the successful
derivation; they do not affect the build result.

## Self-review

- Frozen candidate and axis order/identity match the preregistration.
- Native attribution is checked both by Rust validation and JSON Schema.
- Missing results remain rows and cannot carry invented counts.
- Exact counts reject numerator greater than denominator.
- Corpus hashes require canonical lower-case `sha256:` form.
- The result schema has a distinct identity from the frozen preregistration
  schema.
- Unrelated Task 8 worktree changes were not staged or modified.

## Concern / follow-up

The JSON Schema can enforce row shape and attribution but cannot by itself
prove unique coverage of the full candidate × axis matrix. `StudyReport::new`
provides that semantic validation. Any non-Rust report producer must run an
equivalent semantic validator, which should be exposed by the Task 3 runner.

## Review-finding correction

The review identified that derived `Deserialize` and public fields permitted
invalid values after construction, completeness ignored measurement lanes,
and attribution/status rules were weaker than the frozen protocol.

RED was observed after adding review regression tests:

```text
cargo test -p ab-parser-study-report --test report_contract
error[E0599]: no method named `required_modes` found for enum `Candidate`
error[E0599]: no method named `rows` found for struct `StudyReport`
```

The correction adds private immutable fields and read-only accessors, unchecked
serde DTOs converted through validated `TryFrom`, and negative deserialization
tests for row and complete-report inputs. The required matrix is now 108 unique
candidate × axis × mode lanes: both modes for the five included parsers and
native-only lanes for the excluded parser and custom appendix candidate.

Native rows now require `native_parser`, forbid adapter-derived stages, and
forbid an adapter revision. Adapter-normalized rows require both an
adapter-derived stage and non-empty adapter revision. Measured, failed, and
non-comparable statuses each accept only their corresponding missingness/count
states. The JSON Schema mirrors every row-level enforceable rule, restricts the
two native-only dispositions, and fixes the report at 108 rows; Rust semantic
validation additionally proves uniqueness and exact lane identity.

GREEN verification:

```text
cargo test -p ab-parser-study-report
10 passed; 0 failed

nix build ./ab-validator#checks.x86_64-linux.parser-study-report
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
all passed
```
