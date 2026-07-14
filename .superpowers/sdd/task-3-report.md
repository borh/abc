# Task 3 Report — Existing-parser measurement attempt

## Status

Partially implemented with explicit failure evidence. The pinned corpus and
four of five included parser/adapter pairs materialized through flake outputs.
No corpus-scale parser result is claimed: the neutral runner needed to capture
native and adapted per-work outputs is absent, and the historical `aozora`
parser/adapter pins are not exposed by the current flake.

## TDD evidence

Manifest tests were written first. RED was observed as an unresolved
`RawRunManifest` import. A self-review regression then failed compilation on
missing candidate/mode accessors before the exact included-lane set was
checkable. GREEN validates required parser, adapter, corpus, environment,
timeout, protocol, failure, and raw-artifact fields; measured runs cannot have
an empty artifact list.

## Materialization evidence

One Nix invocation materialized the current pinned parsers for `aozora2`,
`aozora-rs`, `aozora2html`, and `aozora-epub3`, plus their four current pinned
adapters. It exited 0. The pinned notation-vector and Aozora Bunko corpus
derivations also built successfully and their NAR hashes are recorded in the
checked identity inputs.

The preregistered legacy `aozora` parser revision and its historical adapter
revision are not current flake outputs. They are retained as build-failure
lanes; a current adapter was not substituted.

## Raw-run evidence

The checked bundle contains exactly ten unique lanes: native and
adapter-normalized for each included existing parser. All carry the frozen
300-second timeout and content hashes for protocol, composite corpus, and host
environment. The legacy lanes are build failures. The other eight are explicit
corpus-execution failures because no conforming full-corpus neutral runner
exists. Their raw artifact arrays are empty by contract; no historical `/db`
dump, untracked reference, current custom-parser output, or fabricated result
was used.

Corpus-scale artifacts remain out of source. The run directory documents the
reproducible Nix materialization command and the requirement that a future
runner hash every external per-work artifact before committing its manifest.

## Verification

Focused verification performed during implementation:

```text
cargo test -p ab-parser-study-report
14 passed; 0 failed (before the final self-review regression)

cargo clippy -p ab-parser-study-report --all-targets -- -D warnings
pass

nix build [eight pinned parser/adapter outputs] --no-link --print-build-logs
pass

nix build ./ab-validator#aozorabunko-corpus \
  ./ab-validator#upstream-aozora-notation-spec --no-link
pass
```

Fresh final verification and its exact counts are recorded in the task handoff
after the final formatting and repository checks.

## Self-review

- The preregistration and fixture manifests were not modified.
- The Task 2 result schema was not modified.
- Native/adapted lanes are distinct and the checked set is exact, not merely a
  row count.
- Failure rows cannot be mistaken for measurements and contain no imputed
  counts or outputs.
- Raw artifact coordinates reject absolute paths and traversal.
- The pre-existing Task 2 report worktree change is not part of this task.

## Concerns

Task 3 is not substantively complete: eight successful builds still lack
per-work native/adapted executions, and the legacy pair lacks flake
materialization. Completing it requires a frozen-protocol runner and a flake
input for the historical adapter source, followed by the actual pinned-corpus
run. The checked failures make that gap reviewable without overstating results.

## Critical review correction

The initial bundle incorrectly represented absence of an execution harness as
candidate `run-failure` evidence. Those rows and their hand-authored identity
files have been deleted. An unattempted parser process is not a parser result.

The raw-manifest contract now requires `attempted: true`; this means the pinned
candidate process was actually started (or its pinned build derivation was
actually evaluated for build-failure evidence). A harness or precondition gap
cannot deserialize as a run. Raw output coordinates also reject POSIX absolute,
Windows drive-qualified, root-relative/backslash, UNC, empty-segment, dot, and
traversal spellings before any host filesystem interpretation.

RED was observed for both corrections: an attempted candidate failure was
initially rejected because the field did not exist, while
`C:\\tmp\\out.json` was initially accepted. The focused tests passed after the
contract changes.

No replacement run bundle is committed. The exact pinned source corpus is
corpus-scale (historically 17,810 valid works), requiring roughly 178,000
independent candidate/mode executions before registered performance
repetitions. There is no checked neutral executor that can first prove exact
inventory selection, per-item completeness/uniqueness, process-group timeout,
and raw byte/hash verification. Producing results in this session would either
skip required validation or exceed the available execution envelope. This is
the explicit corpus-scale blocker; Task 3 remains incomplete rather than
converting the blocker into candidate evidence.

## Neutral executor implementation

Follow-up work added `reports/parser-study/neutral_executor.py`, a checked
executor independent of result aggregation. It deterministically materializes
every frozen notation vector's `source` field in vector-name order, validates
closed inventory rows and source hashes, executes one candidate command per
item with bounded concurrency and per-item timeout, and writes stdout, stderr,
and the outcome itself with SHA-256 identities. Resume accepts an outcome only
after verifying its recorded bytes; the final verifier requires exactly one
ordered outcome per inventory item and rechecks all source/output hashes.

The executor test was written before the module and initially failed at import.
A second RED showed the vector materializer API was absent. Four focused tests
now cover success/resume, malformed manifests, cross-host paths, and sorted
complete vector materialization. This commit contains executor code and tests
only; it does not contain or claim parser measurements.
