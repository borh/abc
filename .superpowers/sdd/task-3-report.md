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

The committed CLI argument handling was corrected so materialization does not
spuriously require an execution inventory. A test-first canonical-index
materializer now consumes `ab-index` order, safely reads plain files or exact
ZIP members, hashes extracted bytes, and supports `--limit` only for explicit
smoke runs (`0` means the entire inventory). RED was the missing API; GREEN is
five focused tests plus ruff and strict mypy.

Real smoke execution used the pinned notation derivation and the canonical
17,886-work index (`corpus_hash`
`sha256:398f092d8aef466ae1b24706545347aafe8533f287c1a9757defde518ba86fb8`).
The first four frozen vectors and first four indexed corpus works each produced
one hash-verified success outcome through the pinned `aozora2` adapted binary.
These temporary smoke artifacts were verified outside the checkout and are not
presented as full-study results.

Before full execution, review found three resume/safety gaps. The executor now
hashes the exact argument vector and declared environment into every outcome
and manifest, refuses resume across an identity change, starts each candidate
in a new session, kills the entire process group on timeout, and resolves every
read artifact beneath the output root while rejecting symlink escapes. RED
proved that a changed command incorrectly reused successes and that an output
symlink escaped containment. Six focused tests, ruff, and strict mypy pass.

The canonical corpus materializer is now Rust and reuses `ab-check`'s exact
plain/ZIP-member byte reader. This avoids Python ZIP central-directory and CRC
differences without changing selected works. Its focused integration test
checks index order, extracted bytes, and inventory output; cargo test and
clippy pass. Applied to the pinned index it materialized all 17,886 selected
works, including malformed archives that blocked Python extraction.

The first full launch correctly failed closed because `ab-index` can contain
multiple primary source members with the same work ID. The materializer now
uses the established `ab-check` identity `work_id` plus the first twelve hex
digits of the indexed-path SHA-256. This retains every selected member without
collision or reordering; the focused test pins the composite identity.

The `aozora-rs` native lane now has a dedicated stdin runner. It decodes input
and directly calls the pinned core's `tokenize`, `scopenize`, and `retokenize`
APIs over the entire text, then emits the native retokenized stream. It does
not call adapter body selection, fallback, mapping, or AAT projection. RED was
the missing native function; its focused test proves ruby recognition and
retention of header text. The adapter-crate tests, formatter, and clippy pass.

Interrupted full lanes exposed that the final manifest was the only resume
index even though per-item outcomes were already durable. Resume now scans
content-addressed outcome files when no manifest exists, reusing a row only if
its embedded execution identity matches. The final completeness and artifact
hash verifier remains mandatory. RED showed identical execution rewrote rows
after deleting the manifest; GREEN preserves them byte-for-byte.

## Completed preregistered execution

All ten included parser/mode lanes were executed independently over all 127
frozen notation vectors and all 17,886 pinned corpus items. Every one of the 20
external manifests now passes a fresh verifier run that checks ordered
completeness plus every outcome, stdout, stderr, source, and manifest-entry
hash. The compact checked manifest records the frozen protocol, corpus,
inventory, parser, adapter, and execution identities without committing
corpus-scale artifacts or machine-local paths.

Corpus outcomes are exact: `aozora` native 17,885 success/1 failure and adapted
17,886 success; `aozora2` native 17,886 success and adapted 17,874 success/12
timeouts; both `aozora-rs` lanes and both `aozora2html` lanes have 17,886
success; `aozora-epub3` native has 17,836 success/50 failures and adapted has
17,773 success/113 failures. Vector lanes have 127 successes except adapted
`aozora-epub3`, which has 123 successes/4 failures.

The first aozora2html manifests detected outcome-hash mismatches caused by
workers surviving an earlier overlapping launch. Neither manifest was
accepted. After all overlapping executors exited, one isolated resume pass per
lane rewrote only mismatches; both complete manifests then passed independent
verification. This demonstrates that the executor failed closed on corrupted
run identity rather than silently accepting the row counts.

## Final evidence-verifier closure

The compact summary is now canonical generated data with a checked verifier.
An explicit untracked resolver maps logical artifact roots to local evidence
and supplies each exact command/environment preimage. Verification recomputes
execution identities, proves candidate/mode command bindings, runs the full
per-item artifact verifier, checks exact counts and manifest hashes, and
requires byte-identical checked-summary regeneration.

The preregistered immediately-before-run host fields were not captured for the
original lanes. They are classified as unavailable, making performance host
comparability unavailable without weakening behavioral artifact integrity.
No post-hoc host state was substituted. The exact original `aozora-rs` native
command preimage was also unrecoverable, so that one fast vector/corpus lane
was rerun into separate replacement roots after capturing all required host
fields. The superseded roots remain preserved.

Different vector/corpus execution hashes for adapted `aozora2` and
`aozora-rs` reflect different Nix store paths from separate builds of the same
pinned source revisions. Because concrete command paths are intentionally part
of execution identity, the divergence is expected and independently
hash-matched rather than normalized away. The aozora2html repair audit records
the recoverable overlap and final-manifest facts while explicitly marking the
unpreserved rejected bytes and mismatched-ID list unavailable.

## Exact frozen-binding closure

The verifier no longer treats any frozen field in the checked summary as an
input assertion. It consumes the preregistration bytes, both materialized
inventory files, content-addressed inventory identities, exact external run
roots, command/environment preimages, and executable materialization
attestations. It independently derives study ID, protocol hash, timeout,
candidate parser/adapter revisions, corpus/vector revisions, inventory hashes
and counts, corpus hash, execution and external manifest hashes, and outcome
counts before byte-comparing the complete regenerated summary.

Materialization attestations bind each execution to candidate/mode, frozen
source revisions, executable byte hash, and the Nix store derivation identity
or content-addressed workspace build. Mutation-negative tests cover protocol,
study ID, timeout, candidate revision, and inventory content/hash/count. The
full 20-lane external verification remains the acceptance gate.
