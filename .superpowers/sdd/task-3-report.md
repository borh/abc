# Task 3 report

## Commit

- Implementation: `a85bc42d` (`refactor(abc): run publication adapters with babashka process`)

## Tests

- Characterization before migration: `bin/kaocha --focus abc.tools.soranoha-test/publication-run-process-preserves-bytes-env-and-nonzero-test` — 1 test, 4 assertions, 0 failures. This was an expected passing characterization of the existing contract.
- Focused contract after migration: the same command — 1 test, 4 assertions, 0 failures.
- Affected suites after migration: `bin/kaocha --focus abc.tools.soranoha-test --focus abc.sim.content-sim-test` — 48 tests, 420 assertions, 0 failures.
- Formatting and delimiter validation: `clj-paren-repair src/abc/tools/soranoha_build_publication.clj test/abc/tools/soranoha_test.clj` — 2 files successful, no changes needed.
- Whitespace validation: `git diff --check` — passed before commit.

## Self-review

- `run-process!` uses the required vector-first/options-last `process/process` call.
- `:extra-env` preserves the inherited environment; the characterization proves both the added `ABC_TEST_ENV` value and inherited `HOME` are visible.
- Raw ISO-8859-1 stdin containing NUL and `0xff` round-trips byte-for-byte through stdout.
- Nonzero exit and stderr remain data, and the result retains exactly `#{:exit :out-bytes :err}` by explicitly remapping `:out` to `:out-bytes`.
- Concurrent stdout/stderr draining is the only intentional behavioral improvement.

## Concerns

None.

---

# ADR Evidence Migration Task 3 Report: Historical Evidence and Future Verification Claim-Header Lint

## Status

Implemented and verified. Existing Task 2B ADR read/listing adapters remain unchanged.

## TDD evidence

### RED

Command:

```sh
cd abc && bin/kaocha --focus abc.tools.adr-test
```

Observed result: exit 2, `42 tests, 93 assertions, 2 failures`. Both failures were the new exact-section assertions: no `:claim-header-outside-acceptance` problems were emitted yet for `Historical Evidence` and `Future Verification`. The traced-adapter/default-deny characterization passed during this RED run.

### GREEN

Command:

```sh
cd abc && bin/kaocha --focus abc.tools.adr-test
```

Observed result: exit 0, `42 tests, 93 assertions, 0 failures`.

Adjacent focused ADR regressions:

```sh
cd abc && bin/kaocha --focus abc.tools.adr-test --focus abc.tools.adr-governance-test --focus abc.tools.adr-claim-migration-test
```

Observed result: exit 0, `56 tests, 179 assertions, 0 failures`.

Static/format checks:

- `clj-paren-repair src/abc/tools/adr.clj test/abc/tools/adr_test.clj`: no changes needed.
- `clj-kondo --lint src/abc/tools/adr.clj test/abc/tools/adr_test.clj`: 0 errors, 0 warnings.
- `git diff --check`: exit 0.

## Files

- `abc/src/abc/tools/adr.clj`
- `abc/test/abc/tools/adr_test.clj`

## Behavior

- Scans only exact `Historical Evidence` and `Future Verification` section bodies with the existing typed `claim-header-pattern`.
- Emits `:claim-header-outside-acceptance` with the exact offending `:section`.
- Ordinary bold prose and typed headers in Acceptance Criteria remain unaffected.
- Keeps the new migration lint audit-only, preserving legacy governance behavior.
- Characterizes byte-equivalent parsed ADR values and sorted ADR names under the traced adapters.
- Proves synthetic direct `.listFiles` and `slurp` implementations fail the existing reachable-Var default-deny lint.

## Commit

`9917ec0f feat(adr): lint claims in non-acceptance sections`

## Concerns

None known.

## ADR lint review follow-up

### RED

Command:

```sh
cd abc && bin/kaocha --focus abc.tools.adr-test
```

Observed result: exit 3, `44 tests, 98 assertions, 3 failures`. The failures showed that list-prefixed typed headers were missed, earlier repeated-section occurrences were overwritten, and repeated headings had no parser diagnostic.

### GREEN

Focused command result: exit 0, `44 tests, 98 assertions, 0 failures`.

Final adjacent verification:

```sh
cd abc && bin/kaocha --focus abc.tools.adr-test --focus abc.tools.adr-governance-test --focus abc.tools.adr-claim-migration-test
```

Observed result: exit 0, `58 tests, 184 assertions, 0 failures`.

`clj-kondo --lint src/abc/tools/adr.clj test/abc/tools/adr_test.clj` reported 0 errors and 0 warnings. `clj-paren-repair` reported no changes needed, and `git diff --check` exited 0.

The fix retains every ordered section occurrence internally for linting, preserves the existing string-valued `:section-bodies` contract, emits `:duplicate-section` parser problems, and strips only a valid CommonMark bullet/ordered marker before applying the existing typed claim-header pattern.

## Final scope correction

The generic section-uniqueness policy was removed while ordered occurrence retention was preserved.

RED command `cd abc && bin/kaocha --focus abc.tools.adr-test` exited 2 with `45 tests, 99 assertions, 2 failures`: repeated target headings and repeated ordinary Notes/Consequences were rejected only by the unwanted `:duplicate-section` problems.

GREEN focused result: `45 tests, 99 assertions, 0 failures`. Final adjacent verification (`adr-test`, `adr-governance-test`, and `adr-claim-migration-test`) passed with `59 tests, 185 assertions, 0 failures`. clj-kondo reported 0 errors and 0 warnings; formatting required no changes and `git diff --check` exited 0.

Repeated Historical Evidence/Future Verification bodies remain fully scanned for typed headers. Repeated Notes and Consequences now remain legacy-valid; no heading-uniqueness policy is introduced.
