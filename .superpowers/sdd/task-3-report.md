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
