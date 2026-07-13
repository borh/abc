# Task 2 report

## Commit

- Implementation: `0a868066c45bed4c705dd665aa4146d9cbd04dfe`

## Tests

- Characterization before migration: `bin/kaocha --focus abc.tools.adr-evidence-capture-test/run-process-captures-working-directory-output-and-status --focus abc.tools.validate-design-bundle-test/run-command-retains-command-and-nonzero-exit-code-test` — 2 tests, 7 assertions, 0 failures.
- Focused contracts after migration: same command — 2 tests, 7 assertions, 0 failures.
- Affected namespaces: `bin/kaocha --focus abc.tools.adr-evidence-capture-test --focus abc.tools.validate-design-bundle-test` — 68 tests, 235 assertions, 0 failures, 2 errors. Both errors are the documented pre-existing `TEI_SCHEMA_PATH` gate (`validate-tei-warning-partition-test` and `validate-tei-smoke-test`).
- Static analysis and Clojure formatting: `nix build ./abc#checks.x86_64-linux.clj-kondo` — passed.
- Whitespace validation: `git diff --check` — passed before commit.

## Concerns

- Direct Kaocha cannot make the whole validation namespace green without `TEI_SCHEMA_PATH`; the two errors were isolated to the known schema-backed tests and the new focused contracts pass independently.
- Filesystem migration and a shared process wrapper were intentionally left out of scope.
