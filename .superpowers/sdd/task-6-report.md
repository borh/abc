# Task 6 report

## Scope and permanent boundaries

- Migrated ordinary publication-build, design-validation, and containment filesystem operations to `babashka.fs`.
- Removed exactly `abc.tools.path-containment`, `abc.tools.soranoha-build-publication`, `abc.tools.source-bundle`, and `abc.tools.validate-design-bundle` from the grandfather set.
- Retained atomic publication promotion with `Files/move`, containment resolution with `toRealPath`, and the source-bundle `Files/createTempFile`/`Files/copy`/`Files/deleteIfExists` staged archive unit.
- Retained source-bundle ZIP/Java validation APIs; temporary paths cross to Java consumers through `fs/file`.

## Old-code golden evidence

- Before production migration, the five exact golden vars passed: 5 tests, 27 assertions, 0 failures.
- Coverage pins publication root decisions/promotion/reuse/flat copy, design temporary-directory cleanup on success and failure, containment missing/inside/symlink escape, and source archive staged-file cleanup on success and failure.

## New-code evidence

- Exact goldens plus filesystem policy: 14 tests, 75 assertions, 0 failures.
- Full `abc.tools.soranoha-test`: 38 tests, 349 assertions, 0 failures.
- Direct four-namespace Kaocha run distinguishes the direct environment: the schema-backed TEI vars error because `TEI_SCHEMA_PATH` is unset. After correcting a migrated traversal's `Path`/`File` result type, the complete Soranoha namespace is green.
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` ran 904 tests and 4054 assertions with 0 failures, but the derivation failed on the unrelated `abc.tools.tei-cache-test/missing-schema-still-fails-loudly-test` (`NoSuchFileException: no/such/schema.rng`).
- Focused policy sensitivity proves every retained permanent entry is live; exception rationale strings are nonblank.

## Self-review

- Directory traversal preserves `File` results at existing ZIP/Java consumers and stable sorting for flat publication copies.
- Both `fs/with-temp-dir` conversions preserve cleanup dynamic extent on normal return and exceptions.
- The source-bundle staging implementation is byte-for-byte untouched.
- `task-3-report.md` was not staged or modified by Task 6.

## Concerns

- The focused Nix derivation has one unrelated TEI cache error described above; all Task 6 focused suites and policy checks are green.
