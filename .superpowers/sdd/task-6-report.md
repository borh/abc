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
- Initial `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` exposed a deterministic Task 2 regression in `abc.tools.tei-cache-test/missing-schema-still-fails-loudly-test`: `fs/last-modified-time` threw before the schema-load error wrapper. Moving cache-key construction inside that wrapper restored the public loud-failure contract.
- `bin/kaocha --focus abc.tools.tei-cache-test`: 4 tests, 31 assertions, 0 failures.
- Fresh `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests`: exit 0.
- Focused policy sensitivity proves every retained permanent entry is live; exception rationale strings are nonblank.

## Self-review

- Directory traversal preserves `File` results at existing ZIP/Java consumers and stable sorting for flat publication copies.
- Both `fs/with-temp-dir` conversions preserve cleanup dynamic extent on normal return and exceptions.
- The source-bundle staging implementation is byte-for-byte untouched.
- `task-3-report.md` was not staged or modified by Task 6.

## Concerns

- None.
