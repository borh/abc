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
- Outer design-bundle cleanup red: the focused test failed to compile because the real temp-ownership seam did not yet exist.
- Outer and publication-view cleanup vars: 2 tests, 11 assertions, 0 failures; the outer root is absent after both normal return and a thrown validation callback.
- Cleanup vars plus filesystem policy: 11 tests, 59 assertions, 0 failures.
- Fresh full relevant `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests`: exit 0.
- Fresh `nix build ./abc#checks.x86_64-linux.clj-kondo`: exit 0.
- Focused policy sensitivity proves every retained permanent entry is live; exception rationale strings are nonblank.

## Self-review

- Directory traversal preserves `File` results at existing ZIP/Java consumers and stable sorting for flat publication copies.
- Both `fs/with-temp-dir` conversions preserve cleanup dynamic extent on normal return and exceptions.
- `validate-design-bundle!` routes through the tested `with-design-temp-dir` ownership seam, so the outer cleanup test exercises its actual dynamic-extent owner without executing unrelated validation stages.
- The source-bundle staging implementation is byte-for-byte untouched.
- `task-3-report.md` was not staged or modified by Task 6.

## Concerns

- None.

---

# ADR Evidence Migration Task 6: Foundation Stage A

## Result

- Frozen baseline verification passed before edits: 9 tests, 85 assertions.
- RED corpus contract failed with 14 expected lifecycle/claim/ledger failures.
- Six foundation ADRs now expose the approved lifecycle fields and exactly 35
  typed claims; all 35 reviewed baseline rows map to the final claim-ID union.
- The 36 frozen normative sections remain byte-identical under the baseline
  guard.
- Inventory is 146 baseline criteria / 146 live criteria, with exactly 35 in
  `foundation-runtime-identity` and zero unclassified.
- Governance audit exits zero in audit mode. Foundation debt is exactly 35
  `missing-claim-evidence` problems and no other foundation problem kind.

## Verification

- Task 6 focused suites: 63 tests, 222 assertions, 0 failures.
- `nix build ./abc#checks.x86_64-linux.clj-kondo`: passed with zero errors and
  the same nine existing warnings; all Clojure files formatted correctly.
- `git diff --check`: passed.
- `just validate-migration` reached the schema mirror gate and failed on
  `isolated Nix schema mirror drift: adr-evidence-run.schema.json`, between
  `abc/schemas/adr-evidence-run.schema.json` and
  `ab-validator/data/abc-schemas/nix-schemas/adr-evidence-run.schema.json`.
  This is recorded as branch-level integration debt to repair before Task 7;
  Task 6 did not fold the schema-mirror repair into the Stage A claim commit.

## Scope guard

No evidence descriptors, run bundles, registration template, or evidence
registry entries were created or modified. Stage B has not begun.
