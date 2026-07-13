# Task 5 report

## Status

Complete. The six Soranoha orchestration/staging namespaces now use
`babashka.fs` for ordinary filesystem predicates, creation, traversal, names,
parents, sizes, and relativization. The filesystem-policy grandfather set was
reduced by exactly those six namespaces.

## Characterization and compatibility

- Existing focused suites characterized snapshot-root selection, optional
  warnings/run summaries, recursive archive selection and ordering, annotation
  discovery, ADR repository errors/order, corpus directory validation, and
  subprocess fixture discovery.
- Added direct staging coverage for replacement of stale output, verbatim
  run-summary copying, and absence of the optional run summary.
- Preserved `java.io.File` at downstream JSON, manifest, process, and public
  return-value boundaries. CLI and exception-data paths remain strings at their
  established boundaries.
- Recursive `.tar.zst` selection remains explicitly sorted by path.

## Verification

- Symlink traversal golden test at parent `bfdf4167`: 1 test, 1 assertion,
  0 failures (disposable detached worktree).
- The same test before the blocker fix at `8c6af4b3`: 1 failure; only
  `z.tar.zst` was found and `linked/a.tar.zst` / `linked/b.tar.zst` were
  omitted.
- The same test after replacing `fs/glob` with direct recursive
  `babashka.fs` traversal: 1 test, 1 assertion, 0 failures. Directory symlinks
  are followed, cycles are not pruned, traversal errors are not suppressed,
  and archive results retain explicit path sorting.
- Post-fix `abc.tools.soranoha-test` plus
  `abc.tools.filesystem-policy-test`: 46 tests, 395 assertions, 0 failures.
- Focused Task 5 suites plus `abc.tools.filesystem-policy-test`: 115 tests,
  604 assertions, 0 failures.
- `nix build ./abc#checks.x86_64-linux.clj-kondo`: passed.
- `git diff --check`: passed.
- No project-local nREPL server was available; Kaocha and the Nix Clojure check
  provided compile/load verification.

## Self-review

Reviewed the patch for sort-key preservation, optional-file semantics,
filesystem consumer types, path-string compatibility, and policy exactness.
The unrelated tracked `.superpowers/sdd/task-3-report.md` modification was not
edited or staged.

---

# ADR Evidence Migration Task 5: Direct Foundation Assertions

## Status

Implemented and verified. No Task 6 corpus/ADR migration work or evidence-protocol changes were made.

## TDD evidence

RED command:

```sh
cd abc && bin/kaocha \
  --focus abc.tools.foundation-evidence-test \
  --focus abc.tools.materialize-import-test \
  --focus abc.tools.validate-design-bundle-test/evidence-input-catalog-equals-the-pure-schema-validation-read-set-test \
  --focus abc.sim.divergences-test
```

Result: exit 1, `1 tests, 1 assertions, 1 errors, 0 failures`; test loading failed on the intentionally missing `validate/evidence-input-paths` public boundary.

After the review fixes, the combined Task 5 boundary run passed with
`24 tests, 63 assertions, 0 failures`.

Final plan-specified command:

```sh
cd abc && bin/kaocha \
  --focus abc.tools.foundation-evidence-test \
  --focus abc.tools.materialize-import-test \
  --focus abc.sim.divergences-test
```

Result after the review fixes: `23 tests, 62 assertions, 0 failures`.

Complete design-bundle namespace with the pinned upstream TEI schema:

```sh
nix develop ./abc#default -c bash -lc \
  'cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test'
```

Result: `68 tests, 232 assertions, 0 failures`.

## Assertions and boundaries

- Both committed manifest examples conform to `manifest.schema.json`.
- The failure fixture has exact failure kind/status, null content, the exact
  errors sidecar, the complete non-null identity-coordinate map including
  `manifest_schema_hash`, null for every other coordinate, and only a
  top-level `artifact_id`. An extra non-null `tokenizer_build_hash`
  counterexample is rejected.
- Fresh materialization returns exactly `#{:parser-ir :warnings}` and both
  manifests conform to the manifest schema. A missing-warnings counterexample
  is rejected.
- The shell wrapper is exactly delegation/setup. The workflow parser proves
  that checkout precedes the Nix design-bundle step in the same job and that
  the command runs from repository root; a synthetic split-job workflow is
  rejected.
- D7 is structurally `:fixed`, dated `2026-07-12`, and names the separated identity roles.
- Diagnostic schema identity requires exact-current equality independently of registered parser-IR compatibility.
- `evidence-input-paths` exactly equals the traced repository read set of the supported pure schema-validation path.

## Additional verification

- `nix run ./abc#validate-design-bundle`: exit 0; all schema/fixture checks passed.
- `nix build ./abc#checks.<current-system>.clj-kondo --print-build-logs`: exit 0, 0 errors, all source files formatted correctly. The check reports the same nine pre-existing repository warnings.
- `clj-paren-repair` on all five modified Clojure files: no changes needed after formatting the new namespace.
- `git diff --check`: exit 0.

## Files

- `abc/test/abc/tools/foundation_evidence_test.clj`
- `abc/src/abc/tools/validate_design_bundle.clj`
- `abc/test/abc/tools/validate_design_bundle_test.clj`
- `abc/test/abc/tools/materialize_import_test.clj`
- `abc/test/abc/sim/divergences_test.clj`

## Concerns

None known.

## Review-fix TDD evidence

The new negative assertions were first loaded before their narrow predicates
existed. Focused Kaocha failed at compile time on the intentionally unresolved
`failure-coordinate-errors` symbol (`1 tests, 1 assertions, 1 errors, 0
failures`). After implementing the deterministic generated-key,
identity-coordinate, and job/step workflow predicates, the split
`abc.tools.foundation-evidence-test` namespace passed with `8 tests, 17
assertions, 0 failures`.

Schema conformance, failure semantics, failure coordinates, top-level
`artifact_id`, generated output set/conformance, wrapper delegation, and CI
wiring now have distinct stable `deftest` Vars. Task 7's plan references these
Vars directly and forbids aggregate wrapper tests around them.
