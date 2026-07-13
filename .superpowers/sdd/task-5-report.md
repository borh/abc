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
