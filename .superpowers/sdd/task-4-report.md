# Task 4 report

## Scope

Migrated the nine materialization/workflow namespaces to direct `babashka.fs`
0.5.34 usage. Paths remain `Path` values while filesystem logic is internal and
are converted to `File` at manifest, hashing, TAR, JSON, and stream consumers.
The source-workset traversal uses a sorted local `fs/list-dir` tree and
explicitly declines to recurse through symbolic links.

The filesystem policy grandfather set shrank by exactly nine entries:

- `abc.tools.materialize-annotations`
- `abc.tools.materialize-import`
- `abc.tools.materialize-publication`
- `abc.tools.snapshot-index`
- `abc.tools.source-snapshot-workset`
- `abc.tools.materialize-source-snapshot`
- `abc.tools.tar`
- `abc.tools.workflow`
- `abc.tools.workflow.cache`

## Old green

Command (before production edits):

```sh
bin/kaocha --focus abc.tools.materialize-annotations-test --focus abc.tools.materialize-import-test --focus abc.tools.materialize-publication-test --focus abc.tools.snapshot-index-test --focus abc.tools.source-snapshot-workset-test --focus abc.tools.materialize-source-snapshot-test --focus abc.tools.tar-test --focus abc.tools.workflow-test --focus abc.tools.workflow.cache-test
```

Result: **63 tests, 284 assertions, 0 failures**.

## New green

Command (unchanged suites plus policy):

```sh
bin/kaocha --focus abc.tools.materialize-annotations-test --focus abc.tools.materialize-import-test --focus abc.tools.materialize-publication-test --focus abc.tools.snapshot-index-test --focus abc.tools.source-snapshot-workset-test --focus abc.tools.materialize-source-snapshot-test --focus abc.tools.tar-test --focus abc.tools.workflow-test --focus abc.tools.workflow.cache-test --focus abc.tools.filesystem-policy-test
```

Result: **72 tests, 352 assertions, 0 failures**.

## Commit

`refactor(abc): use babashka fs in materialization workflows`

## Concerns

None. TAR entry ordering remains caller-controlled. Recursive workset discovery
is deterministic and does not follow directory symlinks; dangling symlinks are
listed safely but never traversed.

## Review follow-up

Added a regression for nil `:summary-path`. Before the production correction:

```sh
bin/kaocha --focus abc.tools.materialize-publication-test/materialize-publication-batch-without-summary-does-not-write-report-test
```

Result: **1 test, 2 assertions, 2 failures**. The returned summary incorrectly
contained `workflow_run_path`, and a workflow-run write targeted the suite cwd.
After restoring conditional summary-directory construction, the same command
reported **1 test, 2 assertions, 0 failures**.

Affected-suite verification:

```sh
bin/kaocha --focus abc.tools.materialize-publication-test --focus abc.tools.tar-test --focus abc.tools.workflow-test --focus abc.tools.filesystem-policy-test
```

Result: **25 tests, 179 assertions, 0 failures**.

Quality verification:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0. Workflow joins now remain `Path` values internally and convert
to `File` only at the manifest writer. The TAR bare-filename test now uses a
unique file in the real suite cwd and does not mutate `user.dir`.

## Final audit cleanup

Removed the unused batch `summary-file` binding and the now-unused
`clojure.java.io` alias from `materialize-publication`.

```sh
bin/kaocha --focus abc.tools.materialize-publication-test --focus abc.tools.filesystem-policy-test
```

Result: **20 tests, 133 assertions, 0 failures**.

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0.

---

# ADR Evidence Migration Task 4: Hermetic Design Bundle and Synthetic Git-cliff Check

## Status

Implemented and verified. No evidence contracts were changed.

## RED evidence

Before editing, from the monorepo root:

```sh
nix run ./abc#validate-design-bundle
```

Exited 1 after `==> Checking git-cliff configuration` with `could not find repository` at the Nix-store ABC source.

The new focused sentinel test was then run before production changes:

```sh
cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test/design-bundle-does-not-run-repository-history-checks-test
```

Result: `1 tests, 1 assertions, 1 errors, 0 failures`; the error was the expected `repository-history sentinel reached` at the old final Git-cliff stage.

## GREEN evidence

Focused sentinel: `1 tests, 1 assertions, 0 failures`.

Complete namespace in the pinned flake dev-shell environment:

```sh
nix develop ./abc#default -c bash -lc \
  'cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test'
```

Result: `67 tests, 231 assertions, 0 failures`.

The direct local namespace run passed 65 tests and retained its two documented `TEI_SCHEMA_PATH` environment errors; the pinned dev shell supplied the required upstream schema for the complete GREEN run.

Production boundaries:

```sh
bash abc/nix/check-git-cliff-config.sh abc/cliff.toml
nix run ./abc#validate-design-bundle
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build "./abc#checks.${system}.git-cliff-config" --print-build-logs
```

All exited 0. The design-bundle output ended after IIIF with `design bundle validation ok` and contained no Git-cliff stage. The dedicated check generated a nonempty changelog in a synthetic repository.

Quality checks:

- `nix fmt abc/flake.nix`: passed.
- `clj-paren-repair` on the modified Clojure files: no changes needed.
- `clj-kondo --lint src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj`: 0 errors, 0 warnings.
- `git diff --check`: exit 0.

## Files

- `abc/src/abc/tools/validate_design_bundle.clj`
- `abc/test/abc/tools/validate_design_bundle_test.clj`
- `abc/nix/check-git-cliff-config.sh`
- `abc/flake.nix`

## Concerns

None known.
