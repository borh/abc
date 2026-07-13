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
