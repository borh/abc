# Final traversal fix report

## Scope

Fixed every traversal finding from the final review:

- `source-snapshot-workset` follows directory symlinks and treats failed directory listings as empty.
- `soranoha-layout-report` and `schema` include files below directory symlinks.
- `soranoha`, `soranoha-build-publication`, and the source workset treat `fs/list-dir` I/O/security failures like the former `File.listFiles` `nil` result.
- Traversal remains explicitly sorted and keeps the former cycle behavior (no cycle pruning).

The fix is site-local `tree-seq` plus `babashka.fs/list-dir`; no shared filesystem wrapper or injectable seam was introduced.

## Evidence

### Old behavior

A disposable worktree at migration base `abc41632` ran direct executable assertions against the old production functions. All six assertions passed:

- layout follows directory symlinks
- schema discovery follows directory symlinks
- workset follows directory symlinks
- archive traversal treats an unlistable directory as empty
- publication ZIP traversal treats an unlistable directory as empty
- workset traversal treats an unlistable directory as empty

The disposable worktree and proof script were removed after the run.

### Current branch red

The new tests were run before production edits:

```text
65 tests, 441 assertions, 3 errors, 2 failures
```

The two failures were linked-directory omissions in schema and layout traversal. The three errors were the source-workset linked-directory omission and `AccessDeniedException` from source-workset/archive traversal. (The archive test initially also exposed a Path-vs-File test-fixture write error; the fixture was corrected before green verification.)

### Current branch green

Focused affected suites:

```text
65 tests, 442 assertions, 0 failures
```

Policy:

```text
9 tests, 48 assertions, 0 failures
```

Repository gates:

- `nix build ./abc#checks.x86_64-linux.clj-kondo` — passed
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests` — passed
- `just validate-migration` — passed, including formatting, Python quality, flake checks, and ABC/validator evaluation

## Concerns

None. The deliberately inherited `file-seq` cycle behavior means a directory-symlink cycle can still recurse indefinitely; preserving rather than silently pruning that behavior is intentional for this consistency refactor.
