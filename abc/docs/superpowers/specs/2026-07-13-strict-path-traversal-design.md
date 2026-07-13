# Strict Path Traversal Design

## Status

Approved for detailed planning on 2026-07-13.

## Context

The babashka.fs consistency migration preserved the behavior of
`clojure.java.io/file-seq`: descendant directory symlinks were followed,
directory-listing failures were treated as empty, and symlink cycles were not pruned.
That preservation was appropriate during a uniformity refactor, but it is not an
external compatibility requirement.

Five production sites now repeat the compatibility traversal:

- checked-in schema discovery;
- Soranoha layout reporting;
- source-snapshot work-directory discovery;
- Soranoha staged archive discovery;
- Aozora work ZIP discovery for publication builds.

The duplication makes this security- and correctness-sensitive policy easy to drift.
Following descendant directory symlinks also permits traversal outside the lexical
root and exposes callers to cycles.

## Nix compatibility evidence

The current flake store source contains no child-directory symlinks under `abc/`.
Checked-in schemas are regular store files. Nix may provide an entire traversal root as
a store path or symlink; the new contract permits the root itself to be a symlink and
therefore remains compatible with this layout.

## Decision

Add one compound helper to `abc.tools.files`:

```clojure
(sorted-path-tree root) ; => lazy sequence of java.nio.file.Path
```

This is not a generic filesystem facade. It owns one cohesive traversal policy used by
the five call sites.

### Contract

- Convert `root` to `java.nio.file.Path` once.
- Yield the root first, even when it is missing or is itself a symlink.
- Permit traversal through the root when the root resolves to a directory, including
  when the root itself is a symlink.
- For descendants, yield symbolic links but never descend through a symbolic link whose
  target is a directory.
- Sort children by their normalized string path before recursion.
- Return a lazy depth-first, pre-order sequence.
- Propagate `IOException`, `SecurityException`, and other listing failures.
- Do not resolve real paths, track visited identities, or add cycle detection. Child
  directory links are not followed, so the ordinary symlink-cycle case is removed by
  policy.

File symlinks are yielded. Consumers continue to decide whether a yielded entry counts
as a regular file; existing `fs/regular-file?`, reading, and size behavior therefore
remain at the consumer boundary.

## Implementation shape

The helper uses `fs/path`, `fs/directory?`, `fs/sym-link?`, and sorted `fs/list-dir`.
The recursive child realization remains inside `lazy-seq`, so listing errors surface
when callers realize that part of the tree, consistent with a lazy traversal API.

The five production sites replace their local `tree-seq` blocks with
`files/sorted-path-tree`. Site-specific filtering, relative-path calculation, and final
sorting remain local because they express domain output rather than traversal policy.

Remove comments describing `file-seq` compatibility and remove the duplicated
IOException/SecurityException suppression.

## Intentional behavior changes

This work deliberately changes two behaviors:

1. Files and work directories reachable only beneath a descendant directory symlink are
   no longer discovered.
2. Directory-listing failures now abort the operation instead of silently producing a
   partial result.

These changes are desirable: traversal remains lexically bounded, cycles through
directory links are avoided, and incomplete reports/build inputs cannot masquerade as
complete results.

The root itself may still be a symlink so Nix/store and explicit operator-selected roots
continue to work.

## Testing

Test the helper directly for:

- deterministic depth-first pre-order;
- a missing root being yielded once;
- a symlinked root being traversed;
- descendant directory symlinks being yielded but not traversed;
- file symlinks being yielded;
- directory listing failure propagating.

Update the five consumer characterization tests to assert the new contract:

- linked descendant schemas, archives, ZIPs, and work directories are excluded;
- listing failures surface rather than returning partial results;
- observable consumer outputs retain their explicit deterministic ordering.

The listing-failure test should use a controllable seam at the helper boundary or a
filesystem permission fixture that is proven effective in the Nix test environment. Do
not add dependency injection to production consumers solely for tests.

Acceptance evidence:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
just validate-migration
```

The Nix-focused suite proves checked-in schema discovery still succeeds from the store
source layout.

## Non-goals

- Preserving `file-seq` descendant-link or swallowed-listing-error behavior.
- Adding configurable follow-link or error-suppression flags.
- Resolving real paths or enforcing the separate path-containment security policy.
- Replacing non-recursive `fs/list-dir` call sites.
- Changing consumer-specific file filters, archive formats, report schemas, or relative
  path formats.

## Completion criteria

- All five production consumers use `files/sorted-path-tree`.
- No duplicate compatibility traversal remains.
- Descendant directory links are not traversed and listing failures propagate.
- Root symlink traversal works in focused and Nix-backed tests.
- Consumer output ordering and non-traversal behavior are explicit in tests.
- All acceptance gates pass.
