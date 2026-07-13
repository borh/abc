# Soranoha babashka.fs Consistency Design

## Status

Approved for detailed planning on 2026-07-13.

## Problem

ABC already depends directly on `babashka/fs` 0.5.34 and uses it successfully in
`abc.tools.files`, request-set resolution, validation, and presentation tooling.
Production code nevertheless performs ordinary filesystem work through a mixture of
`java.io.File`, `clojure.java.io/file`, `file-seq`, and direct `java.nio.file.Files`
calls. The mixture obscures semantics, repeats missing-directory and parent-directory
handling, and makes new code likely to copy whichever local idiom it encounters.

This is a consistency refactor. It must not change externally observable paths,
ordering, overwrite behavior, symlink behavior, errors, file formats, or return types
that callers depend on.

## Decision

Use `babashka.fs` directly for ordinary production filesystem operations and path
manipulation. Do not grow `abc.tools.files` into a one-function-per-operation facade.
Keep `abc.tools.files` only for ABC-specific compound operations such as repository
paths, JSON-line reading, normalized relative paths, recursive deletion, copy-with-
parent-creation, and hashing.

Migrate all safe production sites. Retain `clojure.java.io/file` only where a Java API
requires `java.io.File`, or where preserving an existing public return type requires
it. Retain direct NIO or library-specific filesystem calls where their semantics are
the point of the code rather than incidental plumbing.

## Operation policy

Ordinary operations use these `babashka.fs` concepts:

| Existing operation | Preferred operation | Contract to preserve |
| --- | --- | --- |
| `.exists` | `fs/exists?` | False for a missing path; real access failures still surface |
| `.isFile` | `fs/regular-file?` | Existing symlink-following behavior must be characterized |
| `.isDirectory` | `fs/directory?` | Existing symlink-following behavior must be characterized |
| `.mkdirs` | `fs/create-dirs` | Idempotent creation, including existing directories |
| `.listFiles` | `fs/list-dir` | Missing-directory behavior and filtering are preserved explicitly |
| `file-seq` | `fs/glob`, `fs/walk-file-tree`, or a small local traversal | Recursive scope, symlink policy, and deterministic ordering remain unchanged |
| `.getCanonicalFile` / `.getCanonicalPath` | `fs/canonicalize` | Resolution behavior and string/File return shape remain unchanged at the boundary |
| `.getAbsolutePath` / `.toAbsolutePath` | `fs/absolutize` | Normalization is added only where it already existed |
| `Path.relativize` | `fs/relativize` | Separator normalization remains owned by existing helpers |
| ordinary copy/delete/move | `fs/copy`, `fs/delete`, `fs/delete-tree`, `fs/move` | Replacement, recursive, and missing-target behavior are explicit |
| temporary files/directories | `fs/create-temp-file`, `fs/with-temp-dir` where equivalent | Cleanup timing and failure behavior remain unchanged |

Path composition may continue to use `io/file` at a Java `File` boundary. Elsewhere,
new or migrated path manipulation uses `fs/path`, and conversion to `fs/file` happens
only at the consumer boundary.

## Deliberate low-level exceptions

The migration must not mechanically replace these operations:

- Atomic publication using `Files/move` with `ATOMIC_MOVE` and
  `REPLACE_EXISTING`, including temporary-file cleanup.
- POSIX permission reads/writes and other `Files` APIs with no equivalent contract in
  the selected `babashka.fs` version.
- Real-path containment and symlink escape checks in `path_containment.clj`; ordinary
  predicates inside that namespace may migrate, but the `toRealPath` security
  boundary remains explicit NIO.
- ZIP, TAR, XML, RDF, Schematron, `StreamSource`, and other Java/library APIs that
  require a `java.io.File` or stream.
- Byte-level reads where a Java consumer requires a byte array and replacement would
  obscure the contract.
- JGit path and repository APIs.

Every remaining direct `Files` mutation or `java.io.File` filesystem operation must
have one of these concrete reasons. Compatibility alone is not a reason to preserve an
ordinary operation.

## Migration batches

The implementation is split by behavior so each commit is reviewable and reversible.

1. Predicates and directory creation: migrate existence/type checks and `mkdirs`, with
   characterization for missing paths, symlinked files/directories, and idempotence.
2. Directory listing and recursive traversal: migrate `listFiles` and `file-seq`,
   preserving missing-directory results, filters, relative paths, and explicit sorting.
3. Path manipulation: migrate canonicalization, absolutization, parents, filenames,
   and relativization where no Java boundary requires a `File`.
4. Mutation and temporary paths: migrate ordinary copies, non-atomic moves, deletion,
   and equivalent temporary-path operations. Atomic/permission-sensitive sites remain
   documented exceptions.
5. Enforcement and cleanup: remove unused imports, add the production source policy,
   and verify the complete repository.

The batches may touch the same namespace. They are organized around contracts rather
than around a promise that each file changes only once.

## Behavioral contracts

### Representation

Internal migrated paths are `java.nio.file.Path` values returned by `babashka.fs`.
Functions that currently expose `java.io.File` continue to expose it unless all callers
are migrated in the same reviewed batch and the type is not part of an external seam.
Strings written into manifests, logs, CLI output, and exception data remain byte-for-
byte equivalent, including separator normalization already provided by
`abc.tools.files/relative-path`.

### Traversal and ordering

`fs/list-dir` and traversal results are not assumed to be ordered. Any site whose
output is deterministic today retains or gains an explicit sort using the same key.
Sites that deliberately treat a missing directory as empty continue to do so rather
than relying on library defaults. Recursive traversal does not follow symbolic links
unless the existing implementation demonstrably did.

### Errors and mutation

The migration preserves whether a missing path returns false/nil/empty, throws, or is a
no-op at each call site. Copy and move options state replacement behavior explicitly.
Parent creation guards the bare-filename case where `fs/parent` is nil. Cleanup does not
swallow failures that previously surfaced, and a refactor does not introduce silent
best-effort deletion.

### Symlinks and containment

Predicate behavior on symlinks is characterized before replacement. Security-sensitive
containment continues to normalize and resolve real paths with NIO and compare resolved
roots. This refactor must not weaken escape detection by replacing real-path checks with
lexical path normalization.

## Testing strategy

Characterization precedes each behavior batch. Focused tests cover:

- existing, missing, and dangling-symlink predicates;
- directory creation and the bare-filename parent case;
- missing-directory listing behavior;
- traversal inclusion, exclusion, symlink policy, and deterministic order;
- canonical and relative path results;
- copy replacement, deletion of absent paths, recursive deletion, and failure
  propagation;
- atomic and permission-sensitive exception sites remaining unchanged.

The final source-policy test scans production `.clj` and `.cljc` files. It rejects
ordinary direct `java.io.File` filesystem methods (`mkdirs`, `exists`, `isFile`,
`isDirectory`, `listFiles`, `renameTo`, and `delete`) and `file-seq`. It does not reject
`io/file` construction or representation getters needed at Java boundaries. Direct
`java.nio.file.Files` mutation is allowed only in a small, named allowlist of
atomicity-, permission-, containment-, or byte-contract namespaces; the test documents
the rationale beside each exception.

Acceptance evidence is:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
just validate-migration
```

Focused characterization suites and the source-policy test are the primary evidence
for this migration. `just validate-migration` remains broader repository regression
evidence.

## Non-goals

- Replacing `clojure.java.io` streams, readers, writers, or `spit`/`slurp`.
- Hiding `babashka.fs` behind a new generic wrapper namespace.
- Changing public path representations merely for stylistic uniformity.
- Changing archive, XML, RDF, Schematron, JGit, or Java-library integration APIs.
- Altering atomicity, permissions, symlink policy, output ordering, or error contracts.
- Upgrading `babashka.fs` beyond the already pinned 0.5.34 version unless an exact API
  required by this design is absent and the upgrade is reviewed separately.

## Completion criteria

- All safe ordinary production filesystem operations use `babashka.fs` directly or an
  existing ABC-specific compound helper.
- Every remaining low-level operation has a documented semantic reason.
- No new generic filesystem wrapper or fake injection seam is introduced.
- Characterization tests cover the load-bearing missing-path, symlink, ordering,
  replacement, and failure behavior.
- The production source policy and all acceptance gates pass.
