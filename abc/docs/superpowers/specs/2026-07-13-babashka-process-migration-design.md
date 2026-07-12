# Production subprocess migration to babashka.process — design

Date: 2026-07-13

## Goal

Replace every production use of `clojure.java.shell` and raw
`ProcessBuilder` in `abc/` with `babashka.process` 0.6.25 while preserving
observable behavior. Test fixtures may continue using `ProcessBuilder` to
simulate external programs.

This is a behavior-preserving simplification. Filesystem cleanup, command
behavior changes, and unrelated dependency upgrades are out of scope.

## Scope

The production migration covers:

- `abc.tools.soranoha-build-publication`;
- `abc.tools.annotation-join-stats-run`;
- `abc.tools.validate-design-bundle`;
- `abc.tools.adr-evidence-capture`;
- `abc.tools.workflow.nix-bridge`;
- `abc.tools.aozora-replay`; and
- `abc.tools.source-bundle-report`.

After migration, no tracked production namespace under `abc/src` may import
`clojure.java.shell`, construct a `ProcessBuilder`, or start a process through
`Runtime.exec`. A source-contract test enforces those exact predicates; it must
not reject `Runtime/getRuntime` uses that only query `availableProcessors`.

`abc.tools.diagram.presentation-svg` is existing precedent for direct
`babashka.process/process` use and is already compliant, so it is not a
migration target.

## API selection

Use `babashka.process` directly at each call site. Do not introduce an
`abc.tools.process` wrapper: the present contracts differ materially, so a
single wrapper would merely replace Java APIs with a flag-driven fake seam.

- Use `babashka.process/sh` for blocking execution whose caller consumes the
  familiar captured `{:exit :out :err}` result and handles nonzero status.
- Use `babashka.process/shell` for blocking execution with inherited console
  I/O and throw-on-nonzero behavior.
- Use `babashka.process/process` plus dereference for explicit stdin/stdout
  types, added environment variables, working directories, asynchronous pipe
  draining, or caller-owned nonzero handling.

Commands are passed as argument vectors and never through an operating-system
shell. In babashka.process 0.6.25 the first argument is still tokenized unless
it names an existing path. Therefore every migrated command's first argument
must remain either a space/quote-free executable token or an existing executable
path; later arguments remain distinct vector elements.

## Per-site contracts

### Publication build

The adapter runner must:

- inherit the parent environment and add only its `extra-env` entries;
- accept optional byte-array stdin;
- return stdout as a byte array and stderr as a string;
- drain stdout and stderr concurrently, intentionally removing the current
  sequential-read deadlock risk without changing successful output;
- return nonzero exit status rather than throwing; and
- preserve the current `{:exit :out-bytes :err}` internal result.

The babashka result keys must be explicitly remapped: `:out` becomes
`:out-bytes`, while `:exit` and `:err` retain their names.

Git provenance remains best-effort: command failure or executable-start
failure returns nil from `git-sh` rather than failing publication construction.

### Annotation join statistics

The runner must:

- add its supplied environment entries without replacing the parent
  environment;
- accept optional UTF-8 string stdin;
- drain stdout and stderr without deadlock, including large concurrent output;
- tolerate a broken stdin pipe when a child exits early; and
- return `{:exit :out :err}` with string output for nonzero status.

Do not rely on the library's background `:in` feeder for this site. Start the
process, manually write to its `:in` stream, catch `IOException` around that
write exactly as today, and let babashka.process drain captured output.

### Validation command execution

Validation commands continue inheriting stdin, stdout, and stderr. Nonzero
status continues throwing Soranoha-owned `ExceptionInfo` containing `:command`
and `:exit-code`; babashka.process exception data must not leak as the public
contract.

### ADR evidence capture

Git and evidence commands continue running in the requested repository root,
capturing stdout and stderr strings, and returning the existing
`{:exit-code :stdout :stderr}` map. Git failures keep their existing evidence-
capture exception shape.

Explicitly remap babashka's `{:exit :out :err}` keys to
`{:exit-code :stdout :stderr}` before returning or attaching the map as
exception data.

### Workflow Nix bridge

`default-runner` retains the injectable `[args-vector] -> {:exit :out :err}`
contract. Nonzero Nix status remains data until `realize-flake-output!` converts
it to `:nix-bridge-failure`.

### Aozora replay

`git*` retains its current non-throwing result map, optional working-directory
behavior, and optional output encoding. `git!` remains the owner of converting
nonzero status into its existing contextual exception.

### Source-bundle report

The 7-Zip listability probe continues returning a boolean. Missing executables
and other process-start failures retain current exception behavior; a normal
nonzero tool exit means “not listable.”

String stdin/stdout/stderr relies on babashka.process 0.6.25's UTF-8 defaults,
matching the explicit UTF-8 behavior of the current Nix/Linux implementation.

## Testing strategy

Characterize each distinct contract before changing production code:

- captured stdout/stderr and nonzero status;
- inherited I/O and Soranoha-owned exception data;
- byte-array stdin/stdout plus added environment;
- working-directory execution;
- large concurrent stdout/stderr without deadlock;
- early child exit while stdin is being written;
- injectable Nix runner result shape; and
- missing executable behavior where currently observable.

Prefer real, tiny cross-platform commands already available in the Nix test
environment. Existing tests that inject runners remain unit tests and must not
invoke networked or unpinned external tools.

Run the focused namespace tests after each site group. At completion run:

- the source-contract test proving that the seven production migrations contain
  no `clojure.java.shell`, `ProcessBuilder`, or `Runtime.exec` execution;
- every focused subprocess contract test named above;
- `nix build ./abc#checks.x86_64-linux.clj-kondo`;
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests`;
- `just nix-format-check` if Nix files change (none are expected).

Those source and focused subprocess tests are the acceptance gate for this
migration. Run `just validate-migration` afterward as a repository regression
check; it is the existing ab-validator/TEI/schema migration gate and does not
directly exercise these subprocess call sites.

## Migration and rollback

Land the work in small commits grouped by compatible process contract:

1. captured `sh` sites;
2. working-directory and inherited-I/O sites;
3. byte and streamed runners; and
4. source-contract enforcement and full verification.

Each commit is independently revertible. No persisted data, schema, CLI, or
workflow format changes, so rollback is code-only.

## Out of scope

- Filesystem migration to `babashka.fs`.
- A common project process wrapper.
- Shell pipelines or implicit shell evaluation.
- Changing command output, encodings, exit policy, exception data, or
  environment inheritance.
- Replacing `ProcessBuilder` in tests that model an external executable.
