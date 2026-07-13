# Task 6 Report: Operational Namespace Closure and Closed Descriptor Policy

## Status

Implemented and verified at head `5c334c98` before the Task 6 commit. No
descriptor-schema or catalog-shape conflict required `NEEDS_CONTEXT`.

## RED

Added `abc.tools.adr-evidence-operational-test` before production code and ran:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-operational-test
```

Observed the required load error because
`abc.tools.adr-evidence-operational` did not exist: 1 test, 1 assertion,
1 error.

During trust-boundary self-review, added a second focused regression proving
closure-manifest containment precedes namespace reads. It failed with
`:missing-operational-namespace` instead of the required
`:invalid-operational-closure`, then passed after reordering validation.

## GREEN

Final focused test command:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-operational-test \
  --focus abc.tools.adr-evidence-observation-catalog-test
```

Result: 21 tests, 104 assertions, 0 failures.

Final lint command:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; the `abc-clj-kondo` derivation built successfully.

Additional hygiene checks:

```sh
git diff --check
scripts/comment-hygiene-check.sh
```

Result: both exit 0; comment hygiene reported all criteria pass.

## Files

- Created `abc/src/abc/tools/adr_evidence_operational.clj`.
- Created `abc/test/abc/tools/adr_evidence_operational_test.clj`.
- Modified `abc/src/abc/tools/adr_evidence_observation_catalog.clj` with the
  single-row, family-discriminated catalog selector.

No plan, specification, or progress document was edited. Pre-existing changes
to Task 2–5 reports were preserved and excluded from the Task 6 commit.

## Implemented Contracts

- Namespace resolution checks all `[src test] x [.clj .cljc]` candidates,
  rejects unsafe candidates before reads, rejects ambiguity, parses only the
  first `ns` form through `tools.reader` with reader evaluation disabled,
  expands prefix libspecs, ignores external namespaces, and treats only the
  exact `abc` segment as owned.
- Descriptor versions 1, 2, focused-v3, and operational-v1 are closed by exact
  key set. Operational-v1 is additionally discriminated by `clojure` versus
  `nix-only`; the branches cannot borrow each other's fields.
- Catalog row selection binds descriptor stem, observation key, focus or
  operational command coordinates, entrypoint kind, and RFC 8785/JCS contract
  hash.
- Clojure operational closure manifests have an exact schema, exact `[abc]`
  ownership policy, same descriptor stem, exact catalog entrypoints, exact
  derived closure paths, and contained coordinates.
- Operational `exact-v1` requires equality among the derived required set and
  descriptor explicit inputs. Offline validation additionally compares the
  bundle explicit set and bundle input keys, checks required bindings and
  current hashes, accumulates stable problems, and never invokes the command.
- Nix-only exactness is descriptor plus reviewed catalog determinants only; it
  does not invent a closure manifest, namespaces, or Clojure lock inputs.
- Generic `repo-files-v1` behavior was not changed.

## Exact Test Coverage

- `src`/`test` and `.clj`/`.cljc` resolution.
- Duplicate candidate ambiguity across roots and extensions.
- Prefix libspec expansion and transitive cycles.
- Missing owned namespaces, external namespaces, and exact segment-prefix
  behavior (`abc.*` owned, `abcdef.*` external).
- Reader-eval suppression and exclusion of in-body/dynamic `require`.
- Traversal, absolute, and symlink-escape coordinates.
- Unsorted and traversal-bearing operational manifests.
- Closed v1/v2 rejection of catalog fields and positive focused-v3 loading.
- Operational mode, branch, manifest, catalog command, and JCS hash mismatch.
- Missing/extra descriptor explicit inputs and bundle input keys.
- Pure-Nix `tei-profile-drift` shape without invented namespaces.
- Offline problem accumulation and a command argv that would create a marker
  if executed; the marker remains absent.

## Self-Review

Depth: architecture/trust-boundary review.

- Ownership, parsing, catalog selection, exact-set comparison, and offline
  hashing remain separate pure/value-oriented stages. No command execution or
  mutable shared state is mixed into policy validation.
- The Clojure and Nix-only branches share descriptor/catalog plumbing but own
  different exact-set derivations. This keeps the critical absence of
  automatic Clojure inputs in the Nix-only branch explicit.
- Manifest containment now precedes namespace resolution, matching the
  capture-sequence trust boundary. All later reads operate on coordinates
  already accepted by `path-state`.
- Offline validation cannot derive an exact set after an invalid descriptor or
  manifest, so it reports those root-cause problems rather than speculative
  downstream set differences. Once context is valid, independent bundle-set,
  missing-binding, and hash problems accumulate deterministically.

## Concerns

- Task 7 must call `offline-policy-problems` only in addition to generic bundle
  schema/path/hash validation; this function deliberately does not duplicate
  every generic run-bundle check.
- Focused-v3 runtime-manifest contents remain owned by
  `abc.tools.adr-evidence-runtime-inputs`; Task 6 validates its required closed
  descriptor coordinate and catalog binding without duplicating that runtime
  closure implementation.

## Review Hardening Follow-up

### Root causes

- Focused-v3 catalog binding projected `last(:argv)` to a symbol and compared
  only that value with the catalog focus. Shell indirection, options, and
  multiple focuses could therefore retain the expected final token while
  changing the command that would execute.
- Focused-v3 descriptor shape checked only that `:runtime-input-manifest` was a
  nonempty string. It did not establish containment, same-stem identity, or
  explicit-input membership before returning the descriptor context.
- Namespace parsing returned only requirements, discarding the namespace
  declared by the `ns` form. A path-derived candidate could therefore claim a
  different namespace without detection.
- The v2 closed-key regression omitted v2's required runtime-manifest field,
  so the forbidden catalog field was not isolated as its sole defect.

### Follow-up RED

Added focused regressions and ran:

```sh
cd abc
bin/kaocha \
  --focus abc.tools.adr-evidence-operational-test/owned-namespace-resolution-fails-closed-test \
  --focus abc.tools.adr-evidence-operational-test/focused-v3-rejects-adversarial-runner-and-argv-test \
  --focus abc.tools.adr-evidence-operational-test/focused-v3-runtime-manifest-coordinate-is-bound-before-context-test
```

Observed 3 tests, 13 assertions, 9 failures: four manifest-coordinate cases,
four adversarial runner/argv cases, and the declared-namespace mismatch were
all incorrectly accepted.

### Follow-up implementation

- Focused-v3 context admission now derives the repository runner exactly as v2
  does: `bin/kaocha` for `clojure-test-v1` and normalized
  `<component-root>/bin/kaocha` for `component-clojure-test-v1`.
- It requires exact argv `[runner "--focus" catalog-focus]`, explicit runner
  membership, and a contained regular executable runner. Shell indirection,
  multiple focuses, extra options, unbound runners, and non-executable runners
  fail as `:invalid-focused-evidence-runner`.
- Runtime-manifest admission requires the profile-specific ADR-input prefix,
  the exact descriptor filename, explicit membership, and a contained regular
  file. Missing, traversal, wrong-stem, symlink-escape, and unbound coordinates
  fail as `:invalid-runtime-input-manifest`.
- The source reader now returns both the declared namespace and static
  requirements. The resolver requires the declaration to equal the requested
  owned namespace before traversing dependencies.
- The v2 catalog-field test now starts with its required runtime-manifest and
  otherwise valid focus/profile bindings, leaving the extra catalog field as
  the isolated closed-key defect.

Runtime-manifest EDN contents are still not read by this focused-v3 admission
step; `abc.tools.adr-evidence-runtime-inputs` remains their owner. No command,
Kaocha summary, workspace/staging root, or capture orchestration behavior was
added.

### Follow-up GREEN

Final focused suite:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-operational-test \
  --focus abc.tools.adr-evidence-observation-catalog-test
```

Result: 23 tests, 115 assertions, 0 failures.

Final lint and hygiene:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
git diff --check
scripts/comment-hygiene-check.sh
```

Result: all commands exit 0; comment hygiene reports all criteria pass.

### Follow-up self-review

- The catalog-selected focus is now part of an exact command value rather than
  an argv substring/projection, closing the command-identity trust gap.
- Runner and manifest checks validate coordinates and metadata only. Execution
  and runtime-input closure remain in their existing owners, avoiding a second
  orchestration path.
- Namespace identity is checked immediately after reading the sole `ns` form
  and before its requirements enter the traversal queue, so a false declaration
  cannot influence closure membership.

## Selected-row freshness correction

The integration follow-up had added the whole observation catalog to focused
required inputs and operational exact inputs. That contradicted the protocol:
the descriptor binds the selected row's canonical contract hash, while an
unrelated catalog row must not stale the observation.

Two regression cases first failed: focused-v3 context loading rejected a
descriptor that bound its descriptor, manifest, and runner without the whole
catalog, and operational offline validation reported the catalog as a missing
exact input after only an unrelated row was added. Removing the catalog from
those required input sets fixed both while retaining contained catalog loading
and selected-row hash recomputation.

The focused RED command reported 1 error and 1 failure. The same two-focus
command then passed with 2 tests and 3 assertions.

Final diff review found that bundle validation still skipped offline policy
validation whenever a generic hash or input problem existed. A focused-v3
regression first returned only `:artifact-hash-mismatch`; after removing that
guard it accumulated the artifact mismatch, missing protocol inputs, and
missing selected observation. The combined operational, capture, and bundle
suites passed with 40 tests and 152 assertions.

The first full sandboxed suite also showed that its writable `abc/` fixture
copied the monorepo lock beside the component but omitted the monorepo
`flake.nix` read by the exact Stage A governance predicate test. The Nix check
now copies both root flake files into that parent fixture. The isolated
governance and filesystem-policy regressions passed with 3 tests and 13
assertions; the rebuilt Nix suite passed with 1048 tests and 4666 assertions.

The repository-wide validation then exposed the Nix 2.34 `flake show --json`
schema change. The root output-contract smoke test now requests all systems and
normalizes the versioned `inventory` envelope while retaining the legacy JSON
path. Its focused smoke command and the subsequent `just validate-migration`
run both exited zero.
