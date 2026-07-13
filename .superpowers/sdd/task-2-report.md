# Task 2 Report: Alias-aware source reading and exact pure capabilities

## Status

Complete.

Commit: `6e2e075f feat(abc): resolve evidence source aliases locally`

## RED evidence

1. Added the alias/concurrency test around a temporary `fixture.alias-test` namespace requiring `abc.sim.artifact-manifest` as `am`, with eight concurrent reads, exact auto-resolved keyword coverage, source metadata coverage, and a `find-ns` non-mutation assertion.
2. Added positive reachable-graph cases for `charred.api/write-json-str` and `clojure.core/re-pattern`, plus negative neighboring capability cases for `charred.api/read-json` and `clojure.core/load-string`.
3. The first focused run failed during test compilation because the requested public `read-source-forms!` API did not yet exist (`No such var: runtime/read-source-forms!`).
4. After the minimal first-cycle rename exposing the existing reader, the focused suite reached the intended behavioral RED state:
   - `::am/run-summary-events` raised an invalid-keyword reader error.
   - `charred.api/write-json-str` was rejected as `:forbidden-evidence-capability`.
   - `clojure.core/re-pattern` was rejected as `:forbidden-evidence-capability`.
   - Result: 28 tests, 95 assertions, 3 errors, 0 failures.

## GREEN implementation

- Renamed the source reader to the public `read-source-forms!` interface and updated its two internal callers.
- Reads the first form under `*read-eval* false`.
- Derives a plain symbol-to-symbol alias map only from `:require` vector libspecs containing `:as`.
- Reads later forms with an invocation-local `clojure.tools.reader/*alias-map*` binding.
- Does not create namespaces, install aliases, enter namespaces, or bind `*ns*` to a created namespace.
- Retains the indexing push-back reader, preserving line and column metadata.
- Added exactly `charred.api/write-json-str` and `clojure.core/re-pattern` to `audited-safe-external-vars`.

## Files changed

- `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`

## Verification

- `cd abc && bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test`
  - GREEN run 1: 28 tests, 98 assertions, 0 failures.
  - GREEN run 2: 28 tests, 98 assertions, 0 failures.
- `nix build ./abc#checks.x86_64-linux.clj-kondo`
  - Exit 0; derivation built successfully.
- `git diff --check`
  - Exit 0.
- Source scan for `create-ns`, `alias`, `in-ns`, and `binding [*ns*` in the two task files found no matches.

## Self-review

- The resolver state is dynamic and thread-local for each invocation; eight futures exercise repeated concurrent reads and return identical forms.
- An empty local alias map also prevents accidental fallback to aliases in the caller's ambient `*ns*`.
- Namespace names and aliases remain symbols, so no `Namespace` objects are created or registered.
- The negative capability cases demonstrate exact admission rather than namespace-wide or family-wide admission.
- No plan, specification, or progress files were edited.

## Concerns

None. The implementation intentionally covers ordinary vector `:require` libspecs with `:as`, which is the alias syntax required by the task brief; it does not introduce a broader source-reader protocol.

## Critical read-eval fix

Fix commit: `b7dd3c2296ebfd891545d1c5e8f99e7ca8607846 fix(abc): disable tools reader eval`

### Regression RED

- Added `source-reader-disables-read-eval-test` with a temporary source containing a `#=` payload that sets a unique JVM system property.
- The test requires both rejection of the source and proof that the property remains unset, then clears the property in `finally`.
- Initial focused regression command:
  - `cd abc && bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test/source-reader-disables-read-eval-test`
  - Result: 1 test, 2 assertions, 2 failures.
  - `read-source-forms!` returned without throwing.
  - The sentinel property contained `"executed"`, proving that the payload ran.

### Root cause confirmation

- Installed dependency: `org.clojure/tools.reader` 1.5.2.
- Its namespace excludes `clojure.core/*read-eval*` and defines a distinct `clojure.tools.reader/*read-eval*` dynamic Var whose default is `true`.
- Its `#=` dispatch calls `read-eval`, which checks the tools.reader Var before evaluating the next form.
- A direct identity check returned `:same-var? false` for `#'clojure.core/*read-eval*` and `#'clojure.tools.reader/*read-eval*`.
- Therefore the original unqualified core binding did not affect `clojure.tools.reader/read`.

### Minimal fix

- Changed only the reader-boundary binding from `*read-eval*` to `reader/*read-eval*`.
- No reader protocol, alias semantics, capability inventory, or other behavior changed.

### Fix GREEN and verification

- Focused regression: 1 test, 2 assertions, 0 failures.
- Full focused suite run 1: 29 tests, 100 assertions, 0 failures.
- Full focused suite run 2: 29 tests, 100 assertions, 0 failures.
- `nix build ./abc#checks.x86_64-linux.clj-kondo`: exit 0; derivation built successfully.
- `git diff --check`: exit 0.

### Fix concerns

None.
