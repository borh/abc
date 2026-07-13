# Task 4 Report: Workspace-Aware Ephemeral Ownership

## Status

Complete.

## RED evidence

Command:

```sh
cd abc
bin/kaocha --focus abc.tools.evidence-io-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test
```

Result: exit 1. Test loading failed at `evidence_io_test.clj:81` with
`No such var: evidence-io/with-owned-ephemeral-root`. This was the expected
failure because the ownership API had not been implemented.

The first post-implementation run also exposed a test assertion-shape error:
the workspace-default test compared the whole trace result with the callback
value. Correcting the assertion to inspect `:value` required no production
change.

## GREEN evidence

Focused tests:

```sh
cd abc
bin/kaocha --focus abc.tools.evidence-io-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test
```

Result: exit 0, 39 tests, 131 assertions, 0 failures.

Lint:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0.

Diff hygiene:

```sh
git diff --check
```

Result: exit 0 with no output.

## Tests added or extended

- `ephemeral-root-requires-trace-and-external-workspace-test`
- `read-trace-workspace-defaults-to-canonical-identity-test`
- `owned-ephemeral-root-owns-lifecycle-and-traces-rereads-test`
- `owned-ephemeral-root-cleans-up-after-throw-test`
- `owned-ephemeral-root-requires-active-trace-test`
- Extended `every-trusted-callback-position-is-explicit-and-fail-closed-test`
  for the exact `with-owned-ephemeral-root` callback position `#{0}`, hidden
  callback rejection, and direct literal callback reachability.
- Extended the boundary analyzer fixture with only the new owner Var.

## Files changed

- `abc/src/abc/tools/evidence_io.clj`
- `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- `abc/test/abc/tools/evidence_io_test.clj`
- `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`

## Self-review

- `with-read-trace` canonicalizes `workspace-root` and defaults it to the
  canonical identity root.
- The shared overlap validation rejects canonical overlap in either direction
  against both identity and workspace roots.
- `with-ephemeral-root` remains the only scoped authorization mechanism.
  `with-owned-ephemeral-root` owns lifecycle with `fs/with-temp-dir`, validates
  the generated root, and delegates authorization to `with-ephemeral-root`.
- Cleanup is verified after both normal completion and exceptions.
- Rereads remain deduplicated ephemeral trace entries.
- Only `abc.tools.evidence-io/with-owned-ephemeral-root` was added to the trusted
  adapter inventory and audited higher-order signatures, at callback position
  `#{0}`.
- Existing unrelated Task 2 and Task 3 report changes were not modified.

## Concerns

None.
