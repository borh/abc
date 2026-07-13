# Task 5 Report: Evidence-only validated destination and exclusive writer

## Status

Implemented and committed as `0f302b74` (`feat(abc): publish evidence bundles exclusively`).

The commit contains only:

- `abc/src/abc/tools/evidence_output.clj`
- `abc/test/abc/tools/evidence_output_test.clj`

## RED evidence

Initial required RED:

```sh
cd abc && bin/kaocha --focus abc.tools.evidence-output-test
```

Result: exit 1, 1 error, because `abc.tools.evidence-output` did not exist:

```text
Could not locate abc/tools/evidence_output__init.class,
abc/tools/evidence_output.clj or abc/tools/evidence_output.cljc on classpath.
1 tests, 1 assertions, 1 errors, 0 failures.
```

Trust-boundary regression RED added during self-review:

```sh
cd abc && bin/kaocha --focus \
  abc.tools.evidence-output-test/staging-symlink-cannot-escape-from-an-identity-tree-test
```

Result: exit 1, 1 failure. Canonical-only validation returned `nil` instead of
`:invalid-evidence-destination` for a staging symlink lexically inside the
repository and canonically outside it. Validation was then tightened to check
both normalized lexical and canonical relations.

## GREEN evidence

Fresh focused test run before commit:

```sh
cd abc && bin/kaocha --focus abc.tools.evidence-output-test
```

Result: exit 0, `11 tests, 37 assertions, 0 failures.`

Fresh lint/format check with both new files staged so the flake source included
them:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; derivation `abc-clj-kondo` built successfully.

Shared deterministic file writer immutability check:

```sh
git diff --exit-code 8e00eecf -- abc/src/abc/tools/json.clj
```

Result: exit 0 with no diff.

Additional checks:

```sh
git diff --cached --check
rg -n "ATOMIC_MOVE|write-deterministic-json-file!|Files/move|Files/copy" \
  abc/src/abc/tools/evidence_output.clj \
  abc/test/abc/tools/evidence_output_test.clj
```

Results: cached diff check exited 0; forbidden publication/fallback search had
no matches.

## Implemented behavior

`validated-destination`:

- validates existing repository, workspace, and staging roots through
  `abc.tools.path-containment/path-state`;
- canonicalizes returned staging/output files with `babashka.fs`;
- rejects staging overlap with repository/workspace in either direction using
  both normalized lexical and canonical paths;
- requires output's lexical and canonical parent to equal staging root;
- rejects indirect descendants, symlink escapes, and any preexisting output,
  including a symlink directory entry; and
- allocates or mutates nothing.

`write-json-exclusive!`:

- accepts only `ValidatedDestination` instances;
- serializes only through `write-deterministic-json-str`;
- allocates a same-directory UUID sibling with `CREATE_NEW` and `WRITE`;
- retries sibling name collisions without replacing or deleting the colliding
  file;
- writes all bytes and calls `FileChannel.force(true)` before publication;
- publishes exclusively with `Files/createLink(output, temp)`;
- propagates unsupported-link and destination-exists failures without any move
  or copy fallback; and
- removes its sibling after publication failure, publication success, or a
  write/force failure. The channel closes before failure cleanup is attempted.

## Test coverage

The focused suite covers:

- pure canonical destination construction;
- repository/workspace/staging equality, ancestor, and descendant overlap;
- indirect output descendants;
- preexisting output;
- output symlink escape;
- staging symlink escape from an identity tree;
- serialization failure without allocation;
- forced sibling collision and retry;
- partial sibling cleanup after forced storage failure;
- unsupported hard-link fail-closed behavior and cleanup;
- successful deterministic publication and `force(true)`;
- exclusive second-write rejection while preserving the first artifact; and
- rejection of an unvalidated map.

## Self-review

- The publication linearization point is the same-directory hard link.
- There is no `ATOMIC_MOVE`, move, copy, or shared JSON file-writer call.
- Output bytes are complete and forced before the hard link can expose them.
- The output destination is never replaced; a second publication fails with
  `FileAlreadyExistsException`.
- Writer-owned temporary siblings are removed in `finally`; partial-write
  cleanup happens after closing the channel.
- A preexisting colliding sibling is deliberately preserved because it belongs
  to another writer.
- Existing unrelated modifications to Task 2-4 report files were not staged or
  edited.

## Concerns

No blocker and no ambiguous portability result on the current Linux
filesystem. The real hard-link success and real second-write rejection paths
both passed. Filesystems/providers without hard-link support will receive the
provider exception and fail closed, as verified through the unsupported-link
transition seam; there is intentionally no fallback.

## Review follow-up: partial-write and cleanup assertion defects

Commit `5c334c98` (`test(abc): cover partial evidence write cleanup`) fixes the
Important and Minor review findings without changing the publication protocol.
It contains only:

- `abc/src/abc/tools/evidence_output.clj`
- `abc/test/abc/tools/evidence_output_test.clj`

### Root causes

- The prior `force-channel!` failure seam ran only after the production write
  loop had drained the entire buffer. It tested cleanup of a complete but
  unforced sibling, not cleanup after a partial write.
- The prior success cleanup expressions used `babashka.fs/ends-with?`, whose
  path-component semantics do not treat UUID filenames such as
  `.bundle.json.<uuid>.tmp` as ending with the path component `.tmp`. Those
  assertions could therefore return empty even when such a sibling leaked.

### Follow-up RED evidence

The prefix-write regression and the corrected sibling assertions were added
before the production seam. Running:

```sh
cd abc && bin/kaocha --focus abc.tools.evidence-output-test
```

failed with exit 1 while loading the suite:

```text
Unable to resolve var: output/write-buffer! in this context
1 tests, 1 assertions, 1 errors, 0 failures.
```

This established that no seam existed at the actual channel-write boundary.

### Follow-up implementation and assertion proof

The existing production buffer-draining loop was extracted unchanged into the
private `write-buffer!` function. Normal success and publication tests still
execute that real loop. The regression seam writes a strict prefix, records
that the written count is smaller than the serialized byte count, then throws.
The test proves:

- the exception propagates;
- at least one byte but fewer than all bytes reached the sibling;
- the channel is closed before the assertion observes cleanup;
- output was never published;
- the writer-owned sibling is absent; and
- the deliberately preexisting collision remains, with its contents intact.

Success and second-write cleanup assertions now compare the complete sibling
filename set to `#{"bundle.json"}`. A leaked UUID sibling would add a second
set member and fail the equality, independent of path-component semantics. The
partial-write assertion likewise expects exactly the one unrelated collision,
so it catches any leaked writer-owned sibling while proving collision
preservation.

### Follow-up GREEN evidence

Fresh focused suite:

```sh
cd abc && bin/kaocha --focus abc.tools.evidence-output-test
```

Result: exit 0, `12 tests, 44 assertions, 0 failures.`

Fresh staged lint/format check:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; derivation `abc-clj-kondo` built successfully.

Shared writer check:

```sh
git diff --exit-code 8e00eecf -- abc/src/abc/tools/json.clj
```

Result: exit 0 with no diff.

Additional review checks:

```sh
git diff --cached --check
rg -n "ATOMIC_MOVE|write-deterministic-json-file!|Files/move|Files/copy" \
  abc/src/abc/tools/evidence_output.clj \
  abc/test/abc/tools/evidence_output_test.clj
```

Results: cached diff check exited 0 and the forbidden fallback search had no
matches.

### Follow-up self-review and concerns

- The production loop still writes until `ByteBuffer.hasRemaining` is false.
- The seam is private and changes no public interface.
- Failure cleanup remains outside `with-open`, so deletion is attempted only
  after channel closure.
- No unrelated collision is removed during retry or partial-write cleanup.
- Exact sibling-name equality makes both success cleanup assertions
  non-vacuous.
- No portability blocker was found; the follow-up does not alter hard-link
  behavior.

## Integration policy registration

The sandboxed full-suite gate exposed that the exclusive writer's intentional
`Files/exists`, `Files/createLink`, and `Files/deleteIfExists` calls had not
been registered in the repository's exact filesystem-policy map. The writer
implementation was unchanged; the test-owned policy now lists precisely those
three operations with a nonblank rationale. The focused policy regressions
passed with 3 tests and 13 assertions together with the root-flake fixture
check, and the rebuilt Nix suite passed with 1048 tests and 4666 assertions.
