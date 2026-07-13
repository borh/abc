# Task 3 Report: Typed evidence observation catalogs

## Status

Complete and verified on `feat/adr-evidence-corpus-migration` from base
`b7dd3c22`.

## RED evidence

The new catalog tests and registrar join test were written before the
production namespace or checked data existed.

Command:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-register-test
```

Observed result: exit 1, one test/error. Test loading failed because
`abc.tools.adr-evidence-observation-catalog` did not exist. This was the
expected missing-feature RED, not a fixture or syntax failure.

## GREEN implementation

- Added a family-neutral closed catalog with separate sorted focused and
  operational collections. Shape, scalar domains, collection order,
  uniqueness, determinant containment, lexical path normalization, and dotted
  namespace coordinates are validated without a foundation row-count policy.
- `catalog-problems` accumulates and sorts all structural problems. Malformed
  rows and mixed collection values fail as data rather than throwing.
- `load-catalog!` validates the catalog coordinate as a contained regular file
  before calling `files/read-edn`, and every thrown validation failure carries
  the complete problem vector.
- Added an explicit field-by-field string-domain I-JSON projection. The digest
  pipeline is exactly `projection -> RFC 8785 UTF-8 bytes -> SHA-256 ->
  sha256:` formatting; generic EDN serialization is never hashed.
- Added catalog-to-template joins that accumulate and sort unknown,
  unbound, malformed, and duplicate `[claim-id observation-id]` problems.
- Added closed, kind-specific conformance finding identities with mandatory
  coordinate and value-domain checks. Message/source positions are ignored;
  unknown kinds, incomplete identities, invalid coordinate types, and
  absolute/machine-local paths fail closed.
- Added one checked catalog with 35 unique focused observations and two
  operational observations. The three repeated focused Vars and three
  design-bundle bindings collapse to their single observation identities;
  `source-bundle-corpus` remains separate.
- Added the exact current transitional debt generated mechanically from the
  checked catalog and analyzer.

## Known-answer evidence

The test contains literal canonical JSON bytes and a literal digest. Neither
expected value calls the projection, JCS writer, or hash function under test.
The literal JSON was independently piped to `sha256sum`, producing:

```text
f9492de66eaa225319318694ce7ece66978fc0903f5ebaffd24eba1e82070ed1
```

The test pins
`sha256:f9492de66eaa225319318694ce7ece66978fc0903f5ebaffd24eba1e82070ed1`.
Because the JCS API returns Java byte arrays, order-independent byte equality
is asserted over their byte vectors; exact UTF-8 content is separately
compared with the literal JSON string.

## Exact arithmetic

- Focused observations: 35.
- Operational observations: 2.
- Total observation identities: 37.
- Focused claim bindings: 38.
- Operational claim bindings: 4.
- Total claim bindings: 42.
- Distinct foundation claim IDs: 35.
- Transitional normalized findings: 53.
  - Unresolved focused Vars: 14.
  - Existing focused Vars missing direct boundary ownership: 21.
  - Additional capability, I/O, reader/span, or call-graph blockers: 18.

The foundation-only test carries all 42 binding identities, validates the
join, and compares the checked debt value exactly with live normalization.
The generic catalog test separately accepts a valid catalog with different
row counts.

## Files

- `abc/src/abc/tools/adr_evidence_observation_catalog.clj`
- `abc/test/abc/tools/adr_evidence_observation_catalog_test.clj`
- `abc/test/abc/tools/adr_evidence_register_test.clj`
- `abc/data/adr-evidence/foundation-observation-catalog.edn`
- `abc/data/adr-evidence/foundation-capture-conformance-debt.edn`

No plan, specification, progress, governance, registry, descriptor, manifest,
promotion, or run-bundle file was changed by Task 3.

## Verification

Final focused command:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-register-test
```

Result: exit 0; **14 tests, 109 assertions, 0 failures**.

Formatting/delimiter gate:

```sh
cd abc
clj-paren-repair src/abc/tools/adr_evidence_observation_catalog.clj \
  test/abc/tools/adr_evidence_observation_catalog_test.clj
```

Result: both files valid and formatted.

Repository Clojure gate, after staging the exact new files so flake `self`
included them:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; derivation
`/nix/store/kz5y0cf9rh3a8fdccn2zlgfa6p9qcl7p-abc-clj-kondo.drv` built.

Additional gates:

- `scripts/comment-hygiene-check.sh`: exit 0.
- `git diff --cached --check`: exit 0.
- staged scope: exactly the five Task 3 files.
- debt scan: no `:message`, `:line`, or `:column` keys.
- governance remains configured in audit mode; staged diff contains no
  governance, run-bundle, or registry path.

## Self-review and independent review

Self-review hardened malformed-row accumulation, normalized determinant
spellings, and exact foundation binding arithmetic.

The independent read-only review initially found:

1. determinant aliases such as `./flake.nix` needed rejection;
2. debt identity needed per-kind closed projections and relative paths;
3. entrypoint namespace symbols needed to reject qualified Var symbols; and
4. recognized debt kinds needed mandatory typed semantic coordinates.

All four were fixed with negative tests. Final re-review verdict: resolved,
with no remaining Critical or Important issue.

## Concerns

None. The 53-item debt is deliberately transitional and exact; Task 8 owns
the atomic change to an empty debt vector together with the missing narrow
tests and direct boundary ownership.

## Controller-review closure fix

### Root cause and RED evidence

Two controller findings were reproduced regression-first.

1. `exact-keys-problems` used Clojure's natural `sort` on arbitrary EDN map
   keys. A focused row containing unknown keys `"string-key"` and `42`
   reached `clojure.lang.Util/compare` and threw `ClassCastException` before
   `catalog-problems` could return its accumulated vector.
2. `normalized-finding-path` asked the Linux filesystem whether the supplied
   value was absolute before normalizing slash spelling. Both
   `C:\\tmp\\finding.clj` and `\\\\server\\share\\finding.clj` therefore
   appeared host-relative and were admitted.

RED command:

```sh
cd abc
bin/kaocha \
  --focus abc.tools.adr-evidence-observation-catalog-test/heterogeneous-unknown-row-keys-are-accumulated-deterministically-test \
  --focus abc.tools.adr-evidence-observation-catalog-test/normalized-conformance-findings-reject-cross-host-machine-paths-test
```

Observed result: exit 3; **2 tests, 3 assertions, 1 error, 2 failures**.
The error was the expected heterogeneous-key `ClassCastException`; the two
failures showed that drive-qualified and UNC paths returned normally instead
of throwing.

### Minimal fixes

- Missing/unknown diagnostic key vectors now use `sort-by pr-str`, providing
  a deterministic total order across heterogeneous EDN key types without
  changing accepted catalog shapes.
- Raw Windows drive prefixes and UNC prefixes are rejected before calling
  `fs/path`, `fs/absolute?`, or the shared coordinate normalizer. Host-native
  absolute/traversal checks remain unchanged.

Focused regression GREEN command: the same two-focus command above.

Result: exit 0; **2 tests, 4 assertions, 0 failures**.

Full Task 3 GREEN command:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-register-test
```

Result: exit 0; **16 tests, 113 assertions, 0 failures**.

Staged repository Clojure gate:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; derivation
`/nix/store/p5abr6580kgv07miqlbish1y9jkzam7i-abc-clj-kondo.drv` built.

### Fix self-review

- Heterogeneous unknown keys remain diagnostic data and are sorted only for
  reporting; required/accepted schema key sets are unchanged.
- Drive detection is case-insensitive and rejects every leading ASCII drive
  qualifier, including drive-relative spellings. UNC detection covers both
  backslash and already slash-normalized forms.
- Machine-local detection happens before all host-dependent path parsing, so
  behavior does not depend on the capture host OS.
- The staged fix contains only the catalog source and its focused test. No
  catalog/debt values, governance state, descriptor, registry, or run bundle
  changed.

## Single-backslash cross-host closure fix

### RED evidence

The controller's remaining path case was added to the existing cross-host
regression with both Windows root-relative `\\tmp\\finding.clj` and
traversal-shaped `\\..\\outside.clj` spellings.

Command:

```sh
cd abc
bin/kaocha \
  --focus abc.tools.adr-evidence-observation-catalog-test/normalized-conformance-findings-reject-cross-host-machine-paths-test
```

Observed result: exit 2; **1 test, 4 assertions, 2 failures**. The existing
drive-qualified and UNC assertions passed, while both new single-leading-
backslash values returned normally instead of throwing.

### Minimal fix and GREEN evidence

The raw cross-host predicate now rejects any leading backslash rather than
only a two-backslash UNC prefix. This one-character boundary change retains
UNC rejection and closes Windows root-relative and traversal-shaped spellings
before `fs/path`, `fs/absolute?`, or slash normalization runs.

Focused regression GREEN: the same command exited 0 with **1 test, 4
assertions, 0 failures**.

Full Task 3 command:

```sh
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-register-test
```

Result: exit 0; **16 tests, 115 assertions, 0 failures**.

Staged Clojure gate:

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Result: exit 0; derivation
`/nix/store/bkcma3kr7xcyr458gns1kbw5d6bad1yi-abc-clj-kondo.drv` built.

### Self-review

- The fix changes only raw machine-local classification; accepted normalized
  repository-relative paths and their identities are unchanged.
- Any one-backslash prefix is nonportable as a repository-relative coordinate
  and is rejected uniformly, including UNC, Windows root-relative, and
  backslash traversal spellings.
- The staged scope remains exactly the catalog source and its focused test;
  no data, governance, registry, descriptor, or run-bundle file changed.
