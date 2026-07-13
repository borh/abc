# Task 2 report

- Commit: `6a377788` (`refactor(abc): use babashka fs in leaf filesystem code`)
- Scope: migrated the nine grandfathered leaf namespaces for registries, schema discovery, facts/report enumeration, diagram existence checks, and Schematron/TEI canonical cache keys. `abc.tools.files` was already compliant; its missing-delete and parentless-copy characterizations remain unchanged.
- Policy: removed exactly `abc.tools.request-set-resolver`, `abc.tools.schema`, `abc.tools.facts`, `abc.tools.soranoha-layout-report`, `abc.tools.source-bundle-report`, `abc.tools.diagram.adr-graph`, `abc.tools.diagram.core`, `abc.tools.schematron`, and `abc.tools.tei` from both legacy baseline sets. Permanent exception mechanics and maps were unchanged.

## Old green

Run from `abc/` with `TEI_SCHEMA_PATH=/nix/store/gd772mj2jm1pih1g7larhlkm5irlhbql-tei_all.rng`:

```sh
bin/kaocha --focus abc.tools.files-test \
  --focus abc.tools.request-set-resolver-test \
  --focus abc.tools.schema-test \
  --focus abc.tools.facts-test \
  --focus abc.tools.soranoha-layout-report-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.diagram.core-test \
  --focus abc.tools.schematron-test \
  --focus abc.tools.tei-test
```

Result: 86 tests, 263 assertions, 0 failures.

## New green

The same focused command produced 86 tests, 263 assertions, 0 failures.

```sh
bin/kaocha --focus abc.tools.filesystem-policy-test
```

Result: 9 tests, 96 assertions, 0 failures.

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo --no-link
```

Result: exit 0; Clojure lint and formatting passed.

## Concerns

- The brief's `bin/kaocha` command must be run from `abc/`; from the monorepo root the executable is `abc/bin/kaocha` but the dependency aliases resolve against the wrong working directory.
- Plain-shell TEI tests require the pinned schema path from `nix develop ./abc`; the path above is the value exposed by this checkout's dev shell.
- No production concern remains. Paths stay as `Path` values through traversal/filter/sort and convert to `File` only at existing Java/Clojure IO consumers.

## Review fixes

- Added `checked-in-schema-resources-discovers-nested-schemas-in-stable-order-test`. Red command: `cd abc && bin/kaocha --focus abc.tools.schema-test`; the new fixture-root call failed with `ArityException`, proving the old helper could not exercise/preserve nested discovery. Green result after the fix: 13 tests, 67 assertions, 0 failures.
- Restored recursive schema discovery with root `*.schema.json` plus recursive `**/*.schema.json` globs, deduplication, and an explicit relative-path sort.
- Kept `Path` values through request-set predicates, schema/facts/report traversal, and Schematron/TEI canonical cache operations. Conversion to `File` now occurs only at JSON readers, `scan-zip`, `SchematronResourceSCH/fromFile`, and other Java consumers.

Fresh final verification:

```sh
cd abc
TEI_SCHEMA_PATH=/nix/store/gd772mj2jm1pih1g7larhlkm5irlhbql-tei_all.rng \
  bin/kaocha --focus abc.tools.files-test \
  --focus abc.tools.request-set-resolver-test --focus abc.tools.schema-test \
  --focus abc.tools.facts-test --focus abc.tools.soranoha-layout-report-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.diagram.adr-graph-test \
  --focus abc.tools.diagram.core-test --focus abc.tools.schematron-test \
  --focus abc.tools.tei-test
```

Output: `87 tests, 264 assertions, 0 failures.`

```sh
cd abc
TEI_SCHEMA_PATH=/nix/store/gd772mj2jm1pih1g7larhlkm5irlhbql-tei_all.rng \
  bin/kaocha --focus abc.tools.filesystem-policy-test
```

Output: `9 tests, 96 assertions, 0 failures.`

```sh
nix build ./abc#checks.x86_64-linux.clj-kondo --no-link
```

Output: exit 0; lint and formatting passed.
