# Task 5 Report: Add Parser-IR to TEI Body Renderer

## Status

Completed.

## Scope Delivered

- Added pure TEI body renderer in `src/abc/tools/parser_ir_tei.clj`.
- Added focused unit tests in `test/abc/tools/parser_ir_tei_test.clj`.
- Kept the namespace limited to hiccup rendering plus char declaration collection.
- Did not serialize XML, write files, or invoke `ab-validator`.

## TDD Evidence

### RED

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-tei-test
```

Observed result:

- Exit code: `1`
- Failure mode: test namespace failed to load because `abc.tools.parser_ir_tei` did not exist yet.
- Relevant error: `Could not locate abc/tools/parser_ir_tei__init.class, abc/tools/parser_ir_tei.clj or abc/tools/parser_ir_tei.cljc on classpath.`

This satisfied the required missing-production-code failure for the new test namespace.

### GREEN

Focused TEI renderer verification:

```bash
bin/kaocha --focus abc.tools.parser-ir-tei-test
```

Observed result:

- Exit code: `0`
- `4 tests, 11 assertions, 0 failures.`

Required focused command from the brief:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test --focus abc.tools.parser-ir-plaintext-test --focus abc.tools.parser-ir-tei-test --focus abc.tools.tei-header-unit-test
```

Observed result:

- Exit code: `0`
- `14 tests, 26 assertions, 0 failures.`

## Design-Bundle Validation

Command:

```bash
nix run .#validate-design-bundle
```

Observed result:

- Exit code: `0`
- Final line: `design bundle validation ok`

Additional notable successful gates from the output:

- `tei rng validation ok`
- `tei project rng validation ok`
- `tei schematron validation ok`

## Implementation Notes

- `render` now returns:

```clojure
{:body [:text [:body ...]]
 :char_declarations [...]
 :node_counts {...}
 :omitted [...]}
```

- Paragraph handling follows the brief:
  - inline nodes accumulate inside `[:p ...]`
  - `heading`, `page-break`, `image`, and `caption` flush the current paragraph before emitting body-level nodes
- `gaiji` rendering preserves the header/body contract:
  - `[:g {:ref "#X"}]` is always emitted
  - declarations are deduplicated by `:xml-id`
  - declaration order matches first appearance
  - referenced ids strip a leading `#`
  - generated ids are only used when no reference exists

## Commit

Created commit:

```text
1509e8c feat(parser-ir): render TEI body from parser IR
```

## Self-Review

- Confirmed write scope stayed within the two task files.
- Confirmed no XML emission logic was added to the TEI body renderer.
- Confirmed the task’s gaiji declaration invariant is covered by focused tests and passes design-bundle validation.
- No further issues found in the scoped diff.

## Review Fixes

### Scope

- Derived `covered-node-types` from the TEI renderer dispatch map instead of a hand-maintained literal.
- Replaced the permissive TEI renderer fallback with an explicit unsupported-node exception.
- Added a regression for gaiji nodes without `reference`, pinning the generated id and preserved raw marker.

### RED

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-tei-test
```

Observed result:

- Exit code: `1`
- `5 tests, 14 assertions, 1 failures.`
- Relevant failure: `coverage-test` expected `parser-ir-tei/covered-node-types` to match the renderer dispatch surface, but the runtime lookup returned `nil` before the single-source dispatch map existed.

### GREEN

Focused TEI verification:

```bash
bin/kaocha --focus abc.tools.parser-ir-tei-test
```

Observed result:

- Exit code: `0`
- `5 tests, 14 assertions, 0 failures.`

Required cross-check command:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test --focus abc.tools.parser-ir-plaintext-test --focus abc.tools.parser-ir-tei-test --focus abc.tools.tei-header-unit-test
```

Observed result:

- Exit code: `0`
- `15 tests, 29 assertions, 0 failures.`

Validation command:

```bash
nix run .#validate-design-bundle
```

Observed result:

- Exit code: `0`
- Final line: `design bundle validation ok`
