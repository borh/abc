# Task 2 Report

## Summary

Implemented the schema-derived parser-IR vocabulary helper and the publication policy loader/hash helper, plus the policy artifact and focused tests that verify renderer coverage against the schema node vocabulary.

## TDD Evidence

### RED

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test
```

Result:

- Failed as expected before implementation.
- Primary error: `Could not locate abc/tools/parser_ir_publication_policy.clj or abc/tools/parser_ir_publication_policy__init.class on classpath.`
- Exit status: non-zero

### GREEN

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test
```

Result:

- `4 tests, 5 assertions, 0 failures.`
- Exit status: 0

## Commit

- `369e612` - `test(parser-ir): derive renderer coverage from policy`

## Self-Review

- `node-types` derives the vocabulary from `schemas/parser-ir.schema.json` by reading the `const` inside each `allOf` node definition, matching the brief and the actual schema layout.
- `coverage-errors` reports missing renderer coverage deterministically in sorted order.
- `policy-hash` canonicalizes parsed JSON through JCS before hashing, so the identity is based on content rather than file bytes.
- The policy artifact contains the required `plaintext` and `tei` renderer policy key sets and matches the schema-derived vocabulary exactly.

## Task 2 Fix Follow-up

### RED

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-publication-policy-test
```

Result:

- `coverage-errors-report-unexpected-node-policies-test` failed as intended.
- Expected: `["plaintext renderer has unexpected parser-IR node policy for image"]`
- Actual: `[]`
- Exit status: 1

### GREEN

Command:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test
```

Result:

- `5 tests, 6 assertions, 0 failures.`
- Exit status: 0
