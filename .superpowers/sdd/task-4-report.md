# Task 4 Report: Manifest Index Producer Lookup and Copied-Field Validation

## Scope

Implemented Task 4 only, within the requested write set plus this report:

- `abc/src/abc/tools/manifest_index.clj`
- `abc/test/abc/tools/manifest_index_test.clj`
- `.superpowers/sdd/task-4-report.md`

No unrelated files were modified or reverted.

## Summary

Added Task 4 support for:

- preserving `manifest_identity_object` and provenance fields in manifest index entries
- finding parser-IR producer candidates from indexed entries and identity coordinates
- validating copied analysis identity fields against the referenced producer entry

## Red/Green Test Cycle

### Red

Added the Task 4 helpers and tests described in the brief:

- `analysis-manifest`
- `parser-ir-manifest`
- `parser-ir-producer-lookup-test`
- `analysis-copied-field-validation-test`

Ran the exact focused failure command from the brief:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.manifest-index-test/parser-ir-producer-lookup-test --focus abc.tools.manifest-index-test/analysis-copied-field-validation-test"
```

Observed expected failure:

- compiler error for missing var `manifest-index/parser-ir-producer-candidates`

This confirmed the new tests were exercising missing Task 4 behavior rather than passing on existing code.

### Green

Implemented the production changes in `abc/src/abc/tools/manifest_index.clj`:

- extended `manifest->index-entry` with:
  - `"manifest_identity_object"`
  - `"provenance_used"`
  - `"provenance_was_derived_from"`
- added:
  - `parser-ir-copied-fields`
  - `identity-value`
  - `parser-ir-producer-candidates`
  - `entry-by-artifact-id`
  - `find-producer-entry`
  - `copied-field-errors`
  - `analysis-copied-field-errors`
  - `validate-analysis-copied-fields!`

Updated `index-entries-test` expected maps to include the new indexed fields with `nil`/empty defaults.

Ran the exact passing command from the brief:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.manifest-index-test"
```

Result:

- `4 tests, 6 assertions, 0 failures`

## Notes

- During the green step, Kaocha surfaced two syntax issues introduced while transcribing the new forms. Both were corrected immediately and the full `abc.tools.manifest-index-test` namespace was re-run to verify the final state.
- The Graal/Truffle interpreter warning shown by `nix develop` did not affect test correctness.

## Commit

Committed with:

```bash
git commit -m "feat(abc): validate analysis producer identity fields"
```
