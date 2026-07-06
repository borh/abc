# Active Surface Lint Baseline

Captured during Task 3 of `2026-07-04-dependency-and-code-quality-hardening`.

Initial command:

```bash
nix run nixpkgs#clj-kondo -- --lint src/abc/tools test/abc/tools --fail-level warning
```

Initial result before fixes:

```text
errors: 0, warnings: 12
```

Warnings fixed in `8ad3f72`:

- `src/abc/tools/linked_art.clj`: missing `clojure.string` require and return type hint placement.
- `src/abc/tools/logging.clj`: unused `filters-installed` value expression.
- `src/abc/tools/manifest_to_rdf.clj`: unused `ByteArrayOutputStream` import.
- `src/abc/tools/metadata_record.clj`: unused `abc.tools.files` require.
- `src/abc/tools/person_record.clj`: unused `abc.tools.files` require.
- `src/abc/tools/parser_ir_plaintext.clj`: unused renderer args and redundant nested `let`.
- `test/abc/tools/aozora_history_audit_test.clj`: unused `split-year` binding.
- `test/abc/tools/materialize_import_test.clj`: unused `testing` refer.
- `test/abc/tools/person_drift_test.clj`: unused `testing` refer.

Final active-surface lint result after fixes:

```text
errors: 0, warnings: 0
```
