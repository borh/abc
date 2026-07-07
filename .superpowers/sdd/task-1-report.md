# Task 1 Report: Production Orthographic Annotation Sidecar Producer

## Status

Implemented the production orthographic annotation sidecar producer in `ab-aat-to-parser-ir` with:

- a new library entrypoint at `ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations`
- a new CLI subcommand `detect-ortho-annotations`
- sentence visible-text helper exposure for parser-IR sentence reconstruction
- an integration test that proves sentence coordinates come from parser-IR sentence spans

## TDD Evidence

### Red

Added `detect_orthographic_annotations_uses_parser_ir_sentence_coordinates` to:

- `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

Ran:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- detect_orthographic_annotations_uses_parser_ir_sentence_coordinates
```

Observed expected failure:

```text
error[E0433]: cannot find `ortho_detect` in `ab_aat_to_parser_ir`
```

This confirmed the new test was exercising missing production behavior rather than passing against existing code.

### Green

Implemented the new module, exports, CLI subcommand, and sentence helper rename.

Re-ran the focused test:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- detect_orthographic_annotations_uses_parser_ir_sentence_coordinates
```

Observed:

```text
test detect_orthographic_annotations_uses_parser_ir_sentence_coordinates ... ok
test result: ok. 1 passed; 0 failed
```

### Regression / Acceptance Check

Ran the second focused acceptance test:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- cli_convert_with_ortho_annotations_emits_sentence_tags
```

Observed:

```text
test cli_convert_with_ortho_annotations_emits_sentence_tags ... ok
test result: ok. 1 passed; 0 failed
```

## Files Changed

- `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs`
- `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`
- `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`
- `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

## Implementation Notes

- Kept `OrthoAnnotationsBundle` ownership in `ab-aat-to-parser-ir` as requested.
- Reused `PreparedConverter::convert(..., ConversionOptions::default())` to derive parser-IR sentence rows before detector execution.
- Reconstructed sentence text from parser-IR `nodes[start..end]` using visible-text projection so detector byte offsets stay aligned to parser-IR sentence spans.
- Added a defensive out-of-bounds check for sentence `node_range`.
- Wired the CLI to build `HeuristicV1` with `ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()`.

## Self-Review

- The library API matches the task brief exactly.
- The new test proves coordinates come from parser-IR sentence spans, not from any separate plaintext reprojection.
- The CLI path uses the same repo-root and schema-loading conventions as the existing `convert` command.
- The implementation is scoped to the requested crate files only.

## Concerns

- Adding `ab-morph-analyzers` to this crate’s manifest causes Cargo to rewrite `ab-validator/Cargo.lock` when tests run. That file was outside the requested write-ownership list, so I did not include it in the code changes themselves. If the branch requires a clean worktree or a `--locked` workflow, this lockfile update will need an explicit decision.
