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

---

# Task 1 Follow-up Report: Review Findings Fixes

## Review Findings Addressed

1. Important: `ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations` now populates per-sentence `SentenceSpan::char_offset` using cumulative visible-text character offsets derived from parser-IR sentence text.
2. Minor: added a deterministic CLI smoke test for `detect-ortho-annotations` that exercises subcommand dispatch without requiring a real dictionary by forcing `AB_VIBRATO_DICT` to a nonexistent path and asserting the expected failure message.

## RED/GREEN Evidence for `char_offset`

### Red

Added `detect_orthographic_annotations_populates_sentence_char_offsets` to:

- `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

Ran:

```bash
cargo test --manifest-path /home/bor/Projects/soranoha/.worktrees/feat-parser-ir-followups-plan/ab-validator/Cargo.toml -p ab-aat-to-parser-ir --test integration detect_orthographic_annotations_populates_sentence_char_offsets
```

Observed failure:

```text
test detect_orthographic_annotations_populates_sentence_char_offsets ... FAILED
assertion `left == right` failed
  left: [0, 0, 0]
 right: [0, 8, 16]
```

### Green

Updated `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs` to track cumulative character offsets while building sentence spans.

Re-ran:

```bash
cargo test --manifest-path /home/bor/Projects/soranoha/.worktrees/feat-parser-ir-followups-plan/ab-validator/Cargo.toml -p ab-aat-to-parser-ir --test integration detect_orthographic_annotations_populates_sentence_char_offsets
```

Observed:

```text
test detect_orthographic_annotations_populates_sentence_char_offsets ... ok
test result: ok. 1 passed; 0 failed
```

## Tests Run and Outputs

```bash
cargo test --manifest-path /home/bor/Projects/soranoha/.worktrees/feat-parser-ir-followups-plan/ab-validator/Cargo.toml -p ab-aat-to-parser-ir --test integration detect_orthographic_annotations_populates_sentence_char_offsets
```

```text
test detect_orthographic_annotations_populates_sentence_char_offsets ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 44 filtered out
```

```bash
cargo test --manifest-path /home/bor/Projects/soranoha/.worktrees/feat-parser-ir-followups-plan/ab-validator/Cargo.toml -p ab-aat-to-parser-ir --test integration detect_orthographic_annotations_uses_parser_ir_sentence_coordinates
```

```text
test detect_orthographic_annotations_uses_parser_ir_sentence_coordinates ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 44 filtered out
```

```bash
cargo test --manifest-path /home/bor/Projects/soranoha/.worktrees/feat-parser-ir-followups-plan/ab-validator/Cargo.toml -p ab-aat-to-parser-ir --test integration cli_detect_ortho_annotations_reports_missing_dictionary
```

```text
test cli_detect_ortho_annotations_reports_missing_dictionary ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 44 filtered out
```

## Files Changed

- `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs`
- `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- `.superpowers/sdd/task-1-report.md`
