# P4A1 Task 7 upstream duplicate-fact fix

## Result

Fixed the duplicate classified-source candidate at the rewrite/classifier fact
boundary. A streamed tortoise has `RecoveredVerbatim` delimiters and a `Text`
interior. Accent decomposition's composed offset map then rebased both
delimiter facts to the same whole decoded-source interval, so capture correctly
rejected the duplicate.

The final implementation reconciles facts at the sanitizer/classifier boundary.
Each authenticated `AccentDecompositionApplied` span corresponds to the
sanitizer's one whole-tortoise `MapEdit`. Classifier recovery fragments contained
by that interval are replaced by exactly one `RecoveredVerbatim` candidate for
the map-edit identity. This also covers runs split by a newline or interrupted
by a recognized nested child.

Literal tortoise forms have no accent diagnostic, so they retain the
classifier's delimiter/body provenance boundaries. Recognized nested facts are
not suppressed. Normalized text, serialized Aozora source, diagnostics, and
rendered output are unchanged.

This is not capture-level or global deduplication. A fold-level characterization
emits the same classified span twice as two independent candidates and proves
that both facts remain present.

## TDD evidence

RED:

- `rewritten_tortoise_emits_one_recovery_fact_for_map_edit_identity`
  failed with 2 recovered facts instead of 1. The facts covered the opening and
  closing delimiters separately.
- The independently-emitted multiplicity test passed before the fix, pinning
  the no-dedup side of the contract.

GREEN:

- Focused live tortoise regression passed.
- Rewritten tortoise regressions split by a newline and by a recognized nested
  ruby passed with one recovered fact for the complete rewrite interval; the
  ruby fact remains present.
- A non-rewritten literal tortoise retains two recovered delimiter facts and
  its accepted-text body rather than being falsely collapsed.
- Independent duplicate emission regression passed with two retained facts.
- `plain_provenance` passed and pins the literal tortoise's delimiter/body
  provenance while normalized/serialized/rendered outputs remain byte-identical.
- Full `cargo test -p ab-aozora-pipeline` passed: 145 unit tests, all integration
  suites, properties, and doc tests.
- AAT capture no longer reaches the duplicate-entry rejection; its focused test
  proceeds to the expected in-progress Task 7 policy-hash drift fail-stop.
- Pipeline library clippy and the changed `classified_source_facts` target pass
  with `-D warnings`; comment hygiene and `git diff --check` pass.

`cargo clippy --all-targets` still reaches the two pre-existing
`plain_provenance.rs` warnings recorded by Task 5 (`items_after_statements` and
`usize` to `u32`). They are unrelated to this fix and were not broadened into
the commit.
