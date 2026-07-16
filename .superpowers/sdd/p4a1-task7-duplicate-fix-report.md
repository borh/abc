# P4A1 Task 7 upstream duplicate-fact fix

## Result

Fixed the duplicate classified-source candidate at its classifier emission
source. A streamed tortoise form is an unrecognized source form as a whole;
its delimiters were previously `RecoveredVerbatim` while its interior text was
`Text`. Accent decomposition's composed offset map then rebased both delimiter
facts to the same whole decoded-source interval, so capture correctly rejected
the duplicate.

The tortoise streaming path now marks its complete contiguous span
`RecoveredVerbatim`. `push_plain` consequently coalesces the open, body, and
close events into the single classified span that the source form represents.
Normalized text, serialized Aozora source, diagnostics, and rendered output are
unchanged.

This is not capture-level or global deduplication. A fold-level characterization
emits the same classified span twice as two independent candidates and proves
that both facts remain present.

## TDD evidence

RED:

- `streamed_tortoise_recovery_emits_one_fact_for_its_one_classified_span`
  failed with 2 recovered facts instead of 1. The facts covered the opening and
  closing delimiters separately.
- The independently-emitted multiplicity test passed before the fix, pinning
  the no-dedup side of the contract.

GREEN:

- Focused live tortoise regression passed.
- Independent duplicate emission regression passed with two retained facts.
- `plain_provenance` passed and pins `〔literal〕` as one recovered span while
  its normalized/serialized/rendered outputs remain byte-identical.
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
