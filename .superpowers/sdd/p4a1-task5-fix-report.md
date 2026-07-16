# P4A1 Task 5 review-fix report

## Result

- Replaced the `ForwardAttr` authorization catch-all with an explicit mapping
  for every currently defined attribute variant. The mandatory residual arm
  now returns `None`, so a future parser variant remains unclassified until an
  ABC policy decision admits it.
- Added a unit characterization covering every current `ForwardAttr` variant,
  including the specialized `bouten` and `combine_upright` mappings. Rust's
  non-exhaustive enum boundary prevents constructing a hypothetical future
  variant in this downstream crate; the residual `None` arm is therefore the
  executable fail-closed assertion for that case.
- Replaced the tautological interval properties with bounded independent
  byte-set oracles. Generated `文`/`｜` sequences independently define the
  expected semantic and opaque byte sets, exercise empty input, adjacency,
  alternation, duplicates, and merged parser spans, and compare those sets to
  the projected fact unions.
- The projection remains in the existing fused fold and output behavior is
  unchanged.

## Verification

From `ab-validator/`:

```text
cargo test -p ab-aozora-pipeline --test classified_source_facts
7 passed, 0 failed

cargo test -p ab-aozora-pipeline --lib
144 passed, 0 failed

cargo clippy -p ab-aozora-syntax -p ab-aozora-pipeline --lib -- -D warnings
passed

cargo fmt --all -- --check
passed

../scripts/comment-hygiene-check.sh
comment-hygiene: all criteria pass
```

An exploratory `--all-targets` clippy invocation also reached an unrelated
pre-existing lint in `tests/plain_provenance.rs`; the library targets changed
here are clean under `-D warnings`.
