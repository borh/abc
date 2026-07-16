# Task 3 report: interval algebra with Hegel properties

## Status

Complete. The crate now exposes bounded half-open interval construction,
normalization, subtraction, intersection, and checked total length.

## TDD evidence

Initial RED command:

```text
cd ab-validator
cargo test -p ab-parser-rq-source-accountability --test interval_properties
```

The command exited 101 with the expected compile failure:

```text
error[E0432]: unresolved import `ab_parser_rq_source_accountability::interval`
could not find `interval` in `ab_parser_rq_source_accountability`
```

The checked-overflow regression was also mutation-checked. With `checked_add`
temporarily replaced by `wrapping_add`, this command exited 101:

```text
cargo test -p ab-parser-rq-source-accountability --test interval_properties \
  total_length_rejects_u64_overflow
```

The assertion that `total_len(&[maximal, maximal]).is_err()` failed. Restoring
`checked_add` made the test pass.

Final GREEN command:

```text
cargo test -p ab-parser-rq-source-accountability --test interval_properties
```

Result: 7 passed, 0 failed.

## Property evidence

Hegel runs 100 generated cases per property by default. The focused target
contains three Hegel properties:

- normalization is invariant under input reversal and duplication;
- normalization is idempotent and produces sorted, disjoint, maximal unions;
- subtraction and intersection partition source length and remain disjoint.

The generator draws a `u16` bound and two bounded `u16` endpoints per interval,
orders the endpoints, and widens them to `usize`. Empty intervals remain in the
generated input domain and are removed by normalization.

## Verification

All commands exited 0:

```text
cargo fmt --all -- --check
cargo test -p ab-parser-rq-source-accountability --test interval_properties
cargo test -p ab-parser-rq-source-accountability
cargo clippy -p ab-parser-rq-source-accountability --all-targets -- -D warnings
../scripts/comment-hygiene-check.sh
git diff --check
```

The full crate test run passed 8 integration tests: 1 Hegel smoke test and 7
interval tests. The Rust Hegel API matched the plan; bounded integer methods did
not require importing the `Generator` trait, so that unused import was omitted.

## Review

Independent review found no Critical or Important issues and approved the task.
Its sole Minor observation was the lack of a direct checked-overflow regression;
the final change adds and mutation-verifies that regression.

## Scope

Only pure Rust interval value logic and its tests were added. No file I/O,
diagnostic authorization, taxonomy, corpus logic, or analyzer behavior is
included.
