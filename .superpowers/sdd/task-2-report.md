# Task 2 report: ABC source-accountability protocols

## Outcome

Implemented four ABC-owned Draft 2020-12 JSON protocols and the closed, empty
`parser-rq-ignored-regions-v1` taxonomy. Every protocol rejects unknown
top-level fields; nested records are also closed. The work protocol restricts
decoding to non-lossy `utf-8`, `utf-8-bom`, or `windows-31j`, uses only
`decoded_utf8` coordinates, carries four interval vectors and exact counters,
and preserves the raw schema-v3 diagnostic capture as a logical blob reference.
The aggregate keeps work completeness independent from byte observations and
uses disjoint `ok`/`unavailable` branches: unavailable aggregates require
nonempty errors and prohibit trusted numeric observations.

## TDD evidence

RED was run after adding only the registration/closure tests and minimal valid
fixtures:

```text
bin/kaocha --focus ...schemas-are-closed-test --focus ...v1-taxonomy-is-empty-test
exit 2
2 tests, 2 assertions, 2 errors
FileNotFoundException: schemas/parser-rq-ignored-regions.schema.json
FileNotFoundException: data/parser-rq-ignored-regions-v1.json
```

After implementing and registering the protocols, focused GREEN was:

```text
4 tests, 16 assertions, 0 failures
```

This includes the schema-registration test, negative unknown-field test, empty
taxonomy test, and the live evidence-input read-catalog regression test.

## Verification

- `nix run .#schema-drift`: pass, `monorepo schema drift check ok`.
- `nix build ./abc#checks.x86_64-linux.clj-kondo`: pass.
- `scripts/comment-hygiene-check.sh`: pass.
- `git diff --cached --check`: pass.
- `nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests`: 1,233 tests,
  8,148 assertions, one pre-existing failure only.

The plan named `nix run ./abc#schema-drift`, but the ABC subflake does not expose
that app. The first invocation failed at flake output resolution. The root agent
authorized the live AGENTS.md entry point, `nix run .#schema-drift`, which passed.

The remaining broader-suite failure is
`abc.tools.filesystem-policy-test/production-filesystem-policy-test`, reporting
`isFile` and `getCanonicalFile` in `abc/src/abc/tools/parser_rq_capture.clj:59`.
That file and violation are present in `HEAD`, are outside Task 2, and were not
modified here. After staging the new schema files so the Nix git source could
see them, no Task 2 or evidence-catalog failure remained.

## Self-review

- All four roots and every introduced nested object use
  `additionalProperties: false`.
- The taxonomy is exact, closed, and empty; it does not introduce a generic rule
  engine or ignored-byte policy.
- No lossy encoding value, corpus index artifact, diagnostic authorization, R2
  observation, or machine-local storage path was introduced.
- Index entries are only a wire contract; no hand-authored production index was
  added.
- Aggregate unavailable status cannot carry numerator, denominator, uncovered
  count, or interval witnesses.
