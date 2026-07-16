# P4A1 Task 4 report

Status: complete.

Implemented the ABC-owned classified-source protocol boundary:

- three closed Draft 2020-12 schemas for policy, work ledger, and capture generation;
- the canonical `ab-aozora` v1 policy, with all 36 Task 2 cases mapped onto the
  live Task 3 binary plain provenance and no wildcard role or disposition;
- exact semantic/preserved target identity
  `{artifact_ref,value_hash,relation}`, where relation is `emits|preserves`;
- conditional target, structural-witness, normalization-proof, and opaque-source
  evidence requirements;
- duplicate policy-row rejection and a JCS-derived capture `generation_ref`
  shared by all four generation members;
- validator registration, immutable fixtures, and canonical schema/policy hash
  assertions.

TDD evidence:

- RED: focused Kaocha failed compiling because
  `parser-rq-classified-source-policy-errors` did not exist.
- GREEN: focused Kaocha passes with 1 test, 19 assertions, 0 failures.

Verification:

- `cd abc && bin/kaocha --focus abc.tools.validate-design-bundle-test/parser-rq-classified-source`
- `nix run .#schema-drift`
- `nix build ./abc#checks.x86_64-linux.clj-kondo`
- `scripts/comment-hygiene-check.sh`
- `git diff --check`

Concern: the 36-row policy freezes the empirically observed live matrix. The
three declared but unobserved final node tags remain represented by their live
classifier outcomes (`warichu` as a directive and `framed` as `BlockOpen`);
the policy does not fabricate unavailable `NodeKind` observations.
