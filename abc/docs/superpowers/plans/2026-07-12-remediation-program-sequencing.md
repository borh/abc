# Logical Remediation Program Sequencing

Date: 2026-07-12
Status: Execution ledger for review

## Required Order

```text
shared source-assertion foundation
             |
governance audit -> governance enforcement
             |                 |
             |       parser comparison / qualification
             |
        +----+----+
        |         |
      rights    temporal
        |         |
        +----+----+  (each mapping family may arrive independently)
             |
          identity
```

The foundation and governance audit mode must exist before rights containment
or temporal schema work begins. Rights and temporal may execute concurrently
after that point. Identity may consume either completed generation mapping and
must not wait for both. Parser work begins after governance enforcement and is
otherwise independent.

Every intermediate commit keeps the existing required checks green. Temporary
semantic containment states use explicit publication blockers rather than
shipping incomplete publication data.

## In-Flight Worktree Integration Gates

Two pre-existing worktrees own code that later remediation tasks would touch:

| Branch | Worktree | Owned overlap | Required integration gate |
| --- | --- | --- | --- |
| `feat/parser-fork-phase4` | `.worktrees/parser-fork-phase4` | `ab-aat-to-parser-ir`, AAT schemas/mapping, parser coordinates, Phase 4 admission evidence | Merge Phase 4 and establish a green Rust/Nix baseline before freezing parser-study revisions or executing parser remediation Tasks 2–8. |
| `fix/aozora-evolution-divergences` | `.claude/worktrees/tokenizer-full-corpus-comparison` | `aozora_ingest.clj`, ingest tests, simulation divergence/failure behavior | Merge simulation hardening and establish green unit/simulation baselines before rights Task 4 or temporal Task 3 changes ingest integration. |

The shared foundation, governance plan, rights containment/investigation/schema
tasks, temporal characterization/schema tasks, and identity inventory may
proceed without waiting for these merges because their owned files do not
overlap. Do not cherry-pick partial uncommitted Phase 4 work or reproduce the
simulation fixes in remediation branches.

After either prerequisite merges, rebase or recreate the relevant remediation
worktree from the integrated main branch and rerun its baseline checks before
continuing. The simulation branch's two-tier failure taxonomy and
build/reconcile/write safety behavior become characterization requirements;
the remediation must preserve them.

## ADR Number Reservation

| ADR | Owner |
| --- | --- |
| 0033 | typed evidence and lifecycle closure |
| 0034 | rights assessment and external statements |
| 0035 | temporal knowledge state |
| 0036 | identity lattice and generation equivalence |
| 0037 | custom-parser ownership and neutral comparison |
| 0038 | custom-parser release qualification |

No parallel worker may allocate another ADR in this range. New decisions start
at ADR 0039 unless this ledger is amended first.

## Shared Foundation Task

**Files:** Create `abc/schemas/source-assertion.schema.json`,
`abc/src/abc/tools/source_assertion.clj`, and
`abc/test/abc/tools/source_assertion_test.clj`; update
`abc/schemas/schema-contracts.json`.

**Interface:** `validation-errors` accepts a value and returns deterministic
problem maps. A valid value has exactly `source`, `field`, `lexical_value`, and
`snapshot_hash`; `lexical_value` is string or null and `snapshot_hash` is a
formatted SHA-256 value.

- [ ] Write failing schema and Clojure tests for valid present/missing lexical
  values, missing provenance, extra fields, and malformed hashes.
- [ ] Run `cd abc && bin/kaocha --focus abc.tools.source-assertion-test`; expect
  failure because the schema/module do not exist.
- [ ] Implement the schema and validator without rights or temporal semantics.
- [ ] Register the schema and run the focused test; expect zero failures.
- [ ] Run `cd abc && clojure -M:abc/validate-design-bundle`; expect exit 0 and
  the source-assertion schema listed as validated.
- [ ] Commit only foundation files with
  `git commit -m "feat(metadata): add shared source assertion contract"`.

## Program Completion Gate

After all applicable plans finish, run `just validate-migration`. Completion
requires exit 0, with every nested Nix no-build check, Python quality check,
Nix formatting check, schema drift check, and component flake evaluation
reported successful.
