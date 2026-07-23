# ADR Evidence Apparatus: Subtractive Simplification

**Date:** 2026-07-23 · **Status:** Draft (for review)

Remove the ADR 0034 typed-evidence apparatus and rely on machinery that already
exists (the ADR parser, the auto-discovered test suites, git). The change adds no
new evidence registry, generated result, or generic evidence schema; it does
create a superseding ADR (amending ADR 0031's evidence clause) and some gate
wiring.

## Diagnosis

~10,000 LOC of governance Clojure and ~300 evidence files check that decisions
hold, and inspection shows the model is almost entirely ceremony:

- In the observed 177-row registry, claim-kind and evidence-kind form five
  one-to-one pairs, so the evidence-kind field carries no information the
  claim-kind lacks. (The `claim-evidence-compatibility.edn` matrix itself is not a
  bijection — it permits `domain-interpretation → {expert-assessment,
  external-authority}` and shares `external-authority` with `external-semantics` —
  but nothing in the corpus uses that freedom.)
- All 177 `:expected` are `{:operator := :value true}` — no non-boolean bound.
- The 25 `operational`/`corpus` entries carry no measurement: their `observations`
  are `value:true` + an exit code (verified across all 25). This proves the
  boolean wrappers are redundant; it does not by itself classify the underlying
  command, some of which may consume a real domain measurement.
- The bootstrap snapshot base64-re-implements a git tree.
- The gate does not run the checks it gates on: `nix flake check --no-build`
  evaluates only; just `phase5-checkpoint` and `monorepo-adr-governance` build.

## Method: disposition each Accepted criterion, once

There is **no permanent evidence taxonomy** to encode — a fixed taxonomy that
dictates its own audit verifies itself. The four dispositions below are a
temporary, action-oriented review aid, not a stored classification.

Start the review from the **Accepted ADR criteria**, not the registry: registry
rows are evidence *entries*, so starting there cannot reveal a conjunct that has
no evidence at all. Enumerate every Accepted criterion, decompose its conjunctive
prose into individual conjuncts during review, and only then consult
`adr-evidence.edn` rows as lookup material. (The registry's 156 unique claim IDs
do exactly equal the 156 Accepted claim IDs, which is a useful cross-check — but
equality of IDs is not equality of conjuncts.) Give each conjunct one of four
dispositions:

1. **Retained executable predicate** — an ordinary test already in the Kaocha
   suite (or one of the independent operations below). Keep the test; delete its
   wrapper and registry row. Execution is enforced by running the suite; no
   per-claim result is stored. (Finding a test symbol records a reviewed
   migration disposition, not a proof that the test's assertions entail the
   conjunct — that entailment stays a human review judgment.)
2. **Preserved domain artifact + validator** — a real measurement under its own
   existing schema, plus the ordinary test that validates it. Keep both; delete
   the wrapper.
3. **Preserved human source** — a reviewed external document (e.g.
   `docs/evidence/external/custom-parser-ownership-assessment.md`). Keep it; delete
   the wrapper. Its freshness is Change D, not this step.
4. **Apparatus self-check to retire** — the row proves only the apparatus's own
   machinery. Delete outright.

A conjunct that is none of these is a finding, surfaced for decision. The
disposition table is **not committed**, but it must be **visible in the PR**
(description or review attachment): every claim/conjunct, its disposition, its
retained target, and any exception — so reviewers can confirm the whole corpus was
covered. Ephemeral, but auditable at review time.

## The four changes (kept separate, so failures and policy are attributable)

### A. Gate activation and operability (a decision, measured first)
The Kaocha suite already contains the tests that 87 of 94 descriptors execute.
The core independent operations are `validate-design-bundle` (**run** it; building
its app is insufficient), `source-bundle-corpus`, and `tei-profile-drift`;
`schema-drift` already runs in the root gate. **This set is not final:** the PR0
disposition review found 15 claims backed by non-Kaocha, apparatus-named checks (8
`adr-evidence-tei-*`, 6 `parser-publication-evidence`,
`adr-evidence-aozora-history-audit-cli`) whose *domain* validation must survive.
Each is resolved to either a re-pointed Kaocha test (drops out) or an added
standing check (joins this list) — so the final gate set is settled by the
disposition review, not assumed here.
Before placing these in every root validation, run a disposable cold/warm timing
and cache probe: the reviewer measured approximate closure sizes of ~74 MiB
(suite), ~3.3 GiB (`source-bundle-corpus`), ~6.3 MiB (TEI); the corpus build is
material. If the standing cost exceeds an agreed budget, keep that operation as a
CI, scheduled, or explicit release check rather than in the root gate. Rust is out
of scope: no descriptor invokes Rust; a later Rust-only gap is a separate
enforcement change.

### B. Apparatus deletion and test unwrapping (structural)
Delete only self-consumed projections, capture files, tracing, and their schemas
(Inventory below). Three obligations, each with an explicit verification condition:

- **Unwrap retained tests.** ~30 test files import the runtime-inputs helper and
  ~186 sites load descriptors or validated read-traces. For each retained test,
  lift the domain assertion out of `with-validated-read-trace!`, swap
  evidence-owned temp dirs for ordinary ones, and delete tracing-only assertions.
  Confirm every retained focused test still appears in Kaocha's post-unwrapping
  test plan, and execute the suite.
- **Unwrap `evidence_io` repo-wide, not just `source_bundle.clj`.** 12 consumer
  files import it across 33 `record-read!` sites (excluding the definition); after
  `adr_evidence_runtime_inputs` is deleted, 11 consumers remain to edit. B is a
  repository-wide `record-read!` removal, verified by **zero remaining `evidence-io`
  references**.
- **Rewrite the 8 breaking ADR citations.** Eight Accepted criteria cite
  `test/abc/tools/adr_evidence_capture_test.clj`, which B deletes (ADRs 0002,
  0007, 0023, 0024, 0025, 0030, 0032, 0038). Rewrite each prose citation to its
  retained predicate/source, then run the strict ADR validator — otherwise its
  existing path-existence check reports 8 missing-evidence-path problems.

### C. Governance-mode and duplicate-wiring cleanup (structural)
Collapse governance to one strict validator with ordinary nonzero failure. Remove
the legacy/audit modes, `validate-adrs-legacy`, `validate-repository-legacy`,
`audit-only-problem-kinds`, `--workspace-root`, audit-exits-zero, and JSON report.
The gate prints each problem and exits nonzero when `(seq problems)` — an explicit
CLI check, **not** a Clojure `assert` (which `*assert*` can disable and which gives
poor error output). Pinned debt is already empty; no waiver code. Drop the
root derivation's workspace copy, temp-git init/commit, report emission, and
jq/migration-debt comparison. Once `--workspace-root` is gone, compare the
component and root governance derivations; if they enforce the same validator,
keep **one** canonical derivation rather than two simplified copies.

### D. Human-source freshness policy (decide **before** the relevant part of B)
The ownership assessment records "Assessment date: 2026-07-12 / Review after:
2026-10-12" — a continuing obligation a one-time acceptance review cannot detect
going stale. This must be resolved *before* B deletes `governance-as-of.edn` and
the ownership test, or the "separate" changes are still coupled. Choose:
- **(a)** make the assessment immutable historical evidence, amend ADR 0038 to
  drop the continuing-freshness claim, and delete the epoch file + ownership test;
  or
- **(b)** retain (or replace with) one small domain-specific expiry check keyed to
  `Review after`, and keep the minimal time input it needs.

Related but **independent** decision: `adr.clj`'s evidence-path check recognizes
only `test/`, `fixtures/`, and `nix/` roots — not `docs/evidence/external/*.md`. So
human-source citations are not machine-path-checked today. This is orthogonal to
the freshness choice: even under D(a) (historical evidence) a rewritten citation may
warrant path-existence checking, while D(b) could use a direct domain expiry check
without extending ADR path parsing at all. Decide separately: (i) whether `adr.clj`
recognizes the `docs/evidence/external/` root; (ii) the freshness policy above.

## Deletion Inventory (complete)

**Source namespaces (+ their tests):** `adr_evidence.clj` (sole caller is
governance), `adr_evidence_capture.clj`, `adr_evidence_bundle.clj`,
`adr_evidence_register.clj`, `adr_evidence_runtime_inputs.clj`,
`adr_evidence_observation_catalog.clj`, `adr_evidence_operational.clj`,
`adr_evidence_bootstrap.clj`, `adr_evidence_inventory.clj`,
`adr_claim_migration.clj`, `evidence_io.clj`, `evidence_output.clj`, plus
`evidence_test_support.clj`. After the `*read-trace*` binder is gone, every
`record-read!` is the identity — unwrap it and delete the dead branch in
`source_bundle.clj`.

**Data / artifacts:** `docs/evidence/adr-capture/`, `adr-inputs/`, `adr-runs/`,
`adr-entries/`, `adr-bootstrap/`; `docs/adr/adr-evidence.edn`,
`adr-claim-migration.edn`, `adr-claim-migration-baseline.json`,
`claim-evidence-compatibility.edn`; `data/adr-evidence/`;
`docs/reports/adr-claim-migration-inventory.json`,
`docs/reports/adr-evidence-migration.json`; `nix/adr-problem-identities.jq`.
**Schemas:** `adr-evidence-run`, `adr-evidence-bootstrap`,
`adr-claim-migration-baseline`. **Wiring:** the `adr-evidence-*` / `adr-claim-migration`
`deps.edn` aliases and focused-evidence Nix checks; the bootstrap, without
replacement (git identifies historical states; nothing consumes a promotion proof).

**Conditional on Change D:** `governance-as-of.edn` and
`parser_ownership_assessment_evidence_test` are deleted **only under D(a)**; under
D(b) the epoch input and a domain expiry check are retained. Resolve D first.

**Decision (resolved):** the closed claim-kind vocabulary is dropped — claim-kind
text may stay in prose but is unparsed and unenforced.

**Preserved:** `adr.clj` (leaner; its existing check that prose-cited `test/`,
`fixtures/`, `nix/` paths exist is the only standing machine signal — it does not
cover human sources; whether to add the `docs/evidence/external/` root is decision
(i) above, independent of the freshness choice), a thin strict governance CLI,
`docs/evidence/external/` documents, and any genuine domain measurement under its
own schema.

**Quantified:** ≥ **8,741 LOC** directly countable (the ~7,680 stack + 424 for
`adr_evidence` + 637 for `evidence_io`/`evidence_output`/support and tests), plus
uncounted `record-read!` unwrapping and the mode/derivation collapse.

## Ledger (load-bearing figures; each command copy-runnable)

```sh
# 177 evidence entries (grep -c returns 1: the EDN is one line)
grep -o ':claim-id "[^"]*"' abc/docs/adr/adr-evidence.edn | wc -l
# 156 unique claim IDs
grep -o ':claim-id "[^"]*"' abc/docs/adr/adr-evidence.edn | sort -u | wc -l
# 177/177 expected == true
grep -o ':value true' abc/docs/adr/adr-evidence.edn | wc -l
```
Verified separately: all 25 operational/corpus bundles are `value:true` + exit
code; `adr_evidence.clj`'s only production caller is governance; `evidence_output`'s
only caller is capture; `*read-trace*`'s only binder is the runtime-inputs helper.

## Change E — Malli consolidation (separate behavior-preserving cleanup)

A separate commit series, run after Change B. **Boundary:** Malli removes real code
**only in surviving Clojure-owned structural validation** — never the semantic
audit, gate execution, filesystem/hash checks, freshness policy, the ADR
graph/reciprocity rules, or cross-language wire artifacts already governed by JSON
Schema / TEI / SHACL. Full tasks, ordering, behavior contract, and the
registry-architecture that avoids a dependency cycle are in
`2026-07-23-malli-consolidation-design.md`.

## Sequencing

1. Resolve **D** and predeclare the operability budget.
2. Enumerate Accepted ADR criteria; verify the 156-ID equality second.
3. Record conjunct-level dispositions **visibly in the PR**.
4. Rewrite the 8 soon-to-break ADR citations.
5. Activate and **measure** the gate (**A**).
6. Perform **B**, including all `evidence_io` callers.
7. Perform **C**.
8. Handle **E** (Malli) as a separate behavior-preserving cleanup.

## Falsifiers

- An Accepted conjunct fits none of the four dispositions.
- A retained test, after unwrapping, no longer executes in the Kaocha plan.
- The standing gate's measured cost exceeds the agreed operability budget (then a
  heavy operation moves to CI/scheduled/release, not root).
- A boolean-wrapper deletion would remove a real domain measurement (then that
  measurement is preserved under its own schema — disposition 2).
