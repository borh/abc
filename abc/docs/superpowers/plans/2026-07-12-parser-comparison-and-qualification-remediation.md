# Parser Comparison and Qualification Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Record custom-parser ownership, produce a preregistered unbiased comparison of existing parsers, and qualify the custom parser through separate admission/release gates.

**Architecture:** ABC owns the policy and citations; `ab-validator` owns measurements. Comparison is a neutral research artifact, not a production-ownership contest. The project-owned custom parser is admitted and release-qualified only through exact, machine-readable predicates.

**Tech Stack:** Rust measurement tooling in `ab-validator`, Clojure evidence/admission validation in `abc`, JSON/EDN reports, Nix checks.

## Global Constraints

- Follow `2026-07-12-remediation-program-sequencing.md`; begin after governance
  enforcement and reserve ADRs 0037 and 0038.
- Custom-parser ownership is independent of comparison rankings.
- Preregister candidate scope, metrics, denominators, adapter treatment, and analysis before inspecting refreshed results.
- Report native and adapter-normalized behavior separately.
- No unqualified aggregate winner score.
- Parser comparison cannot satisfy admission or release qualification.
- Phase 4 is integrated in `main` at `ac2be926`. Treat that commit as the
  custom-parser baseline candidate, not yet a frozen baseline. Its scientific
  checkpoint and cargo check pass, but cargo fmt and clippy remain red; do not
  freeze Task 1 revisions until those integrated Rust/Nix checks also pass.
  Phase 4 owns the AAT schema, mapping/conversion audit, integration tests, and
  custom-parser coordinates that this plan must measure rather than overwrite.

---

## File Structure

- `ab-validator/docs/studies/aozora-parser-comparison-preregistration.md` — frozen study protocol.
- `ab-validator/schemas/parser-comparison-study.schema.json` — machine-readable study/report contract.
- Existing comparator/adapters/report crates — measurements.
- `abc/data/parser-evidence-citations.edn` and parser evidence validator — typed citations.
- `abc/docs/adr/0037-custom-parser-ownership-and-neutral-comparison.md` — corrective ADR.
- `abc/docs/adr/0038-custom-parser-release-qualification.md` — qualification ADR.
- `ab-validator/docs/superpowers/reports/...` — immutable result artifacts.

### Task 1: Freeze the research protocol

- [ ] Verify integrated commit `ac2be926` with the `ab-validator` cargo
  check/clippy/fmt and focused Phase 4 checkpoint tests; record the observed
  results alongside the frozen custom-parser baseline.
- [ ] Inventory every serious existing parser and write explicit inclusion/exclusion rules independent of results.
- [ ] Define research questions, corpus, revisions, native/adapted modes, exact denominators, failure/timeout/silent-drop rules, uncertainty, and missing-data handling.
- [ ] Define per-axis outputs for construct coverage, fidelity, robustness, diagnostics, spans, performance, maintenance, packaging, and license.
- [ ] Ban an unqualified aggregate score; require sensitivity analysis for any use-case-specific weighting.
- [ ] Validate the preregistration against its schema, hash it, and commit before executing refreshed measurements.
- [ ] Commit `docs(parser): preregister neutral parser comparison`.

### Task 2: Make reports preserve native/adaptor attribution

- [ ] Add failing Rust tests showing a source-lexer fallback or mapper cannot be reported as native parser capability.
- [ ] Extend the report IR with `measurement_mode`, parser revision, adapter revision, corpus hash, numerator, denominator, missingness, and caveats.
- [ ] Ensure every candidate emits per-axis results, including failures and non-comparable axes.
- [ ] Run the relevant `ab-validator` cargo tests/checks through Nix.
- [ ] Commit `feat(parser-study): separate native and adapted measurements`.

### Task 3: Measure existing parsers under the frozen protocol

- [ ] Materialize every included parser revision and adapter from the preregistration; record build failures as results rather than silently excluding candidates.
- [ ] Run native and adapted modes independently over the pinned corpus, capturing raw per-work outputs before aggregation.
- [ ] Verify every run records parser, adapter, corpus, environment, timeout, and protocol hashes.
- [ ] Commit immutable raw run manifests and hashes with `data(parser-study): record preregistered existing-parser runs`.

### Task 4: Generate and validate immutable comparison reports

- [ ] Aggregate only with the preregistered formulas and denominators; reject any result row missing raw provenance.
- [ ] Generate machine-readable and narrative reports separating observations, interpretations, limitations, missing data, and use-case sensitivity.
- [ ] Add a drift test proving the reports regenerate byte-identically from the raw run manifests.
- [ ] Hash and register reports as corpus measurement, benchmark, or comparator evidence—never admission evidence.
- [ ] Commit `docs(parser): publish preregistered existing-parser comparison`.

### Task 5: Measure the custom parser on shared instruments

- [ ] Run the project-owned parser through every applicable frozen instrument without changing existing-parser results or metric definitions.
- [ ] Mark owned-contract axes lacking an analogue as non-comparable; do not assign competitors zero.
- [ ] Publish the custom-parser appendix with the same raw provenance, uncertainty, and limitation fields.
- [ ] Add sensitivity comparisons that can falsify custom-parser performance or fidelity claims without revisiting ownership.
- [ ] Commit `docs(parser): add custom-parser comparison baselines`.

### Task 6: Repair ADR semantics and ownership

- [ ] Create ADR 0037 stating project-owned custom-parser ownership independent of rankings and narrowing ADR 0030 to provenance/reusable-source conclusions.
- [ ] Preserve prior reports as historical evidence; do not rewrite inconvenient results.
- [ ] Add evidence-validator tests rejecting comparison citations when used for admission or release claims.
- [ ] Run ABC parser-evidence/governance tests and commit `docs(adr): separate parser ownership from comparison`.

### Task 7: Define and execute release qualification

- [ ] Pin the qualification corpus and predeclare fatal, span, silent-drop, diagnostic, IR, publication-structure, time, memory, and timeout predicates.
- [ ] Emit exact observed/expected values and derived verdicts; verify 0.969 fails a 1.0 predicate.
- [ ] Admit the exact custom parser/adapter/mapping tuple through ADR 0023 separately from release qualification.
- [ ] Create ADR 0038 only after every release predicate passes; failing results leave it Proposed.
- [ ] Run ABC and ab-validator focused Nix checks; require exit 0 with cargo check, clippy, fmt, tests, and ABC parser-evidence governance successful.
- [ ] Run root `just validate-migration`; require exit 0 from all root and component checks.
- [ ] Commit `feat(parser): qualify project-owned parser for release` only if the gate passes.

### Task 8: Establish falsifiable maintenance economics

- [ ] Add a quarterly report template covering upstream review, selective ports, security, missed fixes, fork-only defects, planned divergence, remeasurement, and maintainer availability.
- [ ] Define focused-session duration and a disposable trial-merge procedure.
- [ ] Record the trial merge as a re-derivable expert estimate; record reproducible timings/conflict counts separately as benchmarks.
- [ ] Add typed evidence expiry/review dates and explicit decision-revisit predicates.
- [ ] Commit `docs(parser): add custom-parser maintenance evidence protocol`.
