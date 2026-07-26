# ADR-Evidence Disposition Review (PR0 scaffold — ephemeral, not for commit)

**Date:** 2026-07-23 · Working artifact for Change B. Attach to the deletion PR; do not commit permanently.

Proposed disposition per registry-backed claim. **Human review owns the semantic verdict** (does each cited test's assertion entail the criterion?) and must additionally decompose ADR *criterion prose* to catch any conjunct that has **no** registry entry — this scaffold is registry-derived and cannot see those.

Legend: **D1** retained executable predicate (Kaocha/op) · **D1\*** retained predicate whose check is apparatus-named (preserve the domain validation, re-point the citation) · **D2** stored domain measurement · **D3** human source · **D4** apparatus self-check to retire.

## Tally

- **D1** — 159 (Kaocha/op-backed) — but see the two caveats below; some are really D4
- **D1\*** — 15 (apparatus-named domain checks — preserve validation, re-point)
- **D2?** — 2 (Track-R resource; confirm measurement)
- **D4** — 1 (+ the 0031/0034 apparatus rows below)
- 177 registry entries across 156 unique claims (21 extra entries = the 12
  multi-conjunct claims; each conjunct's bundle is dispositioned as its own row)

## Two caveats the automated pass cannot resolve (human review required)

1. **D1 means "a Kaocha test runs," not "a *domain* test runs."** ADR **0031**
   (governance validation) and ADR **0034** (typed evidence — being *superseded*
   by this change) have Kaocha tests that are **self-checks of the apparatus being
   deleted** (`adr_evidence_test`, `adr_governance_test`,
   `adr-0034-evidence-test`, the `adr-0031-*` fixtures). Their D1-looking rows are
   really **D4 (retire)** — except the subset of 0031 that proves the *surviving*
   `adr.clj` structural parser / relation / dependency checks, which stay D1.
   Split 0031 conjunct-by-conjunct; retire all of 0034's evidence-protocol
   conjuncts.
2. **"No registry entry" conjuncts are invisible here.** This scaffold is
   registry-derived, so it lists only conjuncts that already have evidence. The
   review must read each Accepted criterion's prose and confirm no conjunct is
   *missing* an entry (an omitted-conjunct finding).

## Needs human judgement (everything not plain D1)

| Claim | kind | disposition | note |
|---|---|---|---|
| ADR-0006-C4 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-project-cross-schema-invalid` — apparatus-named domain check; re-point/keep |
| ADR-0006-C5 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-figure-warning` — apparatus-named domain check; re-point/keep |
| ADR-0012-C2 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-project-valid-fixtures` — apparatus-named domain check; re-point/keep |
| ADR-0012-C3 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-upstream-rng` — apparatus-named domain check; re-point/keep |
| ADR-0012-C4 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-schematron-invalid-ids` — apparatus-named domain check; re-point/keep |
| ADR-0012-C5 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-enrichment-warning` — apparatus-named domain check; re-point/keep |
| ADR-0012-C5 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-figure-warning` — apparatus-named domain check; re-point/keep |
| ADR-0012-C6 | fixture-behavior | **D1*** | nixcheck `adr-evidence-tei-publication-sidecars` — apparatus-named domain check; re-point/keep |
| ADR-0022-C5 | operational-behavior | **D1*** | nixcheck `adr-evidence-aozora-history-audit-cli` — apparatus-named domain check; re-point/keep |
| ADR-0024-C4 | fixture-behavior | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0025-C1 | fixture-behavior | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0025-C2 | fixture-behavior | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0025-C3 | fixture-behavior | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0025-C4 | structural-invariant | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0025-C5 | fixture-behavior | **D1*** | nixcheck `parser-publication-evidence` — apparatus-named domain check; re-point/keep |
| ADR-0034-C3 | corpus-behavior | **D4** | apparatus self-check (ADR 0034 superseded) — RETIRE |
| ADR-0040-C2 | operational-behavior | **D2?** | Track-R resource run — confirm real measurement (0040/0041) |
| ADR-0040-C3 | structural-invariant | **D2?** | Track-R resource run — confirm real measurement (0040/0041) |

## Plain D1 (Kaocha/op-backed) — bulk, listed by ADR for completeness

- **ADR-0001**: C1, C1, C2, C3, C4, C4, C4, C5
- **ADR-0002**: C1, C2, C3, C4
- **ADR-0006**: C1, C1, C1, C2, C3, C6, C7, C8, C9
- **ADR-0007**: C1, C2, C3, C4
- **ADR-0008**: C1, C2, C2, C2, C2, C3, C4
- **ADR-0009**: C1, C2, C3, C4, C5, C5, C6, C7
- **ADR-0010**: C1, C2, C3, C4, C4, C5, C5, C5, C5
- **ADR-0011**: C1, C1, C1, C1, C2, C3
- **ADR-0012**: C1
- **ADR-0013**: C1, C2, C3
- **ADR-0014**: C1, C2, C3, C4
- **ADR-0015**: C1, C2, C3, C4, C5, C6
- **ADR-0016**: C1, C2, C3, C4
- **ADR-0017**: C1, C2, C3, C4, C5, C5
- **ADR-0018**: C1, C2, C3
- **ADR-0020**: C1, C2, C3, C4
- **ADR-0021**: C1, C10, C11, C12, C2, C3, C4, C5, C6, C7, C8, C9, C9
- **ADR-0022**: C1, C2, C3, C4, C6
- **ADR-0023**: C1, C2, C3, C4, C5
- **ADR-0024**: C1, C2, C3, C5, C6
- **ADR-0025**: C6
- **ADR-0029**: C1, C2, C3, C4, C5
- **ADR-0030**: C1, C2, C3
- **ADR-0031**: C1, C2, C3, C4
- **ADR-0032**: C1, C2
- **ADR-0033**: C1, C10, C11, C11, C11, C2, C3, C4, C5, C6, C7, C8, C9
- **ADR-0034**: C1, C2
- **ADR-0038**: C1, C2, C3
- **ADR-0039**: C1, C2, C3, C4, C5
- **ADR-0040**: C1
- **ADR-0041**: C1, C2, C3
- **ADR-0042**: C1, C2, C3

## What this changes for the parent plan

**Change A — the independent-check set is larger than three.** The plan listed
`validate-design-bundle` + `source-bundle-corpus` + `tei-profile-drift`. But 15
claims (ADR 0006, 0012, 0022, 0024, 0025) are backed by Nix checks *not* in the
Kaocha suite: 8 `adr-evidence-tei-*` (Schematron/RNG/ODD validation), 6
`parser-publication-evidence`, and `adr-evidence-aozora-history-audit-cli`. Their
**validation logic is domain** (TEI profile validation, parser-IR publication,
history-audit CLI) and must survive; the `adr-evidence-` prefix is apparatus
wrapping. Per check, decide: (a) the underlying validation is already a Kaocha
test → re-point the citation and drop the wrapper (D1); or (b) it is a genuinely
independent operation → keep it and **add it to Change A's standing gate list**.
This must be resolved before Change B deletes "focused-evidence Nix checks" — some
of those checks are the *only* executable evidence for real domain claims.

**Change D — the human source (ADR 0038-C1) is D3 as expected**, plus 0038 has
C2/C3 as plain D1. Confirm the freshness decision covers only the assessment.

## Residual human tasks (this scaffold cannot do them)

1. **Split ADR 0031 / retire ADR 0034** conjunct-by-conjunct (caveat 1).
2. **Prose-decompose all 156 Accepted criteria** to catch omitted conjuncts
   (caveat 2).
3. **Resolve each D1\*** — re-point to a Kaocha test, or promote to a Change A
   standing check.
4. **Confirm D2? (ADR 0040-C2/C3)** — Track-R; check whether the resource run
   holds a real measurement (→ D2, preserved) or only asserts structure (→ D1).
   Coordinate with the fenced Track-R (D5) audit.
5. **Verify each D1's test is a domain predicate**, not another apparatus
   self-check missed by caveat 1.

## Verdict

The mechanical bulk (≈159 of 177) is clean and low-risk. The **real work is ~35
rows**: 15 D1\* re-points (which also correct Change A), the 0031/0034 apparatus
split, 2 Track-R rows, and the prose-decomposition sweep for omitted conjuncts.
No conjunct is unaccounted for at the registry level; whether any is unaccounted
for at the *prose* level is the one thing only the human sweep can close.
