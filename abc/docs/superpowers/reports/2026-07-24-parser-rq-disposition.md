# Parser-RQ Disposition Table — ADRs 0039–0042 under the warm-capability contract

Date: 2026-07-24
Companion to `2026-07-24-system-simplification-audit.md` (F1). Decision:
**retain a warm requalification capability; no mass retirement.** This table
classifies every claim and apparatus component so a future retirement (after
the next full-Aozora qualification, or an explicit no-further-qualification
decision) starts from dispositions, not archaeology.

Classifications: **live** (product pathway or maintained
release/operations capability — stays), **frozen** (historical verdict —
pinned artifact, no machinery needed beyond integrity checking),
**hybrid** (frozen fact + live verifier), **investigate** (possible
obsolete apparatus; not proven unnecessary for both the live kernel and the
historical audit path, so it stays until proven).

## Adopted capability contract (from review)

| Capability | Promise | Current provider | Status |
|---|---|---|---|
| Historical audit | One command verifies accepted P5 closed membership, hashes, decision binding — no re-measurement; minutes | `parser-rq-campaign verify-promotion` against `docs/adr/decisions.edn` (Recipe 1 in `docs/parser-rq-runbook.md`), CI-pinned as the `parser-rq-p5-promotion-audit` check | met — after repair. Post-review audit found verify-promotion still parsed the retired Markdown `Status:` grammar, so the exact committed audit failed on main while synthetic-fixture tests stayed green; fixed by resolving the dependency decisions by slug from decisions.edn (`--decisions` replaces `--adr-0040`/`--adr-0041`). `verify-capture` is not a provider here — it checks capture integrity, not decision binding. |
| Bounded requalification smoke | candidate → capture → projection → evaluation → promotion on a small fixture in CI | abc checks `parser-rq-campaign-site`, `parser-rq-campaign-orchestrator`, `parser-rq-core-attempt`, `parser-rq-admission-promotion-smoke`; root `parser-rq-production-wiring` | met |
| Full requalification | Operator starts a new qualification from a clean Linux/cgroup-v2 host within one working day, from a documented runbook | `docs/parser-rq-runbook.md` (Recipe 2), `bin/parser-rq-campaign-capture.sh`, `config/parser-rq-site.example.json` | **documented, awaiting host exercise** — Recipe 2 now carries the exact command sequence (preflight → candidate → seal-readiness/authorize → capture → compose/verify → admission → evaluate/publish → project → promote); "met" only after a real host run |
| Scope expansion | Candidate/corpus/predicate/instrument identities rotate without reconstructing the protocol from git history | campaign identity operations + ADR 0040 c3 / 0041 c1 rotation rules | met |

## Claim dispositions

### custom-parser-release-qualification (0039, Accepted)

| Claim | Statement (short) | Disposition |
|---|---|---|
| c1 | Qualification corpus pinned and hash-addressed; tampering detected | **hybrid** — corpus facts frozen (`data/parser-release-qualification-corpus.edn`); hash verification live (historical audit) |
| c2 | Predicate set predeclares nine dimensions with exact thresholds | **hybrid** — predicate facts frozen (`data/parser-release-qualification-predicates.edn`); shape verification live |
| c3 | Predicate evaluation exact; absent observation never passes | **live** — the evaluation kernel (`parser_release_qualification.clj`) |
| c4 | Comparison/neutral citations structurally rejected as release evidence | **live product pathway** — `parser_evidence.clj` release-class boundary; `citable-hashes` in every snapshot |
| c5 | Release-qualified only when every predicate passes for an admitted build | **live** — the release gate |

### process-tree-memory-qualification (0040, Accepted)

| Claim | Statement (short) | Disposition |
|---|---|---|
| c1 | Predicate 8: process-tree cgroup memory, 2 GiB, zero swap, content-addressed wrapper | **live** — instrument for any future run (`parser_rq_resource.clj`) |
| c2 | Hinoki smoke run recorded measured + right-censored workloads | **frozen** — `docs/superpowers/reports/2026-07-17-parser-rq-resource-hinoki-smoke.json` is a historical verdict; the citing test is its integrity check |
| c3 | Rotation never relabels old observations; P5 recaptured all nine | **live protocol rule** (identity rotation) over **frozen** P5 facts |

### parser-release-instrument-bindings (0041, Accepted)

| Claim | Statement (short) | Disposition |
|---|---|---|
| c1 | Final instrument bindings rotate predicate-set hash, semantics unchanged | **hybrid** — binding facts frozen; hash-rotation rule live |
| c2 | Core-attempt analyzer authenticates closed three-repetition index | **live** — `parser_rq_core_attempt.clj` (requal path) |
| c3 | Admission/promotion transaction rejects siblings, stale evaluations, drift | **live** — the promotion transaction (`parser_rq_campaign.clj`) |

### portable-parser-rq-evidence-integrity (0042, Accepted)

| Claim | Statement (short) | Disposition |
|---|---|---|
| c1 | Active contracts carry no machine identity / site policy | **live protocol invariant** (`parser_rq_portability_test.clj`) |
| c2 | One-store verification rejects missing/escaping/truncated/extra/mismatched members | **live** — this IS the historical-audit command capability |
| c3 | Authorization binds portable readiness; promotion needs the integrity receipt | **live** — authorization binding |

## Apparatus dispositions

**Live kernel (stays as maintained capability):**
`parser_release_qualification.clj` (evaluation kernel),
`parser_rq_campaign.clj` (identity/authorization/capture-verify/promotion —
also the historical-audit command), `parser_rq_capture.clj`,
`parser_rq_core_attempt.clj`, `parser_rq_resource.clj`,
`parser_rq_member.clj`, `parser_rq_publication.clj`,
`parser_rq_publication_materialize.clj`,
`parser_rq_parser_ir_conformance.clj`, `parser_rq_diagnostic_completeness.clj`,
`parser_rq_diagnostic_gap.clj`, `parser_rq_decoded_utf8.clj`,
`parser_rq_source_recognition.clj`, `parser_rq_predicate_hardening.clj`,
`parser_rq_classified_source.clj`, `parser_maintenance_evidence.clj`;
the three CLI aliases; the five RQ Nix checks; `bin/parser-rq-*.sh`;
`config/parser-rq-site.example.json`.

**Live product pathway:** `parser_evidence.clj` (+ its test) — independent
of any retirement decision.

**Frozen facts (data, stay as pinned artifacts):** all
`data/parser-rq-*-v1.{json,edn}` policy files,
`data/parser-release-qualification-{corpus,predicates}.edn`, the Hinoki
resource witness, the P5 compat tuple
(`../ab-validator/docs/superpowers/reports/2026-07-12-ab-aozora-phase5-c5-compat.edn`).

**Investigate (possible obsolete apparatus — nothing retired yet):**
- `parser_phase5_frozen_tuple.clj` (206 + 318 test lines): re-verifies one
  frozen artifact by re-stating its expected map in code. The natural
  successor is a closed-manifest integrity check (see the frozen-verdict
  design track); until that exists, this *is* the tuple's audit path — keep.
- `parser_rq_source_accountability.clj` (721 + 1,207 test lines): the
  largest single component. Its measurement role serves predicates 2–3 for
  future runs (live), but its campaign-report aggregation surface may
  exceed what requalification needs. Split candidate on next touch —
  measure/aggregate vs report.
- Schema surface: 44 `parser-rq-*` schemas all currently back either
  capture/evaluation shapes (live) or frozen aggregates. A schema-by-schema
  pass belongs to the same future retirement decision, not now.

## Gaps to close (small, no new machinery)

1. ~~Write the requalification runbook~~ — done: `docs/parser-rq-runbook.md`
   (historical audit + clean-host requalification). Exercise it before
   designing more evidence infrastructure.
2. The frozen-verdict closed-manifest protocol (an ADR 0043 supersession) is
   **deferred** until it demonstrably serves both the P5 tuple and the
   Hinoki witness and deletes more apparatus than it introduces — building
   a protocol merely to retire one verifier would repeat the original
   pattern.

## Trigger for revisiting mass retirement

After the next full-Aozora qualification is accepted, or upon an explicit
decision that Soranoha will not qualify another publication candidate.
Until then, per the audit's corrected principle, this apparatus is owned
capability, not ceremony.
