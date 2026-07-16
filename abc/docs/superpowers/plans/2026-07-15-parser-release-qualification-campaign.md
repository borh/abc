# Parser Release-Qualification Measurement Campaign — Program Roadmap

> **This is a program roadmap, not a single task-by-task implementation plan.**
> The campaign spans several independently-testable subsystems; per the
> writing-plans rule that independent subsystems produce independently testable
> software, each is delivered by its own **focused plan** (listed below). This
> document owns the cross-cutting contracts, the dependency graph, and the split.
> Execute a focused plan with superpowers:subagent-driven-development /
> superpowers:executing-plans; do not execute this roadmap directly.

**Goal:** Produce a real verdict for every ADR 0039 release predicate from a
committed instrument on one pinned build, so the gate renders an honest
qualification result. Success is *honest verdicts*, not a green gate: an honest
`fail`, or a `:conflict`/`:missing` admission that leaves ADR 0039 `Proposed`, is
a correct outcome. The only failure is a fabricated/imputed number or a weakened
predicate.

**Design:** `docs/superpowers/specs/2026-07-15-parser-release-qualification-campaign-design.md`
(read it first). Two tracks: **Track R** (release qualification → ADR 0039) is the
spine; **Track S** (neutral-study axes → the study, never gates ADR 0039) is a
separable appendix. The gate moves to **Capture → Derive → Drift** (decision B):
corpus-scale evidence is captured once on hinoki into a three-tier store
(committed manifests + external content-addressed blobs + a rebinding verifier),
and each predicate observation is a deterministic projection of the committed
manifests, drift-tested — no hand-authored numbers.

**Tech Stack:** Clojure (`abc.tools.parser-release-qualification`,
`abc.tools.aat-parser-ir-compat`, Malli), Rust (`ab-aat-to-parser-ir audit-corpus`,
`ab-check`), Python (capture/derivation harnesses, ruff), Nix checks, EDN/JSON
manifests. Heavy captures/audits run on `hinoki.hyakutake-barbel.ts.net`;
governance recapture follows `.superpowers/sdd/task-6-report.md`.

## Global Constraints

- **Integrity mandate (overrides everything):** never fabricate or impute a
  measurement. No instrument → `:unavailable`; below threshold → `:fail`; `0.969`
  fails `1.0`. Do not weaken/round/reinterpret a predicate — a predicate change
  needs its own evidenced ADR (ADR 0039's rule).
- **Evidence-class separation is structural** (`abc.tools.parser-evidence`): Track
  S never feeds the gate.
- **Two distinct identity relations** (see Cross-Cutting Contract 1): observation
  *coherence* is over the full `qualification_identity`; *admission* is over its
  nine-field match-key **projection**. Never conflate them.
- **No corpus-scale artifacts in the repo.** Commit manifests + logical blob
  identities (`sha256:<digest>` + length + media type) only; blobs live in the
  external store, resolved by runtime config, streamed and re-hashed by the
  verifier (Cross-Cutting Contract 2).
- **Immutable history.** Admission appends a *new* registry generation; never
  rewrite the existing `004deaf`/0.4.0 row or any committed manifest.
- **Governance stays green.** Any ADR-evidence-closure touch (gate report,
  citations, registry) is recaptured on hinoki; `just validate-migration` exits 0.
- **Heavy work on hinoki.**

## Cross-Cutting Contracts (own here; every focused plan depends on these)

### Contract 1 — Identity, coherence, and admission (fixes prior Blocker 1)

`qualification_identity` is a superset of the ADR-0023 match key, so a full-map
"row match" is impossible. Two explicit relations, each with a named, tested
function (all in `abc.tools.parser-release-qualification`, reusing
`abc.tools.aat-parser-ir-compat`):

```clojure
(qualification-identity-ref identity)   ; canonical hash of the FULL identity
(coherent-observations? identity observations) ; every envelope refs that hash

(admission-query identity)              ; PROJECTION onto the 9 match-keys ONLY:
;; {:aat_version :aat_adapter :aat_adapter_version
;;  :mapping_id :mapping_version :mapping_hash :mapping_schema_hash
;;  :parser_ir_schema_id :parser_ir_schema_hash}
(admitted? registry identity)           ; (compatible? registry (admission-query identity))
```

- The bundle `:identity` must therefore carry the **decomposed** match-key fields
  (not only the bundled `"ab-aozora 0.6.0 … (git …)"` string), so `admission-query`
  can project them.
- The gate precondition uses **`compatible?`** (match-key membership) — *not*
  `admission-report` (which additionally requires full evidence-scope equality).
  R6 uses the stricter `admission-report` to *add* the row; the release gate only
  asks "is this tuple admitted?"
- `admission-query` names its exact fields; it does **not** implicitly reuse
  `match-keys` without documenting the projection.

### Contract 2 — Three-tier evidence and store lifecycle (fixes prior Suggestion 5)

- **Committed (in repo):** capture manifests carrying, per blob, a **logical
  identity** `{:sha256 "<digest>" :bytes <n> :media_type "…"}` — never a path;
  plus inventories, denominators (in explicit units), and small witnesses.
- **External store (not in repo):** the corpus-scale blobs, addressed by their
  `:sha256`. `artifact_root` is a **configured store locator resolved at runtime**,
  distinct from the committed content identity — do not call it a content address.
- **Rebinding verifier (hinoki):** resolves logical identities through explicit
  runtime config, **streams and re-hashes** each blob (never trusts store
  metadata), and confirms `:bytes`. A blob **absent or hash-mismatched yields
  `:unavailable`/error, never a derived observation.** Durability is a stated
  retention guarantee of the store, not of the repo.

## Dependency graph and the plan split (fixes prior Blocker 3)

Eight focused plans; the crux is that Foundation contains four separately
reviewable systems and must land first.

| # | Focused plan (to write) | Delivers | Depends on |
|---|---|---|---|
| P0 | `…-parser-rq-foundation.md` | Contracts 1 & 2 made real: plumbing probe, manifest contract, envelope contract, external-store + rebinding verifier, gate coherence precondition | — |
| P1 | `…-parser-rq-source-accountability.md` | R1 source-span coverage (byte denominator + versioned taxonomy) → R2 silent-drops | P0 |
| P2 | `…-parser-rq-publication.md` | R3 publication-structure validation | P0 |
| P3 | `…-parser-rq-resource.md` | R4 per-work memory (single mechanism, chosen in-plan) | P0 |
| P4A1 | `…-parser-rq-source-claim-ledger.md` | Characterize consumption paths; ABC claim protocol/policy; custom-parser ledger emission | P0, P1 |
| P4A2 | `…-parser-rq-source-claim-r1-migration.md` | Ledger-authoritative R1; retain node-span coverage as supporting evidence | P4A1 |
| P4A3 | `…-parser-rq-diagnostic-gap-partition.md` | Versioned diagnostic authorization and R2 partition over ledger gaps | P4A2 |
| P4B | `…-parser-rq-predicate-hardening.md` | R5 (predicates 4 & 5) instrument code | P0, P1 |
| P5 | `…-parser-rq-admission-promotion.md` | Pin the final implementation commit; capture R1–R5; R6 admit; R7 recapture + gate + conditional ADR 0039 | P0–P4 (barrier) |
| PS | `…-parser-rq-study-axes.md` (or its own spec) | Track S: S2, S1, S3, S4 | P0 contracts only; never gates Track R |

```
P0 ──┬── P1 ──┬── P4A1 ── P4A2 ── P4A3 ─┐
     │        └── P4B                   ├──► P5 (barrier: needs P0–P4)
     ├── P2           │
     └── P3 ──────────┘
PS runs parallel to everything; S4 (host-controlled KM latency) is the long pole.
```

**Corrections baked into the split:**
- **P4A1-P4A3/R2 work and P4B/R5 instrument code do not depend on
  admission.** P5 first pins the final
  implementation commit, then captures predicate 5 against that candidate's own
  mapping + schema hash and separately derives whether the candidate is admitted.
- **Predicate 4 is settled as envelope-completeness** (fixes prior Suggestion 4):
  it stays predicate 4 and discloses vacuity. A *diagnostic-recall* predicate is a
  separate future ADR/workstream (oracle + challenge corpus) that does **not**
  change this campaign's predicate set or graph. The P0 disposable probe tests
  plumbing only and cannot select recall.

## Focused-plan interfaces (what each plan must open with)

Each focused plan is written to the full writing-plans standard — exact paths,
interfaces, **named tests with code and expected failure**, exact commands,
minimal implementation, independently testable tasks. Their entry contracts:

- **P0 Foundation** — see the companion focused plan
  `2026-07-16-parser-rq-foundation.md` (written now). Tasks: (0a) disposable
  diagnostic-plumbing probe; (0b) capture-manifest Malli
  contract + tests; (0c) observation-envelope contract + tests; (0d) external-store
  config + rebinding verifier (`verify_external`-style) + tests; (0e) gate
  coherence precondition (`admission-query`/`admitted?`/`coherent-observations?`,
  `gate-status` requires precondition ∧ nine passes) + tests. Migrate the committed
  bundle to envelope form (predicates 2/3/6/8 stay `:instrument-missing`).
- **P1 Source accountability** — Consumes P0's manifest/envelope contracts.
  The implemented generation pins the **byte** denominator and measures
  Parser-IR node-span coverage as supporting evidence. P4A2 supersedes its R1
  numerator with authenticated source claims; a work-count denominator remains
  invalid. Work-completeness is a separate assertion.
- **P2 Publication** — wire `publication-bundle-validate.py` against
  `parser-ir-publication-preservation.schema.json` over the pinned build's
  publication output; ratio denominator in works, every work contributes a record.
- **P3 Resource** — **choose one** peak-RSS mechanism in-plan (GNU `time -v`
  `getrusage` vs cgroup `memory.peak` vs a wrapper) with the rationale; do not
  leave three open. Host pinned + disclosed in the manifest.
- **P4A1 Source claims** — characterizes live consumption paths, freezes the
  closed ABC role/result policy, and emits authenticated ledgers from the custom
  parser.
- **P4A2 R1 migration** — derives claimed/unclaimed bytes from validated ledgers
  and retains node-span coverage under a separately named supporting
  observation.
- **P4A3 Diagnostic gap partition** — consumes P4A2 gaps and raw schema-v3
  captures under a closed ABC-owned `ab-aozora` policy. Unknown vocabulary and
  internal diagnostics fail closed; authenticated empty streams are available
  and disclose vacuity. Diagnostics classify gaps but never increase R1 claims.
- **P4B Predicate hardening** — predicate 4 discloses vacuity (envelope-completeness,
  settled); predicate 5 re-measured against the **pinned candidate** schema.
- **P5 Admission + promotion** — pin the final implementation commit before any
  authoritative capture (the binary bakes `self.rev`); capture R1–R5 for that
  identity; then R6: `ab-aat-to-parser-ir audit-corpus … --compat-edn-out`
  → append a **new** registry generation → `clojure -M:abc/aat-compat-admission --
  --candidates <row>` requires `:admitted`. R7 (barrier): generate the bundle from
  committed manifests, drift-test, conditional ADR 0039 promotion (honest fail
  leaves it Proposed), governance green.
- **PS Study axes** — S2 diagnostics (nearly ready: fixture + conformance scorer
  exist), S1 fidelity oracle, S3 span accuracy (may end `non_comparable` for
  span-less adapter lanes), S4 KM latency + frozen bootstrap (needs a
  host-controlled per-repetition re-run; long pole). Same Rigor Bar; never gates
  Track R.

## Self-review

- The roadmap no longer overclaims: it is a program that decomposes into seven
  independently-testable focused plans, matching the reviewer's split and the
  writing-plans rule. Only P0 (Foundation) is fully written now; the rest are
  written just-in-time as their predecessors' contracts settle (P1–P5 all bind to
  P0's manifest/envelope contracts, so writing them before P0 lands would speculate
  on an unfixed contract).
- Prior Blocker 1 fixed: admission is a *projection* (`admission-query` → nine
  match-keys) checked with `compatible?`, distinct from full-identity coherence;
  named functions, not implicit `match-keys` reuse.
- Prior Blocker 2 fixed: P4B/R5 depends on the **pinned candidate**, not admission;
  the graph and the R5 language now agree (measurement ⟂ admission).
- Prior Suggestion 4 fixed: predicate 4 settled as envelope-completeness; recall is
  a separate future ADR that does not perturb this graph.
- Prior Suggestion 5 fixed: Contract 2 specifies logical blob identity, runtime
  resolution, absent/mismatch → `:unavailable`, streaming re-hash, retention;
  `artifact_root` is a store locator, not a content address.
- Integrity, immutability, evidence-class separation, hinoki, and governance
  recapture are carried as Global Constraints and restated in each focused plan.
