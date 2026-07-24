# System Simplification Audit — Hickey-lens review of abc/ (rev 3)

Date: 2026-07-24
Mode: **Audit** (analysis; F2 since executed).
Rev 2 incorporated an external architecture/protocol review (reachability and
evidence-model corrections). Rev 3 corrects remaining count errors, records
F2 as executed, adopts the F3 retirement decision and the F1 warm-capability
contract, and adds the figure style kit follow-up.

## Problem statement

The decisions.edn migration removed one instance of a recurring meta-pattern:
verification apparatus accreting around an activity and persisting after the
activity concludes. This audit asked where else the pattern holds and what
the highest-leverage subtraction is.

Scale at audit start (pre-F2): 104 src namespaces (23.3k lines), 136 test
namespaces (30.4k lines), 78 JSON Schemas (`schemas/*.schema.json`; 84 files
in `schemas/` including ODD/RNG/SCH/SHACL/contracts), 21 `:abc/*` aliases
(20 after F2), ~6 Nix gates.

## Headline principle (as corrected in review)

An Accepted decision does not establish that its **operational capabilities**
have concluded. The repository contains a third category between "hot
publication path" and "dead": **maintained qualification, release, recovery,
and operator capability.**

> **Apparatus follows an explicitly owned capability or evidence lifecycle.**

Each piece of apparatus is classified as: **live product pathway**,
**maintained release/operations capability**, or **frozen historical
verdict** — only the third disposition permits retirement to pinned
artifacts.

## Findings

### F1. Parser release-qualification family — retain a warm qualification path
**Status: mass retirement deferred by decision. Disposition table is the next artifact.**

Concluded-campaign facts: ADRs 0039–0042 Accepted 2026-07-20; phase-5 tuple,
resource witness, corpus, and policy files frozen and hash-pinned. Production
publication consumes `parser-evidence/citable-hashes` (via `snapshot_index`).

Reachability (corrected in rev 2): the family is supported operational
capability — three command aliases (`:abc/parser-rq-campaign`, `-member`,
`-publication-materialize`), the root `parser-rq-production-wiring` check,
abc campaign-site/orchestrator checks, campaign CLI
candidate/authorization/capture/evaluation/promotion operations, and
ab-validator schema/data consumers. The accepted claims include **live
release-safety behavior**, not merely historical measurements: exact
predicate evaluation, release-class exclusion, all-predicates-pass gating,
instrument identity, admission/promotion rejection, closed evidence
membership, portable authorization binding.

Size, counted as the named campaign family (15 `parser_rq_*` +
`parser_release_qualification` + `parser_maintenance_evidence` +
`parser_phase5_frozen_tuple`), **excluding** the live `parser_evidence` pair
(225 src + 280 test lines): **4,803 src + 4,327 test lines**, plus 44 of the
78 JSON Schemas.

**Adopted capability contract:**

| Capability | Promise |
|---|---|
| Historical audit | One command verifies the accepted P5 closed membership, hashes, and decision binding without rerunning measurements; CI-scale, minutes. |
| Bounded requalification smoke | End-to-end candidate → capture → projection → evaluation → promotion stays exercised on a small fixture in CI. |
| Full requalification | From a clean Linux/cgroup-v2 host, an operator can start a new qualification via a documented runbook within one working day. |
| Scope expansion | Candidate, corpus, predicate, and instrument identities can rotate without reconstructing the protocol from git history. |

"Recoverable from git history" is archival, not maintained capability.

**Next artifact — claim-by-claim disposition table for ADRs 0039–0042,
provisionally classifying:**
- *Frozen/hybrid:* current corpus facts, predicate-set facts, P5
  measurements, the historical resource witness.
- *Live:* exact predicate evaluation, release gate, release-class boundary,
  identity rotation, admission/promotion transaction, portability,
  closed-membership verification, authorization binding.
- *Obsolete apparatus:* only components proven unnecessary for both the live
  kernel and the historical audit path.

Revisit mass retirement after the next full-Aozora qualification is accepted
— or after an explicit decision that Soranoha will not qualify another
publication candidate.

### F2. Presentation figure pipeline — EXECUTED (commit 24cad723)
**Status: merged to main and pushed 2026-07-24.**

Framing: freeze the JADH 2026 talk deliverable; retire its reproducible
rendering pipeline; retain and test the generic DOT emitter.

Removed: `presentation_svg` (SVG normalizer/sanitizer), `presentation_figures`,
`presentation_model`, `presentation_registry`, their four test suites, the
`presentation-diagram-drift` check, the `presentation-diagrams` app +
launcher, the fonttools/fontconfig/noto-CJK toolchain, the devShell font
environment, the `:abc/presentation-diagrams` alias, and the two
filesystem-policy permanent exceptions. Retained: `diagram/graphviz.clj` with
its characterization test, `graphviz` in the devShell, frozen
`docs/figures/*.{dot,svg}`.

Measured: **−1,842 net lines at commit level** (1,849 deletions, 7
insertions across 12 files). The earlier 1,678 figure counts only the
presentation source/test family, excluding the Nix/policy/docs edits.
Verification: full kaocha 1090 tests 0 failures (−45 = the deleted suites);
clj-kondo one fewer warning than baseline; `diagram-drift`, `adr-governance`,
`clj-kondo`, `contract-surface`, `clj-nix-focused-tests` pass;
`presentation-diagram-drift` absent from the checks set.

**Follow-up (from post-merge review): the figure style kit.** Three surviving
details contradict the retained-capability claim: the emitter still stamps
output with the deleted CLI banner (graphviz.clj:174); it ignores the
supplied black `:canvas` token and hardcodes `bgcolor="transparent"`
(graphviz.clj:176) — the deleted sanitizer was what applied the black
canvas, so a direct `dot -Tsvg` render loses the background the off-white
palette depends on; and Noto Sans CJK JP stays hardcoded (graphviz.clj:177)
while its pinned Fontconfig environment was removed, making font resolution
impure. Adopted approach — a small owned style kit, Graphviz presentation
figures only (Mermaid styling is a separate visual-system decision):

1. One canonical presentation theme value beside graphviz.clj: semantic
   palette (black canvas; off-white primary / gray secondary text; cyan
   identity; amber evidence/validation; green publication outputs), Noto
   Sans CJK JP, 34/24 graph-body type scale, 2px rules.
2. Emitter consumes `:font-family` and `:canvas`; black is the default
   presentation background, transparency an explicit override.
3. Replace the dead CLI banner with provenance naming
   `abc.tools.diagram.graphviz/dot`.
4. Keep Noto in the Nix devShell with a small isolated Fontconfig config —
   no fonttools/subsetting, no ImageMagick/librsvg, no app, no drift gate.
5. Focused tests: semantic tokens, canvas application, font selection,
   role-color mapping, non-color reinforcement; plus a tiny Nix render
   smoke check (retained capability, not a frozen-talk drift gate).
6. `docs/architecture-presentation.edn` moves beside the JADH artifacts as
   frozen source metadata (it must not sit at an apparently live
   architecture path without validation).

### F3. Unadopted target-graph engine — RETIRE (complete vertical slice)
**Status: approved; execution pending an out-of-repo-consumer confirmation.**

`workflow/target|report|nix_bridge|cache|query` (419 src lines + tests):
a **proposed cross-language contract surface**, never adopted. Repository
evidence: the design spec is Status: Proposed
(specs/2026-07-09-workflow-target-graph-evaluator-design.md:3) and describes
node summaries as a cross-language operational contract (line 573) that no
accepted decision cites; the cache design explicitly concluded "do not
build" (specs/2026-07-09-workflow-cache-aware-evaluation-design.md:68);
`eval-target` reaches only `workflow.report/eval-target-step`, which has no
production caller; cache and query exports have no source callers.

**Adopted cutover plan:**
- Confirm no out-of-repo consumer.
- Delete the five namespaces and all associated tests.
- Delete `workflow-nodes.schema.json`, examples, invalid fixtures, both
  schema-contract entries and both schema-contract scripts' entries, and the
  ab-validator mirror copies.
- Remove `node_summary_ref` from `schemas/workflow-run.schema.json` and its
  emitter branch in `workflow.clj`.
- Advance the workflow-run contract version 0.2.0 → 0.3.0 (never rewind).
- Regenerate schema hashes/mirrors atomically.
- Add an absence assertion that `node_summary_ref` and `workflow-nodes` are
  no longer part of the supported contract.
- Breaking-change commit: the schema acceptance surface narrows.

### F4. validate_design_bundle — revise before scheduling
**Severity: follow-up.**

1,503 src + 3,855 test lines; CI gate (`.#validate-design-bundle`, justfile
`evidence-gate`); mandated by the Accepted `v0-design-bundle-validation`
decision (9 claims). Stays. Its centrality is not per se bad modularity —
cross-artifact integration invariants belong at an integration boundary.
Split: move domain-local checks to owning layers; retain cross-layer
contracts centrally. Before any shrink, characterize its exact stage
sequence and cross-boundary read set (`evidence-input-paths` is itself cited
by a decision claim).

### F5. Publication-chain decomplection — ordinary refactors
**Severity: follow-up.**

- `soranoha.clj` (1,296 lines) braids CLI dispatch (18 subcommands) with
  inline snapshot-root materialization (~220–575), snapshot
  reproduce/validate, report writers, and the 8-step rehearsal workflow
  (~857–1148). Extract the two cohesive blocks; leave a thin dispatcher.
- `materialize_publication`'s `batch-workflow-run` cluster resembles but is
  not verbatim `workflow/summarize-run` (timestamp, duration, workflow
  identity, record semantics differ). Extract a shared run-record
  constructor under characterization; do not reroute batch publication
  through the runtime workflow engine.
- Duplicated definitions: `compact-hashes` ×3 (soranoha.clj:220,
  materialize_source_snapshot.clj:37, materialize_publication.clj:213);
  `now-utc` ×2 and the workflow-run schema id ×2 (workflow.clj:7,13 vs
  materialize_publication.clj:682,685).

### F6. Withdrawn draft-1 claims
- `parser_ir_vocabulary.clj` is test-support enforcing renderer vocabulary
  coverage (consumed by parser_ir_tei/plaintext/publication_policy tests),
  not dead code.
- `workflow/validate-run` is directly load-bearing via soranoha's
  `validate-workflow` command, independent of `workflow_graph`.

## Frozen-verdict evidence model — redesign path only

A hash authenticates bytes, not a decision claim: it cannot show the
artifact passed the applicable predicate, that the predicate was intended,
that the input set was complete, that the right schema/validator versions
ran, or that no members are missing. A credible frozen verdict needs a
**closed manifest** (result + exact inputs + membership rules + schema and
validator identity/version + canonicalization + hash algorithm, with defined
semantics for directory hashing, symlinks, extra files, external retention).
Decision statuses have no "activity concluded" state; `:evidence` is a
vector of paths; ADR 0043 chose executable evidence over stored per-claim
results and requires a superseding ADR to reintroduce a typed protocol. If
pursued: design as an explicit ADR 0043 supersession with the three evidence
dispositions.

## Sequence (rev 3 statuses)

1. **F2 — executed** (24cad723, merged and pushed).
2. **Style kit** — approved; implement (Graphviz-scoped).
3. **F3 — approved**; execute as complete vertical deletion with contract
   version 0.2.0 → 0.3.0 and mirror sync.
4. **F1 — deferred**; produce the claim-by-claim disposition table under the
   adopted capability contract.
5. F4–F5 as follow-up refactors when next touching those files.
