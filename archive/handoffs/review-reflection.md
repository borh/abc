# Reflection on External Review (ADRs + handoff corpus)

> Review received 2026-07-02. This is my honest accounting: which critiques
> land, which I push back on with reasoning, and what I acted on versus
> deferred. Verification discipline applies to the reviewer's claims too —
> I checked each consequential one against the actual artifacts before
> accepting it, same standard I hold subagent reports to.

## What the reviewer got right (act on it)

### §1.1 mapping identity hash — REAL BUG, FIXED

Verified the claim against `owned-mapping-design.md` §1.5: the spec promoted
`mapping_schema_hash` into `manifest_identity_object`, but the variability it
guards lives in the mapping **document** (rule set: I-09, U-01, ruby-scope
default), not the schema. Two mapping versions validating against the same
schema would share the schema hash → identical `artifact_id` + different
content → silent reproducibility conflict (the exact failure ADR 0001 exists
to prevent).

**Acted:** corrected §1.5 to use `aat_parser_ir_mapping_hash` (document JCS
hash) as the identity dimension; demoted `mapping_schema_hash` to
`derived_from` provenance only. Errata appended.

**Calibration note worth recording:** this bug is the kind the committed
ADR 0001 SMT check does NOT catch. The SMT verifies the *rule's* internal
consistency (no counterexample to "conflict ⇒ fail"), not the *field-selection
correctness* (whether the right hash was chosen as identity-bearing). SMT gates
are a complement to design review, not a substitute — reaffirming the
formal-verification-assessment's own "calibrate, don't reflex" caveat.

### §1.4 warnings-code invention contradicts ADR 0002 — REAL, DEFER

Verified: the probe invented `code='AAT_WARNING'` 94,307× (one bucket for
all uncoded warnings). ADR 0002's `source_span_coverage` + warning-code
requirements make this a letter-of-schema-vs-spirit violation — corpus-level
aggregation by code becomes meaningless. The reviewer's fix (push coded
warnings upstream as an additive AAT v1.x field) is correct and allowed by
the AAT contract's additive-extension rule. **Deferred** to the mapping CLI
implementation (Task 7) — it belongs in the ab-validator mapping crate, not
the abc spec.

### §1.5 span semantics deferred-but-load-bearing — REAL, AGREE ON PRIORITY

Verified: ADR 0002 gates `source_span_coverage` = 100% while
`parser-ir.schema.json` leaves `start`/`end` semantics unspecified (the
full-corpus probe had to guess `start=byte_start, column=null`). You cannot
measure coverage against undefined coordinates. The reviewer is right that
this should be an early small ADR, not a deferral. **Agreed** — promoted to
the near-term list.

### §1.3 ruby.direction is the largest + cheapest-to-fix loss — AGREE, SCHEDULE

Verified against the probe: `ruby.direction` LOSS fires ~1.76M times; left
vs. right ruby is semantically meaningful; TEI can represent it. Reviewer's
point (permanent v1 loss is inconsistent with the faithful-TEI-publication
purpose) is well-taken. **Scheduled** into the batched parser-IR additions
(see §1.3 below) rather than leaving it in the loss ledger indefinitely.

### §2 orphan data models, ADR status drift, WIP imbalance, ADR 0004 release floor — AGREE

All four verified: ADR 0017 lines 154-158 explicitly acknowledge
metadata-record and person-record have no standalone ADR; the status drift
(0007/0009/0010/0015 Draft-but-implemented) is exactly what the
RFC-restructure spec Task 3 reconciles; the WIP-imbalance (marker registry +
bounded-workset + mapping + restructure all proposed while ADR 0002 candidate
reports and the cost-envelope benchmark haven't run) is real and the
reviewer's soft-WIP-limit prescription is right; ADR 0004's detached-signature
floor decision is correctly identified as release-blocking. All accepted into
the roadmap.

### §3 "exact-equality is the root cause, not a follow-up" — AGREE, RESEQUENCED

The reviewer is right that I under-framed Tier 1's coverage limit. The
asymmetry (`validate-schematron-invalid-fixture!` checks only *missing*
expected rules, not *extra* fired rules) is exactly how the
`source-span-external-ref.xml` drift happened. I had it as "a stronger
separate check worth adding later" — but it's the root cause, not an
enhancement. **Resequenced:** exact-equality is now the immediate next
commit, not an unbounded ticket. (The .odd-derivation source choice the
reviewer also recommends — derive from `.odd` `constraintSpec`, cross-check
`.sch` — is *already* what I implemented; verified the `rule-universe` fn
reads `.odd`.)

### §4 divergence sidecar capped path list, kunten raw-preservation check — AGREE

Both accepted. The capped-path-list suggestion for LOSS/UNSUPPORTED is a
cheap scholarly-auditability improvement. The kunten raw-preservation check is
important: silent character-semantics loss would violate ADR 0002's "no silent
drops" gate *today*, not in v2 — so it's not just a v2 vocabulary note, it's
a current-correctness probe to run.

## Where I think the reviewer over-states or I push back

### §1.2 UNSUPPORTED demotion — partially correct, but the mode-binding is better than I had, not worse than `--strict`

The reviewer's core point is right: post-I-09 the empirical case for demotion
weakens (warigaki fires 0× in aozora-rs data), and the real unmeasured risk
is aozora2html. But the reviewer's proposed fix — bind strictness to dev vs
release mode (drop-sidecar in dev, release-smoke fails on critical-severity
records) — is **better than my `--strict` flag**, not a critique of demotion
itself. I'm adopting the mode-binding and dropping the CLI flag: it converts
operator-discipline into a gate, which is the right shape. So this is "agree,
and your fix improves on mine."

The one calibration: the reviewer frames the demotion as "loses its empirical
footing" post-I-09, but the demotion was never *only* about the empirical 30.6%
rate — it was also that `refuse` as a *default* for a publication pipeline is
the wrong call when a single node kind can halt ingestion. The mode-binding
preserves both concerns. Adopting it wholesale.

### §4 "a cap for path lists" — agree, but not for every category

The reviewer suggests capped path lists (first N) for LOSS/UNSUPPORTED.
Right for UNSUPPORTED (rare, worth auditing each). For LOSS at 1.76M
occurrences (ruby.direction), even N=10 is noise — the per-rule aggregate +
first-path is the right granularity there. Concretely: cap path lists only
for UNSUPPORTED and STRUCTURAL (rare, audit-each), keep count+first-path for
the high-frequency LOSS/INVENTION categories. Not a disagreement, a
refinement.

## Verification discipline applied to the reviewer

Same standard I hold subagent reports to — checked each consequential claim
against the actual artifacts before accepting:

| Reviewer claim | Verified against | Result |
|---|---|---|
| §1.1 schema hash vs document hash | `owned-mapping-design.md` §1.5 text | confirmed — real bug |
| §1.3 ruby.direction ~1.76M | full-corpus-probe.md + raw output | confirmed (1,764,113×) |
| §1.4 warnings-code invention 94K | owned-mapping-design.md + ADR 0002 | confirmed |
| §1.5 span semantics deferred-but-load-bearing | ADR 0002 line 38 + ADR 0012 line 70 + parser-ir schema | confirmed |
| §2 orphan data models | ADR 0017 lines 154-158 | confirmed (explicitly acknowledged) |
| §3 asymmetry is root cause | validate-schematron-invalid-fixture! source | confirmed |

Every consequential claim held up. The review is unusually well-grounded.

## Net changes made this round

- `owned-mapping-design.md` §1.5 **corrected**: `aat_parser_ir_mapping_hash`
  (document hash) replaces `aat_parser_ir_mapping_schema_hash` (schema hash)
  as the identity dimension; schema hash demoted to provenance. Errata
  appended documenting the bug + the SMT-limit calibration note.

## Revised near-term roadmap (incorporating the review)

| # | Action | Source | Status |
|---|---|---|---|
| 1 | Mapping identity hash fix (document not schema) | review §1.1 | ✅ done |
| 2 | UNSUPPORTED strictness bound to dev/release mode, drop `--strict` | review §1.2 | scheduled (owned-mapping Task 5/7) |
| 3 | Exact-equality Schematron coverage (the root cause) | review §3 | immediate next commit |
| 4 | Batched parser-IR additions: `ruby.direction` + span semantics + ADR 0018 renames in one cascade | review §1.3/§1.5/§2 | scheduled |
| 5 | Run the two cheap decisive probes (aozora2html full-corpus; 1k-work Nix eval) | review §2 WIP-limit | scheduled (measurement-blocked) |
| 6 | ADR status reconciliation + retroactive ADRs for the two orphan data models | review §2 | scheduled (RFC-restructure Task 3) |
| 7 | ADR 0004 detached-signature floor decision; hash compatibility registry into release verification | review §2 + §4 trust-surface | scheduled |
| 8 | Kunten raw-preservation check (current-correctness, not just v2) | review §4 | scheduled |
| 9 | Capped path lists for UNSUPPORTED/STRUCTURAL sidecars only | review §4 (refined) | scheduled |

## The meta-point the reviewer makes

The review closes with the right synthesis: the project's core bet
(content-addressed identity + derived views + drift gates everywhere) is
sound, but the project now needs the same probe-before-commit discipline
applied to its *design corpus* that it applies to its *text corpus*. The
full-corpus probe proved this when it overturned Findings B/D; the review
proves it again when its §1.1 catches a correctness bug that survived my own
SMT-verification (because SMT checks consistency, not field selection).

The prescription — close the loop faster between provisional design and
decisive measurement, and accept that ADR review is itself a measurement not a
formality — is the right one to carry forward. The probe culture is
established; the lesson is to apply it sooner and to the design layer, not
just the data layer.
