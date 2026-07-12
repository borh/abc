# Consolidated parser Phase 5 — bare-toggle inline classification, denominator closure, coverage-green hygiene

**Date:** 2026-07-12
**Status:** approved design (brainstorm decisions recorded below)
**Follows:** `2026-07-11-consolidated-parser-phase4-level3-admission-activation-design.md`
(Phase 4 merged to main `ac2be926`; `ab-aozora 0.5.0` is the activated
publication lane, AAT schema v2, mapping `0.3.0`).
**Evidence base:** `docs/superpowers/reports/2026-07-12-bare-toggle-placement-attribution.md`
(committed with this spec) and
`2026-07-11-keigakomi-yokogumi-denominator-attribution.md`.

## Goal

Close the named classifier ceiling — the bare-toggle marker forms
`［＃横組み］…［＃横組み終わり］` and `［＃罫囲み］…［＃罫囲み終わり］` —
with one gated identity rotation (C5, `ab-aozora 0.5.0 → 0.6.0`), resolve
the keigakomi 44-marker denominator residual to an attribution or a frozen
errata, and restore the end-to-end green coverage verdict
(`IR_PUBLICATION_COVERAGE_COMPLETE`) by completing the ABC custom-contract
`0.3.0` confirmation, plus three small hygiene items.

## Ground truth that drives the design

The placement-attribution audit (2026-07-12, pinned corpus store path
identical to the frozen fidelity summary's) establishes:

- **Every** bare-toggle occurrence is mid-line; **zero** markers stand
  alone on a line.
- **Every** close pairs with an open **on the same line**: 1,589 yokogumi
  pairs (338 works) and 25 keigakomi pairs (8 works); **zero** cross-line
  pairs.
- 10 surplus `［＃横組み］` opens are unpaired (one per work, no close
  anywhere in the work).
- Corpus usage is inline: Latin runs inside vertical text, boxed words
  inside headings, multiple pairs per line, and cross-construct nesting
  (a yokogumi pair inside a keigakomi pair on one line).

**Consequence:** the bare forms are *inline span* markup. The Phase 3/4
carried framing ("extend the `yokogumi_block`/`keigakomi_block`
classifiers") is superseded for these forms: the emission target is the
existing AAT schema v2 `inline_container` kinds `"yokogumi"` /
`"keigakomi"` (`data/aat-schema.json` `$defs/inline_container`), for which
`ab-aat-to-parser-ir` conversion arms already exist
(`crates/ab-aat-to-parser-ir/src/convert.rs`). No AAT schema bump: the
document stays `version 2`.

## Decisions (brainstorm, all confirmed)

1. **Scope:** bare-toggle classifiers + hygiene cluster + keigakomi
   44-residual. Warigaki/kunten vocabulary ADR deferred to Phase 6.
2. **Unpaired markers raw-preserve** (fail-closed; the compound-jizume
   fallback precedent). No auto-close at EOF or at structural boundaries.
3. **One identity rotation (C5)** covering both construct families;
   hygiene and residual work are non-identity tasks.
4. **Keigakomi residual: attribute-or-errata**, not phase-blocking.
5. **ABC drift: full confirmation** — flip the `0.3.0` custom contract to
   `CONFIRMED_BY_ABC_INTEGRATION`, coverage back to `COMPLETE`.
6. **Representation: inline spans, same-line only.** No cross-line or
   block classification of bare forms (zero corpus instances; a stray
   open pairing with a stray close thousands of lines away must never
   swallow a document).
7. **Inline-ATTRIBUTE forms excluded** (`［＃「X」は横組み］` 116 and the
   keigakomi inline-attr family 238): different recognition mechanism
   (back-reference to preceding quoted text); named follow-up, not
   Phase 5.

## Contract 1 — recognition and emission (the C5 classifier)

Recognition operates on the exact tokens only:

| construct | open token | close token |
| --- | --- | --- |
| yokogumi | `［＃横組み］` | `［＃横組み終わり］` |
| keigakomi | `［＃罫囲み］` | `［＃罫囲み終わり］` |

These tokens cannot substring-match the verbose `ここから`/`ここで` block
forms or the inline-attr forms (both carry intervening characters), so the
existing block classifiers and raw handling of other families are
untouched by construction.

**Pairing rule (same line only, fail-closed):**

- Within one decoded-source line, markers of both constructs are paired by
  a document-order stack per construct.
- An open pairs with the nearest following close of the same construct on
  the **same line**.
- Cross-construct proper nesting is admitted (e.g.
  `［＃罫囲み］…［＃横組み］…［＃横組み終わり］…［＃罫囲み終わり］`);
  the nested pair becomes a child `inline_container` of the outer pair's
  content.
- Fail-closed exclusions — **every marker of the affected construct on
  that line stays a byte-identical raw node** (current behavior, no new
  warning codes); for the interleaving case both constructs' markers on
  the line stay raw:
  - an open with no same-line close (includes the 10 corpus unpaired
    opens and any hypothetical cross-line pair),
  - a close with no same-line open,
  - same-construct re-open before the pending open closes on that line,
  - improper cross-construct interleaving (overlap without nesting, e.g.
    A-open B-open A-close B-close).

**Emission (adopted pair):** one `inline_container` node with `kind`
`"yokogumi"` or `"keigakomi"`, whose `content` is the normally-parsed
inline content between the two markers (other inline markup inside the
span parses exactly as it would outside it), and whose `span` covers the
first byte of the open marker through the last byte of the close marker in
ADR 0024 decoded-source coordinates. The two marker tokens are consumed by
the adoption (they do not additionally appear as raw nodes) — mirroring
how verbose containerOpen/Close raw pairs disappear into typed blocks.

**Observability preflight (named plumbing task, Phase 4 blocker-5
lesson):** before the classifier is written, a task must verify on real
wire output that the facade delivers bare-toggle markers as observable
in-line nodes with exact source text and spans usable for the emission
contract above. If any facade change is required to observe them, the
facade version bumps (0.3.0 → 0.3.1) and joins C5's identity join key;
otherwise the facade stays 0.3.0. The plan must not assume either outcome.

**Corpus-bound expectations (checked by the delta audit, not asserted
blindly):** adopted yokogumi pairs = 1,589; adopted keigakomi pairs = 25;
unpaired raw-preserved opens = 10; all other works byte-identical.

## Contract 2 — mapping 0.4.0

New inline emission produces AAT paths mapping `0.3.0` has no rules for
(verified: `data/aat-to-parser-ir-mapping-v2.json` carries yokogumi /
keigakomi rules only for the `*_block` kinds — S-05/S-06/S-09/S-10,
U-13/U-14/U-39/U-40). Therefore:

- `data/aat-to-parser-ir-mapping-v2.json` is edited in place:
  `mapping_version` `0.3.0 → 0.4.0`, `source_aat_version` stays `2`, new
  transform-rule descriptions for the inline `yokogumi` / `keigakomi`
  container paths with observed-occurrence counters measured from the C5
  dump (the S-08/I-32 accounting pattern: every observed path accounted,
  no silent categories).
- The new mapping hash is computed at freeze time and becomes the frozen
  registry coordinate of the C5 row. The `0.3.0` hash
  (`sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`)
  remains the frozen coordinate of the existing rows — the registry is
  append-only; no retro-edits.
- Converter dispatch is unchanged: AAT version 2 selects the current
  schema/mapping files; `ab-check` likewise. The frozen v1 pair is
  untouched.
- The converter README's mapping section is updated in the same task
  (version, hash, rule inventory) — no stale-hash repeat of the v1 line
  defect.

## Contract 3 — identity, gates, ceremony (C5)

**Identity discipline (Phase 4 lessons, all binding):** gate instruments
land **before** the candidate identity closes; the candidate commit is
recorded in a machine-readable identity file
(`.superpowers/sdd/phase5-identity.json`) with the full join key
(`ab-aozora 0.6.0 aat-schema 2 facade <0.3.0|0.3.1> wire-schema 3`,
mapping `0.4.0` + hash); candidates are never derived from `HEAD`; every
local build and hinoki run is candidate-commit-bound.

**Gates (all fail-closed, all on the pinned 17,886-work corpus):**

1. **Delta audit** — new `bare-toggle-adoption` mode in
   `reports/aat-fidelity/audit-aat-delta.py`. Grammar: for each differing
   work, the AAT delta must consist exactly of adopted-pair rewrites
   (remove the two raw marker nodes, insert one `inline_container` of the
   matching kind whose content equals the previously-adjacent inline
   content and whose span satisfies Contract 1); counters
   `adopted_yokogumi_pairs`, `adopted_keigakomi_pairs`,
   `unpaired_raw_preserved`; expected values 1,589 / 25 / 10; any other
   difference class → exit 2.
2. **Conformance** — 25/25 `must` on the ab-aozora lane plus the seed
   lane, zero drift outside adopted works.
3. **Perf** — measure-first on the pinned hash-pinned workset; workset
   median ≤ +10% blocks; `001562_56145` remains the individually watched
   work (its +34.8% individual regression is median-absorbed but any
   growth is investigated before admission). Inline-path changes touch
   span composition — this is the phase's riskiest gate; the Phase 4
   `from_entries` lesson (no `serde_json::Value` round-trips on hot
   paths) applies to the new inline assembly.
4. **Conversion audit** — 17,886 / 17,886 / 0
   (parsed / raw-preserved / diagnostic) under mapping `0.4.0`.

**Ceremony order (Phase 4-proven, unchanged):** producer gate reports
frozen → registry row appended to `abc/data/aat-parser-ir-compatibility.edn`
(C5 tuple + mapping 0.4.0 coordinate) → admission run with byte-exact
whole-row equality (copy `--compat-edn-out` verbatim), `:admitted`
captured → run-set repoint: **one atomic commit** updating the `ab-aozora`
entry of `reports/aat-fidelity/run-sets/current.json` to the C5 dump
(`aat_dir`, `adapter_version_contains`, `content_hash`) with an explicit
allowed-paths list; no fixture or run-set content staged before that
commit → `reports/aat-fidelity/verify-phase5-checkpoint.py` binds the gate
summaries, both audit modes, the live admission re-run (fail-closed), and
the repoint commit's tree (parent/child checks, `ACTIVATION_REQUIRED` /
allowed-prefixes pattern from the Phase 4 verifier) → hinoki dump
retention: the C5 dump joins the never-delete list; the C4 dump
`ab-aozora-phase4-c4-27772b1` **remains never-delete** (prior run-set
states reference it) — retention is append-only.

**Rollback:** `git revert` of the repoint commit restores the C4 lane
binding; registry rows are append-only and are not removed.

## Contract 4 — keigakomi 44-residual (attribute-or-errata)

Extend the denominator-attribution tooling
(`reports/aat-fidelity/denominator-attribution.py` or a sibling script,
same pinned corpus store path) to a **per-work, per-pattern diff**: for
each of `decoration.keigakomi`'s `source_patterns` (from
`data/aozora-syntax-coverage.toml`), compare this scan's match count
against the frozen instrument's per-construct total, localizing which
works/forms account for 717 − 673 = 44. Bounded outcome, either:

- **Attribution:** a frozen report naming the forms/works that reconcile
  the 44 (and, if the frozen denominator turns out to double-count or
  include a form outside the matrix alternation, saying so plainly — the
  frozen summary itself is never edited); or
- **Errata:** a frozen report recording the residual as irreducible under
  the hypotheses checked, designating **673** (or the reconciled figure)
  as the effective keigakomi denominator that future classifier rates
  must cite.

Not phase-blocking; C5's gates do not depend on it.

## Contract 5 — ABC custom-contract 0.3.0 confirmation

Mechanism per `docs/handoffs/source-region-coverage-abc-integration.md`
and `reports/parser-ir/publication-coverage.py`: confirmation requires the
trusted snapshots (`TRUSTED_ABC_PRESERVATION_SCHEMA_PATH` and the
source-region schema/policy/manifest snapshot hashes) to match the
ABC-side `0.3.0` artifacts, plus the required record/coverage classes and
counters. The task syncs the ab-validator-side trusted snapshots to the
ABC `0.3.0` artifacts (reviewing, not rubber-stamping, the diff — the
snapshot sync is the integration review), then regenerates coverage.

**Exit assertions (all four, in one regenerated coverage summary):**

- `source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`
- custom-contract verdict `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- TEI-profile contract verdict `TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- top-level `IR_PUBLICATION_COVERAGE_COMPLETE`

with the three source-authority occurrence counters still 0 and
`parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`.
Ordering: this lands **before** the C5 post-repoint coverage regeneration,
so the phase's final coverage evidence is green end-to-end under the
activated C5 wiring.

## Contract 6 — hygiene items

1. **Converter README v1 hash line:** correct the v1 parser-IR schema hash
   citation to `a1e1b506…` (the hash the frozen v1 mapping binds). Doc
   line only; no gate reads it.
2. **Facade-level Segments-skip assertion:** add a facade
   (`crates/ab-aozora-facade`) test asserting `ruby_entries` excludes
   Segments-base (gaiji) ruby — the invariant the adapter-level
   `gaiji_base_ruby_keeps_v1_typed_emission` test relies on, pinned at the
   layer that owns it.
3. **`verify-golden-spans.py` CRLF false positives:** suppress the
   documented CRLF-normalization projection artifact by matching its
   precise signature (not blanket newline normalization), count and
   report each suppression, and fail on any span mismatch that does not
   carry the signature. The tool must run clean on the goldens with an
   explicit `crlf_artifact_suppressed: N` counter rather than a green
   verdict that hides the family.

## Non-goals

- Inline-attr forms (`［＃「X」は横組み］` 116; keigakomi inline-attr 238)
  — named follow-up, same target node kinds, different mechanism.
- Cross-line bare-toggle pairing or block classification of bare forms
  (zero corpus instances; hazardous failure mode).
- Warigaki/kunten vocabulary ADR (Phase 6).
- Bare-toggle warning enrichment (unpaired markers stay silently raw, as
  today).
- Any edit to frozen evidence reports or the frozen v1 schema/mapping
  pair.

## Task overview (plan will detail)

1. Observability preflight (facade wire check for bare toggles).
2. Instruments: `bare-toggle-adoption` delta-audit mode + tests;
   `verify-phase5-checkpoint.py` skeleton.
3. Classifier: same-line pairing + inline_container emission + tests
   (corpus-pinned fixtures for each fail-closed exclusion class).
4. Mapping 0.4.0 + converter README refresh.
5. C5 identity close, hinoki full run, four gates.
6. Registry row → admission → atomic run-set repoint → checkpoint.
7. Keigakomi residual attribution/errata.
8. ABC 0.3.0 confirmation + green coverage regeneration.
9. Hygiene: README hash line, facade Segments-skip test, CRLF suppression.
10. Closure: handoff addendum, ledger, memory.
