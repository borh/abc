# Consolidated parser Phase 5 — bare-toggle inline classification, denominator closure, coverage-green hygiene

**Date:** 2026-07-12
**Status:** revised design, revision 2 — review round 1 (blockers P5-1…P5-5,
suggestions P5-6…P5-10, nits) incorporated; pending re-approval for
planning.
**Follows:** `2026-07-11-consolidated-parser-phase4-level3-admission-activation-design.md`
(Phase 4 merged to main `ac2be926`; `ab-aozora 0.5.0` is the activated
publication lane, AAT schema v2, mapping `0.3.0`).
**Evidence base:** `docs/superpowers/reports/2026-07-12-bare-toggle-placement-attribution.md`
**Revision 3** (grammar-true scan; every discovered candidate classified —
17,886 works + 5 non-work archives + 2 tolerant-7zz-only recoveries with
zero bare-toggle content + 2 unreadable — via the shared
`reports/lib/corpus_reader.py` contract) and
`2026-07-11-keigakomi-yokogumi-denominator-attribution.md`.

## Goal

Close the named classifier ceiling — the bare-toggle marker forms
`［＃横組み］…［＃横組み終わり］` and `［＃罫囲み］…［＃罫囲み終わり］` —
with one gated identity rotation (C5, `ab-aozora 0.5.0 → 0.6.0`), resolve
the keigakomi 44-marker denominator residual to an attribution or a frozen
errata, and restore the end-to-end green coverage verdict
(`IR_PUBLICATION_COVERAGE_COMPLETE`) by completing the ABC custom-contract
`0.3.0` confirmation, plus three small hygiene items.

## Task 0 — repair the Phase 4 baseline (precondition, P5-1)

At the spec commit, the integrated Phase 4 Rust baseline is red under the
repository's own gates:

- `cargo fmt --check` fails: committed formatting drift in
  `crates/ab-aozora/tests/wire.rs` (~line 84) and
  `crates/ab-aozora-aat/src/lib.rs`.
- `just clippy` runs `cargo clippy --workspace --all-targets
  --all-features`, which enables the facade's stub `cst`/`query` features —
  documented in `crates/ab-aozora-facade/Cargo.toml` as *intentionally*
  failing to compile (their `pub use aozora_cst::* / aozora_query::*`
  re-exports have no crate to resolve; the crates were excluded from the
  lift set by design). The gate and the stub-feature design contradict
  each other; both cannot stand.

Task 0 resolves both before any Phase 5 instrumentation lands:

1. Commit the formatting fixes (`cargo fmt`).
2. Resolve the clippy contradiction by **removing the stub `cst`/`query`
   features and their `#[cfg(feature = …)]`-gated sections in
   `crates/ab-aozora-facade/src/lib.rs`** (never-lifted crates; the
   sections are permanently dead code — YAGNI). Alternative, if
   upstream-feature-name parity must survive: scope the clippy gate's
   feature set instead; the plan review adjudicates if the implementer
   finds a reason to prefer it. Either way the resolution is its own
   reviewed commit.
3. Exit criteria, all green on one commit: `cargo check --workspace
   --all-targets`, `cargo fmt --check`, `just clippy`, the Phase 4
   checkpoint test suite, and a live
   `reports/aat-fidelity/verify-phase4-checkpoint.py` → `CHECKPOINT OK`.

Phase 5 work may not begin until Task 0's exit criteria hold; every
subsequent task inherits the stay-green obligation.

## Ground truth that drives the design

The placement + adoption-grammar audit (Revision 3 — shared reader
contract with windows-31j member names, local-header bypass, and
tolerant-7zz recovery; every discovered candidate classified into
work / non-work / recovered-extra / unreadable, fail-closed on exactly
17,886 works; the two recovered-extra texts verified to contain zero
bare-toggle markers; per-work identity / reader-path / decode-mode /
sha256 recorded) establishes:

- **Every** bare-toggle marker is mid-line; **zero** stand alone on a
  line; **zero** pairs span lines.
- Under the exact adoption grammar of Contract 1 (not naive pairing):
  **adopted pairs = 1,582 yokogumi + 25 keigakomi**; declined markers =
  **24** (10 orphan opens + 14 rollback markers), attributed in full:
  the 10 orphans are the literal editorial example line
  `（例）［＃横組み］` quoted in 凡例 sections; the 14 rollbacks are one
  malformed 14-marker quotation line in `000094_42338`. Marker
  arithmetic closes exactly: 3,238 = 2 × 1,607 + 24.
- 0 improper interleavings, 0 orphan closes, 1 proper cross-construct
  nesting, 0 lines mixing adopted and invalid groups.
- Corpus usage is inline: Latin runs inside vertical text, boxed words
  inside headings, multiple pairs per line.

**Consequence:** the bare forms are *inline span* markup. The Phase 3/4
carried framing ("extend the `yokogumi_block`/`keigakomi_block`
classifiers") is superseded for these forms: the emission target is the
existing AAT schema v2 `inline_container` kinds `"yokogumi"` /
`"keigakomi"` (`data/aat-schema.json` `$defs/inline_container`). No AAT
schema bump: the document stays `version 2`.

## Decisions (brainstorm + review round 1)

1. **Scope:** bare-toggle classifiers + hygiene cluster + keigakomi
   44-residual. Warigaki/kunten vocabulary ADR deferred to Phase 6.
2. **Unpaired/invalid markers raw-preserve** (fail-closed; the
   compound-jizume fallback precedent). No auto-close at EOF or at
   structural boundaries. The corpus vindicates this: the 10 orphan opens
   are notation examples that must never classify.
3. **One identity rotation (C5)** covering both construct families.
   Hygiene and residual work are non-identity tasks in independently
   reviewable commits; none of them are part of C5's identity.
4. **Keigakomi residual: attribute-or-errata**, not phase-blocking.
5. **ABC drift: full integration confirmation** — flip the `0.3.0` custom
   contract to `CONFIRMED_BY_ABC_INTEGRATION`, coverage back to
   `COMPLETE`, with recorded provenance (Contract 5).
6. **Representation: inline spans, same-line only.** No cross-line or
   block classification of bare forms (zero corpus instances; a stray
   open pairing with a stray close thousands of lines away must never
   swallow a document).
7. **Inline-ATTRIBUTE forms excluded** (`［＃「X」は横組み］` 116 and the
   keigakomi inline-attr family 238): different recognition mechanism
   (back-reference to preceding quoted text); named follow-up, not
   Phase 5.
8. **(Review P5-4) Line grammar is a total two-pass algorithm over one
   global nesting stack** — specified normatively in Contract 1;
   per-construct stacks are insufficient (they cannot distinguish proper
   nesting from interleaving).

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

**Line grammar (normative, total, two passes; the Python reference model
is `classify_tokens` — with text wrapper `classify_line` — in
`reports/aat-fidelity/bare-toggle-placement.py`, and the Rust
implementation must mirror it test-for-test — the
`reports/lib/terminal_provenance.py` precedent). The token-level entry
point is what lets the delta audit derive expected adoptions and decline
reasons INDEPENDENTLY from the baseline AAT dump's raw marker nodes,
rather than copying the placement report's numbers (review P5-4):**

*Pass 1 — scan the line's bare-toggle tokens in order over ONE global
nesting stack; record candidates and invalid constructs; emit nothing:*

- **open K:** if a frame of construct K is already anywhere on the stack,
  mark K invalid for this line (same-construct reopen); push the frame
  regardless, so scanning stays total and deterministic.
- **close K, stack empty:** mark K invalid (orphan close).
- **close K, top of stack is K:** pop; record a candidate pair (with its
  nesting position).
- **close K, top of stack is J ≠ K:** improper interleaving; mark BOTH J
  and K invalid; do not pop; the close pairs with nothing.
- **end of line:** every frame still on the stack marks its construct
  invalid (orphan open).

*Pass 2 — adoption, only after the whole line's validity is known:*

- A candidate pair is **adopted** iff its construct was not marked
  invalid on this line.
- Every marker of an invalid construct on this line — including the
  markers of candidate pairs rolled back by construct invalidation —
  stays a **byte-identical raw node** (current behavior, no new warning
  codes).
- **Invalidation is construct-scoped per line, decided explicitly
  (review P5-4 item 4):** a valid construct's pair nested inside an
  invalidated construct's markers still adopts. The invalid markers
  remain raw inline nodes; the valid pair never depended on their
  extent. (Corpus consequence: `000106_55753` adopts its 7 valid pairs
  even though another of its lines carries the `（例）` orphan.)
- Rewrites are atomic per line: a line either receives exactly its
  adopted-pair rewrites or is byte-identical.

**Emission (adopted pair):** one `inline_container` node with `kind`
`"yokogumi"` or `"keigakomi"`, whose `content` is the normally-parsed
inline content between the two markers (other inline markup inside the
span parses exactly as it would outside it; a nested adopted pair becomes
a child `inline_container`), and whose `span` covers the first byte of
the open marker through the last byte of the close marker in ADR 0024
decoded-source coordinates. The two marker tokens are consumed by the
adoption (they do not additionally appear as raw nodes) — mirroring how
verbose containerOpen/Close raw pairs disappear into typed blocks.

**Property/model tests (required, review P5-4):** in addition to
corpus-pinned fixtures for every grammar branch (adopt, sequential pairs,
proper nesting, interleave, reopen, orphan open, orphan close,
valid-beside-invalid on one line, rollback of an earlier pair by a later
orphan), the Rust classifier gets a hegel property/model target (the
repository's property-testing idiom, cf. the ruby-oracle spec) over
generated token/text streams asserting at minimum:

- every marker is consumed exactly once (adopted) or preserved exactly
  once (raw) — never both, never neither;
- invalid lines are byte-identical to their input;
- classification is deterministic and idempotent;
- changing one line never affects any other line's output;
- emitted containers form a properly nested tree; parent spans contain
  child spans; visible inline order is preserved.

**Observability preflight (named plumbing task, Phase 4 blocker-5
lesson):** before the classifier is written, a task must verify on real
wire output that the facade delivers bare-toggle markers as observable
in-line nodes with exact source text and spans usable for the emission
contract above. If any facade change is required to observe them, the
facade version bumps (0.3.0 → 0.3.1) and joins C5's identity join key;
otherwise the facade stays 0.3.0. The plan must not assume either
outcome.

**Corpus-bound expectations — PARSER-VISIBLE universe (amendment 3,
checked by the gates, not asserted blindly):** adopted yokogumi pairs
**1,552**; adopted keigakomi pairs 25; declined raw-preserved markers
**14** (all reopen-rollback: the `000094_42338` line); 0 orphan opens;
0 orphan closes; 0 interleavings. The gate's universe is standalone raw
marker nodes in the C4 AAT dump, not source text: 70 source-text markers
(1,582/25/24, the SOURCE-TEXT universe, unchanged and still true of the
source text) never surface as standalone raw nodes — see the placement
report's Revision 4 reconciliation
(`docs/superpowers/reports/2026-07-12-bare-toggle-placement-attribution.md`),
independently re-derived by
`reports/aat-fidelity/bare-toggle-visibility-reconciliation.py` before
this rebinding.

**Decline observability (review P5-10, independence per P5-4):** declines
must be countable per run, not silent — and the counts must be DERIVED,
not copied. The delta audit (Contract 3, gate 1) reconstructs each
line's marker token sequence from the BASELINE dump's raw nodes, runs
`classify_tokens` on it, and thereby computes expected adoptions and
decline reasons per work independently of the placement report; it then
binds the candidate's observed behavior to that derivation. The
placement report is preregistered design-time evidence, not a gate
input. The placement instrument remains a standing tool so any future
corpus rotation re-derives the expectations before re-gating.
Adapter-side warning enrichment stays a non-goal.

## Contract 2 — mapping 0.4.0 (generation-preserving, P5-5)

New inline emission produces AAT paths mapping `0.3.0` has no rules for
(verified: `data/aat-to-parser-ir-mapping-v2.json` carries yokogumi /
keigakomi rules only for the `*_block` kinds — S-05/S-06/S-09/S-10,
U-13/U-14/U-39/U-40). Therefore:

- **Freeze `0.3.0` first:** a byte-exact copy of the current
  `data/aat-to-parser-ir-mapping-v2.json` is committed as
  `data/aat-to-parser-ir-mapping-v2-0.3.0.json` (immutable, the
  `aat-schema-v1.json` precedent) **before** any 0.4.0 edit. The current
  tree must always contain every mapping generation any registry row
  binds — retaining a hash without the value it identifies is not
  retention, and C4 conversion evidence must remain reproducible from
  the checkout.
- `data/aat-to-parser-ir-mapping-v2.json` then becomes `mapping_version`
  `0.4.0` (`source_aat_version` stays `2`), adding transform-rule
  descriptions for the inline `yokogumi` / `keigakomi` container paths
  with observed-occurrence counters measured from the C5 dump (the
  S-08/I-32 accounting pattern: every observed path accounted, no silent
  categories).
- **Span projection stated explicitly (review P5-7):** the AAT
  `inline_container` span is marker-inclusive (Contract 1), but the
  converter intentionally does not project AAT byte bounds — parser-IR
  spans are constructed from visible decoded text
  (`crates/ab-aat-to-parser-ir/src/convert.rs`, `append_visible_content_
  text` region around line 2014). Mapping 0.4.0's rule descriptions for
  these paths must state this projection so "conversion arms already
  exist" cannot be misread as marker-inclusive span preservation.
- **Dispatch binds the generation, not just the AAT version:** with two
  mapping generations for schema v2, AAT-version-only selection is
  insufficient. The converter (and `ab-check` where it validates
  conversions) must select and verify the tuple (AAT schema generation,
  mapping version, mapping hash, mapping file, parser-IR schema hash),
  fail-closed on any mismatch; registry rows carry the full tuple, and
  `verify-phase5-checkpoint.py` verifies the hash of the mapping file
  each gate actually used.
- The `0.3.0` hash
  (`sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`)
  remains the frozen coordinate of the existing rows, now permanently
  backed by the frozen file. The registry is append-only; no retro-edits.
- The converter README's mapping section is updated in the same task
  (versions, hashes, generation table, rule inventory).

## Contract 3 — identity, gates, ceremony (C5)

**Identity discipline (Phase 4 lessons, all binding):** gate instruments
land **before** the candidate identity closes; the candidate commit is
recorded in a machine-readable identity file
(`.superpowers/sdd/phase5-identity.json`) with the full join key
(`ab-aozora 0.6.0 aat-schema 2 facade <0.3.0|0.3.1> wire-schema 3`,
mapping `0.4.0` + hash); candidates are never derived from `HEAD`; every
local build and hinoki run is candidate-commit-bound.

**Gates (all fail-closed, all on the pinned 17,886-entry corpus):**

1. **Delta audit** — new `bare-toggle-adoption` mode in
   `reports/aat-fidelity/audit-aat-delta.py`, **two separate checks**
   (review P5-6):
   - *Delta grammar over differing works:* every C4→C5 AAT difference
     must consist exactly of adopted-pair rewrites (remove the two raw
     marker nodes, insert one `inline_container` of the matching kind
     whose content equals the previously-adjacent inline content and
     whose span satisfies Contract 1). Counters bound to expected
     values — **parser-visible universe (amendment 3)**:
     `adopted_yokogumi_pairs == 1552`,
     `adopted_keigakomi_pairs == 25`. Any other difference class →
     exit 2.
   - *Whole-candidate invariant scan (not derivable from diffs):* over
     the full C5 dump, every declined marker — the **14** (all
     reopen-rollback) — is verified present as a raw node with
     source text and span identical to its C4 counterpart, with decline
     reason counters bound (`orphan_open == 0`, `rollback == 14`,
     `orphan_close == 0`, `interleave == 0`). (The source-text universe's
     1582/25/24 with `orphan_open == 10` remains true of the source text
     but does not bind the gate; see the placement report's Revision 4
     reconciliation for why 70 source-text markers are never
     parser-visible.)
   - Byte equality in both checks is evaluated after substituting the
     adapter identity join key and an explicitly enumerated list of
     allowed metadata changes (and nothing else).
2. **Conformance** — ab-aozora lane 25/25 `must`; seed lane over its 30
   official-docs vectors holds the C4 baseline (22 pass / 8 warning /
   0 fail / 0 skip) with zero regressions; zero drift outside adopted
   works.
3. **Perf** — measure-first on the pinned hash-pinned workset; a median
   regression greater than 10% blocks admission; `001562_56145` remains
   the individually watched work (its +34.8% individual regression is
   median-absorbed but any growth is investigated before admission).
   Inline-path changes touch span composition — this is the phase's
   riskiest gate; the Phase 4 `from_entries` lesson (no
   `serde_json::Value` round-trips on hot paths) applies to the new
   inline assembly.
4. **Conversion audit** — 17,886 attempted / 17,886 succeeded / 0 failed
   under mapping `0.4.0`, with the frozen `0.3.0` file untouched.

**Ceremony order (Phase 4-proven, unchanged):** producer gate reports
frozen → registry row appended to `abc/data/aat-parser-ir-compatibility.edn`
(C5 tuple + mapping 0.4.0 coordinates per Contract 2) → admission run
with byte-exact whole-row equality (copy `--compat-edn-out` verbatim),
`:admitted` captured → run-set repoint: **one atomic commit** updating
the `ab-aozora` entry of `reports/aat-fidelity/run-sets/current.json` to
the C5 dump (`aat_dir`, `adapter_version_contains`, `content_hash`) with
an explicit allowed-paths list; no fixture or run-set content staged
before that commit → `reports/aat-fidelity/verify-phase5-checkpoint.py`
binds the gate summaries, both delta-audit checks, the mapping-generation
tuple of Contract 2, the live admission re-run (fail-closed), and the
repoint commit's tree (parent/child checks, allowed-prefixes pattern from
the Phase 4 verifier) → hinoki dump retention: the C5 dump joins the
never-delete list; the C4 dump `ab-aozora-phase4-c4-27772b1` **remains
never-delete** (prior run-set states reference it) — retention is
append-only.

**Rollback:** `git revert` of the repoint commit restores the C4 lane
binding (mapping 0.3.0 stays present in-tree per Contract 2, so the
reverted state is fully reproducible); registry rows are append-only and
are not removed.

## Contract 4 — keigakomi 44-residual (attribute-or-errata, P5-8)

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
  the hypotheses checked. **An irreducible residual selects no winner**:
  the errata preserves both figures — scanner-defined denominator 673,
  frozen-instrument denominator 717, unresolved difference 44 — and
  requires every future keigakomi rate to be labeled with the denominator
  definition it uses (or reported as an interval over both). Designating
  a single authoritative denominator requires an explained semantic
  choice, which an unexplained residual cannot supply.

Not phase-blocking; C5's gates do not depend on it.

## Contract 5 — ABC custom-contract 0.3.0 integration confirmation (P5-9)

Mechanism per `docs/handoffs/source-region-coverage-abc-integration.md`
and `reports/parser-ir/publication-coverage.py`: confirmation requires the
trusted snapshots (`TRUSTED_ABC_PRESERVATION_SCHEMA_PATH` and the
source-region schema/policy/manifest snapshot hashes) to match the
ABC-side `0.3.0` artifacts, plus the required record/coverage classes and
counters.

Because copying the artifacts makes hash equality true by construction,
the sync is only as strong as its recorded review. The task must produce
a provenance record (in the task's report, cited by the coverage
evidence) containing: the source ABC commit; source and destination
hashes per artifact; the reviewed semantic diff (0.2.x → 0.3.0, what
changed and why it is compatible); the review command evidence; and the
compatibility conclusion. The result is labeled **integration
confirmation** — the two sides agree on the 0.3.0 contract — not an
independent semantic proof.

**Exit assertions (all four, in one regenerated coverage summary):**

- `source_region_contract.verdict == "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`
- custom-contract verdict `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- TEI-profile contract verdict `TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION`
- top-level `IR_PUBLICATION_COVERAGE_COMPLETE`

with the three source-authority occurrence counters still 0 and
`parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`.
Ordering: the confirmation lands **before** the C5 post-repoint coverage
regeneration, so the phase's final coverage evidence is green end-to-end
under the activated C5 wiring.

## Contract 6 — hygiene items (independent commits, not part of C5)

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
- Bare-toggle warning enrichment (declined markers stay silently raw at
  the adapter; observability lives in the audit counters per Contract 1).
- Any edit to frozen evidence reports or the frozen v1 schema/mapping
  pair.

## Task overview (plan will detail)

0. Baseline repair (fmt drift; cst/query stub-feature contradiction; all
   gates green + Phase 4 `CHECKPOINT OK`).
1. Observability preflight (facade wire check for bare toggles).
2. Instruments: `bare-toggle-adoption` delta-audit mode (both checks) +
   tests; `verify-phase5-checkpoint.py` skeleton; grammar fixtures + the
   Rust↔Python model mirror tests.
3. Classifier: two-pass line grammar + inline_container emission +
   branch fixtures + hegel property/model target.
4. Mapping generation freeze (0.3.0 file) + mapping 0.4.0 +
   generation-binding dispatch + converter README refresh.
5. C5 identity close, hinoki full run, four gates.
6. Registry row → admission → atomic run-set repoint → checkpoint.
7. Keigakomi residual attribution/errata (dual-denominator rules per
   Contract 4).
8. ABC 0.3.0 integration confirmation with provenance record + green
   coverage regeneration.
9. Hygiene: README hash line, facade Segments-skip test, CRLF
   suppression (independent commits).
10. Closure: handoff addendum, ledger, memory.
