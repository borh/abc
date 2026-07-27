# Handoff: Parser-RQ Corpus Tiering (2026-07-27)

Continues `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`,
which grew 721 → 1,562 lines this session. Read that document's *Status* block
first; this file records what a fresh session needs that the design does not
say about itself.

## State

Branch `parser-rq-corpus-tiering`, **13 commits ahead of `main`**, unmerged and
unpushed. Working tree clean. Nine commits this session, **one of which touches
code** (`3a329ea0`); the rest are the design document.

Tests: **162 tests / 816 assertions / 0 failures** across
`abc.tools.parser-rq-campaign-test`, `parser-release-qualification-test`,
`parser-rq-source-accountability-test`, `validate-design-bundle-test`. The two
errors in wider runs are `TEI_SCHEMA_PATH must be set` and were **proven
pre-existing on a stashed clean tree**. `nix flake check` and the full suite
were **not** run — they need that env var, unset in this environment.

```
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.parser-rq-campaign-test
```

## Decided — five entries now await hand-authored `decisions.edn` records

| Item | Decision | Basis |
|---|---|---|
| **Q11** | Tier 2 binds publication's **successfully-selected** projection; named the *publication-workload snapshot*; carries a declared denominator (`candidates_considered`, `derive_failures`, `rejected`) | Constructibility, not preference: a pre-attempt candidate is `{:file :relpath :row}` — no `:source_sha256` (required by `corpus-snapshot-hash`) and no primary-text member (required by Q9's join key), because both come from the fallible `inspect-selected-work!` |
| **D7** | `unexpected-fatal-failures ≤ 0` | Owner's selection. **Unimplemented**: untangle `allowed_dispositions` first — it lists the *status* value `protocol_error` in a *disposition* vocabulary |
| **Q14** | Fix the instrument, not the threshold | Reading 3 (`accounted/eligible`) rejected in kind — it folds "the parser could not type this" into the numerator. Reading 2 (lower threshold) rejected *for now*, correct only once the instrument measures what it claims |
| **Q16** | Bind the classified-source policy into the identity | **Decided and landed** — the only code change this session |

The fifth pending entry is the amendment to `parser-release-instrument-bindings`
that Q16 implies.

## Closed by measurement

- **Q2** — premise falsified. 2,421 `ParserResidue` occurrences over 299 works,
  **100%** via the `x-provenance == "parser-derived"` arm, **100%** carrying real
  non-zero source spans (61,002 B). The `is_parser_raw_residue` string heuristic
  that Q4 flags as the hazard **never fired once**.
- **D6 corrected** — it named the wrong instrument. `source_span_coverage` is
  ledger-authoritative (`parser-rq-source-recognition-v1`);
  `parser_release_qualification.clj:190` demotes node-span coverage to
  `:parser_ir_node_span_coverage`.
- **Q12** — **0 of 299 real works reach `source_span_coverage = 1.0`** (fold
  0.9640, worst 0.2466) while the governed 3-work corpus is exactly 1.0.
  Confirmed through the **built** `ab-parser-rq-source-accountability` binary,
  byte-identical to the Python reconstruction on all six quantities.
- **Q1** — the capture layer is **232.4 ms/work, 9.25× all of `ab-check`**; only
  2.2% is parsing and 4.2% is the non-authoritative P1 analysis. Byte-proportional
  at ~5,400 ms/MB decoded, so one serial full-snapshot pass is **78–85 min**,
  ≈3.5× the entire ×3 `ab-check` campaign the design had used as its cost model.
- **Q6 narrowed** — 662 KB/work, **15.2× expansion** over decoded source,
  **≈12 GB** per capture generation at 17,878 works. Retention across generations
  and closed-membership cost remain unmeasured.

## The one code change (`3a329ea0`)

`instrument-policy-paths` is now member → vector, and `:source_recognition` binds
both the ignored-regions taxonomy **and**
`data/parser-rq-ab-aozora-classified-source-v1.json`. Wire shape is unchanged
(`member -> sha256`), so `parser-rq-candidate.schema.json` is untouched:
single-document members keep their hash **byte-for-byte**, multi-document members
fold their documents' hashes in declared order. Exactly one coordinate moved —
`source_recognition` `sha256:c099072a…` → `sha256:81547652…`.

> **This rotated `qualification_identity_ref`. Any existing captured evidence is
> stale against the new identity.** Intended: it is what makes the eventual
> policy amendment attributable.

## Open — ranked

1. **The Q15 denominator decision. Owner's call, and it blocks the work.**
   Declaring header/tail ineligible is a denominator reduction — the hazard Q11
   rejected. The argument for it: it stops charging the instrument for bytes it
   was never given, rather than hiding unrecognized body content. But it changes
   what `source_span_coverage = 1.0` *means*, so it should not be decided
   unilaterally.
2. **Then implement Q15 (rescoped), two independent pieces.** (i) Ignored-regions
   rules derived from `aozora_body_range` — **no new heuristic needed**, and it
   handles the works with fewer than two separator lines and no `底本：` line that
   defeated this session's hand-rolled detector. (ii) The **twelve** unmapped
   `DirectiveKind` arms in `node_policy` (`ab-aozora-pipeline/src/fold.rs:154`
   maps only `Unknown` and `WarichuOpen`), plus their `ConstructId` values and
   policy rules. Then **re-measure**: the ≈0.9889 estimate uses this session's
   heuristic, not the real projection.
3. **Q13** — drop or rename `:parser_ir_node_span_coverage`. It now has a cost
   argument as well as a naming one: 9.80 ms/work to produce a number that is
   authoritative for no predicate and reads as a ratio it does not compute.
4. **D7 prerequisites**, then **Q9**, **Q3**, **D8**, **Q5**. Q5's governed host
   class matters considerably more after Q1.

Any policy or `ConstructId` change must also update
`parser-rq-classified-source-authority-v1.json`, which pins the policy by
`raw_bytes_hash` and `identity_hash` and **fails closed** otherwise. The policy
is `include_bytes!`-embedded (`ab-aozora-capture/src/classified_source.rs:20`),
so edits require a rebuild.

## Warnings a reviewer should carry

**The frame mechanism was stated three times and was wrong twice.** First a
"missing `publication_metadata` rule" (inferred from the policy file), then a
corrected byte split, finally the traced answer: the ledger lexes
`decoded.span_text` = `sanitized.body` while `eligible_bytes` counts
`decoded.text`, so `source_span_coverage` divides a body-derived numerator by a
whole-file denominator. **Treat any mechanism in the design document that is not
traced to a specific line as suspect.** The failure mode each time was inferring
a cause from a correlation.

A **buggy frame heuristic** also shipped and was corrected in-document:
`find(b'-'*40)` matches overlapping positions inside a single 55-hyphen rule, so
"the second separator" was the second match inside the *first* rule. That put the
notation legend in "body interior" and inflated it from a corrected **11,293** to
119,144. Both corrections are recorded in the document, not silently fixed.

**Always run the governed 3-work corpus as a control.** A node-span coverage
reconstruction looked like a major finding until the corpus returned
0.58/0.43/0.33 where the predicate passes at 1.0 — which is what revealed the
*reconstruction* was measuring the wrong thing, not the instrument.

## Constraints

- `decisions.edn` edits are authored by hand, never by tooling.
- Published manifests are immutable; `tools/corpus_inventory.py` and
  `tools/corpus_tail_set.py` stay non-authoritative and must not become
  governance inputs.
- Untracked files are invisible to `scripts/python-quality.sh` (it reads
  `git ls-files`) and to `nix flake check` — `git add` before running.
- `examples/dump_ledger.rs` in `ab-aozora-capture` was **deliberately not
  committed**: a permanent example in the release-gated capture crate is governed
  surface for a one-off probe. Recreate as ~12 lines piping stdin through
  `ab_aozora_capture::classified_source_ledger_from_bytes`, then
  `cargo build --release --example dump_ledger -p ab-aozora-capture`.

## Reproduction

The pinned corpus is already local at `/home/bor/Dependencies/aozorabunko` at rev
`0e9ea3e586eb0aa34039fabfc85a407d2f98b165` — **no refetch needed**.

Probe scripts were written to the session scratchpad and do not survive it;
recreate from the design document's method notes if needed:
`q2_residue_probe.py` (residue classification), `q12_ledger_probe.py` (ledger
dispositions and coverage), `partb_diagnose.py` (unaccounted-byte adjacency),
`q1_capture_cost.py` (capture-layer scaling fit), `q14_build_inputs.py`
(`capture-corpus` inputs).

The last builds a **synthesized qualification identity** mirroring the real
parser-IR `derived_from` so per-work records return `ok`. It measures coverage
and **authenticates nothing**; its `qualification_identity_ref` is meaningless
outside those runs.
