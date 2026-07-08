# Ortho `HistoricalToModern` Scope + `determinism_tier` Interaction — Design (U3, U4)

**Date:** 2026-07-08
**Status:** RESOLVED (design). No code change lands from this spec; it pins the
semantics and the constraints a future implementer inherits.
**Decisions carried in:** U3 → **keep `HistoricalToModern` reserved as a
documented Phase-3 lane** (user, 2026-07-08); U4 → **document the invariant,
defer any tier gate** (user, 2026-07-08).
**Parent:** `2026-07-08-ortho-normalized-tokenizer-input-policy-design.md`
(Issue 2). Closes that spec's open questions U3 and U4.
**Prior art:** `2026-07-05-ortho-detect-design.md` (Phase 3 = historical
orthography, future); `abc/docs/v0-design-bundle/tokenizer-determinism.md`
(Exact-tier classification); `abc/docs/superpowers/specs/2026-07-07-analysis-artifact-identity-design.md`
(effective-tier / cache-policy design).

## Why these two together

Both are the residual "confirm before pinning" questions left open when Issue 2
(P0–P5) landed. Neither requires code now; each needs its semantics pinned so
the next implementer does not have to re-derive them and does not accidentally
break an invariant. They are unrelated in mechanism but identical in kind: a
reserved surface (a normalization kind; a determinism claim) whose contract was
implicit.

---

## U3 — `HistoricalToModern` scope

### What it is today (evidence)

`OrthoNormalization::HistoricalToModern` is a **typed placeholder with no
implementation**:

- Declared in `ab-validator/crates/ab-ortho-detect/src/types.rs:30` with the
  gloss "Historical kana → modern kana. Dictionary-backed. Not reversible. No
  detector emits this in v1; reserved for future use."
- The only place it is *constructed* in the `ab-ortho-detect` crate is the
  policy unit test `kind_order_is_significant`
  (`policy.rs:239-257`), which uses it solely to prove kind **order** changes
  the policy hash. No production path builds it.
- Both detectors hardcode the other kind and call the mechanical converter:
  `HeuristicV1::detect` (`heuristic.rs:80,85`) and
  `MlLogisticRegression::detect` (`ml.rs:93,98`) both set
  `kind: ScriptKatakanaToHiragana` and `normalized_text = kata_to_hira(...)`.
  The heuristic even **rejects** any sentence containing hiragana
  (`heuristic.rs:100-102`), so historical-kana prose (けふ, ゐ) cannot reach it.
- `kata_to_hira` (`script.rs:18-96`) is purely mechanical (codepoint
  arithmetic + static tables). It has **no** historical→modern logic. It
  *produces* historical kana ゐ/ゑ as output of obsolete-katakana mapping
  (ヸ→ゐ゛, ヹ→ゑ゛ at `script.rs:35-42`) but never consumes or modernizes them.
- The kind is declared in the parser-IR schema enum
  (`abc/schemas/parser-ir.schema.json:207` and its nix mirror) as one of
  `["ScriptKatakanaToHiragana", "HistoricalToModern"]`.

Historical kana otherwise appears in the repo only as **corpus data and
analysis probes**, never as a converter: e.g. the read-only DuckDB probe
`reports/morph-warehouse/queries/050-historical-kana-probes.sql`, and
segmentation-disagreement triage notes (植ゑつけた, あつた).

### The genuinely different transform

`HistoricalToModern` is not a harder version of kata→hira; it is a different
class of transform, which is why it cannot be bolted onto the existing path:

| Axis | `ScriptKatakanaToHiragana` (active) | `HistoricalToModern` (reserved) |
|---|---|---|
| Mechanism | Char-by-char, context-free | Word-level, dictionary-backed |
| Reversibility | Near-bijective (only ヴ/obsolete kana perturb byte length) | Many-to-one (けふ→きょう), not reversible |
| Detector | Character cascade / ML sigmoid | Needs a lexicon or learned model |
| Identity binding | `HeuristicV1` (no external artifact) or `MlLogisticRegression{model_hash}` | Needs a **dictionary hash** — no `OrthoDetectorId` variant carries one today |

### Decision: keep reserved, and pin the future contract

Per the user decision, the variant **stays** as a documented Phase-3 lane (the
original `2026-07-05-ortho-detect-design.md:477-481` already scoped it there,
and `reports/…/2026-07-05-phase2-human-recall.md:109` warns against building
Phase 3 on a detector that misses 36% of legitimate targets). We do **not**
remove it (removal would churn the parser-IR enum now and again on re-add) and
we do **not** design the dictionary detector now (the dictionary choice is
unresolved — `reports/…/2026-07-05-phase2-investigation.md:259-265`, "UniDic
`lForm`? full lexicon vs. common auxiliaries?").

What this spec pins so the future lane starts designed, not blank:

1. **`kinds` stays an open set within `POLICY_SCHEMA_VERSION`
   `ortho-input-normalization-v1`.** Adding `HistoricalToModern` to a policy's
   `kinds` only changes the hash of policies that *use* it (the descriptor shape
   is unchanged), so no schema-version bump and no re-identification of existing
   `ScriptKatakanaToHiragana`/identity artifacts is forced by the future kind.
   v1 detectors emit only `ScriptKatakanaToHiragana`; this is structural today
   (both detectors hardcode it) and need not be enforced by a new assertion.
2. **A `HistoricalToModern` detector must bind its dictionary into identity.**
   `OrthoDetectorId` (`types.rs:47-54`) currently has `HeuristicV1` and
   `MlLogisticRegression{model_hash}`. A dictionary-backed detector needs a new
   variant carrying a `dictionary_hash` (mirroring how `model_hash` binds the ML
   model), so the policy hash distinguishes two historical dictionaries. Without
   it the policy identity would be under-specified and the determinism claim
   (U4) unpinnable.
3. **Remap is whole-span-only for historical spans.** `ortho_normalize`
   (`lib.rs:42-80`) already emits one offset-map entry per annotation covering
   the whole sentence span, and `OffsetMap::to_original` (`types.rs:86-146`)
   remaps a length-changing entry cleanly *only* when the queried span exactly
   covers it; any interior sub-span returns `CrossesBoundary`. For kata→hira the
   per-char offset is usually stable, so interior token spans usually remap; for
   a word-level historical transform there is no clean intra-span byte
   correspondence, so **interior token spans will systematically hit
   `CrossesBoundary`**. That path is already honest (the warehouse writes an
   `ortho_remap_crosses_boundary` error row — Issue 2 Invariant 4), so the
   failure is diagnostic, not a crash — but a Phase-3 detector should expect it
   and either annotate at token granularity or accept whole-span-only remap.
4. **Gate detection on `orthographic_style` metadata.** The corpus already
   classifies each work's orthography — `metadata-record.schema.json:70-72`
   enumerates `新字新仮名 / 新字旧仮名 / 旧字新仮名 / 旧字旧仮名 / その他`, validated at
   `import_aozora.rs:133`. A future `HistoricalToModern` detector should fire
   only on works whose `orthographic_style` is a 旧仮名 (old-kana) variant,
   rather than detecting historical orthography blind. This narrows the recall
   problem and reuses an existing, hash-bound identity coordinate.

### U3 non-goals

- No 旧字体→新字体 (kanji-form) normalization — out of scope for the ortho lane
  entirely; `orthographic_style` records it but nothing normalizes it.
- No change to `ScriptKatakanaToHiragana` behavior or the `kata_to_hira` table.
- No parser-IR enum change now (the variant stays declared).

---

## U4 — `determinism_tier` interaction

### What it is today (evidence)

`determinism_tier` is **declarative provenance, consumed by no code yet**:

- Tokenizer profile allows four values —
  `tokenizer-profile.schema.json:105-107`
  `["exact", "stable", "bounded", "exploratory"]`, required at `:21`.
- Analysis recipe allows **only** `["exact"]` —
  `analysis-recipe.schema.json:57`, required at `:18`. Both shipped recipes and
  the fixture profile declare `exact`
  (`fixture-tokenizer-ja-v1.json:10`, the two `analysis-recipes/*.json`).
- **No Rust or Clojure production code reads or branches on it** — it is
  validated only by the JSON-schema enum and participates in artifact identity
  by being one of the hashed profile/recipe fields. (Grep over `abc/src` and
  `ab-validator/crates` finds no consumer; the only hits are schema tests and
  unrelated `stable`/`exact` substrings.)

The tier semantics are defined in `tokenizer-determinism.md`: **Exact = run the
fixture twice in the same environment, output is byte-identical**
(`:45-53, 71-76`), and the **normalization policy is one of the required pins**
for that classification (`:18-20`). A tokenizer that cannot meet exact
repeatability is `Stable`/`Bounded` instead.

The *designed* (not-yet-built) governance is in
`2026-07-07-analysis-artifact-identity-design.md:601-620`: cache/replay policy
uses an **effective tier = the lowest tier across the recipe, input-view
producer, tokenizer profile, model weights, and auxiliary artifacts**. `exact`
→ content-addressed release replay (verify `content_hash`); `stable`/`bounded`
→ input-addressed, trusted signed cache only; `exploratory` → local, not
published.

### Decision: they are orthogonal; document the invariant, defer the gate

`determinism_tier` and `input_normalization_policy_hash` are **independent
sibling fields** on the tokenizer-profile object with no schema coupling
(the only `if/then` in the schema relates `thread_policy.mode`↔`max_threads`).
They stay orthogonal because **every implemented normalization is
deterministic**:

- `ScriptKatakanaToHiragana` is a pure function — no float, no I/O, no model
  (`script.rs:18-96`); the heuristic is documented deterministic
  (`types.rs:42`).
- The ML detector is deterministic **given a pinned model**: the model is bound
  by `model_hash` into `OrthoDetectorId` and therefore into the policy hash
  (`ml.rs:22-35`, `policy.rs:218-236`); scoring is a fixed sigmoid over pinned
  weights (`ml.rs:70-77`). No note in the ML reports flags float
  nondeterminism as a live risk.

Therefore **declaring a non-identity normalization policy does not, by itself,
lower a profile's `determinism_tier`.** The normalization step is already one of
the pinned inputs the Exact classification covers, and Issue 2's P5
reproducibility golden
(`ab-morph-analyzers/tests/ortho_reproducibility_golden.rs`) is precisely the
"run twice → byte-identical derived input" evidence the classification procedure
(`tokenizer-determinism.md:45-53`) demands for the normalization dimension.

The invariant to record (for whoever builds the effective-tier machinery):

> **Effective-tier rule for normalization.** When the effective tier is
> computed (per `analysis-artifact-identity-design.md:601-620`), the input
> normalization policy is one of its inputs. A normalization contributes tier
> `exact` iff it is deterministic *and* every artifact it depends on is pinned
> into `input_normalization_policy_hash` (the detector, and for ML the
> `model_hash`; for a future `HistoricalToModern`, the `dictionary_hash` from
> U3). A future non-deterministic normalizer, or one whose dictionary/model is
> not bound into the policy hash, contributes a tier below `exact` and pulls the
> min down. Mechanical kata→hira and the pinned-model ML path both contribute
> `exact`.

We **defer** building any gate: nothing consumes `determinism_tier` today and no
recipe accepts a non-exact tier, so a profile/recipe agreement check or an
effective-tier computation would be enforcement with no caller (YAGNI). The
"pinned-detector guardrail" (asserting non-identity normalization requires a
pinned detector id) is **already structurally true** — `HeuristicV1` is
deterministic by construction and `MlLogisticRegression` cannot exist without a
loaded, hashed model — so it needs no new code.

### U4 non-goals

- No new schema field, no coupling between `determinism_tier` and
  `input_normalization_policy_hash`, no version bump.
- No effective-tier computation, no profile↔recipe tier agreement check now.
- No change to the recipe schema's `exact`-only restriction.

---

## Decisions

| # | Decision | Rationale |
|---|---|---|
| I2-D15 | Keep `HistoricalToModern` reserved as a documented Phase-3 lane; do not remove, do not design the dictionary detector now | It has no implementation and an unresolved dictionary; removal churns the parser-IR enum twice, and reports advise against building Phase 3 on the current detector's recall. |
| I2-D16 | `kinds` stays an OPEN set within `ortho-input-normalization-v1`; v1 emits only `ScriptKatakanaToHiragana` (structural, not asserted) | Adding a kind changes only the hash of policies that use it; no schema-version bump or re-identification of existing artifacts is forced. |
| I2-D17 | A future `HistoricalToModern` detector must add an `OrthoDetectorId` variant binding a `dictionary_hash`, and should gate on `orthographic_style` (旧仮名) metadata | Pins the policy identity (needed for the U4 determinism claim) and reuses an existing hash-bound coordinate to narrow recall; whole-span-only remap is an accepted, already-honest constraint. |
| I2-D18 | `determinism_tier` and `input_normalization_policy_hash` are orthogonal; a deterministic (mechanical or pinned-ML) normalization preserves the tier | The normalization policy is already a required pin for Exact classification; every implemented kind is deterministic and pinned into the policy hash. |
| I2-D19 | Document the effective-tier rule for normalization; defer building any tier gate | Nothing consumes `determinism_tier` today and no recipe accepts non-exact; a gate would be enforcement with no caller. The rule is recorded for when the effective-tier machinery is built. |

## Open questions (deferred, not blocking)

- **Historical-orthography dictionary choice** (from the original ortho design's
  Open Question #3): UniDic `lForm` vs. a full historical lexicon vs. a
  common-auxiliary subset. Belongs to the Phase-3 issue, not here.
- **Effective-tier machinery** (from `analysis-artifact-identity-design.md`):
  when built, it must take the normalization policy as an input per I2-D19.
