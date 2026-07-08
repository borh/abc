# Ortho-Normalized Tokenizer-Input Policy — Design (provisional)

**Date:** 2026-07-08
**Status:** design + P0 + P1 landed (2026-07-08). Load-bearing forks resolved;
U1 (bridge direction → Rust) and U2 (opaque hash boundary) resolved. P0 (thread
`--ortho-detect` through all run paths) and P1 (structured policy identity) are
implemented; P2–P5 remain.
**Owner:** ab-validator (producer) + abc (identity/manifest)
**Related:**
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md` (the
orthographic detector + `<s type="orthographic-katakana">` evidence this reuses),
`2026-07-07-ruby-reading-evidence-contract.md` (the sibling "evidence must not
replace source text" rule).

## Problem

Orthographic evidence already reaches TEI as `<s type="orthographic-katakana">`,
but morph/tokenizer runs do **not** consume an ortho-normalized analyzer input
view in any manifested, reproducible way. The machinery mostly exists but is
gated off, unpersisted, and disconnected from the identity contracts.

## Current state (evidence)

Investigated on `main @ 94a564a`:

- **The derived view already exists — in one code path only.** The serial JSONL
  morph-run does detect → `ortho_normalize(source, annotations)` → tokenize the
  **normalized** doc → `remap_spans` back to **source** coordinates → attach
  `ortho_annotations` / `ortho_offset_map` to the `Analysis`
  (`ab-morph-run/src/pipeline.rs:769-889`). Source text is preserved on the
  `Analysis` after a successful remap (`pipeline.rs:869-873`).
- **Gated off + not threaded.** `--ortho-detect` defaults to `Off`
  (`main.rs:80-97`); the **warehouse**, **parallel-worker**, and **selected**
  paths hardcode `OrthoDetectMode::Off` with a `TODO(phase2-followup)`
  (`pipeline.rs:302,403,1183`). Publication uses the warehouse path, so today it
  always tokenizes source text.
- **No provenance is persisted.** `RunManifest` (`compact.rs:166-181`) records
  only CLI `analyzer_args` strings; the warehouse `RunAnalyzerRow`
  (`ab-warehouse/src/schema.rs:262-267`) records `run_id/analyzer_id/analyzer_arg/
  analyzer_family`. The detector id, offset map, and normalization mode live only
  in memory and are dropped. `detector_id` is never even consulted in the pipeline.
- **The identity contracts are fixture-only.** `tokenizer-profile.schema.json`
  has an opaque `input_normalization_policy_hash` (`abc/schemas/…:68`), populated
  only by `abc/data/tokenizer-profiles/fixture-tokenizer-ja-v1.json` with sentinel
  hashes; no real producer. `input_view_kind` is locked to the single value
  `parser-ir-plaintext-body-v1` in `request-set.schema.json` and
  `analysis_identity.clj:9-10`. (`analysis-result.schema.json` already admits a
  second kind, `token-stream-v1`, but nothing ortho.)
- **The two manifest worlds are disjoint.** The Rust run is keyed by free-text
  analyzer-id strings; the ABC identities are content hashes resolved from fixture
  registries. No code bridges a Rust `analyzer_id` to an ABC
  `tokenizer_profile_hash` / `input_views` entry.
- **The detector is span-scoped and typed.** `ortho_normalize` normalizes only
  detector-flagged spans (`ab-ortho-detect/src/lib.rs:40-78`); kinds are
  `ScriptKatakanaToHiragana` (active) and `HistoricalToModern` (reserved);
  `detector_id` is `HeuristicV1 | MlLogisticRegression{model_hash}`
  (`types.rs:47-54`).

## Goal

Make an ortho-normalized analyzer input view a **first-class, reproducible,
manifested** option: source text stays preserved and output stays in source
coordinates; the tokenizer input may be a derived ortho-normalized view; and both
the Rust run and the ABC identity record which normalization policy produced that
input.

## Invariants (non-negotiable)

1. **Source text preserved.** The derived view is analyzer input only; TEI /
   parser-IR / warehouse source text and all output spans stay in source
   coordinates (guaranteed today by `remap_spans`; the spec pins it).
2. **Single source of ortho truth.** The *same* detector annotations drive both
   the TEI `<s type="orthographic-katakana">` evidence and the analyzer input
   normalization — never two independent normalizers.
3. **Reproducible.** The derived input is a pure function of (source text,
   normalization policy); the policy is content-hashed so a run is reproducible
   from source + policy hash.
4. **Honest on failure.** If span remap fails, the run does not silently emit
   normalized-coordinate output as if it were source coordinates (today it routes
   an `ortho_remap_error`); the spec keeps that a hard, recorded error.

## Design forks (recommendations + tradeoffs)

### F1 — Where the normalization identity lives

| Option | Verdict |
|---|---|
| On the **input view** `policy_hash` only | Under-specified: the tokenizer profile can't declare what input it expects. |
| On the **tokenizer-profile** `input_normalization_policy_hash` only | Under-specified: an analysis result can't record what was actually applied. |
| **Both, with distinct roles + a validation that they agree (RECOMMENDED)** | The tokenizer profile *declares* the input normalization it expects; the input view (request-set + analysis-result) *records* what was applied; a run is valid iff `profile.input_normalization_policy_hash == input_view.policy_hash`. Closes the loop with one canonical value carried in two roles (declaration vs. actuality). |

### F2 — New `input_view_kind` vs. reuse

**Reuse `parser-ir-plaintext-body-v1`; distinguish by `policy_hash`.** An
ortho-normalized run still emits plaintext-body morphemes in **source**
coordinates — same *kind* of view, different input normalization. The existing
`{input_view_kind, policy_hash}` pair already models this. Define a canonical
**identity policy** (`policy_hash` = hash of the no-op policy) as today's default
and an **ortho-normalized-v1 policy** as the new option. (Alternative — a new
`…-ortho-normalized-v1` kind — is more explicit but adds enum churn and wrongly
implies different output coordinates; rejected.)

### F3 — What `input_normalization_policy_hash` covers (structured identity)

Give the currently-opaque hash a **structured, canonicalized policy descriptor**
so it is reproducible and auditable, e.g.:

```json
{
  "policy_schema_version": "ortho-input-normalization-v1",
  "algorithm": "ortho-normalize-v1",
  "detector": { "detector_id": "HeuristicV1" },
  "kinds": ["ScriptKatakanaToHiragana"],
  "coordinate_system": "source-preserving-remap"
}
```

`detector` carries `{ "detector_id": "HeuristicV1" }` or
`{ "detector_id": {"MlLogisticRegression": {"model_hash": "sha256:…"}}}`. The
**identity policy** is the canonical descriptor with an empty `kinds` (or an
`"algorithm": "identity"` sentinel), producing today's default hash. The hash is
JCS-SHA256 of this descriptor, computed identically in Rust and ABC (the same
discipline as `tokenizer-profile-hash`).

### F4 — Span-scoped, detector-driven (pin the existing behavior)

The derived view normalizes **only detector-flagged spans**, and the annotations
are shared provenance with the TEI evidence (Invariant 2). This is already how
`ortho_normalize` works; the spec makes it a decision, not an accident. No
whole-document blind fold.

### F5 — Default stays opt-in

Ortho-normalization is selected per tokenizer-profile / request (source-identity
remains the default). The input view + profile record which applied, so a corpus
can carry both source-identity and ortho-normalized analyses side by side, each
independently identified.

## Decomposition (phases)

- **P0 — Thread `--ortho-detect` through all run paths. DONE (2026-07-08).** The
  three `TODO(phase2-followup)` hardcodes are removed: the warehouse serial branch,
  the warehouse parallel worker, and the `selected`/`rerun-full` path now honor the
  detector. `run_analyze_aat_warehouse` and `run_analyze_aat_selected` gained
  trailing `(ortho_detect, ortho_ml_model)` params; `WarehouseParallelOptions`
  carries them to each worker; the `rerun-full` CLI gained `--ortho-detect` /
  `--ortho-ml-model` (with the same `ml`-requires-model guard as `analyze`) so a
  rerun can reproduce the original run's tokenization. Behavior at the `Off`
  default is unchanged (243 existing tests green). *Known follow-up (perf, not
  correctness):* in the parallel path the detector is rebuilt per batch inside
  `run_analyze_aat_serial`; when `Heuristic` is requested **without** the default
  vibrato analyzer in the run's analyzer set, each batch reloads UniDic. When
  vibrato IS in the set (the common corpus case) the loaded dictionary is reused.
  Hoisting the detector to once-per-worker is a later optimization.
- **P1 — Structured normalization-policy identity. DONE (2026-07-08).** Added
  `ab_ortho_detect::policy::NormalizationPolicy` — the structured descriptor
  (`policy_schema_version` + `algorithm` + `detector` + `kinds` +
  `coordinate_system`), its JCS-SHA256 `policy_hash()`, and the `identity()`
  sentinel. Canonicalization mirrors `abc.tools.jcs` (sorted keys, compact,
  UTF-8); the descriptor uses only slash-free ASCII strings/arrays so the
  escaping-sensitive corners of RFC 8785 never fire and ABC would agree
  byte-for-byte if it ever verified. **Agreed constants (Rust is the sole
  producer; ABC reads these opaquely):**
  - identity sentinel:
    `sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813`
    (canonical: `{"algorithm":"identity","coordinate_system":"source-preserving-remap","detector":null,"kinds":[],"policy_schema_version":"ortho-input-normalization-v1"}`)
  - heuristic-v1 kata→hira:
    `sha256:1670ff1d5ff27575dc63ffd448cb140b3d497247bb7b36e1b4e2f1623aa0fa2c`
    (`detector.detector_id = "HeuristicV1"`, `kinds = ["ScriptKatakanaToHiragana"]`)
  - ML runs bind the model bytes: `detector.detector_id =
    {"MlLogisticRegression":{"model_hash":"sha256:…"}}`, so each model is a
    distinct policy identity.
  Both hashes are pinned by unit tests; a change to canonicalization breaks them
  loudly. Not yet wired into any manifest (that is P2).
- **P2 — Persist ortho provenance in the Rust world.** Record per analysis:
  ortho mode, `detector_id`, `input_normalization_policy_hash`, remap status, and
  whether the offset map was non-identity — in `RunManifest` and/or a warehouse
  row. This is the data the ABC bridge consumes.
- **P3 — Input-view identity.** Allow a non-identity `policy_hash` on
  `parser-ir-plaintext-body-v1` input views (request-set + analysis-result);
  keep Invariant 1. No new enum value (F2).
- **P4 — ABC bridge.** Populate `tokenizer-profile.input_normalization_policy_hash`
  from the real policy; record the applied policy in request-set `input_views`;
  add the profile ⇄ input-view agreement check (F1).
- **P5 — Reproducibility test.** A run + its recorded policy hash regenerate the
  identical derived input from source; a golden fixture pins detector →
  normalized-text → remapped spans end-to-end on the warehouse path.

## Open questions (incubate before committing P2/P4)

- **U1 — Bridge direction (architectural). RESOLVED (2026-07-08):** all
  compute-heavy work stays in **Rust**. The Rust runner computes the normalization
  policy descriptor + hash and persists the provenance (P2); **ABC reads** that
  Rust-produced provenance to populate identities — ABC does not recompute
  normalization or re-derive the policy hash. This fixes the ownership: Rust is the
  producer of the policy identity, ABC is the consumer/recorder. The remaining P4
  question is narrowed to the *transport* (which warehouse row/manifest field ABC
  reads), not who computes.
- **U2 — Structured vs. opaque hash boundary. RESOLVED (2026-07-08):** the
  descriptor stays a **Rust-owned struct**; ABC trusts the `policy_hash` opaquely
  and gets **no** separate checked-in JSON schema for the descriptor. This follows
  from U1 (I2-D8): ABC never validates or recomputes the descriptor, so a
  checked-in schema would be governance surface with no consumer. The canonical
  form + agreed hashes are documented here for auditability, and the Rust unit
  tests are the guard. If a future consumer needs to *construct* (not just read) a
  policy hash outside Rust, revisit and promote the descriptor to a checked-in
  schema then.
- **U3 — `HistoricalToModern` scope.** The reserved kind is out of the v1 policy;
  confirm before pinning `kinds` as a closed set in the descriptor.
- **U4 — `determinism_tier` interaction.** How the tokenizer-profile
  `determinism_tier` composes with a normalization policy (does normalization
  change the tier?). Likely orthogonal, but confirm.

## Non-goals

- Ruby-driven tokenizer input (Issue 3 — ruby stays reading evidence).
- Any change to source text, output spans, sentence segmentation, or parser-IR.
- New detectors or normalization kinds beyond the existing typed set.
- Phase-5 ranking / RRF integration.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| I2-D1 | Reuse `parser-ir-plaintext-body-v1`; carry normalization on `policy_hash` | Output stays source-coordinate plaintext body; the view kind is unchanged, only the input normalization differs. |
| I2-D2 | Normalization identity lives on both the profile (declares) and the input view (records), with a run-time agreement check | One canonical value, two roles; closes the identity loop. |
| I2-D3 | `input_normalization_policy_hash` = JCS-SHA256 of a structured policy descriptor (algorithm + detector_id + kinds) | Reproducible and auditable, not an opaque sentinel. |
| I2-D4 | Derived view is span-scoped and detector-driven; annotations are shared with the TEI evidence | Single source of ortho truth (Invariant 2). |
| I2-D5 | Ortho-normalization stays opt-in; source-identity is the default policy | Corpus can carry both, each independently identified. |
| I2-D6 | Thread `--ortho-detect` through warehouse/parallel/selected paths (P0) | Publication runs on the warehouse path; without this the feature can't ship. |
| I2-D7 | Keep source preservation + honest-remap-failure as hard invariants | Matches the ruby-evidence contract's "evidence never replaces source." |
| I2-D8 | Bridge direction (U1): compute-heavy work stays in Rust — Rust produces the policy identity + provenance, ABC reads it | Single producer of the normalization policy hash; ABC never recomputes normalization. |
| I2-D9 | Hash boundary (U2): policy descriptor is a Rust-owned struct; ABC trusts `policy_hash` opaquely, no checked-in descriptor schema | ABC never validates/recomputes the descriptor, so a schema would add governance surface with no consumer. |
