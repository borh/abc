# Ortho-Normalized Tokenizer-Input Policy — Design (provisional)

**Date:** 2026-07-08
**Status:** design + P0 + P1 + P2 landed (2026-07-08). Load-bearing forks
resolved; U1 (bridge direction → Rust) and U2 (opaque hash boundary) resolved.
P0 (thread `--ortho-detect`), P1 (structured policy identity), and P2 (persist
run-level provenance on the `runs` warehouse table + honest-failure fix) are
implemented; P3–P5 remain.
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

> **CORRECTION (2026-07-08, during P3 investigation).** The original phrasing
> below ("distinguish by `policy_hash`") conflated two *orthogonal* policies.
> The existing `policy_hash` (request-set `inputView`) / `plaintext_policy_hash`
> (analysis-result `plaintextInputView`) is the **plaintext projection** policy
> (parser-IR body → plaintext, `coordinate_system: unicode-scalar-value`) — it is
> NOT the ortho-normalization policy. The normalization policy is a separate field
> the tokenizer profile already declares (`input_normalization_policy_hash`, read
> at `materialize_tokenized.clj:83`). So reusing `parser-ir-plaintext-body-v1`
> stands (the view kind is unchanged), but the ortho identity is a **new field on
> the input view** (`input_normalization_policy_hash`, recording what was
> applied), NOT an overload of the projection `policy_hash`. See the revised P3.

**Reuse `parser-ir-plaintext-body-v1`; add a distinct normalization field.** An
ortho-normalized run still emits plaintext-body morphemes in **source**
coordinates — same *kind* of view, different input normalization. The
`input_view_kind` is unchanged; the applied normalization is recorded by a new
`input_normalization_policy_hash` field on the input view (identity sentinel =
today's default). (Alternative — a new
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
- **P2 — Persist ortho provenance in the Rust world. DONE (2026-07-08).** The
  policy is uniform per run, so it is persisted at run grain on the `runs`
  warehouse table (additive columns, no `SCHEMA_VERSION` bump — same discipline
  as Issue 3's oracle column): `ortho_detect_mode` (`off`|`heuristic`|`ml`),
  `input_normalization_detector_id` (serialized `OrthoDetectorId`, NULL for off),
  and `input_normalization_policy_hash` (the identity ABC reads; identity
  sentinel hash when off). `resolve_run_normalization` computes these once at the
  warehouse top-level (loading the ML model once to bind its `model_hash`),
  threaded through both the serial and parallel/merge runs-row writes.
  **Honest-failure fix (Invariant 4):** P0 exposed a latent hole — an ortho
  remap failure on the **warehouse** path (which has no `errors_writer`) was only
  `eprintln!`'d, never persisted. It now writes an `ortho_remap` error row
  (`ortho_remap_crosses_boundary` / `ortho_remap_uncovered_offset`) to the
  warehouse errors table, mirroring the analyze-failure arm. *Scope note:* the
  per-source "did normalization fire / offset-map non-identity" flags from the
  original P2 sketch are **deferred** — the run-level hash is what the P4 bridge
  consumes; per-source enrichment can be added if P4 shows a need (YAGNI). Not
  persisted on the JSONL `RunManifest` path (warehouse is the ABC-consumed path).
- **P3 — Input-view identity. DONE (2026-07-08).** Added the required
  `input_normalization_policy_hash` field (identity sentinel for off) on both
  `parser-ir-plaintext-body-v1` input views; regenerated the full cross-project
  cascade. See `2026-07-08-ortho-input-view-and-abc-bridge-design.md`.
- **P4 — ABC bridge. DONE (2026-07-08), one deliberate gap.** Agreement check
  (`assert-input-normalization-agreement!`) wired into `materialize-tokenized!`;
  tokenizer-profile fixture rewired to the real identity hash; Rust emits the T1
  `run-normalization-provenance.json` sidecar. The live ABC path does not yet
  read the sidecar (ABC has no warehouse-consumption seam yet) — mechanism in
  place, wiring deferred as YAGNI. T1 transport confirmed.
- **P5 — Reproducibility test. DONE (2026-07-08).** A golden fixture pins the
  detector-driven derivation chain the warehouse run path executes
  (`sentence_split` → `HeuristicV1::detect` → `ortho_normalize` → `remap_spans`)
  end-to-end from a fixed source: the derived (normalized) text, the remapped
  morpheme spans/surfaces in original-doc coordinates, and the recorded
  heuristic-v1 `policy_hash` — the same hash the warehouse run records
  (`resolve_run_normalization`) and the tokenizer profile declares (abc).
  Reproducibility is asserted by re-running the chain and comparing byte-for-byte.
  Test: `ab-morph-analyzers/tests/ortho_reproducibility_golden.rs` +
  `tests/fixtures/ortho-reproducibility-golden.json`. Pins the length-PRESERVING
  kata→hira path (byte spans unchanged, surfaces rebuilt from the original
  katakana); complements `remap_vu.rs`, which pins the length-CHANGING ヴ→う゛
  case. Driven with the character-level cascade + an empty first-pass token
  stream, so it needs no analyzer dictionary and stays a fast unit test. The
  literal pipeline plumbing (AAT read → parquet write) and the on-warehouse hash
  recording are already covered by `warehouse_mode_writes_sealed_parquet_*` and
  `resolve_run_normalization_heuristic_matches_policy_hash`; a heavy real-dictionary
  warehouse run for P5 would duplicate those without adding coverage (YAGNI).

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
