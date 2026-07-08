# Ortho Input-View Identity + ABC Bridge — Combined Design (P3+P4)

**Date:** 2026-07-08
**Status:** IMPLEMENTED (2026-07-08). P3 (input-view records the applied
normalization) and P4 (ABC agreement check + Rust T1 provenance emitter) are
landed and green (both suites). One deliberate gap remains: the *live* ABC
publication path does not yet consume the T1 sidecar (ABC has no
warehouse-consumption seam at all yet) — the check is exercised via fixtures.
See "Landed vs. remaining" below.
**Decisions carried in:** field is **required, identity sentinel for off**
(user, 2026-07-08); compute stays in **Rust**, ABC reads (U1/I2-D8); descriptor
is a Rust-owned struct, hash trusted opaquely (U2/I2-D9).
**Parent:** `2026-07-08-ortho-normalized-tokenizer-input-policy-design.md`.

## Why P3 and P4 are one design

P3 adds the field that *records* the applied normalization on the input view;
P4 *populates and checks* it against the tokenizer profile's declaration. The
field is useless without the check, and the check is impossible without the
field — and both edit ABC's hashed identity surface, so they share one cascade
(version bumps, schema-contracts, nix mirrors, fixture regeneration). Splitting
them would re-identify every artifact twice.

## Corrected model — three distinct hashes

Investigation during P3 found the parent spec's F2 conflated two policies. The
correct model has **three** independent hashes on the tokenization path:

| Hash | Lives on | Means | Status |
|---|---|---|---|
| `plaintext_policy_hash` / `policy_hash` | analysis-result `plaintextInputView`; request-set `inputView` | **Projection** policy: parser-IR body → plaintext, `coordinate_system: unicode-scalar-value` | exists, unchanged |
| `input_normalization_policy_hash` (declared) | tokenizer profile | Normalization the profile **expects** on its input | exists (fixture sentinel `sha256:1000…0001`), read at `materialize_tokenized.clj:83` |
| `input_normalization_policy_hash` (applied) | request-set `inputView` + analysis-result `plaintextInputView` | Normalization actually **applied** for this run | **NEW (P3)** |

Projection (parser-IR → plaintext) and normalization (kata→hira on that
plaintext) are orthogonal. P3 adds the *applied* normalization record; it does
**not** touch the projection hash or the `input_view_kind` (still
`parser-ir-plaintext-body-v1`).

## Current state (evidence)

- ABC is **fixture-driven**: `materialize-tokenized!` is exercised only by
  `validate_design_bundle` with `input-plaintext-policy-hash =
  (files/example-hash "13")` (`validate_design_bundle.clj:998`). There is **no
  live warehouse→ABC bridge** — the `parquet`/`duckdb` hits in ABC are
  `read-json` schema loads, not warehouse reads.
- The tokenizer profile already **declares** `input_normalization_policy_hash`
  and it flows into tokenized-manifest `:used` provenance
  (`materialize_tokenized.clj:83`) — but nothing records or checks the *applied*
  value.
- The request-set schema's own JCS hash is embedded in every request-set
  identity object (`request_set_resolver.clj:268` → `analysis_identity.clj:70`),
  so **any** edit to `request-set.schema.json` re-identifies every request-set —
  making explicit-required (Q2) cost nothing extra over optional.
- Rust already produces the applied hash: warehouse
  `runs.input_normalization_policy_hash` (P2), identity sentinel
  `sha256:530c5968…816d3813` when off; heuristic-v1
  `sha256:1670ff1d…3aa0fa2c`.

## P3 — input-view records the applied normalization

**Schema edits (field required, identity sentinel for off — Q2):**

1. `abc/schemas/request-set.schema.json` `$defs.inputView`: add
   `input_normalization_policy_hash` (`$ref: #/$defs/hash`) to `properties` and
   `required`. Bump `version` `0.1.2 → 0.1.3`.
2. `abc/schemas/analysis-result.schema.json` `$defs.plaintextInputView`: add the
   same required field. Bump `version` `0.1.1 → 0.1.2`.
3. `abc/src/abc/tools/analysis_identity.clj`: extend `canonical-input-views`
   sort key (line 52-53) to `(juxt input_view_kind policy_hash
   input_normalization_policy_hash)` so the applied hash participates in identity
   deterministically. `allowed-input-view-kinds` is unchanged.

**Identity/default:** the "off" sentinel value is the **Rust-produced** identity
hash `sha256:530c5968…816d3813` (P1) — ABC uses this constant verbatim, never
recomputes it (U1). Every existing (source-identity) fixture gets this value.

## P4 — ABC reads the applied hash and enforces agreement

**Transport (the narrowed U1 remainder — SUB-DECISION, see Open questions).**
ABC needs the *applied* `input_normalization_policy_hash` from the run. Two ways:

- **T1 (recommended): Rust emits a run-provenance JSON sidecar** next to the
  warehouse (run-level: `run_id`, `ortho_detect_mode`, `detector_id`,
  `input_normalization_policy_hash`). ABC reads JSON (which it already does
  everywhere); no parquet/duckdb dependency enters ABC; keeps compute in Rust and
  hands ABC an ABC-shaped value. Small addition to the Rust runner.
- **T2: ABC reads `runs.parquet` directly** (duckdb/parquet). Fewer artifacts but
  pulls a parquet reader into ABC and couples it to the warehouse layout.

**Agreement check (I2-D2 / F1).** At `materialize-tokenized!` (where the
tokenizer profile and the run meet), assert equality of the three:

```
tokenizer_profile.input_normalization_policy_hash   (declared)
  == input_view.input_normalization_policy_hash      (recorded, P3)
  == runs.input_normalization_policy_hash            (applied, from T1/T2)
```

Any mismatch is a hard error (the run applied a different normalization than the
profile declared). On success, populate the input-view record from the applied
value.

**Fixture wiring.** Replace the tokenizer-profile fixture's sentinel
`input_normalization_policy_hash` (`sha256:1000…0001`,
`abc/data/tokenizer-profiles/fixture-tokenizer-ja-v1.json`) with the real Rust
identity hash `sha256:530c5968…816d3813` (the fixture is source-identity), so the
agreement check passes end-to-end in `validate_design_bundle`.

## Cascade checklist (execute in order, once approved)

1. Edit the two schemas (P3 §1–2) + version bumps.
2. Edit `analysis_identity.clj` sort key (P3 §3).
3. Recompute JCS schema hashes; update **both** `schema-contracts.json`
   (`abc/schemas/` and `ab-validator/data/abc-schemas/`) for request-set +
   analysis-result.
4. Update nix mirrors `ab-validator/data/abc-schemas/nix-schemas/{request-set,
   analysis-result}.schema.json`.
5. Update `abc/test/abc/tools/schema_test.clj` `cross-project-schema-versions`
   (request-set `0.1.2→0.1.3`, analysis-result `0.1.1→0.1.2`) + the
   analysis-result example hash assertions (lines ~119-121).
6. Regenerate affected fixtures with the sentinel field + new identity objects:
   `abc/data/request-sets/{full-corpus-analysis-basic-ja, smoke-basic-ja,
   full-corpus-publication-basic-ja, full-corpus-basic-ja}.json`,
   `abc/examples/v0/example-work/analysis-result.json`, and any
   `request-set-definitions/*` that carry `input_views`.
7. P4 transport (T1): add the Rust run-provenance JSON emitter; P4 check +
   fixture rewire (tokenizer-profile hash).
8. `scripts/monorepo-schema-drift.sh` green; `clojure -M:test` green; Rust
   `cargo test` green.

## Landed vs. remaining

**Landed (2026-07-08):**
- P3: required `input_normalization_policy_hash` on both input views; schemas +
  versions + both contracts + nix mirrors + fixtures regenerated; drift green.
- P4 producer: `materialize-analysis` emits the field (identity sentinel default).
- P4 check: `analysis-identity/assert-input-normalization-agreement!` wired into
  `materialize-tokenized!` (declared ⇄ applied); tokenizer-profile fixture rewired
  to the real identity hash.
- P4 transport (T1): the Rust runner writes `run-normalization-provenance.json`
  (`RUN_NORMALIZATION_PROVENANCE_FILE`) into the final run dir on both the serial
  and merge paths, carrying `input_normalization_policy_hash` for ABC to read.

**Remaining (deliberate, not speculative):**
- The live ABC publication path does not yet *read* the T1 sidecar — ABC has no
  warehouse/run consumption seam today (the whole materialize flow is
  fixture-driven). When that seam is built, `materialize-tokenized!` should be
  passed `:applied-normalization-policy-hash` from the sidecar instead of
  defaulting to the profile's declared value. Building that seam speculatively
  now would be YAGNI; the mechanism (emitter + reader-shaped JSON + check) is in
  place for it.
- P5 (reproducibility golden) still follows.

## Open questions

- **U-P4 (transport): T1 (Rust-emitted run-provenance JSON) vs T2 (ABC reads
  parquet).** Recommend T1. Decide before P4 implementation.
- **U3 — `HistoricalToModern` scope.** Still reserved/out. The applied-hash for
  v1 is always the kata→hira or identity policy; confirm before any run emits a
  second kind.
- **U4 — `determinism_tier` interaction.** The tokenizer profile's
  `determinism_tier` vs. a normalization policy — likely orthogonal (normalization
  is deterministic), but confirm the profile need not change tier when it declares
  a non-identity normalization.

## Non-goals

- No change to the projection `plaintext_policy_hash` semantics or coordinate
  systems.
- No new `input_view_kind`.
- No per-source normalization flags (deferred at P2).
- P5 (reproducibility golden) is separate and follows P4.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| I2-D10 | Projection policy and ortho-normalization policy are distinct hashes; P3 adds a NEW `input_normalization_policy_hash` on the input view, not an overload of `policy_hash` | Corrects F2; they describe orthogonal transforms (parser-IR→plaintext vs kata→hira on plaintext). |
| I2-D11 | The applied-normalization field is **required**, identity sentinel for off | schema_hash re-identifies every artifact on any edit anyway, so explicit-required costs nothing over optional and removes hash ambiguity (Q2). |
| I2-D12 | The "off" sentinel is the Rust-produced identity hash `sha256:530c5968…`, used verbatim by ABC | Single producer (U1); ABC never recomputes it. |
| I2-D13 | P3 and P4 land as one cascade | Both edit hashed identity; splitting re-identifies artifacts twice. |
| I2-D14 | Transport is T1 — Rust emits a run-provenance JSON sidecar ABC reads | Keeps parquet out of ABC; hands ABC an ABC-shaped value; compute stays in Rust. Confirmed (user, 2026-07-08). |
