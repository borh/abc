# Parser fork provenance (hard detach)

Date: 2026-07-10
Authority: ADR 0030 (selection), ADR 0031 (hard detach),
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`.

## Detach point

- Upstream: `github.com/P4suta/aozora`
- Rev: `1a4f864603970983719655aa4af4525958ac2d38` (flake input
  `upstream-aozora-src`, locked 2026-07-08; confirmed against
  `flake.lock`'s `upstream-aozora-src.locked.rev`, `narHash =
  sha256-XS7BdCpcbfJoTRYcrHWwcCErmQFRfFF7U7FUQU2Ezsg=`)
- Resolved source path this run: `nix eval --raw
  .#upstream-parser-aozora.src` → `/nix/store/fli8hpdmsvz4m72c8rxlc25kjmq0j20z-source`
- License: workspace `license = "Apache-2.0 OR MIT"` (root `LICENSE-APACHE`,
  `LICENSE-MIT`, `NOTICE` files present at upstream repo root; no
  per-crate LICENSE files — crates reference the workspace license field
  only, so the root files are what must be copied into each lifted crate)

## Lifted crates (LIFT_SET)

| upstream crate | fork crate | workspace-internal deps |
| --- | --- | --- |
| aozora-spec | ab-aozora-spec | (none); dev-dep only: aozora-proptest (path) |
| aozora-veb | ab-aozora-veb | (none); dev-dep only: aozora-proptest (path) |
| aozora-encoding | ab-aozora-encoding | (none; build-deps: external only — `phf_codegen`, no aozora-* build-deps) |
| aozora-scan | ab-aozora-scan | spec |
| aozora-syntax | ab-aozora-syntax | spec, veb, encoding |
| aozora-pipeline | ab-aozora-pipeline | spec, syntax, encoding, scan |
| aozora-render | ab-aozora-render | spec, syntax, pipeline |
| aozora (umbrella) | ab-aozora-facade | render, pipeline, syntax, spec, encoding (required); proptest, cst, query (optional, feature-gated — cst/query not lifted, see below) |
| aozora-proptest (test-support) | ab-aozora-proptest | (none) |

Closure notes (Step 1 evidence):
- `aozora-render` depends on `aozora-spec`, `aozora-syntax`, `aozora-pipeline`
  — all three are already in the expected LIFT_SET, so this is a diamond
  dependency (umbrella depends on both `render` and `pipeline` directly),
  not a cycle, and does **not** trigger the closure-growth STOP condition.
- No `aozora-*` crate outside the expected nine appeared anywhere in the
  Step 1 scan. Closure confirmed closed at the expected LIFT_SET.

Not lifted: aozora-cst / aozora-query (feature-gated off in the umbrella —
Step 2 evidence), aozora-cli (the shim in crates/ab-aozora-cli reimplements
only the 3-kind inspect dispatch over `ab_aozora_facade::json`), and all
bindings/tooling crates per the design's minimal-core decision.

## Step 2 evidence: cst/query feature gating

`aozora/Cargo.toml` `[features]`:
```
default = []
json = ["dep:serde", "dep:serde_json", "serde_json/preserve_order"]
schema = ["json", "dep:schemars"]
proptest = ["dep:proptest", "dep:aozora-proptest"]
cst = ["dep:aozora-cst"]
query = ["cst", "dep:aozora-query"]
```
`default = []` — `cst` and `query` are both optional and absent from
`default`. `lib.rs` gates the re-export blocks with `#[cfg(feature =
"cst")]` (line 246) and `#[cfg(feature = "query")]` (line 281). STOP
condition does not apply: cst/query are confirmed feature-gated off by
default.

## External dependency surface added

Per-crate external (non-`aozora-*`) dependency declarations (Step 3
`grep` output, includes each crate's `[features]` block where present):

```
== aozora
serde = { workspace = true, optional = true }
serde_json = { workspace = true, optional = true }
schemars = { workspace = true, optional = true }
proptest = { workspace = true, optional = true }
default = []
json = ["dep:serde", "dep:serde_json", "serde_json/preserve_order"]
schema = ["json", "dep:schemars"]
proptest = ["dep:proptest", "dep:aozora-proptest"]
cst = ["dep:aozora-cst"]
query = ["cst", "dep:aozora-query"]

== aozora-render
memchr = { workspace = true }

== aozora-pipeline
aho-corasick = { workspace = true, features = ["std"] }
memchr = { workspace = true }
smallvec = { workspace = true }

== aozora-syntax
thiserror = { workspace = true }
miette = { workspace = true }
phf = { workspace = true }
serde = { workspace = true, optional = true }
default = []
serde = ["dep:serde", "aozora-spec/serde"]

== aozora-scan
aho-corasick = { workspace = true, optional = true }
bumpalo = { workspace = true }

== aozora-spec
thiserror = { workspace = true }
miette = { workspace = true }
serde = { workspace = true, optional = true }

== aozora-veb
(none)

== aozora-encoding
encoding_rs = { workspace = true }
thiserror = { workspace = true }
miette = { workspace = true }
phf = { version = "0.14", default-features = false }   # [dependencies]
phf_codegen = "0.14"                                    # [build-dependencies]

== aozora-proptest
proptest = { workspace = true }
```

Workspace-pinned versions for the crates.io deps actually used above
(from `$SRC/Cargo.toml` `[workspace.dependencies]`; Task 3 copies these
exact versions):

| crate | version |
| --- | --- |
| thiserror | 2.0.18 |
| miette | 7.6.0 |
| serde | 1.0 (features = ["derive"]) |
| serde_json | 1.0 |
| schemars | 1.0 (features = ["preserve_order"]) |
| proptest | 1.11 |
| bumpalo | 3.20 (features = ["collections"]) |
| smallvec | 1.15 |
| memchr | 2.8 |
| aho-corasick | 1.1 (default-features = false; `std` turned on per-consumer) |
| phf | 0.14 (workspace default features = ["macros"]; aozora-encoding overrides to `default-features = false`) |
| phf_codegen | 0.14 (declared ad hoc in aozora-encoding, not in workspace.dependencies) |
| encoding_rs | 0.8.35 |

Note: `phf_codegen = "0.14"` is declared directly in
`aozora-encoding/Cargo.toml`'s `[build-dependencies]`, not via
`workspace = true` — it is the one dependency in the LIFT_SET surface not
present in the upstream `[workspace.dependencies]` table. Task 3 must add
it explicitly (not copy-from-workspace-table) when standing up the forked
crate's manifest.

Upstream workspace license/version metadata (for Task 3's Cargo.toml
headers): `version = "0.4.1"`, `edition = "2024"`, `rust-version =
"1.96.0"`, `license = "Apache-2.0 OR MIT"`.

Full audit of docs.rs / feature-flag surface is deferred to Task 4 per
the template.

## Test inventory

| crate | inherited tests (files) | retained | dropped |
| --- | --- | --- | --- |
| aozora-spec | tests/property_sentinel.rs; unit/proptest in src/pair.rs, src/offset.rs, src/sentinels.rs, src/trigger.rs, src/span.rs, src/slugs.rs, src/diagnostic.rs | all | none (no `[[bench]]`) |
| aozora-veb | tests/property_eytzinger_search.rs, tests/property_map_keyspace.rs; unit/proptest in src/eytzinger.rs, src/map.rs | all | none (no `[[bench]]`) |
| aozora-encoding | tests/fuzz_regressions.rs, tests/gatekeeper.rs, tests/property.rs; unit/proptest in src/suijun.rs, src/lib.rs, src/gaiji.rs | all | none (no `[[bench]]`) |
| aozora-scan | tests/property_backend_equiv.rs; unit/proptest in src/lib.rs, src/naive.rs, src/trait_def.rs | all | benches only: `scanner_bakeoff` |
| aozora-syntax | tests/ (none); unit/proptest in src/degraded.rs, src/accent.rs, src/format.rs, src/ast/store.rs, src/lint.rs, src/alloc.rs, src/ast/intern.rs, src/lib.rs, src/ast/payload.rs, src/ast/mod.rs | all | benches only: `accent_decompose` |
| aozora-pipeline | tests/authoring_diagnostics.rs, tests/deep_nesting.rs, tests/diagnostic_ordering.rs, tests/fuzz_regressions.rs (+ fuzz_regressions/ dir), tests/property_annotation_unknown.rs, tests/property_lex_output.rs, tests/property_scan_equiv.rs, tests/slug_canonical_round_trip.rs, tests/streaming_semantics.rs (+ snapshots/ dir); unit/proptest in src/lexer/mod.rs, src/lib.rs, src/pipeline.rs, src/lexer/instrumentation.rs, src/fold.rs, src/lexer/classify/gaiji.rs, src/lexer/offset.rs, src/lexer/token.rs, src/lexer/pair.rs, src/lexer/tokenize.rs | all | benches only: `tokenize_compare`, `classify_kaeriten`, `boot` |
| aozora-render | tests/byte_identical_html.rs, tests/fuzz_regressions.rs (+ fuzz_regressions/ dir), tests/gatekeeper.rs, tests/property_emit_symmetry.rs, tests/property_html_roundtrip.rs, tests/property_serialize_idempotent.rs, tests/serialize_fixed_point.rs, tests/snapshot_html_golden.rs (+ snapshots/ dir); unit/proptest in src/lib.rs, src/classes.rs, src/serialize.rs, src/spelling/html.rs | all | none (no `[[bench]]`) |
| aozora (umbrella) | tests/catalogue_properties.rs, tests/corpus_incremental_merge.rs, tests/corpus_splice_tiling.rs, tests/corpus_sweep.rs, tests/json_format.rs, tests/lint_catalogue.rs, tests/property_json_round_trip.rs, tests/property_public_api.rs, tests/splice_api.rs; unit/proptest in src/diagnostics_text.rs, src/json.rs, src/lib.rs, src/document.rs, src/splice.rs, src/incremental.rs | all | none (no `[[bench]]`) |
| aozora-proptest | tests/ (none); src/config.rs, src/generators.rs, src/lib.rs contain no `#[test]`/`proptest!` (it is the generator library consumed by the other crates' tests, not itself a test target) | all (nothing to drop) | none (no `[[bench]]`) |

`.proptest-regressions` files (`catalogue_properties.proptest-regressions`,
`property_json_round_trip.proptest-regressions`,
`property_public_api.proptest-regressions` in `aozora`;
`property_serialize_idempotent.proptest-regressions` in `aozora-render`)
are retained alongside their `.rs` counterparts — they are proptest
regression-seed corpora, not benches, and fall under "no inherited test
deleted."

Rule: no inherited test deleted; `[[bench]]` targets dropped (criterion is
not lifted). Any deviation must be recorded here with a reason before it
lands.

## Verbatim-lift statement

Phase 1 lifts are rename-only (package/lib names, intra-workspace dep
paths, provenance headers). No semantic edits. Gate evidence:
- Gate B: <link conformance comparison report when Task 7 lands>
- Perf gate: <link perf parity report when Task 8 lands>
- Gate A: <link corpus parity report when Task 9 lands>
