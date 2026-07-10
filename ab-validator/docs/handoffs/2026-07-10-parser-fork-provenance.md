# Parser fork provenance (hard detach)

Date: 2026-07-10
Authority: ADR 0030 (selection), ADR 0032 (hard detach),
`docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`.

Note: 2026-07-10: ADR renumbered 0031→0032 after main allocated 0031 to
governance validation; frozen gate-evidence JSONs retain the binary's
original 0031-bearing version string.

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
| aozora-corpus (test-support, dev-only) | ab-aozora-corpus | (none) |

**LIFT_SET extension (2026-07-10, Task 3 escalation resolved)**:
`aozora-corpus` added as a 10th, dev-only, test-support crate. It is a
`publish = false` dev-dependency of `aozora`/`ab-aozora-facade` only
(`[dev-dependencies] aozora-corpus = { path = "../aozora-corpus" }`),
consumed by 3 of the umbrella's test files
(`corpus_sweep.rs`/`corpus_incremental_merge.rs`/`corpus_splice_tiling.rs`).
Task 2's dependency-closure grep scanned `[dependencies]` contents only
(its `sed` range stopped at the first `[dev-dependencies]`/
`[build-dependencies]`/`[features]`/`[[...]]` heading), so this dev-only
edge was not surfaced until Task 3 attempted the lift and hit a STOP
condition (documented in `.superpowers/sdd/task-3-report.md`). Controller
ruling: extend LIFT_SET per the plan's Global Constraint ("test-only
workspace deps are lifted too") — `aozora-corpus` is the same category as
`aozora-proptest`: dev-only, `publish = false`, 2,572 lines, no network
code, zero `aozora-*` deps of its own (verified: `aozora-corpus/Cargo.toml`
`[dependencies]` lists only `blake3`/`num_cpus`/`rayon`/`thiserror`/
`walkdir`/`zstd`, no aozora-internal deps).

Closure notes (Step 1 evidence):
- `aozora-render` depends on `aozora-spec`, `aozora-syntax`, `aozora-pipeline`
  — all three are already in the expected LIFT_SET, so this is a diamond
  dependency (umbrella depends on both `render` and `pipeline` directly),
  not a cycle, and does **not** trigger the closure-growth STOP condition.
- No `aozora-*` crate outside the expected nine appeared anywhere in the
  Step 1 scan. Closure confirmed closed at the expected LIFT_SET.

Not lifted: aozora-cst / aozora-query (feature-gated off in the umbrella —
Step 2 evidence), aozora-cli (the shim in crates/ab-aozora-cli reimplements
only the 4-kind inspect dispatch (nodes, diagnostics, gaiji, pairs) over
`ab_aozora_facade::json`), and all bindings/tooling crates per the design's
minimal-core decision.

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

== aozora-corpus
blake3 = { workspace = true }
num_cpus = { workspace = true }
rayon = { workspace = true }
thiserror = { workspace = true }
walkdir = { workspace = true }
zstd = { workspace = true }
tempfile = { workspace = true }                         # [dev-dependencies]
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
| blake3 | 1.5 |
| num_cpus | 1.16 |
| rayon | 1.12 |
| walkdir | 2.5 |
| zstd | 0.13 |
| tempfile | 3.27 (dev-dep only) |

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

Note: Task 2's dependency-closure scan covered `[dependencies]` contents
only — `[dev-dependencies]` (and `[build-dependencies]`/`[features]`)
blocks were out of its grep range by construction. `aozora-corpus`, a
dev-only dependency of the umbrella crate, was therefore not discovered
by Task 2 and was instead surfaced empirically at lift time (Task 3, when
`ab-aozora-facade`'s dev-dependency on it failed to resolve). See the
LIFT_SET extension note above.

### Task 4: cargo-deny audit — genuinely-new third-party crates

Computed via `cargo tree -p ab-aozora-facade --features json
--no-default-features` and `cargo tree -p ab-aozora-corpus`, name+version
pairs unioned and diffed against `main:ab-validator/Cargo.lock`'s crate
set (`git show main:ab-validator/Cargo.lock`, read-only — main's checkout
at `/home/bor/Projects/soranoha` was not modified). Beyond the 10
`ab-aozora-*` crates themselves, the following third-party crates are
genuinely new to the dependency graph (not present in `main` at any
version):

| crate | version | pulled in by |
| --- | --- | --- |
| arrayref | 0.3.9 | blake3 (aozora-corpus) |
| arrayvec | 0.7.8 | blake3 (aozora-corpus) |
| blake3 | 1.8.5 | aozora-corpus (direct dep) |
| constant_time_eq | 0.4.2 | blake3 (aozora-corpus) |
| miette | 7.6.0 | aozora-syntax / aozora-spec / aozora-encoding (direct dep) |
| miette-derive | 7.6.0 | miette |
| num_cpus | 1.17.0 | aozora-corpus (direct dep) |
| phf | 0.14.0 | aozora-encoding (direct dep) |
| phf_codegen | 0.14.0 | aozora-encoding (direct build-dep) |
| phf_generator | 0.14.0 | phf_codegen |
| phf_macros | 0.14.0 | phf |
| phf_shared | 0.14.0 | phf / phf_codegen / phf_generator / phf_macros |
| siphasher | 1.0.3 | phf_shared |
| unicode-width | 0.1.14 | miette |

`crossbeam-epoch` is deliberately excluded from this "genuinely new"
list: it was already present in `main`'s `Cargo.lock` at v0.9.18 (pulled
in via `rayon`, pre-existing); this branch only changed its resolved
version (to v0.9.20, see Follow-ups below), it did not introduce the
crate.

**`deny.toml` was introduced by this task.** No `cargo-deny` gate existed
in the `ab-validator` workspace before this commit — `cargo deny check`
previously had no config file to run against (default config only,
which fails on this graph's `MIT OR Apache-2.0` compound licenses). The
new `ab-validator/deny.toml` allows a permissive license set (plus two
single-term exceptions: `MPL-2.0` for `option-ext`, `CDLA-Permissive-2.0`
for `webpki-root-certs`) and ignores three unmaintained-crate advisories
plus downgrades the `num-bigint` yanked-crate lint to a warning, all four
pre-existing on `main` before the parser fork. `cargo deny check` now
exits 0.

#### Follow-ups (pre-existing, outside Phase 1 scope)

All four items below were confirmed present in `main:ab-validator/Cargo.lock`
before this branch's changes (i.e., not introduced by the ab-aozora-* lift
or this task) and are deliberately deferred rather than fixed here:

- `RUSTSEC-2024-0436` — `paste` unmaintained (creator archived repo, no
  safe upgrade available); reached via `argmin` → `rucrf` →
  `vibrato-rkyv` → `ab-morph-analyzers`.
- `RUSTSEC-2025-0141` — `bincode` unmaintained, two versions in the graph
  (`1.3.3` via `ab-ortho-detect(-ml)`, `2.0.1` via `rucrf`/`vaporetto`/
  `vibrato-rkyv`).
- `RUSTSEC-2024-0384` — `instant` unmaintained; reached via `argmin`.
- `num-bigint` v0.4.7 yanked version; reached via `num` → `arrow-*`/
  `parquet` → `ab-morph-run`/`ab-warehouse`.

One additional pre-existing item was **fixed in-branch, not deferred**:
`RUSTSEC-2026-0204` (security vulnerability — invalid pointer dereference
in `crossbeam-epoch`'s `fmt::Pointer` impl, reached via `rayon`, pulled in
by the newly-lifted `ab-aozora-corpus` among others) was resolved by
`cargo update -p crossbeam-epoch --precise 0.9.20` (lockfile-only patch
bump; `crossbeam-epoch` was already a transitive dependency on `main` at
v0.9.18, so this is a version bump, not a new dependency). `cargo build
--workspace` and `cargo test --workspace` both pass clean after the bump
(1755 tests, 0 failures).

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
| aozora-corpus | tests/ (none); unit tests inline in src/archive.rs, src/error.rs, src/filesystem.rs, src/in_memory.rs, src/lib.rs, src/parallel.rs, src/vendored.rs (64 unit tests total) | all | none (no `[[bench]]`) |

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
- Gate B: `FORK_CONFORMANCE_PARITY_CONFIRMED` —
  `docs/superpowers/reports/2026-07-10-fork-parity-conformance.md`
  (byte-identical scoring on the 127-vector P4suta suite and the
  30-vector official-docs seed; one shim CLI-surface gap — missing
  `inspect pairs` — was caught by the gate and fixed in `1fa0316c`
  before the confirming re-run)
- Perf gate: `PASS` — [2026-07-10 fork-parity perf report](docs/superpowers/reports/2026-07-10-fork-parity-perf.md) (workset `perf-workset-v1`, 6 works, 1+5 runs/bin; regression -0.07% vs 10% threshold; no new timeouts)
- Gate A: `FORK_PARITY_CONFIRMED` — [2026-07-10 fork-parity corpus report](docs/superpowers/reports/2026-07-10-fork-parity-corpus.md) (all 17,886 works: reference resolved fail-closed via run-set `current-aat-fidelity-2026-07-09` with content hash verified, fork dump generated under explicit `--aozora-bin` at rev `2263b92a` on hinoki, semantic JSON parity with the single allowlisted pointer `/meta/adapter_version`; 0 missing, 0 diverged, 0 `fatal_error`)
