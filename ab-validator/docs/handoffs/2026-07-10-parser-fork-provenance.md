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

**`ab-aozora-facade` is no longer rename-only (2026-07-10, Phase 2, commits
`aaccd333`/`cd9ea986`)**: fork-owned divergence from upstream `aozora`'s
`json` feature — the wire-projection surface was split into a serde-only
`entries` feature (typed entry structs / `*_entries()` constructors /
`SCHEMA_VERSION`, no `serde_json`, never `preserve_order`) and a narrower
`json` feature (the actual `serde_json`-serializing envelope projection,
still `preserve_order`-requiring). This is fork-owned discretion under ADR
0032 (hard detach — the fork is not obligated to mirror upstream's feature
graph 1:1), taken to let entries-only consumers build without pulling
`serde_json/preserve_order` into their feature graph at all, ahead of the
Phase 2 narrowed feature-unification rule below.

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

## Feature-unification hazard (found at branch finishing)

**Defect.** `crates/ab-aozora-facade`'s `json` feature enables
`serde_json/preserve_order` (required for upstream byte-stable envelopes —
Gate B byte-parity depends on it; this requirement is not being removed).
`crates/ab-aozora-cli` (a workspace member at the time) enabled
`facade/json`, so Cargo's feature-unification rules turned
`preserve_order` on for **every** crate in any `--workspace` build —
including production `ab-aat-to-parser-ir`, whose canonical
sorted-key JSON output silently switched to insertion order.

**Reproduction.** `cargo test --workspace` →
`ab-aat-to-parser-ir` integration suite 50/57 FAIL. `cargo test -p
ab-aat-to-parser-ir --test integration` alone (outside the unified
workspace feature graph) → 57/57 PASS. The 7-test delta is exactly the
set of assertions comparing serialized JSON against a canonical
(sorted-key) golden fixture.

**Fix (isolation).** `ab-aozora-cli` moved from the root workspace
`members` list to `exclude` (mirroring the existing `adapters/aozora-rs`
pattern) and given its own `[workspace]` table in
`crates/ab-aozora-cli/Cargo.toml` so it builds standalone (own
`Cargo.lock`, own `target/` dir under `crates/ab-aozora-cli/target/`).
This removes the shim — and therefore `facade/json` — from the root
workspace's unified feature graph entirely; `preserve_order` no longer
leaks into `ab-aat-to-parser-ir` or any other production crate. The
shim retains byte-parity with upstream inside its own, isolated
workspace (verified via `cargo test --manifest-path
crates/ab-aozora-cli/Cargo.toml` against the pinned upstream binary:
2/2 goldens pass).

**Hard constraint for Phase 2.** `ab-aozora-aat` (or any future crate)
cannot join the root workspace while depending on `facade/json` until
either (a) `ab-aat-to-parser-ir`'s JSON serialization is made canonical
independent of `serde_json` feature state (i.e. explicit key ordering at
the call site, not reliance on `BTreeMap`/default-map ordering), or (b)
the `json` feature's `preserve_order` requirement is otherwise decoupled
from workspace-wide feature unification (e.g. by isolating every
`json`-feature consumer into its own standalone workspace, as done here
for `ab-aozora-cli`).

**Phase 2 resolution (narrowed rule).** Both (a) and (b) landed. (a):
`ab-aat-to-parser-ir::canonical_json::sort_keys_deep` (see
`crates/ab-aat-to-parser-ir/src/canonical_json.rs`) explicitly re-sorts
object keys at every call site that turns a `Value` into canonical bytes,
so its output is byte-canonical regardless of whether the compiling
workspace's unified feature graph resolves `serde_json::Map` to
`BTreeMap` or `IndexMap` — this is the "converter hardening" referenced
below, landed at commit `482279c0`. (b): the root workspace now carries a
standing guard, `tests/workspace-no-preserve-order.sh`, which fails the
build if `preserve_order` ever resolves into the root workspace's unified
feature graph; `tools/preserve-order-canary` is the deliberate,
permanently-excluded `preserve_order` consumer that exercises
`sort_keys_deep` under `IndexMap`-backed `Map` to prove the
canonicalization holds even when insertion order is adversarial — it must
never join the root workspace (see its own `exclude` entry in the root
`Cargo.toml`). What actually runs these two: the `just
preserve-order-hazard-check` recipe (justfile), which runs the guard
script followed by `cargo test --manifest-path
tools/preserve-order-canary/Cargo.toml`; and, independently,
`ab-aozora-aat`'s always-on
`aat_json_from_bytes_is_byte_exact_under_default_map_ordering` unit test
(`crates/ab-aozora-aat/src/lib.rs`), which asserts exact serialized bytes
from a fixed input and therefore fails on its own — with no separate
recipe invocation required — if `preserve_order` ever leaks into that
crate's compiled feature graph and flips its `Value` map ordering. Neither
of these runs automatically as part of plain `cargo test --workspace`
except the latter (it is an ordinary `#[test]` in a workspace member); the
guard script and the canary crate require the dedicated justfile recipe.

With both defenses in place, the hard constraint above is **narrowed**:
any future crate enabling `serde_json/preserve_order` must live outside
the root workspace unless every workspace consumer of canonical JSON is
order-independent by construction. `ab-aozora-facade`'s `entries`/`json`
feature split (see the LIFT_SET annotation above) is the concrete
instance of "order-independent by construction" for the `entries`
feature: it never touches `serde_json`, so it carries no
`preserve_order` risk regardless of what else joins the workspace.

## Shim deleted (Phase 2 end, checkpoint contract)

`crates/ab-aozora-cli` was deleted at the end of Phase 2, superseded by
`ab-aozora` (the Phase 2 umbrella crate covering the shim's inspect
dispatch inside the root workspace). Deletion followed the checkpoint
contract: a fail-closed verifier
(`ab-validator/reports/aat-fidelity/verify-phase2-checkpoint.py`)
independently re-confirmed all three frozen gate summaries attest the
same candidate commit, the same binary (parity/perf), and all PASS,
before the shim's sources, goldens, and standalone `Cargo.lock` were
removed. Evidence:
- `docs/superpowers/reports/2026-07-10-phase2-absorption-parity.md`
- `docs/superpowers/reports/2026-07-10-phase2-perf.md`
- `docs/superpowers/reports/2026-07-10-phase2-conformance-echo.md`

## Phase 3 follow-ups

- **Retire the live crates.io `aozora-pipeline` dependency in
  `ab-aozora-aat`.** `crates/ab-aozora-aat/Cargo.toml` still depends on
  upstream `aozora-pipeline` (pinned exact, `=0.4.1`, per ADR 0032 — no
  floating upstream) and `src/lib.rs` imports
  `aozora_pipeline::lexer::sanitize`, ported verbatim from the frozen
  adapter's dependency surface. This is the one place a non-forked,
  non-vendored upstream crate still runs inside the fork's owning crate;
  the fork's own `ab-aozora-pipeline` (already lifted, see the LIFT_SET
  table above) exposes `ab_aozora_pipeline::lexer::sanitize::sanitize`
  publicly, but behavioral equivalence between crates.io `0.4.1` and the
  `1a4f864` lift has not been verified (the lift postdates the published
  release; e.g. later sanitize changes may not exist in `0.4.1`). Swapping
  the import for the fork's own lexer sanitize path is therefore a Phase 3
  item, gated by Phase 3's own conformance/parity evidence — it is a
  behavior-relevant change to gate-covered code and must not be folded into
  a metadata-only fix.

### Phase 3 closure (2026-07-11)

Phase 3 (capability + span semantics) is CHECKPOINT OK
(`reports/aat-fidelity/verify-phase3-checkpoint.py`, C0=`c3cb16908b0134ca03856735f10445eeb2a92957`,
C1=`a81edf066ecc0e9ac12e04c4ccc2551c27009161`,
C2=`a3f91f53fcae9bc18f577ea5b746f3be7f228fcb`). Follow-up disposition:

- **Sanitize dependency retired.** The crates.io `aozora-pipeline =0.4.1`
  pin described above is gone: `ab-aozora-aat` now calls the fork's own
  `ab_aozora_pipeline::lexer::sanitize::sanitize`, with byte-parity between
  the old and new sanitize path proven by the stage-0 evidence path
  (`docs/superpowers/reports/2026-07-11-phase3-stage0-sanitize-parity.summary.json`,
  17886/0/0 compared/missing/diverged).
- **Diagnostics codes + classifiers landed at rotation A**, gated by the
  rotation-A evidence set (`docs/superpowers/reports/2026-07-11-phase3-capability-delta.summary.json`,
  `…-phase3-capability-conformance-gate.summary.json`,
  `…-phase3-capability-perf.summary.json`) and registered in
  `abc/data/aat-parser-ir-compatibility.edn` (adapter `ab-aozora` 0.2.0,
  git `a81edf06`, rotation A row).
- **Span semantics landed at rotation B per ADR 0024**, gated by the
  rotation-B evidence set (`docs/superpowers/reports/2026-07-11-phase3-span-confinement.summary.json`,
  `…-phase3-span-conformance-gate.summary.json`,
  `…-phase3-span-perf.summary.json`) and registered in
  `abc/data/aat-parser-ir-compatibility.edn` (adapter `ab-aozora` 0.3.0,
  git `a3f91f53`, rotation B row); during rotation-B evidence gathering a
  bare-CR line-index bug surfaced (line-start counting undercounted files
  using lone `\r` terminators) and was fixed parser-side in
  `a3f91f53fcae9bc18f577ea5b746f3be7f228fcb` (`crates/ab-aozora-aat/src/lib.rs`,
  line boundaries now honor `\n`/`\r\n`/bare `\r` uniformly), which is why
  the rotation-B candidate commit moved from the originally-planned
  `5afe01c9` to `a3f91f53` mid-phase.
- **Upstream-first obligation closed per ADR 0032's replacement clause**:
  now that the fork's own sanitize path has passed stage-0 byte-parity
  gates, the fork owns its full sanitize→AAT pipeline with no live,
  non-vendored upstream crate remaining inside `ab-aozora-aat`'s
  behavior-relevant dependency surface.
- **Deferred to Phase 4:** jizume AAT surfacing (the recognizer functions
  exist per Task 8 but are not wired into block emission; jizume corpus
  AAT stays byte-stable through Phase 3) and parser-IR schema/warning
  enrichment.
- **Phase 4 watch item:** rotation-B perf shows median −4.09% (candidate
  faster) but 3 of the 6 workset works individually regressed (up to
  +34.77%) — acceptable under the gate's median threshold but worth
  tracking if span-confinement cost grows with future schema work
  (`docs/superpowers/reports/2026-07-11-phase3-span-perf.summary.json`).
- **Also carried on the Phase 4 ledger** (recorded in the execution ledger,
  omitted from the list above): the bare-toggle marker forms
  （［＃横組み］…終わり ~3,188, ［＃罫囲み］ toggles ~25）as the classifier
  ceiling; the keigakomi 44-marker denominator residual (673 matrix vs 717
  frozen, UNRESOLVED); and the `:corpus` label normalization trap at
  admission (registry equality is byte-exact — rows must copy tool-emitted
  labels verbatim).

### Phase 4 closure (2026-07-12)

Phase 4 (level-3 admission + activation + legacy-lane retirement) is
CHECKPOINT OK (`reports/aat-fidelity/verify-phase4-checkpoint.py`; see
`docs/superpowers/reports/2026-07-12-phase4-acceptance-wholesale.md`).
Candidates and join keys:

| Candidate | Commit | Version join key |
|---|---|---|
| C3 | `c2b9b396271755bc0b898fdaed17c9c9cbe4666d` | `ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3` |
| C4 | `27772b1b75c9ceeb0b724095bbbb47f774f3a275` | `ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3` |

Activation commit: `1333b43dd103480d9064935cd1cf3027706531ee` (`ab-aozora`
swapped in as the sole publication lane in
`reports/aat-fidelity/run-sets/current.json`, five active lanes). Retirement
of the legacy `aozora` crate/lane (this commit) follows the activation
commit and is the final Phase 4 task; rollback for either is `git revert`.

**Evidence inventory** (six gates, two audits, one split, one admission
capture, one bundle validation, one wholesale checkpoint, pre/post coverage):

- Six gates: `2026-07-12-phase4-c3-delta`, `…-c3-conformance-gate`,
  `…-c3-perf` (C3); `…-c4-confinement`, `…-c4-conformance-gate`, `…-c4-perf`
  (C4) — all under `docs/superpowers/reports/`.
- Two conversion audits: `2026-07-12-ab-aozora-phase4-c3-conversion-audit`,
  `…-c4-conversion-audit` (17886/17886/0 parsed/raw-preserved/diagnostic,
  both candidates).
- Split: `2026-07-12-terminal-provenance-colophon-split` (Revision 2;
  `classes.identical + classes.source_note_appended == 17886`,
  `classes.source_note_appended == split.works_with_terminal_provenance ==
  17735`).
- Admission: `2026-07-12-phase4-admission-report.txt` (`:status :admitted`,
  re-run live by the checkpoint verifier, not just a stale capture).
- Bundle: `2026-07-12-phase4-bundle-validation`
  (`PHASE4_BUNDLE_VALIDATION_PASS_WITH_PREEXISTING_BLOCKED_FINDING`).
- Wholesale: `2026-07-12-phase4-acceptance-wholesale` (`CHECKPOINT OK`).
- Pre/post coverage: `2026-07-12-phase4-preadmission-coverage` and
  `…-phase4-postactivation-coverage` (both assert
  `source_authority_gate.gate_status == SOURCE_AUTHORITY_GATE_PASS` with the
  three occurrence counters at 0, and
  `parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`).

**Retained dumps** (never delete): `ab-aozora-phase4-c3-c2b9b39` and
`ab-aozora-phase4-c4-27772b1` under `/db/ab-validator/aat-corpus/` join the
never-delete list (alongside the Phase 3 `ab-aozora-phase3-span-a3f91f5`
dump). The superseded pre-perf-fix C3 attempt dump,
`ab-aozora-phase4-c3-f71432a` (the `+11.54%` BLOCK later resolved by the
`c2b9b396` perf fix), is DELETABLE on hinoki — not deleted by this task;
cleanup is a controller action.

**Perf trajectory — closes the Phase 3 −4.09% watch item**: Phase 3 span
rotation B measured −4.09% (candidate faster) with per-work regressions up
to +34.77%. Phase 4 C3 (`c2b9b396`) measured **−7.96%** workset median
(`2026-07-12-phase4-c3-perf.summary.json`), and C4 (`27772b1b`) measured
**−7.93%** (`2026-07-12-phase4-c4-perf.summary.json`), both well inside the
≤10% threshold and both improving on the Phase 3 margin (the C3 run also
recovered from an interim `f71432a` pre-fix attempt that measured +11.54%
and BLOCKed). Per-work spread stayed stable C3→C4 (every delta within 0.2pp
of its C3 value); the heaviest v2-feature-density work, `001562_56145`,
individually regressed +34.89% at C3 / +34.76% at C4 — consistent with the
Phase 3 watch item's per-work spread and still comfortably absorbed by the
workset median. The watch item is closed: the trend held through both
Phase 4 rotations with no growth.

**Carried forward past Phase 4** (unresolved, not gating any Phase 4 gate):

- Bare-toggle marker forms (`［＃横組み］…終わり` ~3,188, bare
  `［＃罫囲み］` ~25) — explicit Phase 4 non-goal, raw-preserved.
- Keigakomi 44-marker denominator residual (673 matrix vs 717 frozen,
  UNRESOLVED; `docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.md`).
- Warigaki/kunten vocabulary — awaits its own vocabulary ADR; both remain
  raw-preserved and surface as `classified_but_not_admitted` closure-gap
  entries (rule IDs `A-07`, `A-29`, `A-32`, `A-102`, `A-108`, `A-112`), not
  true unsupported gaps.
- ABC custom-contract `0.3.0`-candidate classified-gaps drift: the ABC
  source-region policy rotated to `policy_version 0.3.0`
  (`source-region-policy-measurement-status-drift`) before the Python-side
  `reports/lib/source_region.py` `SOURCE_REGION_POLICY_VERSION` pin was
  updated to match, so `IR_PUBLICATION_COVERAGE_*` reads
  `BLOCKED_SOURCE_REGION_CONTRACT_MISSING`/`BLOCKED_CLASSIFIED_GAPS` rather
  than `COMPLETE` in the raw coverage-report verdict; the three required
  zero counters and `FIVE_PARSER_EVIDENCE_COMPLETE` are unaffected (traced
  in `2026-07-12-phase4-acceptance-wholesale.md`, Step 1).
- Four legacy comparison-lane dumps (`aozora-rs`, `aozora2`, `aozora2html`,
  `aozora-epub3`) are absent from hinoki `/db` — pre-existing, unrelated to
  any Phase 4 gate (comparison-lane evidence only, not live-lane evidence).
- `verify-golden-spans.py` CRLF false positives — a pre-existing,
  pre-documented CRLF-normalization projection artifact (not a span bug),
  green modulo this one known false-positive family since Phase 3.

Legacy-lane retirement (this commit): `adapters/aozora/` (the crate),
`reports/aat-fidelity/run-aozora-aat-full.sh`, the `upstream-aozora-src`
flake input, every `AB_AOZORA_BIN` binding, and the corresponding
justfile/flake/script lanes are removed. `ab-aozora` is now the sole live
Aozora parser lane; the legacy lane's Phase 4 evidence remains archived at
`reports/aat-fidelity/run-sets/2026-07-12-aozora-legacy-archive.json`
(content hash retained, nix `source` pin dropped since its flake input no
longer exists). Rollback for the retirement is `git revert` of this commit
(and, if reverting activation too, of `1333b43d` as well, in that order).
