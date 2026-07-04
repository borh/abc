# aozora (P4suta) Adapter Design

**Date:** 2026-07-04
**Status:** Design spec / provisional — produced via `hammock-driven-design`;
routed from `architecture-triage`. Pending `rich-hickey-review` once
direction is confirmed; pending `deepening-review` once the adapter exists.
**Scope:** Add a fifth adapter, `aozora`, wrapping the P4suta `aozora` Rust
parser (`references/parsers/aozora`, v0.4.1) at the same tier as `aozora2`
and `aozora-rs`.
**Prerequisite reading:**
- `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`
  (AAT JSON is the normative adapter contract; `ab-ir` is not).
- `docs/adapter-fidelity.md` (fidelity matrix and per-adapter policy).
- `docs/aat-contract.md` (AAT semantics, axes, selectors).
- `P4suta/aozora-notation-spec` pinned by git revision before use. This is
  an unofficial notation specification and conformance-vector suite; it is
  comparison evidence, not the source-authority gate.

---

## Review-lens routing (architecture-triage)

| Finding | Route | Status |
|---|---|---|
| Direction/seam choice unsettled; high cost-of-being-wrong; unverified assumptions about aozora's surfaces | `hammock-driven-design` | This spec — seam settled (§2.3) |
| Provenance, dep pinning, contract-leak, gaiji faithfulness policy, two-coordinate-system reconciliation, version pin grounding | `rich-hickey-review` | Folded into this revision (§4.1.1, §4.2, §4.4, U4, §5); see response table below |
| Shared AAT-builder abstraction across now-5 mappers | `deepening-review` | Queued after adapter exists (§8 caveat now noted) |
| Adapter already at one implementation; no braided cleanup in existing code | (no `codebase-simplification`) | N/A |

### rich-hickey-review response

All empirical claims were verified against the codebase (`receiving-code-review`
discipline: verify, then act) before amending.

| # | Finding | Severity | Resolution |
|---|---|---|---|
| 1 | Version pin ungrounded (workspace `version = 0.4.1` ≠ checkout HEAD) | Blocker (overstated) | Verified: `git tag` lists `v0.4.1`; `git describe` = `v0.4.1-235-g5df2cfa`; HEAD `5df2cfa` is 235 commits past the tag; workspace `version = "0.4.1"` is stale metadata, not an absent tag. **Corrected: pin an exact commit SHA; U4 now states this and §5 cites the SHA, not the stale 0.4.1 label.** |
| 2 | `"no ab-*"` policy stricter than the established Direct-tier pattern | Strong suggestion | Verified: `aozora2` depends on `ab-source-syntax`; `aozora-rs` used `ab-ir`+`ab-source-syntax`; only `aozora2html`/`aozora-epub3` are zero-`ab-*`. `decode_source_bytes` is duplicated 3× already. **Adopted graded policy (§3, §4, §4.4).** |
| 3 | Library `Tree` surface asserted, not verified; mapper complexity hidden | Strong suggestion | Verified `SourceNodeOwned`/`NodeRefOwned`/`ContainerPair`/`PairLink` shapes. The mapper **must reconcile two coordinate systems** (source vs normalized). **Added §4.1.1 traversal sketch.** |
| 4 | Drift mitigation conflates harness stability with measurement continuity | Question | **Added to §5 matrix row: schema-stable but node-kind projections may drift across upstream versions.** |
| 5 | Direct-fidelity tier unqualified | Strong suggestion | **§5 row is now "Provisional Direct" pending U1 characterization gate.** |
| 6 | House-style gaps vs epub3 design (wire contract, test strategy, `--mode html` fidelity) | Strong suggestion | **Added §4.5 wire contract, §4.6 `--mode html` fidelity note, §9 test strategy, §10 build/Nix, §11 files-to-modify, §12 falsifier table.** |
| 7 | Smoke-check reproducibility hazard | Strong suggestion | **§10 pins smoke fixtures to the already-pinned aozora reference checkout (conformance fixtures), not to TEI-EAJ counterparts or `/db`.** |
| 8 | Shared-builder deferral misses AST-vs-XHTML distinction | Nit | **Added sentence to §8.** |
| 9 | Provenance routing well-executed; risk calibration tight enough | Nit | Acknowledged; U1/U4 risk re-leveled to blocking-for-fidelity-claim, non-blocking-for-seam. |
| 10 | Drift-mitigation framing precision | Nit | **§4.2 reworded to name the ab-ir-preemption path explicitly.** |

---

## 1. Problem statement

The P4suta `aozora` parser is a large, native-Rust Aozora Bunko notation
parser checked out at `references/parsers/aozora`. Unlike the existing
adapter targets, it is not a wrapped gem/JVM tool; it is a Rust workspace
with a single front-door library crate `aozora` ("Document + Tree"). The
harness should evaluate it as a fifth adapter, comparable to the existing
four, without breaking the adapter boundary decision.

The integration must:

1. Emit AAT JSON conforming to `data/aat-schema.json` — the normative
   contract — and depend on **no `ab-*` crate as a contract**. (`ab-ir` may
   be used only as an optional in-workspace convenience, never as the
   stable external adapter API.)
2. Match the existing adapter wire contract: stdin honbun bytes → AAT JSON
   on stdout, `--version`, `--mode {aat,html}`, exit-code semantics.
3. Be classifiable as **Direct** fidelity in `docs/adapter-fidelity.md`
   (parser-normalized nodes + upstream gaiji resolution), matching the
   `aozora2` / `aozora-rs` tier — not the `aozora2html` Indirect tier.
4. Fit the existing build / test / flake / justfile infrastructure.
5. Avoid the drift failure that left `aozora-rs` broken: never treat the
   upstream parser's Rust AST API as a versioned contract.

---

## 2. Probe results (decisive evidence)

Two disposable probes were run to settle the seam decision before writing
implementation code. Their results are recorded here as evidence, not
re-derived later.

### 2.1 Dependency weight (probe A)

`cargo tree -p aozora -e no-dev` in the aozora checkout shows the `aozora`
umbrella crate pulls **only**: `aozora-encoding`, `aozora-pipeline`,
`aozora-scan`, `aozora-spec`, `aozora-syntax`, `aozora-render`,
`aozora-veb`, plus routine small crates (`encoding_rs`, `miette`,
`thiserror`, `phf`, `aho-corasick`, `smallvec`, `bumpalo`, `memchr`).
**The wasm / ffi / py / lsp / pandoc / bench crates are NOT pulled by the
`aozora` library crate** — they are separate workspace members. Dep weight
for a library seam is low.

### 2.2 Output surfaces (probe B)

Three upstream output surfaces were exercised against the conformance
fixture set at `references/parsers/aozora/crates/aozora-conformance/fixtures/render/`
(124 fixtures covering ruby subtypes, gaiji variants, accent, bouten,
bold/italic block+inline, heading, keigakomi, font-size, alignment,
warichu, etc.):

| Surface | Carries | Loses | Viable as adapter boundary? |
|---|---|---|---|
| `aozora inspect {nodes,pairs,container-pairs}` JSON envelope | kind + source span; container kind via container-pairs; gaiji resolution | **content, tree nesting** — envelope is a flat per-node span list, not a tree | **No.** A faithful Direct AAT needs text content + nesting; this surface cannot supply them. |
| `aozora pandoc` (Pandoc AST JSON, `pandoc-api-version` 1.23) | nested tree, content, `aozora-*` class tags, gaiji description/mencode, bouten metadata, container classes | **some container kinds** — `bold_block`, `heading_block` project as `aozora-container-unknown` even though `inspect container-pairs` returns `kind=bold`; Aozora headings are not Pandoc `Header` | **Partial.** Rich enough for content+nesting, but loses container-kind labels that the introspection envelope preserves. Subprocess seam would need to combine two surfaces. |
| Library `Tree` (`Document::new(src).parse()`) | `source_nodes()`, `pairs()`, `container_pairs()` (with kind), `diagnostics()`, `to_html_with()`, `to_source_with()`, `sanitized()`, full owned AST | (none — this is the parser's own representation) | **Yes — Direct fidelity.** |

### 2.3 Crux settlement

**Neither subprocess surface alone is sufficient for a Direct-fidelity
adapter.** The introspection envelopes are span-only; the Pandoc projection
drops container-kind labels and does not map Aozora headings to Pandoc
Headers. Combining both subprocess surfaces would be a lossy compromise
that re-derives nesting from offsets across two inconsistent coordinate
systems — exactly the kind of braided bookkeeping `rich-hickey-review`
flags.

**Therefore the adapter must use the library `Tree` (seam A).** This is
the same shape as `aozora2` and `aozora-rs`. The drift risk that sank
`aozora-rs` is mitigated by §4 (pin + contract boundary), not by avoiding
the library seam.

### 2.4 Independent notation-spec comparison source

The separate `P4suta/aozora-notation-spec` repository is useful, but it must
have the right role. It is not an official Aozora Bunko standard and it does
not replace this repo's source-authority gate. It is a comparison oracle for
parser behavior:

- the README describes an RFC-style draft specification with ABNF grammar,
  processing model, diagnostics, and machine-readable conformance vectors;
- the cloned head inspected for this design was
  `b60665fd50b596c967254f99b61f418495656fef`;
- that head contains 127 `conformance/vectors/*/vector.json` cases: 25
  `must`, 99 `should`, and 3 `may`;
- the vector schema requires a `[provenance:...]` note, which is the right
  anti-circularity property for an oracle: expected results are not simply
  copied from one parser's output;
- the catalogue covers families that are currently high-priority for the
  source-authority burn-down: editor annotations, structural markers,
  headings, indentation/layout, kunten/kaeriten, tables/columns, fractions,
  gaiji, warichu, keigakomi, horizontal writing, and side glyphs.

Use this repository in two ways:

1. **Adapter conformance comparison.** Run the new `aozora` adapter, and
   later all adapters where possible, against the vectors and report
   pass/warn/fail by vector level and projection (`nodes`, `pairs`,
   `diagnostics`, `serialize`, optional `html`).
2. **Matrix gap audit.** Diff the vector feature families against
   `data/aozora-syntax-coverage.toml` and the source-inventory unknown
   classes. A vector can justify adding a test case or a source pattern, but
   full-corpus source inventory remains the authority for representability
   coverage claims.

---

## 3. Design goals and constraints

| Goal | How we satisfy it |
|---|---|
| Direct fidelity | Map `Tree::source_nodes` / `pairs` / `container_pairs` / `diagnostics` → AAT; record upstream gaiji resolution as-emitted (no inline oracle patching). |
| Boundary compliance | Adapter emits AAT JSON validated against `data/aat-schema.json`; depends on `aozora` + `aozora-encoding` only; depends on **no `ab-*` crate** (the schema-only path proven by `aozora2html`). |
| Drift safety | Pin an exact aozora rev in `flake.nix`; vendor lockfile; the adapter contract is AAT JSON, never the upstream Rust AST. If `aozora`'s AST API changes, only the mapper changes — the harness boundary is unaffected. |
| Adapter parity | Same CLI contract, same `meta` fields, same exit-code semantics as `aozora-epub3-adapter` and `aozora2html-adapter`. |
| Reproducibility | Nix builds the pinned reference `aozora` crate offline from a vendored lockfile; flake smoke check mirrors `aozora-epub3-smoke-check`. |
| Independent conformance evidence | Pin `aozora-notation-spec` separately and run its vectors as a parser-behavior comparison suite. Do not convert vector success into a TEI representability claim without source-inventory and AAT/IR evidence. |

---

## 4. Module decomposition

```
adapters/aozora/
  Cargo.toml          # depends on aozora + aozora-encoding (pinned); no ab-*
  src/
    main.rs            # stdin → Tree → AAT JSON / HTML; --version, --mode
    lib.rs             # mapper entry: Tree -> serde_json::Value AAT
    aat.rs             # node-kind → AAT block/inline projection
    gaiji.rs           # upstream gaiji resolution → AAT gaiji nodes
    source.rs          # encoding detection, honbun extraction, meta provenance
  tests/
    smoke.rs           # round-trip AAT on representative fixtures
benches/
  adapter_bench.rs     # optional, parity with aozora-rs adapter
```

### 4.1 Adapter entry point (Rust library seam)

- **Input:** honbun bytes on stdin (UTF-8 or Shift_JIS; `aozora-encoding::decode_auto`).
- **Parse:** `Document::new(src).parse()` → `Tree`.
- **Project:** walk `Tree::source_nodes()` and `container_pairs()` into AAT
  blocks/inlines, preserving source spans. Project `Tree::pairs()` for
  ruby / bouten / bracket nesting. Project `Tree::diagnostics()` into
  AAT `meta` parse-status (settles the `parse_complete` signal).
- **Gaiji:** use the upstream resolver (`aozora::json::gaiji_at` /
  `Tree`-equivalent) and record the **upstream result** in AAT, including
  unresolved upstream results. Faithfulness policy matches the `aozora-rs`
  matrix row: record upstream behavior as emitted; do not patch
  oracle-correct gaiji results inline.
- **Output:** AAT JSON to stdout (schema-validated against
  `data/aat-schema.json` in tests). `--mode html` emits
  `Tree::to_html_with()` wrapped in the same AAT `meta` envelope used by
  the other adapters.

### 4.2 Why library seam, and the drift mitigation

The library seam is the same choice `aozora-rs` made, and `aozora-rs`
broke — but it broke because it depended on **`ab-ir` Rust internals as if
they were a contract**, not because it depended on its upstream parser's
AST. This adapter depends on:

- its **upstream parser's** AST (necessary for Direct fidelity; pinned to a
  rev, drift is bounded to mapper edits and never reaches the harness), and
- **no `ab-*` crate at all** (schema-only, the proven `aozora2html` path).

So the `aozora-rs` failure mode cannot recur here: the harness boundary is
AAT JSON, and `ab-ir` is not on the dependency graph.

### 4.3 Why not the subprocess seams (rejected)

- **`inspect` JSON alone:** span-only, no content/nesting — cannot produce a
  faithful AAT. Rejected.
- **Pandoc JSON alone:** drops container kinds (`aozora-container-unknown`
  for bold/heading blocks) and does not map Aozora headings to Pandoc
  Headers. Rejected as the sole surface.
- **Pandoc + inspect combined:** lossy cross-surface re-derivation of
  nesting; a braided mapper with two coordinate systems. Rejected as a
  complexity/faithfulness regression versus the library seam, which is
  cheap here (§2.1) and is the established Direct-adapter shape.

---

## 5. Fidelity matrix row (to add to `docs/adapter-fidelity.md`)

| Adapter | Faithfulness level | Upstream dependency | Entry point used | Preserved behavior | Known fidelity gaps / limits |
|---|---|---|---|---|---|
| `aozora` | Direct | `aozora` crate v0.4.1 (P4suta), pinned rev | `Document::new → Tree::source_nodes` / `pairs` / `container_pairs` / `diagnostics`; upstream gaiji resolver | Parser-normalized nodes, upstream gaiji resolution (incl. unresolved results), ruby/gaiji nesting, bouten metadata, container kinds, diagnostics → parse-status. | Characterization pending (§6 unknown U1). Aozora headings are not Pandoc `Header` upstream; adapter must emit AAT heading directly from `container_pairs` kind, not via Pandoc. |

---

## 6. Open unknowns (recorded, not blocking direction)

These do not change the seam decision (settled in §2). They must be
resolved during implementation, before the smoke / fidelity gates pass.

- **U1 — Feature coverage characterization.** Run the adapter against the
  TEI-EAJ all-work structural-probe workset once those Aozora counterparts
  are materialized, and against the P4suta parser fixture set already
  present under `references/parsers/aozora`. Confirm AAT axes present in the
  other adapters (ruby+nesting, warigaki, accent, figure, style scopes,
  jisage/keigakomi/yokogumi/caption, heading, chitsuki, jizume, burasage,
  scoped tcy, block bold/italic/font-size) are projected. Do not limit this
  characterization to a single title; the acceptance evidence is the
  TEI-EAJ all-work comparison plus full-corpus source/AAT measurement.
- **U2 — Gaiji coverage characterization.** Probe aozora's resolver
  against the gaiji fixture subset and record how many JIS-form gaiji
  remain unresolved upstream (the `aozora-rs` analog gap). This sets the
  "faithful adapter output may still fail oracle correctness" caveat for
  the matrix row.
- **U3 — `parse_complete` / failure mapping.** Confirm how `Tree::diagnostics`
  severities map to AAT `meta.parse_complete` / warnings, matching the
  `aozora-epub3` `[ERROR]`/`[WARN]` policy.
- **U4 — Pin rev.** Choose an exact `aozora` rev (commit SHA; there is no
  release tag at 0.4.1 on the cloned checkout as observed) for the
  `flake.nix` input and the `Cargo.toml` git dep. Must be verified
  reproducible offline.
- **U5 — TEI-EAJ counterpart source inputs.** The ABC workset export names
  TEI-EAJ files with candidate Aozora work IDs, but most local ABC
  counterparts are still missing. Before using this adapter for Level 3 TEI
  evidence, materialize the Aozora source inputs for the whole TEI-EAJ
  candidate workset under `references/aozorabunko` or document them as
  external inputs like the other adapters' AAT dirs.
- **U6 — Notation-spec vector harness.** Decide whether to vendor/pin
  `P4suta/aozora-notation-spec` as a flake input or a reference checkout,
  then add a runner that can compare the new adapter's projections against
  vector `nodes`, `pairs`, and `diagnostics`. Report `must` failures as
  adapter blockers; report `should`/`may` divergences as warnings with
  notes. This harness is independent from the AAT schema check and the
  parser-IR conversion audit.

---

## 7. Consequences

- A fifth `--aat-dir aozora=…` entry joins `justfile` cross-adapter
  comparison recipes and `reports/aat-fidelity/run-cross-adapter-report.sh`.
- `flake.nix` gains a `reference-aozora-p4suta-src` input + an
  `aozora*Adapter` package + a `aozora-smoke-check` runCommand, mirroring
  `aozora-epub3-smoke-check`.
- A pinned `aozora-notation-spec` reference joins the evaluation harness as
  comparison evidence. The first landing slice may be report-only; CI should
  gate only local fixture smoke until the pinning and licensing path is
  settled.
- `docs/adapter-fidelity.md` gains the row in §5.
- `crates/README.md` adapter-facing note is unaffected (no new `ab-*`
  adapter helper is introduced; the schema remains the contract).
- The `aozora-rs` broken-adapter maintenance notice is unchanged; this
  adapter does not revive it.

---

## 8. Rejected alternatives

- **Make `inspect` JSON the adapter boundary (subprocess, seam B).**
  Rejected: introspection envelopes are span-only and carry no content or
  nesting. A faithful Direct AAT cannot be built from them (§2.2).
- **Make Pandoc JSON the adapter boundary (subprocess).** Rejected: drops
  container-kind labels and does not map Aozora headings to Pandoc Headers
  (§2.2, §4.3). Would require a lossy combined-surface mapper.
- **Make `ab-ir` the adapter SDK for this adapter.** Rejected by the
  2026-07-03 boundary decision: `ab-ir` is not version-disciplined as an
  external contract; new adapter-facing tooling must consume AAT JSON.
- **Indirect tier (map `aozora render` HTML → AAT).** Rejected: duplicates
  the `aozora2html` Indirect tier, loses spans/diagnostics, adds no new
  measurement value over the existing four adapters.
- **Treat `aozora-notation-spec` as the sole authority.** Rejected: it is
  explicitly unofficial. Its vectors are valuable independent evidence, but
  source-inventory measurement and AAT/parser-IR conversion evidence remain
  the gates for this system's representability and TEI claims.
- **Premature shared AAT-builder across the now-5 mappers.** Deferred to
  `deepening-review` after this adapter exists; proposing it now would
  create a one-implementation seam.

---

## 9. Incubation note

This spec is provisional per `hammock-driven-design`. The seam decision is
firm (decisive probe evidence in §2); the recorded unknowns U1–U6 are
implementation-time characterizations, not direction reversals. Before
implementation, `rich-hickey-review` should run on this spec for
provenance/dep-pinning/contract-leak; after the adapter exists,
`deepening-review` should run on the 5-mapper set for a possible shared
AAT-builder helper.
