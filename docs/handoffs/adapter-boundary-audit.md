# Adapter ↔ Workspace Boundary Audit

Evidence audit (read-only) of the four parser adapters' coupling to `ab-*`
workspace crates, and whether the AAT JSON schema or the `ab-ir` Rust types
constitute the de-facto adapter contract. Cites `file:line`.

Investigation date: 2026-07-02. Scope: `adapters/{aozora-rs,aozora2,aozora2html,
test-adapter}` + `crates/ab-ir` + `crates/ab-source-syntax`. Method: `grep` for
`use ab_*::` and fully-qualified `ab_*::` call sites across `src/`, `benches/`,
`tests/`; cross-reference against `Cargo.toml` path deps and the `ab-ir`
public API surface; read `aat.rs`/`main.rs`/`lib.rs` to classify each symbol.

Note: all three Rust adapters are `exclude`d from the root workspace
(`Cargo.toml:11-16`) and are standalone packages pinned to this checkout via
path deps (`../../crates/...`). `test-adapter` is a bash script embedding
Python, not a Rust crate (`adapters/test-adapter/test-adapter:1`).

A predecessor handoff (`docs/handoffs/crate-classification.md:58-71,79-90`)
already flagged §3 of this same boundary. This report is the focused
evidence audit that task asked for; it confirms and tightens those findings
with per-symbol classification and the `aozora2html`/`test-adapter`
JSON-only proofs.

---

## 1. Per-adapter `ab-*` dependency inventory + symbol classification

Classification key:
- **DATA TYPE = AAT-JSON-representable**: the symbol has a direct JSON
  serialization in `data/aat-schema.json` / `ab_ir::blocks_to_aat_projection`
  (`crates/ab-ir/src/lib.rs:380-564`). Renaming or reshaping it changes
  emitted JSON.
- **INTERNAL helper**: parser-construction plumbing with no independent JSON
  node; only its *effect* is observable in JSON (e.g. constructors,
  traversers, projection, provenance counters, mutable accessors).
- **PARSER-PLUMBING (not AAT)**: source-text event model from
  `ab-source-syntax`; feeds adapter logic but never itself serialized into AAT.

### 1.1 `aozora-rs` (heaviest consumer)

Path deps — `adapters/aozora-rs/Cargo.toml:6-7`:
```toml
ab-ir = { path = "../../crates/ab-ir" }
ab-source-syntax = { path = "../../crates/ab-source-syntax" }
```

`use ab_ir::{...}` — `adapters/aozora-rs/src/aat.rs:3-9` (single import cluster):
```nix
use ab_ir::{
    Block, GaijiKind, GaijiRef, Inline, ProjectedText, Provenance, RubyPlacement, StyleAttr,
    StyleAttrValue,
};
```
| Imported symbol | Classification | Evidence (definition → JSON emit) |
| --- | --- | --- |
| `Block` | DATA TYPE | enum `crates/ab-ir/src/lib.rs:5-37`; serialized in `blocks_to_aat_projection` `lib.rs:398-468` (`paragraph`/`heading`/`jisage_block`/`caption_block`/`warichu`/`figure`/`break` kinds). Constructor calls all over `aat.rs` (e.g. `Block::Paragraph`, `Block::Heading`, `Block::page_break()` `aat.rs:449,439,453`). |
| `Inline` | DATA TYPE | enum `crates/ab-ir/src/lib.rs:39-90`; every variant serialized in `inline_node_to_aat_json_without_warnings` `lib.rs:566-672`. Built in `aat.rs` via ~12 constructors (`Inline::text_with_provenance`, `ruby_with_attrs`, `style_with_attrs`, `scope`, `font_size`, `warigaki`, `gaiji_with_provenance`, `Raw`, `EditorNote`, `FigureRef` … call sites `aat.rs:102,135,162,178,192,432,454,471,...`). |
| `GaijiRef` | DATA TYPE | struct `crates/ab-ir/src/lib.rs:247`; serialized by `gaiji_to_aat_json` `lib.rs:749-769` → JSON `{kind:"gaiji", description, resolved, jis_code, unresolved_reason, x-description-format}`. Built in `aat.rs` via `Inline::gaiji_with_provenance`. |
| `GaijiKind` | DATA TYPE (drives `jis_code`) | enum `crates/ab-ir/src/lib.rs:281-307`; consumed by `gaiji_jis_code` `lib.rs:771-783` → JSON `jis_code` string shape. Add a variant and the `jis_code` field changes shape. |
| `RubyPlacement` | DATA TYPE | enum `crates/ab-ir/src/lib.rs:349`; serialized to JSON `"direction"` (`placement.as_str()` `"right"/"left"`) in `lib.rs:587,638`. |
| `StyleAttr` / `StyleAttrValue` | DATA TYPE (→ `x-*` attrs) | `crates/ab-ir/src/lib.rs:230-244`; serialized by `style_json_with_attrs` `lib.rs:674-683` into top-level JSON keys (`x-indent`, `x-break-kind`, `x-annotation-type`, …). `aat.rs` builds many (`attr_text`/`attr_string`/`attr_int` `aat.rs:1033-1051`). |
| `Provenance` | DATA TYPE (→ `x-provenance`) | enum `crates/ab-ir/src/lib.rs:345`; serialized by `with_provenance` `lib.rs:885-890` → JSON `x-provenance` field when `!= Parser`. Passed to every constructor in `aat.rs`. |
| `ProjectedText` | INTERNAL helper | `crates/ab-ir/src/lib.rs:339`; *return type* of `visible_projection` `lib.rs:488`. Used as the projection-check value `aat.rs:119,124,137`. Not itself serialized into AAT JSON — only its `.visible_text` string is compared. |

Quantitatively: ~124 distinct `ab_ir::` call sites in `aat.rs` (grep output);
8 of the 8 imported symbols appear in the production builder paths, not just
`#[cfg(test)]`.

`ab_ir::` fully-qualified call sites outside `aat.rs` (production code in
`lib.rs`/`metrics.rs`):
- `ab_ir::provenance_counts(&result.blocks)` — `adapters/aozora-rs/src/lib.rs:48,93,400,451,506` and `adapters/aozora-rs/src/metrics.rs:79`. INTERNAL helper (counts for `meta.metrics`); its output *shape* is documented JSON though (`provenance_counts` → JSON keys `parser_nodes`/`parser_normalized_nodes`/`source_supplement_nodes`/`source_fallback_nodes` in `metrics.rs`).
- `ab_ir::blocks_to_aat_projection(&result.blocks)` — `adapters/aozora-rs/src/lib.rs:110`. **This is the serialization call** —INTERNAL-but-contract-bearing: its implementation is the AAT JSON projection.
- `ab_ir::semantic_summary(&result.blocks, &projection.warnings)` — `adapters/aozora-rs/src/lib.rs:111`. INTERNAL helper producing `meta.semantic_summary`.
- `ab_ir::visible_projection(&blocks)` — `adapters/aozora-rs/src/aat.rs:119,124,137`. INTERNAL helper (projection check), feeds fallback decision; not serialized directly.
- `ab_ir::block_content_mut(block)` / `ab_ir::block_content(block)` — `adapters/aozora-rs/src/aat.rs:464,642,753,2006,2199,2240`. INTERNAL traversal mutator.
- `Vec<ab_ir::Block>` return type — `adapters/aozora-rs/src/lib.rs:146`.

`ab_source_syntax::` usage (PARSER-PLUMBING, never serialized into AAT itself):
- `use ab_source_syntax::SourceEvent;` — `adapters/aozora-rs/src/lib.rs:3`.
- `use ab_source_syntax::{self, SourceAnnotationsBoth, SourceEvent};` — `adapters/aozora-rs/src/source.rs:6`; events/annotations sourced at `source.rs:77-79,142`.
- ~50 fully-qualified `ab_source_syntax::` call sites in `aat.rs` (e.g. `source_events`, `source_annotations_both_from_events`, `comparison_lossy_body`, `SourceEventKind::Gaiji/Ruby/Text/EditorialNote/Command/SegmentBoundary`, `EditorialNoteKind::BottomTextCorrection`, `SourceAnnotations`, `SourceAnnotationsBoth`, `LocatedMarker`) — all PARSER-PLUMBING: they tokenize raw Aozora source into events the adapter then maps into `ab_ir::Block`. None of these types appear in the emitted AAT JSON.

### 1.2 `aozora2`

Path dep — `adapters/aozora2/Cargo.toml:7`:
```toml
ab-source-syntax = { path = "../../crates/ab-source-syntax" }
```
`use ab_source_syntax as source_syntax;` — `adapters/aozora2/src/lib.rs:2`
**but gated `#[cfg(test)]`** — `adapters/aozora2/src/lib.rs:1`. The only
production use is the test helper `source_syntax::comparison_lossy_body(text)`
at `adapters/aozora2/src/lib.rs:1521` (inside `#[cfg(test)] mod tests` … the
`#[cfg(test)] fn source_visible_text` at `lib.rs:1519-1523`).

So in **non-test builds aozora2 has zero `ab-*` dep usage** despite declaring the
Cargo path dep. It emits AAT JSON directly via `serde_json::json!` (see §3).
Upstream parser: `aozora-core` 0.7.1 (`adapters/aozora2/Cargo.toml:6`).

No `ab-ir` dep, no `ab-plaintext` dep. Confirmed by grep: zero
`use ab_` / `ab_ir::` / `ab_source_syntax::` matches in `adapters/aozora2/src/`
outside the `#[cfg(test)]` alias.

### 1.3 `aozora2html`

Path deps — `adapters/aozora2html/Cargo.toml:6-13`: **no `ab-*` crate at all**.
Dependencies are `anyhow`, `clap`, `encoding_rs`, `roxmltree`, `regex`,
`serde`, `serde_json`, `sha2`. Upstream: Ruby gem `aozora2html` 3.0.1
(`adapters/aozora2html/src/model.rs:5`).

Grep for `use ab_` / `ab_ir::` / `ab_source_syntax::` / `ab_plaintext::` over
`adapters/aozora2html/src/` and `tests/`: **zero matches**. Emits AAT JSON via
`serde_json::{json, Value}` (`adapters/aozora2html/src/lib.rs:2,79,88,124,152`;
`model.rs:1`; `xhtml_mapper.rs:4`; `source_derived.rs:4`).

This is the cleanest proof that **the JSON schema alone is sufficient** to build
a conformant adapter (§3).

### 1.4 `test-adapter`

`adapters/test-adapter/test-adapter` is a bash script (no Cargo, no Rust).
Its `--mode aat` branch heredocs a Python program (`adapters/test-adapter/
test-adapter:18-86`) that hand-builds the AAT document with `json.dump`
(`test-adapter:81-95`): top-level `version=1`, `work_id`, `blocks[0]` a
`paragraph`, `meta` with `adapter`/`adapter_version`/`source_encoding`/
`source_hash`/`parse_complete`/`warnings`. No `ab-*` crate (impossible — it is
not Rust). Grep `use ab_`/`ab_ir`/`ab_source` over `adapters/test-adapter/`:
zero matches. **Second proof (§3) that JSON-only is sufficient.**

### 1.5 Summary table

| Adapter | `ab-ir` Rust dep | `ab-source-syntax` dep | Other `ab-*` | JSON emit mechanism |
| --- | --- | --- | --- | --- |
| `aozora-rs` | yes (`Cargo.toml:6`) | yes (`Cargo.toml:7`), prod | none | builds `Vec<ab_ir::Block>` → `ab_ir::blocks_to_aat_projection` (`lib.rs:110`) |
| `aozora2` | no | declared (`Cargo.toml:7`) but **`#[cfg(test)]` only** (`lib.rs:1`) | none | `serde_json::json!` directly (`lib.rs:35-46`) |
| `aozora2html` | no | no | none | `serde_json::json!` directly (`lib.rs:71-91`) |
| `test-adapter` | n/a (bash) | n/a | n/a | Python `json.dump` (`test-adapter:81-95`) |

---

## 2. `aozora-rs` data flow — is `ab-ir` the serializer or just intermediate parsing?

**`ab-ir` is both**: the adapter uses `ab-ir` types as intermediate parsing
scaffolding AND `ab-ir` performs the final JSON serialization. The adapter
itself never hand-builds JSON for `blocks`.

Data flow (production path, `adapters/aozora-rs/src/lib.rs:21-41`):
1. `aat_json_from_bytes` decodes bytes (`lib.rs:23`), parses with `aozora-rs-core`
   (`lib.rs:31`), and calls `build_aat_result` (`lib.rs:32`) which constructs a
   `Vec<ab_ir::Block>` via `aat::retokenized_to_aat_blocks` /
   `build_initial_without_source_annotations` / `build_fallback_*` — all of which
   build `ab_ir::Block`/`Inline` value-trees (`aat.rs:115-448`).
2. `build_aat` (`lib.rs:104-128`) then calls
   `ab_ir::blocks_to_aat_projection(&result.blocks)` (`lib.rs:110`) — this is the
   serialization step; it lives inside `ab-ir` (`crates/ab-ir/src/lib.rs:476-525`,
   which itself calls `json!({...})` from `ab_ir::lib.rs:1`). The returned
   `serde_json::Value` blocks are placed into the top-level document.
3. The remaining top-level envelope (`version`, `work_id`, `meta`) *is*
   hand-built by the adapter with `serde_json::json!` (`lib.rs:113-127`), and the
   final `serde_json::to_writer` writes it out (`lib.rs:38-40`).

So: `ab-ir` = intermediate parse representation + serializer for `blocks`;
adapter = envelope builder. The adapter does **not** invoke `serde_json::json!`
to construct block/inline nodes the way `aozora2`/`aozora2html` do; it relies on
`ab-ir` to do that projection. This is exactly why renaming `Block::Jisage` or
adding an `Inline` variant silently changes the emitted JSON with no schema
review — the JSON shape is a derived consequence of the Rust enum, not a
declared contract the adapter chose to emit.

`projection.rs` is unrelated to this boundary: it contains a *comparison*
helper (`ProjectionSummary`, `check_with_source_visible`) that compares
source-visible vs projected-visible text lengths/order — it imports no `ab-*`
crate and produces no JSON.

---

## 3. Adapters emitting AAT JSON WITHOUT an `ab-*` dep — yes

Three of four adapters prove the JSON schema is sufficient on its own:

- **`aozora2html`** — zero `ab-*` dependency in `Cargo.toml`
  (`adapters/aozora2html/Cargo.toml:6-13`); zero `use ab_` in source. Builds the
  whole document, including `blocks`, by constructing `serde_json::Value`
  objects with `serde_json::json!`
  (`adapters/aozora2html/src/lib.rs:71-95`, `model.rs:67-92`, `xhtml_mapper.rs`,
  `source_derived.rs`). Emits `"version": 1` at `lib.rs:72`.
- **`aozora2`** — has an `ab-source-syntax` Cargo path dep
  (`adapters/aozora2/Cargo.toml:7`) but it is **only reached from
  `#[cfg(test)]` code** (`adapters/aozora2/src/lib.rs:1-2,1519-1523`). In
  production builds it is `ab-*`-free and emits the full AAT document via
  `serde_json::json!` (`adapters/aozora2/src/lib.rs:35-46`), with block/inline
  builders like `gaiji_json`, `ruby_json`, `style_json`, `raw_json`
  (`lib.rs:1110,1247,1273,1299`) all returning `serde_json::Value`.
- **`test-adapter`** — not Rust at all; Python `json.dump` builds the document
  (`adapters/test-adapter/test-adapter:81-95`). No `ab-*` involvement possible.

`aozora-rs` is the lone outlier that uses `ab-ir` as a typed builder +
serializer. Whatever typing benefit `ab-ir` provides is *not required* to
produce schema-valid AAT — two production adapters (`aozora2`, `aozora2html`)
and the fixture adapter (`test-adapter`) achieve compliance without it.

**Conclusion:** the JSON schema (`data/aat-schema.json`, JSON Schema
draft 2020-12, `$id https://abc.local/schemas/aat-v1.json`,
`"version": {"const": 1}`) plus `docs/aat-contract.md` semantics
(`data/aat-schema.json` is "the normative validation schema for AAT v1
document shape" per `docs/aat-contract.md:13-15`) are sufficient on their own.
`ab-ir` is a *convenience* typed builder, not the contract.

---

## 4. `ab-ir` versioning, ChangeLog, semver, public-API discipline

**None exists.** Evidence:

- **Version**: `crates/ab-ir/Cargo.toml:3` → `version.workspace = true`, which
  resolves to `[workspace.package] version = "0.1.0"` in the root
  `Cargo.toml:25`. That is a Cargo package version, not an API contract
  version, and it is shared across every workspace member (so bumping it
  claims an ABI change for crates that did not change).
- **No ChangeLog**: `find crates/ab-ir -iname 'CHANGELOG*'` and `-iname '*.md'`
  → no results. The crate has no `CHANGELOG.md`, no `RELEASES.md`, no crate
  README other than the repo `../../README.md` (`Cargo.toml:7`).
- **No stability markers**: grep `crates/ab-ir/src/` for `#[doc(hidden)]`,
  `#[stable]`, `#[unstable]`, `since =`, `rustversion`, `semver` → zero
  matches. Every item is unconditionally `pub` at crate root.
- **No public-API gating**: `crates/ab-ir/src/lib.rs:1-2` opens with
  `pub mod aat_view;` and re-exports `semantic_summary::{SemanticSummary,
  SemanticSummaryNode, SourceSpan, semantic_summary}` (`lib.rs:3`). The whole
  crate surface — `Block`, `Inline`, `GaijiRef`, `GaijiKind`, `Provenance`,
  `RubyPlacement`, `StyleAttr`, `StyleAttrValue`, `ProjectedText`,
  `ProvenanceCounts`, `AatProjection`, `ProjectionWarning`, `InlineVisitor`,
  `walk_inline`, plus ~60 inherent constructors on `Inline`/`Block`, plus
  `blocks_to_aat_json`/`blocks_to_aat_projection`/`visible_projection`/
  `provenance_counts`/`block_content`/`block_content_mut` (`lib.rs:117,326`,
  `470,475,488,834,931,940`) — is public with no `#[doc(hidden)]`, no
  `#[non_exhaustive]`, no feature flags.

**Would breaking `ab-ir` break adapters?**

- *Today, inside this repo*: only **partial** blast radius. `aozora-rs` is the
  sole adapter that imports `ab-ir` (`adapters/aozora-rs/src/aat.rs:3`), so a
  breaking change to `Block`/`Inline`/`GaijiRef`/etc. breaks only `aozora-rs`
  at compile time. `aozora2`, `aozora2html`, `test-adapter` keep compiling
  because they do not depend on `ab-ir` (§1). All four are path-dep pinned to
  this checkout (`../../crates/ab-ir`) and excluded from the workspace
  (`Cargo.toml:11-16`), so they rebuild in lockstep — a breakage is caught
  *immediately* by CI, not deferred.
- *The real contract leak*: the *JSON shape* `ab-ir` emits is the contract.
  `crates/ab-ir/src/lib.rs:380-564` (`blocks_to_aat_projection`) hand-builds
  JSON via `serde_json::json!` from the same `use serde_json::json;` import
  (`lib.rs:1`) that `aozora2`/`aozora2html` use directly. Change a variant in
  `Block`/`Inline` and `blocks_to_aat_projection` silently changes the emitted
  JSON — *bypassing* the schema review that `data/aat-schema.json` is meant to
  gate. The schema file does not auto-derive from the Rust types; there is no
  test asserting "schema ↔ `blocks_to_aat_projection` output" equivalence
  found in `ab-ir`'s tests.
- *The moment an adapter is published or built outside this workspace* against
  a `crates.io`/git `ab-ir` tag, the Rust API (`Block`/`Inline`/`GaijiRef`/…)
  becomes an **unversioned external contract** with no semver discipline.
  `aozora-rs` would break on a minor `ab-ir` bump with no signal, while two
  sister adapters emit identical JSON untouched.

So: `ab-ir` has **no version field that means anything, no ChangeLog, no semver,
no public-API marker**. Breaking its Rust API breaks at minimum `aozora-rs`;
breaking its emitted JSON shape breaks all four adapters' compliance — but
*nobody is required to notice*, because the JSON shape is a derived
consequence of unannotated `pub` enums rather than a declared, versioned
contract.

---

## 5. Rich-Hickey-style Protocol-Boundary finding

> **Observation:** Two contracts are braided under one name. The *documented*
> adapter↔workspace boundary is the AAT JSON schema
> (`data/aat-schema.json` + `docs/aat-contract.md`, with explicit `version = 1`
> and "required-field changes require AAT v2" rules at `aat-contract.md:18-27`).
> But `aozora-rs` — the reference adapter — does not consume that contract; it
> consumes `ab-ir` Rust types (`adapters/aozora-rs/src/aat.rs:3`) and lets
> `ab-ir::blocks_to_aat_projection` (`crates/ab-ir/src/lib.rs:475`) synthesize
> the JSON from unversioned, un-gated `pub` enums. The wire contract and the
> typed contract are thus **the same artifact described twice**, with neither
> description deriving from the other, and with **no versioning on the Rust
> half**. Three of four adapters (`aozora2`, `aozora2html`, `test-adapter`)
> demonstrably honor the JSON contract with zero `ab-ir` dependency (§1, §3),
> so the Rust-typed path is provably *optional* — yet for `aozora-rs` it is
> the binding one.

> **Risk:** A maintainer reshapes `Block`/`Inline`/`GaijiRef` (rename a variant,
> add a field, change `StyleAttrValue` representation) believing they are editing
> an internal helper, and silently reshapes the emitted AAT JSON that all four
> adapters are contractually required to produce — with no schema-validation
> step bridging the two, no `#[non_exhaustive]` warning, no crate-level version
> bump, and no ChangeLog entry. The schema file (`data/aat-schema.json`) cannot
> catch this because nothing regenerates or diff-checks it against
> `ab-ir`'s `json!` output. Conversely, a JSON-schema change (e.g. AAT v2) is
> *invisible* to `aozora-rs` until someone hand-updates `ab-ir` — the Rust
> types and the schema can silently drift. The boundary is also *implicit* in
> trust terms: `ab-ir` is workspace-internal code path-depped into adapters that
> are `exclude`d from the workspace (`Cargo.toml:11-16`), so "internal" and
> "adapter-facing" are the same code today but will diverge the moment an
> adapter is published or pinned at a distance. Time/identity: there is no
> record of which `ab-ir` Rust-API shape produced a given emitted JSON, because
> the JSON shape has no provenance field tying it to the serializing Rust
> version — `adapter_version` in `meta` (`adapters/aozora-rs/src/lib.rs:115`,
> VALUE at `lib.rs:18`) records the *upstream* parser, not the `ab-ir`
> projection version.

> **Alternative A — Make the JSON schema the sole contract; demote `ab-ir` to
> an optional, internal helper.** Forbid adapters from depending on `ab-ir` for
> emit; require every adapter to emit JSON against `data/aat-schema.json`
> (the pattern `aozora2`/`aozora2html`/`test-adapter` already follow). Keep
> `ab-ir` as an *in-repo, not adapter-facing* library used only by workspace
> consumers (`ab-oracle`'s `AatDocument` selector at
> `crates/ab-ir/src/aat_view.rs`). Add a CI test that schema-validates actual
> `blocks_to_aat_projection` output, so the helper cannot silently drift from
> the schema.
> **Tradeoff:** `aozora-rs` loses typed builders and must hand-write JSON like
> `aozora2` does — more boilerplate, more risk of typos the schema *will*
> catch. Removes a real ergonomic win for the heaviest adapter. Forces the
> schema to be genuinely complete (it currently permits `x-*` extension
> fields freely, `aat-contract.md:7-11`).
>
> **Alternative B — Promote `ab-ir` Rust types to a real, versioned contract.**
> Add `#[non_exhaustive]` to `Block`/`Inline`/`GaijiRef`/`GaijiKind`/
> `StyleAttr`/`StyleAttrValue`; introduce an independent crate version
> (not `version.workspace = true`); add `CHANGELOG.md` with semver policy; add
> `#[doc(hidden)]` or a `pub(crate)` private surface for purely-internal
> helpers like `provenance_counts`/`block_content_mut`; add a
> schema-derivation or schema-equivalence test so a Rust change that alters
> emitted JSON is a *breaking* change requiring a major bump. Document the
> `ab-ir` API as the adapter contract in `ab-ir` crate docs and re-point
> `docs/aat-contract.md` to "the JSON shape `ab-ir` vMAJOR.minor is
> guaranteed to emit."
> **Tradeoff:** substantial process overhead for a crate consumed by exactly
> one adapter today; `#[non_exhaustive]` adds `..` exhaustiveness friction
> to every `match` in `ab-ir` itself and in `ab-oracle`'s selector; semver
> discipline on a 0.1.0 internal crate may be theater until a second external
> consumer appears. Risks codifying a typed contract that the JSON was
> specifically designed to be free of.
>
> **Alternative C — Derive the schema from the types (or the types from the
> schema), making the two contracts one.** Generate `data/aat-schema.json`
> from `ab-ir`'s `blocks_to_aat_projection` by snapshotting its output
> (cherly/insta-style golden schema), or generate `serde`-typed structs from
> the JSON schema (`typify`/`schemars`) and have `ab-ir` serialize through
> `#[derive(Serialize)]` instead of hand-rolled `json!` (`crates/ab-ir/src/
> lib.rs:380-672`). The current hand-rolled `json!` projection
> (`lib.rs:476-525`, `566-672`, `749-783`) is exactly the seam that lets the
> two descriptions diverge — collapsing it removes the divergence surface.
> **Tradeoff:** `#[derive(Serialize)]` would impose a single canonical JSON
> shape per Rust variant, removing the handful of bespoke projections (e.g.
> `Block::Figure` → `paragraph` with `x-figure`, `lib.rs:447-458`;
> `Block::Warichu` → `paragraph` with `x-warichu`, `lib.rs:437-446`; the
> `with_provenance` extension-injection, `lib.rs:885-890`) that currently
> let ab-ir paper over schema gaps. Schema-first generation re-introduces a
> codegen step and a dependency on a derive crate. Highest up-front cost,
> lowest long-term drift cost.

> **Severity: Follow-up (Strong suggestion if a second adapter or external
> consumer of `ab-ir` is imminent).** Today the vendored, path-pinned,
> workspace-`exclude`d layout means a breakage is caught locally and
> immediately, so this is not a present-tense correctness blocker. It becomes
> **Blocker-strong** the moment `ab-ir` is published or any adapter is built
> outside this checkout, because the de-facto Rust-typed contract then has no
> versioning safety net at all. Per `rich-hickey-review`: this is a
> **Protocols** lens finding (wire/schema contract lacking versioning rules for
> the Rust-typed path) best routed to `hammock-driven-design` before any
> refactor; **no final option is selected here** — the three alternatives above
> are presented for that follow-up design conversation.
