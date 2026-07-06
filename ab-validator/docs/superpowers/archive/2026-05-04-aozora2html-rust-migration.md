# Aozora2HTML Rust Migration Implementation Plan (Revised)

> ARCHIVED: 2026-05-05 — Migration completed and Rust implementation in `adapters/aozora2html` is now active; this document is kept for historical reference only.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the Python mapping stage in `adapters/aozora2html` with a Rust implementation while preserving existing behavior of the Ruby parser wrapper and existing fixture/parity expectations.

**Architecture:** Keep Ruby parsing and input-normalization (encoding/CRLF/synthetic document) in `adapters/aozora2html/aozora2html-adapter` unchanged. Introduce a Rust crate that performs mapping in two explicit stages:

- `xhtml_mapper.rs`: deterministic XHTML tree walk -> canonical AAT blocks + warnings.
- `source_derived.rs`: deterministic source-text reconciliation/enrichment -> enriched blocks + semantic_summary.

A third orchestrator layer `lib.rs` composes them; it does not flatten them into one ad hoc function. A final serializer builds existing legacy output JSON shape.

**Tech Stack:** Rust 2024, `roxmltree` (fixed), `clap`, `serde_json`, `ab-source-syntax`, `regex`, `sha2`, `once_cell`, optional `fancy-regex` only if required by regex audit, existing shell wrapper in Bash, `jsonschema` checks in existing Python tests.

---

### Task 1: Lock behavior contract in executable tests (no behavior changes)

**Files:**
- Modify: `adapters/aozora2html/tests/test_mapper.py`

- [ ] **Step 1: Add migration contract tests as code assertions (not prose)**

Add tests that assert current adapter contract before any Rust code exists:

- `--mode`, `--version`, parser-failed envelope shape, and baseline envelope fields.
- `meta.adapter == "aozora2html"`, `meta.adapter_version` format, `source_encoding`, `source_hash`, `parse_complete`, warnings.
- Existing `x-` extension fields that are used by fidelity tooling are preserved in output (`x-provenance`, `x-caption-provenance`, etc.).

- [ ] **Step 2: Add Rust baseline helper but gate parity tests by env var to avoid red until implementation is present**

In `test_mapper.py`, add helper functions:

- `run_rust_mapper(input: bytes, backend_args)` -> bytes.
- `run_python_baseline(input: bytes)` -> bytes.
- `canonicalize(aat_json)` to normalize non-semantic drift (as currently done by existing tests).

Guard comparison tests with explicit opt-in:

```python
PARITY_WITH_RUST = os.environ.get("AOZORA2HTML_PARITY", "0") == "1"
```

and `pytest.skip` when not enabled.

- [ ] **Step 3: Add fixture-level differential checks (enabled only under `AOZORA2HTML_PARITY=1`)**

For each `fixtures/*.txt`:
- run Ruby wrapper in Python path to produce baseline xhtml and AAT baseline,
- run Rust mapper path from the same wrapper output,
- canonicalize both and assert `meta.parse_complete`, `meta.source_*`, `blocks`, `meta.warnings`, and `meta.semantic_summary.syntax` equality.

Expected before Rust implementation: these tests are skipped unless `AOZORA2HTML_PARITY=1` and toolchain artifact exists.

### Task 1.5: Pre-lock checks before Task 2 implementation (blocking)

**Files:**
- Modify: `docs/superpowers/plans/2026-05-04-aozora2html-rust-migration.md`
- Modify: `adapters/aozora2html/src/model.rs` (scaffold only)
- Modify: `adapters/aozora2html/Cargo.toml` (scaffold dependencies)

- [ ] **Step 1: Rust regex compatibility audit (required before coding `source_derived.rs`)**

Create a migration table in this plan for every regex-driven helper in `adapter.py` used by `source_derived.rs` (`source_derived_block_scope`, `source_derived_inline_content`, `source_derived_split_gaiji_notes`, `source_derived_inlined_gaiji_content`, `source_derived_gaiji_content`, `source_derived_ruby_and_reference_content`, caption helpers):
- source Python `re` pattern
- Rust equivalent (`regex`, `fancy_regex`, or explicit parser refactor)
- support equivalence risk
- required approximation strategy

If any pattern is unsupported by either Rust regex crate, explicitly list it and defer implementation until mitigation is chosen.
If the audit shows no unsupported constructs, `fancy-regex` is not added as a dependency.

- [ ] **Step 2: Lock `model.rs` types before implementation**

Define shared contracts first:
- `AtBlock`, `AtInline`
- `SourceDerivedContext`, `SourceDerivedSummary`
- `MappingInput`, `MappingResult`
- `MappingError` + `MappingErrorKind`
- parser-failed serialization helpers

Run `cargo check --manifest-path adapters/aozora2html/Cargo.toml` on this skeleton and treat failures as blocking before starting Task 2a/2b.

Treat unresolved `ab-source-syntax` crate/path availability as a hard block: verify crate path and exported symbol (`decode_source_bytes`) before marking Task 1.5 complete.

### Task 2: Add Rust crate scaffold and explicit module boundaries

**Files:**
- Create: `adapters/aozora2html/Cargo.toml`
- Create: `adapters/aozora2html/src/main.rs`
- Create: `adapters/aozora2html/src/lib.rs`
- Create: `adapters/aozora2html/src/model.rs`
- Create: `adapters/aozora2html/src/xhtml_mapper.rs`
- Create: `adapters/aozora2html/src/source_derived.rs`
- Create: `adapters/aozora2html/src/jis2ucs.rs`
- Create: `adapters/aozora2html/build.rs` (if `phf` map generation is enabled)
- Modify: `adapters/aozora2html/README.md`

- [ ] **Step 1: Define migration API boundaries explicitly**

`lib.rs` exposes:

```rust
pub struct MappingInput {
    pub xhtml: Vec<u8>,
    pub source_bytes: Vec<u8>,
    pub parse_complete: bool,
}

pub struct MappingResult {
    pub blocks: Vec<serde_json::Value>,
    pub warnings: Vec<serde_json::Value>,
    pub semantic_summary: Option<serde_json::Value>,
}

pub fn map_with_protocol_bytes(
    xhtml: &[u8],
    source_bytes: &[u8],
    parse_complete: bool,
) -> Result<serde_json::Value, MappingError>;

pub fn map_with_protocol(input: MappingInput) -> Result<serde_json::Value, MappingError>;
```

`map_with_protocol_bytes` is the hot-path API for no-copy library callers and should be the preferred entry point.

`main.rs` keeps the wrapper protocol (`--source`, `--xhtml`, etc.) and should read those files as raw bytes (never decode in main), then pass borrowed bytes into
`map_with_protocol_bytes`.

`map_with_protocol` returns `Result<serde_json::Value, MappingError>` so callers can convert failures into the existing parser-failed envelope shape (`meta.error`, `meta.warnings`, and compatibility fields).

`map_with_protocol` composes:

1) `xhtml_mapper::from_xhtml(&xhtml)` -> base blocks + warnings.
2) `source_derived::apply_source_recovery(base, &decoded.text, &summary)` -> enriched blocks + summary.
3) envelope serialization.

- [ ] **Step 2: Define source-text ownership**

`map_with_protocol` accepts `source_bytes`, decodes and hashes source text internally by calling `ab-source-syntax::decode_source_bytes` so `source_encoding` and `source_hash` remain source-internal and deterministic.

Add a scaffold `Cargo.toml` check that resolves `ab-source-syntax` from the repository path and fails fast in CI if the crate or feature path changes.

- [ ] **Step 3: Choose and document XML parser choice now (roxmltree only)**

Use `roxmltree` in `xhtml_mapper.rs` only. It is DOM-based and closest to Python `lxml` usage (`find_all`, child walk, itertext-like behavior). Do not use `quick-xml` to avoid architecture drift.

`Cargo.toml` dependencies include `roxmltree` and no competing XML crate.

- [ ] **Step 4: Define `model.rs` types before implementation starts**

`model.rs` defines all intermediate Rust types shared by both passes:

- `AtBlock`, `AtInline`
- `SourceDerivedContext` (warning + semantic summary accumulator)
- `SourceDerivedSummary`
- small helper value objects used by summary emission (projection/warning records)

Each stage must consume/produce explicit typed structures from this module to avoid signature drift across tasks.

`model.rs` also defines the serialized error shape used for parser-failed envelopes (`MappingError`, `AdapterFailureEnvelope`) so both CLI and direct callers get deterministic envelopes.

### Task 2a: Implement source-agnostic XHTML mapper (`xhtml_mapper.rs`)

**Files:**
- Modify: `adapters/aozora2html/src/xhtml_mapper.rs`
- Modify: `adapters/aozora2html/src/model.rs`

- [ ] **Step 1: Port mechanical structural walk**

Implement mapping for:

- `find_main_text`
- `<div class="main_text">` discovery
- block splitting (`<br/>`) and paragraph assembly
- block elements: `h1/h2/h3`, `div` (`jisage`, `midashi`), `p`
- inline elements: text nodes, `ruby`, `img`, `span`, `em`, `sub`, `br`, `hr`

- [ ] **Step 2: Add parser-only decorations and warning behavior**

Map class-based decorations to AAT-adjacent JSON using existing Python semantics:

- `futoji`, `shatai`, `white_sesame_dot`, `sesame_dot_after`, `underline_double`, `daiN`, `shoN`, `keigakomi`.
- unmapped elements produce parser warnings and visible fallback text as in current Python.

- [ ] **Step 3: Add crate tests for XHTML parser stage only**

Add unit tests under `adapters/aozora2html/src/lib.rs` (or `tests/`) for:

- minimal blocks/ruby/span/div flows,
- warning emission on unmapped tags,
- gaiji image fallback to `gaiji` or `figure` per existing behavior.

Track a dedicated parser-only fixture list for this stage (e.g. `xhtml_mapper_suite`) and run stage-only assertions only on those fixtures.
Acceptance: stage can produce identical `blocks/warnings` for that parser-only fixture list.

### Task 2b: Implement high-risk source-derived recovery (`source_derived.rs`) as separate pass

**Files:**
- Modify: `adapters/aozora2html/src/source_derived.rs`
- Modify: `adapters/aozora2html/src/model.rs`
- Modify: `adapters/aozora2html/src/lib.rs`

- [ ] **Step 1: Source-derived block-level recovery pass (scaffold first)**

Port in the same order as Python:

- `source_derived_block_scope`
- indentation/line-break/caption wrappers from full-line patterns.

Add tests for one fixture per case:
- whole-work scope blocks,
- caption block,
- tcy/yokogumi block,
- page/line break patterns.

- [ ] **Step 2: Source-derived inline recovery pass (regex-heavy)**

Port in the same order as Python, because overlapping patterns make ordering semantics observable. Preserve exact call sequence and outputs:

- `source_derived_inline_content`
- `source_derived_split_gaiji_notes`
- `source_derived_inlined_gaiji_content`
- `source_derived_gaiji_content`

Add explicit ordering tests around overlapping markers:
- gaiji markers at line edges,
- nested ruby/notes,
- multiple unmatched/ambiguous patterns.

Before implementation, verify each regex used in this step against the Task 1.5 audit table and attach its concrete engine choice (`regex` vs `fancy-regex`) in comments.

- [ ] **Step 3: Ruby + marker enrichment pass**

Port in the same order as Python, immediately after Step 2, and preserve exact call ordering:

- `source_derived_ruby_and_reference_content`
- `normalize_source_derived_paragraph`/`paragraph_*` helpers that depend on these results.

- [ ] **Step 4: Attach-following-caption enrichment pass**

Add an explicit `attach_following_captions` pass after paragraph normalization and before returning final blocks:

- map a single-figure paragraph immediately followed by caption-only paragraph,
- merge caption into the figure node with `x-caption-provenance`,
- append `figure.image_caption` summary entries with preserved payload keys.

Add tests for:
- annotation ruby,
- annotation + left/right reading,
- one-line ruby marker variants,
- accent/nested forbidden markers.

- [ ] **Step 5: Capabilities and summary emission in this pass**

Emit the same `meta.semantic_summary.syntax` keys used today:

- `gaiji.marker`, `ruby.basic`, `decoration.*`, `figure.image_inline`, `figure.image_caption`, `projection.warning`.

Assert with targeted tests that summary value payloads preserve `kind`, `value`, `provenance` shape.

### Task 2c: JIS2UCS handling and provenance rules

**Files:**
- Modify: `adapters/aozora2html/src/jis2ucs.rs`
- Modify: `adapters/aozora2html/Cargo.toml`
- Optional: `adapters/aozora2html/build.rs`

- [ ] **Step 1: Remove runtime filesystem search dependency for the table**

Keep the JIS2UCS source-of-truth in-repo as `adapters/aozora2html/data/jis2ucs.yml` and load via `include_str!` in Rust at compile time. Generate or update this file as part of migration docs.

Prefer a compile-time constant mapping path:

- use a `build.rs` that parses `data/jis2ucs.yml` at build time and emits `src/generated_jis2ucs.rs`,
- expose a `phf::Map` (preferred default) from generated code; allow `const` hash map only if performance/compatibility review explicitly permits.

Do not use general runtime YAML parse in normal operation.
Allow runtime YAML parse only under the explicit `runtime-jis2ucs` feature (for local debugging), and document that CI must not enable it.

- [ ] **Step 2: Implement deterministic parser with unit tests**

- parse table once via `once_cell::sync::Lazy` cache only in the non-`build.rs` fallback path,
- normalize row numbers (`0-03-04` style normalization equivalent to Python normalize function),
- map Unicode/unknown behavior parity for gaiji resolution.

Add tests for:
- `normalize_jis_code`,
- successful decode,
- unresolved codepoint fallbacks.

### Task 3: Define output type strategy and preserve extension fields

**Files:**
- Modify: `adapters/aozora2html/src/model.rs`
- Modify: `adapters/aozora2html/src/lib.rs`

- [ ] **Step 1: Use explicit internal representation + JSON serializer, not `ab-ir` output model**

`ab-ir` has useful core types, but `adapter.py` emits parser-specific extension fields (`x-*`, custom source-derived keys) that are not modeled cleanly in `ab-ir` today. Decision:

- Internal Rust `model.rs` structs capture required invariants for mapping passes.
- Final serialization is `serde_json::Value` to preserve exact legacy output contract.

Do not attempt to force `x-*` fields through `ab-ir` without a dedicated schema extension review.

### Task 4: Wrapper orchestration and backend switch

**Files:**
- Modify: `adapters/aozora2html/aozora2html-adapter`
- Modify: `adapters/aozora2html/adapter.py` (kept as legacy fallback path)
- Modify: `adapters/aozora2html/README.md`

- [ ] **Step 1: Replace dual-flag fallback with one explicit backend selector**

Use:

- `AOZORA2HTML_BACKEND=rust|python` (default: `python` initially, then later flip to `rust` when parity gate passes).

No automatic fallback on non-zero status. If selected backend fails, fail fast with visible error.

- [ ] **Step 2: Preserve wrapper contract exactly**

Keep the same behavior surface:

- parser invocation and encoding shim unchanged,
- `--version` output format unchanged,
- `--mode html` returns raw XHTML unchanged,
- parser-failed JSON envelope unchanged.

- [ ] **Step 3: Make protocol coupling explicit**

Comment in `main.rs`/wrapper that Rust CLI flags match wrapper contract (`--source`, `--xhtml`, `--parser-failed`, `--parser-error-file`) even though these are adapter-protocol concerns, not natural Rust API concerns.

### Task 5: Parity gate, deprecation, and final cutover

**Files:**
- Modify: `adapters/aozora2html/tests/test_mapper.py`
- Modify: `adapters/aozora2html/tests/fixtures` (if strict canonicalization requires updates)
- Modify: `docs/aat-fidelity-rust-migration.md` (new or update existing)
- Modify: `docs/adapter-fidelity.md` (if wording changes after migration)

- [ ] **Step 1: Enable default-Rust mode after full fixture parity**

When `AOZORA2HTML_PARITY=1` and fixture parity suite is green:

- change `adapters/aozora2html/aozora2html-adapter` to flip `AOZORA2HTML_BACKEND` default from `python` to `rust`,
- keep `AOZORA2HTML_BACKEND=python` as explicit emergency path.

- [ ] **Step 2: Require parity checks in migration gate tests**

Run:

```bash
AOZORA2HTML_PARITY=1 AOZORA2HTML_BACKEND=rust pytest adapters/aozora2html/tests/test_mapper.py -vv
```

and fidelity scripts:

```bash
bash tests/aat-fidelity-cross-summary-xhtml-smoke.sh
bash tests/aat-fidelity-cross-adapter-script-smoke.sh
```

- [ ] **Step 3: Move Python to explicit legacy status**

In README/docs, mark `adapter.py` as fallback-only and not default.

- [ ] **Step 4: Decide final rollback policy only after two stable release cycles**

If parity remains green for two CI runs, replace fallback references with legacy status and limit direct Python execution paths to explicit operator override.

If parity fails after default flip in CI or release smoke, do not keep Rust-default behavior. Add a required rollback task to switch the wrapper default back to `python` and reopen migration gate before any new release.

### Task 6: Workspace and maintenance choices

**Files:**
- Modify: `adapters/aozora2html/Cargo.toml`
- Modify: `justfile` (optional if automation commands are added)

- [ ] **Step 1: Keep adapter crates out of workspace members (consistent with existing `adapters/aozora2` pattern)**

Do not add `adapters/aozora2html` to root workspace members. Keep crate built via script using explicit `--manifest-path` like existing adapter workflows.

- [ ] **Step 2: Add just recipes for Rust build/test in migration mode**

Add concise recipes:

- build Rust mapper,
- run Rust-focused migration tests,
- run parity suite.

Exact command target should match wrapper path and keep parity env var usage consistent.

- [ ] **Step 3: Add a baseline performance gate**

Track a lightweight benchmark target before enabling default Rust:
- verify `map_with_protocol_bytes` avoids unnecessary clones in profiler/criterion smoke,
- document expected allocation profile for fixture corpus (hot path should be linear and mostly borrowed),
- use this as a planning guard for subsequent optimization tasks.

### Self-Review

- [ ] **Execution check (replace checklist with assertions):**

```bash
AOZORA2HTML_PARITY=1 AOZORA2HTML_BACKEND=rust pytest adapters/aozora2html/tests/test_mapper.py -vv
cargo test --manifest-path adapters/aozora2html/Cargo.toml
bash tests/aat-fidelity-cross-summary-xhtml-smoke.sh
bash tests/aat-fidelity-cross-adapter-script-smoke.sh
```

If all pass, review passes.

**Plan complete and saved to `docs/superpowers/plans/2026-05-04-aozora2html-rust-migration.md`. Two execution options:**

1. **Subagent-Driven (recommended)** - dispatch one worker per task and run an explicit milestone review between tasks.
2. **Inline Execution** - execute tasks in this session with checkpoints.
