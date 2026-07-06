# Adapter Fidelity Fixes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve currently identified upstream adapter semantics instead of flattening or mislabeling them.

**Architecture:** Keep the change narrowly in adapter projection and AAT schema support. `aozora2` continues to use `aozora-core` parsing, but its AAT projection will emit schema-valid nodes for accent, figure, and inline containers. A separate markdown report records adapter fidelity boundaries.

**Tech Stack:** Rust 2024, `aozora-core` 0.7.1, JSON Schema 2020-12, cargo tests.

---

### Task 1: `aozora2` Projection Fidelity

**Files:**
- Modify: `adapters/aozora2/src/lib.rs`
- Modify: `adapters/aozora2/src/main.rs`
- Modify: `data/aat-schema.json`

- [ ] **Step 1: Add failing Rust tests for accent, figure, wrappers, version, and HTML mode**

Add tests in `adapters/aozora2/src/lib.rs` that construct `aozora_core::Node` values and assert:
- `Node::Accent` becomes `{ "kind": "accent", "code", "name", "resolved" }`.
- `Node::Img` becomes `{ "kind": "figure", "filename", "alt", "css_class", "width", "height" }`.
- `Node::Tcy`, `Node::Yokogumi`, `Node::Caption`, `Node::FontSize`, `Node::Keigakomi`, and `Node::Midashi` keep wrapper semantics.
- `VERSION` names `aozora-core-0.7.1`.

Run:

```bash
cargo test --manifest-path adapters/aozora2/Cargo.toml --lib -- --nocapture
```

Expected: FAIL on the new assertions before implementation.

- [ ] **Step 2: Implement schema-valid projection**

Update `append_aozora_node` so upstream nodes are not silently flattened:
- `Accent` emits `accent`.
- `Img` emits `figure`.
- Inline wrappers emit inline containers.
- `Midashi` emits a `style` inline container with `style_type: "midashi"` plus level/style extension fields.
- `BlockStart`, `BlockEnd`, and `Note` emit `raw` nodes rather than disappearing.

- [ ] **Step 3: Extend AAT schema for `figure` and inline `keigakomi`**

Add a `figure` inline definition and include it in the inline union. Add `keigakomi` to inline container kinds.

- [ ] **Step 4: Replace fake HTML mode**

Make `--mode html` return a clear unsupported error instead of escaped source wrapped in `<p>`.

- [ ] **Step 5: Verify**

Run:

```bash
cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture
cargo test --manifest-path crates/ab-check/Cargo.toml -- --nocapture
cargo test --manifest-path crates/ab-coverage/Cargo.toml -- --nocapture
```

Expected: all pass.

### Task 2: Fidelity Report

**Files:**
- Create: `docs/adapter-fidelity.md`

- [ ] **Step 1: Document current adapter boundaries**

Create a concise table covering:
- upstream dependency/version,
- parser entry point,
- preserved projection,
- lossy/known limitations,
- recommended tests/oracle follow-up.

- [ ] **Step 2: Verify docs are present**

Run:

```bash
test -s docs/adapter-fidelity.md
```

Expected: exit 0.
