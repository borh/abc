# Adapter Upstream Faithfulness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ensure parser adapters record upstream gaiji behavior faithfully, while keeping expected Unicode resolution in a separate oracle/reporting layer.

**Architecture:** Adapter AAT records what the upstream parser emitted. Oracle correctness is computed later from independent JIS/gaiji references, so reports can distinguish faithful-upstream, correct, unresolved, wrong, and dropped outcomes.

**Tech Stack:** Rust adapters, `aozora-core`, existing AAT JSON projection, shell/Cargo tests with temp/build outputs under `/db/ab-validator`.

---

### Task 1: Fix aozora2 Gaiji Faithfulness

**Files:**
- Modify: `adapters/aozora2/src/lib.rs`

- [ ] **Step 1: Write failing tests**

Add tests that prove the adapter loses upstream gaiji resolution today:

```rust
#[test]
fn parse_inline_content_preserves_aozora2_resolved_jis_gaiji() {
    let content = parse_inline_content("耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて");

    assert_eq!(content[0]["kind"], "text");
    assert_eq!(content[0]["value"], "耳朶を");
    assert_eq!(content[1]["kind"], "gaiji");
    assert_eq!(content[1]["description"], "「てへん＋掌」、第4水準2-13-47");
    assert_eq!(content[1]["resolved"], "撑");
    assert_eq!(content[1]["jis_code"], "2-13-47");
    assert_eq!(content[2]["kind"], "text");
    assert_eq!(content[2]["value"], "えて");
}
```

Run:

```bash
TMPDIR=/db/ab-validator/tmp TMP=/db/ab-validator/tmp TEMP=/db/ab-validator/tmp \
  CARGO_TARGET_DIR=/db/ab-validator/target-aozora2 \
  cargo test --manifest-path adapters/aozora2/Cargo.toml parse_inline_content_preserves_aozora2_resolved_jis_gaiji -- --nocapture
```

Expected: FAIL because `resolved` is empty and `jis_code` is null.

- [ ] **Step 2: Implement minimal AST-backed inline mapping**

Use `aozora_core::tokenize` and `aozora_core::parse` inside `parse_inline_content`, then convert supported `Node` variants:

```rust
fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    aozora_nodes_to_aat_content(&aozora_core::parse(&aozora_core::tokenize(text)))
}
```

Map `Text`, `Gaiji`, `Ruby`, and simple wrapper nodes to existing AAT JSON fields. Use `Node::to_text()` for unsupported inline nodes so the adapter remains conservative.

- [ ] **Step 3: Run aozora2 tests**

Run:

```bash
TMPDIR=/db/ab-validator/tmp TMP=/db/ab-validator/tmp TEMP=/db/ab-validator/tmp \
  CARGO_TARGET_DIR=/db/ab-validator/target-aozora2 \
  cargo test --manifest-path adapters/aozora2/Cargo.toml -- --nocapture
```

Expected: PASS.

### Task 2: Characterize Other Adapter Behavior

**Files:**
- Modify only if tests reveal an adapter wrapper is unfaithful.

- [ ] **Step 1: Probe aozora-rs and aozora2html on the same gaiji marker**

Run their adapter commands with:

```text
耳朶を※［＃「てへん＋掌」、第4水準2-13-47］えて
```

Record whether the wrapper preserves upstream behavior, drops it, or explicitly represents unresolved behavior.

- [ ] **Step 2: Add targeted tests for any confirmed wrapper mismatch**

Only change adapter wrapper code after a failing test shows that our wrapper diverges from upstream behavior.

### Task 3: Verification

**Files:**
- Modified files from Tasks 1 and 2.

- [ ] **Step 1: Run targeted adapter tests**

Run all touched adapter test suites using `/db/ab-validator` for temp and target directories.

- [ ] **Step 2: Run CLI sample**

Confirm `adapters/aozora2` emits:

```json
{"kind":"gaiji","description":"「てへん＋掌」、第4水準2-13-47","resolved":"撑","jis_code":"2-13-47"}
```

for the sample marker.
