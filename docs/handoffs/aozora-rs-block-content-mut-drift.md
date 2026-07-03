# aozora-rs `block_content_mut` drift — finding

Date: 2026-07-03
Status: open (needs decision)
Audit ref: `docs/handoffs/crates-optimization-audit.md` §2.8
Resolves-to: NOT a code fix — a decision on aozora-rs's maintenance path.

## Summary

The audit (§2.8) flagged that `adapters/aozora-rs/src/aat.rs` calls
`ab_ir::block_content_mut` while `fn block_content_mut` had zero matches in
`crates/ab-ir`. The audit framed this as "adapter-contract drift" and routed
it to `hammock-driven-design` before any edit. The adapter-boundary decision
(`docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`)
has since settled the high-level question: **AAT JSON is the normative adapter
contract; `ab-ir` remains an optional in-workspace convenience library for
adapters that want typed builders, and ab-ir may change lockstep with the
workspace while adapters are vendored in-repo.**

This finding records the concrete drift, corrects the audit's framing after
verification, and states the decision that is still open.

## Verified evidence (post-audit re-check, 2026-07-03)

- `crates/ab-ir/src/lib.rs:1159` defines `pub fn block_content(block: &Block) -> &[Inline]`. It matches `Block::Break { .. } => &[]`.
- `crates/ab-ir/src/lib.rs` does **not** define `block_content_mut`. `rg "fn block_content_mut" crates/ab-ir/` → 0 hits.
- `adapters/aozora-rs/src/aat.rs:464,642,753` call `ab_ir::block_content_mut(...)`.
  (The audit also cited lines 2006, 2199, 2240; those are `block_content` — the
  immutable accessor — not `_mut`. Three call sites, not six.)
- `git log -S "fn block_content_mut" -- crates/ab-ir/` shows the function was
  **added** in `983a343` ("feat: add parser-neutral IR crate") and **removed**
  in `60841e2` ("chore: complete code quality improvement plan").

## Correction to the audit's framing

The audit implied `block_content_mut` was missing for "versioning" reasons.
Verification shows the removal was a **compile-driven consequence of an enum
shape change**, not a deliberate API withdrawal:

- Pre-`60841e2`, `Block::Break` had a `content: Vec<Inline>` field.
- Current `Block::Break` has only `kind: BreakKind` — no `content`.
- The old `block_content_mut` matched `Block::Break { content, .. } => content`.
  Once `Break` lost `content`, that arm no longer compiled. Because
  `block_content_mut` had **zero in-workspace callers** (aozora-rs is excluded
  from the workspace build; `Cargo.toml:18-23` excludes `adapters/aozora-rs`),
  deleting it was the path of least resistance and `cargo build --workspace`
  stayed green.

So the drift is not "ab-ir dropped a function aozora-rs still calls." It is
"aozora-rs has fallen behind ab-ir's API by more than one function — it also
relies on a `Block::Break` shape that no longer exists." Restoring
`block_content_mut` alone would NOT make aozora-rs build: the `Block::Break`
arm that the old body matched is gone from the enum.

## Why this is filed, not fixed

The boundary decision allows ab-ir to change lockstep with the workspace and
explicitly leaves per-adapter migration out of scope: "New adapter-facing
tooling must consume AAT JSON ... not ab-ir internals ... ab-ir changes can
remain lockstep with the workspace while adapters are vendored in this
repository." The current measurement path is aozora2html (JSON-only, no ab-ir
dep), not aozora-rs. aozora-rs is live (recent commits `ed37b70`, `2ef2dc5`,
`eacd5b3`) but is not what the current measurement run uses.

Per `codebase-simplification`'s PROTECT-before-TRANSFORM rule and
`receiving-code-review`'s verify-before-acting rule, choosing a migration path
for aozora-rs is a design decision, not a behavior-preserving refactor. The
three plausible paths are mutually exclusive and each has real consequences:

## Decision options

1. **Migrate aozora-rs off `block_content_mut`.** Rewrite the three call sites
   (`aat.rs:464,642,753`) to use `block_content` plus direct field mutation, or
   to construct replacement `Block` values instead of mutating in place.
   Keeps ab-ir minimal; costs an aozora-rs-only change set. Restores
   `cargo build` in `adapters/aozora-rs/` standalone. Does not touch ab-ir.

2. **Restore `block_content_mut` with a Break arm that compiles.** Since
   `Block::Break` has no `content`, the mutable accessor cannot borrow a Vec
   from it; the Break arm would have to return a borrowed empty Vec
   (e.g. via a `thread_local!` or `once_cell` static) or `panic!`. Both are
   ugly and reintroduce an API the workspace does not use. Not recommended.

3. **Accept aozora-rs as broken-pending-JSON-migration and document it.** The
   boundary decision directs new adapter tooling to JSON; aozora-rs's typed
   path is legacy. If aozora-rs is not on any current measurement or release
   path, leave it broken with a README note, and revisit when (or if) it is
   revived as a typed adapter.

## Recommendation

Option 1 if aozora-rs is still a live measurement target; Option 3 if it is
not. Option 2 only if aozora-rs must build immediately AND cannot be migrated
in the same window — and even then, the Break-arm contortion makes it worse
than Option 1.

This finding does not block the in-workspace Rust work (the workspace builds
and tests green without aozora-rs). It blocks only standalone `cargo build`
in `adapters/aozora-rs/`, which is not exercised by `cargo test --workspace`
or any current gate.
