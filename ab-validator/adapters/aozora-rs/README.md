# aozora-rs adapter — maintenance notice

> **Status: not building (2026-07-03).** This adapter is intentionally left in a
> broken state pending a migration to the AAT-JSON adapter boundary.

## Why it is broken

The typed adapter depends on `ab-ir` Rust internals. The workspace removed
`ab_ir::block_content_mut`, and the current `Block::Break` variant no longer
carries a `content` field, so the old mutable accessor cannot be cleanly
restored. `cargo build` in this directory fails with three `E0425` errors at
`src/aat.rs:464,642,753`.

See:
- `docs/handoffs/aozora-rs-block-content-mut-drift.md` for the full drift
  finding.
- `docs/handoffs/aozora2html-buckets-and-aozora-rs-path.md` for the decision
  rationale.

## Why it is not being fixed now

The project boundary decision recorded in
`docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md`
makes **AAT JSON** the normative adapter contract. `ab-ir` is an optional
in-workspace convenience library and may change lockstep with the workspace.

This adapter is also not on any current gate:
- It is excluded from the workspace in `Cargo.toml`.
- `justfile` has no recipes for it.
- Current full-corpus measurement runs use `adapters/aozora2html/`, which emits
  AAT JSON without any `ab-*` dependency.

## Last active development

Typed-adapter work was last actively developed on **2026-05-04**
(commit `ed37b70` and neighbors).

## Re-evaluation trigger

Revive this adapter **only if** it is explicitly chosen as a live measurement
or release target again. The revival path is **not** to chase `ab-ir` API
changes; it is to migrate the adapter to emit AAT JSON conforming to
`data/aat-schema.json`, consistent with the `aozora2html` adapter path.
