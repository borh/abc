# Parser-Neutral IR Design

**Goal:** Separate parser comparison from the long-term goal of a complete Aozora parser by introducing a parser-neutral intermediate representation that can be produced by multiple parser adapters and projected into AAT without regex-owned semantics.

## Problem

The current system has two concerns braided together:

- Comparison asks: "What did two parser implementations produce, and where do they differ?"
- Parser development asks: "What is the correct interpretation of every Aozora Bunko construct?"

The validation harness can compare pass/fail reports and persisted AAT artifacts, but the adapter AAT mapping still contains regex-derived visible-text and ruby/gaiji supplement logic. That makes the comparison useful as a smoke test, but weak as an AST comparison.

## Direction

Introduce a new workspace crate, `ab-ir`, that owns parser-neutral document values and AAT projection. Parser adapters will map their native parse output into `ab-ir::Document`; AAT becomes one projection from the IR. Regex may still exist in indexing and heuristic checks, but semantic AST/AAT construction should move toward parser-derived IR values.

## Boundary

This increment does not create our final parser. It creates a stable target for parser implementations.

- `ab-ir` owns document/block/inline types and AAT JSON projection.
- `aozora-rs-adapter` maps `aozora-rs-core::Retokenized` into `ab-ir`.
- `ab-compare` continues comparing reports and AAT artifacts; later it can compare IR summaries.
- `ab-check` may keep regex heuristic properties, with confidence labels.

## Regex Policy

Allowed:

- Feature indexing and corpus routing.
- Heuristic validation properties.
- Temporary fallback paths marked as adapter metrics or diagnostics.

Not acceptable as an end-state source of truth:

- Ruby base/reading semantics.
- Gaiji semantic extraction.
- Block/style/heading structure.
- Parser-visible text projection used as AST truth.

## First Implementation Slice

1. Add `crates/ab-ir`.
2. Move the existing parser-neutral AAT block/inline value model into `ab-ir`.
3. Add AAT projection and visible-text projection to `ab-ir`.
4. Port `aozora-rs-adapter` to use `ab-ir` values.
5. Keep the current regex fallback/supplement behavior in the adapter, but make it structurally isolated so later work can remove it without changing the AAT projection API.

## Success Criteria

- `ab-ir` tests prove typed IR projects to current AAT shape.
- `aozora-rs-adapter` no longer owns parser-neutral AAT types or JSON projection.
- Existing adapter tests and golden fixture still pass.
- Workspace tests and clippy pass.
- The design makes it clear that comparison infrastructure is not the final parser.
