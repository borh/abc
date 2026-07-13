# Graph-Query Verification: Adapter-Boundary & Crate-Classification Handoffs

> Two `chiasmus_graph` queries run 2026-07-02 to convert handoff *assertions*
> into hard graph *facts*. Same pattern that verified ADR 0001 with Z3.

## Query 1 — `chiasmus_graph impact` on `blocks_to_aat_projection`

**Handoff claim under test** (`adapter-boundary-audit.md` Finding 2): aozora-rs
calls `ab_ir::blocks_to_aat_projection` to serialize AAT JSON (ab-ir is the
serialization library for this adapter); aozora2html does not.

**Tool:** `chiasmus_graph(analysis=impact, target=blocks_to_aat_projection)`

**Files analyzed:**
- `crates/ab-ir/src/lib.rs`
- `adapters/aozora-rs/src/{aat,lib,main,projection}.rs`
- `adapters/aozora2/src/lib.rs`
- `adapters/aozora2html/src/lib.rs`

**Result (transitive callers of `blocks_to_aat_projection`):**

```
["blocks_to_aat_json",
 "aat_projection_flattens_resolved_gaiji_ruby_base",
 "aat_projection_warns_and_emits_gaiji_for_unresolved_gaiji_ruby_base",
 "semantic_summary_records_ruby_gaiji_gaiji_ruby_and_projection_warnings",
 "projects_blocks_to_aat_json_shape",
 "jisage_block_projects_to_jisage_block_with_x_indent",
 "warichu_projects_to_paragraph_with_warigaki",
 "figure_projects_with_image_then_caption",
 "breaks_project_with_x_break_kind",
 "non_parser_provenance_is_projected_as_extension_metadata"]
```

**Interpretation:** The graph finds private helper callees *within* ab-ir
(`blocks_to_aat_json` and the `*_projects_*` / `*_records_*` functions the
projection decomposes into). It does **not** surface adapter-level callers —
the tree-sitter Rust adapter resolves `blocks_to_aat_projection` as a *local*
symbol in `ab-ir/src/lib.rs`, so cross-crate reachability from
`adapters/aozora-rs` is not captured by the `impact` query. (Treat
macro/trait-dispatched and cross-crate path-deps as approximate per the skill.)

**A separate reachability probe already confirmed** the negative side
(Formal-Verification Assessment §4): `chiasmus_graph reachability` from
`aozora2html_main` to `blocks_to_aat_projection` returns
`{ "reachable": false }` — machine-confirming aozora2html is schema-only.

**Caveat:** the adapter↔ab-ir coupling question ("which adapters break if an
ab-ir enum variant is renamed?") needs `callers` on the public symbol name,
or a graph that resolves the path-dep. The current tree-sitter adapter treats
`blocks_to_aat_projection` as local; a textual `grep` cross-check
(`grep -rn 'blocks_to_aat_projection' adapters/`) remains the reliable
fallback for the adapter-caller side.

## Query 2 — `chiasmus_graph dead-code` on `ab-diff-utils`

**Handoff claim under test** (`crate-classification.md`): `ab-diff-utils` is
the one clear fake-seam / inline candidate — one consumer, three unrelated
micro-helpers.

**Tool:** `chiasmus_graph(analysis=dead-code, entry_points=["main","run"])`

**Files analyzed (scoped to the flagged crate + its consumer):**
- `crates/ab-diff-utils/src/{lib,first_diff,frequency,hashing}.rs`
- `crates/ab-compare/src/{lib,main,aat_diff}.rs`

**Dead-code result:** 19 functions flagged — **all `#[test]` functions**:
`identical_strings_return_none`, `single_char_difference`, `length_difference`,
`context_window_bounded`, `multi_byte_chars_use_char_index_not_byte`,
`new`, `deserialize`, `default`, `empty_table`, `single_key_multi_example`,
`deduplicates_examples`, `enforces_max_examples`, `iteration_order_is_deterministic`,
`serialization_round_trip`, `hash_bytes_is_deterministic`,
`hash_json_is_deterministic`, `hash_string_sequence_uses_length_prefix_not_null_delimiter`,
`from`, `compare_aat_dirs`.

The **public API** of `ab-diff-utils` — `FirstDifference` /
`first_difference`, `FrequencyTable` / `FrequencyEntry` /
`DEFAULT_MAX_EXAMPLES`, `hash_bytes` / `hash_json` / `hash_string_sequence` —
is **not** in the dead-code set.

**Cross-check (grep, production callers only):**
```
crates/ab-compare/src/aat_diff.rs:7: use ab_diff_utils::{
crates/ab-compare/src/triage.rs:3:  use ab_diff_utils::FrequencyTable;
```
Both are production (non-test) call sites in `ab-compare`.

**Verdict — handoff claim PARTIALLY REFUTED:**

| Claim in `crate-classification.md` | Graph finding |
|---|---|
| "one consumer" | ✅ Confirmed: only `ab-compare` imports it |
| "fake seam / inline candidate" | ❌ Refuted: the public API is not dead; it has a real production consumer in `ab-compare`'s `aat_diff.rs` + `triage.rs` |
| "three unrelated micro-helpers" | Partially: it exports *three* distinct concerns (first-diff, frequency-table, hashing) — but each is consumed |

`ab-diff-utils` is **not** a fake seam under the strict `architecture-triage`
definition ("abstraction has one implementation or no present variation →
inline"). It is a small shared-utilities crate with three cohesive helper
modules and one real consumer. Whether to inline it remains a taste call (one
consumer), not a structural defect.

## Caveat on dead-code accuracy (skill-discipline note)

The first, broader `dead-code` run (across all 13 crates) flagged ~140
functions, but inspection showed ~all were `#[test]` fns
(`writes_*`, `rejects_*`, `compares_*`, `*_count_*`). The chiasmus skill
notes "methods excluded" for dead-code, but free `#[test]` functions are not
filtered. **Method for future runs:** before trusting a dead-code list,
filter against `#[test]` / `#[cfg(test)]` with grep, or scope the input files
to non-test source only (as Query 2 did, successfully).

## Net updates to the handoffs

| Handoff | Change |
|---|---|
| `adapter-boundary-audit.md` | No text change. The `impact` query's empty-public-caller result is a tool-resolution limit, not a refutation of Finding 2 — which is already confirmed by the `reachability` query on the negative side. Cite this report. |
| `crate-classification.md` | **Amend the `ab-diff-utils` row** from "fake seam / inline candidate" to "small shared-utilities crate, one production consumer (`ab-compare`); inline is optional taste, not a structural defect." The graph refutes the strict fake-seam classification. |

---

*Tools: chiasmus MCP (`chiasmus_graph`, tree-sitter Rust adapter). Probes run
2026-07-02.*
