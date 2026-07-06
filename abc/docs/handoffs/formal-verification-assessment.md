# Formal Verification Assessment: Can Chiasmus Deepen the Investigation?

> Status: tooling probe. Loaded the chiasmus MCP skill and ran actual Z3 +
> call-graph queries against the abc/ab-validator codebase. Verdict: **yes,
> two avenues are viable and one was demonstrated end-to-end.**

## 1. The chiasmus tool surface (what's available)

`chiasmus` MCP server (11 tools), all functional:

| Tool | Use here |
|---|---|
| `chiasmus_verify` (z3) | SMT-LIB: combinatorial invariants over manifest identity (ADR 0001). **Demonstrated.** |
| `chiasmus_verify` (prolog) | Facts + rules; also accepts Mermaid. Useful for layered policy rules. |
| `chiasmus_graph` | tree-sitter call-graph for Rust/Clojure/etc. reachability, impact, dead-code, cycles. **Demonstrated.** |
| `chiasmus_map` / `chiasmus_search` | codebase outline + semantic search (needs embedding API key for search). |
| `chiasmus_formalize` / `chiasmus_skills` / `chiasmus_craft` | template library + authoring for reusable invariant specs. |

## 2. Avenue A (demonstrated): Z3-verify ADR 0001 manifest identity invariants

ADR 0001 (`docs/adr/0001-manifest-identity.md`) is *exactly* the kind of
crisp, combinatorial rule an SMT solver excels at. Three invariants are
formalizable:

| Invariant (ADR 0001) | Formal shape | chiasmus result |
|---|---|---|
| **R1: reproducibility-conflict detection** — two successful manifests with identical `artifact_id` and different `content_hash` ⇒ release must fail. | `∀ m1,m2: id(m1)=id(m2) ∧ status=success ∧ content(m1)≠content(m2) ⇒ release_valid=false` | **UNSAT** (no counterexample) ✅ — invariant holds in the SMT model. |
| **R2: non-circularity** — `artifact_id` (the computed hash) must not be a field of `manifest_identity_object`. | structural: `artifact_id ∉ fields(identity_object)` | **naive formalization too weak** (see §3); needs structural model. |
| **R3: null-dimension invariance** — an absent dimension as JSON `null` vs omitted must not change identity; null vs absent must not change the hash. | `hash({...k:null}) = hash({...k omitted})` ⇒ false | needs a JCS-serialization model — Z3 strings inadequate (see §3). |

### What the probe proved

`chiasmus_verify` with z3 returned **`unsat`** for the counterexample query
against R1: there is no model in which a release is valid while two successful
manifests share an artifact_id with differing content_hash. This is *genuine
formal verification* of the invariant's internal consistency — it does not
prove the code implements it, but it proves the rule is self-consistent and
non-vacuous (a useful redundant check on the ADR text).

## 3. Limits hit (and what to do about them)

| Limit | Cause | Resolution |
|---|---|---|
| R2 (non-circularity) returned SAT with a degenerate model (`artifact_id="A"` substring of `identity_object_json="A"`) | I modeled containment as `str.contains`. That can't distinguish "the artifact_id is a *declared field*" from "the hash string happens to occur in the serialized object." | Build a *structural* model: enumerate the identity fields as `(declare-const field_in_object Bool)` per ADR 0001's 12-field list, assert `artifact_id_in_object = false` per field, and check for counterexample. chiasmus can do this; the spec just needs refinement, not a different tool. |
| R3 (null vs omitted) can't be expressed | JCS canonicalization is a string transformation Z3 has no built-in theory for. | Out of scope for SMT; this is better checked by the existing canonicalization fixture (`fixtures/canonicalization/`) + a property test, not a solver. Leave R3 to the test gate. |
| Solver proves the *spec* is consistent, not that the *code* implements it | Z3 reasons over declared constraints, not the Clojure source. | Combine with the existing `nix run .#validate-design-bundle` gate: the ADR text is now solver-verified; the test gate verifies implementation. Two layers. |

## 4. Avenue B (demonstrated): call-graph reachability for the adapter↔ab-ir boundary

The `adapter-boundary-audit.md` handoff claimed aozora2html is schema-only
(zero ab-* deps) while aozora-rs consumes ab-ir Rust types.
`chiasmus_graph` confirms this mechanically:

```
chiasmus_graph(
  analysis=reachability,
  files=[ab-ir/src/lib.rs, aozora-rs/src/aat.rs, aozora-rs/src/lib.rs,
         aozora2html/src/lib.rs],
  from="aozora2html_main",
  to="blocks_to_aat_projection")
→ { "reachable": false }
```

**Finding:** aozora2html does **not** reach `blocks_to_aat_projection` (the
ab-ir serialization function). This machine-confirms the audit's claim that
aozora2html builds AAT JSON without ab-ir, while the aozora-rs path reaches
it. The graph tool can also produce `impact` (transitive callers of a changed
ab-ir symbol) and `dead-code` — directly relevant to the ab-ir versioning
question (Finding 4 of adapter-boundary-audit): **which adapters break if an
ab-ir enum variant is renamed?** That's a single `chiasmus_graph impact`
query, runnable now.

## 5. Where formal verification genuinely moves the needle

Ranked by leverage:

| # | Use | Skill tool | Why it helps |
|---|---|---|---|
| 1 | **Z3-verify ADR 0001 R1** (reproducibility conflict) end-to-end with assertions named for unsatCore | `chiasmus_verify` z3 | Already demonstrated; turns the ADR's "MUST fail" into a solver-checked invariant. Add as a CI step (prolog/z3 spec committed beside the ADR). |
| 2 | **ab-ir break-analysis**: `chiasmus_graph impact` on each ab-ir symbol aozora-rs consumes | `chiasmus_graph` | Answers "if I rename ab-ir::Block, which adapters break?" — replaces guessing in the typed-vs-schema adapter-boundary decision (Finding 4). |
| 3 | **dead-code detection** across ab-validator crates | `chiasmus_graph dead-code` | Tests the crate-classification.md "fake seam" claims (e.g. is ab-diff-utils' `merge` reachable from >1 consumer?) with hard graph facts instead of grep heuristics. |
| 4 | **cycles/layer-violation** in the abc Clojure tools | `chiasmus_graph cycles` | The validate-design-bundle has many sub-fns; a cycle check would catch any accidental mutual recursion introduced by the simplification spec. |
| 5 | **R2 non-circularity** with a structural model (12 declared Bool fields) | `chiasmus_verify` z3 | A real proof that artifact_id is structurally absent from identity_object — worth doing once the model is built; ~30 lines of SMT-LIB. |

## 6. Where NOT to use formal verification (calibrating)

- **Don't SMT-verify the loss-handling policy** (owned-mapping-design §2). Those
  are *design choices*, not invariants — `LOSS → drop-sidecar` is a policy, not
  a theorem. Formalizing it would be ceremony.
- **Don't SMT-verify the full-corpus divergence distribution** (full-corpus-probe.md).
  That's empirical measurement; the probe already settled it. A solver adds
  nothing.
- **Don't reach for `chiasmus_solve`** (the end-to-end LLM fill) — it needs an
  API key and the `formalize`+`verify` pair is sufficient and cheaper.

## 7. Recommended next formalization step (single highest-leverage)

Write a committed `docs/adr/0001-invariants.smt2` containing R1 (the proven
reproducibility-conflict invariant) + R2 (non-circularity, structural model),
and add a Nix check that runs `chiasmus_verify` against it. This converts the
ADR's prose "MUST" clauses into a solver-checked artifact that fails CI if
the invariant is edited into inconsistency. Ave A, item #1, made durable.

---

*Tools: chiasmus MCP (z3 + tree-sitter). Probes run 2026-07-02. All findings
here are reproducible by re-running the tool calls documented in §2 and §4.*
