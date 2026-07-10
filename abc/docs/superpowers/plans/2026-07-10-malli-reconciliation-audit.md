# Malli Reconciliation Audit (G0)

> Read-only audit ordered by the Clojure Quality Remediation plan (Batch G0).
> Diffs `2026-04-28-malli-integration-improvements.md` against the current tree
> to decide whether any malli work belongs in the current remediation run.

**Date:** 2026-07-10. **Verdict: defer all remaining malli work to a separate plan.**

## Summary

The 2026-04 plan's **foundation** (its Tasks 1, 5, 6) landed and then evolved
well beyond what the plan specified. Its two **instrumentation-payoff** tasks did
not land — and are now **obsolete**, because their target namespaces
(`abc.aozora`, `abc.tei`, `abc.annotation`) **do not exist** in this tree.

## Landed (foundation + beyond)

- `abc.tools.malli/install!` = require + compose + `set-default-registry!` +
  `instrument!` — `malli.clj:258-271`.
- `cached-schema` / `cached-schema-hash` — `malli.clj:283-301`.
- `humanize-validation-errors` (`:315-327`), `explain-or-throw!` (`:355-372`),
  design-bundle `:fn` schemas (`:205-256`).
- Record validators route through the cache: `metadata_record.clj:26-37`,
  `person_record.clj:41-55`; `validate_design_bundle.clj:450/462/465` uses
  `explain-or-throw!`.
- `:once` `(am/install!)` fixtures across registry-exercising tests.
- **Beyond the plan:** the whole `contract-schemas` block (`malli.clj:86-203`),
  `explain-contract` (`:273-280`), `explanation-messages` (`:329-353`) — serving
  `parser_evidence.clj` / `aat_parser_ir_compat.clj` via an explicit registry.

## Not landed / obsolete

- **Task 7** (activate `m/=>` contracts under instrumentation): zero `m/=>` /
  `mx/defn` anywhere; the target files `src/abc/aozora.clj`, `src/abc/tei.clj`,
  `src/abc/annotation.clj` **do not exist**. Obsolete.
- **Task 8** (CSV-cell malli transformer `:decode/csv` / `mt/transformer`): 0
  hits; `abc.aozora` doesn't exist; `aozora_csv.clj` parses with `java.time`/regex.
  Obsolete.
- `-Dmalli.registry/type=custom` jvm-opt: **not needed** — verified `install!`
  works with the property `nil` (malli's default registry is mutable). Close as
  "won't do", not "todo".
- `nix/clj-nix-deps.edn` edits: file doesn't exist; the `-e` require mechanism was
  superseded by per-namespace `:once` fixtures + kaocha auto-discovery.

## Two residual gaps (both CONFIRMED)

- **(a) `mi/instrument!` is a no-op** — there are zero `m/=>`/`mx/defn` contracts
  (only the docstring words at `malli.clj:4-5`). `install!` still calls
  `(mi/instrument!)` at `:270`, wrapping nothing.
- **(b) Constant generators** — the two schemas the generator-backed tests
  exercise (`::parser-evidence-entry` `:129`, `::aat-parser-ir-compat-entry` `:172`)
  carry a single whole-map `:gen/elements`, so `mg/generate` returns the fixed
  example and "generate → validate same schema" tests are tautologies
  (`parser_evidence_test.clj:81-83`, `malli_test.clj:63-70`,
  `validate_design_bundle_test.clj:1763-1766`).

## Why deferred (not folded into this run)

No function in `metadata_record`, `person_record`, `parser_evidence`,
`aat_parser_ir_compat`, or the materializers has its **full input+output shape**
described by an **existing** registry key — the record validators are described by
**JSON Schema**, and the entry schemas describe one entry, not the `{:entries […]}`
wrapper the functions take. So every worthwhile `m/=>` contract requires designing
at least one **new** wrapper schema — by definition outside a "minimal" scope. The
only zero-new-schema contract is an *output-only* `::sha256-hash` on the two
`record-hash` fns, which is not worth a task.

## Recommendation

1. **Defer** `m/=>` activation and the constant-generator fix to a dedicated,
   separately-scoped plan that includes the small new wrapper schemas.
2. Optionally (hygiene, not required): either add the first real `m/=>` or leave
   `(mi/instrument!)` in place as the anchor for future contracts — do **not**
   remove it without that decision.
3. Close the `-Dmalli.registry/type=custom` plan item as "won't do".

This confirms the remediation plan's choice to keep malli out of executable scope.
