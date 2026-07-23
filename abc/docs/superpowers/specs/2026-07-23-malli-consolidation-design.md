# Change E — Malli Consolidation (behavior-preserving)

**Date:** 2026-07-23 · **Status:** Draft (for review)

**Parent:** Change E of `2026-07-23-adr-evidence-apparatus-decomplection-design.md`.
A **separate** commit series, run **after** Change B (B deletes `evidence_io.clj`
and unwraps `record-read!` repo-wide, removing `malli.clj`'s only cross-cutting I/O
dependency, inside `cached-schema`/`cached-schema-hash`).

## Where Malli helps — and where it must not

Malli removes real code **only in surviving Clojure-owned structural validation**.
Never for: the semantic migration audit or claim↔test correspondence; semantic
entailment; gate execution; filesystem existence/hash checks; freshness policy; the
ADR graph / reciprocity rules; or cross-language wire artifacts already governed by
JSON Schema, TEI, or SHACL. Using it there would recreate the deleted apparatus as
schemas.

## Behavior contract (what "behavior-preserving" means here)

This change deliberately alters internal Clojure surfaces, so the preserved surface
must be named explicitly.

- **Preserved (must not change):** CLI exit codes and operator-facing output; the
  set of documents accepted vs rejected; all hashes and serialized data;
  documented, operator-facing validation **messages**.
- **Intentionally internal (may change):** Malli registry-key identities (e.g.
  `::am/parser-evidence-entry`), helper-namespace locations (`cached-schema` moving
  namespaces), raw Malli `explanation` values and error ordering, `install!`, and
  unsupported/undocumented function arities.

If any "internal" item turns out to be published (an external caller, a documented
API), E must either add a compatibility alias or be relabelled an API change for
that item — it is not silently behavior-preserving.

## Registry architecture (resolves the dependency-cycle blocker)

Domains already depend on `abc.tools.malli` for scalars, so `malli.clj` must not
depend back on domains to compose their schemas. Use one-directional values, no
central registry, no assembler namespace:

```clojure
;; abc.tools.malli — shared values only, depends on nothing domain-specific
(def scalar-schemas { ::sha256-hash …, ::nonblank-string … })

;; each domain namespace — owns its schemas and its own registry
(def schemas { ::parser-evidence-entry … })
(def registry
  (mr/composite-registry (m/default-schemas) am/scalar-schemas schemas))
```

Validation call sites take a registry (or a schema resolved against one)
**explicitly**. `explain-contract`, currently coupled to the central
`contract-schemas` map, is replaced by validation against the caller's explicit
registry (or plain `m/explain` with `{:registry …}`); it cannot survive E3
unchanged.

## Current state (verified 2026-07-23)

`abc.tools.malli` (421 LOC) braids five concerns:

- **shared scalar schemas** — `::sha256-hash`, `nonblank-string?`,
  `concrete-adapter?`, `workspace-logical-path?`;
- **domain contract schemas** — `parser-evidence-entry`,
  `aat-parser-ir-compat-entry`, `design-bundle-schemas`, and their private example
  data (`parser-evidence-examples`, `compat-entry-example`);
- **global registry mutation + instrumentation** — `compose-project-registry`,
  `install!` (`mr/set-default-registry!` **and** `mi/instrument!`);
- **JSON-Schema file caching/hashing + networknt error formatting** —
  `cached-schema`, `cached-schema-hash`, and `humanize-validation-errors` (which
  walks the m3/networknt error tree, `m3-leaf-errors` — this is **not** Malli
  explanation);
- **Malli explanation** — `explanation-messages`, `explain-or-throw!`
  (`m/explain` + `me/humanize`).

`abc.tools.schema` (116 LOC) already owns the JSON-Schema/networknt concern
(`read-schema`, `schema-hash`, `validation-errors`, `validate-json!/jsonl!`), so E1
consolidates into an existing home.

Two effects are conflated in the lifecycle and must be handled separately:
`mr/set-default-registry!` **does** have a production consumer —
`validate_design_bundle` resolves `::am/…` keyword schemas through the default
registry (e.g. `explain-or-throw! ::am/manifest-inputs`); `mi/instrument!` has **no
observed production invocation** — it wraps `citable-hashes`, which the design-bundle
path never calls. `install!` is called by **10** test fixtures today (recount after
Change B removes evidence tests).

## Tasks (execution order; each behavior-preserving under the contract above)

### T1 — Delete the fixed generator scaffolding (was E5; first, so later tasks don't move doomed code)
`mg/generate ::…-entry` then validating with the generating schema is tautological:
`:gen/elements` are 4 and 1 fixed examples. Delete these self-validation tests in
`malli_test.clj:63,67`, `validate_design_bundle_test.clj:3245,3249`, **and**
`parser_evidence_test.clj:278` (generated round-trip/self-validation) and `:288`
(generated `:sha256` mutation). If the mutation assertion is worth keeping, rewrite
it as table-driven explicit fixtures. Remove the now-unused `*-examples` data and
`:gen/elements` hooks.
- **Verify:** removed tests were tautological; suite green.

### T2 — Move JSON-Schema cache/hash and networknt formatting to `schema.clj` (was E1)
Move `cached-schema`, `cached-schema-hash`, and **`humanize-validation-errors`**
(networknt) to `abc.tools.schema`, beside `read-schema`/`schema-hash`/
`validation-errors`. Call `schema/schema-hash` directly rather than dynamically
resolving `manifest/schema-hash` (which just delegates back). After this,
`schema.clj` no longer requires `abc.tools.malli`, and `malli.clj` keeps only
Malli-explanation logic (`explanation-messages`, `explain-or-throw!`).
- **Characterize:** pin `cached-schema`/`cached-schema-hash` outputs for a
  representative path.
- **Verify (byte-level OK here):** parsed schema values remain equal and hashes
  match exactly; the two error representations (networknt vs Malli) are no longer
  in one namespace.

### T3 — Move domain contract schemas to their owners, with explicit registries (was E2)
Move `parser-evidence-entry` to `parser_evidence.clj`, `aat-parser-ir-compat-entry`
to `aat_parser_ir_compat.clj`, `design-bundle-schemas` to
`validate_design_bundle.clj`. Introduce each domain's own `registry` (the
architecture above) during the move. `malli.clj` retains only `scalar-schemas` and
the Malli-explanation helpers.
- **Characterize:** record which namespaces reference each schema keyword and each
  validation call site (including the `validate_design_bundle` `::am/…` sites).
- **Verify (equivalence, not byte-identity):** the same documents are accepted and
  rejected, and documented operator-facing messages are equivalent. Do **not**
  promise byte identity for raw Malli explanations, schema paths, or keyword
  identities — those are internal.

### T4 — Delete the duplicated key checks; decide `edn-registry` from its real callers (was E3)
Add the wrapper `[:map [:entries [:vector {:min 1} entry-schema]]]` in each domain
and delete the manual `required-entry-keys` / `required-evidence-scope-common-keys`
vectors and missing-key functions — Malli **continues to reject** missing keys and
empty `:entries`, now via the wrapper (add a mutation-rejection test pinning the
message). **Retain** duplicate-identity, filesystem/hash, and policy checks as
ordinary functions.
- `abc.tools.edn-registry` exposes `missing-entry-key-errors` **and** the
  higher-level `registry-errors` orchestration. The wrapper replaces only the
  former. Deleting `edn-registry` therefore requires also replacing `registry-errors`
  (entry-error + duplicate-error composition) with local composition in each domain;
  if that is out of scope, `edn-registry` **stays** for `registry-errors`. Decide
  per surviving caller — do not assume deletion. (Its third caller,
  `adr_evidence_runtime_inputs`, is already deleted by Change B.)
- **Verify:** valid registries still validate; missing-key / empty-`:entries`
  documents still rejected with equivalent messages.

### T5 — Remove the global registry + instrumentation lifecycle (was E4; trust/state)
Two effects, handled separately:
- **Registry mutation (has a consumer):** replace `mr/set-default-registry!` with
  explicit registry passing. `explain-or-throw!` (and `explain-contract`'s
  replacement) take a registry argument; `validate_design_bundle`'s `::am/…` sites
  pass their domain registry. No global default-registry mutation remains.
- **Instrumentation (no consumer):** remove `mi/instrument!`, the `m/=>` on
  `citable-hashes`, and the `install!` entry point. Make `citable-hashes`
  zero-arity (load + validate) with a private helper for the already-validated
  value; **characterize any public use of the 1-arity first** and keep it public if
  found.
- Recount and update the `install!` test fixtures (10 today, fewer after B); each
  becomes an explicit-registry setup or is dropped where it only installed.
- **Verify:** the design-bundle validator rejects the same malformed inputs with no
  global registry mutation; focused suites green. **If characterization finds a real
  consumer of instrumentation, stop and keep it** — this task is contingent on there
  being none.

## Verification (whole change)

1. `abc-clj-kondo` and `clj-nix-focused-tests` green before and after each task.
2. T2: JSON-Schema hashes and parsed values byte-identical to pre-E.
3. T3/T4: the accepted/rejected document sets and documented operator-facing
   messages are equivalent (raw Malli explanations are internal and may reorder).
4. T5: the design-bundle validator rejects the same inputs; no global registry
   mutation remains.
5. End state: `malli.clj` = scalar schemas + Malli explanation; `schema.clj` = all
   JSON-Schema cache/hash + networknt formatting; domain schemas + registries live
   with their owners.

## Falsifiers

- A domain cannot compose its own registry without referencing another domain (then
  the scalar/registry split is wrong — fix the split, do not add an assembler).
- Characterization shows `mi/instrument!` catches a contract no explicit validation
  replaces (then T5 keeps instrumentation for that path).
- A public caller depends on the 1-arity `citable-hashes` or on `install!` (then
  keep it, or add a compatibility alias — and relabel that item an API change).
- Deleting a `required-entry-keys` vector changes which documents validate (then the
  wrapper is not yet equivalent — fix the schema).
- `edn-registry` retains a legitimate `registry-errors` caller after T4 (then it
  stays).

## Non-goals

- No `{:closed true}` / open-vs-closed policy change.
- No new schema over any cross-language wire artifact.
- **No speculative schemas.** Do not add the absent `::schema-id`/`::schema-hash`
  scalars — they have no current consumer.
- No coupling to Changes A–D beyond running after B.
