# SHACL Enforcement and Failure-Manifest RDF — Design

Status: Approved
Date: 2026-04-27
Source milestone: `docs/next-steps.md` "Remaining Work" item 1 (RDF view expansion) and unfinished bundle item from `docs/v0-design-bundle/README.md` (failure manifest fixture).

## Goal

Close the canonical-JSON ↔ RDF/PROV-O loop by:

1. Running `schemas/manifest.shacl.ttl` against the RDF view of every manifest the v0 contract harness produces or carries as a fixture.
2. Exercising the failure-manifest subtype end-to-end: JSON schema → RDF generation → SHACL conformance.

Generate-and-validate on demand only. No checked-in `failure-manifest.example.ttl` in this milestone, analogous to the policy in ADR 0011 for high-churn generated material.

## Non-Goals

- No new shapes added to `manifest.shacl.ttl`.
- No SLSA/SHACL-DS/cross-graph validation.
- No standalone `nix run .#validate-shacl` app; SHACL runs inside `validate-design-bundle` only.
- No checked-in `failure-manifest.example.ttl` fixture, no parity test for the failure case.
- No tightening of the JSON manifest schema to forbid `errors`-role sidecars on successful artifacts (see Cross-Layer Consistency below).
- No post-processing `add-shacl-relations` layer that would separate mapping from SHACL-driven enrichment. Currently unnecessary: the role-specific predicate is mandated by the mapping spec, not SHACL. Revisit if SHACL-only requirements ever drive new branches in `manifest->graph`.

## Architecture

Three small units, each with one purpose.

### `abc.tools.shacl` (new)

Thin wrapper over `org.apache.jena/jena-shacl`. No project knowledge beyond Turtle reading; reusable for any Jena graph.

API:

- `load-shapes-graph` — reads `schemas/manifest.shacl.ttl`, returns a Jena `Graph`. Loaded once per `validate-design-bundle` run.
- `validate!` — takes `{:keys [shapes-graph data-graph label]}`, runs `ShaclValidator/get .validate`. Returns `:ok` on conformance. On non-conformance, throws `ex-info` with `:errors` set to a vector of structured violation maps.

Violation map shape:

```clojure
{:severity   "Violation"           ; sh:resultSeverity local name
 :focus-node "<iri-or-blank-id>"   ; sh:focusNode (string form)
 :path       "<predicate-iri>"     ; sh:resultPath (or nil if absent)
 :message    "..."                 ; sh:resultMessage (or generated fallback)
 :source     "<shape-iri>"         ; sh:sourceShape (or nil)
 :label      "examples/.../foo.json"} ; caller-supplied data label
```

Rules:

- All violations from one validation are collected before throwing — no fail-fast.
- Rendering of violation maps for terminal output is **not** the responsibility of this namespace. Callers render.

### `abc.tools.manifest-to-rdf` (one mapping change + two regression-coverage tests)

One new behavioral change:

- **Errors-role sidecar predicate.** Sidecars whose `role == "errors"` are linked from the artifact via `abc:hasErrorArtifact` instead of the current generic `abc:hasSidecar`. Drives the SHACL `FailureShape sh:property abc:hasErrorArtifact sh:minCount 1` requirement; mandated by `docs/v0-design-bundle/manifest-to-rdf.md` independent of SHACL. All other sidecar roles continue to use `abc:hasSidecar`.

Two existing behaviors covered by new tests (no implementation change required):

- **Failure typing.** When `artifact_kind == "failure"`, the artifact node already carries `a abc:FailureArtifact` (verified at `src/abc/tools/manifest_to_rdf.clj:277`). New test asserts this; reverting that line should fail the test.
- **Null-content branch.** When `content == null`, the artifact node already omits `abc:contentHash` and `dcterms:format` (verified at `src/abc/tools/manifest_to_rdf.clj:280`). New test asserts this.

The success-case `examples/v0/example-work/manifest.ttl` byte-for-byte parity test must continue to pass after the errors-role predicate change. Since the success fixture uses a `warnings`-role sidecar (no `errors` role), the change does not affect its serialized form; this is verified empirically by the parity test, not assumed.

### `abc.tools.validate-design-bundle` (wiring)

One new step inserted between the existing `==> Checking materialized RDF views` and `==> Checking imported ab-validator output` steps:

```
==> Validating SHACL shapes
shacl shapes ok
```

Process:

1. Load shapes graph once via `shacl/load-shapes-graph`.
2. For each target manifest, build the RDF data graph via `manifest-to-rdf/manifest->graph` and call `shacl/validate!`.
3. Targets:
   - every materialized manifest produced earlier in this run (parser-IR, warnings)
   - `examples/v0/example-work/manifest.json`
   - `examples/v0/example-work/failure-manifest.example.json`
4. Render any thrown `ex-info` `:errors` here as `<severity>: <focus-node> <path> — <message> (<label>)`. Surface via the existing `check-errors!` printer or an inline `doseq`.

The shapes graph is reused for all data graphs; data graphs are constructed per-manifest and discarded.

## Cross-Layer Consistency

The JSON manifest schema currently allows a successful artifact to carry an `errors`-role sidecar (`role` is an open enum). In RDF this would surface as a non-failure artifact node with `abc:hasErrorArtifact` but no `abc:FailureArtifact` typing. SHACL `ArtifactShape sh:or` is satisfied via the content+format branch in that case, so SHACL still passes. This is acknowledged as a permissive shape, not a tightened invariant, and out of scope for this milestone.

## Data Flow

```
JSON manifest path
  → files/read-json
  → manifest-to-rdf/manifest->graph     (existing Aristotle/Jena, with errors-role predicate fix)
  → shacl/validate! shapes-graph
  → :ok | throw ex-info {:errors [violation-maps]}
                                          → rendered by validate-design-bundle
```

## Test Plan

### `test/abc/tools/shacl_test.clj` (new)

Positive:

- Example success manifest (`examples/v0/example-work/manifest.json`) RDF graph conforms (`validate!` returns `:ok`).
- Example failure manifest (`examples/v0/example-work/failure-manifest.example.json`) RDF graph conforms.

Negative graphs are constructed declaratively from data via Aristotle (`aa/graph :simple`, `aa/add` with map literals). No mutation of a known-good graph; each invalid graph stands alone.

- Graph with `abc:Artifact` node missing `abc:artifactId` triggers `ArtifactShape` violation. Assert `:errors` is non-empty and at least one entry's `:source` references `ArtifactShape`.
- Graph with `abc:FailureArtifact` node missing `abc:hasErrorArtifact` triggers `FailureShape` violation.
- Graph with `abc:Artifact` carrying `abc:validationStatus "unknown"` (outside the `sh:in` enum) triggers `ArtifactShape` enum violation.

### `test/abc/tools/manifest_to_rdf_test.clj` (extension)

For the failure manifest input, assert the generated graph:

- types the artifact node with both `abc:Artifact` and `abc:FailureArtifact` (regression coverage for existing failure typing);
- contains at least one `(artifact, abc:hasErrorArtifact, ?)` triple (verifies new errors-role predicate);
- contains no `(artifact, abc:contentHash, ?)` triple (regression coverage for null-content branch);
- contains no `(artifact, dcterms:format, ?)` triple (regression coverage for null-content branch).

Existing success-case tests and the `manifest.ttl` byte-for-byte parity test remain unchanged and must still pass.

### `test/abc/tools/validate_design_bundle_test.clj` (extension)

The existing happy-path test now also exercises the SHACL step end-to-end (the temp-materialize harness must not throw under the new step).

## Dependencies

- Add `org.apache.jena/jena-shacl` to `deps.edn` (Jena 5.3 line, matching what Aristotle already pulls in).
- Add the same coordinate to `nix/clj-nix-deps.edn`.
- Regenerate `deps-lock.json` via `bin/update-clj-nix-lock`.

## Error Handling

- SHACL conformance is binary per data graph: either `:ok` or throw.
- All violations from one graph validation are aggregated into one `ex-info :errors` vector of structured maps.
- Rendering happens in `validate-design-bundle`, not in `abc.tools.shacl`. The wrapper namespace stays a pure validation boundary.

## Acceptance Criteria

- `clojure -M:test` (focused namespaces) passes including the new `shacl-test` and the extended `manifest_to_rdf_test` and `validate_design_bundle_test`.
- `nix run .#validate-design-bundle` prints the new `==> Validating SHACL shapes` line and exits 0.
- `nix flake check` evaluates and the focused-test check passes with the new dependency.
- `examples/v0/example-work/manifest.ttl` byte-for-byte parity test continues to pass.
- The new `manifest_to_rdf_test` failure assertions guard existing behavior: removing the `(when failure? {:rdf/type :abc/FailureArtifact})` line at `src/abc/tools/manifest_to_rdf.clj:277-278`, or removing the `content` guard at `:280-282`, must cause those tests to fail. (Regression coverage, not new behavior.)
- The new `manifest_to_rdf_test` `abc:hasErrorArtifact` assertion fails before the errors-role predicate change is implemented, and passes after. (TDD coverage for the one new behavior.)

## Sequencing

1. Add `jena-shacl` dependency to `deps.edn` and `nix/clj-nix-deps.edn`; regenerate lock.
2. Write failing `manifest_to_rdf_test` cases for the failure-graph assertions (TDD red).
3. Implement the errors-role predicate change in `manifest->graph`; tests go green.
4. Write `abc.tools.shacl` namespace TDD: positive test against example success manifest first, then failure manifest, then negative cases with declaratively constructed graphs.
5. Wire SHACL pass into `validate-design-bundle` and extend the harness test.
6. Run `nix run .#validate-design-bundle` end-to-end and `clojure -M:test`.
7. Archive `docs/next-steps.md` to `docs/archive/2026-04-27-v0-contract-harness.md` and replace it with a fresh next-steps pointing at the post-milestone state.

## References

- `schemas/manifest.shacl.ttl` — shapes targeted by the new validation step.
- `docs/v0-design-bundle/manifest-to-rdf.md` — sidecar predicate rules (independent of SHACL).
- `docs/adr/0011-generated-fixture-policy.md` — generate-on-demand precedent.
- `src/abc/tools/manifest_to_rdf.clj` — current mapping implementation.
- Apache Jena SHACL — `https://jena.apache.org/documentation/shacl/`.
