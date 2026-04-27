# SHACL Enforcement and Failure-Manifest RDF — Design

Status: Approved
Date: 2026-04-27
Source milestone: `docs/next-steps.md` "Remaining Work" item 1 (RDF view expansion) and unfinished bundle item from `docs/v0-design-bundle/README.md` (failure manifest fixture).

## Goal

Close the canonical-JSON ↔ RDF/PROV-O loop by:

1. Running `schemas/manifest.shacl.ttl` against the RDF view of every manifest the v0 contract harness produces or carries as a fixture.
2. Exercising the failure-manifest subtype end-to-end: JSON schema → RDF generation → SHACL conformance.

Generate-and-validate on demand only. No checked-in `failure-manifest.example.ttl` in this milestone (deferred per ADR 0011 spirit until the failure RDF mapping stabilizes).

## Non-Goals

- No new shapes added to `manifest.shacl.ttl`.
- No SLSA/SHACL-DS/cross-graph validation.
- No standalone `nix run .#validate-shacl` app; SHACL runs inside `validate-design-bundle` only.
- No checked-in `failure-manifest.example.ttl` fixture, no parity test for the failure case.

## Architecture

Three small units, each with one purpose.

### `abc.tools.shacl` (new)

Thin wrapper over `org.apache.jena/jena-shacl`.

- `load-shapes-graph` — reads `schemas/manifest.shacl.ttl`, returns a Jena `Graph`. Loaded once per `validate-design-bundle` run.
- `validate!` — takes `{:keys [shapes-graph data-graph label]}`, runs `ShaclValidator/get .validate`, returns `:ok` on conformance or throws `ex-info` carrying an `:errors` vector when non-conformant.
- Each violation is rendered as `<severity>: <focus-node> <path> — <message>` (skipping nil parts gracefully) so the existing `-main` printer in `validate-design-bundle` handles output without changes.
- All violations from one validation are collected before throwing — no fail-fast.
- No project-specific logic beyond Turtle reading; the namespace is reusable for any Jena graph.

### `abc.tools.manifest-to-rdf` (extension)

Three behavioral fixes verified by tests, applied minimally inside the existing `manifest->graph` builder:

1. **Failure typing.** When `artifact_kind == "failure"`, add `a abc:FailureArtifact` alongside `a abc:Artifact, prov:Entity` on the artifact node. SHACL `FailureShape` targets `abc:FailureArtifact`.
2. **Errors-role sidecar predicate.** Sidecars whose `role == "errors"` are linked from the artifact via `abc:hasErrorArtifact` (per `docs/v0-design-bundle/manifest-to-rdf.md`). All other roles continue to use `abc:hasSidecar`. The SHACL `FailureShape` requires `abc:hasErrorArtifact sh:minCount 1`, so the fix is load-bearing.
3. **Null-content branch.** When `content == null`, the artifact node must omit `abc:contentHash` and `dcterms:format`. The existing JSON-null branch is asserted by test rather than re-implemented (verify, fix only if broken).

No mapping changes for non-failure manifests beyond predicate (1) and (2). The success-case `examples/v0/example-work/manifest.ttl` byte-for-byte parity test must continue to pass.

### `abc.tools.validate-design-bundle` (wiring)

One new step inserted between the existing `==> Checking materialized RDF views` and `==> Checking imported ab-validator output` steps:

```
==> Validating SHACL shapes
shacl shapes ok
```

Process:

1. Load shapes graph once via `shacl/load-shapes-graph`.
2. For each manifest in this set, build the RDF graph and validate:
   - every materialized manifest produced earlier in this run (parser-IR, warnings)
   - `examples/v0/example-work/manifest.json`
   - `examples/v0/example-work/failure-manifest.example.json`
3. Surface any thrown `ex-info` through the existing `check-errors!` printer.

The shapes graph is reused for all data graphs; data graphs are constructed per-manifest and discarded.

## Data Flow

```
JSON manifest path
  → files/read-json
  → manifest-to-rdf/manifest->graph    (existing Aristotle/Jena path, with new failure-typing + errors-predicate fixes)
  → shacl/validate! shapes-graph
  → :ok | throw ex-info {:errors [...]}
```

## Test Plan

### `test/abc/tools/shacl_test.clj` (new)

Positive:
- Example success manifest (`examples/v0/example-work/manifest.json`) RDF graph conforms.
- Example failure manifest (`examples/v0/example-work/failure-manifest.example.json`) RDF graph conforms.

Negative (constructed in-test by mutating a known-good graph):
- Graph missing `abc:artifactId` triggers `ArtifactShape` violation.
- Failure artifact missing `abc:hasErrorArtifact` triggers `FailureShape` violation.
- Bad `abc:validationStatus` enum value (e.g. `"unknown"`) triggers violation.

Each negative test asserts the thrown `ex-info` contains a non-empty `:errors` vector.

### `test/abc/tools/manifest_to_rdf_test.clj` (extension)

- Failure manifest input produces a graph that:
  - has `abc:FailureArtifact` as one of the artifact's `rdf:type` values;
  - has at least one `abc:hasErrorArtifact` triple from the artifact;
  - has no `abc:contentHash` triple on the artifact;
  - has no `dcterms:format` triple on the artifact.
- Existing success-case tests and the `manifest.ttl` byte-for-byte parity test remain unchanged.

### `test/abc/tools/validate_design_bundle_test.clj` (extension)

- Existing happy-path test now also exercises the SHACL step (extends the temp-materialize check with a non-throwing SHACL pass).

## Dependencies

- Add `org.apache.jena/jena-shacl` to `deps.edn` (Jena 5.3 line, matching what Aristotle pulls in).
- Add the same coordinate to `nix/clj-nix-deps.edn`.
- Regenerate `deps-lock.json` via `bin/update-clj-nix-lock`.

## Error Handling

- SHACL conformance is binary: either `:ok` or throw.
- All violations from a single graph validation are aggregated into one `ex-info :errors` vector.
- The existing `-main` in `validate-design-bundle` already prints `:errors` line-by-line — no printer changes needed.

## Acceptance Criteria

- `clojure -M:test` (focused namespaces) passes including the new `shacl-test` and the extended `manifest_to_rdf_test` and `validate_design_bundle_test`.
- `nix run .#validate-design-bundle` prints the new `==> Validating SHACL shapes` line and exits 0.
- `nix flake check` evaluates and the focused-test check passes with the new dependency.
- `examples/v0/example-work/manifest.ttl` byte-for-byte parity test continues to pass.
- Reverting the failure-typing change in `manifest-to-rdf` causes the failure SHACL test to fail (proves the new shape is enforced, not skipped).

## Sequencing

1. Add `jena-shacl` dependency, regenerate lock.
2. Write `abc.tools.shacl` namespace (TDD: write failing positive test first, then negative tests, then implementation).
3. Verify and (if needed) fix `manifest-to-rdf` failure typing and errors-role predicate (TDD: write failure-graph assertions first, then implement).
4. Wire SHACL pass into `validate-design-bundle` and extend the harness test.
5. Run `nix run .#validate-design-bundle` end-to-end.
6. Archive `docs/next-steps.md` to `docs/archive/2026-04-27-v0-contract-harness.md` and replace with a fresh next-steps pointing to the post-milestone state.

## References

- `schemas/manifest.shacl.ttl` — shapes targeted by the new validation step.
- `docs/v0-design-bundle/manifest-to-rdf.md` — sidecar predicate rules.
- `docs/adr/0011-generated-fixture-policy.md` — generate-on-demand precedent.
- Apache Jena SHACL — `https://jena.apache.org/documentation/shacl/`.
