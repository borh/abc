# Next Steps

## Current Milestone

The next implementation milestone is an executable v0 contract harness:

> Given a checked-in imported parser-output fixture, ABC validates the boundary
> bundle, computes stable schema and artifact identities, emits parser-IR and
> warning manifests into temporary or caller-selected output directories, and
> verifies deterministic regeneration without committing generated
> `examples/**` outputs.

This keeps ABC focused on its own boundary contract. Do not move next into full
corpus processing, parser execution inside ABC, database services, RO-Crate
packaging, tokenizer pipelines, or public release security.

Generated materialized manifests are intentionally not checked in under
`examples/materialized-import/`. They churn whenever identity inputs, schema
hashes, generated timestamps, manifest shape, or fixture source files change.
The reviewable contract is the source fixture plus the Clojure tests and
`validate-design-bundle` temp-generation checks.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- out/imported/parser-ir.manifest.json -o out/imported/parser-ir.ttl
nix flake check
nix run github:jlesquembre/clj-nix#deps-lock -- --deps-include nix/clj-nix-deps.edn
clojure -M:abc/validate-design-bundle
clojure -M:abc/materialize-import examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
clojure -M:abc/manifest-to-rdf out/imported/parser-ir.manifest.json -o out/imported/parser-ir.ttl
```

The direct Clojure validator expects external tools such as `xmllint` and
`git-cliff` on `PATH`. The Nix app is the portable local/CI entry point because
it supplies those tools.

The full legacy test suite is not yet the v0 contract gate. For the executable
contract slice, `nix flake check` runs the focused Clojure tests with a
clj-nix dependency cache generated from `nix/clj-nix-deps.edn`. When running
outside Nix, use the focused tool/text tests:

```bash
clojure -M:test -e '(require (quote clojure.test)
                             (quote abc.text-test)
                             (quote abc.ndc-test)
                             (quote abc.tools.hash-test)
                             (quote abc.tools.jcs-test)
                             (quote abc.tools.schema-test)
                             (quote abc.tools.manifest-index-test)
                             (quote abc.tools.manifest-to-rdf-test)
                             (quote abc.tools.validate-design-bundle-test)
                             (quote abc.tools.materialize-import-test))
                    (let [r (clojure.test/run-tests
                             (quote abc.text-test)
                             (quote abc.ndc-test)
                             (quote abc.tools.hash-test)
                             (quote abc.tools.jcs-test)
                             (quote abc.tools.schema-test)
                             (quote abc.tools.manifest-index-test)
                             (quote abc.tools.manifest-to-rdf-test)
                             (quote abc.tools.validate-design-bundle-test)
                             (quote abc.tools.materialize-import-test))]
                      (when (pos? (+ (:fail r) (:error r)))
                        (System/exit 1)))'
```

## Implemented Contract Pieces

- `abc.tools.hash` owns SHA-256 formatting, parsing, byte/string/file hashing,
  and `sha256:<64 lowercase hex>` validation.
- `abc.tools.jcs` owns canonical JSON bytes/strings for identity hashing.
- `abc.tools.json` owns deterministic pretty JSON for generated fixture bytes.
  This is separate from JCS and is not the ArtifactID canonicalization
  algorithm.
- `abc.tools.schema` owns schema hashing and JSON/JSONL validation through M3.
- `abc.tools.manifest-index` owns manifest index entries and duplicate
  ArtifactID/content-hash conflict detection.
- `abc.tools.manifest-to-rdf` owns the deterministic v0 RDF/Turtle view and
  exposes `clojure -M:abc/manifest-to-rdf` / `nix run .#manifest-to-rdf`.
- `abc.tools.manifest` remains the manifest construction API and delegates hash,
  JCS, JSON, and schema responsibilities to the focused namespaces.
- `abc.tools.materialize-import` writes parser-IR and warnings manifests from
  `examples/ab-validator-output/`.
- `abc.tools.validate-design-bundle` materializes imported output into a temp
  directory, validates generated manifests, and validates the repository-local
  design bundle.
- `nix/clj-nix-deps.edn` is the lean dependency surface for sandboxed focused
  Clojure tests under `nix flake check`; regenerate `deps-lock.json` when it
  changes.

## Validation Rules

Identity and schema hashing:

- `artifact_id` is the SHA-256 hash of RFC 8785-style canonical JSON bytes for
  `manifest_identity_object`.
- `artifact_id` identifies the derivation coordinate, not the materialized output
  bytes.
- Output bytes are identified by `content.content_hash`.
- Schema hashes are SHA-256 over canonical JSON bytes of the parsed schema value.
  Pretty-printing, source whitespace, and object member order do not affect the
  schema hash.
- JSON arrays preserve order during canonicalization.

Imported-output boundary:

- `examples/ab-validator-output/parser-ir.json` validates against
  `schemas/parser-ir.schema.json`.
- `examples/ab-validator-output/warnings.jsonl` validates line-by-line against
  `schemas/diagnostic.schema.json`.
- `examples/ab-validator-output/run-summary.jsonl` validates line-by-line against
  `schemas/run-summary.schema.json`.
- `examples/ab-validator-output/manifest-inputs.json` validates against
  `schemas/manifest-inputs.schema.json`.
- `examples/ab-validator-output/comparison-report.json` validates against
  `schemas/comparison-report.schema.json`.

Run-summary structure:

- exactly one `run-start`
- zero or more `work-result`
- exactly one `run-complete`
- all events use the same `run_id`
- each event includes `run_id`

Materialized manifest behavior:

- parser IR and warnings manifests have distinct `artifact_id` values.
- `artifact_id` is distinct from `content.content_hash`.
- `manifest_identity_object` does not include `artifact_id`.
- `manifest_schema_hash` uses the same schema-hash rule as ADR 0001.
- deterministic pretty JSON output is stable for repeated runs with identical
  input and `--generated-at`.
- producer-supplied parser IR and diagnostic schema hashes must match ABC's
  checked-in schemas unless a future compatibility rule is registered.
- release validation fails if successful manifests reuse one `artifact_id` with
  more than one `content.content_hash`.
- the deterministic RDF/Turtle view sorts provenance and sidecar data whose
  semantic order is not meaningful.

## Remaining Work After This Milestone

1. Expand the manifest-to-RDF view beyond the current core entity/activity/
   sidecar mapping if RDF publication becomes a release target.
2. Decide whether the wider legacy dependency graph should also move behind
   clj-nix, or remain outside the v0 contract gate until those namespaces are
   active again.

## Done Criteria For This Milestone

- `nix run .#validate-design-bundle` passes.
- `clojure -M:abc/validate-design-bundle` passes when `xmllint` and `git-cliff`
  are on `PATH`.
- focused `abc.tools.*` contract tests pass.
- `nix flake check` evaluates the app/devshell surface, the lightweight
  contract-source check, and the clj-nix-backed focused Clojure test check.
- `nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z` writes valid manifests.
- `nix run .#manifest-to-rdf -- out/imported/parser-ir.manifest.json -o out/imported/parser-ir.ttl` writes a deterministic Turtle view.
- repeated materialization with the same inputs and timestamp is byte-stable.
- no generated `examples/materialized-import/**` files are committed.
