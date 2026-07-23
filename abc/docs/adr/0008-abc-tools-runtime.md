# ADR 0008: ABC Tools Runtime

Status: Accepted
Date: 2026-04-26
Accepted: 2026-04-28
Validation scope: operational
Release authority: development
Supersedes: none
Amends: ADR 0006
Source: `docs/adr/0006-v0-design-bundle-validation.md`

## Implementation Status

Acceptance criteria satisfied as of 2026-04-28. `abc.tools.*` namespaces now
cover validation, manifest-to-RDF, materialization, SHACL, TEI RelaxNG,
metadata-record build/validate, TEI-header generation, and Aozora CSV
ingestion. All entry points are exposed as Nix apps (`validate-design-bundle`,
`materialize-import`, `manifest-to-rdf`, `aozora-ingest`).

## Context

ABC now has one validation shell script and will soon need commands for
materializing imported parser output, generating manifests, checking hashes, and
building publication views. Keeping that logic in Bash would make JSON parsing,
hash rules, schema validation, and error handling difficult to test.

ABC is still a Clojure repository, and the JVM remains useful for XML/TEI and
Relax NG tooling. Nix should provide stable entry points and pinned tools, but
Nix derivations and shell scripts should not become the application logic.

## Decision

ABC pipeline tooling lives in Clojure namespaces under `abc.tools`.

The first tool is:

```bash
clojure -M:abc/validate-design-bundle
```

Nix exposes the same command as:

```bash
nix run .#validate-design-bundle
```

`bin/validate-design-bundle.sh` remains as a compatibility wrapper and calls the
Clojure entry point.

For v0, the Clojure validator performs JSON Schema Draft 2020-12 checks through
the `com.networknt/json-schema-validator` JVM library (pinned at 3.0.6).
Clojure owns orchestration, file selection, fixture hash checks, JSONL parsing,
run-summary checks, and external command reporting.

## Runtime Boundaries

- Clojure owns ABC command logic.
- Nix owns dependency pinning and command exposure.
- Bash wrappers are allowed only as thin compatibility shims.
- `com.networknt/json-schema-validator` owns JSON Schema validation inside the
  JVM.
- `xmllint` and `git-cliff` remain external tools invoked by the Clojure CLI
  until native replacements are justified.

## Acceptance Criteria

- **ADR-0008-C1 — structural-invariant:** `bin/validate-design-bundle.sh`
  delegates to the Clojure tool without duplicating validation logic, as
  asserted by `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0008-C2 — operational-behavior:** The supported
  `nix run .#validate-design-bundle` app exits zero, as asserted by
  `test/abc/tools/foundation_evidence_test.clj` and
  `test/abc/tools/validate_design_bundle_test.clj`.
- **ADR-0008-C3 — structural-invariant:** The validation workflow checks out
  the repository before invoking the exact repository-root Nix design-bundle
  command in the same job, as asserted by
  `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0008-C4 — structural-invariant:** The named pure validation-helper read
  set exactly equals `abc.tools.validate-design-bundle/evidence-input-paths`,
  as asserted by `test/abc/tools/parser_maintenance_evidence_test.clj`.

## Rollback

If the Clojure tools runtime becomes a burden, keep the file-level command
contracts and replace implementation internals behind the same Nix app names.
