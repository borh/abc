# ADR 0008: ABC Tools Runtime

Status: Accepted
Date: 2026-04-26
Accepted: 2026-04-28
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
the `org.clojars.jules_gosnell/m3` JVM library. Clojure owns orchestration, file
selection, fixture hash checks, JSONL parsing, run-summary checks, and external
command reporting.

## Runtime Boundaries

- Clojure owns ABC command logic.
- Nix owns dependency pinning and command exposure.
- Bash wrappers are allowed only as thin compatibility shims.
- `m3` owns JSON Schema validation inside the JVM.
- `xmllint` and `git-cliff` remain external tools invoked by the Clojure CLI
  until native replacements are justified.

## Acceptance Criteria

- `clojure -M:abc/validate-design-bundle` validates the current design bundle.
- `bin/validate-design-bundle.sh` calls the Clojure tool rather than duplicating
  validation logic.
- `nix run .#validate-design-bundle` works locally.
- CI continues to run the same validation command through Nix.
- Pure validation helpers have Clojure tests.

## Rollback

If the Clojure tools runtime becomes a burden, keep the file-level command
contracts and replace implementation internals behind the same Nix app names.
