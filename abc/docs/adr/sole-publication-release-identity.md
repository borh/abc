# Sole Publication Release Identity

## Implementation Status

Accepted. `soranoha build-publication` is the only operation that selects
official source, renders the corpus through the sole per-work renderer
`abc.tools.materialize-publication/materialize-publication!`, constructs the one
live `snapshot-index.json` (version `0.2.0`) from the actual selection, parser
runtime identity, failures, and the four canonical artifact references, and
atomically installs the resulting root. Release admissibility is recomputed,
fail-closed, over the completed candidate by
`abc.tools.publication-release/verify-release-root!`; only its fresh result
controls installation and the release-facing exit status. The competing
request-set rehearsal/reproduction producer was retired in the same campaign.

## Context

Before this campaign the direct build derived per-work artifacts but never
assembled a single authenticated release value, and a locally derived
`release_admissible` flag on `source-selection-report.json` stood in for a real
release decision. Two producers could both advertise a publication identity, and
occupancy of an output path was mistaken for admission.

The retained system has four deep boundaries: `parser-release-authority`
authenticates the exact Accepted parser candidate; `materialize-publication!`
renders one work; `snapshot-index` constructs and validates the one corpus
identity; and `publication-release` owns the fail-closed `verify-release-root!`.
The build façade owns source trust, assembly, installation, and the
release-facing exit status. Projection commands consume the index; none render
or reconstruct it.

## Decision

- **Sole assembler/installer.** `build-publication` is the only operation that
  may select official source, render a corpus, construct `snapshot-index.json`,
  and install the resulting root. No compatibility wrapper repeats that
  composition.
- **One live identity.** Snapshot-index `0.2.0` is the sole live publication
  identity value. Its identity object closes over the source selection hash, the
  authenticated parser coordinates, the failure set, and the artifact set; the
  index and every manifest/reference locator are relative.
- **Recomputable admissibility, no receipt.** Admissibility is decided only
  through `verify-release-root!`, which reads the candidate the index names,
  recomputes the closure, loads and hashes the authority envelopes from the same
  bytes their loaders parsed, and calls the pure `release-problems` once. There
  is no `admission.json`, admitted marker, or occupancy-based inference;
  installation alone never implies admissibility. The build writes the fresh
  verifier result into the derived `publications/publications-report.json`, which
  is excluded from the closed scan and from index identity.
- **Exact parser-authority dependency.** The release depends on the exact
  Accepted `custom-parser-release-qualification` decision and its
  `:release-authority :publication`. `:validation-scope` is the governed fact of
  that record; it is never reinterpreted as an ordered runtime scope hierarchy. A
  future scope change is a superseding decision, not a hardcoded set of
  "stronger" keywords.
- **Exact closure.** A completed release closes over exactly its four canonical
  per-work references (source, parser-IR, plaintext, TEI). The closure verifier
  rejects an unreferenced regular file inside a referenced per-work directory and
  any referenced manifest, content, or sidecar whose bytes or hashes disagree,
  exempting only the derived `publications/publications-report.json`.

## Consequences

Source-trust and rights remain fail-closed and now surface as recomputed
verifier problems rather than a pre-render assertion: a `fixture` build or a
rights-blocked corpus renders real artifacts into an inspectable, non-admissible
diagnostic root, exits nonzero, and never replaces a prior root on a strict
workflow exception. Because `verify-release-root!` accepts paths only and
packages the problem vector, boolean, and loader-derived hashes itself, the
build cannot substitute an asserted verdict for a computed one.

## Evidence

- `test/abc/tools/soranoha_test.clj`
- `test/abc/tools/snapshot_index_test.clj`
- `test/abc/tools/publication_release_test.clj`
- `test/abc/sim/content_sim_test.clj`
