# Release Parser Identity Approval

## Implementation Status

Accepted. `abc.tools.parser-release-authority/authenticate` authenticates the
release parser identity as a content-addressed governed record,
`data/release-parser-identity-v1.edn`, bound to this decision. Authentication
requires BOTH integrity — the record's `qualification_identity_ref` and
`candidate_ref` recompute from its own bytes through
`format-sha256(sha256-json-jcs(canonical-value x))` — AND approval — this
Accepted `:release-authority :publication` decision binding that exact
`candidate_ref`, `schema_version`, and record path. Neither integrity nor a
present decision alone authenticates.

## Context

The release parser identity was previously authenticated by recomposing the
`parser-rq-campaign` promotion verification (runs root, registry, measurements,
report, and provenance paths) with a governance decision. That coupled the
release boundary to the qualification campaign's file layout and re-derived the
candidate on every release.

This decision decouples the two. The campaign remains the historical record of
how the candidate was qualified; the release now authenticates a single,
inert-until-approved governed record. `candidate_ref` commits transitively to
the executables, mapping, parser-IR schema, adapter, and converter, so the
binding checks only the content-authority value, the protocol discriminator
(`schema_version`), and the object selector (the record path being
authenticated) rather than re-deriving those coordinates.

## Consequences

- `authenticate`'s inputs collapse to `{:release_parser_identity_path
  :decisions_path}`; its return shape (candidate ref, qualification identity,
  executable provenance, decision, authority hashes) is unchanged, so
  downstream release admissibility and runtime-identity comparison are
  untouched.
- The build-match CI check resolves the approved executable hashes through
  `authenticate` (the full integrity + binding path) and compares them
  byte-for-byte against the freshly built `ab-aozora` and `ab-aat-to-parser-ir`
  binaries; a companion `just release-parser-reproducible` recipe proves both
  binaries rebuild reproducibly.
- `sole-publication-release-identity` depends on this decision (not the
  qualification campaign) for its parser release authority.

## Dependencies

Depends on `owned-aat-parser-ir-mapping`: the mapping and target parser-IR
schema whose hashes the qualification identity carries are owned artifacts.
