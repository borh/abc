# JSON-LD Context Policy v0

Status: Draft
Date: 2026-04-28

ABC JSON-LD publication views must declare explicit contexts.

The canonical manifest remains JCS-canonical JSON, not JSON-LD. JSON-LD
compaction or expansion must not affect ArtifactID. JSON-LD context versions
are publication-profile inputs and must be hashable.

## Policy

- Context documents live under `contexts/` and are versioned like other
  publication-profile inputs.
- Context hash is recorded as publication-profile metadata.
- Linked Art JSON-LD views are regenerated from canonical ABC manifests and
  metadata records.
- Context changes may change the derived JSON-LD publication view, but they do
  not change canonical manifest identity.

## Fixtures

- `contexts/abc-v0.jsonld`
- `examples/v0/example-work/lod/linked-art-candidate.jsonld`
- `examples/v0/example-work/lod/linked-art-expanded.normalized.json`
- `examples/v0/example-work/lod/jsonld-context-validation-result.json`

## Acceptance Criteria

1. JSON-LD context is versioned.
2. Context hash is recorded as publication-profile metadata.
3. JSON-LD compaction/expansion tests do not change canonical manifest
   identity.
4. Linked Art view can be regenerated from canonical manifests and metadata
   records.

## References

- Linked Art model: https://linked.art/model/
- Linked Art profile and contexts: https://linked.art/model/profile/
