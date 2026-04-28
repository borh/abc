# Example Work Bundle Fixture

Status: Draft

This directory shows the target layout for the first end-to-end Aozora example.
The current files are design fixtures with dummy hashes, not real corpus
outputs.

Files:

- `source.manifest.json`: source artifact manifest for a work extracted from a
  corpus snapshot.
- `parser-ir.json`: parser IR fixture covering text, ruby, gaiji, editor note,
  and heading nodes.
- `warnings.jsonl`: warning sidecar fixture.
- `tei.xml`: TEI output fixture.
- `validation.json`: validation result sidecar fixture. No JSON Schema exists
  for this sidecar yet.
- `tei-validation-result.json`: ADR 0012 TEI validation-result sidecar target
  covering well-formedness, Relax NG, and Schematron layers.
- `manifest.json`: TEI artifact manifest fixture.
- `manifest.ttl`: deterministic RDF/PROV-O view fixture.
- `lod/`: derived publication-view fixtures for PROV-O and Linked Art
  evaluation. Linked Art is not canonical for v0.
- `iiif/applicability.json`: ADR 0014 decision fixture recording that this
  text-only v0 bundle does not require IIIF.
- `query-index-entry.json`: query index fixture. No JSON Schema exists for
  this index entry yet.
- `failure-manifest.example.json`: first-class failure manifest fixture.

Provenance note: the source artifact is modeled as an extracted/described view
of the pinned corpus snapshot, so it lists the corpus snapshot in both
`provenance.used` and `provenance.was_derived_from`.

Selection criteria for the real work:

- includes ruby,
- includes at least one editor note,
- preferably includes gaiji,
- small enough for fast fixture review,
- has stable Aozora metadata in the pinned snapshot.
