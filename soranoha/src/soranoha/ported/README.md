# soranoha.ported — the abc copy lane (D12/R2)

Verbatim copies of abc's per-work pipeline code, `abc.tools.*` →
`soranoha.ported.*`. Divergence is prevented by the Slice-1
byte-equivalence acceptance test, not by review of these files; do not
"improve" them here. This lane retires with abc (post-JADH2026, D6/D7).

Deliberate deviations from the abc originals, each with no
artifact-byte consequence:

- `logging.clj` is a stderr shim (abc binds Telemere).
- `metadata_record.clj` / `person_record.clj`: RDF/Turtle mapping and the
  Jena/aristotle requires stripped — the kernel never renders RDF and
  those functions were the only consumers.
- `files.clj`: `load-jena-model` removed (same reason).
- Record JSON-Schema paths resolve through `soranoha.ported.assets/*root*`
  instead of the working directory (the kernel binds it to the abc
  checkout; D20 — the schemas stay abc-owned, one source of truth).

The kernel-native identity path (`soranoha.core.canonical` /
`soranoha.core.hash`) is NOT part of this lane: it implements the
protocol spec's named canonicalizer and is bound to abc by the shared
cross-language vector fixture.
