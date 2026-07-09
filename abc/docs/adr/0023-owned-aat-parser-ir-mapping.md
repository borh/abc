# ADR 0023: Owned AAT to Parser-IR Mapping and Compatibility Registry

Status: Accepted
Date: 2026-07-03
Accepted: 2026-07-03
Supersedes: none
Amends: ADR 0001
Depends on: ADR 0001, ADR 0006, ADR 0007, ADR 0009, ADR 0024,
`docs/handoffs/owned-mapping-design.md`,
`docs/handoffs/full-corpus-probe.md`

## Context

ABC accepts parser-IR produced outside this repository. The same AAT input can
produce different parser-IR content when the AAT to parser-IR mapping changes,
even if the parser-IR schema is unchanged. Treating only the parser-IR schema
hash as identity would miss that transform variability.

The full aozora-rs probe measured 17,894 AAT files and produced a generated
25-rule mapping candidate with zero measured `UNSUPPORTED` records. The
sibling `ab-validator` repo then restored the producer-owned mapping document
and measured current aozora2html AAT output. That aozora2html run found
warigaki in 243 works / 4,050 nodes and kunten in 472 works / 22,504
observations. Those counts are decision inputs for later vocabulary work; they
are not compatibility evidence for the aozora-rs mapping.

## Decision

Use the owned mapping split described as Option C in
`docs/handoffs/owned-mapping-design.md`:

- ABC owns the accepted shapes: mapping schema, divergence schema, manifest
  identity fields, and the adapter-scoped compatibility registry.
- ab-validator owns mapping documents, mapping generation, adapter
  measurement, and the executable transform from AAT JSON to parser-IR.
- AAT JSON is the adapter contract at this boundary. `ab-ir` remains optional
  and internal unless it is separately versioned later.

The mapping document hash is identity-bearing. `manifest-inputs.json` carries
`mapping_hash`, and materialized parser-IR manifests copy it into
`manifest_identity_object.aat_parser_ir_mapping_hash`. The mapping schema hash
is provenance-only and is carried by AAT mapping provenance.

Parser-IR emitted from AAT may carry legacy mapping provenance in
`parser-ir.json` `derived_from`. The completed `ab-aat-to-parser-ir` converter
instead publishes mapping provenance in `divergence.json`, alongside the
adapter, target parser-IR schema identity, and per-work divergence records. ABC
matches that provenance plus `mapping_hash` against
`data/aat-parser-ir-compatibility.edn`. Registry matches are exact over:

- AAT version
- adapter and adapter version
- mapping id, version, document hash, and schema hash
- parser-IR schema id and schema hash

Registry entries are adapter-scoped evidence claims, not adapter-neutral
schema claims. Wildcard or "any adapter" entries are invalid. Each entry must
carry an `evidence_scope` with the measured adapter, corpus, evidence type, and
mode-specific measured counts. Historical `:mapping-generation` evidence keeps
the generated rule count and unsupported-file count. Current
`:conversion-audit` evidence records file success/failure counts, parser-IR
node counts, divergence counts, rule coverage, and unsupported occurrences.

Loss handling remains auditable and mode-bound. Divergence records are
aggregated per rule with count and first path. Development behavior may record
critical divergence and continue, but release-smoke gates fail on critical
divergence records. This avoids an operator-controlled strictness flag.

## Rejected Alternatives

### Hand-maintain the mapping table in ABC

Rejected because the mapping is producer behavior. ABC should validate the
documented contract and evidence, not own a second copy of transform logic.

### Use `mapping_schema_hash` as the manifest identity dimension

Rejected because two mapping documents can validate against the same schema
while producing different parser-IR content. ADR 0001 protects identity against
that rule-set variability, so the mapping document hash must be the identity
dimension.

### Add adapter-neutral compatibility entries

Rejected because aozora-rs and aozora2html expose different measured AAT
vocabulary. The compatibility registry accepts each adapter only when
producer-owned measurement evidence exists for that adapter and mapping
version.

### Make `UNSUPPORTED` refuse by default

Rejected by full-corpus measurement. Earlier mapper behavior classified
`style` as unsupported in 30.6% of aozora-rs works before the I-09
STYLE-to-EMPHASIS rule. Hard refusal as a default would have made a mapping
policy mistake halt broad ingestion.

## Consequences

ABC's design-bundle validation now checks the registry before accepting
AAT-derived parser-IR. Adding a new adapter or mapping version requires a
producer-owned mapping document hash and a measured registry entry scoped to
that adapter.

The checked-in registry keeps the older `0.1.0` `aozora-rs-adapter`
mapping-generation entry for legacy `derived_from` fixtures. It also authorizes
the completed `0.1.1` conversion-audit evidence for `aozora-rs` and
`aozora2html` using the canonical mapping hash recorded in
`data/aat-parser-ir-compatibility.edn` (the registry is the source of truth
for the mapping hash; it is not re-stated here so that a mapping rotation does
not make this ADR stale).

## Deferred Decisions

- Whether parser-IR or a future AAT version should add first-class warigaki.
- Whether parser-IR or a future AAT version should add first-class kunten.
- Whether adapter-fidelity metadata such as metrics or semantic summaries
  should become parser-IR or manifest provenance.
- How long older parser-IR schema hashes remain accepted through the registry.
- Whether the provisional marker registry becomes a production registry after
  its measurement probes run.
