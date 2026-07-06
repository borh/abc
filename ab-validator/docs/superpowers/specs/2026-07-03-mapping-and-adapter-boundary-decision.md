# Mapping and Adapter Boundary Decision

Date: 2026-07-03
Status: proposed

## Context

ABC measurement commits `0f36135` and `ca30a92` changed the AAT-to-parser-IR mapping path from a synthesized manual table to a generated measured artifact. The ab-validator-owned artifact is now `data/aat-to-parser-ir-mapping-v1.json`, generated from executable mapper policy and folded aozora-rs corpus rule buckets.

The current generated mapping evidence is recorded in `docs/superpowers/reports/2026-07-03-aat-parser-ir-mapping-generation.md`:

- `ruby.direction` projects directly into parser-IR.
- `style` maps to parser-IR `emphasis`.
- `windows-31j-lossy` maps to parser-IR `source.encoding = Shift_JIS` with an `AMBIGUITY` ledger entry.
- The measured aozora-rs corpus has zero `UNSUPPORTED` files.
- The generated mapping validates against ABC `schemas/aat-parser-ir-mapping.schema.json`.

The normative adapter output remains AAT JSON (`data/aat-schema.json`, `docs/aat-contract.md`). Existing adapters use different implementation boundaries:

- `aozora-rs` consumes `ab-ir` Rust types directly.
- `aozora2html` emits AAT JSON without any `ab-*` dependency.
- The current aozora2html full-corpus AAT run completed with the current adapter and produced the measurement recorded in `docs/superpowers/reports/2026-07-03-aozora2html-aat-full-current.md`.

`ab-ir` is useful, but it is not currently version-disciplined as an external contract: it uses the workspace version, exposes public enums, and has no explicit public API compatibility policy.

## Decision

The adapter boundary is the AAT JSON schema and contract docs. `ab-ir` remains an optional in-workspace convenience library for adapters that want typed builders, not the normative external adapter contract.

The AAT-to-parser-IR mapping document is generated from measured executable policy. It must not be hand-created from the historical 27-rule synthesized table.

Manifest identity and ABC compatibility-registry hardening stay paused until the generated mapping artifact exists, validates, and has the two corpus measurements attached: aozora-rs zero `UNSUPPORTED` and current aozora2html AAT corpus evidence.

## Consequences

- New adapter-facing tooling must consume AAT JSON and the schema contract, not `ab-ir` internals.
- Mapping-consuming tooling must consume `data/aat-to-parser-ir-mapping-v1.json` as a generated artifact, not a copied manual table.
- `ab-ir` changes can remain lockstep with the workspace while adapters are vendored in this repository.
- If `ab-ir` is published or advertised as an external adapter SDK, a separate decision must add semver policy, changelog discipline, and compatibility markers such as `#[non_exhaustive]` where appropriate.
- The owned-mapping CLI must not rely on unpublished `ab-ir` Rust API stability and must not start from manifest identity or compatibility registry work before the generated mapping artifact is in place.
- Durable warigaki policy claims require the current aozora2html full-corpus measurement, not the aozora-rs corpus alone.
- Current aozora2html policy claims must account for the measured completeness caveat: 197 schema-invalid or missing reports and 302 parse-incomplete or missing reports in the 2026-07-03 full-corpus run.

## Rejected Alternative

Make `ab-ir` the normative adapter SDK now. This would require versioning work before the measurement loop is restored and would conflict with the proven schema-only aozora2html path.

Hand-create `data/aat-to-parser-ir-mapping-v1.json` from the old 27-rule synthesized table. ABC's full-corpus probe now invalidates that workflow: the live candidate has 25 observed folded buckets, zero `UNSUPPORTED`, and policy changes for ruby direction, style, and lossy Shift_JIS source encoding.
