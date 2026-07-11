# Temporal Knowledge-State Remediation Design

Date: 2026-07-11
Status: Proposed design for review
Parent: `2026-07-11-adr-logical-remediation-program-design.md`
Depends on: `2026-07-11-adr-evidence-and-lifecycle-remediation-design.md`

## Purpose

Represent date knowledge honestly so unknown, absent, and inapplicable states
cannot collide under a single `null` value.

## Current Fault

The CSV parser recognizes `不詳` and `未詳` as “not known” but maps them to JSON
null. The ADR invariant says identity null means not applicable, then declares
this unknown-date mapping consistent. Provenance preserves the lexical marker,
but the canonical temporal value loses the knowledge state.

## Decision

Replace nullable canonical dates with a tagged value:

```json
{
  "source_assertion": {
    "source": "aozora",
    "field": "生年月日",
    "lexical_value": "未詳",
    "snapshot_hash": "sha256:..."
  },
  "state": "known|unknown|not-recorded|not-applicable",
  "edtf": "192X|null",
  "precision": "day|month|year|decade|century|null"
}
```

Rules:

- `known` requires an EDTF value and compatible precision.
- `unknown` means the source positively states that a date is not known.
- `not-recorded` means the source provides no value.
- `not-applicable` means the domain attribute does not apply to the subject.
- Non-`known` states require `edtf=null` and `precision=null`.
- `source_assertion` uses the umbrella's shared envelope and does not determine
  state without a versioned parse rule.
- Parse corrections preserve raw and corrected representations.

## Precision and Loss

Decade and century EDTF remain valid known states. Japanese sub-century
qualifiers such as `初`, `前半`, and `末` are not silently represented as exact
centuries. The parser either emits a richer interval in a future profile or
records an explicit lossy-correction diagnostic alongside the broad century.

## RDF Contract

Known values emit the current precision-honest EDTF view. Unknown,
not-recorded, and not-applicable do not emit fabricated date literals. They may
emit explicit ABC knowledge-state nodes when the publication profile enables
them. SHACL validates the state/value dependency.

## Identity and Migration

The tagged temporal value participates in person-record identity. Consequently
unknown and not-recorded records no longer collide. The migration rotates
person-record and dependent metadata hashes and records old-to-new derivation
mappings.

The migration classifies every current null by source evidence. A null without
enough evidence is `not-recorded`; it is never guessed to be `not-applicable`.
No automatic migration produces `not-applicable` unless the domain model
establishes that the attribute cannot apply.

`not-applicable` is forward-looking vocabulary; the current corpus is expected
to produce no instances unless the migration discovers and documents a real
domain rule.

Migration totality and disjointness are checked as `clojure.test.check`
properties over generated source rows and the pinned corpus: each input maps to
exactly one state or quarantine, and bucket union equals the input population.
Hegel is not used because it has no Clojure implementation.

## Acceptance Criteria

- Schema tests enforce every state/value combination.
- `不詳` and `未詳` map to `unknown` with their lexical values preserved.
- an empty CSV cell maps to `not-recorded`.
- no current source record maps to `not-applicable` without an explicit rule.
- decade, century, BCE, and full/partial dates retain their current valid EDTF
  behavior.
- lossy sub-century parsing emits a stable diagnostic and audit entry.
- migration counts partition the complete person corpus without overlap.
- historical records remain valid under their original schema.

## Safe Fallback

Continue reading the tagged schema but stop producing it. Do not collapse new
states back to null in new canonical records; that would lose information.
