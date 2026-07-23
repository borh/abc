# ADR 0024: Parser-IR Additive Additions — Span Coordinate Semantics and `ruby.direction`

Status: Accepted
Validation scope: fixture
Release authority: publication
Date: 2026-07-02
Accepted: 2026-07-03
Supersedes: none
Depends on: ADR 0002 [scope: source_span_coverage gate]
Source: `docs/handoffs/review-reflection.md` §1.3/§1.5, `docs/handoffs/full-corpus-probe.md`

## Implementation Status

Accepted on 2026-07-03 after `schemas/parser-ir.schema.json` gained
`span.coordinate_system` and `ruby.direction`, the imported ab-validator
boundary fixture was rotated to parser-IR schema hash
`sha256:b22d3f24676d443972a543305a2536783762d6a102c42b2efafbf92849d16f13`,
and `nix run .#validate-design-bundle` completed successfully.

The v0 design-bundle parser-IR example still uses synthetic fixture hashes
inside the design example manifest; that fixture remains schema-valid and is
not the external parser boundary hash gate.

## Context

External review §1.3 and §1.5 identified two load-bearing gaps in `schemas/parser-ir.schema.json`:

1. **Span semantics are deferred-but-load-bearing.** ADR 0002 gates `source_span_coverage` = 100% for parser candidates, but the schema leaves `span.start`/`span.end` semantics unspecified. The full-corpus probe had to guess `start=byte_start` and `column=null`. Coverage cannot be measured against undefined coordinates.
2. **`ruby.direction` LOSS is the largest single divergence in real data.** The full-corpus probe measured `ruby.direction` LOSS at ~1.76M occurrences (1,764,113×). Left vs right ruby is semantically meaningful in Japanese typesetting and TEI can represent it; leaving it as permanent v1 LOSS is inconsistent with faithful TEI publication.

The review proposed batching these additive parser-IR additions into one schema-hash cascade rather than separate rotations. ADR 0018 predicate renames were already executed and verified, so they are intentionally excluded from this cascade.

## Decision

Add two backwards-compatible, optional fields to `schemas/parser-ir.schema.json` in a single batched change:

### Span coordinate model

The `span` object keeps its required fields `{start, end}` and gains an explicit coordinate model:

- `coordinate_system`: optional string, const `"decoded_utf8"`.
- `line`: already optional in the schema, documented as 1-based.
- `column`: already optional and nullable in the schema.

When `coordinate_system` is present (or defaulted), `start`/`end` are UTF-8 byte offsets into the **decoded** source string; `line` is 1-based; `column` may be `null`.

This matches the AAT contract (`aat-contract.md:180-187`), which defines the default AAT coordinate system as `decoded_utf8` with UTF-8 byte offsets and 1-based line numbers. Aligning parser-IR with the AAT contract removes the ambiguity that forced the full-corpus probe to guess.

**Backwards-compatibility choice:** `line`, `column`, and `coordinate_system` remain optional. Existing fixtures that carry only `{start, end}` or `{start, end, line, column}` continue to validate. A migration note was rejected in favor of optional fields because:

- The schema already carried `line`/`column` as optional fields.
- Requiring `coordinate_system` would invalidate every existing parser-IR fixture.
- Optional fields give the same semantic clarity without pushing migration burden onto producers.

### `ruby.direction`

The `rubyNode.ruby` object gains an optional `direction` field:

- Type: string enum `["left", "right"]` or `null`.
- Semantics: reading placement relative to base text (`left` = before base, `right` = after base).

This promotes `ruby.direction` out of the v1 LOSS ledger. Existing fixtures that omit `direction` continue to validate; the field is additive.

## Consequences

- `schemas/parser-ir.schema.json` changes, which changes the JCS schema hash used by parser-IR manifests.
- Existing parser-IR fixtures remain valid because all new fields are optional.
- The fixture regeneration + schema-hash rotation happened once for this batch, not once per addition.

## Rollback

If the additive span/ruby fields prove insufficient or wrong, supersede this
ADR and introduce a new parser-IR schema hash. Existing fixtures remain valid
because all added fields are optional; do not reinterpret parser-IR emitted
under this schema hash under a new coordinate or ruby model.

## Acceptance Criteria

- **ADR-0024-C1 — fixture-behavior:** The current parser-IR schema accepts `decoded_utf8` spans and ruby direction values `left`, `right`, and null, and rejects an unknown coordinate system.
- **ADR-0024-C2 — fixture-behavior:** The parser-IR span definition continues to accept the legacy `{start,end}` and `{start,end,line,column}` object shapes.
- **ADR-0024-C3 — fixture-behavior:** Current publication fixtures carry and consume `decoded_utf8` spans.
- **ADR-0024-C4 — fixture-behavior:** TEI rendering preserves ruby direction using a profile-valid `rend` value. See `test/abc/tools/parser_publication_evidence_test.clj`.
- **ADR-0024-C5 — structural-invariant:** Parser-IR schema JCS hash changes propagate to the parser-IR schema coordinate copied into materialized manifests.
- **ADR-0024-C6 — operational-behavior:** The supported design-bundle application exits zero over the current parser-IR schema and fixture. Evidence boundary: `test/abc/tools/validate_design_bundle_test.clj`.

## Deferred follow-ups

1. **Producer implementation:** Future AAT -> parser-IR mapping code should emit `span.coordinate_system: "decoded_utf8"` and project known AAT `ruby.direction` values into parser-IR.
2. **AAT scope remains invented:** Rule **I-01** (`ruby.scope` INVENTION) remains an invention because AAT still does not supply scope. (The task framing referenced "I-02"; the live mapping document assigns `ruby.scope` to I-01 and `gaiji.raw_marker` to I-02, so the correction is recorded here.)
