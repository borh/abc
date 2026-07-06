# Aozora Source Region And Apparatus Contract

Status: Proposed design; supersedes `malformed_noise_occurrences` as a final
vocabulary
Date: 2026-07-06
Owner boundary: ab-validator owns source-region measurement, source-inventory
classification, and parser/adapter evidence. ABC owns TEI/header/back matter
policy, custom preservation, and renderer behavior.

## Problem

The current source-authority report has the right intent but the wrong terminal
shape for front matter, end matter, and Aozora source apparatus.

The current counter `malformed_noise_occurrences` combines at least two
different facts:

1. Aozora source apparatus such as `［＃］` in notation legends, usually in lines
   like `［＃］：入力者注...`.
2. Truly malformed source residues such as an unterminated `［＃` or `※［＃`.

Those are not the same domain object. Source legends, body-end markers,
terminal provenance, and colophon material are central to faithful conversion.
They may be excluded from body plaintext, but they are not malformed and should
not be hidden under a noise counter.

The correct end goal is:

> every source-observed Aozora occurrence is assigned a document region and a
> publication disposition. Body markup maps to TEI P5, TEI plus ABC extension,
> or custom preservation. Front/back/source apparatus maps to TEI
> header/front/back or custom preservation. Only genuinely invalid source
> residues become diagnostics.

## Evidence Ledger

Current measured source-authority report:

- `docs/superpowers/reports/2026-07-04-source-authority-representability.md`
- `works_scanned: 17894`
- `unknown_markers_total: 14886`
- `unallowlisted_unknown_markers_total: 0`
- legacy `malformed_noise_occurrences: 13936`

The largest reviewed unknown class is not malformed source:

- `CommandFullwidth` raw `［＃］`: 13,920 occurrences in the canonical report.
- Direct corpus spot-checking shows the dominant context is the standard Aozora
  notation legend line `［＃］：入力者注...`.

The truly malformed residue classes are small:

- `MalformedCommand` raw `［＃`: 8 occurrences.
- `MalformedAccentNotation` raw `〔`: 6 occurrences.
- `MalformedGaiji` raw `※［＃`: 1 occurrence.
- `MalformedImplicitRuby` raw `《`: 1 occurrence.

So the legacy counter is mostly source apparatus, not malformed corpus data.

## Decision

Introduce source-region-aware occurrence classification as part of the final
publication coverage contract.

Each source occurrence should have four independent facts:

1. **Source identity**
   - Work id, source file, line/byte span when available, raw marker, scanner
     kind, and normalized source-inventory row when known.
2. **Document region**
   - Where the occurrence lives in the Aozora source document.
3. **Apparatus kind**
   - What role the occurrence plays when it is not ordinary body markup.
4. **Publication disposition**
   - How the occurrence is represented in TEI, ABC custom preservation, or
     diagnostics.

This separates values that were previously braided together:

- body vs front/back location
- marker recognition vs publication policy
- source apparatus vs malformed residue
- plaintext exclusion vs TEI/custom preservation

## Source Regions

The region vocabulary should be explicit and closed in v1:

| Region | Meaning | Plaintext policy |
|---|---|---|
| `front_matter` | Material before the main body: title metadata, notation legends, input notes, source setup. | Excluded unless source explicitly marks visible body text. |
| `body` | Main work text and body-level Aozora markup. | Visible base text only. |
| `back_matter` | Material after the body: source attribution, bibliography, input/proofing metadata, colophon. | Excluded from body plaintext. |
| `source_apparatus` | Explanatory Aozora markup examples or source-processing instructions that may occur in front/back contexts. | Excluded from body plaintext. |
| `unknown_region` | Scanner cannot yet assign region. | Blocks full source-region admission until reviewed. |

`source_apparatus` can overlap front/back in the source model, but the report
should carry one primary `document_region` plus one `apparatus_kind` so counts
remain stable.

## Apparatus Kinds

Initial apparatus kinds:

| Kind | Examples | Disposition |
|---|---|---|
| `notation_legend` | `［＃］：入力者注...` | TEI `encodingDesc/editorialDecl` or ABC preservation sidecar. |
| `notation_placeholder` | `［＃…］`, `［＃（…）］` in explanatory lines | TEI `encodingDesc/editorialDecl` or ABC preservation sidecar. |
| `body_end_boundary` | `［＃本文終わり］` | Region boundary; preserve as source boundary evidence, not body text. |
| `terminal_provenance` | `［＃地付き］` source attribution and bibliography lines | TEI back/source note or ABC preservation sidecar. |
| `colophon_metadata` | input, proofing, bottom-text source lines | TEI header/sourceDesc/revision-like policy or ABC sidecar. |
| `malformed_source` | Unterminated command/ruby/accent/gaiji starts | Diagnostic/preservation record; not a body markup feature. |

The legacy `out_of_body` and `malformed_noise` scopes should be treated as
migration names, not terminal categories.

## End-State Occurrence Format

The machine-readable source-authority report should eventually expose an
occurrence summary with this shape:

```json
{
  "source_region_coverage": {
    "body_typed_occurrences": 0,
    "body_raw_preserved_occurrences": 0,
    "source_apparatus_occurrences": 0,
    "front_matter_occurrences": 0,
    "back_matter_occurrences": 0,
    "malformed_source_occurrences": 0,
    "unsupported_body_markup_occurrences": 0,
    "unknown_region_occurrences": 0,
    "unknown_unreviewed_occurrences": 0
  }
}
```

Each reviewed class should also carry:

```json
{
  "kind": "CommandFullwidth",
  "raw": "［＃］",
  "document_region": "front_matter",
  "apparatus_kind": "notation_legend",
  "publication_disposition": "custom_sidecar",
  "source_inventory_row": null,
  "tei_target": "encodingDesc/editorialDecl",
  "plaintext_projection": "omit",
  "occurrences": 13920,
  "sample_works": ["000005_5"]
}
```

The exact field names may evolve during implementation, but the values above
are the contract: source apparatus is not malformed, and body-text omission is
not the same as unrepresented source.

## Completion Gate

The source-region portion of the full Aozora-to-TEI/custom goal is complete only
when all of the following are true for the measured corpus scope:

- `unknown_unreviewed_occurrences == 0`
- `unknown_region_occurrences == 0` for recurring classes
- `unsupported_body_markup_occurrences == 0`
- every `source_apparatus` class has a TEI/header/front/back or custom-sidecar
  disposition
- every `back_matter` source-note/provenance class has a TEI back/header or
  custom-sidecar disposition
- every `malformed_source` class is counted separately from source apparatus and
  retained as diagnostic evidence
- plaintext output is body-visible text only, excluding front/back/source
  apparatus by typed region, not by ad hoc deletion

The gate may still pass with malformed source diagnostics if those diagnostics
are reviewed and preserved. It must not pass with unclassified body markup.

## TEI And Custom Mapping Policy

Source regions map as follows:

| Source fact | Preferred target | Notes |
|---|---|---|
| Body Aozora markup | TEI P5 where faithful; TEI plus ABC extension or custom sidecar where exact source identity would be lost. | This is the main publication mapping. |
| Source notation legend | TEI `encodingDesc/editorialDecl` or ABC sidecar. | It explains markup policy; it is not body content. |
| Body-end boundary | Region boundary plus preservation record. | It should drive segmentation; it is not visible body text. |
| Final source attribution | Parser-IR `source-note` with `placement="back"` when source evidence supports it. | TEI back/source note, not body base text. |
| Colophon/provenance metadata | TEI header/sourceDesc/back policy or ABC sidecar. | Exact placement is ABC-owned. |
| Truly malformed source residue | Diagnostic/preservation sidecar. | Do not treat as unsupported Aozora syntax. |

This policy is compatible with the current plaintext rule:

> plaintext contains no ruby readings or other metadata; it is visible body text.

## Scanner Design Constraint

Do not add a second source-recognition scanner for this contract.

The region/apparatus layer should build on the shared source marker recognition
engine already used by source inventory. Adding a parallel front/back scanner
would create a false authority seam: two recognizers would have to agree on
which byte ranges are markers before they can disagree on region or policy.

The correct decomposition is:

1. shared marker recognition
2. source document segmentation into front/body/back
3. occurrence classification by region and apparatus kind
4. publication disposition

## Migration From Current Report

Current fields remain useful but should be read as legacy/current-state
evidence:

| Current field | Migration target |
|---|---|
| `typed_occurrences` | `body_typed_occurrences` plus any typed front/back rows if later modeled. |
| `raw_preserved_occurrences` | body raw preservation, source-note preservation, or custom sidecar depending region. |
| `out_of_body_occurrences` | split into `front_matter_occurrences`, `back_matter_occurrences`, and boundary/provenance apparatus kinds. |
| `malformed_noise_occurrences` | split into `source_apparatus_occurrences` and `malformed_source_occurrences`. |
| `unsupported_occurrences` | `unsupported_body_markup_occurrences`; should remain zero for completion. |
| `needs_research_occurrences` | unknown/unreviewed buckets with owner and next action. |

The expected first split from the current corpus is:

- most `CommandFullwidth` `［＃］` occurrences move to
  `source_apparatus_occurrences` / `notation_legend`
- `［＃本文終わり］` moves to `back_matter` boundary accounting
- `［＃…］` and `［＃（…）］` move to notation-placeholder apparatus
- the 16 true malformed-start occurrences move to
  `malformed_source_occurrences`

Do not claim the exact split until the region-aware report has been
implemented and regenerated.

## Implementation Slices

1. **Schema and terminology split**
   - Add source-region coverage fields to
     `data/aozora-source-inventory.schema.json`.
   - Keep legacy counters only as compatibility fields or remove them in a
     deliberate schema-version rotation.

2. **Allowlist scope rotation**
   - Replace terminal scopes `out_of_body` and `malformed_noise` with explicit
     scopes such as `front_matter_legend`, `notation_placeholder`,
     `body_end_boundary`, `back_matter_provenance`, and `malformed_source`.

3. **Region-aware source inventory**
   - Detect front/body/back regions from Aozora separators and body-end markers.
   - Classify notation legend lines by context, not only by raw marker.
   - Add a fixture where the same raw marker is apparatus in front matter but
     would be reviewed differently inside body text.

4. **Report regeneration**
   - Regenerate source-authority JSON/Markdown.
   - Verify `unsupported_body_markup_occurrences == 0`.
   - Verify actual malformed source count is separated from source apparatus.

5. **ABC handoff**
   - Handoff the front/back/source-apparatus classes to ABC so TEI header/back
     and custom sidecar policy can be admitted explicitly.

## Rejected Alternatives

### Keep `malformed_noise_occurrences`

Rejected. It preserves the gate outcome but loses the domain distinction that
matters for conversion: source legends and terminal provenance are valid source
apparatus, not malformed input.

### Treat Front And Back Matter As Ignorable

Rejected. They should not appear in body plaintext, but ignoring them would make
the publication conversion incomplete. They need typed omission/preservation
policy.

### Let Parser Consensus Decide Front/Back Regions

Rejected. Parser output is supporting evidence. Source region classification is
source-authoritative because front/back matter is exactly where parsers often
drop or normalize information.

## Self-Review

- Trust boundary: source recognition and region classification are measured by
  ab-validator; ABC owns publication placement.
- Values vs places: raw marker, source region, apparatus kind, and publication
  disposition are separate values, not overloaded counters.
- Plaintext policy: plaintext remains visible body text only.
- Completion honesty: full mapping cannot hide source apparatus under malformed
  or out-of-body counters.
