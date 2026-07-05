# ABC TEI Namespace Extension Design

Date: 2026-07-06

## Status

Draft design approved for planning.

## Problem

ABC can now publish parser-IR into TEI P5 plus a JSON preservation sidecar, and the current ab-validator coverage report shows no true unsupported parser-IR gaps. The remaining publication closure gaps are policy gaps:

- accent: 14 rules
- figure_metadata: 85 rules
- heading_jisage_structure: 7 rules
- style_rendition: 14 rules

These are not evidence that Aozora Bunko markup cannot be represented. They are evidence that the current ABC TEI profile does not yet declare how the renderer records source-exact Aozora presentation facts in XML when TEI P5 has no exact native slot, or when TEI's native slot is intentionally policy-normalized.

The goal is to map all parser-IR into either TEI P5 or an explicit ABC custom contract. Plain TEI levels 2 and 3 are useful milestones, but they are not the exact target.

## Decision

Add an ABC XML namespace extension to the ABC TEI profile:

```xml
xmlns:abc="https://w3id.org/abc/ns/tei"
```

The extension is attributes-only for v0. It adds `abc:*` attributes to ordinary TEI P5 elements. It does not add custom `abc:*` elements to the TEI body.

TEI P5 remains the publication structure. The JSON preservation sidecar remains the authoritative preservation ledger for facts that are not fully represented in TEI. ABC namespaced TEI attributes make the generated XML self-describing and queryable for source-exact Aozora facts that TEI policy projection would otherwise hide.

## Non-Goals

- Do not replace the JSON preservation sidecar.
- Do not encode TEI level 4 or 5 semantic enrichment from Aozora markup alone.
- Do not put ruby readings, style metadata, accent codes, or source metadata into plaintext output.
- Do not admit parser failures as TEI policy. Parser and adapter fidelity remain separate evidence concerns.
- Do not add custom `abc:*` body elements in v0.

## Namespace Vocabulary

The v0 namespace vocabulary is deliberately small:

| Attribute | Value | Applies To | Purpose |
| --- | --- | --- | --- |
| `abc:ir-pointer` | parser-IR pointer string | any generated TEI element | Trace TEI output to parser-IR field or node when useful for audit. |
| `abc:source-pointer` | AAT/source pointer string | any generated TEI element | Trace TEI output to source-derived AAT field when the mapping is policy-normalized. |
| `abc:source-marker` | original Aozora marker or compact marker token | any generated TEI element | Preserve exact source marker identity when TEI `@rend` is normalized. |
| `abc:accent-code` | source accent code string | `tei:hi`, `tei:seg`, `tei:span` | Preserve Aozora accent code beyond generic emphasis. |
| `abc:style-source` | Aozora style token or marker name | `tei:hi`, `tei:head`, `tei:p`, `tei:seg` | Preserve exact source style when rendered through TEI `@rend` or `@rendition`. |
| `abc:layout-kind` | normalized parser-IR layout kind | `tei:p`, `tei:seg`, `tei:head`, `tei:div` | Preserve layout class such as jisage, burasage, chitsuki, jizume, tcy, yokogumi, or keigakomi. |
| `abc:layout-value` | compact key/value layout payload | `tei:p`, `tei:seg`, `tei:head`, `tei:div` | Preserve layout parameters when TEI `@rend` is intentionally human-oriented. |
| `abc:figure-class` | source figure class token | `tei:figure`, `tei:graphic` | Preserve Aozora figure CSS/class identity. |

All values are strings in v0. Attribute semantics are constrained by Schematron and documented vocabulary tables, not by a separate custom XML schema language. Values that need richer structure remain in `preservation.json`.

## Family Mapping

### Accent

Parser-IR accent-derived emphasis renders as TEI inline emphasis:

```xml
<hi rend="accent" abc:accent-code="accent-acute" abc:source-pointer="blocks[].content[].accent">Cafe</hi>
```

The TEI `@rend` value carries the publication-facing policy classification. `abc:accent-code` carries the source-exact Aozora code. The JSON preservation sidecar remains the source of truth for full rule id, category, count, and first path.

This admits the `accent` family through the `tei_plus_abc_extension` lane.

### Style Rendition

Parser-IR style emphasis renders with TEI `hi`, `head`, `p`, or `seg` plus TEI `@rend` or `@rendition`:

```xml
<hi rend="bold" abc:style-source="太字" abc:source-pointer="blocks[].content[].style">強調</hi>
```

When the parser-IR style already maps directly to a TEI rendition token, `abc:style-source` is optional. It is required when the source marker and TEI rendition differ or when the source marker is otherwise lost.

Heading style losses use `abc:style-source` on `tei:head`, not a custom element.

This admits the `style_rendition` family through the `tei_policy_projection` lane.

### Figure Metadata

Figures render as TEI `figure` and `graphic`:

```xml
<figure abc:figure-class="fig-large">
  <graphic url="images/0001.png" width="320px" height="240px"/>
</figure>
```

Use TEI `@width` and `@height` on `graphic` when source dimensions are available and can be emitted as valid TEI attribute values. Use `abc:figure-class` for exact Aozora figure class/CSS identity. Use `@rend` for renderer-facing normalized figure display policy when appropriate.

Deeply nested source figure occurrences that are represented in parser-IR as visible text projection remain backed by `preservation.json`; the XML attribute extension admits only the TEI-visible figure/class/dimension policy, not a claim that nested source structure has been reconstructed.

This admits the `figure_metadata` family when both conditions hold:

- TEI output carries dimensions/classes where parser-IR exposes them.
- `preservation.json` records residual source-exact figure facts not reconstructable in TEI.

### Heading and Jisage Structure

Headings render as TEI `head`; indentation and block layout render as TEI `p`, `seg`, or structural wrappers with TEI `@rend` plus ABC layout attributes:

```xml
<p rend="jisage indent(2)" abc:layout-kind="jisage" abc:layout-value="indent=2">字下げ本文</p>
```

The normalized `@rend` value is for TEI consumers and human inspection. The `abc:layout-kind` and `abc:layout-value` values are the stable ABC audit vocabulary.

This admits the `heading_jisage_structure` family through the `tei_policy_projection` lane after the ABC profile and renderer tests show that heading and jisage nodes have a declared TEI projection.

## Renderer Policy

The ABC TEI renderer should follow this order:

1. Emit standard TEI elements and standard TEI attributes whenever a direct TEI representation exists.
2. Add `abc:*` attributes only when TEI output needs source-exact audit facts that would otherwise be lost or normalized.
3. Record full provenance, rule ids, counts, and source values in `preservation.json`.
4. Omit all metadata from plaintext except visible base text explicitly admitted by plaintext policy.

The extension must not turn `@rend` into an unstructured dump. `@rend` remains publication-facing. `abc:*` attributes carry ABC source and parser-IR audit facts.

## Profile Contract

ABC owns the TEI profile and namespace declaration. The profile should:

- Declare the `abc` namespace in the ODD and generated schema artifacts.
- Permit the v0 `abc:*` attributes on the selected TEI elements.
- Add Schematron checks for required source-exact attributes where policy requires them.
- Keep existing ruby, gaiji, source-span, and header checks intact.
- Preserve TEI P5 validity for consumers that tolerate foreign namespaced attributes.

The profile hash changes when the ODD changes. Existing TEI artifacts remain tied to the old profile hash. New artifacts with the ABC namespace extension carry the new profile identity.

## Coverage Gate

ab-validator should add an admission bucket distinct from the existing custom JSON sidecar bucket:

```json
"admitted_by_tei_profile": {
  "count": 120,
  "counts_by_family": {
    "accent": 14,
    "figure_metadata": 85,
    "heading_jisage_structure": 7,
    "style_rendition": 14
  }
}
```

A row can move into `admitted_by_tei_profile` only when all relevant conditions are true:

- ABC TEI profile declares the namespace and permits the attribute vocabulary.
- ABC renderer emits the expected TEI element and `abc:*` attributes for a fixture covering the family.
- ABC validation accepts the generated TEI under the updated profile.
- The JSON preservation sidecar remains valid for source-exact facts that the TEI attributes summarize rather than fully preserve.

The coverage report must continue to distinguish:

- TEI-native projection
- TEI plus ABC namespace extension
- JSON custom preservation sidecar
- True unsupported gaps

## Testing

ABC-side tests should cover:

- ODD/profile validation of representative `abc:*` attributes.
- TEI rendering for one fixture per family: accent, style rendition, figure metadata, heading/jisage structure.
- Plaintext output remains metadata-free.
- Design bundle validation accepts the updated TEI plus preservation sidecar.

ab-validator-side tests should cover:

- Coverage report recognizes the new ABC TEI namespace/profile evidence.
- The 120 policy rows move out of `classified_but_not_admitted` only when profile and renderer evidence is present.
- Existing `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION` behavior remains unchanged for JSON-only preservation rows.

## Rejected Alternatives

### Custom ABC Elements in TEI

Rejected for v0. Custom elements would make the XML extension more explicit, but they increase schema complexity and reduce compatibility with normal TEI consumers. The current gaps are facts about rendition, source pointers, figure metadata, and layout parameters; attributes on TEI elements are enough.

### TEI-Only Projection With No ABC Namespace

Rejected. TEI-only projection hides source-exact Aozora details inside policy-normalized `@rend` strings or forces every audit query to join against the JSON sidecar. That is weaker than the goal: map all IR into TEI or a custom schema with an explicit contract.

### JSON Sidecar Only

Rejected as the sole solution. The preservation sidecar is still necessary, but XML consumers should be able to see that a TEI element is carrying an ABC-admitted source/layout/style fact without opening a separate JSON file.

## Implementation Sequence

1. ABC updates the TEI ODD/profile to declare `abc` and permit the v0 attributes.
2. ABC renderer emits the attributes for one fixture per remaining family.
3. ABC validation and materialization prove profile/schema compatibility.
4. ab-validator syncs the updated ABC profile evidence and teaches publication coverage to admit rows through `admitted_by_tei_profile`.
5. ab-validator regenerates the IR publication coverage report and verifies `classified_but_not_admitted.count == 0` for the measured policy rows.

## Self-Review

- Completion scan: this spec has no incomplete markers or empty sections.
- Scope: the design is limited to an attributes-only ABC namespace extension and coverage admission.
- Boundary: ABC owns TEI profile/rendering; ab-validator owns measurement and admission reporting.
- Plaintext: explicitly remains metadata-free.
- Sidecar: preserved as authoritative provenance, not replaced by XML attributes.
