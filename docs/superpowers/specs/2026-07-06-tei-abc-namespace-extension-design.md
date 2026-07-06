# ABC TEI Namespace Extension Design

Date: 2026-07-06

## Status

Draft design revised after review.

## Problem

ABC can now publish parser-IR into TEI P5 plus a JSON preservation sidecar, and the current ab-validator coverage report shows no true unsupported parser-IR gaps. The latest report has three closure buckets:

- `admitted_by_custom_contract`: 31 rows
- `classified_but_not_admitted`: 120 rows
- `true_unsupported_gaps`: 0 rows

This design covers the remaining `classified_but_not_admitted` policy rows:

- accent: 14 rules
- figure_metadata: 85 rules
- heading_jisage_structure: 7 rules
- style_rendition: 14 rules

The companion gap-closure design originally grouped eleven publication gap families. The custom-sidecar families are already admitted by the ABC preservation contract in the current report, and parser-IR/schema-delta families are outside this TEI namespace design. This spec is therefore not the full closure contract. It is the TEI-profile admission contract for the current 120 policy rows.

These 120 rows are not evidence that Aozora Bunko markup cannot be represented. They are evidence that the current ABC TEI profile does not yet declare how the renderer records source-exact Aozora presentation facts in XML when TEI P5 has no exact native slot, or when TEI's native slot is intentionally policy-normalized.

The goal is to map all parser-IR into either TEI P5 or an explicit ABC custom contract. Plain TEI levels 2 and 3 are useful milestones, but they are not the exact target.

## Decision

Add an ABC XML namespace extension to the ABC TEI profile:

```xml
<TEI xmlns="http://www.tei-c.org/ns/1.0"
     xmlns:abc="https://w3id.org/abc/ns/tei"
     abc:vocab-version="0">
```

The extension is attributes-only for v0. It adds `abc:*` attributes to ordinary TEI P5 elements. It does not add custom `abc:*` elements to the TEI body.

TEI P5 remains the publication structure. The JSON preservation sidecar remains the authoritative preservation ledger for duplicated audit facts. ABC namespaced TEI attributes are derived projections from parser-IR and preservation evidence. They make renderer-asserted source facts visible and queryable in XML, but they do not replace the sidecar as the audit authority.

## Non-Goals

- Do not replace the JSON preservation sidecar or create a second independent source of truth.
- Do not encode TEI level 4 or 5 semantic enrichment from Aozora markup alone.
- Do not put ruby readings, style metadata, accent codes, or source metadata into plaintext output.
- Do not admit parser failures as TEI policy. Parser and adapter fidelity remain separate evidence concerns.
- Do not add custom `abc:*` body elements in v0.
- Do not close custom-sidecar, parser-IR schema-delta, or converter-delta families in this namespace design.

## Namespace Vocabulary

The v0 namespace vocabulary is deliberately small:

| Attribute | Value | Applies To | Purpose |
| --- | --- | --- | --- |
| `abc:vocab-version` | `"0"` | `tei:TEI` | Pin the ABC TEI extension vocabulary major version. |
| `abc:preservation-record` | sidecar record id such as `r000042` | elements listed in the admission matrix | Join TEI output to `preservation.json.records[].record_id`. |
| `abc:ir-pointer` | folded parser-IR pointer string | elements listed in the admission matrix | Trace TEI output to parser-IR field or node when useful for audit. |
| `abc:source-pointer` | folded AAT/source pointer string | elements listed in the admission matrix | Trace TEI output to source-derived AAT field when the mapping is policy-normalized. |
| `abc:source-marker` | original Aozora marker or compact marker token | elements listed in the admission matrix | Preserve exact source marker identity when TEI `@rend` is normalized. |
| `abc:accent-code` | source accent code string | `tei:hi`, `tei:seg`, `tei:span` | Preserve Aozora accent code beyond generic emphasis. |
| `abc:style-source` | Aozora style token or marker name | `tei:hi`, `tei:head`, `tei:p`, `tei:seg` | Preserve exact source style when rendered through TEI `@rend` or `@rendition`. |
| `abc:layout-kind` | normalized parser-IR layout kind | `tei:p`, `tei:seg`, `tei:head`, `tei:div` | Preserve layout class such as jisage, burasage, chitsuki, jizume, tcy, yokogumi, or keigakomi. |
| `abc:layout-params` | closed key/value payload | `tei:p`, `tei:seg`, `tei:head`, `tei:div` | Preserve layout parameters when TEI `@rend` is intentionally human-oriented. |
| `abc:figure-class` | source figure class token | `tei:figure`, `tei:graphic` | Preserve Aozora figure CSS/class identity. |

All values are strings in v0. Attribute semantics are constrained by Schematron and documented vocabulary tables, not by a separate custom XML schema language. Values that need richer structure remain in `preservation.json`.

### Pointer and Join Grammar

`abc:preservation-record` is the only XML-to-sidecar join key in v0. Its value must match the preservation sidecar record id grammar:

```text
record-id = "r" 6DIGIT
```

`abc:ir-pointer` and `abc:source-pointer` are folded audit pointers, not join keys. They use the same folded pointer convention as the mapping and preservation reports:

```text
pointer       = segment *("." segment)
segment       = name / name "[]" / pseudo
name          = ALPHA *(ALPHA / DIGIT / "_" / "-")
pseudo        = "(" 1*(ALPHA / DIGIT / "_" / "-" / "." ) ")"
```

Pointers are intentionally index-free. If a consumer needs exact record identity, it must use `abc:preservation-record`.

### Layout Parameter Grammar

`abc:layout-params` is not free-form prose. It uses this closed payload grammar:

```text
layout-params = pair *(";" pair)
pair          = key "=" value
key           = "indent" / "first-line-indent" / "continuation-indent" /
                "align" / "offset-from-end" / "width" / "marker" / "border"
value         = 1*(ALPHA / DIGIT / "_" / "-" / "." / ":" / "#")
```

Allowed keys are tied to `abc:layout-kind`. For example, `jisage` may use `indent`, `burasage` may use `first-line-indent` and `continuation-indent`, and `keigakomi` may use `border`. Richer layout records stay in `preservation.json`.

## Family Mapping

### Accent

Parser-IR accent-derived emphasis renders as TEI inline emphasis:

```xml
<hi rend="accent"
    abc:accent-code="accent-acute"
    abc:source-pointer="blocks[].content[].accent"
    abc:preservation-record="r000042">Cafe</hi>
```

The TEI `@rend` value carries the publication-facing policy classification. `abc:accent-code` carries the source-exact Aozora code. The JSON preservation sidecar remains the source of truth for full rule id, category, count, and first path.

This admits the `accent` family through the `tei_plus_abc_extension` lane.

### Style Rendition

Parser-IR style emphasis renders with TEI `hi`, `head`, `p`, or `seg` plus TEI `@rend` or `@rendition`:

```xml
<hi rend="bold"
    abc:style-source="太字"
    abc:source-pointer="blocks[].content[].style"
    abc:preservation-record="r000043">強調</hi>
```

When the parser-IR style already maps directly to a TEI rendition token, `abc:style-source` is optional. It is required when the source marker and TEI rendition differ or when the source marker is otherwise lost.

Heading style losses use `abc:style-source` on `tei:head`, not a custom element.

This admits the `style_rendition` family through the `tei_policy_projection` lane.

### Figure Metadata

Figures render as TEI `figure` and `graphic`:

```xml
<figure abc:figure-class="fig-large" abc:preservation-record="r000044">
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
<p rend="jisage indent(2)"
   abc:layout-kind="jisage"
   abc:layout-params="indent=2"
   abc:preservation-record="r000045">字下げ本文</p>
```

The normalized `@rend` value is for TEI consumers and human inspection. The `abc:layout-kind` and `abc:layout-params` values are the stable ABC audit vocabulary.

This admits the `heading_jisage_structure` family through the `tei_policy_projection` lane after the ABC profile and renderer tests show that heading and jisage nodes have a declared TEI projection.

## Authority and Reconciliation

`abc:*` attributes are derived projections. They are never the independent source of truth for an audit fact that also appears in `preservation.json`.

The reconciliation contract is:

1. The renderer may emit `abc:*` attributes only from parser-IR fields or preservation records it has already constructed.
2. Any TEI element with source-exact `abc:*` attributes must include `abc:preservation-record` when a corresponding preservation record exists.
3. `abc:preservation-record` must resolve to exactly one `preservation.json.records[].record_id`.
4. If an inline attribute duplicates a sidecar value, the values must match after documented normalization.
5. If XML and sidecar disagree, the sidecar wins and validation must fail.

Schematron can enforce XML-local shape rules. Cross-file reconciliation between TEI and `preservation.json` belongs in ABC design-bundle validation and ab-validator coverage checks.

The current ABC preservation sidecar schema version `0.1.0` admits `custom_sidecar` records. This namespace design requires a schema-compatible widening, such as a `tei_profile_projection` record class or an equivalent profile-evidence section, before `abc:preservation-record` can be mandatory for TEI-profile rows. Until that widening exists, the coverage gate must not claim sidecar-reconciled XML attributes.

## Admission Matrix

The profile gate is this matrix, not the mere existence of an `abc` namespace.

| Family | TEI target | Required `abc:*` attributes | XML-local rule | Cross-file rule |
| --- | --- | --- | --- | --- |
| `accent` | `tei:hi`, `tei:seg`, or `tei:span` with `@rend` containing `accent` | `abc:accent-code`, `abc:preservation-record` | `abc:accent-code` is non-empty and `abc:preservation-record` matches `r[0-9]{6}`. | The sidecar record resolves and carries the same accent code after documented normalization. |
| `style_rendition` | `tei:hi`, `tei:head`, `tei:p`, or `tei:seg` with policy-normalized `@rend` / `@rendition` | `abc:style-source` when source marker differs from TEI rendition; `abc:preservation-record` when exact source style is sidecar-backed | If `abc:style-source` is present, it is non-empty. If `abc:preservation-record` is present, it matches `r[0-9]{6}`. | Any duplicated style source value equals the linked sidecar value. |
| `figure_metadata` | `tei:figure` / `tei:graphic` | `abc:figure-class` when source class exists; `abc:preservation-record` when class or residual nested figure metadata is sidecar-backed | `abc:figure-class` is non-empty. `tei:graphic @width/@height` use valid TEI dimension strings when emitted. | Linked sidecar preserves exact source class and residual figure metadata not carried by TEI. |
| `heading_jisage_structure` | `tei:head`, `tei:p`, `tei:seg`, or `tei:div` | `abc:layout-kind`; `abc:layout-params` for parameterized layouts; `abc:preservation-record` when exact source layout is sidecar-backed | `abc:layout-kind` is one declared layout token. `abc:layout-params` matches the closed grammar and uses keys allowed for that layout kind. | Linked sidecar preserves exact source marker and layout parameters where TEI `@rend` is normalized. |

`abc:ir-pointer`, `abc:source-pointer`, and `abc:source-marker` are optional annotations unless a future row-specific gate requires them. They do not replace `abc:preservation-record`.

The `abc:layout-kind` vocabulary includes `font-size`, `tcy`, `keigakomi`, and `yokogumi` because those are valid parser-IR layout concepts. They are not counted in the current 120-row namespace admission bucket unless they appear in the current `classified_but_not_admitted` report.

## Renderer Policy

The ABC TEI renderer should follow this order:

1. Emit standard TEI elements and standard TEI attributes whenever a direct TEI representation exists.
2. Add `abc:*` attributes only when TEI output needs source-exact audit facts that would otherwise be lost or normalized.
3. Add `abc:preservation-record` whenever the XML attribute summarizes a sidecar-backed source-exact fact.
4. Record full provenance, rule ids, counts, and source values in `preservation.json`.
5. Omit all metadata from plaintext except visible base text explicitly admitted by plaintext policy.

The extension must not turn `@rend` into an unstructured dump. `@rend` remains publication-facing. `abc:*` attributes carry ABC source and parser-IR audit facts.

The renderer is not the sole fidelity oracle. ABC renderer tests prove that the projection shape exists. ab-validator admission should independently sample reconstructable source facts, such as source markers or accent codes, against source inventory evidence before claiming source-exact XML fidelity for a family. If independent sampling is not possible for a family, the report must label the XML attributes as renderer-asserted projections and rely on `preservation.json` for authoritative audit evidence.

## Profile Contract

ABC owns the TEI profile and namespace declaration. The profile should:

- Declare the `abc` namespace in the ODD and generated schema artifacts.
- Require `abc:vocab-version="0"` on generated `tei:TEI` roots that use the extension.
- Permit the v0 `abc:*` attributes on the selected TEI elements.
- Add Schematron checks for required source-exact attributes where policy requires them.
- Add Schematron checks for `abc:preservation-record` and `abc:layout-params` lexical shape.
- Keep existing ruby, gaiji, source-span, and header checks intact.
- Preserve validity under the ABC customized TEI profile. Strict validators that use an unrelated TEI schema may reject the declared foreign attributes; that is an interoperability caveat, not an ABC profile failure.

The profile hash changes when the ODD changes. Existing TEI artifacts remain tied to the old profile hash. New artifacts with the ABC namespace extension carry the new profile identity.

The namespace URI is stable across v0 documents, but the vocabulary major version is not implicit in the URI. Breaking vocabulary changes must increment `abc:vocab-version`; additive non-breaking attributes may keep the same major version if the ABC profile hash changes and compatibility is documented.

## Coverage Gate

ab-validator should add an admission bucket distinct from the existing custom JSON sidecar bucket. The counts below are derived from `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` after the ABC preservation sidecar admitted 31 custom-contract rows:

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

These counts intentionally differ from the earlier eleven-family gap-closure design table, which described the pre-admission design problem. The current generated report is the source of truth for implementation.

A row can move into `admitted_by_tei_profile` only when all relevant conditions are true:

- ABC TEI profile declares the namespace and permits the attribute vocabulary.
- ABC renderer emits the expected TEI element and `abc:*` attributes for a fixture covering the family.
- ABC profile enforces the Admission Matrix's XML-local rules.
- ABC validation accepts the generated TEI under the updated profile.
- The JSON preservation sidecar, or its widened successor, remains valid for source-exact facts that the TEI attributes summarize rather than fully preserve.
- Cross-file validation confirms `abc:preservation-record` values resolve to sidecar records.
- ab-validator independently samples reconstructable source facts where possible, or labels the family as renderer-asserted when no independent source check exists.

The coverage report must continue to distinguish:

- TEI-native projection
- TEI plus ABC namespace extension
- JSON custom preservation sidecar
- True unsupported gaps

The implementation target for this spec is that these 120 rows move from `classified_but_not_admitted` to `admitted_by_tei_profile`. If no new rows are introduced, that also makes `classified_but_not_admitted.count == 0` in the current report. If sibling work introduces or reclassifies other rows, completion is measured by the four-family delta, not by pretending this namespace design owns unrelated rows.

## Testing

ABC-side tests should cover:

- ODD/profile validation of representative `abc:*` attributes.
- TEI rendering for one fixture per family: accent, style rendition, figure metadata, heading/jisage structure.
- Cross-file validation that `abc:preservation-record` resolves into `preservation.json`.
- Failure cases for missing required attributes from the Admission Matrix.
- Plaintext output remains metadata-free.
- Design bundle validation accepts the updated TEI plus preservation sidecar.

ab-validator-side tests should cover:

- Coverage report recognizes the new ABC TEI namespace/profile evidence.
- The 120 policy rows move out of `classified_but_not_admitted` only when profile and renderer evidence is present.
- The coverage gate rejects unresolved `abc:preservation-record` joins.
- Source-authority sampling checks reconstructable source facts where available.
- Existing `CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION` behavior remains unchanged for JSON-only preservation rows.

## Rejected Alternatives

### Custom ABC Elements in TEI

Rejected for v0. Custom elements would make the XML extension more explicit, but they disrupt downstream XPath/XSLT that matches normal TEI element names and create a larger ODD/RNG surface. Foreign attributes declared by the ABC customized profile compose better with standard TEI element structure. The current gaps are facts about rendition, source pointers, figure metadata, and layout parameters; attributes on TEI elements are enough.

### TEI-Only Projection With No ABC Namespace

Rejected. TEI-only projection hides source-exact Aozora details inside policy-normalized `@rend` strings or forces every audit query to join against the JSON sidecar. That is weaker than the goal: map all IR into TEI or a custom schema with an explicit contract.

### JSON Sidecar Only

Rejected as the sole solution. The preservation sidecar is still necessary, but XML consumers should be able to see that a TEI element is carrying an ABC-admitted source/layout/style fact without opening a separate JSON file.

## Implementation Sequence

1. ABC updates the TEI ODD/profile to declare `abc`, require `abc:vocab-version="0"`, and permit the v0 attributes.
2. ABC widens the preservation sidecar contract to record TEI-profile projection evidence or an equivalent joinable profile-evidence section.
3. ABC renderer emits the attributes for one fixture per covered family.
4. ABC validation and materialization prove profile/schema compatibility, missing-attribute failures, and TEI-to-sidecar record resolution.
5. ab-validator syncs the updated ABC profile evidence and teaches publication coverage to admit rows through `admitted_by_tei_profile`.
6. ab-validator regenerates the IR publication coverage report and verifies the four covered families move from `classified_but_not_admitted` to `admitted_by_tei_profile`.

## Self-Review

- Completion scan: this spec has no incomplete markers or empty sections.
- Scope: the design is limited to an attributes-only ABC namespace extension and coverage admission.
- Boundary: ABC owns TEI profile/rendering; ab-validator owns measurement and admission reporting.
- Plaintext: explicitly remains metadata-free.
- Sidecar: preserved as authoritative provenance, not replaced by XML attributes.
