# Soranoha TEI extension vocabulary

Namespace IRI: `https://w3id.org/soranoha/ns/tei`
Conventional prefix: `snh:`
Current vocabulary version: `0`

Soranoha publishes TEI P5. Where the source markup carries information TEI has
no element for, that information is recorded as an attribute in this namespace
rather than by inventing an element. Every published TEI root binds the
namespace and declares the vocabulary version:

```xml
<TEI xmlns="http://www.tei-c.org/ns/1.0"
     xmlns:snh="https://w3id.org/soranoha/ns/tei"
     snh:vocab-version="0">
```

These attributes are derived audit projections. They record what the parser
established about the Aozora source; they do not replace the source records
from which they were derived, and a consumer may ignore them without losing the
transcription.

## Versioning

The namespace IRI identifies the extension and never changes. The vocabulary's
major version is not implied by that IRI, which is why `snh:vocab-version`
carries it separately: a breaking vocabulary change increments it, while an
additive attribute may keep the same major version when compatibility is
documented.

The profile hashes recorded in a work's `tei-validation.json` — `odd_hash`,
`rng_hash`, `schematron_hash` — answer a different question. They change on any
profile edit, additive ones included, so they say which profile validated a
document but cannot tell you whether an existing `snh:*` reader is still
correct. That is what the vocabulary version is for.

Version `0` is deliberately attributes-only. Custom elements in this namespace
are not part of it.

## Attributes

All are optional and may appear on any element, subject to the constraints
below. Only the first three are emitted by the current pipeline; the rest are
declared for projections that are specified but not yet produced.

| Attribute | Value | Meaning |
|---|---|---|
| `snh:vocab-version` | `0` | Vocabulary major version. Required on the root of any document carrying `snh:*` attributes. |
| `snh:layout-kind` | token, or space-separated tokens when scopes co-apply | Layout fact projected verbatim from the parser IR. Paragraph layouts are `burasage`, `chitsuki`, `jisage`, `jizume`, `line-jisage`; inline scopes are `baseline-position`, `chitsuki`, `emphasis`, `exponent`, `font-size`, `fraction`, `keigakomi`, `small-script`, `tcy`, `yokogumi`. |
| `snh:layout-params` | `key=value` pairs joined by `;` | Parameters of the layout fact, for example `indent=2` or `align=right;offset-from-end=1`. |
| `snh:preservation-record` | `r` followed by six digits | Record id in the publication-preservation sidecar. |
| `snh:ir-pointer` | JSON Pointer | Location in the parser IR of the projected fact. |
| `snh:source-pointer` | pointer string | Location in the source or AAT of the projected fact. |
| `snh:source-marker` | string | Original Aozora marker, kept when a TEI-native rendering normalizes it away. |
| `snh:accent-code` | string | Aozora accent notation code. |
| `snh:style-source` | string | Aozora style source marker. |
| `snh:figure-class` | string | Aozora figure class. |

Permitted values of `snh:vocab-version` are declared once, in the attribute's
value list in [the ODD](../schemas/tei-profile.odd), and the generated Relax NG
enforces them. Admitting a new version is one addition to that list; older
documents stay valid because the list grows rather than being replaced.

## Schematron rule identifiers

Constraint violations are published in each work's `tei-validation.json` as
`findings[].rule_id`. The identifiers are stable names, not sequence numbers.

| Rule ID | Severity | Requires |
|---|---|---|
| `snh-tei-header-title` | error | A main title in `teiHeader/fileDesc/titleStmt`. |
| `snh-tei-header-source-work-id` | error | An Aozora work ID or source work identifier in the header. |
| `snh-header-language-declared` | error | At least one `profileDesc/langUsage/language` with a non-empty `@ident`. |
| `snh-ruby-complete` | error | Ruby structures contain both base and reading components. |
| `snh-ruby-base-non-empty` | error | A ruby base contains text or a declared character. |
| `snh-ruby-reading-non-empty` | error | A ruby reading is not empty. |
| `snh-gaiji-reference` | error | Gaiji retains its original marker and either a resolved replacement or unresolved status. |
| `snh-gaiji-chardecl-resolution` | error | A gaiji `@ref` fragment resolves to a `charDecl/char` in the same document. |
| `snh-char-resolution-form` | error | A `charDecl/char` declares at least one of `mapping`, `unicodeProp`, `localProp`, `desc`. |
| `snh-figure-accessibility` | warning | Figures preserve a nonempty description or caption reference where available. |
| `snh-source-span-reference` | error | Elements claiming source spans point to valid span identifiers. |
| `snh-source-span-target-exists` | error | Each fragment id in `@source` resolves to an `@xml:id` in the same document. |
| `snh-transcription-vs-annotation` | warning | Linguistic enrichment is not mixed into transcription unless declared in the header. |
| `snh-vocab-version-declared` | error | A root carrying `snh:*` attributes declares `snh:vocab-version`. |
| `snh-preservation-record-shape` | error | `snh:preservation-record` values have the form `r000000`. |
| `snh-layout-params-shape` | error | `snh:layout-params` uses the `key=value` grammar above. |

A schema pass does not establish source fidelity or publication rights. See
[TEI validation](tei-validation.md) for how the two validation layers run, and
[source accountability](source-accountability.md) for the separate fidelity
evidence.

## Provenance of this document

The vocabulary is defined by [`soranoha/schemas/tei-profile.odd`](../schemas/tei-profile.odd),
from which the Relax NG and Schematron are generated. Where this page and the
ODD disagree, the ODD is authoritative.
