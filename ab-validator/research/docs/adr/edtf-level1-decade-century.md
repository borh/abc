# EDTF Level 1 — Decade Markers and BCE Century Prose

## Implementation Status

Repository history establishes Accepted status by import and the 2026-07-09
evidence backfill, while no distinct acceptance date survives, so the decision
date is used. The live contracts are `schemas/person-record.schema.json`,
`abc.tools.aozora-csv/parse-date`, and `abc.tools.person-record/record->graph`;
`test/abc/tools/aozora_csv_test.clj` and
`test/abc/tools/person_record_test.clj` cover them.

## Context

ADR 0015 admitted only EDTF Level 0 lexical shapes (`YYYY[-MM[-DD]]`,
optional leading `-` for BCE) into the v0 person-record grammar and
explicitly deferred Level 1 (`?`, `~`, `%`, `XX` digit placeholders,
intervals, sets) to a future ADR. End-to-end corpus ingest of
`list_person_all_extended_utf8.csv` revealed that two corpus persons
carry temporal values that are real, not unknown, but below Level 0
precision:

| `person_id` | name | raw | shape |
| --- | --- | --- | --- |
| 001713 | サッフォ | `紀元前7世紀末` (birth) | Japanese BCE century prose, end-of-century |
| 001713 | サッフォ | `紀元前6世紀初` (death) | Japanese BCE century prose, start-of-century |
| 001919 | シェミン マーガレット | `192X` (birth) | EDTF Level 1 decade marker |

ADR 0015's tolerance behavior (`run-corpus!` skip-on-validate-fail)
keeps the ingest from crashing on these, but at the cost of dropping
two known historical figures from the corpus. The `不詳` /
`unknown-marker` route is not the right home for them either: ADR
0015's hard rule designates the JSON record's EDTF string as the
*temporal source of truth*, and these values carry temporal
information at coarser precision — they are not knowledge gaps. The
current "fail and audit" behavior was designed as a deliberate signal
that a Level 1 ADR was needed; that signal has now been received.

## Decision

**Widen the v0 lexical grammar.** `nullableDate` accepts the union of
ADR 0015's Level 0 grammar plus two Level 1 shapes:

```
^-?\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$  -- L0 (ADR 0015)
^-?\d{3}X$                                                 -- decade (L1)
^-?\d{2}XX$                                                -- century (L1)
```

with `null` still permitted. The decade and century shapes use the
EDTF Level 1 capital-`X` digit placeholder. They have no XSD
precision-typed equivalent (XSD's coarsest temporal type is
`xsd:gYear`), so the RDF view treats them differently from Level 0
shapes — see "RDF emission" below.

**BCE century-prose translation.** Aozora's CSV uses Japanese
natural-language century prose for ancient dates. The parser
recognizes the family

```
紀元前N世紀(初(頭)?|末|半ば|前半|後半)?
```

and translates it to the corresponding EDTF century marker under
ADR 0015's astronomical year numbering. The mapping is

> **N BCE century → `-{N-1:02d}XX`**

(zero-padded to two digits for `N` ≤ 100; wider for earlier
centuries, though the corpus does not exercise those). This is
**lossless at century precision**: 紀元前N世紀 covers conventional
years `(100N)` BCE through `((N-1)·100 + 1)` BCE, which under
astronomical year numbering is the closed interval
`[-(100N - 1), -((N-1)·100)]`. EDTF `-{N-1:02d}XX` lexically matches
exactly that interval (e.g. `-06XX` matches `-0699` through `-0600`).

| raw | astronomical interval | EDTF lexical |
| --- | --- | --- |
| `紀元前1世紀` | `-0099`..`0000` | `-00XX` |
| `紀元前6世紀` | `-0599`..`-0500` | `-05XX` |
| `紀元前7世紀` | `-0699`..`-0600` | `-06XX` |
| `紀元前N世紀` | `-(100N-1)`..`-((N-1)·100)` | `-{N-1:02d}XX` |

The qualifier (`初`, `末`, `半ば`, `前半`, `後半`) is **not encoded
in EDTF Level 1**: there is no sub-century precision in the standard.
The qualifier is preserved verbatim in the `parse_corrections`
audit trail under rule `century-prose` (raw = the full Japanese
phrase, corrected = the EDTF century marker). A future ADR layering
EDTF Level 2 / OWL-Time `time:ProperInterval` may re-mine these
audit entries to refine `紀元前7世紀末` into a sub-century range
(roughly `-0610`..`-0600`) without invalidating canonical identity,
because the canonical EDTF string and the audit trail together carry
all the information present in the source.

**CE century prose is out of scope for v0.1.** Conventional Western
century numbering (1st century CE = years 1..100, 7th century CE =
601..700) does not align with EDTF century markers (`06XX` covers
years 0600..0699 — splitting the 7th conventional century at a year
boundary). The Aozora corpus does not currently contain CE century
prose for persons, so v0.1 only translates `紀元前N世紀…`. If CE
prose appears later, a future ADR must decide between (a) accepting a
1-year alignment loss, (b) introducing intervals, or (c) a calendar-
specific encoding. v0.1's parser leaves CE prose unparsed; downstream
schema validation rejects.

**Decade markers.** EDTF Level 1 admits `\d{3}X` directly (`192X`
denotes "any year in 1920–1929"). The parser does **not** synthesize
or translate decade markers; it admits them verbatim from the source.
ADR 0016 simply widens the schema/SHACL to recognize the lexical
shape. No new parse rule is introduced for decade — the audit trail
records nothing because there is no transformation. (Japanese
decade prose like `1920年代` is not in the corpus and is deferred.)

**RDF emission for Level 1 shapes.** XSD has no datatype for decade
or century precision. Per ADR 0015's principle that the EDTF echo
is the precision-honest carrier and the XSD literal is a precision-
typed view, decade and century shapes are emitted **only** as
`abc:edtfDateOfBirth` / `abc:edtfDateOfDeath` typed `abc:EDTF`. The
RDA Group 2 predicates (`rdag2:dateOfBirth`, `rdag2:dateOfDeath`)
are simply omitted from the graph for these records — the SHACL
shape's `sh:maxCount 1` is satisfied by zero, and the EDTF echo
remains queryable. SHACL widens the `abc:EDTF` pattern to admit
the new shapes:

```
^(-?\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?|-?\d{3}X|-?\d{2}XX)$
```

**Calendar validity.** `assert-calendar-valid!` continues to gate
only `^-?\d{4}-\d{2}-\d{2}$` shapes via `java.time.LocalDate`.
Year-month and year-only shapes were already exempt; decade and
century shapes are exempt for the same reason — there is no calendar
day to validate.

**Provenance and corpus tolerance.** The single-work CLI path
remains fail-closed. The corpus path's existing skip-on-validate
behavior (ADR 0015) still applies for genuinely out-of-grammar
inputs (e.g. EDTF Level 2 uncertainty markers, prose forms not yet
translated). With ADR 0016 in place, the full
`list_person_all_extended_utf8.csv` ingests with zero work skips:
the prior 4 skips collapsed once `192X` and the two `紀元前N世紀…`
forms are admitted.

## Hard Rule (carried forward from ADR 0015)

The EDTF string in the JSON record is the temporal source of truth.
ADR 0016 widens what that string can lexically be; it does not move
the source-of-truth boundary. RDF emission, SHACL, and downstream
consumers continue to derive from the canonical string. Future RDF-
emission changes (OWL-Time, CIDOC-CRM E52 four-boundary, sub-century
refinement using the `century-prose` audit trail) must not change
the EDTF string and therefore must not change `person-record-hash`.

## Consequences

- `schemas/person-record.schema.json` `nullableDate` widens to the
  L0 ∪ L1-decade ∪ L1-century union shown above.
- `schemas/person-record.schema.json` `parseCorrection.rule` enum
  gains `century-prose`. Decade markers introduce no new rule (no
  transformation).
- `schemas/manifest.shacl.ttl` `abc:edtfDateOfBirth` /
  `abc:edtfDateOfDeath` pattern widens to the same union.
- `abc.tools.aozora-csv/parse-date` recognizes the
  `紀元前N世紀(初|末|…)?` family before the partial-date branch and
  emits a `century-prose` correction; otherwise unchanged.
- `abc.tools.person-record/record->graph` (`date-literal-for`)
  returns nil for decade/century shapes; `person-data` skips
  `:rdag2/dateOfBirth` / `:rdag2/dateOfDeath` when the XSD literal
  is nil. The EDTF echo always emits when the date is non-null.
- `person-record/validate!` calendar-validity gate is unchanged
  (only full-date shapes go through `LocalDate.parse`).
- All person-record and metadata-record hashes rotate again under
  the schema-hash cascade; the schema change is the load-bearing
  difference.
- Corpus ingest reaches 17,810 / 17,810 works and 1,334 / 1,334
  persons, with サッフォ (BCE century prose) and シェミン マーガレット
  (decade marker) restored to canonical identity. `parse_corrections`
  on サッフォ records the original `紀元前7世紀末` / `紀元前6世紀初`
  prose alongside the `-06XX` / `-05XX` corrected values.

## Out-of-Scope (v0.1, deferred to future ADRs)

- **CE century prose** (`7世紀`, `6世紀後半`, etc.). Does not align
  with EDTF century markers without a 1-year loss. Not in the corpus.
- **EDTF Level 1 uncertainty/approximation qualifiers** (`?`, `~`, `%`).
- **EDTF Level 1 intervals and sets** (`1984/1999`, `{1984, 1986}`).
- **Sub-century refinement** using the `century-prose` audit trail
  (`紀元前7世紀末` → roughly `-0610`..`-0600`). Belongs to an
  EDTF Level 2 / OWL-Time interval ADR.
- **Japanese era prose** (`明治25年`, `天平17年`). HuTime Calendar LOD
  territory; deferred per ADR 0015.

## References

- ADR 0015: docs/adr/temporal-modeling.md
- EDTF specification (LoC):
  https://www.loc.gov/standards/datetime/
- ISO 8601-2 / EDTF Level 1 capital-`X` digit placeholder.
- HuTime Calendar LOD (era support, deferred): https://ap.hutime.org/
