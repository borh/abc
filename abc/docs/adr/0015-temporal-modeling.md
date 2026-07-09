# ADR 0015: Temporal Modeling for Bibliographic Dates

Status: Accepted
Date: 2026-04-29
Accepted: 2026-07-03

## Implementation Status

Accepted after the temporal model became the live person-record contract.
`schemas/person-record.schema.json` admits the constrained EDTF lexical union
for `date_of_birth` and `date_of_death` plus audited `parse_corrections`;
`abc.tools.aozora-csv/parse-date` normalizes partial dates, recoverable
lexical defects, BCE notation, and unknown sentinels; `abc.tools.person-record`
emits precision-typed RDA Group 2 literals where XSD has a faithful datatype and
parallel `abc:EDTF` echoes; and `schemas/manifest.shacl.ttl` enforces the RDF
date/datatype shape. ADR 0016 later widens the same path for EDTF Level 1
decade and century forms.

## Context

ABC's person records carry `date_of_birth` / `date_of_death`. The corpus
ingest of Aozora's `list_person_all_extended_utf8.csv` (1,334 unique
persons) surfaces shapes that the current `xsd:date`-only model cannot
express:

| Shape | Count | Examples |
| --- | ---: | --- |
| `YYYY-MM-DD` | 2,065 | `1892-03-01` |
| Empty / unknown | 251 | |
| `YYYY` | 297 | `1941`, `1048` |
| `YYYY-MM` | 14 | `1904-01` |
| Malformed but recoverable | 39 | `1888-6-12`, `723-08-15`, `1869- 02-22` |
| BC | 2 | `前347`, `前427` |

The previous behavior dropped any non-ISO shape from the RDF view to
avoid an Aristotle/Jena `xsd:date` lexical-form crash. That is lossy in
two distinct ways: it discards genuinely partial dates (year-only is a
truthful statement of what we know) and it discards recoverable typos
(`1888-6-12` and `1888-06-12` denote the same day).

Future ABC milestones (Japanese era citations, archaeological dates,
period gazetteers, uncertain manuscript dates) will demand richer
temporal modeling than `xsd:date` provides. The standards landscape:

| Standard | Strength | Weakness for ABC v0 |
| --- | --- | --- |
| OWL-Time | W3C; `Instant` / `ProperInterval` / Allen relations | Heavyweight for what is mostly precise points |
| EDTF (LoC / ISO 8601-2) | String superset of ISO 8601; expresses partial, uncertain, approximate, intervals, sets | Not natively queryable in SPARQL; opaque literals |
| CIDOC-CRM E52 Time-Span | Four-boundary uncertainty model widely used in cultural heritage | Heavy modeling; collapses for precise dates |
| HuTime Calendar LOD | Native Japanese-era support, calendar conversions | Adds an external dependency |
| PeriodO | Stable URIs for named historical periods | Not a date-shape problem; a vocabulary gap |
| RDF-star | Annotates triples with temporal validity | Solves a different problem (statement validity, not value uncertainty) |

## Decision

**Canonical storage (JSON record) — constrained EDTF lexical subset.**
v0 admits the lexical grammar
`^-?\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$ | null`:
a calendar date, year-month, or year, with an optional leading `-`
for BCE. The regex enforces month 01–12 and day 01–31; full
calendar validity (rejecting Feb 31, `-0426-02-31`, etc.) is
enforced parser-side via `java.time.LocalDate` / `YearMonth` for
all full and year-month dates, including BCE — `java.time` accepts
a leading `-` on the year, so signed full dates go through the same
gate. Year-only forms (`1941`, `-0426`) skip this check; the regex
is sufficient there.
All 2,065 fully-precise dates remain byte-identical (`1892-03-01`).
Partial dates use the natural EDTF shape (`1941`, `1904-01`). BCE
inputs use the signed-year form (see "BCE conversion" below). Empty
stays explicit JSON `null`. EDTF's uncertainty markers (`?`, `~`,
`%`), approximations, intervals (`1984/1999`), and sets
(`{1984, 1986}`) are out of v0 scope; later ADRs may widen the
grammar.

**Parse-time normalization.** The parser auto-corrects three distinct
classes of input before validation:

- *Cosmetic normalization* — same-calendar, lexical-only fixes: zero-
  pads single-digit month/day (`1888-6-12` → `1888-06-12`), strips
  interior whitespace (`1869- 02-22` → `1869-02-22`), pads short
  years (`723-08-15` → `0723-08-15`), collapses repeated dashes from
  obvious typos (`1850-08--18` → `1850-08-18`), and normalizes
  unambiguous dot date separators (`1839.1.1` → `1839-01-01`).
- *Semantic source-calendar conversion* — converts `前N` (N BCE)
  from Japanese-style anno-Domini-relative notation to ISO 8601-2 /
  XSD 1.1 astronomical year numbering (see "BCE conversion"). This
  is not lexical cleanup; it is a calendar-system conversion whose
  numeric value differs from the source. Future era/calendar
  handling (Meiji-25, lunar dates, etc.) belongs in this class.
- *Sentinel-to-null normalization* — Aozora's CSV uses Japanese
  natural-language sentinels (`不詳` "unknown", `未詳` "not yet
  ascertained") in date columns. These are knowledge-gap markers, not
  dates; v0 maps them to `null`, the same value used for an empty
  cell. The information they carry (gap vs. truly empty) is preserved
  in `parse_corrections` under rule `unknown-marker`. Future work
  may promote knowledge-gap state to a first-class field if downstream
  consumers need to distinguish "we know the date is unknown" from
  "the cell was blank".

All correction classes are recorded as `parse_corrections` entries
with distinct `rule` values — `pad-month`, `pad-day`, `pad-year`,
`strip-whitespace`, `collapse-multi-dash`,
`normalize-date-separator`, `bce-astronomical`, `unknown-marker` —
so the audit trail distinguishes them. All are required to keep
`record-hash` stable across cosmetic CSV revisions and across notation
choices in the source.

EDTF Level 1 shapes that v0 does **not** normalize remain in scope
for a future ADR: decade markers (`192X`), uncertainty/approximation
qualifiers (`1892?`, `1892~`), century-level prose (`紀元前7世紀末`),
intervals, and sets. The corpus contains a handful of such values;
they survive `parse-date` verbatim and trip the schema regex, which
is the intended outcome — `parse-date` is not the place to invent
EDTF Level 1 semantics, and silently dropping them would erase the
signal that a richer grammar is needed.

**Validation tolerance at corpus scale.** Per-record
`person-record/validate!` is fail-closed: a malformed date or any
other schema violation throws. The single-work ingest path propagates
that exception (failure should be loud when you're targeting one
record). The corpus ingest path (`run-corpus!`) catches
`person-record/validate!` failures, logs a structured warning
identifying the offending `person_id` + reason, and continues with
the next person. The exit summary includes a `persons-skipped` count
alongside `persons-written`. This keeps a single bad row from
aborting an end-to-end ingest of 1,300+ persons, while still
surfacing the failure for follow-up.

**Provenance boundary.** The pure parser
(`parse-person-fields-from-row`) returns `{:fields ..., :corrections
[...]}`; it has no source URL, retrieval timestamp, or file hash
context. The ingester (which holds the ZIP path) computes
`original_file_hash` from the CSV bytes and reads `retrieved_at`
from the CSV entry's stored mtime; `source_url` is null unless
supplied via `--source-url`. With those three facts in hand, the
ingester emits `source_csv_provenance` and merges `:corrections`
into it as `parse_corrections`. Programmatic callers that pass rows
directly (without a ZIP context) skip provenance — their records
carry no `source_csv_provenance` block, and the audit trail is
dropped in keeping with that no-source-identity promise.
`source_csv_provenance` is excluded from canonical identity, so
attaching the audit trail does not affect `record-hash`. Schema:
`source_url` and `retrieved_at` accept null (the local-ingest
case); `original_file_hash` is required when the block is present.

**BCE conversion.** ABC follows ISO 8601-2 / XSD 1.1 astronomical
year numbering: year `0000` corresponds to 1 BCE, `-0001` to 2 BCE,
and so on. Therefore N BCE maps to lexical year `-(N-1)` (zero-padded
to four digits):

| Aozora `前N` | astronomical year | EDTF/XSD lexical |
| --- | --- | --- |
| `前1` | 0 | `0000` |
| `前2` | -1 | `-0001` |
| `前427` | -426 | `-0426` |

This is a semantic conversion, not lexical preservation: the printed
number changes by one. The convention is documented in the parser
and surfaced as a `parse_corrections` entry on the record.

**RDF emission (v0) — native XSD types, no OWL-Time wrapper yet.**
For each non-null date the RDF view emits two triples on the same
person: a typed XSD literal under the existing RDA Group 2 predicate,
and the canonical EDTF string under a parallel ABC predicate:

| Lexical form | RDA literal | EDTF echo |
| --- | --- | --- |
| `YYYY-MM-DD`, `-YYYY-MM-DD` | `rdag2:dateOfBirth "…"^^xsd:date` | `abc:edtfDateOfBirth "…"^^abc:EDTF` |
| `YYYY-MM`, `-YYYY-MM` | `rdag2:dateOfBirth "…"^^xsd:gYearMonth` | `abc:edtfDateOfBirth "…"^^abc:EDTF` |
| `YYYY`, `-YYYY` | `rdag2:dateOfBirth "…"^^xsd:gYear` | `abc:edtfDateOfBirth "…"^^abc:EDTF` |
| JSON null | predicate omitted | predicate omitted |

(`rdag2:dateOfDeath` / `abc:edtfDateOfDeath` follow the same pattern.)

The EDTF echo preserves the canonical lexical value as a first-class
RDF term so consumers can ignore the three-way XSD datatype dispatch
when they only want the source value.

**v0 RDF is faithful per-precision, not uniformly range-queryable.**
Once `rdag2:dateOfBirth` may carry `xsd:date`, `xsd:gYearMonth`, or
`xsd:gYear`, a SPARQL `FILTER (?dob >= "1900"^^xsd:date)` only
matches the precise-date subset; gYear and gYearMonth literals are
incomparable to xsd:date in standard SPARQL. v0 records this as a
known limitation; uniform range queries arrive when an OWL-Time
interval view (or CIDOC-CRM E52 four-boundary view) is added by a
later ADR. Until then, range-query consumers either filter by
datatype or project to a uniform precision.

SHACL relaxes to allow any of `xsd:date`, `xsd:gYearMonth`, or
`xsd:gYear` on the RDA predicates, and constrains
`abc:edtfDateOfBirth` / `abc:edtfDateOfDeath` with `sh:maxCount 1`,
`sh:datatype abc:EDTF`, and
`sh:pattern "^-?\\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$"`.
The pattern is
required because `sh:datatype` only checks the datatype IRI, not the
constrained v0 lexical grammar; without it, an out-of-grammar EDTF
literal would pass SHACL.

**Future-work hooks (not v0).** When the project encounters
uncertainty (`?`, `~`), Japanese-era source data, or historical
periods, layer in:

- **OWL-Time `time:ProperInterval`** for partial dates as ranges
  (e.g., `1941` → `[1941-01-01, 1941-12-31]`). Today this is
  derivable from the XSD literal; only emit it when consumers need
  it.
- **CIDOC-CRM E52 four-boundary** for genuinely uncertain dates.
- **HuTime Calendar LOD** for era-based source dates (Meiji 25,
  etc.) and calendar conversions.
- **PeriodO URIs** for named historical periods.

Each addition is a separate ADR. The canonical JSON record's EDTF
string is forward-compatible with all of them.

## Hard Rule

The EDTF string in the JSON record is the temporal source of truth.
RDF views are derived. Future RDF-emission changes (OWL-Time, E52,
HuTime) must not change the EDTF string and therefore must not
change `person-record-hash`.

## Consequences

- `schemas/person-record.schema.json` `nullableDate` widens to
  `^-?\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$ | null`.
- `schemas/person-record.schema.json` `csvProvenance` gains an optional
  `parse_corrections` array of
  `{field, raw, corrected, rule}` entries; `source_url` and
  `retrieved_at` accept null so the local-ingest flavor can attach
  the parse-corrections audit trail without needing an upstream URL.
- `schemas/manifest.shacl.ttl` accepts any of `xsd:date`,
  `xsd:gYearMonth`, `xsd:gYear` on `rdag2:dateOfBirth` /
  `rdag2:dateOfDeath`, and constrains the parallel
  `abc:edtfDateOfBirth` / `abc:edtfDateOfDeath` to `abc:EDTF` with
  `sh:maxCount 1`.
- `abc.tools.aozora-csv/parse-person-fields-from-row` returns
  `{:fields, :corrections}`; the ingester merges `:corrections`
  into the record's `source_csv_provenance.parse_corrections` when
  it has CSV-level provenance to attach.
- `abc.tools.person-record/record->graph` dispatches on lexical shape
  to pick the XSD datatype and additionally emits the EDTF echo
  under `abc:edtfDateOfBirth` / `abc:edtfDateOfDeath`.
- A new datatype IRI `https://w3id.org/abc/EDTF` (prefix `abc:EDTF`)
  marks EDTF lexical literals; consumers that don't recognize the
  datatype can still recover the lexical string (e.g., via SPARQL
  `STR()`).
- ~352 person records and every metadata record that references them
  get a one-time `person-record-hash` / `metadata-record-hash`
  rotation when the corrected dates land. The 000879 (芥川) fixture
  is unaffected (full ISO dates).
- The two BCE entries in the corpus shift from
  `前347` / `前427` to `-0346` / `-0426` under astronomical year
  numbering. The conversion is documented in the parser and visible
  in `parse_corrections`.

## Known deprecated vocabulary

The `rdag2:` prefix used here is bound to
`http://RDVocab.info/ElementsGr2/` (see `abc.tools.rdf-prefixes` and
`schemas/manifest.shacl.ttl`). That namespace is the **deprecated** RDA
Group 2 element set: the RDA Registry states that the draft elements at
`rdvocab.info` were never officially published and are deprecated in favour of
the published `rdaregistry.info` element sets.

The published equivalents, per the RDA deprecation map, are:

| Deprecated (this ADR) | Published RDA property |
| --- | --- |
| `rdag2:dateOfBirth` | `http://rdaregistry.info/Elements/a/P50121` (`rdaa:P50121`) |
| `rdag2:dateOfDeath` | `http://rdaregistry.info/Elements/a/P50120` (`rdaa:P50120`) |

This is tracked technical debt, not a correctness defect: the predicates
exist, carry the intended RDA-at-creation-time semantics, and the ADR is
internally consistent. Switching to the published `rdaregistry.info/Elements/a/`
predicates would rotate every committed person/metadata RDF fixture and
`person_record_hash` / `metadata_record_hash` and is therefore deferred to a
follow-up ADR that schedules it with the schema-hash cascade, following the
same discipline as ADR 0017 / ADR 0018.

## References

- EDTF specification (LoC): https://www.loc.gov/standards/datetime/
- OWL-Time: https://www.w3.org/TR/owl-time/
- CIDOC-CRM E52 Time-Span: https://cidoc-crm.org/Issue/ID-90-scope-notes-of-e52
- HuTime Ontology: https://ap.hutime.org/
- PeriodO: https://perio.do/
- XSD date/time datatypes:
  https://www.w3.org/TR/xmlschema11-2/#dateTimeStamp
- RDA Registry (published element sets): https://www.rdaregistry.info/Elements/a/
- RDA deprecation map (deprecated `rdvocab.info` → published `rdaregistry.info`):
  https://www.rdaregistry.info/rgArch/mapRDAOld2NewProp.html
