# SPEC Verification Report: `validate-bundle-simplification-spec.md`

**Verdict:** EXECUTE-AS-IS  
**Verified by:** independent read of the files cited in the spec  
**Date:** 2026-07-02

---

## 1. Inline Schematron expected-rule sets

All inline Schematron rule-id sets live in a single call to `validate-tei-schematron!` in `src/abc/tools/validate_design_bundle.clj`. The spec’s line numbers are essentially correct; the set literals fall on these lines:

| Line(s) | Fixture | Declared rule-id set |
|---|---|---|
| 595–596 | `fixtures/tei/warnings/figure-missing-desc.xml` | `#{"abc-figure-accessibility"}` |
| 597–598 | `fixtures/tei/warnings/transcription-enrichment-undeclared.xml` | `#{"abc-transcription-vs-annotation"}` |
| 599–600 | `fixtures/tei/invalid/missing-title.xml` | `#{"abc-tei-header-title"}` |
| 601–602 | `fixtures/tei/invalid/char-empty-decl.xml` | `#{"abc-char-resolution-form"}` |
| 603–604 | `fixtures/tei/invalid/gaiji-dangling-ref.xml` | `#{"abc-gaiji-chardecl-resolution"}` |
| 605–606 | `fixtures/tei/invalid/gaiji-missing-ref.xml` | `#{"abc-gaiji-reference"}` |
| 607–608 | `fixtures/tei/invalid/header-no-language.xml` | `#{"abc-header-language-declared"}` |
| 609–610 | `fixtures/tei/invalid/missing-source-work-id.xml` | `#{"abc-tei-header-source-work-id"}` |
| 611–612 | `fixtures/tei/invalid/ruby-empty-base.xml` | `#{"abc-ruby-base-non-empty"}` |
| 613–614 | `fixtures/tei/invalid/ruby-empty-reading.xml` | `#{"abc-ruby-reading-non-empty"}` |
| 615–616 | `fixtures/tei/invalid/ruby-missing-reading.xml` | `#{"abc-ruby-complete"}` |
| 617–618 | `fixtures/tei/invalid/source-span-external-ref.xml` | `#{"abc-source-span-reference"}` |
| 619–620 | `fixtures/tei/invalid/source-span-dangling-ref.xml` | `#{"abc-source-span-target-exists"}` |

Distinct rule-ids actually hard-coded inline: **13**.

```clojure
#{"abc-figure-accessibility"
  "abc-transcription-vs-annotation"
  "abc-tei-header-title"
  "abc-char-resolution-form"
  "abc-gaiji-chardecl-resolution"
  "abc-gaiji-reference"
  "abc-header-language-declared"
  "abc-tei-header-source-work-id"
  "abc-ruby-base-non-empty"
  "abc-ruby-reading-non-empty"
  "abc-ruby-complete"
  "abc-source-span-reference"
  "abc-source-span-target-exists"}
```

✅ The spec’s claim of "13 Schematron rule-ids" hard-coded inline is accurate.

## 2. Inline person-drift expected-code sets

All inline drift code sets live in a single call to `validate-drift-fixtures!` in `src/abc/tools/validate_design_bundle.clj`, lines 623–664. The spec’s line range is accurate.

| Line(s) | Fixture | Declared code set |
|---|---|---|
| 623–624 | `fixtures/v0/invalid/drift/broken-index-target` | `#{:index-target-missing}` |
| 625–626 | `fixtures/v0/invalid/drift/asymmetric-index` | `#{:event-missing-from-participant-index}` |
| 626–627 | `fixtures/v0/invalid/drift/orphan-event-file` | `#{:orphan-event-file :event-missing-from-participant-index}` |
| 628–630 | `fixtures/v0/invalid/drift/unsorted-participants` | `#{:participants-not-sorted :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| 631–634 | `fixtures/v0/invalid/drift/dangling-snapshot-ref` | `#{:unknown-snapshot-reference :participant-not-covered :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| 635–637 | `fixtures/v0/invalid/drift/invalid-role` | `#{:invalid-had-role :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| 638–640 | `fixtures/v0/invalid/drift/invalid-agent` | `#{:invalid-agent-iri :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| 641–644 | `shacl-missing-date/graph.ttl` | `#{:shacl-violation}` |
| 645–648 | `split-cardinality-one-successor/graph.ttl` | `#{:shacl-violation :rdf-participant-prov-mismatch}` |
| 649–652 | `merge-cardinality-one-predecessor/graph.ttl` | `#{:shacl-violation}` |
| 653–656 | `typing-missing-subclass/graph.ttl` | `#{:missing-rdf-type :rdf-participant-prov-mismatch}` |
| 657–660 | `typing-missing-activity/graph.ttl` | `#{:missing-rdf-type :shacl-violation :rdf-participant-prov-mismatch}` |
| 661–664 | `rdf-participant-prov-mismatch/graph.ttl` | `#{:rdf-participant-prov-mismatch}` |

Distinct drift failure codes actually hard-coded inline: **11**.

```clojure
#{:index-target-missing
  :event-missing-from-participant-index
  :orphan-event-file
  :participants-not-sorted
  :unknown-snapshot-reference
  :participant-not-covered
  :invalid-had-role
  :invalid-agent-iri
  :shacl-violation
  :missing-rdf-type
  :rdf-participant-prov-mismatch}
```

The spec’s headline of "20 drift codes" refers to the total universe emitted by `person_drift.clj` (see §3.2), not to the count declared inline. The inline map covers 11 of the 20 codes. The task’s CONTEXT phrase "20 person-drift failure codes are hard-coded inline" is therefore slightly loose — only 11 distinct codes are hard-coded inline, but the spec is correct that the authoritative source defines 20 in total and about half are missing fixtures.

## 3. Authoritative sources

### 3.1 Schematron rule universe

`schemas/tei-profile.sch` contains all 13 `abc-*` rule-ids as `<pattern id="abc-...">`. The same 13 ids are generated from `schemas/tei-profile.odd` at lines 98–230 (`<constraintSpec ident="abc-...">`).

Rule-ids in schema source vs. inline sets:

| Rule-id | In schema | Asserted by at least one fixture |
|---|---|---|
| `abc-tei-header-title` | ✅ | ✅ `missing-title.xml` |
| `abc-tei-header-source-work-id` | ✅ | ✅ `missing-source-work-id.xml` |
| `abc-header-language-declared` | ✅ | ✅ `header-no-language.xml` |
| `abc-ruby-complete` | ✅ | ✅ `ruby-missing-reading.xml` |
| `abc-ruby-base-non-empty` | ✅ | ✅ `ruby-empty-base.xml` |
| `abc-ruby-reading-non-empty` | ✅ | ✅ `ruby-empty-reading.xml` |
| `abc-gaiji-reference` | ✅ | ✅ `gaiji-missing-ref.xml` |
| `abc-gaiji-chardecl-resolution` | ✅ | ✅ `gaiji-dangling-ref.xml` |
| `abc-char-resolution-form` | ✅ | ✅ `char-empty-decl.xml` |
| `abc-figure-accessibility` | ✅ | ✅ `figure-missing-desc.xml` (warning) |
| `abc-source-span-reference` | ✅ | ✅ `source-span-external-ref.xml` |
| `abc-source-span-target-exists` | ✅ | ✅ `source-span-external-ref.xml`, `source-span-dangling-ref.xml` |
| `abc-transcription-vs-annotation` | ✅ | ✅ `transcription-enrichment-undeclared.xml` (warning) |

No inline rule-id is missing from the schema, and every schema rule is asserted by at least one fixture. The authoritative source is complete for rule-ids.

### 3.2 Drift failure-code universe

`src/abc/tools/person_drift.clj` defines failure codes in Clojure vectors/maps across several functions. There is **no single authoritative registry**; the codes are scattered. Verified emitted codes:

| # | Code | Source function / location | Notes |
|---|---|---|---|
| 1 | `:participants-not-sorted` | `event-json-coherence-failures`, line 88 | |
| 2 | `:used-not-sorted` | `event-json-coherence-failures`, line 91 | |
| 3 | `:generated-not-sorted` | `event-json-coherence-failures`, line 94 | |
| 4 | `:duplicate-snapshot-id` | `event-json-coherence-failures`, line 96 | |
| 5 | `:unknown-snapshot-reference` | `event-json-coherence-failures`, line 98 | |
| 6 | `:participant-not-covered` | `event-json-coherence-failures`, line 100 | |
| 7 | `:participant-in-both-used-and-generated` | `event-json-coherence-failures`, line 102 | |
| 8 | `:snapshot-prefix-usage-mismatch` | `event-json-coherence-failures`, lines 104 and 108 | One code, two usage variants (`used` and `was_generated_by`) |
| 9 | `:unresolved-curie-prefix` | `event-json-coherence-failures`, line 113 | |
| 10 | `:invalid-had-role` | `event-json-coherence-failures`, line 115 | |
| 11 | `:invalid-agent-iri` | `event-json-coherence-failures`, line 117 | |
| 12 | `:missing-rdf-type` | `typing-coherence-failures`, line 224 | |
| 13 | `:unexpected-rdf-type` | `typing-coherence-failures`, line 226 | |
| 14 | `:rdf-participant-prov-mismatch` | `graph-participant-prov-failures`, line 274 | |
| 15 | `:shacl-violation` | `shacl-failures`, line 289 | wraps SHACL errors |
| 16 | `:schema-hash-mismatch` | `schema-hash-failure`, line 320 | |
| 17 | `:index-target-missing` | `referential-integrity-failures`, line 396 | |
| 18 | `:event-missing-from-participant-index` | `referential-integrity-failures`, line 402 | |
| 19 | `:orphan-event-file` | `referential-integrity-failures`, line 406 | |
| 20 | `:exception` | `validate-drift-events!`, line 430 | catch-all |

✅ The spec’s claim of "20 drift failure codes" total is accurate. The source is complete in the sense that every hard-coded inline code exists in `person_drift.clj`, but it is **scattered** across the file — there is no explicit registry set. This matches the spec’s diagnosis that derivation requires either an explicit registry (`failure-codes`) or static analysis.

## 4. Duplication in tests

`test/abc/tools/validate_design_bundle_test.clj` duplicates both inline maps verbatim:

* `validate-tei-schematron-expected-findings-test` (lines 206–234) reproduces every fixture/rule-id pair from production. Note: the test’s inline set for `source-span-external-ref.xml` is `#{"abc-source-span-reference" "abc-source-span-target-exists"}` (lines 231–232), which differs from the production set.
* `validate-drift-fixtures-smoke-test` (lines 383–428) reproduces every fixture/code pair from production.

✅ The spec’s claim of test duplication is accurate.

## 5. Filed bugs

### 5.1 `source-span-external-ref.xml` production set is incomplete

Fixture file `fixtures/tei/invalid/source-span-external-ref.xml` contains:

```xml
<p source="https://example.invalid/span-1">External source span reference.</p>
```

The Schematron rules applied to `*[@source]` are:

* `abc-source-span-reference`: every source reference must start with `#`. The value `https://example.invalid/span-1` violates this.
* `abc-source-span-target-exists`: every reference must resolve to an existing `@xml:id`. Because the value has no `#`, `substring-after($s, '#')` returns the empty string, and no element has `@xml:id=""`, so this rule also fires.

Therefore the actual error rule set for this fixture is `#{"abc-source-span-reference" "abc-source-span-target-exists"}`. The production inline set at line 617–618 declares only `#{"abc-source-span-reference"}`. The gate still passes because `validate-schematron-invalid-fixture!` (lines 204–215) only checks for *missing* expected rules and silently permits extras.

✅ Filed bug (a) is **real**.

### 5.2 ~10 drift codes have no negative fixture

Cross-referencing the 20 emitted codes from §3.2 against the existing fixtures under `fixtures/v0/invalid/drift/`:

| Code | Covered by negative fixture |
|---|---|
| `:participants-not-sorted` | ✅ `unsorted-participants` |
| `:used-not-sorted` | ❌ |
| `:generated-not-sorted` | ❌ |
| `:duplicate-snapshot-id` | ❌ |
| `:unknown-snapshot-reference` | ✅ `dangling-snapshot-ref` |
| `:participant-not-covered` | ✅ `dangling-snapshot-ref` |
| `:participant-in-both-used-and-generated` | ❌ |
| `:snapshot-prefix-usage-mismatch` | ❌ (both usage variants) |
| `:unresolved-curie-prefix` | ❌ |
| `:invalid-had-role` | ✅ `invalid-role` |
| `:invalid-agent-iri` | ✅ `invalid-agent` |
| `:missing-rdf-type` | ✅ `typing-missing-subclass`, `typing-missing-activity` |
| `:unexpected-rdf-type` | ❌ |
| `:rdf-participant-prov-mismatch` | ✅ multiple TTL fixtures |
| `:shacl-violation` | ✅ multiple TTL fixtures |
| `:schema-hash-mismatch` | ❌ |
| `:index-target-missing` | ✅ `broken-index-target` and others |
| `:event-missing-from-participant-index` | ✅ `asymmetric-index` and others |
| `:orphan-event-file` | ✅ `orphan-event-file` and others |
| `:exception` | ❌ |

Uncovered distinct codes: **9** (`:used-not-sorted`, `:generated-not-sorted`, `:duplicate-snapshot-id`, `:participant-in-both-used-and-generated`, `:snapshot-prefix-usage-mismatch`, `:unresolved-curie-prefix`, `:unexpected-rdf-type`, `:schema-hash-mismatch`, `:exception`). If the two usage variants of `:snapshot-prefix-usage-mismatch` are counted separately, the gap is **10** items.

✅ Filed bug (b) is **real**.

---

## Overall verdict

**EXECUTE-AS-IS.**

The spec’s factual premises are all verified:

1. The 13 inline Schematron rule-ids exist at the cited locations in `src/abc/tools/validate_design_bundle.clj`.
2. The 20 drift failure codes exist in `src/abc/tools/person_drift.clj` (scattered, no registry). Only 11 of those 20 are currently declared inline in `validate_design_bundle.clj`.
3. The authoritative sources (`schemas/tei-profile.sch` / `.odd` and `person_drift.clj`) contain the full code/rule universe claimed.
4. Both inline maps are duplicated in `test/abc/tools/validate_design_bundle_test.clj`.
5. Both filed semantic bugs are real: the production Schematron set for `source-span-external-ref.xml` is incomplete, and approximately 10 drift codes/items lack negative fixtures.

**One caveat for the implementer:** the drift authoritative source is itself scattered (no `failure-codes` set). Derivation cannot be a simple "read one var" until the spec’s recommended registry is created in `person_drift.clj`. Parsing the namespace statically is brittle. Adding an explicit `person-drift/failure-codes` set is the only robust way to make the source authoritative rather than source-scattered.
