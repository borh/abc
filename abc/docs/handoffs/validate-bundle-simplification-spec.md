# Codebase Simplification Audit: `validate-design-bundle` expected-set drift

**Mode:** Audit + Patch spec (no code edits).  
**Scope:** `src/abc/tools/validate_design_bundle.clj`, with cross-references to `schemas/tei-profile.sch`, `schemas/tei-profile.odd`, and `src/abc/tools/person_drift.clj`.  
**Skill:** `codebase-simplification` — PROTECT first, VERIFY behavior, file risky/semantic changes separately.

---

## 1. OBSERVE — inline expected-rule / expected-code sets

Every inline set literal that ties a fixture to a hand-maintained rule/code expectation is listed below. The same values are duplicated between the production gate and its test file, so both are inventoried.

### 1.1 Schematron rule-id sets

**Authoritative source:** `schemas/tei-profile.odd` defines each rule via `<constraintSpec ident="abc-..." scheme="schematron">` (lines 104–288).  The consumed validation artifact `schemas/tei-profile.sch` mirrors those ids as `<pattern id="abc-...">` (lines 16–130); the `.sch` is generated from the `.odd` via `extract-isosch.xsl`.

**Current `.sch` rule universe (13 ids):**

```clojure
#{"abc-tei-header-title"
  "abc-tei-header-source-work-id"
  "abc-header-language-declared"
  "abc-ruby-complete"
  "abc-ruby-base-non-empty"
  "abc-ruby-reading-non-empty"
  "abc-gaiji-reference"
  "abc-gaiji-chardecl-resolution"
  "abc-char-resolution-form"
  "abc-figure-accessibility"
  "abc-source-span-reference"
  "abc-source-span-target-exists"
  "abc-transcription-vs-annotation"}
```

**Production inline sets** in `src/abc/tools/validate_design_bundle.clj`:

| Lines | Fixture | Declared set |
|-------|---------|--------------|
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

**Test inline duplicate sets** in `test/abc/tools/validate_design_bundle_test.clj`:

| Lines | Fixture | Declared set |
|-------|---------|--------------|
| 208–209 | `fixtures/tei/warnings/figure-missing-desc.xml` | `#{"abc-figure-accessibility"}` |
| 210–211 | `fixtures/tei/warnings/transcription-enrichment-undeclared.xml` | `#{"abc-transcription-vs-annotation"}` |
| 212–213 | `fixtures/tei/invalid/missing-title.xml` | `#{"abc-tei-header-title"}` |
| 216–217 | `fixtures/tei/invalid/char-empty-decl.xml` | `#{"abc-char-resolution-form"}` |
| 218–219 | `fixtures/tei/invalid/gaiji-missing-ref.xml` | `#{"abc-gaiji-reference"}` |
| 220–221 | `fixtures/tei/invalid/gaiji-dangling-ref.xml` | `#{"abc-gaiji-chardecl-resolution"}` |
| 222–223 | `fixtures/tei/invalid/header-no-language.xml` | `#{"abc-header-language-declared"}` |
| 214–215 | `fixtures/tei/invalid/missing-source-work-id.xml` | `#{"abc-tei-header-source-work-id"}` |
| 226–227 | `fixtures/tei/invalid/ruby-empty-base.xml` | `#{"abc-ruby-base-non-empty"}` |
| 228–229 | `fixtures/tei/invalid/ruby-empty-reading.xml` | `#{"abc-ruby-reading-non-empty"}` |
| 224–225 | `fixtures/tei/invalid/ruby-missing-reading.xml` | `#{"abc-ruby-complete"}` |
| 230–232 | `fixtures/tei/invalid/source-span-external-ref.xml` | `#{"abc-source-span-reference" "abc-source-span-target-exists"}` |
| 233–234 | `fixtures/tei/invalid/source-span-dangling-ref.xml` | `#{"abc-source-span-target-exists"}` |

**Observed actual rule-id output** (characterization snapshot, current behavior):

| Fixture | Actual errors | Actual warnings |
|---------|----------------|-----------------|
| `fixtures/tei/invalid/source-span-external-ref.xml` | `#{"abc-source-span-reference" "abc-source-span-target-exists"}` | `#{}` |
| all other invalid fixtures | match the production declared set (single id each) | `#{}` |
| both warning fixtures | match the declared warning set | as declared |

The test declared `source-span-external-ref` with both ids; production declares only one.  The gate `nix run .#validate-design-bundle` still exits 0 because `validate-schematron-invalid-fixture!` (line 204–215) only checks for *missing* expected rules (`set/difference expected-rules actual-errors`) and silently permits extra ones.  This is a semantic asymmetry, not merely duplication.

### 1.2 Person-drift expected-code sets

**Authoritative source:** failure codes are the `:code` keyword constants emitted by the validation functions in `src/abc/tools/person_drift.clj`:

| Code | Source location |
|------|-----------------|
| `:participants-not-sorted` | `person_drift.clj:88` |
| `:used-not-sorted` | `person_drift.clj:91` |
| `:generated-not-sorted` | `person_drift.clj:94` |
| `:duplicate-snapshot-id` | `person_drift.clj:96` |
| `:unknown-snapshot-reference` | `person_drift.clj:98` |
| `:participant-not-covered` | `person_drift.clj:100` |
| `:participant-in-both-used-and-generated` | `person_drift.clj:102` |
| `:snapshot-prefix-usage-mismatch` | `person_drift.clj:104` and `:108` |
| `:unresolved-curie-prefix` | `person_drift.clj:113` |
| `:invalid-had-role` | `person_drift.clj:115` |
| `:invalid-agent-iri` | `person_drift.clj:117` |
| `:missing-rdf-type` | `person_drift.clj:224` |
| `:unexpected-rdf-type` | `person_drift.clj:226` |
| `:rdf-participant-prov-mismatch` | `person_drift.clj:274` |
| `:shacl-violation` (wrapped) | `person_drift.clj:289` |
| `:schema-hash-mismatch` | `person_drift.clj:320` |
| `:index-target-missing` | `person_drift.clj:396` |
| `:event-missing-from-participant-index` | `person_drift.clj:402` |
| `:orphan-event-file` | `person_drift.clj:406` |
| `:exception` | `person_drift.clj:430` |

**Production inline drift fixtures** in `src/abc/tools/validate_design_bundle.clj` lines 623–664:

*Directory fixtures*

| Fixture | Declared codes |
|---------|----------------|
| `fixtures/v0/invalid/drift/broken-index-target` | `#{:index-target-missing}` |
| `fixtures/v0/invalid/drift/asymmetric-index` | `#{:event-missing-from-participant-index}` |
| `fixtures/v0/invalid/drift/orphan-event-file` | `#{:orphan-event-file :event-missing-from-participant-index}` |
| `fixtures/v0/invalid/drift/unsorted-participants` | `#{:participants-not-sorted :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| `fixtures/v0/invalid/drift/dangling-snapshot-ref` | `#{:unknown-snapshot-reference :participant-not-covered :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| `fixtures/v0/invalid/drift/invalid-role` | `#{:invalid-had-role :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |
| `fixtures/v0/invalid/drift/invalid-agent` | `#{:invalid-agent-iri :index-target-missing :event-missing-from-participant-index :orphan-event-file}` |

*Turtle fixture pairs* (each `:event` / `:graph` map, expected code set on the following line):

| Lines | Graph fixture | Declared codes |
|-------|---------------|----------------|
| 641–644 | `shacl-missing-date/graph.ttl` | `#{:shacl-violation}` |
| 645–648 | `split-cardinality-one-successor/graph.ttl` | `#{:shacl-violation :rdf-participant-prov-mismatch}` |
| 649–652 | `merge-cardinality-one-predecessor/graph.ttl` | `#{:shacl-violation}` |
| 653–656 | `typing-missing-subclass/graph.ttl` | `#{:missing-rdf-type :rdf-participant-prov-mismatch}` |
| 657–660 | `typing-missing-activity/graph.ttl` | `#{:missing-rdf-type :shacl-violation :rdf-participant-prov-mismatch}` |
| 661–664 | `rdf-participant-prov-mismatch/graph.ttl` | `#{:rdf-participant-prov-mismatch}` |

**Test inline duplicate sets** in `test/abc/tools/validate_design_bundle_test.clj` lines 383–428: the same fixture/code pairs as above.

**Observed actual code output** matches the declared sets exactly (drift runner enforces equality), so the coverage gap is invisible to CI: ten codes emitted by `person_drift.clj` have no negative fixture at all (`:used-not-sorted`, `:generated-not-sorted`, `:duplicate-snapshot-id`, `:participant-in-both-used-and-generated`, `:snapshot-prefix-usage-mismatch`, `:unresolved-curie-prefix`, `:unexpected-rdf-type`, `:schema-hash-mismatch`, `:exception`, and one of the two usage variants of `:snapshot-prefix-usage-mismatch`).

---

## 2. DIAGNOSE — concretely braided concerns

`validate-design-bundle!` (`src/abc/tools/validate_design_bundle.clj:502–689`) is a single sequential orchestrator that braids the following concerns:

| # | Concern | Evidence | Already split? |
|---|---------|----------|----------------|
| 1 | ab-validator output materialization + format/schema-hash checks | `materialize/materialize-import!`, `validate-ab-validator-output!` | yes (`validate-ab-validator-output!`) |
| 2 | JSON Schema self-validation and example conformance | `validate-json-schemas!` | yes |
| 3 | Manifest index reproducibility conflict detection | `manifest-index/validate-no-reproducibility-conflicts!` | no (single call) |
| 4 | Manifest → RDF/TTL generation smoke test | `manifest-to-rdf/manifest->ttl` | no |
| 5 | SHACL shape conformance for manifests | `validate-shacl!` | yes |
| 6 | Metadata record + persons bundle (schema hashes, contributor integrity, SHACL, TTL parity) | `validate-metadata-bundle!` | yes |
| 7 | Person-drift positive-pass validation | `person-drift/validate-drift-events!` | yes |
| 8 | Canonicalization digest fixtures | `validate-canonicalization!` | yes |
| 9 | XML well-formedness via external `xmllint` | `validate-xml!` | yes |
| 10 | TEI P5 RelaxNG validation (uses `TEI_SCHEMA_PATH` env) | `validate-tei!` with env path | yes |
| 11 | TEI project RelaxNG validation | `validate-tei!` with `"schemas/tei-profile.rng"` | yes |
| 12 | TEI Schematron negative/warning partition with inline expected-rule sets | `validate-tei-schematron!` | yes, but the expected sets are hand-maintained and duplicated |
| 13 | Person-drift negative fixture partition with inline expected-code sets | `validate-drift-fixtures!` | yes, but same hand-maintain/duplicate issue |
| 14 | Linked Art publication view byte-parity | `validate-publication-view!` | yes |
| 15 | IIIF applicability record validation | `iiif/validate-applicability!` | yes |
| 16 | git-cliff configuration smoke test | `validate-git-cliff!` | yes |
| 17 | Process lifecycle / temp-dir cleanup + logging setup | `let` temp dir, `finally` delete, `-main` handler | no (infrastructure) |

**Core complection:** the *partitioning logic* (which fixture is expected to trigger which rule/code) is braided into the *orchestration logic* in two large inline map literals.  The sub-functions already exist; this refactor is not about splitting the gate further.  It is about **deriving the universe of expected rule/code ids from an authoritative source** so the hand-maintained maps cannot drift out of sync with `tei-profile.odd` / `person_drift.clj`.

The additional, subtler complection is the **Schematron assertion asymmetry**: the production runner checks only that expected rules are present (`missing`), not that the actual set is equal.  That allows a fixture to silently trigger extra rules, which is exactly how the production map for `source-span-external-ref.xml` has drifted.

---

## 3. SPEC — derive expected sets from authoritative sources

### 3.1 Schematron derivation

**Data flow sketch:**

```text
                          schemas/tei-profile.odd
                                (source of truth)
                                   │
                                   ▼
                    extract-abc-schematron-rules(.odd | .sch)
                                   │
                                   ▼
                         all-abc-rules : #{String}
                                   │
          ┌────────────────────────┼────────────────────────┐
          │                        │                        │
          ▼                        ▼                        ▼
   declared-invalid-map     declared-warning-map      validate each fixture
   fixture -> rule-set       fixture -> rule-set       actual errors/warnings
          │                        │                        │
          └────────────────────────┼────────────────────────┘
                                   ▼
                    ┌───────────────────────────────┐
                    │ Invariants                      │
                    │ 1. union(actual invalid rules,  │
                    │    actual warning rules)        │
                    │    == all-abc-rules             │
                    │    (every rule fires somewhere) │
                    │ 2. ∀ invalid fixture:           │
                    │    actual-error-rules(fixture)  │
                    │    == declared-invalid(fixture) │
                    │ 3. ∀ warning fixture:           │
                    │    actual-warning-rules(fixture)│
                    │    == declared-warning(fixture) │
                    └───────────────────────────────┘
```

**Implementation shape (minimal, reversible):**

1. Pure extractor `(schematron-rule-ids "schemas/tei-profile.sch")` → set of all `<pattern id="abc-*">` ids.  Parse with `clojure.xml/parse` or `clojure.data.xml`; no new deps.
2. Move the `{:valid-fixtures [...] :warning-fixtures {...} :invalid-fixtures {...}}` map into a single named value — either a private `def` in `validate_design_bundle.clj` or a shared EDN resource under `fixtures/tei/schematron-partition.edn`.
3. `validate-tei-schematron!` becomes:
   * read `schema-path`, compute `all-rules`;
   * for each fixture, collect actual rule ids by severity;
   * assert coverage (1 above);
   * assert each declared set equals the actual set.
4. Tests import the same fixture-declaration value(s), so the production/test duplication disappears.

**Source choice:** `schemas/tei-profile.odd` is normative, but the runtime artifact is `schemas/tei-profile.sch`.  Recommend deriving from `.sch` (fast XML parse, no XSLT toolchain in the test path) and adding a CI/nix assertion that `.sch` is regenerated from `.odd`, or parsing `.odd`'s `<constraintSpec ident="...">` ids if the team prefers the canonical source.  Either way the set is a value, not hand-maintained.

### 3.2 Person-drift code derivation

**Authoritative list:** `src/abc/tools/person_drift.clj` (lines 88–430) is the only place that emits failure `:code`s.  There is no separate registry today.

**Can the same derivation apply?** Yes in principle, but the source is Clojure code rather than XML, so extraction is different:

*Option A — explicit registry (recommended for simplicity):*
Add a public set `person-drift/failure-codes` in `person_drift.clj` that lists every keyword the namespace may emit.  Update existing code to assert that every `:code` it produces belongs to that set (cheap defensive invariant).  The bundle then derives the authoritative code universe from `person-drift/failure-codes`.

*Option B — static analysis:*
A build-time or test-time pass reads `src/abc/tools/person_drift.clj` and collects the keyword literals that appear as `:code` values.  More clever, fragile, and harder to maintain; not recommended.

**Data flow sketch:**

```text
            src/abc/tools/person_drift.clj
                      │
                      ▼
        person-drift/failure-codes : #{Keyword}
                      │
       ┌──────────────┼──────────────┐
       │              │              │
       ▼              ▼              ▼
   declared-map   run each fixture   actual codes
   fixture -> code-set                per fixture
       │              │              │
       └──────────────┼──────────────┘
                      ▼
       Invariants:
       1. union(actual code sets) ⊆ failure-codes
       2. ∀ fixture: actual(fixture) == declared(fixture)
       3. (future) every code in failure-codes fires on at least one fixture
```

The drift runner already enforces exact equality (2), so the refactor mainly removes the duplicated inline map and anchors the allowed code universe to `person_drift.clj`.

### 3.3 Minimal characterization tests (PROTECT)

These must be added **before** any structural change and must encode *current observable behavior*, including the Schematron subset asymmetry.

Add to `test/abc/tools/validate_design_bundle_test.clj`:

1. **`tei-schematron-rule-universe-snapshot-test`**  
   Assert that `(validate/schematron-rule-ids "schemas/tei-profile.sch")` returns exactly the 13 ids listed in §1.1.

2. **`tei-schematron-invalid-fixture-rule-id-snapshot-test`**  
   For each invalid fixture, run `schematron/validate!`, collect error rule ids, and assert equality with the observed snapshot:
   * `missing-title.xml` → `#{"abc-tei-header-title"}`
   * `missing-source-work-id.xml` → `#{"abc-tei-header-source-work-id"}`
   * `char-empty-decl.xml` → `#{"abc-char-resolution-form"}`
   * `gaiji-missing-ref.xml` → `#{"abc-gaiji-reference"}`
   * `gaiji-dangling-ref.xml` → `#{"abc-gaiji-chardecl-resolution"}`
   * `header-no-language.xml` → `#{"abc-header-language-declared"}`
   * `ruby-missing-reading.xml` → `#{"abc-ruby-complete"}`
   * `ruby-empty-base.xml` → `#{"abc-ruby-base-non-empty"}`
   * `ruby-empty-reading.xml` → `#{"abc-ruby-reading-non-empty"}`
   * `source-span-external-ref.xml` → `#{"abc-source-span-reference" "abc-source-span-target-exists"}`
   * `source-span-dangling-ref.xml` → `#{"abc-source-span-target-exists"}`

3. **`tei-schematron-warning-fixture-rule-id-snapshot-test`**  
   Same as (2) for the two warning fixtures, asserting the observed warning sets.

4. **`tei-schematron-subset-semantics-preservation-test`**  
   Assert that `validate/validate-tei-schematron!` returns `nil` when the declared set for `source-span-external-ref.xml` is `#{"abc-source-span-reference"}` (a strict subset of the actual rules).  This pins the current asymmetry so a later exact-match fix is a deliberate, reviewable change.

5. **`drift-failure-code-universe-snapshot-test`**  
   Once a `person-drift/failure-codes` registry exists, assert it equals the current known set (or, before that, snapshot every `:code` keyword found in `person_drift.clj`).

6. **`drift-invalid-fixture-code-snapshot-test`**  
   For each current drift invalid fixture, assert the actual code set equals the current map entries (matching §1.2).

7. **`drift-fixture-exact-match-preservation-test`**  
   Assert that a fixture whose expected set does not equal the actual set throws `clojure.lang.ExceptionInfo` (current behavior; already partially covered by `validate-drift-fixtures-rejects-unexpected-failure-codes-test`).

**Note on the existing test file:** `validate-tei-schematron-expected-findings-test` and `validate-drift-fixtures-smoke-test` already duplicate the production inline maps.  The first refactor step is to **replace those duplicates with references** to the same normalized fixture-declaration value, after the snapshot tests above are green.

---

## 4. RISKY changes — file separately, do NOT fold into the refactor

| Finding | Why it is risky / semantic | Suggested ticket |
|---------|----------------------------|------------------|
| `source-span-external-ref.xml` production map omits `abc-source-span-target-exists` (line 617–618) even though the fixture fires it. | Changing the production expectation to the full set is a correctness fix, not a structural simplification. It also tightens what the gate means. | "Schematron `source-span-external-ref` expected-set is incomplete" |
| `validate-schematron-invalid-fixture!` and `validate-schematron-warning-fixture!` only check for *missing* rules, allowing extra unexpected findings (lines 204–215, 217–228). | Tightening to exact equality changes pass/fail semantics for fixtures that may currently trigger additional rules. | "Enforce exact expected-rule equality for Schematron fixtures" |
| Person-drift negative fixture coverage is incomplete. | Adding a coverage invariant now would make the gate fail for ten uncovered codes; that requires new fixtures and possibly new test data. | "Add negative drift fixtures for uncovered failure codes" |
| Any future rule-id rename in `tei-profile.odd` that does not propagate to fixture declarations. | Not a current bug, but the whole reason for the spec; derivation protects against it. | (handled by this refactor) |
| Decision whether to derive from `.odd` or `.sch`. | Changing the canonical source changes the trust boundary; needs ADR/team sign-off if `.odd` is not already enforced as the source of truth at build time. | "Document canonical source for ABC Schematron rule ids" |

**Principle from `codebase-simplification`:** preserve observable behavior in the refactor patch; every item above is a separate, reviewable follow-up.

---

## 5. PATCH PLAN — sequenced, reversible ladder rungs

All steps assume the characterization tests from §3.3 are in place and green first.

### Rung 1 — PROTECT: add characterization tests
*File:* `test/abc/tools/validate_design_bundle_test.clj`  
Add the seven snapshot/preservation tests listed in §3.3.  Do not touch production code.  Run `clojure -M:test -n abc.tools.validate-design-bundle-test` (or `bin/kaocha --focus abc.tools.validate-design-bundle-test`) and confirm green.

### Rung 2 — normalize inputs: extract fixture-declaration values
*File:* `src/abc/tools/validate_design_bundle.clj`  
Move the Schematron and drift inline maps to named `def` values:

```clojure
(def ^:private tei-schematron-fixtures
  {:schema-path "schemas/tei-profile.sch"
   :valid-fixtures [...]
   :warning-fixtures {...}
   :invalid-fixtures {...}})

(def ^:private person-drift-invalid-fixtures
  {"fixtures/v0/invalid/drift/broken-index-target" #{:index-target-missing}
   ...})
```

Have `test/abc/tools/validate_design_bundle_test.clj` import and reuse these values instead of duplicating them.  This is a pure normalization; behavior is unchanged.

### Rung 3 — add pure derivation helpers (no callers yet)
*New file:* `src/abc/tools/design_bundle_expectations.clj` (or add to `validate_design_bundle.clj` if the team prefers fewer files).  
Implement:

* `(tei-schematron-rule-ids schema-path)` — parse `.sch`, return `#{"abc-..." ...}`.
* Placeholder for `(person-drift-failure-codes)` — initially reads a new `failure-codes` set from `abc.tools.person-drift`.

Add unit tests for these helpers in `test/abc/tools/validate_design_bundle_test.clj`.

### Rung 4 — wire derivation into `validate-tei-schematron!`
*File:* `src/abc/tools/validate_design_bundle.clj`  
Change `validate-tei-schematron!` so it:

1. Computes `all-rules` from the schema path via the new helper.
2. Runs fixtures and computes actual rule ids.
3. Asserts coverage: `(= all-rules (union actual-invalid actual-warnings))`.  
   *(Safe to add because current fixtures already cover all 13 rules, verified in §1.1.)*
4. Keeps the current per-fixture **subset** semantics to preserve observable behavior.  
   *(Do not switch to exact equality here — that is the risky semantic change in §4.)*

Update tests to assert the coverage invariant as well.

### Rung 5 — wire derivation into `validate-drift-fixtures!`
*File:* `src/abc/tools/validate_design_bundle.clj`/`src/abc/tools/person_drift.clj`  

1. In `person_drift.clj`, add a public `failure-codes` set containing every code the namespace may emit, and a small internal assertion (via a helper) that emitted failures are members of that set.
2. In `validate_design_bundle.clj`, change `validate-drift-fixtures!` to derive the allowed code universe from `person-drift/failure-codes`.
3. Assert that every actual code per fixture is in `failure-codes`.  
   *(Safe: it only narrows the declared sets to the authoritative list; it does not require new fixtures.)*
4. Do **not** yet assert full coverage (every code fires somewhere).  File that as the follow-up in §4.

### Rung 6 — VERIFY
Run the full gate and test suite:

```bash
nix run .#validate-design-bundle
bin/kaocha
```

Confirm exit 0 and that the only diffs are extractions/renames — no observable behavior changes.

### Rollback
Each rung is independently revertible via `git revert` of its commit.  Because characterization tests were added first, any unintended behavior change will fail immediately, making bisection trivial.

---

**Bottom line:** the accidental complexity is duplicated, hand-maintained expected-set knowledge in a load-bearing gate.  The smallest safe simplification is to derive the universe of Schematron rule ids from `tei-profile.sch` / `.odd` and the universe of drift codes from `person_drift.clj`, normalize the fixture-partition maps into one place shared by production and tests, and strictly separate any semantic tightening (exact-match assertions, missing fixtures) into follow-up commits.
