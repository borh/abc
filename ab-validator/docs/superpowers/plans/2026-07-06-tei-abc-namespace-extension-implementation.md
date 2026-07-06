# ABC TEI Namespace Extension Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the ABC TEI namespace extension so the current 120 TEI-profile policy rows can be admitted by profile/rendering evidence instead of remaining classified-but-not-admitted.

**Architecture:** ABC remains the owner of TEI profile, renderer, materializer, and preservation sidecar shape. ab-validator syncs ABC schema evidence and updates the coverage gate so the four current TEI-profile families move into an `admitted_by_tei_profile` bucket only when ABC profile evidence is present.

**Tech Stack:** Clojure (`abc` renderer/materializer/tests), JSON Schema 2020-12, TEI ODD/Schematron text fixtures, Python coverage reporter, Bash smoke tests, Nix/just verification.

## Global Constraints

- ABC owns TEI profile/rendering; ab-validator owns measurement and admission reporting.
- The extension namespace is `https://w3id.org/abc/ns/tei` with `abc:vocab-version="0"`.
- The v0 extension is attributes-only; no custom `abc:*` body elements.
- Plaintext output remains metadata-free.
- `preservation.json` remains the authoritative audit ledger; XML attributes are derived projections.
- Current implementation target is the generated report's 120 policy rows: accent 14, figure_metadata 85, heading_jisage_structure 7, style_rendition 14.
- Custom-sidecar rows are already covered by the ABC preservation contract and must remain admitted separately.
- ab-validator may confirm the ABC preservation contract only from the trusted synced schema path `data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json`, with a matching canonical document hash. A temp schema with the same id/version is evidence only.
- TEI profile projection support requires `tei_profile_projection` in both the record `class` enum and `coverage.classes` enum. Do not union the two fields when deciding support.
- Generated report paths for repo-local contract files must be repo-relative so regenerating from a feature worktree and `main` produces stable committed artifacts.

---

### Task 1: ABC Profile And Preservation Contract

**Files:**
- Modify: `/home/bor/Projects/abc/schemas/tei-profile.odd`
- Modify: `/home/bor/Projects/abc/schemas/parser-ir-publication-preservation.schema.json`
- Modify: `/home/bor/Projects/abc/test/abc/tools/materialize_publication_test.clj`
- Modify: `/home/bor/Projects/abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces: preservation schema version `0.2.0` with `coverage.classes` accepting `custom_sidecar` and `tei_profile_projection`.
- Produces: record `class` enum accepting `custom_sidecar` and `tei_profile_projection`.
- Produces: profile contract text declaring ABC namespace attributes and `abc:vocab-version="0"`.

- [ ] **Step 1: Write failing ABC schema/profile tests**

Add assertions to `materialize_publication_test.clj` that expect:

```clojure
(is (= "0.2.0" (get preservation "schema_version")))
(is (some #{"tei_profile_projection"} (get-in preservation ["coverage" "classes"])))
(is (some #(= "tei_profile_projection" (get % "class"))
          (get preservation "records")))
```

Add fixture expectations to `validate_design_bundle_test.clj` only if the existing Schematron fixture snapshot requires explicit fixture registration.

- [ ] **Step 2: Run failing ABC tests**

Run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner abc.tools.materialize-publication-test
```

Expected: failure showing preservation schema version/classes/records still `0.1.0` / `custom_sidecar` only.

- [ ] **Step 3: Widen schema and profile**

Update `schemas/parser-ir-publication-preservation.schema.json`:

```json
"schema_version": { "const": "0.2.0" }
```

Change `coverage.classes.items.enum` to:

```json
["custom_sidecar", "tei_profile_projection"]
```

Change `record.class` from const to enum:

```json
"class": { "enum": ["custom_sidecar", "tei_profile_projection"] }
```

Change `record.construct.enum` to include:

```json
"accent",
"figure_metadata",
"heading_jisage_structure",
"style_rendition"
```

Update `schemas/tei-profile.odd` to document the `abc` namespace, `abc:vocab-version`, `abc:preservation-record`, `abc:accent-code`, `abc:style-source`, `abc:layout-kind`, `abc:layout-params`, and `abc:figure-class`.

- [ ] **Step 4: Run ABC schema/profile tests**

Run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner abc.tools.materialize-publication-test
```

Expected: materialization tests pass.

- [ ] **Step 5: Commit ABC profile/contract slice**

Run:

```bash
cd /home/bor/Projects/abc
git add schemas/tei-profile.odd schemas/parser-ir-publication-preservation.schema.json test/abc/tools/materialize_publication_test.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): declare ABC TEI namespace contract"
```

### Task 2: ABC Renderer And Materializer Projection Evidence

**Files:**
- Modify: `/home/bor/Projects/abc/src/abc/tools/parser_ir_tei.clj`
- Modify: `/home/bor/Projects/abc/src/abc/tools/materialize_publication.clj`
- Modify: `/home/bor/Projects/abc/src/abc/tools/tei_header.clj`
- Modify: `/home/bor/Projects/abc/test/abc/tools/parser_ir_tei_test.clj`
- Modify: `/home/bor/Projects/abc/test/abc/tools/materialize_publication_test.clj`

**Interfaces:**
- Produces: TEI root with ABC namespace declaration and `abc:vocab-version="0"`.
- Produces: renderer attributes for layout/profile projection where parser-IR exposes layout/style facts.
- Produces: `tei_profile_projection` preservation records with deterministic `record_id`s.

- [ ] **Step 1: Write failing renderer/materializer tests**

Update `parser_ir_tei_test.clj` expected Hiccup for layout spans and paragraph layouts to include real `:abc/layout-kind` and `:abc/layout-params` attributes.

Update `materialize_publication_test.clj` to assert generated `tei.xml` includes:

```text
xmlns:abc="https://w3id.org/abc/ns/tei"
abc:vocab-version="0"
abc:preservation-record="r
```

and generated `preservation.json` has at least one record with:

```json
{"class": "tei_profile_projection"}
```

- [ ] **Step 2: Run failing ABC tests**

Run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner abc.tools.parser-ir-tei-test abc.tools.materialize-publication-test
```

Expected: failures showing no ABC namespace attributes and no `tei_profile_projection` records.

- [ ] **Step 3: Implement root namespace serialization**

Update `abc.tools.tei-header/attr-key` so namespaced Hiccup attributes such as `:abc/vocab-version` serialize as `abc:vocab-version`, and ensure `tei-document` in materialization includes:

```clojure
[:TEI {:xmlns:abc "https://w3id.org/abc/ns/tei"
       :abc/vocab-version "0"}
 header body]
```

- [ ] **Step 4: Implement TEI projection attributes**

Update `parser_ir_tei.clj` to produce:

- `:abc/layout-kind` and `:abc/layout-params` for paragraph layout.
- `:abc/layout-kind` and `:abc/layout-params` for `layout-span`.
- `:abc/style-source` when rendering emphasis `style`.
- `:abc/figure-class` and graphic `width` / `height` when image nodes expose those fields.

Do not emit `:abc/preservation-record` in the renderer, because record ids are assigned by the materializer.

- [ ] **Step 5: Implement projection preservation records**

Update `materialize_publication.clj` to append deterministic `tei_profile_projection` records for parser-IR fields that correspond to the four covered families. Use the existing `preservation-record` helper, widened to accept `:class`, and ensure `coverage.classes` contains both classes when both are present.

- [ ] **Step 6: Run ABC tests**

Run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner abc.tools.parser-ir-tei-test abc.tools.materialize-publication-test
nix run .#validate-design-bundle
```

Expected: tests pass and design bundle validates.

- [ ] **Step 7: Commit ABC renderer/materializer slice**

Run:

```bash
cd /home/bor/Projects/abc
git add src/abc/tools/parser_ir_tei.clj src/abc/tools/materialize_publication.clj src/abc/tools/tei_header.clj test/abc/tools/parser_ir_tei_test.clj test/abc/tools/materialize_publication_test.clj schemas/parser-ir-publication-preservation.schema.json
git commit -m "feat(parser-ir): emit ABC TEI profile projection evidence"
```

### Task 3: ab-validator Coverage Admission And Schema Sync

**Files:**
- Modify: `data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json`
- Modify: `reports/parser-ir/publication-coverage.py`
- Modify: `tests/parser-ir-publication-coverage-smoke.sh`
- Regenerate: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Regenerate: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`

**Interfaces:**
- Consumes: ABC preservation schema id `https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json`, version `0.2.0`.
- Produces: `tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"`.
- Produces: `closure_gaps.admitted_by_tei_profile` for the four covered families.

- [ ] **Step 1: Write failing coverage smoke assertions**

Update `tests/parser-ir-publication-coverage-smoke.sh` so a confirmed ABC schema with `tei_profile_projection` support admits the style, figure, heading/jisage, and accent fixture rows into `closure_gaps.admitted_by_tei_profile`.

Add assertions:

```bash
jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_CANDIDATE_PROVIDED"' "$spoofed_summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_MISSING"' "$spoofed_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.count == 0' "$spoofed_summary_json" >/dev/null

jq -e '.custom_contract.verdict == "CUSTOM_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$confirmed_summary_json" >/dev/null
jq -e '.custom_contract.trusted_schema_path_match == true' "$confirmed_summary_json" >/dev/null
jq -e '.custom_contract.contract_hash == .custom_contract.trusted_schema_hash' "$confirmed_summary_json" >/dev/null
jq -e '.tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"' "$confirmed_summary_json" >/dev/null
jq -e '.closure_gaps.admitted_by_tei_profile.count == 4' "$confirmed_summary_json" >/dev/null
jq -e '.closure_gaps.classified_but_not_admitted.count == 2' "$confirmed_summary_json" >/dev/null
```

Also add a focused Python assertion that a schema with only `coverage.classes = ["tei_profile_projection"]` but no matching record class is `TEI_PROFILE_CONTRACT_INCOMPLETE`.

- [ ] **Step 2: Run failing smoke**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
```

Expected: failure because the reporter has no TEI-profile admission bucket.

- [ ] **Step 3: Implement TEI profile contract detection**

Sync the ABC schema into `data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json`.

Update `publication-coverage.py` so `custom_contract_block` confirms the contract only when all of these hold:

- Schema id is `https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json`.
- Schema version is `0.2.0`.
- The supplied path resolves to the trusted synced schema path.
- The supplied contract hash equals the canonical hash of the trusted synced schema.

Return a separate `tei_profile_contract` block in the summary. It is confirmed only when `tei_profile_projection` appears independently in both `record.class` enum and `coverage.classes` enum, and all four TEI-profile constructs are present.

- [ ] **Step 4: Implement `admitted_by_tei_profile`**

Update `closure_gaps` so, when the TEI profile contract is confirmed, rows for families `accent`, `figure_metadata`, `heading_jisage_structure`, and `style_rendition` are moved into `admitted_by_tei_profile`.

- [ ] **Step 5: Run ab-validator tests and regenerate report**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
just parser-ir-publication-coverage-report
python3 -m py_compile reports/parser-ir/publication-coverage.py
git diff --check
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"
  and .custom_contract.trusted_schema_path_match == true
  and .custom_contract.contract_hash == .custom_contract.trusted_schema_hash
  and .custom_contract.trusted_schema_path == "data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json"
  and .tei_profile_contract.verdict == "TEI_PROFILE_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
  and .closure_gaps.admitted_by_custom_contract.count == 31
  and .closure_gaps.admitted_by_tei_profile.count == 120
  and .closure_gaps.classified_but_not_admitted.count == 0
  and .closure_gaps.true_unsupported_gaps.count == 0' \
  docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected: smoke passes, regenerated summary has `closure_gaps.classified_but_not_admitted.count == 0` for the current measured report, and syntax/whitespace checks pass.

- [ ] **Step 6: Commit ab-validator slice**

Run:

```bash
git add data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json reports/parser-ir/publication-coverage.py tests/parser-ir-publication-coverage-smoke.sh docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json docs/superpowers/reports/2026-07-06-ir-publication-coverage.md docs/superpowers/specs/2026-07-06-tei-abc-namespace-extension-design.md docs/superpowers/plans/2026-07-06-tei-abc-namespace-extension-implementation.md
git commit -m "feat(parser-ir): admit ABC TEI profile projection evidence"
```

### Task 4: Integration, Merge, And Final Verification

**Files:**
- Modify only if needed after verification failures.

**Interfaces:**
- Produces: feature branch merged into `main`.

- [ ] **Step 1: Run full focused verification**

Run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner abc.tools.parser-ir-tei-test abc.tools.materialize-publication-test
nix run .#validate-design-bundle
cd /home/bor/Projects/ab-validator/.worktrees/tei-abc-namespace
bash tests/parser-ir-publication-coverage-smoke.sh
just parser-ir-publication-coverage-smoke
python3 -m py_compile reports/parser-ir/publication-coverage.py
git diff --check
```

Expected: all commands exit 0.

- [ ] **Step 2: Merge ab-validator feature branch to main**

Run:

```bash
cd /home/bor/Projects/ab-validator
git checkout main
git merge --ff-only feat/tei-abc-namespace
```

- [ ] **Step 3: Final main verification**

Run:

```bash
cd /home/bor/Projects/ab-validator
bash tests/parser-ir-publication-coverage-smoke.sh
git status --short --branch
cd /home/bor/Projects/abc
git status --short --branch
```

Expected: ab-validator smoke passes. ab-validator `main` includes the feature branch commits. ABC has local commits on `main`.

## Self-Review

- Spec coverage: implements namespace version, profile contract, sidecar widening, renderer projection attributes, cross-file evidence, coverage admission, and plaintext non-regression.
- Scope: limited to the current four measured TEI-profile families and existing custom sidecar behavior.
- Placeholder scan: this plan contains no incomplete task markers beyond intentional checkbox syntax.
- Type consistency: record class names are `custom_sidecar` and `tei_profile_projection`; coverage bucket names are `admitted_by_custom_contract`, `admitted_by_tei_profile`, `classified_but_not_admitted`, and `true_unsupported_gaps`.
