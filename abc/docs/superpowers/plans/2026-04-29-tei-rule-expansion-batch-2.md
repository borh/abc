# TEI Rule Expansion Batch 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three new ABC Schematron rules (`abc-ruby-reading-non-empty`, `abc-char-resolution-form`, `abc-header-language-declared`) to `schemas/tei-profile.odd` so the v0 TEI contract surface tightens against three real loopholes: empty `<rt>` content, empty `<char>` declarations, and undeclared header language.

**Architecture:** Extend the canonical TEI ODD with three new `<constraintSpec scheme="schematron">` blocks; regenerate `schemas/tei-profile.rng` + `schemas/tei-profile.sch` via the pinned `tei-profile-artifacts` Nix derivation (TEI Stylesheets v7.60.0 + Saxon-HE 12.9); add one invalid fixture per rule under `fixtures/tei/invalid/`; wire each fixture into the xmllint list, the project-RNG list (when structurally valid), and the Schematron invalid-fixtures map in `validate_design_bundle.clj`; add one deftest per rule in `schematron_test.clj`; mirror the new fixtures in `validate_design_bundle_test.clj`; update the rule inventory and fixture plan in `docs/tei-validation.md`. The flake's `tei-profile-drift` check continues to byte-diff committed RNG/SCH against ODD-derived artifacts; `nix flake check` is the gate.

**Tech Stack:** TEI ODD + ISO Schematron (XSLT 2.0 via Saxon-HE), Clojure (`abc.tools.schematron`), Nix flake (`tei-profile-artifacts`, `tei-profile-drift`).

---

## Constraints

- Rule idents must match `abc-[a-z0-9-]+` exactly. The flake's pattern-id canonicalization regex requires lowercase + digits + hyphens; mixed-case idents are silently dropped by the inherited-pattern filter. (See `bcacdb5` post-mortem in `docs/tei-validation.md`.)
- The v0 ABC Schematron evaluator (Saxon XPath 2.0) lacks `<sch:let>` and `role="nonfatal"` semantics — those rules cannot be promoted from inherited TEI patterns.
- Identity-stable: schemas/tei-profile.odd is the canonical contract; tei-profile.rng + tei-profile.sch are derived artifacts. The drift gate fails the build if committed artifacts diverge from ODD output.
- The fourth candidate from `docs/next-steps.md` (parser-IR scope-qualifier expectation tied to `explicit`/`inferred`/`grouped`/`mid-word`/`ambiguous`) is **out of scope** for this plan: there is no agreed TEI encoding for those parser-IR fields yet. A follow-up ADR must decide that mapping before a Schematron rule can be written.
- **Blast radius of `abc-header-language-declared`**: this rule fires at the document root (`tei:TEI`), so every existing fixture that lacks `profileDesc/langUsage/language` will newly emit a finding. As of writing, only `examples/v0/example-work/tei.xml` declares `langUsage`. All three valid fixtures (`rashomon-minimal.xml`, `source-span-local-ref.xml`, `transcription-enrichment-declared.xml`), every committed invalid fixture (except the new `header-no-language.xml`), and both warning fixtures need a minimal `<profileDesc><langUsage><language ident="ja">…</language></langUsage></profileDesc>` insertion before the rule lands. Task 4a below handles this prerequisite; if the rule lands before Task 4a, `valid-fixture-has-no-schematron-findings-test` (`test/abc/tools/schematron_test.clj:7`) goes red and so does every existing single-rule invalid-fixture deftest.
- **Fixture-RNG shape**: Project RNG (`schemas/tei-profile.rng`) requires `<idno type="aozora-work-id">…</idno>` inside `<publicationStmt>` and `<sourceDesc>` content as `<p>…</p>` (or a structured `<bibl>` block as in `examples/v0/example-work/tei.xml`). A bare `<idno>` directly under `<sourceDesc>` is rejected with `element idno: Relax-NG validity error : Did not expect element idno there`. Every fixture in this plan follows the `<publicationStmt>{…<idno type="aozora-work-id">…</idno>}</publicationStmt>` + `<sourceDesc><p>…</p></sourceDesc>` shape. Cross-check against `fixtures/tei/valid/rashomon-minimal.xml` whenever in doubt.

## File Structure

- Modify: `schemas/tei-profile.odd` — add three `<constraintSpec>` blocks + three `<item>` entries in the inventory list.
- Regenerated (do not hand-edit): `schemas/tei-profile.rng`, `schemas/tei-profile.sch`.
- Create: `fixtures/tei/invalid/ruby-empty-reading.xml`, `fixtures/tei/invalid/char-empty-decl.xml`, `fixtures/tei/invalid/header-no-language.xml`.
- Modify (Task 4a — `<langUsage>` insertion): `fixtures/tei/valid/rashomon-minimal.xml`, `fixtures/tei/valid/source-span-local-ref.xml`, `fixtures/tei/valid/transcription-enrichment-declared.xml`, every existing `fixtures/tei/invalid/*.xml` (except `header-no-language.xml`), and any existing `fixtures/tei/warnings/*.xml`.
- Modify: `src/abc/tools/validate_design_bundle.clj` — extend xmllint list, project-RNG list (only structurally valid fixtures), and the Schematron invalid-fixtures map.
- Modify: `test/abc/tools/schematron_test.clj` — add one `deftest` per new rule (exact-set assertions).
- Modify: `test/abc/tools/validate_design_bundle_test.clj` — mirror the new fixtures in the runtime parity test.
- Modify: `docs/tei-validation.md` — extend the Rule Inventory table, the Fixture Plan table, and bump the ten-rule baseline reference.
- Modify: `docs/next-steps.md` — drop "Further TEI rule expansion" candidate item; add a milestone entry; bump the count word by one (read-then-bump; do not hard-code a target).
- Optional, after the regenerated artifacts settle: stage them as part of the commit (the drift gate will require it).

---

### Task 1: Add `abc-ruby-reading-non-empty` to the ODD

**Files:**
- Modify: `schemas/tei-profile.odd` — add `<constraintSpec>` and inventory `<item>`.

- [ ] **Step 1: Locate the inventory list and append the new item**

Open `schemas/tei-profile.odd`. Find the `<list>` element that enumerates ABC rule labels (it currently ends with the `abc-source-span-target-exists` item). Append:

```xml
<item><label>abc-ruby-reading-non-empty</label> error: ruby reading components must not be empty (closes the &lt;rb&gt;猫&lt;/rb&gt;&lt;rt&gt;&lt;/rt&gt; loophole that abc-ruby-complete leaves open).</item>
```

- [ ] **Step 2: Add the constraintSpec**

Append a new `<constraintSpec>` next to the existing `abc-ruby-base-non-empty`:

```xml
<constraintSpec ident="abc-ruby-reading-non-empty" scheme="schematron">
  <constraint>
    <sch:rule context="tei:ruby/tei:rt">
      <sch:assert test="normalize-space(string(.)) != ''">tei:ruby/tei:rt must have non-empty content (abc-ruby-reading-non-empty)</sch:assert>
    </sch:rule>
  </constraint>
</constraintSpec>
```

- [ ] **Step 3: Commit just the ODD edit (no regeneration yet)**

```bash
git add schemas/tei-profile.odd
git commit -m "feat(tei): add abc-ruby-reading-non-empty to ODD"
```

This isolates the ODD edit so a regeneration step that fails leaves a recoverable point.

---

### Task 2: Add `abc-char-resolution-form` to the ODD

**Files:**
- Modify: `schemas/tei-profile.odd`

- [ ] **Step 1: Append inventory item**

```xml
<item><label>abc-char-resolution-form</label> error: tei:charDecl/tei:char must declare at least one of tei:mapping, tei:unicodeProp, tei:localProp, or tei:desc (closes the &lt;char xml:id="x"/&gt; empty-declaration loophole).</item>
```

- [ ] **Step 2: Append constraintSpec**

```xml
<constraintSpec ident="abc-char-resolution-form" scheme="schematron">
  <constraint>
    <sch:rule context="tei:charDecl/tei:char">
      <sch:assert test="(some $m in tei:mapping satisfies normalize-space(string($m)) != '') or (some $u in tei:unicodeProp satisfies normalize-space(string($u/@value)) != '') or (some $l in tei:localProp satisfies normalize-space(string($l/@value)) != '') or (some $d in tei:desc satisfies normalize-space(string($d)) != '')">tei:charDecl/tei:char must declare at least one non-empty resolution form: tei:mapping, tei:unicodeProp/@value, tei:localProp/@value, or tei:desc (abc-char-resolution-form)</sch:assert>
    </sch:rule>
  </constraint>
</constraintSpec>
```

The `some $X in tei:Y satisfies …` quantifier checks **every** child of each type, not just the first one. A pure `tei:desc[1]` test would pass `<char xml:id="x"><desc/><desc>real content</desc></char>` only if the first `<desc>` was non-empty, and would (worse) FAIL the same input if the first `<desc>` was empty even though a later one carried real content. The XPath 2.0 existential quantifier closes both loopholes: the assertion succeeds iff at least one child of any of the four element types is non-empty.

- [ ] **Step 3: Commit the ODD edit**

```bash
git add schemas/tei-profile.odd
git commit -m "feat(tei): add abc-char-resolution-form to ODD"
```

---

### Task 3: Add `abc-header-language-declared` to the ODD

**Files:**
- Modify: `schemas/tei-profile.odd`

- [ ] **Step 1: Append inventory item**

```xml
<item><label>abc-header-language-declared</label> error: teiHeader must declare at least one tei:profileDesc/tei:langUsage/tei:language with a non-empty @ident, so downstream tooling can route on document language.</item>
```

- [ ] **Step 2: Append constraintSpec**

The rule asserts presence at the document level by attaching the rule to the root `tei:TEI` (because `tei:teiHeader` would only fire on documents that already have a header):

```xml
<constraintSpec ident="abc-header-language-declared" scheme="schematron">
  <constraint>
    <sch:rule context="tei:TEI">
      <sch:assert test="tei:teiHeader/tei:profileDesc/tei:langUsage/tei:language[@ident and normalize-space(@ident) != '']">teiHeader must declare at least one tei:profileDesc/tei:langUsage/tei:language with non-empty @ident (abc-header-language-declared)</sch:assert>
    </sch:rule>
  </constraint>
</constraintSpec>
```

- [ ] **Step 3: Commit the ODD edit**

```bash
git add schemas/tei-profile.odd
git commit -m "feat(tei): add abc-header-language-declared to ODD"
```

---

### Task 4: Regenerate the derived RNG/SCH artifacts

**Files:**
- Regenerate: `schemas/tei-profile.rng`, `schemas/tei-profile.sch`.

- [ ] **Step 1: Run the regenerate app**

```bash
nix run .#regenerate-tei-profile
```

Expected: writes new `schemas/tei-profile.rng` and `schemas/tei-profile.sch` containing the three new patterns under their declared idents.

- [ ] **Step 2: Confirm the new pattern idents survived canonicalization**

```bash
grep -E 'pattern id="abc-(ruby-reading-non-empty|char-resolution-form|header-language-declared)"' schemas/tei-profile.sch
```

Expected: three matching lines. If any is missing, the canonicalization regex dropped it — re-check the ident is all lowercase/digits/hyphens (no uppercase, no underscores).

- [ ] **Step 3: Validate well-formedness**

```bash
xmllint --noout schemas/tei-profile.rng schemas/tei-profile.sch
```

Expected: no output (well-formed).

- [ ] **Step 4: Stage the regenerated artifacts**

```bash
git add schemas/tei-profile.rng schemas/tei-profile.sch
```

Do not commit yet — the fixtures + harness updates land in the same commit.

---

### Task 4a: Add `<langUsage>` to every existing fixture that does not yet declare one

The new `abc-header-language-declared` rule fires at `tei:TEI`. Every fixture that today lacks `profileDesc/langUsage/language[@ident]` must be updated *before* the rule's deftests run, or the existing `valid-fixture-has-no-schematron-findings-test` and every single-rule invalid-fixture deftest (which assert exact rule-id sets, not `contains?`) will fail.

**Files (modify each):**
- `fixtures/tei/valid/rashomon-minimal.xml`
- `fixtures/tei/valid/source-span-local-ref.xml`
- `fixtures/tei/valid/transcription-enrichment-declared.xml`
- `fixtures/tei/invalid/missing-title.xml`
- `fixtures/tei/invalid/missing-source-work-id.xml`
- `fixtures/tei/invalid/ruby-empty-base.xml`
- `fixtures/tei/invalid/ruby-missing-reading.xml`
- `fixtures/tei/invalid/gaiji-missing-ref.xml`
- `fixtures/tei/invalid/gaiji-dangling-ref.xml`
- `fixtures/tei/invalid/source-span-dangling-ref.xml`
- `fixtures/tei/invalid/source-span-external-ref.xml`
- `fixtures/tei/warnings/figure-missing-desc.xml` *(if it exists in the warnings/ dir; the schematron test references it)*
- `fixtures/tei/warnings/transcription-enrichment-undeclared.xml` *(same caveat)*

**DO NOT modify:**
- `examples/v0/example-work/tei.xml` — already has `langUsage`.
- The new `fixtures/tei/invalid/header-no-language.xml` — that fixture's purpose is to fail the new rule; it must NOT declare langUsage.

- [ ] **Step 1: First confirm the inventory**

The new rule requires `<language ident=...>` inside `<langUsage>`. A fixture can already carry `<profileDesc>` (e.g. for `<textClass>` or other profile children) and still fail the new rule, so grepping on `profileDesc` would under-report. Key on the actual rule requirement instead — `<language ident=`:

```bash
ls fixtures/tei/valid fixtures/tei/invalid fixtures/tei/warnings 2>/dev/null
grep -L '<language ident=' fixtures/tei/valid/*.xml fixtures/tei/invalid/*.xml fixtures/tei/warnings/*.xml 2>/dev/null
```

Expected: the second command lists exactly the files in the bullet list above (minus any that don't exist yet — `warnings/` may be present or absent depending on prior milestones; if absent, drop those two from the list). If the output also lists fixtures that DO have `<language ident=` but in a non-default-namespaced form (e.g. `<tei:language>`), inspect those manually before adding them to the migration set — TEI's default namespace and explicit `tei:` prefix both satisfy the rule, and rewriting one shape into the other could accidentally rotate fixture content.

- [ ] **Step 2: Insert `<profileDesc>` after `</fileDesc>` in each file**

For each file in the list above, locate the `</fileDesc>` closing tag inside `<teiHeader>` and insert immediately after it (with matching indentation):

```xml
    <profileDesc>
      <langUsage>
        <language ident="ja">日本語</language>
      </langUsage>
    </profileDesc>
```

For files whose `<teiHeader>` already contains `<encodingDesc>` after `<fileDesc>` (e.g. `transcription-enrichment-declared.xml`, `gaiji-*.xml` if they declare gaiji), insert `<profileDesc>` immediately *after* the `</encodingDesc>` closing tag (TEI mandates this order: fileDesc → encodingDesc → profileDesc).

- [ ] **Step 3: Re-run xmllint on every modified fixture**

```bash
xmllint --noout fixtures/tei/valid/*.xml fixtures/tei/invalid/*.xml fixtures/tei/warnings/*.xml 2>&1 || true
```

Expected: no output (well-formed). If any fixture is unparseable, fix the indentation/insertion point.

- [ ] **Step 4: Re-run the project-RNG validator on every modified fixture**

```bash
nix run .#validate-design-bundle 2>&1 | sed -n '/project RelaxNG/,/schematron/p'
```

Expected: `tei project rng validation ok`. If a fixture fails RNG, the `<profileDesc>` was inserted in the wrong place (TEI ordering: fileDesc, encodingDesc, profileDesc).

- [ ] **Step 5: Stage but do not commit**

```bash
git add fixtures/tei/
```

The Task 15 final commit bundles this change with the rest of the rule batch.

---

### Task 5: Write fixture for `abc-ruby-reading-non-empty`

**Files:**
- Create: `fixtures/tei/invalid/ruby-empty-reading.xml`

- [ ] **Step 1: Write the fixture**

Use `fixtures/tei/invalid/ruby-empty-base.xml` as the structural template. The body contains a `<ruby>` whose `<rt>` is whitespace-only:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">ruby empty reading fixture</title>
      </titleStmt>
      <publicationStmt>
        <publisher>ABC fixture</publisher>
        <idno type="aozora-work-id">000000</idno>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
    <profileDesc>
      <langUsage>
        <language ident="ja">日本語</language>
      </langUsage>
    </profileDesc>
  </teiHeader>
  <text>
    <body>
      <p><ruby><rb>猫</rb><rt>   </rt></ruby></p>
    </body>
  </text>
</TEI>
```

The shape mirrors `fixtures/tei/valid/rashomon-minimal.xml` (`<idno type="aozora-work-id">` inside `<publicationStmt>`, `<sourceDesc><p>…</p>`); only the `<rt>` payload differs.

- [ ] **Step 2: Confirm well-formedness AND project-RNG validity**

```bash
xmllint --noout fixtures/tei/invalid/ruby-empty-reading.xml
xmllint --noout --relaxng schemas/tei-profile.rng fixtures/tei/invalid/ruby-empty-reading.xml
```

Expected from the second command: `fixtures/tei/invalid/ruby-empty-reading.xml validates`. RNG-rejection of the fixture means the structural shape is wrong (likely `<idno>` placed under `<sourceDesc>` instead of `<publicationStmt>`) — fix before proceeding.

---

### Task 6: Write fixture for `abc-char-resolution-form`

**Files:**
- Create: `fixtures/tei/invalid/char-empty-decl.xml`

- [ ] **Step 1: Write the fixture**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">char empty decl fixture</title>
      </titleStmt>
      <publicationStmt>
        <publisher>ABC fixture</publisher>
        <idno type="aozora-work-id">000000</idno>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
    <encodingDesc>
      <charDecl>
        <char xml:id="empty-char">
          <desc/>
        </char>
      </charDecl>
    </encodingDesc>
    <profileDesc>
      <langUsage>
        <language ident="ja">日本語</language>
      </langUsage>
    </profileDesc>
  </teiHeader>
  <text>
    <body>
      <p>placeholder</p>
    </body>
  </text>
</TEI>
```

The fixture deliberately includes an empty `<desc/>` so it exercises the *non-emptiness* clause in the new rule (presence-only assertion would let this fixture pass). If the rule is later loosened back to presence-only, this fixture must be tightened to a fully empty `<char xml:id="empty-char"/>`.

- [ ] **Step 2: Confirm well-formedness AND project-RNG validity**

```bash
xmllint --noout fixtures/tei/invalid/char-empty-decl.xml
xmllint --noout --relaxng schemas/tei-profile.rng fixtures/tei/invalid/char-empty-decl.xml
```

Expected: `fixtures/tei/invalid/char-empty-decl.xml validates`.

---

### Task 7: Write fixture for `abc-header-language-declared`

**Files:**
- Create: `fixtures/tei/invalid/header-no-language.xml`

- [ ] **Step 1: Write the fixture**

This fixture must omit `profileDesc/langUsage/language` while otherwise being structurally valid TEI:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">header without language fixture</title>
      </titleStmt>
      <publicationStmt>
        <publisher>ABC fixture</publisher>
        <idno type="aozora-work-id">000000</idno>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p>placeholder</p>
    </body>
  </text>
</TEI>
```

This fixture intentionally omits `<profileDesc>` — that is the rule it targets. Do not add `<langUsage>` here.

- [ ] **Step 2: Confirm well-formedness AND project-RNG validity**

```bash
xmllint --noout fixtures/tei/invalid/header-no-language.xml
xmllint --noout --relaxng schemas/tei-profile.rng fixtures/tei/invalid/header-no-language.xml
```

Expected: `fixtures/tei/invalid/header-no-language.xml validates`. (Project RNG does not require `profileDesc`; only Schematron does.)

---

### Task 8: Wire fixtures into `validate-xml!` (xmllint list)

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj` — add three new paths to the `validate-xml!` arglist.

- [ ] **Step 1: Add the three paths**

Find the `(defn validate-xml! [] (run-command! "xmllint" "--noout" …))` list and append (in alphabetical order under `fixtures/tei/invalid/`):

```clojure
                "fixtures/tei/invalid/char-empty-decl.xml"
                "fixtures/tei/invalid/header-no-language.xml"
                "fixtures/tei/invalid/ruby-empty-reading.xml"
```

(Insert each line in alphabetical order among the existing invalid fixtures.)

- [ ] **Step 2: Compile-check via the bundle**

```bash
nix run .#validate-design-bundle 2>&1 | head -40
```

Expected: should pass through `xml fixtures ok`, then likely fail later in the Schematron step (because the harness has no expected-rule entry for the new fixtures yet). That failure is fine; the goal here is only that xmllint accepts the new files.

---

### Task 9: Wire fixtures into project-RNG list

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`

The project-RNG step rejects fixtures that are intentionally structurally invalid (no `<title>`, no `<rt>`). All three new fixtures are structurally valid TEI — they just trip business rules. They go into the project-RNG list.

- [ ] **Step 1: Add the three paths**

Find the `(validate-tei! "schemas/tei-profile.rng" […])` second call (the project-RNG step) and append the three new paths in alphabetical order alongside the other invalid fixtures already listed there.

- [ ] **Step 2: Run project-RNG step**

```bash
nix run .#validate-design-bundle 2>&1 | sed -n '/project RelaxNG/,/schematron/p'
```

Expected: `tei project rng validation ok`. If RNG fails on a new fixture, the fixture is structurally invalid and must be corrected.

---

### Task 10: Wire fixtures into the Schematron invalid-fixtures map

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`

- [ ] **Step 1: Add three entries**

Find the `:invalid-fixtures` map inside `validate-tei-schematron!` and add:

```clojure
"fixtures/tei/invalid/ruby-empty-reading.xml"
#{"abc-ruby-reading-non-empty"}
"fixtures/tei/invalid/char-empty-decl.xml"
#{"abc-char-resolution-form"}
"fixtures/tei/invalid/header-no-language.xml"
#{"abc-header-language-declared"}
```

- [ ] **Step 2: Run the bundle**

```bash
nix run .#validate-design-bundle 2>&1 | tail -30
```

Expected: `tei schematron validation ok` and final `design bundle validation ok`. If any rule does not fire, re-check that the fixture actually trips the assertion (e.g. `<rt>   </rt>` must be whitespace, not absent — absent `<rt>` would trip `abc-ruby-complete` instead).

---

### Task 11: Add Schematron deftests

**Files:**
- Modify: `test/abc/tools/schematron_test.clj`

- [ ] **Step 1: Write three deftests**

Use the existing `ruby-empty-base-fails-ruby-rule-test` (added in commit `bcacdb5`) as the template. For each new rule:

Use exact rule-id set equality (matching the existing rule-batch-1 deftests at `test/abc/tools/schematron_test.clj:88` and `:99`). `contains?` is too loose — once Task 4a lands, every fixture's rule-id set is well-known, so an extra unexpected finding (e.g. a header-language regression) must surface as a test failure rather than be silently swallowed.

```clojure
(deftest ruby-empty-reading-fails-ruby-reading-rule-test
  (testing "ruby with empty rt fails abc-ruby-reading-non-empty"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/ruby-empty-reading.xml"
                               :label "ruby-empty-reading"})]
      (is (= ["abc-ruby-reading-non-empty"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest char-empty-decl-fails-char-resolution-rule-test
  (testing "char with empty desc fails abc-char-resolution-form"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/char-empty-decl.xml"
                               :label "char-empty-decl"})]
      (is (= ["abc-char-resolution-form"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest header-no-language-fails-header-language-rule-test
  (testing "teiHeader without profileDesc/langUsage/language fails abc-header-language-declared"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/header-no-language.xml"
                               :label "header-no-language"})]
      (is (= ["abc-header-language-declared"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))
```

The `schema-path` symbol is the file-level binding at `test/abc/tools/schematron_test.clj:5`, so don't redeclare it in the new tests.

- [ ] **Step 2: Run focused tests via flake check**

```bash
nix flake check 2>&1 | tail -10
```

Expected: `all checks passed!` (focused tests + drift gate). If a test fails, look at the rendered findings to confirm the rule actually fired.

---

### Task 12: Mirror new fixtures in `validate_design_bundle_test.clj`

**Files:**
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Add the three entries to the runtime parity test**

Find `validate-tei-schematron-expected-findings-test` (around line 200 — the test that exercises the same `invalid-fixtures` map shape as the runtime). Append the same three fixture/rule entries used in Task 10.

- [ ] **Step 2: Run focused tests**

```bash
nix flake check 2>&1 | tail -5
```

Expected: still `all checks passed!`.

---

### Task 13: Update `docs/tei-validation.md`

**Files:**
- Modify: `docs/tei-validation.md`

- [ ] **Step 1: Extend the Rule Inventory table**

Append three rows in the existing format:

```markdown
| `abc-ruby-reading-non-empty` | `tei:ruby/tei:rt` must have non-empty content | error | Stops `<rb>猫</rb><rt></rt>` from passing the structural ruby check |
| `abc-char-resolution-form` | Each `tei:charDecl/tei:char` must declare at least one of `mapping`/`unicodeProp`/`localProp`/`desc` | error | Closes the empty-`<char xml:id="x"/>` loophole |
| `abc-header-language-declared` | `teiHeader//profileDesc/langUsage/language[@ident]` must be present and non-empty | error | Lets downstream tooling route on document language |
```

- [ ] **Step 2: Extend the Fixture Plan table**

Append three rows:

```markdown
| `fixtures/tei/invalid/ruby-empty-reading.xml` | Fails `abc-ruby-reading-non-empty` |
| `fixtures/tei/invalid/char-empty-decl.xml` | Fails `abc-char-resolution-form` |
| `fixtures/tei/invalid/header-no-language.xml` | Fails `abc-header-language-declared` |
```

- [ ] **Step 3: Bump the rule-count references**

Change "ten ABC `constraintSpec` pattern IDs" → "thirteen ABC `constraintSpec` pattern IDs" (or wherever "ten" appears as a count). Update the `Updated:` date stamp at the top to `2026-04-29 (rule batch 2)`.

---

### Task 14: Update `docs/next-steps.md`

**Files:**
- Modify: `docs/next-steps.md`

- [ ] **Step 1: Bump milestone count by exactly one**

Read the current count word at the top of `docs/next-steps.md` (currently "Fourteen contract-harness milestones complete:" but the legacy-namespaces plan also bumps this same line; whichever lands first advances the count). Replace whatever number-word is present with the next one (`Fourteen` → `Fifteen`, `Fifteen` → `Sixteen`, etc.). Do not hard-code a target — read first, bump by one, write.

- [ ] **Step 2: Add a milestone bullet**

Right after the ADR 0018 bullet, add:

```markdown
- **2026-04-29 TEI rule expansion batch 2.** Three new ABC Schematron
  rules added to `schemas/tei-profile.odd` and propagated through the
  pinned `tei-profile-artifacts` derivation: `abc-ruby-reading-non-empty`
  (mirrors the base check on `tei:rt`), `abc-char-resolution-form`
  (every `tei:charDecl/tei:char` declares at least one of
  `mapping`/`unicodeProp`/`localProp`/`desc`), and
  `abc-header-language-declared`
  (`teiHeader//profileDesc/langUsage/language[@ident]` is present and
  non-empty). Three invalid fixtures + three deftests cover the new
  rules; the inventory in `docs/tei-validation.md` advances from ten
  to thirteen ABC patterns. The fourth candidate from the previous
  milestone (parser-IR scope-qualifier expectation) is deferred — it
  needs a separate ADR to decide how
  `explicit`/`inferred`/`grouped`/`mid-word`/`ambiguous` are encoded
  in TEI before a Schematron rule can be written. The v0 evaluator's
  `<sch:let>` and `role="nonfatal"` ceiling on inherited TEI rules
  remains.
```

- [ ] **Step 3: Drop the "Further TEI rule expansion" candidate**

Remove the candidate item that starts with `**Further TEI rule expansion.**`. Renumber the remaining candidate items so they run 1, 2, 3 again.

---

### Task 15: Final validation + commit

- [ ] **Step 1: Stage everything**

```bash
git add schemas/tei-profile.odd schemas/tei-profile.rng schemas/tei-profile.sch fixtures/tei/ src/abc/tools/validate_design_bundle.clj test/abc/tools/schematron_test.clj test/abc/tools/validate_design_bundle_test.clj docs/tei-validation.md docs/next-steps.md
```

`fixtures/tei/` is staged as a whole (not just the three new invalid fixtures) because Task 4a edited every existing fixture that lacked `<langUsage>`. If those edits aren't staged, the new `abc-header-language-declared` rule will fire on the existing valid fixtures and the deftests will fail.

- [ ] **Step 2: Run final validation**

```bash
nix run .#validate-design-bundle && nix flake check
```

Expected: bundle prints `design bundle validation ok`; flake check prints `all checks passed!`.

- [ ] **Step 3: Squash earlier per-rule ODD commits if you used Tasks 1–3 commits**

If Tasks 1–3 each committed, interactive rebase those plus this final commit into one logical "feat(tei): rule batch 2" commit. (If the agent worked everything into a single staged change, skip this.)

- [ ] **Step 4: Commit**

```bash
git commit -m "$(cat <<'EOF'
feat(tei): Schematron rule batch 2 — empty rt / empty char / header language

Three new ABC Schematron rules added to schemas/tei-profile.odd and
propagated through the pinned tei-profile-artifacts derivation:
abc-ruby-reading-non-empty, abc-char-resolution-form, and
abc-header-language-declared. Three invalid fixtures + three deftests
cover the new rules; the rule inventory advances from ten to thirteen
ABC patterns. The parser-IR scope-qualifier candidate is deferred to
a future ADR.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Self-Review

- **Spec coverage** — three rules from the next-steps candidate list landed; the fourth (parser-IR scope-qualifier) is explicitly deferred with the reason. ✓
- **Placeholders** — none. Each task lists exact paths, exact code, exact commands. ✓
- **Type/ident consistency** — `abc-ruby-reading-non-empty`, `abc-char-resolution-form`, `abc-header-language-declared` are referenced identically in ODD, fixture lists, deftests, docs, and the bundle test. All are lowercase + digits + hyphens and pass the canonicalization regex. ✓
- **Drift gate** — Task 4 regenerates RNG/SCH and Task 15 stages them; the flake's `tei-profile-drift` check would otherwise fail. ✓
- **`abc-header-language-declared` blast radius** — Task 4a explicitly migrates every existing fixture before the rule's deftests run. The negative fixture (`header-no-language.xml`) is the only fixture that does not get `<langUsage>`. ✓
- **Fixture-RNG shape** — every new invalid fixture mirrors `fixtures/tei/valid/rashomon-minimal.xml`'s structural skeleton (`<idno type="aozora-work-id">` inside `<publicationStmt>`, `<sourceDesc><p>…</p>`). Each Step 2 in Tasks 5/6/7 runs `xmllint --relaxng schemas/tei-profile.rng` so a malformed shape fails before the bundle does. ✓
- **`abc-char-resolution-form` non-emptiness** — the rule asserts both presence AND non-empty content (or non-empty `@value`/text), so `<desc/>` and `<unicodeProp/>` empty shells fail. The fixture in Task 6 deliberately uses `<desc/>` to exercise the non-emptiness clause. ✓
- **Deftests** — exact rule-id sets via `(is (= ["rule-id"] (mapv :rule-id findings)))`, matching the existing `ruby-empty-base-fails-base-non-empty-rule-test` pattern. `contains?` is not used. ✓
- **`docs/next-steps.md` count conflict with legacy-namespaces plan** — Task 14 Step 1 reads-then-bumps rather than hard-coding "Fourteen → Fifteen", so order-of-landing relative to the legacy plan is invariant. ✓
- **Risk** — low for the rules themselves, medium for Task 4a (any committed fixture missed by the inventory grep will silently flip the schematron test red). The grep at Task 4a Step 1 is the safety net.
