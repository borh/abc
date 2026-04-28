# TEI Schematron Validation Harness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `nix run .#validate-design-bundle` enforce the ADR 0012 TEI validation contract with project Relax NG plus Schematron rule checks.

**Architecture:** Keep Jing as the Relax NG compatibility layer in `abc.tools.tei`. Add `abc.tools.schematron` as a small in-process Schematron evaluator for the subset ABC uses: parse `schemas/tei-profile.sch`, require one `sch:rule` per `sch:pattern` until ISO rule-claim semantics are implemented, evaluate each rule context and `sch:assert`/`sch:report` test using Saxon XPath 2.0, and return structured findings keyed by pattern ID. Wire `abc.tools.validate-design-bundle` to validate the canonical example plus valid/warning/invalid TEI fixtures and to fail when expected Schematron failures are missing.

**Tech Stack:** Clojure 1.12, Jing, Saxon-HE 9.6.0-4 already present in the lockfile, Java DOM namespace-aware XML parsing, `clojure.test`, existing Nix/clj-nix harness. Saxon is declared explicitly at the existing locked version for this milestone; upgrading to Saxon 12.x is a separate dependency-refresh decision.

---

## File Structure

- Create `src/abc/tools/schematron.clj`: parses `.sch` files and evaluates rules against XML documents.
- Create `test/abc/tools/schematron_test.clj`: TDD coverage for valid, error, and warning fixtures.
- Create additional TEI fixtures for the previously uncovered rules: source work ID, source-span references, and transcription-vs-annotation declarations.
- Modify `schemas/tei-profile.odd` and `schemas/tei-profile.sch`: use portable pattern IDs as rule IDs and keep the transcription rule's XPath evaluable outside XSLT-specific `current()`.
- Modify `src/abc/tools/validate_design_bundle.clj`: validate new JSON schema/sidecar fixtures, XML fixtures, ODD-derived schema artifacts, and Schematron expected outcomes.
- Modify `test/abc/tools/validate_design_bundle_test.clj`: TDD coverage for expected Schematron failure/warning partitioning and harness loud-fail behavior.
- Modify `deps.edn`: add explicit Saxon dependency so `abc.tools.schematron` does not rely on transitive dependencies.
- Modify `nix/clj-nix-deps.edn`: add Saxon and include `abc.tools.schematron-test` in focused tests.
- Modify `flake.nix`: add contract-surface checks for `src/abc/tools/schematron.clj`, `test/abc/tools/schematron_test.clj`, `schemas/tei-profile.sch`, `schemas/tei-profile.rng`, and `schemas/tei-validation-result.schema.json`.

## Task 1: Add Missing Rule Fixtures and Normalize Rule IDs

**Files:**
- Create: `fixtures/tei/invalid/missing-source-work-id.xml`
- Create: `fixtures/tei/invalid/source-span-external-ref.xml`
- Create: `fixtures/tei/valid/source-span-local-ref.xml`
- Create: `fixtures/tei/warnings/transcription-enrichment-undeclared.xml`
- Create: `fixtures/tei/valid/transcription-enrichment-declared.xml`
- Modify: `schemas/tei-profile.odd`
- Modify: `schemas/tei-profile.sch`

- [ ] **Step 1: Add a source-work-id negative fixture**

Create `fixtures/tei/invalid/missing-source-work-id.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">羅生門</title>
      </titleStmt>
      <publicationStmt>
        <p>ABC fixture with no source work identifier.</p>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p>Missing source work ID.</p>
    </body>
  </text>
</TEI>
```

- [ ] **Step 2: Add source-span positive and negative fixtures**

Create `fixtures/tei/valid/source-span-local-ref.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">羅生門</title>
      </titleStmt>
      <publicationStmt>
        <idno type="aozora-work-id">000127</idno>
        <p>ABC fixture.</p>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p xml:id="span-1" source="#span-1">Local source span reference.</p>
    </body>
  </text>
</TEI>
```

Create `fixtures/tei/invalid/source-span-external-ref.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">羅生門</title>
      </titleStmt>
      <publicationStmt>
        <idno type="aozora-work-id">000127</idno>
        <p>ABC fixture.</p>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p source="https://example.invalid/span-1">External source span reference.</p>
    </body>
  </text>
</TEI>
```

- [ ] **Step 3: Add transcription-vs-annotation positive and warning fixtures**

Create `fixtures/tei/valid/transcription-enrichment-declared.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">羅生門</title>
      </titleStmt>
      <publicationStmt>
        <idno type="aozora-work-id">000127</idno>
        <p>ABC fixture.</p>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
    <encodingDesc>
      <tagsDecl>
        <namespace name="http://www.tei-c.org/ns/1.0">
          <tagUsage gi="w"/>
        </namespace>
      </tagsDecl>
    </encodingDesc>
  </teiHeader>
  <text>
    <body>
      <p><w>宣言済み</w></p>
    </body>
  </text>
</TEI>
```

Create `fixtures/tei/warnings/transcription-enrichment-undeclared.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title type="main" xml:lang="ja">羅生門</title>
      </titleStmt>
      <publicationStmt>
        <idno type="aozora-work-id">000127</idno>
        <p>ABC fixture.</p>
      </publicationStmt>
      <sourceDesc>
        <p>Aozora Bunko source fixture.</p>
      </sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>
      <p><w>未宣言</w></p>
    </body>
  </text>
</TEI>
```

- [ ] **Step 4: Make the transcription rule pure XPath 2.0**

In both `schemas/tei-profile.odd` and `schemas/tei-profile.sch`, replace:

```xml
tei:tagUsage[@gi = local-name(current())]
```

with:

```xml
tei:tagUsage[@gi = local-name()]
```

The custom evaluator uses Saxon XPath directly, not an XSLT-compiled Schematron pipeline, so `current()` is intentionally avoided in v0 rule text.

- [ ] **Step 5: Stop relying on non-portable assert/report IDs**

In `schemas/tei-profile.sch`, remove `id="..."` from every `sch:assert` and `sch:report`. The evaluator keys findings from the containing `sch:pattern/@id`, which matches the portable convention this plan enforces. Leave `sch:pattern/@id` unchanged.

- [ ] **Step 6: Verify fixture XML parses**

Run:

```bash
xmllint --noout schemas/tei-profile.odd schemas/tei-profile.sch fixtures/tei/invalid/missing-source-work-id.xml fixtures/tei/invalid/source-span-external-ref.xml fixtures/tei/valid/source-span-local-ref.xml fixtures/tei/warnings/transcription-enrichment-undeclared.xml fixtures/tei/valid/transcription-enrichment-declared.xml
```

Expected: no output and exit code 0.

- [ ] **Step 7: Commit fixtures and rule normalization**

```bash
git add schemas/tei-profile.odd schemas/tei-profile.sch fixtures/tei/invalid/missing-source-work-id.xml fixtures/tei/invalid/source-span-external-ref.xml fixtures/tei/valid/source-span-local-ref.xml fixtures/tei/warnings/transcription-enrichment-undeclared.xml fixtures/tei/valid/transcription-enrichment-declared.xml
git commit -m "test: cover remaining TEI Schematron rules"
```

## Task 2: Add Schematron Evaluator Tests

**Files:**
- Create: `test/abc/tools/schematron_test.clj`
- Modify later: `src/abc/tools/schematron.clj`

- [ ] **Step 1: Write the failing test**

Create `test/abc/tools/schematron_test.clj`:

```clojure
(ns abc.tools.schematron-test
  (:require [abc.tools.schematron :as schematron]
            [clojure.test :refer [deftest is testing]]))

(def schema-path "schemas/tei-profile.sch")

(deftest valid-fixture-has-no-schematron-findings-test
  (testing "valid TEI fixtures have no Schematron findings"
    (doseq [path ["examples/v0/example-work/tei.xml"
                  "fixtures/tei/valid/rashomon-minimal.xml"
                  "fixtures/tei/valid/source-span-local-ref.xml"
                  "fixtures/tei/valid/transcription-enrichment-declared.xml"]]
      (let [{:keys [label findings]} (schematron/validate!
                                      {:schema-path schema-path
                                       :xml-path path
                                       :label path})]
        (is (= path label))
        (is (= [] findings) (str path " should have no findings"))))))

(deftest missing-title-fails-title-rule-test
  (testing "missing title fixture fails abc-tei-header-title"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/missing-title.xml"
                               :label "missing-title"})]
      (is (= ["abc-tei-header-title"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings)))
      (is (re-find #"main title" (:message (first findings)))))))

(deftest ruby-missing-reading-fails-ruby-rule-test
  (testing "ruby without rt fails abc-ruby-complete"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/ruby-missing-reading.xml"
                               :label "ruby"})]
      (is (= ["abc-ruby-complete"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest gaiji-missing-reference-fails-gaiji-rule-test
  (testing "g without ref/corresp/ana fails abc-gaiji-reference"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/gaiji-missing-ref.xml"
                               :label "gaiji"})]
      (is (= ["abc-gaiji-reference"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest missing-source-work-id-fails-source-id-rule-test
  (testing "header without Aozora work ID fails abc-tei-header-source-work-id"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/missing-source-work-id.xml"
                               :label "missing-source-id"})]
      (is (= ["abc-tei-header-source-work-id"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest source-span-external-reference-fails-source-span-rule-test
  (testing "source attributes must point to local span identifiers"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/source-span-external-ref.xml"
                               :label "source-span"})]
      (is (= ["abc-source-span-reference"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest figure-missing-description-reports-warning-test
  (testing "figure without textual description reports abc-figure-accessibility warning"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/warnings/figure-missing-desc.xml"
                               :label "figure"})]
      (is (= ["abc-figure-accessibility"]
             (mapv :rule-id findings)))
      (is (= [:warning]
             (mapv :severity findings)))
      (is (= [:report]
             (mapv :kind findings))))))

(deftest transcription-enrichment-undeclared-reports-warning-test
  (testing "undeclared transcription enrichment reports abc-transcription-vs-annotation warning"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                               :label "transcription"})]
      (is (= ["abc-transcription-vs-annotation"]
             (mapv :rule-id findings)))
      (is (= [:warning]
             (mapv :severity findings)))
      (is (= [:assert]
             (mapv :kind findings))))))

(deftest multi-rule-pattern-is-rejected-test
  (testing "v0 evaluator rejects patterns with more than one rule until ISO claim semantics exist"
    (let [tmp (java.io.File/createTempFile "abc-sch-multi-rule" ".sch")]
      (try
        (spit tmp (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                       "<sch:schema xmlns:sch=\"http://purl.oclc.org/dsdl/schematron\">"
                       "  <sch:pattern id=\"two-rules\">"
                       "    <sch:rule context=\"*\"><sch:assert test=\"true()\">ok</sch:assert></sch:rule>"
                       "    <sch:rule context=\"*\"><sch:assert test=\"true()\">ok</sch:assert></sch:rule>"
                       "  </sch:pattern>"
                       "</sch:schema>"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"one sch:rule per sch:pattern"
             (schematron/parse-schema (str tmp))))
        (finally
          (.delete tmp))))))
```

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.schematron-test) (clojure.test/run-tests 'abc.tools.schematron-test)"
```

Expected: load failure because `abc.tools.schematron` does not exist.

- [ ] **Step 3: Commit the failing test only**

```bash
git add test/abc/tools/schematron_test.clj
git commit -m "test: add Schematron validation expectations"
```

## Task 3: Implement `abc.tools.schematron`

**Files:**
- Create: `src/abc/tools/schematron.clj`
- Modify: `deps.edn`
- Modify: `nix/clj-nix-deps.edn`
- Test: `test/abc/tools/schematron_test.clj`

- [ ] **Step 1: Add explicit Saxon dependency**

In `deps.edn`, add this dependency next to `org.relaxng/jing`:

```clojure
  ;; Explicit for the v0 Schematron harness. This intentionally matches the
  ;; Saxon version already present in deps-lock.json via the OWL stack; upgrade
  ;; to Saxon 12.x in a separate dependency-refresh change.
net.sf.saxon/Saxon-HE              {:mvn/version "9.6.0-4"}
```

In `nix/clj-nix-deps.edn`, add this dependency next to `org.relaxng/jing`:

```clojure
;; Explicit for the v0 Schematron harness; aligned with the existing lockfile.
net.sf.saxon/Saxon-HE {:mvn/version "9.6.0-4"}
```

- [ ] **Step 2: Write the minimal evaluator**

Create `src/abc/tools/schematron.clj`:

```clojure
(ns abc.tools.schematron
  "Small Schematron evaluator for ABC's v0 profile subset.

  This is not a full ISO Schematron compiler. It evaluates the checked-in
  `schemas/tei-profile.sch` constructs ABC currently uses:
  sch:ns, sch:pattern, sch:rule, sch:assert, and sch:report. Each pattern
  must contain exactly one rule until ISO rule-claim semantics are implemented.
  XPath 2.0 expressions are evaluated with Saxon-HE."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.transform.stream StreamSource]
           [net.sf.saxon.s9api Processor XdmNode]))

(def ^:private schematron-ns "http://purl.oclc.org/dsdl/schematron")

(defn- namespace-aware-document [path]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.. factory newDocumentBuilder (parse (io/file path)))))

(defn- element-children [^org.w3c.dom.Element element local-name]
  (let [nodes (.getChildNodes element)]
    (->> (range (.getLength nodes))
         (map #(.item nodes %))
         (filter #(instance? org.w3c.dom.Element %))
         (filter #(and (= schematron-ns (.getNamespaceURI ^org.w3c.dom.Element %))
                       (= local-name (.getLocalName ^org.w3c.dom.Element %)))))))

(defn- attr [^org.w3c.dom.Element element attr-name]
  (let [v (.getAttribute element attr-name)]
    (when-not (string/blank? v) v)))

(defn- trim-text [^org.w3c.dom.Element element]
  (string/trim (.getTextContent element)))

(defn- parse-namespaces [^org.w3c.dom.Document document]
  (let [nodes (.getElementsByTagNameNS document schematron-ns "ns")]
    (into {}
          (for [i (range (.getLength nodes))
                :let [node (.item nodes i)
                      prefix (attr node "prefix")
                      uri (attr node "uri")]
                :when (and prefix uri)]
            [prefix uri]))))

(defn- parse-check [pattern-id rule-context kind ^org.w3c.dom.Element element]
  {:rule-id pattern-id
   :context rule-context
   :kind kind
   :severity (keyword (or (attr element "role")
                          (if (= kind :report) "warning" "error")))
   :test (or (attr element "test")
             (throw (ex-info "Schematron check is missing @test"
                             {:rule-id pattern-id
                              :context rule-context})))
   :message (trim-text element)})

(defn parse-schema [schema-path]
  (let [document (namespace-aware-document schema-path)
        patterns (.getElementsByTagNameNS document schematron-ns "pattern")]
    {:namespaces (parse-namespaces document)
     :checks
     (vec
      (mapcat
       (fn [pattern-index]
         (let [pattern (.item patterns pattern-index)
               pattern-id (attr pattern "id")
               rules (vec (element-children pattern "rule"))]
           (when-not (= 1 (count rules))
             (throw (ex-info "ABC v0 Schematron requires exactly one sch:rule per sch:pattern until ISO rule-claim semantics are implemented"
                             {:pattern-id pattern-id
                              :rule-count (count rules)})))
           (mapcat
            (fn [rule]
              (let [context (or (attr rule "context")
                                (throw (ex-info "Schematron rule is missing @context"
                                                {:pattern-id pattern-id})))]
                (concat
                 (map #(parse-check pattern-id context :assert %) (element-children rule "assert"))
                 (map #(parse-check pattern-id context :report %) (element-children rule "report")))))
            rules)))
       (range (.getLength patterns))))}))

(defn- processor []
  (Processor. false))

(defn- document-node [^Processor processor xml-path]
  (let [builder (.newDocumentBuilder processor)]
    (.setLineNumbering builder true)
    (.build builder (StreamSource. (io/file xml-path)))))

(defn- compiler [^Processor processor namespaces]
  (let [compiler (.newXPathCompiler processor)]
    (doseq [[prefix uri] namespaces]
      (.declareNamespace compiler prefix uri))
    compiler))

(defn- select-nodes [compiler ^XdmNode document context-expr]
  (let [selector (.load (.compile compiler context-expr))]
    (.setContextItem selector document)
    (vec (iterator-seq (.iterator (.evaluate selector))))))

(defn- boolean-test [compiler node test-expr]
  (let [selector (.load (.compile compiler test-expr))]
    (.setContextItem selector node)
    (.effectiveBooleanValue selector)))

(defn- node-location [^XdmNode node]
  (let [line (.getLineNumber node)]
    (when (pos? line)
      (str line))))

(defn- finding [label check node]
  {:label label
   :rule-id (:rule-id check)
   :severity (:severity check)
   :kind (:kind check)
   :context (:context check)
   :test (:test check)
   :message (:message check)
   :location (node-location node)})

(defn validate!
  "Evaluate an ABC Schematron schema against one XML document.

  Returns {:label string, :findings [...]}. Assertion findings are emitted when
  the test is false. Report findings are emitted when the test is true."
  [{:keys [schema-path xml-path label]}]
  (let [{:keys [namespaces checks]} (parse-schema schema-path)
        proc (processor)
        doc (document-node proc xml-path)
        comp (compiler proc namespaces)
        findings
        (reduce
         (fn [acc check]
           (let [nodes (select-nodes comp doc (:context check))]
             (into acc
                   (keep
                    (fn [node]
                      (let [result (boolean-test comp node (:test check))]
                        (case (:kind check)
                          :assert (when-not result (finding label check node))
                          :report (when result (finding label check node)))))
                    nodes))))
         []
         checks)]
    {:label label
     :findings (vec findings)}))
```

- [ ] **Step 3: Run the focused Schematron tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.schematron-test) (clojure.test/run-tests 'abc.tools.schematron-test)"
```

Expected: all Schematron tests pass.

- [ ] **Step 4: Commit evaluator**

```bash
git add deps.edn nix/clj-nix-deps.edn src/abc/tools/schematron.clj test/abc/tools/schematron_test.clj
git commit -m "feat: add ABC Schematron evaluator"
```

## Task 4: Add Harness-Level Schematron Gates

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`
- Test: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Write failing harness tests**

First adjust `validate-tei-loud-fail-when-env-unset-test` so it expects a generic schema-path error instead of an env-var-specific error. Replace:

```clojure
(is (re-find #"TEI_SCHEMA_PATH" (ex-message e)))
(is (= "TEI_SCHEMA_PATH" (:env-var (ex-data e)))
    (str "ex-data must surface the env var name; got: "
         (pr-str (ex-data e))))
```

with:

```clojure
(is (re-find #"TEI RelaxNG schema path" (ex-message e)))
(is (= :missing-schema-path (:error (ex-data e)))
    (str "ex-data must surface the schema-path error; got: "
         (pr-str (ex-data e))))
```

Add these tests after `validate-tei-loud-fail-when-env-unset-test` in `test/abc/tools/validate_design_bundle_test.clj`:

```clojure
(deftest validate-tei-schematron-expected-findings-test
  (testing "expected invalid fixtures fail with the requested rule IDs"
    (is (nil? (validate/validate-tei-schematron!
               {:schema-path "schemas/tei-profile.sch"
                :valid-fixtures ["examples/v0/example-work/tei.xml"
                                 "fixtures/tei/valid/rashomon-minimal.xml"
                                 "fixtures/tei/valid/source-span-local-ref.xml"
                                 "fixtures/tei/valid/transcription-enrichment-declared.xml"]
                :warning-fixtures {"fixtures/tei/warnings/figure-missing-desc.xml"
                                   #{"abc-figure-accessibility"}
                                   "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                                   #{"abc-transcription-vs-annotation"}}
                :invalid-fixtures {"fixtures/tei/invalid/missing-title.xml"
                                   #{"abc-tei-header-title"}
                                   "fixtures/tei/invalid/missing-source-work-id.xml"
                                   #{"abc-tei-header-source-work-id"}
                                   "fixtures/tei/invalid/gaiji-missing-ref.xml"
                                   #{"abc-gaiji-reference"}
                                   "fixtures/tei/invalid/ruby-missing-reading.xml"
                                   #{"abc-ruby-complete"}
                                   "fixtures/tei/invalid/source-span-external-ref.xml"
                                   #{"abc-source-span-reference"}}})))))

(deftest validate-tei-schematron-loud-fail-test
  (testing "a fixture missing its expected finding makes the harness fail"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing expected Schematron rule"
         (validate/validate-tei-schematron!
          {:schema-path "schemas/tei-profile.sch"
           :valid-fixtures []
           :warning-fixtures {}
           :invalid-fixtures {"fixtures/tei/valid/rashomon-minimal.xml"
                              #{"abc-tei-header-title"}}})))))

(deftest validate-tei-schematron-valid-fixture-loud-fail-test
  (testing "valid fixtures must have no error findings"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"unexpected Schematron error"
         (validate/validate-tei-schematron!
          {:schema-path "schemas/tei-profile.sch"
           :valid-fixtures ["fixtures/tei/invalid/missing-title.xml"]
           :warning-fixtures {}
           :invalid-fixtures {}})))))
```

- [ ] **Step 2: Run the test to verify RED**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: compile failure because `validate/validate-tei-schematron!` does not exist.

- [ ] **Step 3: Implement harness helpers**

First generalize the top of `validate-tei!` in `src/abc/tools/validate_design_bundle.clj`:

```clojure
(when (or (nil? schema-path) (= "" schema-path))
  (throw (ex-info "TEI RelaxNG schema path must be set."
                  {:error :missing-schema-path})))
```

In `src/abc/tools/validate_design_bundle.clj`, add the require:

```clojure
[abc.tools.schematron :as schematron]
```

Add these functions after `validate-tei!`:

```clojure
(defn- schematron-error? [finding]
  (= :error (:severity finding)))

(defn- schematron-warning? [finding]
  (= :warning (:severity finding)))

(defn- rule-ids [findings]
  (set (map :rule-id findings)))

(defn- render-schematron-finding [{:keys [label rule-id severity message]}]
  (str (string/upper-case (name severity)) ": "
       label " " rule-id " — " message))

(defn- validate-schematron-valid-fixture! [schema-path path]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in valid TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))))

(defn- validate-schematron-warning-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-warnings (rule-ids (filter schematron-warning? findings))
        missing (set/difference expected-rules actual-warnings)
        errors (filter schematron-error? findings)]
    (when (seq errors)
      (throw (ex-info "unexpected Schematron error in warning TEI fixture"
                      {:fixture path
                       :errors (mapv render-schematron-finding errors)})))
    (when (seq missing)
      (throw (ex-info "missing expected Schematron warning rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-warnings)})))))

(defn- validate-schematron-invalid-fixture! [schema-path path expected-rules]
  (let [{:keys [findings]} (schematron/validate! {:schema-path schema-path
                                                  :xml-path path
                                                  :label path})
        actual-errors (rule-ids (filter schematron-error? findings))
        missing (set/difference expected-rules actual-errors)]
    (when (seq missing)
      (throw (ex-info "missing expected Schematron rule"
                      {:fixture path
                       :missing (sort missing)
                       :actual (sort actual-errors)})))))

(defn validate-tei-schematron!
  [{:keys [schema-path valid-fixtures warning-fixtures invalid-fixtures]}]
  (doseq [path valid-fixtures]
    (validate-schematron-valid-fixture! schema-path path))
  (doseq [[path expected-rules] warning-fixtures]
    (validate-schematron-warning-fixture! schema-path path expected-rules))
  (doseq [[path expected-rules] invalid-fixtures]
    (validate-schematron-invalid-fixture! schema-path path expected-rules)))
```

- [ ] **Step 4: Run harness tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected: all `abc.tools.validate-design-bundle-test` tests pass, except TEI upstream schema tests may require `TEI_SCHEMA_PATH`. If local `TEI_SCHEMA_PATH` is unset, run:

```bash
ABC_TEI_SCHEMA_SKIP=1 clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected with skip flag: all non-upstream TEI tests pass.

- [ ] **Step 5: Commit harness helpers**

```bash
git add src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat: enforce TEI Schematron fixture gates"
```

## Task 5: Wire Schematron Into `validate-design-bundle`

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Test: `nix run .#validate-design-bundle`

- [ ] **Step 1: Validate new JSON schemas and sidecars**

In `validate-json-schemas!`, bind the new schema:

```clojure
tei-validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
```

Add it to the schema-validity loop:

```clojure
["schemas/tei-validation-result.schema.json" tei-validation-result-schema]
```

After validating comparison report, add:

```clojure
(validate-json! tei-validation-result-schema
                "examples/v0/example-work/tei-validation-result.json")
```

Then add a narrow manifest-reference check so the sidecar is not only a loose JSON file:

```clojure
(let [manifest (files/read-json "examples/v0/example-work/manifest.json")
      validation-sidecars (filter #(= "validation-result" (get % "role"))
                                  (get manifest "sidecars"))]
  (when-not (some #(= "tei-validation-result.json" (get % "path_hint"))
                  validation-sidecars)
    (throw (ex-info "example TEI manifest must reference tei-validation-result.json"
                    {:manifest "examples/v0/example-work/manifest.json"}))))
```

Full byte-for-byte regeneration of `examples/v0/example-work/tei-validation-result.json` and replacement of the design-fixture sidecar hash is deferred to the first materialized TEI validation-result milestone. This plan still gates the sidecar shape and the manifest reference, and Task 5 executes real Schematron findings for the same canonical TEI example.

- [ ] **Step 2: Expand XML fixture well-formedness checks**

Replace `validate-xml!` with:

```clojure
(defn validate-xml! []
  (run-command! "xmllint" "--noout"
                "schemas/tei-profile.odd"
                "schemas/tei-profile.sch"
                "schemas/tei-profile.rng"
                "examples/v0/example-work/tei.xml"
                "fixtures/tei/valid/rashomon-minimal.xml"
                "fixtures/tei/valid/source-span-local-ref.xml"
                "fixtures/tei/valid/transcription-enrichment-declared.xml"
                "fixtures/tei/invalid/missing-title.xml"
                "fixtures/tei/invalid/missing-source-work-id.xml"
                "fixtures/tei/invalid/gaiji-missing-ref.xml"
                "fixtures/tei/invalid/ruby-missing-reading.xml"
                "fixtures/tei/invalid/source-span-external-ref.xml"
                "fixtures/tei/warnings/figure-missing-desc.xml"
                "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"))
```

- [ ] **Step 3: Add project RNG and Schematron steps to the main harness**

In `validate-design-bundle!`, after the existing upstream Relax NG step:

```clojure
(tel/log! :info "==> Validating TEI against project RelaxNG")
(validate-tei! "schemas/tei-profile.rng"
               ["examples/v0/example-work/tei.xml"
                "fixtures/tei/valid/rashomon-minimal.xml"
                "fixtures/tei/valid/source-span-local-ref.xml"
                "fixtures/tei/valid/transcription-enrichment-declared.xml"
                "fixtures/tei/warnings/figure-missing-desc.xml"
                "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                "fixtures/tei/invalid/missing-title.xml"
                "fixtures/tei/invalid/missing-source-work-id.xml"
                "fixtures/tei/invalid/gaiji-missing-ref.xml"
                "fixtures/tei/invalid/ruby-missing-reading.xml"
                "fixtures/tei/invalid/source-span-external-ref.xml"])
(tel/log! :info "tei project rng validation ok")
(tel/log! :info "==> Validating TEI against project Schematron")
(validate-tei-schematron!
 {:schema-path "schemas/tei-profile.sch"
  :valid-fixtures ["examples/v0/example-work/tei.xml"
                   "fixtures/tei/valid/rashomon-minimal.xml"
                   "fixtures/tei/valid/source-span-local-ref.xml"
                   "fixtures/tei/valid/transcription-enrichment-declared.xml"]
  :warning-fixtures {"fixtures/tei/warnings/figure-missing-desc.xml"
                     #{"abc-figure-accessibility"}
                     "fixtures/tei/warnings/transcription-enrichment-undeclared.xml"
                     #{"abc-transcription-vs-annotation"}}
  :invalid-fixtures {"fixtures/tei/invalid/missing-title.xml"
                     #{"abc-tei-header-title"}
                     "fixtures/tei/invalid/missing-source-work-id.xml"
                     #{"abc-tei-header-source-work-id"}
                     "fixtures/tei/invalid/gaiji-missing-ref.xml"
                     #{"abc-gaiji-reference"}
                     "fixtures/tei/invalid/ruby-missing-reading.xml"
                     #{"abc-ruby-complete"}
                     "fixtures/tei/invalid/source-span-external-ref.xml"
                     #{"abc-source-span-reference"}}})
(tel/log! :info "tei schematron validation ok")
```

This deliberately proves the invalid fixtures pass the current project RNG target but fail Schematron. Keep upstream `tei_all.rng` validation for `examples/v0/example-work/tei.xml` until a real ODD-to-RNG generator replaces the permissive checked-in project RNG target.

- [ ] **Step 4: Run the design bundle harness**

Run:

```bash
nix run .#validate-design-bundle
```

Expected output includes:

```text
==> Validating TEI against project RelaxNG
tei project rng validation ok
==> Validating TEI against project Schematron
tei schematron validation ok
design bundle validation ok
```

- [ ] **Step 5: Commit harness wiring**

```bash
git add src/abc/tools/validate_design_bundle.clj
git commit -m "feat: run project TEI RNG and Schematron gates"
```

## Task 6: Update Focused Tests and Nix Contract Surface

**Files:**
- Modify: `nix/clj-nix-deps.edn`
- Modify: `flake.nix`
- Test: `nix flake check`

- [ ] **Step 1: Add Schematron test namespace to focused tests**

In `nix/clj-nix-deps.edn`, add `'abc.tools.schematron-test` to both the `(require ...)` list and the `(test/run-tests ...)` list inside the `:abc/focused-test` command string.

The relevant command string should include:

```clojure
'abc.tools.schematron-test
```

next to `'abc.tools.tei-test`.

- [ ] **Step 2: Add contract-surface checks**

In `flake.nix`, inside `contract-surface`, add:

```bash
test -f ${./src/abc/tools/schematron.clj}
test -f ${./test/abc/tools/schematron_test.clj}
test -f ${./schemas/tei-profile.rng}
test -f ${./schemas/tei-profile.sch}
test -f ${./schemas/tei-validation-result.schema.json}
```

- [ ] **Step 3: Refresh clj-nix lock if dependency metadata changes**

Run:

```bash
bin/update-clj-nix-lock
```

Expected: `deps-lock.json` either remains semantically unchanged for Saxon because the artifact is already present, or records the explicit dependency edge. Keep any deterministic lockfile update.

- [ ] **Step 4: Run flake checks**

Run:

```bash
nix flake check
```

Expected: `all checks passed!`

- [ ] **Step 5: Commit Nix/test wiring**

```bash
git add nix/clj-nix-deps.edn flake.nix deps-lock.json
git commit -m "test: include Schematron gate in focused checks"
```

## Task 7: Final Verification

**Files:**
- All changed files

- [ ] **Step 1: Run focused Clojure tests**

Run:

```bash
ABC_TEI_SCHEMA_SKIP=1 clojure -M:test -e "(require 'abc.tools.schematron-test 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.schematron-test 'abc.tools.validate-design-bundle-test)"
```

Expected: no failures or errors.

- [ ] **Step 2: Run design-bundle validation**

Run:

```bash
nix run .#validate-design-bundle
```

Expected: `design bundle validation ok`, including both:

```text
tei project rng validation ok
tei schematron validation ok
```

- [ ] **Step 3: Run full flake check**

Run:

```bash
nix flake check
```

Expected: `all checks passed!`

- [ ] **Step 4: Check diff hygiene**

Run:

```bash
git diff --check
git status --short
```

Expected: no whitespace errors. `git status --short` should show only intentional files for this branch and any pre-existing unrelated files such as `.claude/` should remain untouched.

## Self-Review

Spec coverage:

- ADR 0012 structural Relax NG layer: Task 5 uses `schemas/tei-profile.rng` with Jing.
- ADR 0012 business-rule Schematron layer: Tasks 1-5 evaluate `schemas/tei-profile.sch`.
- Expected invalid fixtures by rule ID: Tasks 1, 2, and 4 cover title, source work ID, gaiji, ruby, and source-span failures.
- Warning fixture behavior: Tasks 1, 2, and 4 cover `abc-figure-accessibility` and `abc-transcription-vs-annotation` warnings.
- Positive fixtures for source-span and transcription declarations: Task 1 creates them and Tasks 2/5 gate them.
- Canonical example bundle TEI: Tasks 2 and 5 include `examples/v0/example-work/tei.xml` as a Schematron-valid fixture.
- Validation-result sidecar schema and manifest reference: Task 5 validates `examples/v0/example-work/tei-validation-result.json` and asserts `manifest.json` references it. Full deterministic regeneration and replacement of the design-fixture hash remain a named follow-up.
- ISO rule-shadowing divergence: Task 3 rejects patterns with more than one rule until claim semantics are implemented.
- Non-portable assert/report IDs: Task 1 removes dependency on them; Task 3 keys findings from `sch:pattern/@id`.
- CI surface: Task 6 adds focused tests and contract-surface checks.

Placeholder scan:

- No task uses unresolved placeholders. The ODD-to-RNG generator remains explicitly out of this implementation because the checked-in `schemas/tei-profile.rng` is already the project RNG target artifact for this milestone.

Type consistency:

- Public Schematron API is consistently `schematron/validate!`.
- Harness API is consistently `validate/validate-tei-schematron!`.
- Finding keys are consistently `:rule-id`, `:severity`, `:kind`, `:message`, `:label`, and `:location`.
