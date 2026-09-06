(ns abc.tools.schematron-test
  (:require [abc.tools.schematron :as schematron]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def ^:private schematron-ns "http://purl.oclc.org/dsdl/schematron")

(def schema-path "schemas/tei-profile.sch")

(deftest schema-cache-identifies-a-schema-through-a-symlinked-parent-test
  (with-temp-dir [dir]
    (let [real-parent (fs/file dir "real")
          linked-parent (fs/file dir "linked")
          real-schema (fs/file real-parent "schema.sch")]
      (fs/create-dirs real-parent)
      (fs/copy schema-path real-schema)
      (fs/create-sym-link linked-parent real-parent)
      (is (identical? (#'schematron/schematron-resource (str real-schema))
                      (#'schematron/schematron-resource
                       (str (fs/file linked-parent "schema.sch"))))))))

(deftest saxon-runtime-version-is-pinned-stable-line-test
  (testing "Clojure Schematron runtime stays aligned with the Saxon production-line pin"
    (is (= "12.9" (net.sf.saxon.Version/getProductVersion)))))

(defn- namespace-declarations [path]
  (let [factory (doto (javax.xml.parsers.DocumentBuilderFactory/newInstance)
                  (.setNamespaceAware true))
        document (.. factory newDocumentBuilder (parse (io/file path)))
        nodes (.getElementsByTagNameNS document schematron-ns "ns")]
    (for [i (range (.getLength nodes))
          :let [node (.item nodes i)]]
      [(.getAttribute node "prefix") (.getAttribute node "uri")])))

(deftest schema-namespace-declarations-are-unique-test
  (testing "generated Schematron artifacts do not duplicate identical namespace declarations"
    (doseq [path ["schemas/tei-profile.sch" "schemas/tei-profile.rng"]
            :let [declarations (namespace-declarations path)]]
      (is (= (count declarations) (count (set declarations)))
          path))))

(deftest committed-schema-patterns-are-abc-policy-surface-test
  (testing "committed Schematron keeps inherited TEI diagnostics out of the v0 policy surface"
    (let [pattern-ids (schematron/pattern-ids schema-path)]
      (is (= 16 (count pattern-ids)))
      (is (every? #(re-matches #"abc-[a-z0-9-]+" %) pattern-ids))
      (is (not-any? #(re-find #"^schematron-constraint-" %) pattern-ids)))))

(deftest committed-schema-is-valid-for-xslt-and-pure-backends-test
  (testing "the committed artifact stays compatible with both ph-schematron schema models"
    (is (schematron/schema-valid? :xslt schema-path))
    (is (schematron/schema-valid? :pure schema-path))))

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
  (testing "external @source value trips both the # prefix rule and the target-exists rule"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/source-span-external-ref.xml"
                               :label "source-span"})]
      (is (= #{"abc-source-span-reference" "abc-source-span-target-exists"}
             (set (map :rule-id findings))))
      (is (every? #{:error} (map :severity findings))))))

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

(deftest ruby-empty-base-fails-base-non-empty-rule-test
  (testing "ruby with empty rb fails abc-ruby-base-non-empty"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/ruby-empty-base.xml"
                               :label "ruby-empty-base"})]
      (is (= ["abc-ruby-base-non-empty"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

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

(deftest gaiji-dangling-ref-fails-chardecl-resolution-rule-test
  (testing "gaiji local @ref to a missing charDecl/char fails abc-gaiji-chardecl-resolution"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/gaiji-dangling-ref.xml"
                               :label "gaiji-dangling"})]
      (is (= ["abc-gaiji-chardecl-resolution"]
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

(deftest source-span-dangling-ref-fails-target-exists-rule-test
  (testing "source span fragment id with no matching @xml:id fails abc-source-span-target-exists"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/source-span-dangling-ref.xml"
                               :label "source-span-dangling"})]
      (is (= ["abc-source-span-target-exists"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

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

(deftest abc-missing-vocab-version-fails-vocab-version-rule-test
  (testing "TEI with ABC extension attributes must declare abc:vocab-version"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/abc-missing-vocab-version.xml"
                               :label "abc-missing-vocab-version"})]
      (is (= ["abc-vocab-version-declared"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest abc-bad-preservation-record-fails-record-shape-rule-test
  (testing "abc:preservation-record must use deterministic sidecar ids"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/abc-bad-preservation-record.xml"
                               :label "abc-bad-preservation-record"})]
      (is (= ["abc-preservation-record-shape"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest abc-bad-layout-params-fails-layout-params-rule-test
  (testing "abc:layout-params must use key=value payloads"
    (let [{:keys [findings]} (schematron/validate!
                              {:schema-path schema-path
                               :xml-path "fixtures/tei/invalid/abc-bad-layout-params.xml"
                               :label "abc-bad-layout-params"})]
      (is (= ["abc-layout-params-shape"]
             (mapv :rule-id findings)))
      (is (= [:error]
             (mapv :severity findings))))))

(deftest concurrent-validate-calls-do-not-interfere-test
  (testing "each concurrent call collects only its own document's findings from the shared cached resource"
    (let [results (->> (range 8)
                       (mapv (fn [i]
                               (let [[path label] (if (even? i)
                                                    ["fixtures/tei/invalid/missing-title.xml" "missing-title"]
                                                    ["fixtures/tei/invalid/ruby-missing-reading.xml" "ruby"])]
                                 (future
                                   (schematron/validate! {:schema-path schema-path
                                                          :xml-path path
                                                          :label label})))))
                       (mapv deref))]
      (doseq [{:keys [label findings]} results]
        (let [own-rule-id (if (= label "missing-title") "abc-tei-header-title" "abc-ruby-complete")
              other-rule-id (if (= label "missing-title") "abc-ruby-complete" "abc-tei-header-title")]
          (is (= [own-rule-id] (mapv :rule-id findings))
              (str label " must report only its own document's finding"))
          (is (not-any? #{other-rule-id} (map :rule-id findings))
              (str label " must not see the other document's finding")))))))

(deftest multi-rule-pattern-evaluates-all-rules-test
  (testing "ISO Schematron patterns with multiple rules are evaluated"
    (let [schema (java.io.File/createTempFile "abc-sch-multi-rule" ".sch")
          xml (java.io.File/createTempFile "abc-sch-multi-rule" ".xml")]
      (try
        (spit schema (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          "<sch:schema xmlns:sch=\"http://purl.oclc.org/dsdl/schematron\" queryBinding=\"xslt2\">"
                          "  <sch:pattern id=\"two-rules\">"
                          "    <sch:rule context=\"item[@n = '1']\"><sch:assert test=\"false()\">one</sch:assert></sch:rule>"
                          "    <sch:rule context=\"item[@n = '2']\"><sch:assert test=\"false()\">two</sch:assert></sch:rule>"
                          "  </sch:pattern>"
                          "</sch:schema>"))
        (spit xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?><root><item n=\"1\"/><item n=\"2\"/></root>")
        (let [{:keys [findings]} (schematron/validate!
                                  {:schema-path (str schema)
                                   :xml-path (str xml)
                                   :label "multi-rule"})]
          (is (= ["two-rules" "two-rules"] (mapv :rule-id findings)))
          (is (= ["one" "two"] (mapv :message findings))))
        (finally
          (.delete schema)
          (.delete xml))))))

(deftest ruby-components-may-be-declared-source-characters-test
  (with-temp-dir [dir]
    (doseq [component ["rb" "rt"]
            [label base declaration expected]
            [["unresolved source glyph" "<g ref=\"#gaiji-355-17\"/>"
              "<char xml:id=\"gaiji-355-17\"><localProp name=\"rawMarker\" value=\"凵＜茲\"/></char>" #{}]
             ["resolved source glyph" "<g ref=\"#gaiji-355-17\">字</g>"
              "<char xml:id=\"gaiji-355-17\"><mapping type=\"unicode\">字</mapping></char>" #{}]
             ["empty base" "" "" #{"abc-ruby-base-non-empty"}]
             ["whitespace base" " \n\t " "" #{"abc-ruby-base-non-empty"}]
             ["dangling declaration" "<g ref=\"#gaiji-355-17\"/>" "" #{"abc-gaiji-chardecl-resolution"}]
             ["empty declaration" "<g ref=\"#gaiji-355-17\"/>"
              "<char xml:id=\"gaiji-355-17\"><desc> </desc></char>" #{"abc-char-resolution-form"}]
             ["undeclared symbolic glyph" "<g ana=\"unresolved\"/>" "" #{"abc-ruby-base-non-empty"}]]]
      (let [expected (if (= component "rt")
                       (set (map #(if (= % "abc-ruby-base-non-empty") "abc-ruby-reading-non-empty" %) expected))
                       expected)
            xml (-> (slurp "fixtures/tei/invalid/ruby-empty-base.xml")
                    (string/replace "<rb></rb>" (str "<rb>" base "</rb>"))
                    (cond-> (= component "rt") (string/replace #"(?s)<rb>(.*?)</rb><rt>(.*?)</rt>" "<rb>$2</rb><rt>$1</rt>"))
                    (string/replace "</fileDesc>"
                                    (str "</fileDesc><encodingDesc><charDecl>" declaration
                                         "</charDecl></encodingDesc>")))
            path (fs/file dir "ruby.xml")]
        (spit path xml)
        (is (= expected (set (map :rule-id (:findings (schematron/validate!
                                                       {:schema-path schema-path :xml-path (str path) :label label})))))
            label)))))
