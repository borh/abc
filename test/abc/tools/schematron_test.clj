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
