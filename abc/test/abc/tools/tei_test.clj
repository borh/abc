(ns abc.tools.tei-test
  (:require [abc.tools.tei :as tei]
            [abc.test-fs :refer [with-temp-dir]]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:private schema-path (atom nil))

(defn require-schema-path [t]
  (if-let [path (System/getenv "TEI_SCHEMA_PATH")]
    (do
      (reset! schema-path path)
      (t))
    (throw (ex-info "TEI_SCHEMA_PATH must be set to run abc.tools.tei-test. Run via `nix run .#validate-design-bundle` or export the path manually after `curl … tei_all.rng`."
                    {:env-var "TEI_SCHEMA_PATH"}))))

(use-fixtures :once require-schema-path)

(deftest schema-cache-accepts-absolute-and-canonical-schema-paths-test
  (with-temp-dir [dir]
    (let [schema (fs/file dir "schema.rng")]
      (fs/copy @schema-path schema)
      (is (identical? (#'tei/load-schema (str (fs/absolutize schema)))
                      (#'tei/load-schema (str (fs/canonicalize schema))))))))

(deftest validate-undefined-element-test
  (testing "<bogusElement> in TEI namespace surfaces a violation referencing the element name"
    (let [tmp (java.io.File/createTempFile "abc-tei-bogus" ".xml")]
      (try
        (spit tmp (str "<?xml version=\"1.0\"?>"
                       "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
                       "  <teiHeader><fileDesc>"
                       "    <titleStmt><title>t</title></titleStmt>"
                       "    <publicationStmt><p>p</p></publicationStmt>"
                       "    <sourceDesc><p>s</p></sourceDesc>"
                       "  </fileDesc></teiHeader>"
                       "  <text><body><bogusElement/></body></text>"
                       "</TEI>"))
        (let [{:keys [violations label]} (tei/validate! {:schema-path @schema-path
                                                         :xml-path (str tmp)
                                                         :label "bogus"})]
          (is (= "bogus" label))
          (is (seq violations) "must report at least one violation")
          (is (every? #(contains? % :severity) violations))
          (is (every? #(keyword? (:severity %)) violations))
          (is (every? #(contains? % :message) violations))
          (is (some #(re-find #"bogusElement" (:message %)) violations)
              (str "expected a violation mentioning bogusElement, got: " (pr-str violations))))
        (finally
          (.delete tmp))))))

(deftest validate-non-tei-root-test
  (testing "non-TEI root element produces violations and preserves severity keywords"
    (let [tmp (java.io.File/createTempFile "abc-tei-non" ".xml")]
      (try
        (spit tmp "<?xml version=\"1.0\"?><not-tei xmlns=\"x\"/>")
        (let [{:keys [violations]} (tei/validate! {:schema-path @schema-path
                                                   :xml-path (str tmp)
                                                   :label "non-tei"})]
          (is (seq violations))
          (is (every? #(#{:warning :error :fatal} (:severity %)) violations)
              "every violation must use a known severity keyword"))
        (finally
          (.delete tmp))))))

(deftest validate-example-fixture-test
  (testing "examples/v0/example-work/tei.xml validates clean against tei_all.rng"
    (let [{:keys [violations]} (tei/validate! {:schema-path @schema-path
                                               :xml-path "examples/v0/example-work/tei.xml"
                                               :label "example"})
          {warnings true failures false}
          (group-by #(= :warning (:severity %)) violations)]
      (is (empty? failures)
          (str "fixture must produce no error/fatal violations; got: "
               (pr-str failures)))
      (when (seq warnings)
        (println "validate-example-fixture-test: schema warnings:"
                 (pr-str warnings))))))
