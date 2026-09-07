(ns soranoha.links.assertion-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.links.assertion :as assertion]
            [soranoha.links.main :as main]))

(def base "https://example.org/soranoha/")
(def claim
  {:link/target {:entity/kind :entity/person :entity/id "人/一"}
   :link/relation "http://www.w3.org/2004/02/skos/core#closeMatch"
   :link/external {:entity/kind :entity/person :entity/iri "https://example.org/person/1"}
   :link/attribution {:attribution/agent "https://example.org/reviewer/1"
                      :attribution/method "Manual comparison\nwith source"
                      :attribution/evidence ["https://example.org/source/1" "urn:sha256:example"]}})
(def document
  {"schema" "soranoha-external-links/1"
   "assertions" [{"target" {"kind" "person" "id" "人/一"}
                  "relation" (:link/relation claim)
                  "external" {"kind" "person" "iri" "https://example.org/person/1"}
                  "attribution" {"agent" "https://example.org/reviewer/1"
                                 "method" "Manual comparison\nwith source"
                                 "evidence" ["https://example.org/source/1" "urn:sha256:example"]}}]})

(deftest local-identities-distinguish-entity-kinds-from-assertion-content
  (is (= 4 (count (set (map #(assertion/local-iri base {:entity/kind % :entity/id "same"})
                            [:entity/document :entity/work :entity/edition :entity/person])))))
  (is (str/ends-with? (assertion/local-iri base (:link/target claim)) "%E4%BA%BA%2F%E4%B8%80"))
  (let [revised (assoc-in claim [:link/attribution :attribution/method] "Another method")]
    (is (= (assertion/local-iri base (:link/target claim))
           (assertion/local-iri base (:link/target revised))))
    (is (not= (assertion/assertion-id claim) (assertion/assertion-id revised)))))

(deftest identity-and-rdf-ignore-only-unordered-input-order
  (let [reordered (update-in claim [:link/attribution :attribution/evidence] #(vec (reverse %)))
        other (assoc-in claim [:link/target :entity/id] "another")]
    (is (= (assertion/assertion-id claim) (assertion/assertion-id reordered)))
    (is (= (assertion/nquads base [claim other claim])
           (assertion/nquads base [other reordered])))))

(deftest rdf-describes-attributed-claims-without-asserting-relations
  (let [text (assertion/nquads base [claim])]
    (is (str/includes? text "rdf-syntax-ns#Statement"))
    (is (str/includes? text "prov#wasAttributedTo"))
    (is (str/includes? text "prov#wasDerivedFrom"))
    (is (str/includes? text "Manual comparison\\nwith source"))
    (is (not (str/includes? text "owl#sameAs")))
    (is (not-any? #(= (str "<" (:link/relation claim) ">")
                      (second (str/split % #" " 3))) (str/split-lines text))))
  (let [same (assoc claim :link/relation "http://www.w3.org/2002/07/owl#sameAs")
        text (assertion/nquads base [same])]
    (is (str/includes? text "rdf-syntax-ns#predicate> <http://www.w3.org/2002/07/owl#sameAs>"))
    (is (not-any? #(= "<http://www.w3.org/2002/07/owl#sameAs>"
                      (second (str/split % #" " 3))) (str/split-lines text)))))

(deftest boundary-rejects-ambiguous-types-and-missing-attribution
  (is (= [claim] (assertion/read-assertions (json/write-json-str document))))
  (doseq [invalid [(assoc-in claim [:link/target :entity/kind] :assessment/person)
                   (assoc-in claim [:link/target :entity/id] "")
                   (assoc-in claim [:link/target :entity/id] "..")
                   (assoc claim :link/relation "relative")
                   (assoc-in claim [:link/external :entity/iri] "https://example.org/a>\n<bad")
                   (assoc-in claim [:link/attribution :attribution/evidence] [])
                   (update claim :link/attribution dissoc :attribution/agent)
                   (assoc claim :link/confidence 1)]]
    (is (thrown? clojure.lang.ExceptionInfo (assertion/assertion-id invalid))))
  (doseq [invalid [(assoc-in document ["assertions" 0 "target" "kind"] "entity/person")
                   (assoc-in document ["assertions" 0 "extra"] true)
                   (assoc document "schema" "soranoha-external-links/2")]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (assertion/read-assertions (json/write-json-str invalid))))))

(deftest export-validates-before-writing-and-refuses-overwrite
  (let [directory (fs/create-temp-dir {:prefix "external-links"})]
    (try
      (let [input (fs/file directory "links.json") output (fs/file directory "links.nq")]
        (spit input (json/write-json-str document))
        (is (= [(assertion/assertion-id claim)] (get (main/export-files! [input] base output) "assertions")))
        (is (= (assertion/nquads base [claim]) (slurp output)))
        (is (thrown? java.nio.file.FileAlreadyExistsException (main/export-files! [input] base output)))
        (testing "invalid input leaves no partial export"
          (spit input "{}")
          (let [missing (fs/file directory "invalid.nq")]
            (is (thrown? clojure.lang.ExceptionInfo (main/export-files! [input] base missing)))
            (is (not (fs/exists? missing))))))
      (finally (fs/delete-tree directory)))))
