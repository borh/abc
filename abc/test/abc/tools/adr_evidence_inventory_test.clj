(ns abc.tools.adr-evidence-inventory-test
  (:require [abc.tools.adr-evidence-inventory :as inventory]
            [abc.tools.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def sample-adrs
  [{:num 2 :file "0002-two.md" :status "Accepted"
    :criteria [{:criterion-index 0 :body "Second." :claim-id nil :claim-kind nil}]
    :evidence []}
   {:num 1 :file "0001-one.md" :status "Accepted"
    :criteria [{:criterion-index 0 :body "Criterion text."
                :claim-id nil :claim-kind nil}
               {:criterion-index 1 :body "Other." :claim-id nil :claim-kind nil}]
    :evidence [{:criterion-index 0 :path "test/example.clj"}]}
   {:num 99 :file "0099-proposed.md" :status "Proposed"
    :criteria [{:criterion-index 0 :body "Not inventoried."}]
    :evidence []}])

(deftest inventory-is-complete-sorted-and-reviewable
  (let [value (inventory/inventory-value sample-adrs)]
    (is (= "abc-adr-claim-migration-inventory-v1" (get value "schema_version")))
    (is (= 2 (get value "accepted_adr_count")))
    (is (= 3 (get value "accepted_criterion_count")))
    (is (= {"foundation-runtime-identity" 2
            "schema-rdf-tei" 0
            "temporal-person-ingest" 0
            "parser-ir-publication" 1
            "diagrams-governance" 0
            "unclassified" 0}
           (get value "families")))
    (is (= {"adr" 1 "file" "0001-one.md" "criterion_index" 0
            "body" "Criterion text." "claim_id" nil "claim_kind" nil
            "evidence_paths" ["test/example.clj"]
            "family" "foundation-runtime-identity" "disposition" nil}
           (first (get value "criteria"))))
    (is (= [[1 0] [1 1] [2 0]]
           (mapv (juxt #(get % "adr") #(get % "criterion_index"))
                 (get value "criteria"))))))

(deftest inventory-json-is-byte-identical
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-inventory-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        first-file (io/file dir "first.json")
        second-file (io/file dir "second.json")]
    (inventory/write-inventory! first-file sample-adrs)
    (inventory/write-inventory! second-file sample-adrs)
    (is (= (slurp first-file) (slurp second-file)))
    (is (= (inventory/inventory-value sample-adrs)
           (json/read-json-file first-file)))))
