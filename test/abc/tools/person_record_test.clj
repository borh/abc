(ns abc.tools.person-record-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-record :as pr]
            [clojure.test :refer [deftest is testing]]))

(def ^:private schema-path "schemas/person-record.schema.json")
(def ^:private schema-id "https://w3id.org/abc/schemas/person-record.schema.json")

(defn- example-person []
  {"person_record_schema_id" schema-id
   "person_record_schema_hash" (manifest/schema-hash schema-path)
   "person_id" "000879"
   "family_name" "芥川"
   "given_name" "竜之介"
   "family_name_reading" "あくたがわ"
   "given_name_reading" "りゅうのすけ"
   "family_name_sort" "あくたかわ"
   "given_name_sort" "りゆうのすけ"
   "family_name_romaji" "Akutagawa"
   "given_name_romaji" "Ryunosuke"
   "date_of_birth" "1892-03-01"
   "date_of_death" "1927-07-24"
   "person_copyright_expired" true
   "external_links" []})

(deftest validate-accepts-example-test
  (testing "the in-test example person validates"
    (is (= :ok (pr/validate! (example-person))))))

(deftest validate-rejects-missing-required-test
  (testing "validate! throws when family_name is missing"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (dissoc (example-person) "family_name"))))))

(deftest validate-rejects-extra-key-test
  (testing "validate! rejects an unknown property"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (assoc (example-person) "unknown" 1))))))

(deftest record-hash-format-test
  (testing "record-hash returns sha256:<64-hex>"
    (is (re-matches #"^sha256:[0-9a-f]{64}$"
                    (pr/record-hash (example-person))))))

(deftest record-hash-deterministic-test
  (testing "record-hash returns the same value across two calls"
    (is (= (pr/record-hash (example-person))
           (pr/record-hash (example-person))))))

(deftest record-hash-ignores-key-order-test
  (testing "record-hash is independent of map insertion order"
    (let [p (example-person)
          shuffled (into (sorted-map) p)]
      (is (= (pr/record-hash p) (pr/record-hash shuffled))))))

(deftest record-hash-excludes-provenance-test
  (testing "changing source_csv_provenance does not change record-hash"
    (let [base (example-person)
          with-prov-a (assoc base "source_csv_provenance"
                             {"source_url" "https://a.example/x.csv"
                              "retrieved_at" "2026-01-01T00:00:00Z"
                              "original_file_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"})
          with-prov-b (assoc base "source_csv_provenance"
                             {"source_url" "https://b.example/y.csv"
                              "retrieved_at" "2026-12-31T23:59:59Z"
                              "original_file_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"})]
      (is (= (pr/record-hash base) (pr/record-hash with-prov-a)))
      (is (= (pr/record-hash with-prov-a) (pr/record-hash with-prov-b))))))

(deftest record-hash-includes-schema-hash-test
  (testing "mutating person_record_schema_hash changes record-hash"
    (let [p (example-person)
          mutated (assoc p "person_record_schema_hash"
                         "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (is (not= (pr/record-hash p) (pr/record-hash mutated))))))

(deftest record-hash-includes-bibliographic-fields-test
  (testing "mutating family_name_romaji changes record-hash"
    (let [p (example-person)
          mutated (assoc p "family_name_romaji" "Akutagawa-changed")]
      (is (not= (pr/record-hash p) (pr/record-hash mutated))))))

(deftest schema-hash-self-consistent-test
  (testing "the example's embedded schema hash matches the live schema"
    (is (= (manifest/schema-hash schema-path)
           (get (example-person) "person_record_schema_hash")))))

(def ^:private fixture-path "examples/v0/example-persons/000879.json")

(deftest example-person-fixture-validates-test
  (testing "examples/v0/example-persons/000879.json validates against the schema"
    (let [record (files/read-json fixture-path)]
      (is (= :ok (pr/validate! record))))))

(deftest example-person-fixture-schema-hash-test
  (testing "the embedded schema hash matches the live schema"
    (let [record (files/read-json fixture-path)]
      (is (= (manifest/schema-hash schema-path)
             (get record "person_record_schema_hash"))))))

(deftest record->graph-key-triples-test
  (testing "record->graph emits FOAF + RDA Group 2 triples for one person"
    (let [g (pr/record->graph (example-person))
          triples (iterator-seq (.find g))
          predicates (set (map #(.getURI (.getPredicate %)) triples))
          subjects (set (map #(.getURI (.getSubject %)) triples))]
      (is (contains? subjects (pr/person-iri "000879")))
      (is (contains? predicates "http://purl.org/dc/terms/identifier"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/familyName"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/givenName"))
      (is (contains? predicates "http://xmlns.com/foaf/0.1/name"))
      (is (contains? predicates "http://RDVocab.info/ElementsGr2/dateOfBirth"))
      (is (contains? predicates "http://RDVocab.info/ElementsGr2/dateOfDeath")))))
