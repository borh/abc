(ns abc.tools.person-record-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.malli :as am]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-record :as pr]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :once (fn [f] (am/install!) (f)))

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

(deftest validate-rejects-impossible-bce-calendar-day-test
  (testing "validate! rejects calendar-impossible BCE full-date (e.g. -0426-02-31)"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"not a valid calendar date"
         (pr/validate! (assoc (example-person)
                              "date_of_birth" "-0426-02-31"))))))

(deftest validate-accepts-real-bce-calendar-day-test
  (testing "validate! accepts a real BCE full date (LocalDate handles signed years)"
    (is (= :ok (pr/validate! (assoc (example-person)
                                    "date_of_birth" "-0426-01-15"))))))

(deftest validate-rejects-extra-key-test
  (testing "validate! rejects an unknown property"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pr/validate! (assoc (example-person) "unknown" 1))))))

(deftest validate-humanizes-errors-test
  (testing "validate! ex-data carries both :errors and :errors-humanized"
    (try
      (pr/validate! (dissoc (example-person) "family_name"))
      (is false "validate! should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (is (contains? d :errors))
          (is (contains? d :errors-humanized))
          (is (every? string? (:errors-humanized d))))))))

(deftest schema-read-is-cached-test
  (testing "no disk read for schema after the cache is primed"
    (am/cached-schema "schemas/person-record.schema.json")
    (let [record (example-person)
          counter (atom 0)
          original abc.tools.json/read-json-file]
      (with-redefs [abc.tools.json/read-json-file
                    (fn [p]
                      (when (string/ends-with? (str p) "person-record.schema.json")
                        (swap! counter inc))
                      (original p))]
        (pr/validate! record)
        (pr/validate! record)
        (is (zero? @counter))))))

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
  (testing "record->graph emits FOAF + RDA Group 2 + EDTF echo triples"
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
      (is (contains? predicates "http://RDVocab.info/ElementsGr2/dateOfDeath"))
      (is (contains? predicates "https://w3id.org/abc/edtfDateOfBirth"))
      (is (contains? predicates "https://w3id.org/abc/edtfDateOfDeath")))))

(defn- objects-of [graph predicate-uri]
  (->> (iterator-seq (.find graph))
       (filter #(= predicate-uri (.getURI (.getPredicate %))))
       (mapv #(.getObject %))))

(deftest record->graph-precise-date-test
  (testing "YYYY-MM-DD dates emit xsd:date with parallel EDTF echo"
    (let [g (pr/record->graph (example-person))
          [dob] (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth")
          [edtf-dob] (objects-of g "https://w3id.org/abc/edtfDateOfBirth")]
      (is (= "1892-03-01" (.getLiteralLexicalForm dob)))
      (is (= "http://www.w3.org/2001/XMLSchema#date"
             (.getLiteralDatatypeURI dob)))
      (is (= "1892-03-01" (.getLiteralLexicalForm edtf-dob)))
      (is (= "https://w3id.org/abc/EDTF"
             (.getLiteralDatatypeURI edtf-dob))))))

(deftest record->graph-year-only-test
  (testing "YYYY-only dates emit xsd:gYear with parallel EDTF echo"
    (let [person (assoc (example-person)
                        "date_of_birth" "1941"
                        "date_of_death" nil)
          g (pr/record->graph person)
          [dob] (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth")
          [edtf-dob] (objects-of g "https://w3id.org/abc/edtfDateOfBirth")]
      (is (= "1941" (.getLiteralLexicalForm dob)))
      (is (= "http://www.w3.org/2001/XMLSchema#gYear"
             (.getLiteralDatatypeURI dob)))
      (is (= "1941" (.getLiteralLexicalForm edtf-dob)))
      (is (= "https://w3id.org/abc/EDTF"
             (.getLiteralDatatypeURI edtf-dob)))
      (is (empty? (objects-of g "http://RDVocab.info/ElementsGr2/dateOfDeath"))
          "null date_of_death omits the predicate"))))

(deftest record->graph-year-month-test
  (testing "YYYY-MM dates emit xsd:gYearMonth"
    (let [person (assoc (example-person)
                        "date_of_birth" "1904-01"
                        "date_of_death" nil)
          g (pr/record->graph person)
          [dob] (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth")]
      (is (= "1904-01" (.getLiteralLexicalForm dob)))
      (is (= "http://www.w3.org/2001/XMLSchema#gYearMonth"
             (.getLiteralDatatypeURI dob))))))

(deftest record->graph-bce-test
  (testing "Negative-year dates emit xsd:gYear and survive EDTF echo"
    (let [person (assoc (example-person)
                        "date_of_birth" "-0426"
                        "date_of_death" "-0346")
          g (pr/record->graph person)
          [dob] (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth")
          [edtf-dob] (objects-of g "https://w3id.org/abc/edtfDateOfBirth")]
      (is (= "-0426" (.getLiteralLexicalForm dob)))
      (is (= "http://www.w3.org/2001/XMLSchema#gYear"
             (.getLiteralDatatypeURI dob)))
      (is (= "-0426" (.getLiteralLexicalForm edtf-dob))))))

(deftest record->graph-decade-marker-test
  (testing "EDTF Level 1 decade markers emit only the abc:EDTF echo; RDA Group 2 omitted (no XSD precision type)"
    ;; ADR 0016: decade and century markers have no XSD precision-typed
    ;; equivalent. The EDTF echo carries the value; the RDA Group 2
    ;; predicate is omitted, satisfying SHACL `sh:maxCount 1` with zero.
    (let [person (assoc (example-person)
                        "date_of_birth" "192X"
                        "date_of_death" nil)
          g (pr/record->graph person)
          [edtf-dob] (objects-of g "https://w3id.org/abc/edtfDateOfBirth")]
      (is (= "192X" (.getLiteralLexicalForm edtf-dob)))
      (is (= "https://w3id.org/abc/EDTF" (.getLiteralDatatypeURI edtf-dob)))
      (is (empty? (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth"))
          "decade marker omits the RDA Group 2 predicate — no XSD precision type")
      (is (empty? (objects-of g "http://RDVocab.info/ElementsGr2/dateOfDeath"))
          "null date_of_death omits its predicates"))))

(deftest record->graph-bce-century-marker-test
  (testing "EDTF Level 1 BCE century markers emit only the abc:EDTF echo"
    (let [person (assoc (example-person)
                        "date_of_birth" "-06XX"
                        "date_of_death" "-05XX")
          g (pr/record->graph person)
          [edtf-dob] (objects-of g "https://w3id.org/abc/edtfDateOfBirth")
          [edtf-dod] (objects-of g "https://w3id.org/abc/edtfDateOfDeath")]
      (is (= "-06XX" (.getLiteralLexicalForm edtf-dob)))
      (is (= "-05XX" (.getLiteralLexicalForm edtf-dod)))
      (is (= "https://w3id.org/abc/EDTF" (.getLiteralDatatypeURI edtf-dob)))
      (is (empty? (objects-of g "http://RDVocab.info/ElementsGr2/dateOfBirth"))
          "century marker omits the RDA Group 2 predicate")
      (is (empty? (objects-of g "http://RDVocab.info/ElementsGr2/dateOfDeath"))
          "century marker omits the RDA Group 2 predicate"))))

(deftest validate-accepts-edtf-level1-shapes-test
  (testing "validate! accepts EDTF Level 1 decade and century shapes (ADR 0016)"
    (doseq [v ["192X" "200X" "-019X" "-06XX" "-00XX" "20XX"]]
      (is (= :ok (pr/validate! (assoc (example-person) "date_of_birth" v)))
          (str v " admitted by schema"))
      (is (= :ok (pr/validate! (assoc (example-person) "date_of_death" v)))
          (str v " admitted by schema (death)")))))
