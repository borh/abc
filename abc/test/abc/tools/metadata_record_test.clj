(ns abc.tools.metadata-record-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.malli :as am]
            [abc.tools.metadata-record :as mr]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]])
  (:import [org.apache.jena.graph NodeFactory]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(def example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(def example-person
  (delay (files/read-json "examples/v0/example-persons/000879.json")))

(def example-persons-by-id
  (delay {"000879" @example-person}))

(deftest validate-example-record-test
  (testing "the example fixture validates against the schema"
    (is (= :ok (mr/validate! @example-record)))))

(deftest validate-rejects-missing-required-test
  (testing "validate! throws when a required field is missing"
    (let [bad (dissoc @example-record "work")]
      (is (thrown? clojure.lang.ExceptionInfo (mr/validate! bad))))))

(deftest validate-humanizes-errors-test
  (testing "validate! ex-data carries both :errors and :errors-humanized"
    (try
      (mr/validate! {"work" {} "contributors" []})
      (is false "validate! should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (is (contains? d :errors))
          (is (contains? d :errors-humanized))
          (is (every? string? (:errors-humanized d))))))))

(deftest schema-read-is-cached-test
  (testing "no disk read for schema after the cache is primed"
    (am/cached-schema "schemas/metadata-record.schema.json")
    (let [counter (atom 0)
          original abc.tools.json/read-json-file]
      (with-redefs [abc.tools.json/read-json-file
                    (fn [p]
                      (when (string/ends-with? (str p) "metadata-record.schema.json")
                        (swap! counter inc))
                      (original p))]
        (mr/validate! @example-record)
        (mr/validate! @example-record)
        (is (zero? @counter))))))

(deftest record-hash-deterministic-test
  (testing "record-hash returns the same value across two calls"
    (is (= (mr/record-hash @example-record)
           (mr/record-hash @example-record)))))

(deftest record-hash-format-test
  (testing "record-hash returns sha256:<64-hex>"
    (is (re-matches #"^sha256:[0-9a-f]{64}$"
                    (mr/record-hash @example-record)))))

(deftest record-hash-ignores-key-order-test
  (testing "record-hash is independent of map insertion order"
    (let [r1 @example-record
          r2 (into (sorted-map) r1)]
      (is (= (mr/record-hash r1) (mr/record-hash r2))))))

(deftest record-hash-excludes-provenance-test
  (testing "changing source_csv_provenance does not change record-hash"
    (let [base (dissoc @example-record "source_csv_provenance")
          with-prov-a (assoc base "source_csv_provenance"
                             {"source_url" "https://a.example/x.csv"
                              "retrieved_at" "2026-01-01T00:00:00Z"
                              "original_file_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"})
          with-prov-b (assoc base "source_csv_provenance"
                             {"source_url" "https://b.example/y.csv"
                              "retrieved_at" "2026-12-31T23:59:59Z"
                              "original_file_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"})]
      (is (= (mr/record-hash with-prov-a) (mr/record-hash with-prov-b)))
      (is (= (mr/record-hash base) (mr/record-hash with-prov-a))))))

(deftest record-hash-includes-schema-hash-test
  (testing "mutating metadata_record_schema_hash changes record-hash"
    (let [r1 @example-record
          r2 (assoc r1 "metadata_record_schema_hash"
                    "sha256:0000000000000000000000000000000000000000000000000000000000000000")]
      (is (not= (mr/record-hash r1) (mr/record-hash r2))))))

(deftest record-hash-sorts-contributors-test
  (testing "contributors[] order does not change the hash"
    (let [r1 @example-record
          contributors (get r1 "contributors")
          r2 (assoc r1 "contributors" (vec (reverse contributors)))]
      (is (= (mr/record-hash r1) (mr/record-hash r2))))))

(deftest record->graph-key-triples-test
  (testing "record->graph emits the expected work-side triples"
    (let [g (mr/record->graph @example-record)
          triples (iterator-seq (.find g))
          predicates (set (map #(.getURI (.getPredicate %)) triples))]
      (is (contains? predicates "http://purl.org/dc/terms/title"))
      (is (contains? predicates "http://purl.org/dc/terms/creator"))
      (is (contains? predicates "http://purl.org/dc/terms/identifier")))))

(deftest unassessed-copyright-flags-emit-no-external-rights-test
  (testing "no legacy source flag implies an external rights statement"
    (doseq [[label record]
            [["true" (assoc-in @example-record ["work" "copyright_expired"] true)]
             ["false" (assoc-in @example-record ["work" "copyright_expired"] false)]
             ["nil" (assoc-in @example-record ["work" "copyright_expired"] nil)]
             ["malformed" (assoc-in @example-record ["work" "copyright_expired"]
                                    "unknown")]
             ["absent" (update @example-record "work" dissoc
                               "copyright_expired")]]]
      (let [triples (iterator-seq (.find (mr/record->graph record)))
            rights (filter #(= "http://purl.org/dc/terms/rights"
                               (.getURI (.getPredicate %)))
                           triples)]
        (is (empty? rights)
            (str "source state " label
                 " must not emit dcterms:rights"))))))

(deftest source-only-rights-graph-conforms-to-shacl-test
  (testing "the metadata SHACL contract permits a work with no external rights IRI"
    (let [shapes ((requiring-resolve 'abc.tools.shacl/load-shapes-graph))
          data (mr/record+persons->graph
                (assoc-in @example-record ["work" "copyright_expired"] false)
                @example-persons-by-id)
          rights-predicate (NodeFactory/createURI
                            "http://purl.org/dc/terms/rights")]
      (doseq [triple (iterator-seq (.find data nil rights-predicate nil))]
        (.delete data triple))
      (is (= :ok ((requiring-resolve 'abc.tools.shacl/validate!)
                  {:shapes-graph shapes
                   :data-graph data
                   :label "source-only-rights"}))))))

(deftest record+persons->ttl-matches-fixture-test
  (testing "compose-graph + ttl matches the committed metadata-record.ttl"
    (let [expected (slurp "examples/v0/example-work/metadata-record.ttl")]
      (is (= expected (mr/record+persons->ttl @example-record @example-persons-by-id))))))

(deftest record-graph-conforms-to-shacl-test
  (testing "the example record's combined RDF graph conforms to all metadata shapes"
    (let [shapes ((requiring-resolve 'abc.tools.shacl/load-shapes-graph))
          data (mr/record+persons->graph @example-record @example-persons-by-id)]
      (is (= :ok ((requiring-resolve 'abc.tools.shacl/validate!)
                  {:shapes-graph shapes
                   :data-graph data
                   :label "metadata-record-shape-test"}))))))

(deftest build-metadata-record-shape-test
  (testing "build-metadata-record produces a schema-compliant value"
    (let [work (get @example-record "work")
          contributors (get @example-record "contributors")
          built (mr/build-metadata-record
                 {:work work
                  :contributors contributors})]
      (is (= :ok (mr/validate! built)))
      (is (= (get @example-record "metadata_record_schema_id")
             (get built "metadata_record_schema_id")))
      (is (= (get @example-record "metadata_record_schema_hash")
             (get built "metadata_record_schema_hash"))))))

(deftest record-hash-cascades-from-person-hash-test
  (testing "editing a person body changes its hash, which changes any work record that references it"
    (let [work {"work_id" "000999"
                "title" "テスト"
                "title_reading" nil
                "ndc" "NDC 913"
                "orthographic_style" "新字新仮名"
                "copyright_expired" true
                "aozora_available" "2026-01-01"
                "aozora_modified" "2026-01-01"
                "card_url" "https://www.aozora.gr.jp/cards/000999/card999.html"
                "source_editions" [{"title" "x" "publisher" "y"}]}
          person-v1 @example-person
          person-v2 (assoc person-v1 "family_name_romaji" "Akutagawa-changed")
          person-hash-v1 ((requiring-resolve 'abc.tools.person-record/record-hash) person-v1)
          person-hash-v2 ((requiring-resolve 'abc.tools.person-record/record-hash) person-v2)
          contrib-v1 [{"person_id" "000879"
                       "person_record_hash" person-hash-v1
                       "relation_to_work" "著者"}]
          contrib-v2 [{"person_id" "000879"
                       "person_record_hash" person-hash-v2
                       "relation_to_work" "著者"}]
          record-v1 (mr/build-metadata-record {:work work :contributors contrib-v1})
          record-v2 (mr/build-metadata-record {:work work :contributors contrib-v2})]
      (is (not= person-hash-v1 person-hash-v2)
          "person hash must change when family_name_romaji changes")
      (is (not= (mr/record-hash record-v1) (mr/record-hash record-v2))
          "metadata_record_hash must change when contributors[i].person_record_hash changes"))))
