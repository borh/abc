(ns abc.tools.metadata-record-test
  (:require [abc.tools.files :as files]
            [abc.tools.metadata-record :as mr]
            [clojure.test :refer [deftest is testing]]))

(def example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(deftest validate-example-record-test
  (testing "the example fixture validates against the schema"
    (is (= :ok (mr/validate! @example-record)))))

(deftest validate-rejects-missing-required-test
  (testing "validate! throws when a required field is missing"
    (let [bad (dissoc @example-record "work")]
      (is (thrown? clojure.lang.ExceptionInfo (mr/validate! bad))))))

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

(deftest record-hash-sorts-persons-test
  (testing "persons[] order does not change the hash"
    (let [r1 @example-record
          persons (get r1 "persons")
          r2 (assoc r1 "persons" (vec (reverse persons)))]
      (is (= (mr/record-hash r1) (mr/record-hash r2))))))

(deftest build-metadata-record-shape-test
  (testing "build-metadata-record produces a schema-compliant value"
    (let [work (get @example-record "work")
          persons (get @example-record "persons")
          built (mr/build-metadata-record
                 {:work work
                  :persons persons})]
      (is (= :ok (mr/validate! built)))
      (is (= (get @example-record "metadata_record_schema_id")
             (get built "metadata_record_schema_id")))
      (is (= (get @example-record "metadata_record_schema_hash")
             (get built "metadata_record_schema_hash"))))))
