(ns abc.tools.materialize-source-snapshot-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.materialize-source-snapshot :as materialize]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def generated-at "2026-07-04T00:00:00Z")

(defn- workset! [root works]
  (let [workset-file (io/file root "workset.edn")]
    (spit workset-file
          (pr-str {:snapshot_scope "unit-test-source-snapshot"
                   :snapshot_date "2026-07-04"
                   :works works}))
    workset-file))

(deftest materialize-source-snapshot-test
  (let [root (fixture/temp-dir "abc-source-snapshot")
        out-file (io/file root "snapshot.json")]
    (try
      (let [works [(fixture/workset-entry! root
                                           {:slug "one"
                                            :title "一"
                                            :work-id "000001"
                                            :person-id "000101"
                                            :work-hash (fixture/example-hash "a1")})
                   (fixture/workset-entry! root
                                           {:slug "two"
                                            :title "二"
                                            :work-id "000002"
                                            :person-id "000102"
                                            :work-hash (fixture/example-hash "a2")})]
            workset-file (workset! root works)
            result (materialize/materialize-source-snapshot!
                    {:workset-path (str workset-file)
                     :output-path (str out-file)
                     :generated-at generated-at})
            snapshot (files/read-json out-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            manifest-files (map :source-manifest (:works result))
            manifests (map files/read-json manifest-files)
            snapshot-hash (get snapshot "snapshot_hash")
            expected-snapshot-hash
            (hash/format-sha256
             (hash/sha256-json-jcs (get snapshot "snapshot_identity_object")))]
        (is (= out-file (:snapshot result)))
        (is (= expected-snapshot-hash snapshot-hash))
        (is (= "https://w3id.org/abc/source-corpus-snapshot-v0.json"
               (get snapshot "snapshot_schema_id")))
        (is (= ["000001" "000002"]
               (mapv #(get % "work_id")
                     (get-in snapshot ["snapshot_identity_object"
                                       "snapshot_inputs"]))))
        (doseq [[work manifest-file manifest] (map vector works manifest-files manifests)]
          (testing (:slug work)
            (is (.exists manifest-file))
            (is (nil? (schema/validation-errors manifest-schema manifest)))
            (is (= "source" (get manifest "artifact_kind")))
            (is (= snapshot-hash
                   (get-in manifest ["manifest_identity_object"
                                     "corpus_snapshot_hash"])))
            (is (= (get-in (files/read-json (:parser_ir_path work))
                           ["source" "work_content_hash"])
                   (get-in manifest ["manifest_identity_object"
                                     "work_content_hash"])))
            (is (= (metadata-record/record-hash
                    (files/read-json (:metadata_record_path work)))
                   (get-in manifest ["manifest_identity_object"
                                     "metadata_record_hash"])))
            (is (= "index-entry"
                   (get-in manifest ["sidecars" 0 "role"])))
            (is (= "../snapshot.json"
                   (get-in manifest ["sidecars" 0 "path_hint"]))))))
      (finally
        (fixture/delete-tree! root)))))

(deftest materialized-source-snapshot-is-deterministic-test
  (let [root (fixture/temp-dir "abc-source-snapshot-deterministic")]
    (try
      (let [work (fixture/workset-entry! root
                                         {:slug "one"
                                          :title "一"
                                          :work-id "000001"
                                          :person-id "000101"
                                          :work-hash (fixture/example-hash "a1")})
            workset-file (workset! root [work])
            out-a (io/file root "a" "snapshot.json")]
        (materialize/materialize-source-snapshot!
         {:workset-path (str workset-file)
          :output-path (str out-a)
          :generated-at generated-at})
        (let [snapshot-a (slurp out-a)
              manifest-a (slurp (:source_manifest_path work))]
          (materialize/materialize-source-snapshot!
           {:workset-path (str workset-file)
            :output-path (str out-a)
            :generated-at generated-at})
          (is (= snapshot-a (slurp out-a)))
          (is (= manifest-a
                 (slurp (:source_manifest_path work))))))
      (finally
        (fixture/delete-tree! root)))))
