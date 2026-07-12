(ns abc.tools.materialize-source-snapshot-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as materialize]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [abc.tools.source-bundle :as source-bundle]
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

(defn- update-json! [path f]
  (abc-json/write-deterministic-json-file! path (f (files/read-json path))))

(defn- exception-info [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e e)))

(defn- materialize-error [mutate!]
  (let [root (fixture/temp-dir "abc-source-snapshot-corrupt")]
    (try
      (let [work (fixture/workset-entry!
                  root {:slug "one"
                        :title "一"
                        :work-id "000001"
                        :person-id "000101"
                        :work-hash (fixture/example-hash "a1")})
            workset-file (workset! root [work])]
        (mutate! work)
        (exception-info
         #(materialize/materialize-source-snapshot!
           {:workset-path (str workset-file)
            :output-path (str (io/file root "snapshot.json"))
            :generated-at generated-at})))
      (finally
        (fixture/delete-tree! root)))))

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
            (is (= (manifest/file-hash (:source_bundle_path work))
                   (get-in manifest ["content" "content_hash"])))
            (is (some #{(manifest/file-hash (:source_bundle_path work))}
                      (get-in manifest ["provenance" "used"])))
            (is (= "application/json"
                   (get-in manifest ["content" "media_type"])))
            (is (= "source-bundle.json"
                   (get-in manifest ["content" "path_hint"])))
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

(deftest snapshot-records-source-identity-roles-test
  (let [root (fixture/temp-dir "abc-source-snapshot-roles")]
    (try
      (let [work (fixture/workset-entry!
                  root {:slug "one"
                        :title "一"
                        :work-id "000001"
                        :person-id "000101"
                        :work-hash (fixture/example-hash "a1")})
            out-file (io/file root "snapshot.json")]
        (materialize/materialize-source-snapshot!
         {:workset-path (str (workset! root [work]))
          :output-path (str out-file)
          :generated-at generated-at})
        (let [input (get-in (files/read-json out-file)
                            ["snapshot_identity_object" "snapshot_inputs" 0])
              official (files/read-json (:official_source_path work))]
          (is (= (get official "archive_hash") (get input "archive_hash")))
          (is (not= (get input "archive_hash")
                    (get input "work_content_hash")))
          (is (= (get official "bundle_hash")
                 (get input "work_content_hash")))
          (is (= (get official "primary_text_hash")
                 (get input "primary_text_hash")))
          (is (= (get official "primary_text_member")
                 (get input "primary_text_member")))
          (is (= (:source_bundle_path work)
                 (get input "source_bundle_path")))
          (is (= (manifest/file-hash (:source_bundle_path work))
                 (get input "source_bundle_file_hash")))))
      (finally
        (fixture/delete-tree! root)))))

(deftest complete-legacy-workset-remains-readable-test
  (let [root (fixture/temp-dir "abc-source-snapshot-legacy")]
    (try
      (let [work (fixture/legacy-workset-entry!
                  root {:slug "one"
                        :title "一"
                        :work-id "000001"
                        :person-id "000101"
                        :work-hash (fixture/example-hash "a1")})
            out-file (io/file root "snapshot.json")]
        (is (= out-file
               (:snapshot
                (materialize/materialize-source-snapshot!
                 {:workset-path (str (workset! root [work]))
                  :output-path (str out-file)
                  :generated-at generated-at}))))
        (is (= (fixture/example-hash "a1")
               (get-in (files/read-json out-file)
                       ["snapshot_identity_object" "snapshot_inputs" 0
                        "work_content_hash"])))
        (let [source-manifest (files/read-json (:source_manifest_path work))]
          (is (= (fixture/example-hash "a1")
                 (get-in source-manifest ["content" "content_hash"])))
          (is (= "text/plain; charset=Shift_JIS"
                 (get-in source-manifest ["content" "media_type"])))
          (is (= "source.txt"
                 (get-in source-manifest ["content" "path_hint"])))))
      (finally
        (fixture/delete-tree! root)))))

(deftest persisted-source-bundle-byte-hash-detects-later-corruption-test
  (let [root (fixture/temp-dir "abc-source-snapshot-file-hash")]
    (try
      (let [work (fixture/workset-entry!
                  root {:slug "one"
                        :title "一"
                        :work-id "000001"
                        :person-id "000101"
                        :work-hash (fixture/example-hash "a1")})
            out-file (io/file root "snapshot.json")]
        (materialize/materialize-source-snapshot!
         {:workset-path (str (workset! root [work]))
          :output-path (str out-file)
          :generated-at generated-at})
        (let [recorded (get-in (files/read-json out-file)
                               ["snapshot_identity_object" "snapshot_inputs" 0
                                "source_bundle_file_hash"])]
          (spit (:source_bundle_path work) "\n" :append true)
          (is (not= recorded (manifest/file-hash (:source_bundle_path work))))))
      (finally
        (fixture/delete-tree! root)))))

(deftest partial-legacy-new-identity-modes-are-rejected-test
  (doseq [[label mutate!]
          [["missing workset source bundle path"
            #(dissoc % :source_bundle_path)]
           ["missing official archive hash"
            (fn [work]
              (update-json! (:official_source_path work)
                            #(dissoc % "archive_hash"))
              work)]
           ["missing official bundle hash"
            (fn [work]
              (update-json! (:official_source_path work)
                            #(dissoc % "bundle_hash"))
              work)]
           ["missing official primary member"
            (fn [work]
              (update-json! (:official_source_path work)
                            #(dissoc % "primary_text_member"))
              work)]
           ["missing official primary hash"
            (fn [work]
              (update-json! (:official_source_path work)
                            #(dissoc % "primary_text_hash"))
              work)]
           ["missing parser primary hash"
            (fn [work]
              (update-json! (:parser_ir_path work)
                            #(update % "source" dissoc "primary_text_hash"))
              work)]]]
    (let [root (fixture/temp-dir "abc-source-snapshot-mixed")]
      (try
        (let [work (fixture/workset-entry!
                    root {:slug "one"
                          :title "一"
                          :work-id "000001"
                          :person-id "000101"
                          :work-hash (fixture/example-hash "a1")})
              mutated (mutate! work)
              error (exception-info
                     #(materialize/materialize-source-snapshot!
                       {:workset-path (str (workset! root [mutated]))
                        :output-path (str (io/file root "snapshot.json"))}))]
          (is (= :mixed-source-identity-mode (:reason (ex-data error))) label))
        (finally
          (fixture/delete-tree! root))))))

(deftest source-identity-corruptions-have-distinct-diagnostics-test
  (let [other (fixture/example-hash "ff")
        cases
        [{:label "official archive alias"
          :role :archive-alias
          :mutate (fn [work]
                    (update-json! (:official_source_path work)
                                  #(assoc % "source_hash" other)))}
         {:label "source bundle archive provenance"
          :role :source-bundle-archive
          :mutate (fn [work]
                    (update-json! (:source_bundle_path work)
                                  #(assoc % "archive_hash" other)))}
         {:label "source bundle identity construction"
          :role :bundle-construction
          :mutate (fn [work]
                    (update-json! (:source_bundle_path work)
                                  #(assoc-in % ["identity_object" "members" 0
                                                "member_hash"] other)))}
         {:label "official bundle"
          :role :official-bundle
          :mutate (fn [work]
                    (update-json! (:official_source_path work)
                                  #(assoc % "bundle_hash" other)))}
         {:label "parser bundle"
          :role :parser-bundle
          :mutate (fn [work]
                    (update-json! (:parser_ir_path work)
                                  #(assoc-in % ["source" "work_content_hash"]
                                             other)))}
         {:label "parser primary text"
          :role :parser-primary-text
          :mutate (fn [work]
                    (update-json! (:parser_ir_path work)
                                  #(assoc-in % ["source" "primary_text_hash"]
                                             other)))}
         {:label "primary member"
          :role :primary-member
          :mutate (fn [work]
                    (update-json! (:official_source_path work)
                                  #(assoc % "primary_text_member"
                                          "images/cover.png")))}]]
    (doseq [{:keys [label role mutate]} cases]
      (let [error (materialize-error mutate)]
        (is (= role (:identity-role (ex-data error))) label)))))

(deftest coordinated-primary-member-divergence-is-rejected-test
  (let [divergent-hash (fixture/example-hash "dd")
        error
        (materialize-error
         (fn [work]
           (let [primary-member (get (files/read-json
                                      (:official_source_path work))
                                     "primary_text_member")]
             (update-json!
              (:source_bundle_path work)
              #(update % "members"
                       (fn [members]
                         (mapv (fn [member]
                                 (if (= primary-member (get member "path"))
                                   (assoc member "member_hash" divergent-hash)
                                   member))
                               members))))
             (update-json! (:official_source_path work)
                           #(assoc % "primary_text_hash" divergent-hash))
             (update-json! (:parser_ir_path work)
                           #(assoc-in % ["source" "primary_text_hash"]
                                      divergent-hash)))))]
    (is (= :member-identity-projection
           (:identity-role (ex-data error))))))

(deftest reordered-member-metadata-projection-is-rejected-test
  (let [error (materialize-error
               #(update-json! (:source_bundle_path %)
                              (fn [source-bundle]
                                (update source-bundle "members"
                                        (comp vec reverse)))))]
    (is (= :member-identity-projection
           (:identity-role (ex-data error))))))

(deftest coordinated-reordered-and-resigned-identity-is-rejected-test
  (let [error
        (materialize-error
         (fn [work]
           (let [source-bundle-path (:source_bundle_path work)
                 reordered
                 (update-json!
                  source-bundle-path
                  (fn [source-bundle]
                    (let [identity (update (get source-bundle "identity_object")
                                           "members" (comp vec reverse))
                          bundle-hash
                          (source-bundle/bundle-identity-hash identity)]
                      (-> source-bundle
                          (assoc "identity_object" identity
                                 "bundle_hash" bundle-hash)
                          (update "members" (comp vec reverse))))))
                 resigned-hash (get (files/read-json source-bundle-path)
                                    "bundle_hash")]
             (update-json! (:official_source_path work)
                           #(assoc % "bundle_hash" resigned-hash))
             (update-json! (:parser_ir_path work)
                           #(assoc-in % ["source" "work_content_hash"]
                                      resigned-hash))
             reordered)))]
    (is (= :source-bundle-identity-structure
           (:identity-role (ex-data error))))
    (is (= :identity-member-order (:reason (ex-data error))))))

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
