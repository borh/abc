(ns abc.tools.source-snapshot-workset-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.materialize-source-snapshot :as materialize]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.source-snapshot-workset :as workset]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest workset-includes-a-directory-link-that-is-itself-a-work-dir-test
  (let [base (fixture/temp-dir "abc-source-snapshot-linked-work")
        root (io/file base "root")
        external (io/file base "external")]
    (try
      (.mkdirs root)
      (fixture/materialized-work! external {:slug "linked"
                                            :title "一"
                                            :work-id "000001"
                                            :person-id "000101"
                                            :work-hash (fixture/example-hash "a1")})
      (fs/create-sym-link (fs/path root "linked-work")
                          (fs/path external "works" "linked"))
      (is (= ["000001"]
             (mapv :work_id
                   (:works (workset/workset-from-root
                            {:input-root (str root)
                             :path-base (str base)
                             :snapshot-scope "unit-test"
                             :snapshot-date "2026-07-07"})))))
      (finally
        (fixture/delete-tree! base)))))

(deftest workset-excludes-work-dirs-nested-below-a-directory-link-test
  (let [base (fixture/temp-dir "abc-source-snapshot-linked-parent")
        root (io/file base "root")
        external (io/file base "external")]
    (try
      (.mkdirs root)
      (fixture/materialized-work! root {:slug "visible"
                                        :title "二"
                                        :work-id "000002"
                                        :person-id "000102"
                                        :work-hash (fixture/example-hash "a2")})
      (fixture/materialized-work! external {:slug "nested"
                                            :title "一"
                                            :work-id "000001"
                                            :person-id "000101"
                                            :work-hash (fixture/example-hash "a1")})
      (fs/create-sym-link (fs/path root "linked-works") external)
      (is (= ["000002"]
             (mapv :work_id
                   (:works (workset/workset-from-root
                            {:input-root (str root)
                             :path-base (str base)
                             :snapshot-scope "unit-test"
                             :snapshot-date "2026-07-07"})))))
      (finally
        (fixture/delete-tree! base)))))

(deftest workset-from-materialized-root-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset")
        output (io/file root "workset.edn")]
    (try
      (fixture/materialized-work! root {:slug "zeta"
                                        :title "二"
                                        :work-id "000002"
                                        :person-id "000102"
                                        :work-hash (fixture/example-hash "a2")})
      (fixture/materialized-work! root {:slug "alpha"
                                        :title "一"
                                        :work-id "000001"
                                        :person-id "000101"
                                        :work-hash (fixture/example-hash "a1")})
      (let [result (workset/write-workset!
                    {:input-root (str root)
                     :output-path (str output)
                     :snapshot-scope "unit-test-generated-source-snapshot"
                     :snapshot-date "2026-07-07"})
            generated (edn/read-string (slurp output))]
        (is (= output (:output result)))
        (is (= 2 (:works-count result)))
        (is (= "unit-test-generated-source-snapshot"
               (:snapshot_scope generated)))
        (is (= "2026-07-07"
               (:snapshot_date generated)))
        (is (= ["000001" "000002"]
               (mapv :work_id (:works generated))))
        (is (= ["alpha" "zeta"]
               (mapv :slug (:works generated))))
        (testing "work entries are relative to the output workset"
          (is (= [{:slug "alpha"
                   :title "一"
                   :work_id "000001"
                   :person_id "000101"
                   :card_url "https://www.aozora.gr.jp/cards/000101/card000001.html"
                   :aat_path "works/alpha/aat.json"
                   :parser_ir_path "works/alpha/parser-ir.json"
                   :metadata_record_path "works/alpha/metadata-record.json"
                   :official_source_path "works/alpha/official-source.json"
                   :source_bundle_path "works/alpha/source-bundle.json"
                   :source_manifest_path "works/alpha/source.manifest.json"}
                  {:slug "zeta"
                   :title "二"
                   :work_id "000002"
                   :person_id "000102"
                   :card_url "https://www.aozora.gr.jp/cards/000102/card000002.html"
                   :aat_path "works/zeta/aat.json"
                   :parser_ir_path "works/zeta/parser-ir.json"
                   :metadata_record_path "works/zeta/metadata-record.json"
                   :official_source_path "works/zeta/official-source.json"
                   :source_bundle_path "works/zeta/source-bundle.json"
                   :source_manifest_path "works/zeta/source.manifest.json"}]
                 (:works generated)))))
      (finally
        (fixture/delete-tree! root)))))

(deftest generated-workset-requires-source-bundle-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-no-bundle")]
    (try
      (let [work-dir (fixture/materialized-work!
                      root {:slug "broken"
                            :title "一"
                            :work-id "000001"
                            :person-id "000101"
                            :work-hash (fixture/example-hash "a1")})]
        (.delete (io/file work-dir "source-bundle.json"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"missing source-bundle.json"
             (workset/workset-from-root
              {:input-root (str root)
               :snapshot-scope "unit-test"
               :snapshot-date "2026-07-07"}))))
      (finally
        (fixture/delete-tree! root)))))

(deftest read-workset-resolves-source-bundle-path-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-resolve")
        workset-file (io/file root "workset.edn")]
    (try
      (spit workset-file
            (pr-str {:snapshot_scope "unit-test"
                     :snapshot_date "2026-07-07"
                     :works [{:slug "one"
                              :source_bundle_path "one/source-bundle.json"}]}))
      (is (= (str (.getCanonicalFile
                   (io/file root "one" "source-bundle.json")))
             (-> (workset/read-workset workset-file)
                 :works first :resolved_source_bundle_path)))
      (finally
        (fixture/delete-tree! root)))))

(deftest missing-required-file-is-rejected-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-missing")]
    (try
      (let [work-dir (io/file root "works" "broken")]
        (abc-json/write-deterministic-json-file! (io/file work-dir "aat.json")
                                                 {"version" 1})
        (abc-json/write-deterministic-json-file! (io/file work-dir "metadata-record.json")
                                                 (fixture/metadata-record "000001"
                                                                          "一"
                                                                          "000101"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"missing parser-ir.json"
             (workset/workset-from-root
              {:input-root (str root)
               :snapshot-scope "unit-test"
               :snapshot-date "2026-07-07"}))))
      (finally
        (fixture/delete-tree! root)))))

(deftest missing-official-source-is-rejected-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-non-work")]
    (try
      (let [work-dir (io/file root "works" "support-like")]
        (fixture/write-work-files! work-dir {:slug "support-like"
                                             :title "支援ファイル"
                                             :work-id "000001"
                                             :person-id "000101"
                                             :work-hash (fixture/example-hash "a1")})
        (.delete (io/file work-dir "official-source.json"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"missing official-source.json"
             (workset/workset-from-root
              {:input-root (str root)
               :snapshot-scope "unit-test"
               :snapshot-date "2026-07-07"}))))
      (finally
        (fixture/delete-tree! root)))))

(deftest non-card-official-source-is-rejected-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-support")]
    (try
      (let [work-dir (io/file root "works" "support-like")]
        (fixture/write-work-files! work-dir {:slug "support-like"
                                             :title "支援ファイル"
                                             :work-id "000001"
                                             :person-id "000101"
                                             :work-hash (fixture/example-hash "a1")})
        (abc-json/write-deterministic-json-file!
         (io/file work-dir "official-source.json")
         {"work_id" "000001"
          "text_zip_relpath" "tools/JISTABLE.zip"
          "source_hash" (fixture/example-hash "a1")})
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"not an Aozora card/files work source"
             (workset/workset-from-root
              {:input-root (str root)
               :snapshot-scope "unit-test"
               :snapshot-date "2026-07-07"}))))
      (finally
        (fixture/delete-tree! root)))))

(deftest generated-workset-materializes-source-snapshot-test
  (let [root (fixture/temp-dir "abc-source-snapshot-workset-materialize")
        workset-file (io/file root "workset.edn")
        snapshot-file (io/file root "snapshot.json")]
    (try
      (fixture/materialized-work! root {:slug "alpha"
                                        :title "一"
                                        :work-id "000001"
                                        :person-id "000101"
                                        :work-hash (fixture/example-hash "a1")})
      (workset/write-workset!
       {:input-root (str root)
        :output-path (str workset-file)
        :snapshot-scope "unit-test-generated-source-snapshot"
        :snapshot-date "2026-07-07"})
      (let [result (materialize/materialize-source-snapshot!
                    {:workset-path (str workset-file)
                     :output-path (str snapshot-file)})
            snapshot (files/read-json snapshot-file)
            source-manifest (io/file root "works" "alpha" "source.manifest.json")]
        (is (= snapshot-file (:snapshot result)))
        (is (= "works/alpha/aat.json"
               (get-in snapshot ["snapshot_identity_object"
                                 "snapshot_inputs"
                                 0
                                 "aat_path"])))
        (is (= "works/alpha/official-source.json"
               (get-in snapshot ["snapshot_identity_object"
                                 "snapshot_inputs"
                                 0
                                 "official_source_path"])))
        (is (= "cards/000101/files/000001_ruby_fixture.zip"
               (get-in snapshot ["snapshot_identity_object"
                                 "snapshot_inputs"
                                 0
                                 "official_text_zip_relpath"])))
        (is (.exists source-manifest))
        (is (= "source"
               (get (files/read-json source-manifest)
                    "artifact_kind"))))
      (finally
        (fixture/delete-tree! root)))))
