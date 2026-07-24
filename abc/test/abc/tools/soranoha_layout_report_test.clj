(ns abc.tools.soranoha-layout-report-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.snapshot-index-test :as fixture]
            [abc.tools.soranoha-layout-report :as report]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(deftest regular-files-selects-only-files-in-stable-order-test
  (with-temp-dir [dir]
    (fs/create-dirs (fs/file dir "nested"))
    (spit (fs/file dir "b.txt") "b")
    (spit (fs/file dir "a.txt") "a")
    (spit (fs/file dir "nested" "c.txt") "c")
    (is (= ["a.txt" "b.txt" "c.txt"]
           (mapv (comp str fs/file-name) (#'report/regular-files dir))))))

(deftest empty-root-report-has-zero-files-and-bytes-test
  (with-temp-dir [dir]
    (is (= {"file_count" 0 "byte_count" 0 "referenced_manifest_count" 0}
           (get (report/build-report dir {"artifact_references" []} {})
                "actual_root")))))

(deftest regular-files-does-not-descend-through-directory-symlinks-test
  (with-temp-dir [base]
    (let [root (fs/path base "root")
          external (fs/path base "external")]
      (fs/create-dirs root)
      (fs/create-dirs external)
      (spit (fs/file external "linked.txt") "linked")
      (fs/create-sym-link (fs/path root "linked") external)
      (is (= []
             (mapv (comp str fs/file-name) (#'report/regular-files root)))))))

(deftest layout-report-projects-v2-identity-without-rendering-test
  (with-temp-dir [dir]
    (let [{:keys [root index]} (fixture/build-completed-root! (io/file dir "root"))]
      (with-redefs [materialize-publication/materialize-publication!
                    (fn [& _] (throw (ex-info "layout report must not render" {})))]
        (let [identity-object (get index "snapshot_index_identity_object")
              report (report/build-report root index {})]
          (is (= "0.2.0" (get report "report_version")))
          (is (= (get index "snapshot_date") (get report "snapshot_date")))
          (is (= (get identity-object "source_selection_hash")
                 (get report "source_selection_hash")))
          (is (= (get identity-object "parser_config_hash")
                 (get report "parser_config_hash")))
          (is (= (get identity-object "failure_set_hash")
                 (get report "failure_set_hash")))
          (is (nil? (get report "request_set_label"))
              "retired request-set projection is gone")
          (is (nil? (get report "snapshot_label"))
              "retired snapshot-label projection is gone")
          (is (= 4 (get-in report ["actual_root" "referenced_manifest_count"]))))))))
