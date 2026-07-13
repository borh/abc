(ns abc.tools.soranoha-layout-report-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.soranoha-layout-report :as report]
            [babashka.fs :as fs]
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
