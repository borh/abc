(ns abc.tools.soranoha-stage-publication-test
  (:require [abc.tools.files :as files]
            [abc.tools.soranoha-stage-publication :as stage-publication]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest stage-publication-replaces-output-and-copies-optional-run-summary-test
  (let [root (fixture/temp-dir "abc-stage-publication")
        snapshot-root (io/file root "snapshot")
        staged-root (io/file root "staged")
        run-summary (io/file snapshot-root "run-summary.json")
        snapshot {"layout_policy" {"loose_artifact_kinds" []
                                   "batched_artifact_kinds" []
                                   "archive_format" "tar.zst"}
                  "artifact_references" []}]
    (.mkdirs snapshot-root)
    (.mkdirs staged-root)
    (spit (io/file staged-root "stale.txt") "stale")
    (spit run-summary "{\"status\":\"complete\"}\n")
    (let [{:keys [index-file archive-count]}
          (stage-publication/stage-publication!
           {:snapshot-root snapshot-root
            :staged-root staged-root
            :snapshot snapshot})]
      (testing "the staging directory is replaced instead of overlaid"
        (is (not (.exists (io/file staged-root "stale.txt")))))
      (testing "the optional run summary is copied verbatim"
        (is (= (slurp run-summary)
               (slurp (io/file staged-root "run-summary.json")))))
      (is (= snapshot (files/read-json index-file)))
      (is (zero? archive-count)))))

(deftest stage-publication-omits-absent-run-summary-test
  (let [root (fixture/temp-dir "abc-stage-publication-no-summary")
        snapshot-root (io/file root "snapshot")
        staged-root (io/file root "staged")]
    (.mkdirs snapshot-root)
    (stage-publication/stage-publication!
     {:snapshot-root snapshot-root
      :staged-root staged-root
      :snapshot {"layout_policy" {}
                 "artifact_references" []}})
    (is (not (.exists (io/file staged-root "run-summary.json"))))))
