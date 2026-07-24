(ns abc.tools.soranoha-stage-publication-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.files :as files]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.snapshot-index-test :as completed-root]
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

(deftest stage-publication-preserves-source-identity-and-never-renders-test
  (with-temp-dir [dir]
    (let [{:keys [root index]} (completed-root/build-completed-root!
                                (io/file dir "root"))
          staged-root (io/file dir "staged")
          before-identity (get index "snapshot_identity_hash")]
      (with-redefs [materialize-publication/materialize-publication!
                    (fn [& _] (throw (ex-info "staging must not render" {})))
                    materialize-publication/materialize-release-publication!
                    (fn [& _] (throw (ex-info "staging must not render" {})))]
        (let [{staged-snapshot :snapshot}
              (stage-publication/stage-publication!
               {:snapshot-root root
                :staged-root staged-root
                :snapshot index})]
          (testing "locator rewriting does not change snapshot identity"
            (is (= before-identity
                   (get staged-snapshot "snapshot_identity_hash")))
            (is (= (get index "snapshot_index_identity_object")
                   (get staged-snapshot "snapshot_index_identity_object"))))
          (testing "each work's closed file set is co-located by kind and work_slug"
            (is (= "artifacts/tei/by-work/0005_1234_rashomon/tei.manifest.json"
                   (->> (get staged-snapshot "artifact_references")
                        (filter #(= "tei" (get % "artifact_kind")))
                        first
                        (#(get-in % ["locator" "path"])))))
            (is (.exists (io/file staged-root "artifacts" "tei" "by-work"
                                  completed-root/slug
                                  "tei-validation-result.json"))
                "TEI sidecar travels with its manifest and content"))
          (testing "the staged root re-closes over its references"
            (is (empty? (snapshot-index/closure-problems staged-root
                                                         staged-snapshot)))))))))
