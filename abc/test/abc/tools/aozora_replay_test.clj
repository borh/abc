(ns abc.tools.aozora-replay-test
  (:require [abc.sim.model]
            [abc.sim.render :as sim-render]
            [abc.tools.aozora-replay :as replay]
            [abc.tools.json :as abc-json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(deftest locked-pin-test
  (let [dir (temp-dir "abc-replay-lock")
        write! (fn [name value]
                 (let [f (io/file dir name)]
                   (abc-json/write-deterministic-json-file! f value)
                   (str f)))]
    (try
      (is (= (apply str (repeat 40 "a"))
             (replay/locked-pin
              (write! "good.lock"
                      {"nodes" {"aozorabunko-src"
                                {"locked" {"rev" (apply str (repeat 40 "a"))}}}}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/locked-pin (write! "bad.lock" {"nodes" {}}))))
      (finally (delete-recursive dir)))))

(deftest catalog-bytes-fault-test
  (let [m (abc.sim.model/bootstrap 1)
        good-csv (sim-render/rows->csv (sim-render/model->rows m))]
    (is (nil? (replay/catalog-bytes-fault (sim-render/csv->zip-bytes good-csv))))
    (is (= "no-data-rows"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes (sim-render/rows->csv [])))))
    (is (= "no-csv-entry"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes "x" {:no-entry? true}))))
    (is (= "unreadable-zip"
           (replay/catalog-bytes-fault (.getBytes "this is not a zip" "UTF-8"))))))

(defn- pair [prev cur period split-count]
  {"previous_ref" prev "current_ref" cur "period" period
   "status" "ok"
   "drift_summary" {"split_candidates" split-count}
   "ingest" {"works_written" 1 "works_skipped" 0 "skipped_work_ids" []
             "persons_written" 1 "person_conflicts" []}})

(defn- doc [pin pairs excluded]
  {"baseline_format" replay/baseline-format
   "remote_url" replay/default-remote-url
   "pin_rev" pin
   "zip_path" replay/default-zip-path
   "sample_period" "year"
   "excluded" excluded
   "pairs" pairs})

(deftest classify-diff-test
  (let [p1 (pair "r0" "r1" "2023" 0)
        p2 (pair "r1" "r2" "2024" 1)
        old (doc "pinA" [p1 p2] [])]
    (testing "unchanged"
      (is (= :unchanged (:verdict (replay/classify-diff old old)))))
    (testing "configuration-change wins over everything"
      (is (= :configuration-change
             (:verdict (replay/classify-diff
                        old (assoc (doc "pinB" [p1 p2] []) "sample_period" "month"))))))
    (testing "any change with unchanged pin is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinA" [p1 (pair "r1" "r2" "2024" 2)] []))))))
    (testing "pin-bump-shaped: strict final replacement + append"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                         (pair "r3" "r4" "2025" 0)] []))))))
    (testing "digest change on unchanged final refs is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r2" "2024" 9)] []))))))
    (testing "final replacement that moves to a different period is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2025" 1)] []))))))
    (testing "historical pair change is behavioral even with pin bump"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [(pair "r0" "r1" "2023" 5) p2] []))))))
    (testing "exclusion for a historical period is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2023" "reason" "no-csv-entry"}]))))))
    (testing "exclusion for a NEW period is pin-bump-shaped"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2025" "reason" "no-csv-entry"}]))))))
    (testing "pair-changes classification"
      (is (= ["unchanged" "replaced" "added"]
             (mapv #(get % "change")
                   (:pair-changes (replay/classify-diff
                                   old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                                    (pair "r3" "r4" "2025" 0)] [])))))))))

(deftest pair-digest-test
  (let [digest (replay/pair-digest
                {"cur-sha" "2024"}
                {:previous_ref "prev-sha" :current_ref "cur-sha" :status "ok"
                 :drift {"summary" {"split_candidates" 2}}
                 :current_ingest {:works-written 3 :works-skipped 1
                                  :skipped-work-ids ["000101"]
                                  :persons-written 4
                                  :person-conflicts [{"person_id" "000009"
                                                      "chosen_work_id" "000101"
                                                      "work_ids" ["000101" "000102"]}]}})]
    (is (= {"previous_ref" "prev-sha" "current_ref" "cur-sha" "period" "2024"
            "status" "ok"
            "drift_summary" {"split_candidates" 2}
            "ingest" {"works_written" 3 "works_skipped" 1
                      "skipped_work_ids" ["000101"] "persons_written" 4
                      "person_conflicts" ["000009"]}}
           digest))))
