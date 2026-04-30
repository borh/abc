(ns abc.tools.person-drift-history-test
  (:require [abc.tools.json :as json]
            [abc.tools.person-drift-history :as history]
            [abc.tools.person-record :as pr]
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

(defn- person-record [person-id family given]
  {"person_record_schema_id" pr/schema-id
   "person_record_schema_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
   "person_id" person-id
   "family_name" family
   "given_name" given
   "family_name_reading" nil
   "given_name_reading" nil
   "family_name_sort" nil
   "given_name_sort" nil
   "family_name_romaji" nil
   "given_name_romaji" nil
   "date_of_birth" nil
   "date_of_death" nil
   "person_copyright_expired" true
   "external_links" []})

(defn- work-record [person-ids]
  {"contributors"
   (mapv (fn [person-id]
           {"person_id" person-id
            "person_record_hash" (str "sha256:" person-id)
            "relation_to_work" "author"})
         person-ids)})

(defn- write-corpus! [root {:keys [persons works]}]
  (doseq [[person-id record] persons]
    (json/write-deterministic-json-file!
     (io/file root "persons" (str person-id ".json"))
     record))
  (doseq [[work-id record] works]
    (json/write-deterministic-json-file!
     (io/file root "works" (str work-id ".json"))
     record))
  root)

(defn- with-corpora [previous current f]
  (let [previous-dir (temp-dir "abc-drift-history-prev")
        current-dir (temp-dir "abc-drift-history-cur")]
    (try
      (write-corpus! previous-dir previous)
      (write-corpus! current-dir current)
      (f previous-dir current-dir)
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)))))

(deftest unchanged-corpus-reports-no-drift-candidates-test
  (testing "unchanged generated corpus snapshots produce an empty audit"
    (let [p1 (person-record "000001" "Alpha" "One")
          p2 (person-record "000002" "Beta" "Two")
          corpus {:persons {"000001" p1
                            "000002" p2}
                  :works {"000100" (work-record ["000001"])
                          "000200" (work-record ["000002"])}}]
      (with-corpora
        corpus corpus
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= "ok" (get report "status")))
            (is (= 0 (get-in report ["summary" "split_candidates"])))
            (is (= 0 (get-in report ["summary" "merge_candidates"])))
            (is (= 0 (get-in report ["summary" "ambiguous_replacements"])))
            (is (= [] (get report "contributor_edge_changes")))))))))

(deftest metadata-correction-is-not-a-drift-candidate-test
  (testing "same person_id with changed identity fields is reported as metadata correction only"
    (let [previous-person (person-record "000001" "Alpha" "One")
          current-person (assoc previous-person "given_name" "Uno")
          previous {:persons {"000001" previous-person}
                    :works {"000100" (work-record ["000001"])}}
          current {:persons {"000001" current-person}
                   :works {"000100" (work-record ["000001"])}}]
      (with-corpora
        previous current
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= 1 (get-in report ["summary" "metadata_corrections"])))
            (is (= [{"person_id" "000001"
                     "changed_fields" ["given_name"]
                     "previous_hash" (pr/record-hash previous-person)
                     "current_hash" (pr/record-hash current-person)}]
                   (get report "metadata_corrections")))
            (is (= 0 (get-in report ["summary" "split_candidates"])))
            (is (= 0 (get-in report ["summary" "merge_candidates"])))))))))

(deftest contributor-set-expansion-is-not-a-split-candidate-test
  (testing "adding a contributor to an existing edge is classified as an ordinary edge addition"
    (let [p1 (person-record "000001" "Alpha" "One")
          p2 (person-record "000002" "Beta" "Two")
          previous {:persons {"000001" p1}
                    :works {"000100" (work-record ["000001"])}}
          current {:persons {"000001" p1
                             "000002" p2}
                   :works {"000100" (work-record ["000001" "000002"])}}]
      (with-corpora
        previous current
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= 1 (get-in report ["summary" "contributor_edge_additions"])))
            (is (= 0 (get-in report ["summary" "split_candidates"])))
            (is (= [{"change_type" "addition"
                     "work_id" "000100"
                     "relation_to_work" "author"
                     "previous_person_ids" ["000001"]
                     "current_person_ids" ["000001" "000002"]
                     "added_person_ids" ["000002"]}]
                   (get report "contributor_edge_changes")))))))))

(deftest one-to-many-replacement-is-a-split-candidate-only-with-retired-source-test
  (testing "one retired previous ID replaced by multiple new IDs is flagged as a split candidate"
    (let [old (person-record "000001" "Alpha" "One")
          new-a (person-record "abc-000000000001" "Alpha" "A")
          new-b (person-record "abc-000000000002" "Alpha" "B")
          previous {:persons {"000001" old}
                    :works {"000100" (work-record ["000001"])}}
          current {:persons {"abc-000000000001" new-a
                             "abc-000000000002" new-b}
                   :works {"000100" (work-record ["abc-000000000001"
                                                  "abc-000000000002"])}}]
      (with-corpora
        previous current
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= 1 (get-in report ["summary" "split_candidates"])))
            (is (= [{"work_id" "000100"
                     "relation_to_work" "author"
                     "source_person_ids" ["000001"]
                     "target_person_ids" ["abc-000000000001" "abc-000000000002"]}]
                   (get report "split_candidates")))
            (is (= [] (get report "merge_candidates")))
            (is (= [] (get report "ambiguous_replacements")))))))))

(deftest many-to-one-replacement-is-a-merge-candidate-only-with-retired-sources-test
  (testing "multiple retired previous IDs replaced by one new ID is flagged as a merge candidate"
    (let [old-a (person-record "000001" "Alpha" "One")
          old-b (person-record "000002" "Beta" "Two")
          new (person-record "abc-000000000001" "Merged" "Person")
          previous {:persons {"000001" old-a
                              "000002" old-b}
                    :works {"000100" (work-record ["000001" "000002"])}}
          current {:persons {"abc-000000000001" new}
                   :works {"000100" (work-record ["abc-000000000001"])}}]
      (with-corpora
        previous current
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= 1 (get-in report ["summary" "merge_candidates"])))
            (is (= [{"work_id" "000100"
                     "relation_to_work" "author"
                     "source_person_ids" ["000001" "000002"]
                     "target_person_ids" ["abc-000000000001"]}]
                   (get report "merge_candidates")))
            (is (= [] (get report "split_candidates")))
            (is (= [] (get report "ambiguous_replacements")))))))))

(deftest one-to-one-replacement-is-ambiguous-not-drift-candidate-test
  (testing "one ID replaced by another is not enough evidence for split or merge"
    (let [old (person-record "000001" "Alpha" "One")
          new (person-record "000002" "Beta" "Two")
          previous {:persons {"000001" old}
                    :works {"000100" (work-record ["000001"])}}
          current {:persons {"000002" new}
                   :works {"000100" (work-record ["000002"])}}]
      (with-corpora
        previous current
        (fn [previous-dir current-dir]
          (let [report (history/report {:previous-dir (str previous-dir)
                                        :current-dir (str current-dir)})]
            (is (= 0 (get-in report ["summary" "split_candidates"])))
            (is (= 0 (get-in report ["summary" "merge_candidates"])))
            (is (= [{"work_id" "000100"
                     "relation_to_work" "author"
                     "previous_person_ids" ["000001"]
                     "current_person_ids" ["000002"]}]
                   (get report "ambiguous_replacements")))))))))

(deftest writes-report-to-output-path-test
  (testing "write-report! writes deterministic JSON when --output is supplied"
    (let [p1 (person-record "000001" "Alpha" "One")
          corpus {:persons {"000001" p1}
                  :works {"000100" (work-record ["000001"])}}]
      (with-corpora
        corpus corpus
        (fn [previous-dir current-dir]
          (let [output (io/file (temp-dir "abc-drift-history-out") "report.json")]
            (try
              (history/write-report! {:previous-dir (str previous-dir)
                                      :current-dir (str current-dir)
                                      :output (str output)})
              (is (.exists output))
              (is (= "ok" (get (json/read-json-file output) "status")))
              (finally
                (delete-recursive (.getParentFile output))))))))))
