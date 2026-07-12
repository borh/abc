(ns abc.sim.oracle-test
  (:require [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [clojure.test :refer [deftest is testing]]))

(deftest projection-drops-unattached-test
  (let [m (model/bootstrap 1)
        m' (:model (model/apply-event m {:event/type :add-person :pid "900001"
                                         :person (model/base-person)}))
        m'' (:model (model/apply-event m' {:event/type :add-work :wid "800001"
                                           :work (model/base-work "800001")}))
        p (oracle/projection m'')]
    (is (not (contains? (:persons p) "900001")))
    (is (not (contains? (:works p) "800001")))
    (is (= (:edges m'') (:edges p)))))

(deftest confusable-detects-split-shaped-benign-diff-test
  (let [m (model/bootstrap 1)
        ;; benign composition equivalent to a split: remove person A's edge +
        ;; work, then new persons take the same [work relation] key over.
        steps [{:event/type :remove-edge :wid "000101" :relation "著者" :pid "000001"}
               {:event/type :add-person-with-edge :pid "900001"
                :person (model/base-person) :wid "000101" :relation "著者"}
               {:event/type :add-person-with-edge :pid "900002"
                :person (model/base-person) :wid "000101" :relation "著者"}]
        {:keys [states]} (model/fold-history {:initial m :events steps})]
    (is (oracle/confusable? (first states) (peek states)))
    (is (not (oracle/confusable? m m)))))

(deftest model-diff-counts-test
  (let [m (model/bootstrap 2)
        {:keys [states]} (model/fold-history
                          {:initial m
                           :events [{:event/type :edit-person :pid "000001"
                                     :field :family_name :value "改"}
                                    {:event/type :add-edge :wid "000102"
                                     :relation "翻訳者" :pid "000001"}]})
        d (oracle/model-diff (first states) (peek states))]
    (is (= #{} (:added-pids d)))
    (is (= #{"000001"} (:corrected-pids d)))
    (is (= 1 (get-in d [:edge-counts :additions])))))

(deftest semantic-report-strips-locators-test
  (is (= {:drift {"summary" {"split_candidates" 0}}}
         (oracle/semantic-report
          {:drift {"previous_dir" "/tmp/x" "current_dir" "/tmp/y"
                   "summary" {"split_candidates" 0}}
           :work-dir "/tmp/w" :corpus-dirs {:previous "/a"}}))))

(deftest expected-candidates-are-edge-local-test
  (let [m (model/bootstrap 1)
        m1 (:model (model/apply-event m {:event/type :add-edge :wid "000101"
                                         :relation "翻訳者" :pid "000001"}))
        {:keys [applied]} (model/fold-history
                           {:initial m1
                            :events [{:event/type :clean-split :pid "000001"
                                      :targets ["900001" "900002"]
                                      :persons {"900001" (model/base-person)
                                                "900002" (model/base-person)}}]})
        cands (oracle/expected-split-candidates (first applied))]
    (is (= 2 (count cands)))
    (is (= #{["000101" "著者"] ["000101" "翻訳者"]}
           (set (map (juxt #(get % "work_id") #(get % "relation_to_work")) cands))))
    (is (every? #(= ["000001"] (get % "source_person_ids")) cands))
    (is (every? #(= ["900001" "900002"] (get % "target_person_ids")) cands))))

;; P15 participant-update oracle. bootstrap 2: works 000101/000102, sole
;; authors 000001/000002.
(deftest expected-participant-updates-test
  (let [m0 (model/bootstrap 2)
        edited (:model (model/apply-event
                        m0 {:event/type :edit-person :pid "000001"
                            :field :family_name :value "変"}))
        ;; removing 000101 detaches sole author 000001 → drops from projection
        removed (:model (model/apply-event
                         m0 {:event/type :remove-work :wid "000101"}))]
    (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
           (oracle/expected-participant-updates
            m0 edited ["000001" "000002" "999999"]))
        "field edit → hash_changed; untouched and never-present pids quiet")
    (is (= [{"person_id" "000001" "change_type" "removed"}]
           (oracle/expected-participant-updates m0 removed ["000001" "000002"])))
    (is (= [{"person_id" "000001" "change_type" "added"}]
           (oracle/expected-participant-updates removed m0 ["000001"])))
    (is (= [] (oracle/expected-participant-updates m0 m0 ["000001" "999999"]))
        "identical endpoints → no entries")
    ;; cur = edit 000001, then detach 000002 (remove its sole work 000102)
    (let [cur (:model (model/apply-event edited {:event/type :remove-work
                                                 :wid "000102"}))]
      (is (= [{"person_id" "000001" "change_type" "hash_changed"}
              {"person_id" "000002" "change_type" "removed"}]
             (oracle/expected-participant-updates m0 cur ["000002" "000001"]))
          "multiple entries, sorted by person_id regardless of input order"))))
