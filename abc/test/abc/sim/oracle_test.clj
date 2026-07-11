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
