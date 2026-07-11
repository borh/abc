(ns abc.sim.model-test
  (:require [abc.sim.model :as model]
            [clojure.test :refer [deftest is testing]]))

(deftest bootstrap-shape-test
  (let [m (model/bootstrap 3)]
    (is (= 3 (count (:works m))))
    (is (= 3 (count (:persons m))))
    (is (= 3 (count (:edges m))))
    (is (nil? (model/check-invariants! m nil)))))

(deftest benign-events-apply-and-no-op-test
  (let [m (model/bootstrap 1)
        wid (first (keys (:works m)))
        pid (first (keys (:persons m)))]
    (testing "add-edge applies"
      (let [e {:event/type :add-edge :wid wid :relation "翻訳者" :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (= :add-edge (:intent intent)))
        (is (contains? (:edges m') [wid "翻訳者"]))))
    (testing "add-edge no-ops when pid already on the edge"
      (let [rel (second (first (keys (:edges m))))
            e {:event/type :add-edge :wid wid :relation rel :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent))
        (is (= m m'))))
    (testing "remove-edge deletes an emptied edge and may orphan the person"
      (let [[k pids] (first (:edges m))
            e {:event/type :remove-edge :wid (first k) :relation (second k)
               :pid (first pids)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (not (contains? (:edges m') k)))
        ;; person survives in the model (projection drops it later)
        (is (contains? (:persons m') (first pids)))
        (is (nil? (model/check-invariants! m' e)))))
    (testing "edit-person changes one field"
      (let [e {:event/type :edit-person :pid pid :field :family_name :value "改"}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= "改" (get-in m' [:persons pid :family_name])))))
    (testing "remove-work drops the work and its edges"
      (let [e {:event/type :remove-work :wid wid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (not (contains? (:works m') wid)))
        (is (not-any? #(= wid (first %)) (keys (:edges m'))))))))

(deftest invalid-relation-no-op-test
  (let [m (model/bootstrap 1)
        wid (first (keys (:works m)))
        pid (first (keys (:persons m)))]
    (testing "add-edge no-ops on blank relation"
      (let [e {:event/type :add-edge :wid wid :relation "" :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent))
        (is (= m m'))))
    (testing "add-edge no-ops on non-string relation"
      (let [e {:event/type :add-edge :wid wid :relation 5 :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent))
        (is (= m m'))))
    (testing "add-work-with-edge no-ops on non-string relation"
      (let [e {:event/type :add-work-with-edge
               :wid "800001" :work (model/base-work "800001")
               :pid "900001" :person (model/base-person)
               :relation 5}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent))
        (is (= m m'))))))

(deftest fold-history-collects-applied-intents-test
  (let [m (model/bootstrap 1)
        wid (first (keys (:works m)))
        pid (first (keys (:persons m)))
        events [{:event/type :edit-person :pid pid :field :given_name :value "二"}
                ;; stale event: same value again -> no-op after the first applies
                {:event/type :edit-person :pid pid :field :given_name :value "二"}
                {:event/type :remove-work :wid wid}]
        {:keys [states applied]} (model/fold-history {:initial m :events events})]
    (is (= 4 (count states)))
    (is (= [:edit-person :remove-work] (mapv :intent applied)))))
