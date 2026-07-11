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

(deftest clean-split-test
  (let [m (model/bootstrap 2)
        pid "000001"
        e {:event/type :clean-split :pid pid :targets ["900001" "900002"]
           :persons {"900001" (assoc (model/base-person) :given_name "一")
                     "900002" (assoc (model/base-person) :given_name "二")}}
        {m' :model intent :applied} (model/apply-event m e)]
    (is (= :clean-split (:intent intent)))
    (is (not (contains? (:persons m') pid)))
    (is (= #{"900001" "900002"} (get-in m' [:edges ["000101" "著者"]])))
    (is (nil? (model/check-invariants! m' e)))
    (testing "no-op when source has a co-contributor"
      (let [m2 (:model (model/apply-event m {:event/type :add-edge :wid "000101"
                                             :relation "著者" :pid "000002"}))
            r (model/apply-event m2 e)]
        (is (nil? (:applied r)))
        (is (= m2 (:model r)))))))

(deftest clean-merge-test
  (let [m0 (model/bootstrap 1)
        ;; build an edge whose full contributor set is the two sources
        m1 (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                          :pid "000002"
                                          :person (model/base-person)
                                          :wid "000101" :relation "著者"}))
        e {:event/type :clean-merge :pids ["000001" "000002"] :target "900001"
           :person (assoc (model/base-person) :given_name "合")}
        {m' :model intent :applied} (model/apply-event m1 e)]
    (is (= :clean-merge (:intent intent)))
    (is (= #{"900001"} (get-in m' [:edges ["000101" "著者"]])))
    (is (not-any? #(contains? (:persons m') %) ["000001" "000002"]))))

(deftest partial-split-retains-source-test
  (let [m0 (model/bootstrap 1)
        m1 (:model (model/apply-event m0 {:event/type :add-edge :wid "000101"
                                          :relation "翻訳者" :pid "000001"}))
        e {:event/type :partial-split :pid "000001" :targets ["900001"]
           :edge-keys [["000101" "翻訳者"]]
           :persons {"900001" (model/base-person)}}
        {m' :model intent :applied} (model/apply-event m1 e)]
    (is (some? intent))
    (is (contains? (:persons m') "000001"))
    (is (= #{"900001"} (get-in m' [:edges ["000101" "翻訳者"]])))
    (is (= #{"000001"} (get-in m' [:edges ["000101" "著者"]])))
    (testing "no-op when edge-keys are ALL of the source's edges"
      (let [e-all (assoc e :edge-keys [["000101" "著者"] ["000101" "翻訳者"]])]
        (is (nil? (:applied (model/apply-event m1 e-all))))))))

(deftest malformed-events-no-op-test
  ;; totality over structurally possible but invalid payloads: shrinking
  ;; must yield no-ops, never invariant violations
  (let [m (model/bootstrap 1)]
    (doseq [e [;; :persons keys disagree with :targets
               {:event/type :clean-split :pid "000001" :targets ["900001" "900002"]
                :persons {"900001" (model/base-person)}}
               ;; duplicate targets
               {:event/type :clean-split :pid "000001" :targets ["900001" "900001"]
                :persons {"900001" (model/base-person)}}
               ;; source among targets
               {:event/type :clean-split :pid "000001" :targets ["000001" "900001"]
                :persons {"000001" (model/base-person) "900001" (model/base-person)}}
               ;; missing person body
               {:event/type :clean-merge :pids ["000001" "000002"] :target "900001"
                :person nil}
               ;; target = source
               {:event/type :ambiguous-replacement :pid "000001" :target "000001"
                :person (model/base-person)}
               ;; nil work body on a benign add
               {:event/type :add-work-with-edge :wid "800001" :work nil
                :pid "900009" :person (model/base-person) :relation "著者"}]]
      (let [{m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent) (pr-str e))
        (is (= m m') (pr-str e))))))

(deftest impure-and-ambiguous-test
  (let [m (model/bootstrap 2)]
    (testing "impure-split reuses an existing person as one successor"
      (let [e {:event/type :impure-split :pid "000001"
               :existing-target "000002" :new-target "900001"
               :person (model/base-person)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= #{"000002" "900001"} (get-in m' [:edges ["000101" "著者"]])))))
    (testing "ambiguous-replacement swaps 1→1"
      (let [e {:event/type :ambiguous-replacement :pid "000001"
               :target "900001" :person (model/base-person)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= #{"900001"} (get-in m' [:edges ["000101" "著者"]])))
        (is (not (contains? (:persons m') "000001")))))
    (testing "impure-split no-ops when existing-target is unattached"
      (let [m1 (:model (model/apply-event m {:event/type :add-person
                                             :pid "900050"
                                             :person (model/base-person)}))
            e {:event/type :impure-split :pid "000001"
               :existing-target "900050" :new-target "900051"
               :person (model/base-person)}
            {m2 :model intent :applied} (model/apply-event m1 e)]
        (is (nil? intent))
        (is (= m1 m2))))))
