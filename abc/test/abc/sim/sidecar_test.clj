(ns abc.sim.sidecar-test
  "Authoring contract for simulation drift sidecars: authored sidecars
  must pass validate-drift-events! cleanly; each fault injector must
  produce exactly its documented person-drift failure code."
  (:require [abc.sim.model :as model]
            [abc.sim.render :as render]
            [abc.sim.sidecar :as sidecar]
            [abc.tools.person-drift :as drift]
            [clojure.test :refer [deftest is]]))

(def split-intent
  {:intent :clean-split
   :event {:event/type :clean-split :pid "000001"
           :targets ["900001" "900002"]
           :persons {"900001" (model/base-person)
                     "900002" (model/base-person)}}})

(def merge-intent
  {:intent :clean-merge
   :event {:event/type :clean-merge :pids ["900003" "900004"]
           :target "900005" :person (model/base-person)}})

(deftest authored-sidecars-validate-clean-test
  (doseq [intent [split-intent merge-intent]]
    (let [dir (render/temp-dir "sim-sidecar")]
      (try
        (let [event (sidecar/write-sidecars!
                     dir (sidecar/event-for-intent intent))
              result (drift/validate-drift-events! {:persons-dir (str dir)})]
          (is (= :ok (:status result))
              (str (:intent intent) ": " (pr-str result)))
          (is (= 1 (:events result)))
          (is (= (count (distinct (map #(get % "person_id")
                                       (get event "participants"))))
                 (:indexes result))))
        (finally (render/delete-tree! dir))))))

(deftest corrupted-sidecars-fail-validation-test
  (doseq [fault [:schema-hash-mismatch :orphan-event-file
                 :index-target-missing :participants-not-sorted]]
    (let [dir (render/temp-dir "sim-sidecar")]
      (try
        (let [event (sidecar/write-sidecars!
                     dir (sidecar/event-for-intent split-intent))]
          (sidecar/corrupt! dir event fault)
          (let [result (drift/validate-drift-events! {:persons-dir (str dir)})]
            (is (= :error (:status result)) (str fault))
            (is (some #(= fault (:code %)) (:failures result))
                (str fault ": " (pr-str (:failures result))))))
        (finally (render/delete-tree! dir))))))
