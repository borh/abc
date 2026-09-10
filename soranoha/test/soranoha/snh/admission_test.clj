(ns soranoha.snh.admission-test
  "The partition itself, at its own boundary. The reliance tests pin what a
  reliance payload must contain; this pins which bucket a candidate lands in,
  which is what the admission report publishes and what a verifier reproduces."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.snh.admission :as admission]))

(defn- candidate [slug statuses & [reliance]]
  (cond-> {"slug" slug
           "work_assessment" {"status" (first statuses)}
           "contributions" (mapv (fn [status] {"status" status}) (rest statuses))}
    reliance (assoc "reliance" {"status" reliance})))

(defn- decide [candidate]
  (let [{:keys [admitted excluded quarantined]}
        (admission/partition-candidates admission/inclusion-rule [candidate])]
    (cond (seq admitted) [:admitted]
          (seq excluded) [:excluded (get (first excluded) "reason_code")]
          :else [:quarantined (get (first quarantined) "reason_code")])))

(deftest an-independent-candidate-is-decided-on-its-statuses
  (is (= [:admitted] (decide (candidate "a" ["public-domain" "public-domain"]))))
  (is (= [:excluded "in-copyright"]
         (decide (candidate "b" ["public-domain" "in-copyright"])))
      "one in-copyright contribution excludes the whole candidate")
  (is (= [:quarantined "not-fully-evaluated"]
         (decide (candidate "c" ["public-domain" "undetermined"])))))

(deftest a-reliance-candidate-is-decided-on-its-reliance-alone
  (is (= [:admitted] (decide (candidate "d" ["undetermined"] "relied-upon")))
      "the reliance is the decision, so the statuses beneath it do not have to be")
  (testing "a refused reliance quarantines even where a status reads in-copyright"
    ;; This is the shape that looks like a missed exclusion. The candidate is
    ;; on the reliance track, the release could not obtain the determination it
    ;; asked for, and the report says so rather than answering from the track
    ;; that was not taken. `exclude_when_any` decides the independent track.
    (is (= [:quarantined "not-fully-evaluated"]
           (decide (candidate "e" ["in-copyright"] "unavailable"))))
    (is (= [:excluded "in-copyright"]
           (decide (candidate "e" ["in-copyright"])))
        "the same statuses without a reliance record are an independent exclusion")))
