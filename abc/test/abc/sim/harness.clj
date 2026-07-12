(ns abc.sim.harness
  "Seed discipline for the simulation suite: deterministic CI seeds,
  unseeded soak mode, and forced-event application-ratio tracking.
  Spec: docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md"
  (:require [clojure.test :refer [is]]
            [clojure.test.check :as tc]))

(def ci-seeds
  "Checked-in seed corpus. Rotate entries when the soak run finds seeds
  exercising interesting histories; keep exactly this vector in CI."
  [42 4242 424242])

(def soak-factor 15)

(defn soak? []
  (= "1" (System/getenv "ABC_SIM_SOAK")))

(defn check!
  "Run `prop` with test.check. CI mode: once per seed in `ci-seeds`,
  `num-tests` cases each. Soak mode (ABC_SIM_SOAK=1): one unseeded run at
  soak-factor × num-tests. Reports the shrunk counterexample and the seed
  needed to replay it."
  [name num-tests prop]
  (if (soak?)
    (let [{:keys [pass? seed shrunk]} (tc/quick-check (* soak-factor num-tests) prop)]
      (is pass? (str name " (soak; replay with seed " seed "): "
                     (pr-str (:smallest shrunk)))))
    (doseq [seed ci-seeds]
      (let [{:keys [pass? shrunk]} (tc/quick-check num-tests prop :seed seed)]
        (is pass? (str name " (seed " seed "): "
                       (pr-str (:smallest shrunk))))))))

(defn ratio-counter []
  (atom {:applied 0 :total 0}))

(defn tick! [counter applied?]
  (swap! counter (fn [c] (-> c
                             (update :total inc)
                             (cond-> applied? (update :applied inc)))))
  applied?)

(defn assert-applied-ratio!
  "Acceptance criterion: forced events must actually apply in ≥ 90% of
  generated cases, so intent assertions are non-vacuous."
  [name counter]
  (let [{:keys [applied total]} @counter]
    (is (and (pos? total) (>= (/ applied total) 9/10))
        (str name ": forced-event applied ratio " applied "/" total))))

(defn clean-ex-info?
  "Clean two-tier failure: ex-info carrying every required diagnostic key."
  [e required-keys]
  (and (instance? clojure.lang.ExceptionInfo e)
       (every? #(contains? (ex-data e) %) required-keys)))

(defn forbidden-throw?
  "True when the SUT escaped with an exception class the failure taxonomy
  forbids outright (spec §Failure Taxonomy) — asserted even for
  divergence-gated cases so a wrong-behavior regression cannot hide
  behind an open divergence."
  [e]
  (or (instance? NullPointerException e)
      (instance? AssertionError e)
      (instance? StackOverflowError e)
      (instance? java.util.zip.ZipException e)))
