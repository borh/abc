(ns abc.sim.drift-sidecar-sim-test
  "P15: interplay between accepted drift sidecars (_events/ + _indexes/)
  and evolving upstream history, through audit!/scan-history! with
  :drift-persons-dir. Properties assert DESIRED behavior; divergences go
  to abc.sim.divergences (D7+).
  Spec: docs/superpowers/specs/2026-07-12-drift-sidecar-interplay-design.md"
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.sim.sidecar :as sidecar]
            [abc.tools.aozora-history-audit :as audit]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.properties :as prop]))

;; abc.sim.divergences is required from the start so the gate protocol is
;; executable without touching the ns form: if a window diverges, swap the
;; window's bare boolean for (div/expected-failure* :D7 <desc> (fn [] <bool>))
;; inside the property — expected-failure* is the composable non-`is` form.

(defn- successor-pids [applied]
  (case (:intent applied)
    :clean-split (vec (:targets (:event applied)))
    :clean-merge [(:target (:event applied))]))

(defn- participant-pids [event]
  (vec (distinct (map #(get % "person_id") (get event "participants")))))

(def ^:private hex-hash #"^sha256:[0-9a-f]{64}$")

(defn- entry-shape-ok?
  "ADR 0022 null-ness contract + drift_event_ids for one reported entry."
  [event entry]
  (let [prev (get entry "previous_hash")
        cur (get entry "current_hash")]
    (and (= [(get event "drift_event_id")] (get entry "drift_event_ids"))
         (case (get entry "change_type")
           "removed" (and (string? prev) (re-matches hex-hash prev)
                          (nil? cur))
           "added" (and (nil? prev)
                        (string? cur) (re-matches hex-hash cur))
           "hash_changed" (and (string? prev) (string? cur)
                               (re-matches hex-hash prev)
                               (re-matches hex-hash cur)
                               (not= prev cur))
           false))))

(defn- window-matches?
  "Exact-set agreement between the reported drift_participant_updates and
  the oracle for the window (prev-state → cur-state), plus per-entry
  shape. Exact equality doubles as the quietness assertion for untouched
  participants."
  [report event prev-state cur-state]
  (let [entries (:drift_participant_updates report)
        expected (oracle/expected-participant-updates
                  prev-state cur-state (participant-pids event))]
    (and (= expected
            (vec (sort-by #(get % "person_id")
                          (map #(select-keys % ["person_id" "change_type"])
                               entries))))
         (every? #(entry-shape-ok? event %) entries))))

(defn- lifecycle-results
  "Author the sidecar from the applied forced intent; audit the post-event
  window (s-after → s-final: pre-pid absent both sides, appended successor
  edit surfaces as hash_changed) and the spanning window (s-before →
  s-final: pre-pid removed, surviving successors added). Returns
  {:post-event bool :spanning bool} so each window can be asserted — and,
  if a divergence surfaces, gated — independently."
  [hist applied]
  (let [succ (first (successor-pids applied))
        ;; guarantee hash_changed coverage when the successor survives
        ;; attached; "変" is outside the generator name pool, and the
        ;; model's no-op totality makes the append harmless otherwise
        hist' (update hist :events conj
                      {:event/type :edit-person :pid succ
                       :field :family_name :value "変"})
        states (:states (model/fold-history hist'))
        ;; states has one entry per event (initial first): state after the
        ;; forced event = its event index + 1. The forced event map holds
        ;; freshly minted ids, so value-equality indexOf is unambiguous.
        after-idx (inc (.indexOf ^java.util.List (:events hist')
                                 (:event applied)))
        s-before (nth states (dec after-idx))
        s-after (nth states after-idx)
        s-final (peek states)
        event (sidecar/event-for-intent applied)
        drift-dir (render/temp-dir "sim-drift")]
    (try
      (sidecar/write-sidecars! drift-dir event)
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [s-before s-after s-final]
                                         (render/monotone-instants 3))
              window (fn [prev-c cur-c]
                       (audit/audit! {:aozora-repo (str root)
                                      :previous-ref (.getName prev-c)
                                      :current-ref (.getName cur-c)
                                      :work-dir (str work)
                                      :drift-persons-dir (str drift-dir)}))]
          {:post-event (window-matches? (window (nth cs 1) (nth cs 2))
                                        event s-after s-final)
           :spanning (window-matches? (window (nth cs 0) (nth cs 2))
                                      event s-before s-final)}))
      (finally (render/delete-tree! drift-dir)))))

;; P15.lifecycle — accepted event, then later participant edits (post-event
;; window) and the retrospective spanning window, for both event types.
(deftest p15-lifecycle-sim-test
  (doseq [forced [:clean-split :clean-merge]]
    (let [counter (harness/ratio-counter)]
      (harness/check!
       (str "P15.lifecycle/" (name forced)) 10
       (prop/for-all [hist (sgen/history-gen {:length [2 4] :works [3 5]
                                              :forced forced})]
                     (let [fold (model/fold-history hist)
                           applied (sgen/find-applied fold forced)]
                       (if-not (harness/tick! counter (some? applied))
                         true ;; shrunk-away forced event: vacuous
                         (let [{:keys [post-event spanning]}
                               (lifecycle-results hist applied)]
                           ;; per-window verdicts: on divergence, gate only
                           ;; the diverging one via div/expected-failure*
                           ;; (see the ns-form comment), leaving the other
                           ;; window hard
                           (and post-event spanning))))))
      (harness/assert-applied-ratio!
       (str "P15.lifecycle/" (name forced)) counter))))
