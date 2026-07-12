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

(def ^:private synthetic-intent
  "Split whose pre- participant is bootstrap author 000001 and whose post-
  participants are never-ingested fresh pids: only 000001 can ever produce
  an entry; 9999xx pids are absent from every projection."
  {:intent :clean-split
   :event {:pid "000001" :targets ["999901" "999902"]}})

(defmacro ^:private with-sidecar
  "Author + write the synthetic sidecar; bind [event-sym dir-sym]."
  [[event-sym dir-sym] & body]
  `(let [~event-sym (sidecar/event-for-intent synthetic-intent)
         ~dir-sym (render/temp-dir "sim-drift")]
     (try
       (sidecar/write-sidecars! ~dir-sym ~event-sym)
       ~@body
       (finally (render/delete-tree! ~dir-sym)))))

;; P15.localization — the update appears only in the pair whose window
;; spans the participant edit; summary equals the per-pair sum.
(deftest p15-localization-sim-test
  (let [m0 (model/bootstrap 2)
        edit (fn [m pid v]
               (:model (model/apply-event m {:event/type :edit-person
                                             :pid pid :field :family_name
                                             :value v})))
        s1 (edit m0 "000002" "改") ;; pair 1: non-participant edit only
        s2 (edit s1 "000001" "変")] ;; pair 2: participant edit
    (with-sidecar [event drift-dir]
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 s1 s2]
                                         (render/monotone-instants 3))
              s (audit/scan-history! {:aozora-repo (str root)
                                      :from-ref (.getName (first cs))
                                      :work-dir (str work)
                                      :drift-persons-dir (str drift-dir)})
              pairs (:pairs s)]
          (is (= "ok" (:status s)))
          (is (= [0 1] (mapv :drift_participant_update_count pairs)))
          (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
                 (mapv #(select-keys % ["person_id" "change_type"])
                       (:drift_participant_updates (second pairs)))))
          (is (every? #(entry-shape-ok? event %)
                      (:drift_participant_updates (second pairs))))
          (is (= 1 (get (:summary s) "drift_participant_updates"))))))))

;; P15.invalid-sidecar — all four faults pin the shared drift-index-map
;; validation boundary through audit!; :schema-hash-mismatch additionally
;; pins scan-history! propagation (per-pair re-validation must be equally
;; loud, not absorbed into a pair entry).
(deftest p15-invalid-sidecar-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person
                                          :pid "000001"
                                          :field :family_name :value "改"}))]
    (doseq [fault [:schema-hash-mismatch :orphan-event-file
                   :index-target-missing :participants-not-sorted]]
      (with-sidecar [event drift-dir]
        (sidecar/corrupt! drift-dir event fault)
        (render/with-repo [git root work]
          (let [cs (render/commit-history! git root [m0 m1]
                                           (render/monotone-instants 2))
                thrown (try (audit/audit! {:aozora-repo (str root)
                                           :previous-ref (.getName (first cs))
                                           :current-ref (.getName (second cs))
                                           :work-dir (str work)
                                           :drift-persons-dir (str drift-dir)})
                            nil
                            (catch Throwable e e))]
            (is (some? thrown)
                (str fault ": invalid sidecars must not yield a report"))
            (is (not (harness/forbidden-throw? thrown)) (str fault))
            (is (harness/clean-ex-info? thrown [:persons-dir :failures])
                (str fault ": " (pr-str thrown)))))))
    (with-sidecar [event drift-dir]
      (sidecar/corrupt! drift-dir event :schema-hash-mismatch)
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 m1]
                                         (render/monotone-instants 2))
              thrown (try (audit/scan-history!
                           {:aozora-repo (str root)
                            :from-ref (.getName (first cs))
                            :work-dir (str work)
                            :drift-persons-dir (str drift-dir)})
                          nil
                          (catch Throwable e e))]
          (is (some? thrown) "scan-history! must propagate, not absorb")
          (is (not (harness/forbidden-throw? thrown)))
          (is (harness/clean-ex-info? thrown [:persons-dir :failures])))))))

;; P15.rerun — P14's work-dir hygiene contract extended to the sidecar
;; path: drift_participant_updates carry no locator fields, so
;; semantic-report retains them and equality is meaningful.
(deftest p15-rerun-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person
                                          :pid "000001"
                                          :field :family_name :value "改"}))]
    (with-sidecar [event drift-dir]
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 m1]
                                         (render/monotone-instants 2))
              run! (fn [w]
                     (oracle/semantic-report
                      (audit/audit! {:aozora-repo (str root)
                                     :previous-ref (.getName (first cs))
                                     :current-ref (.getName (second cs))
                                     :work-dir (str w)
                                     :drift-persons-dir (str drift-dir)})))
              first-run (run! work)
              reused (run! work)
              fresh-dir (render/temp-dir "sim-fresh-work")
              fresh (try (run! fresh-dir)
                         (finally (render/delete-tree! fresh-dir)))]
          (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
                 (mapv #(select-keys % ["person_id" "change_type"])
                       (:drift_participant_updates first-run)))
              "sidecar entry present in the semantic report")
          (is (= first-run reused fresh)))))))
