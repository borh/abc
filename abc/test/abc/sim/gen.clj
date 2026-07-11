(ns abc.sim.gen
  "test.check generators for evolution histories. Events are generated
  concretely against the evolving state (gen/bind chain); apply-event's
  no-op totality keeps shrunk subsequences valid."
  (:require [abc.sim.model :as model]
            [clojure.test.check.generators :as gen]))

(def relations ["著者" "翻訳者" "校訂者"])

(def name-pool ["新" "改" "旧" "東" "西" "南" "北" "翠" "紅" "蒼"])

(defn- variant-person [i]
  (assoc (model/base-person)
         :family_name (nth name-pool (mod i (count name-pool)))
         :given_name (str "人" i)))

(defn find-applied [fold-result event-type]
  (first (filter #(= event-type (:intent %)) (:applied fold-result))))

;; --- single-event generators against a concrete state -----------------

(defn- gen-edit-person [m]
  (gen/let [pid (gen/elements (vec (keys (:persons m))))
            field (gen/elements [:family_name :given_name :date_of_death])
            v (gen/elements name-pool)]
    {:event/type :edit-person :pid pid :field field
     :value (if (= field :date_of_death) "1971-02-02" v)}))

(defn- gen-edit-work [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))
            v (gen/elements name-pool)]
    {:event/type :edit-work :wid wid :field :title :value (str "作品" v)}))

(defn- gen-add-work-with-edge [m]
  (let [wid (model/fresh-wid m) pid (model/fresh-pid m)]
    (gen/let [rel (gen/elements relations)]
      {:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
       :pid pid :person (variant-person (:next-id m)) :relation rel})))

(defn- gen-add-person-with-edge [m]
  (let [pid (model/fresh-pid m)]
    (gen/let [wid (gen/elements (vec (keys (:works m))))
              rel (gen/elements relations)]
      {:event/type :add-person-with-edge :pid pid
       :person (variant-person (:next-id m)) :wid wid :relation rel})))

(defn- gen-add-edge [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))
            pid (gen/elements (vec (keys (:persons m))))
            rel (gen/elements relations)]
    {:event/type :add-edge :wid wid :relation rel :pid pid}))

(defn- gen-remove-edge [m]
  (if (empty? (:edges m))
    (gen-add-work-with-edge m)
    ;; sequential bindings: pids depends on the chosen edge
    (gen/let [[k pids] (gen/elements (vec (:edges m)))
              pid (gen/elements (vec pids))]
      {:event/type :remove-edge :wid (first k) :relation (second k) :pid pid})))

(defn- gen-remove-work [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))]
    {:event/type :remove-work :wid wid}))

(defn- gen-rare-unattached [m]
  (gen/one-of
   [(gen/return {:event/type :add-person :pid (model/fresh-pid m)
                 :person (variant-person (:next-id m))})
    (gen/return (let [wid (model/fresh-wid m)]
                  {:event/type :add-work :wid wid :work (model/base-work wid)}))]))

(defn- benign-event-gen
  "Weighted benign event against state m. Removal weights are low, which
  together with the fresh-id discipline keeps the confusable?-discard rate
  low (predicate in properties is normative)."
  [m]
  (if (empty? (:works m))
    (gen-add-work-with-edge m)
    (gen/frequency
     [[4 (gen-edit-person m)]
      [3 (gen-edit-work m)]
      [3 (gen-add-work-with-edge m)]
      [3 (gen-add-person-with-edge m)]
      [3 (gen-add-edge m)]
      [1 (gen-remove-edge m)]
      [1 (gen-remove-work m)]
      [1 (gen-rare-unattached m)]])))

;; --- forced drift events ----------------------------------------------
;; Each returns [setup-events forced-event] built against state m so the
;; forced precondition holds at generation time.

(defn- sole-pids [m]
  (vec (filter #(model/sole-contributor? m %) (keys (:persons m)))))

(defn- gen-forced [m forced]
  (let [t1 (model/fresh-pid m)
        t2 (model/fresh-pid (update m :next-id inc))
        mk-persons (fn [& pids]
                     (into {} (map-indexed (fn [i p] [p (variant-person (+ 50 i))]) pids)))]
    (case forced
      :clean-split
      (let [cands (sole-pids m)]
        (if (seq cands)
          (gen/let [pid (gen/elements cands)]
            [[] {:event/type :clean-split :pid pid :targets [t1 t2]
                 :persons (mk-persons t1 t2)}])
          ;; create a fresh sole-contributor work first, then split it
          (let [wid (model/fresh-wid m) pid t1
                t1' (model/fresh-pid (update m :next-id + 2))
                t2' (model/fresh-pid (update m :next-id + 3))]
            (gen/return
             [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
                :pid pid :person (variant-person 51) :relation "著者"}]
              {:event/type :clean-split :pid pid :targets [t1' t2']
               :persons (mk-persons t1' t2')}]))))

      :clean-merge
      (let [wid (model/fresh-wid m) p1 t1 p2 t2
            tgt (model/fresh-pid (update m :next-id + 2))]
        (gen/return
         [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
            :pid p1 :person (variant-person 52) :relation "著者"}
           {:event/type :add-person-with-edge :pid p2 :person (variant-person 53)
            :wid wid :relation "著者"}]
          {:event/type :clean-merge :pids [p1 p2] :target tgt
           :person (variant-person 54)}]))

      :ambiguous-replacement
      (let [attached (vec (filter #(seq (model/edges-of m %)) (keys (:persons m))))]
        (if (seq attached)
          (gen/let [pid (gen/elements attached)]
            [[] {:event/type :ambiguous-replacement :pid pid :target t1
                 :person (variant-person 55)}])
          (let [wid (model/fresh-wid m) pid t1
                tgt (model/fresh-pid (update m :next-id + 2))]
            (gen/return
             [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
                :pid pid :person (variant-person 59) :relation "著者"}]
              {:event/type :ambiguous-replacement :pid pid :target tgt
               :person (variant-person 55)}]))))

      :impure-split
      (let [wid (model/fresh-wid m) pid t1
            newt (model/fresh-pid (update m :next-id + 2))]
        (gen/let [existing (gen/elements (vec (keys (:persons m))))]
          [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
             :pid pid :person (variant-person 56) :relation "著者"}]
           {:event/type :impure-split :pid pid :existing-target existing
            :new-target newt :person (variant-person 57)}]))

      :partial-split
      (let [wid1 (model/fresh-wid m)
            wid2 (model/fresh-wid (update m :next-id inc))
            pid (model/fresh-pid (update m :next-id + 2))
            tgt (model/fresh-pid (update m :next-id + 3))]
        (gen/return
         [[{:event/type :add-work-with-edge :wid wid1 :work (model/base-work wid1)
            :pid pid :person (variant-person 58) :relation "著者"}
           {:event/type :add-work :wid wid2 :work (model/base-work wid2)}
           {:event/type :add-edge :wid wid2 :relation "著者" :pid pid}]
          {:event/type :partial-split :pid pid :targets [tgt]
           :edge-keys [[wid2 "著者"]] :persons (mk-persons tgt)}])))))

;; --- history assembly ---------------------------------------------------

(defn- gen-events
  "Chain n benign events against the evolving state; when i = forced-at,
  splice in [setup... forced...] instead."
  [m n i forced-at forced]
  (if (zero? n)
    (gen/return [])
    (gen/bind (if (= i forced-at)
                (gen-forced m forced)
                (gen/fmap (fn [e] [[] e])
                          (benign-event-gen m)))
              (fn [[setup e]]
                (let [es (conj (vec setup) e)
                      m' (peek (:states (model/fold-history {:initial m :events es})))]
                  (gen/fmap #(into es %)
                            (gen-events m' (dec n) (inc i) forced-at forced)))))))

(defn history-gen
  [{:keys [length works forced] :or {length [5 15] works [5 20]}}]
  (gen/let [n-works (gen/choose (first works) (second works))
            n-events (gen/choose (first length) (second length))
            forced-at (if forced (gen/choose 0 (dec n-events)) (gen/return -1))]
    (let [m0 (model/bootstrap n-works)]
      (gen/fmap (fn [events] {:initial m0 :events events})
                (gen-events m0 n-events 0 forced-at forced)))))

(defn benign-history-gen [opts]
  (history-gen (assoc opts :forced nil)))
