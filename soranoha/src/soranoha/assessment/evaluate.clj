(ns soranoha.assessment.evaluate
  "Stage a resolved assessment graph into the content-addressed store.

  soranoha.assessment.graph decides what every fact is; this records those
  decisions as engine runs, so a release can name the run that produced each
  one, and evaluates the owner's reliance declarations against the facts the
  graph resolved. The split is what lets the evaluation be read and tested
  without a store: everything here needs one, and nothing there does."
  (:require [charred.api :as json]
            [soranoha.kura.cas :as cas]
            [soranoha.assessment.graph :as graph]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.core.canonical :as canonical]
            [soranoha.kura.engine :as engine]))

(def ^:private assessment-fact-version "1")
(def ^:private reliance-version "1")

(defn- stage-result! [store toolchain-id key result stages]
  (let [semantic (graph/semantic-value result)
        basis (:basis result)
        run (engine/run-stage!
             store
             {:stage-id "assessment-fact" :stage-version assessment-fact-version
              :toolchain-id toolchain-id
              :f (fn [_ inputs]
                   {"semantic" (canonical/rfc8785-safe-integer-json-bytes-v1
                                (get inputs "semantic"))
                    "basis" (canonical/rfc8785-safe-integer-json-bytes-v1
                             (get inputs "basis"))})}
             {"fact" key "semantic" semantic "basis" basis})]
    (swap! stages conj (assoc run :fact key))
    (assoc result :semantic-id (get-in run [:outputs "semantic"])
           :basis-id (get-in run [:outputs "basis"]))))

(defn- store-rule
  "The graph's `:rule`, run through the engine so the derived term value is
  content-addressed and its run is on the record.

  The value handed back is read from the store rather than returned from
  `compute`, which is what graph/pure-rule reproduces without one."
  [store toolchain-id stages]
  (fn [key inputs compute]
    (let [run (engine/run-stage!
               store
               {:stage-id (str "assessment-rule/" (get key "predicate"))
                :stage-version graph/rule-version :toolchain-id toolchain-id
                :f (fn [_ _]
                     {"semantic" (canonical/rfc8785-safe-integer-json-bytes-v1 (compute))})}
               (assoc inputs "fact" key))
          value (json/read-json
                 (String. ^bytes (cas/get-bytes (:cas-dir store)
                                                (get-in run [:outputs "semantic"])) "UTF-8"))]
      (swap! stages conj (assoc run :fact key :rule? true))
      value)))

(defn- restrictive-facts-by-work [facts]
  (reduce-kv
   (fn [index fact result]
     (let [predicate (get fact "predicate")
           subject (get fact "subject")]
       (if (and (#{"work-status" "contribution-status"} predicate)
                (= :assessment/available (:state result))
                (#{"in-copyright" "undetermined"} (:value result)))
         (update index (if (= predicate "work-status") subject (first (graph/contribution-parts subject)))
                 (fnil conj []) {"fact" fact "semantic" (graph/semantic-value result)})
         index)))
   {} facts))

(defn- reliance-payload [inputs]
  (let [record (get inputs "record")
        current (get inputs "current")
        reason (cond
                 (not (get inputs "selected")) "missing-selected-work"
                 (some? (get record "exception")) "recorded-exception"
                 (seq (get inputs "restrictions")) "restrictive-independent-assessment"
                 (not= "available" (get current "state")) (get current "reason"))]
    (assoc (dissoc record "slug")
           "issuer" "aozora-bunko" "jurisdiction" "jp"
           "classification" "copyright-expired"
           "status" (if reason "unavailable" "relied-upon") "reason" reason)))

(defn- evaluate-reliances! [store source candidates observations facts toolchain-id stages]
  (let [restrictions (restrictive-facts-by-work facts)]
    (into {}
          (map
           (fn [record]
             (let [slug (get record "slug")
                   observed (get observations slug)
                   current (if observed
                             (aozora/observation->wire observed)
                             {"state" "unavailable" "reason" "missing-reliance-observation"})
                   run (engine/run-stage!
                        store
                        {:stage-id "assessment-reliance" :stage-version reliance-version
                         :toolchain-id toolchain-id
                         :f (fn [_ inputs]
                              {"reliance" (canonical/rfc8785-safe-integer-json-bytes-v1
                                           (reliance-payload inputs))})}
                        {"record" record "current" current
                         "selected" (contains? candidates slug)
                         "restrictions" (vec (sort-by records/fingerprint (get restrictions slug)))})
                   payload (json/read-json
                            (String. ^bytes (cas/get-bytes (:cas-dir store)
                                                           (get-in run [:outputs "reliance"])) "UTF-8"))]
               (swap! stages conj (assoc run :reliance-slug slug))
               [slug payload])))
          (get source "reliances"))))

(defn evaluate!
  "Evaluate validated owner records against freshly captured observations.
  Candidates map slugs to provisional catalog contribution ids; as-of validates
  applicability and never stamps unchanged facts."
  [store source {:keys [observations candidates as-of toolchain-id reliance-observations]}]
  (records/validate! source)
  (graph/validate-view! source observations as-of)
  (doseq [reliance (get source "reliances")]
    (when (pos? (compare (get reliance "decision_date") as-of))
      (records/fail! :future-reliance-decision {:slug (get reliance "slug")})))
  (graph/validate-acyclic! source candidates)
  (let [stages (atom [])
        resolved (graph/resolve! source {:observations observations :candidates candidates
                                         :as-of as-of
                                         :rule (store-rule store toolchain-id stages)})
        ;; Staged in resolution order, so the recorded runs read in the order
        ;; the evaluation established the facts rather than in map order.
        facts (reduce (fn [staged key]
                        (assoc staged key
                               (stage-result! store toolchain-id key
                                              (get-in resolved [:facts key]) stages)))
                      {} (:order resolved))
        reliances (evaluate-reliances! store source candidates reliance-observations
                                       facts toolchain-id stages)]
    {:source source :observations (:observations resolved) :facts facts :candidates candidates
     :findings (mapv #(get-in resolved [:findings (get % "id")]) (get source "findings"))
     :reliances reliances :stages @stages}))
