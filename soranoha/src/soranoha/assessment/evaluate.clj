(ns soranoha.assessment.evaluate
  "Resolve current support before exposing cached values. Dynamic member edges
  belong to this evaluated view; the trace store is disposable build state."
  (:require [charred.api :as json]
            [soranoha.kura.cas :as cas]
            [clojure.string :as str]
            [soranoha.assessment.records :as records]
            [soranoha.assessment.aozora :as aozora]
            [soranoha.core.canonical :as canonical]
            [soranoha.kura.engine :as engine]))

(defn state->wire [state]
  (case state
    :assessment/available "available"
    :assessment/unavailable "unavailable"
    (throw (ex-info "Invalid assessment state" {:reason :invalid-assessment-view :value state}))))

(defn reason->wire [reason]
  (case reason
    nil nil
    (:assessment/missing-selected-work
     :assessment/stale-premise
     :assessment/revoked-support
     :assessment/unavailable-term-premise
     :assessment/death-year-outside-conservative-rule
     :assessment/attribution-not-death-based
     :assessment/wartime-addition-not-discharged
     :assessment/work-type-outside-conservative-rule
     :assessment/publication-timing-unestablished
     :assessment/underlying-work-not-public-domain
     :assessment/before-term-rule-effective-date
     :assessment/unavailable-contribution-assessment
     :assessment/unavailable-contribution-completeness
     :assessment/absent-assessment) (name reason)
    (throw (ex-info "Invalid assessment reason" {:reason :invalid-assessment-view :value reason}))))

(defn- wire->reason [reason]
  (case reason
    ("missing-selected-work"
     "stale-premise"
     "revoked-support"
     "unavailable-term-premise"
     "death-year-outside-conservative-rule"
     "attribution-not-death-based"
     "wartime-addition-not-discharged"
     "work-type-outside-conservative-rule"
     "publication-timing-unestablished"
     "underlying-work-not-public-domain"
     "before-term-rule-effective-date"
     "unavailable-contribution-assessment"
     "unavailable-contribution-completeness"
     "absent-assessment") (keyword "assessment" reason)
    (throw (ex-info "Invalid stored assessment reason" {:reason :invalid-assessment-view :value reason}))))

(def missing-selected-work
  {"state" "unavailable" "reason" "missing-selected-work"})

(def ^:private rule-version "1")
(def ^:private assessment-fact-version "1")
(def ^:private reliance-version "1")

(def ^:private rule-date "2018-12-29")

(defn semantic-value
  "The supported value and its effective date, without its supporting evidence."
  [result]
  (case (state->wire (:state result))
    "available" {"state" "available" "value" (:value result)
                 "effective_date" (:effective-date result)}
    "unavailable" {"state" "unavailable"}))

(defn premise-fingerprint
  "Fingerprint a fact projection for an authored premise. Observations use
  records/fingerprint directly on their captured value."
  [result projection]
  (records/fingerprint
   (case projection
     "evidence-version" {"semantic" (semantic-value result) "basis" (:basis result)}
     "set-membership" (dissoc (semantic-value result) "effective_date")
     (semantic-value result))))

(defn identity-fingerprint [identity observations]
  (records/fingerprint
   {"identity" identity
    "evidence" (into (sorted-map)
                     (map (fn [id] [id (records/fingerprint (get observations id))]))
                     (get identity "evidence"))}))

(defn- unavailable [reason dependencies]
  {:state :assessment/unavailable :reason reason :dependencies (vec dependencies) :basis []})

(defn- latest-date [results]
  (last (sort (keep :effective-date results))))

(defn- available [value date basis dependencies]
  {:state :assessment/available :value value :effective-date date
   :basis (vec (sort-by records/fingerprint (distinct basis))) :dependencies (vec dependencies)})

(defn- stage-result! [store toolchain-id key result stages]
  (let [semantic (semantic-value result)
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

(defn- run-rule! [store toolchain-id key inputs compute stages]
  (let [run (engine/run-stage!
             store
             {:stage-id (str "assessment-rule/" (get key "predicate"))
              :stage-version rule-version :toolchain-id toolchain-id
              :f (fn [_ _]
                   {"semantic" (canonical/rfc8785-safe-integer-json-bytes-v1 (compute))})}
             (assoc inputs "fact" key))
        value (json/read-json
               (String. ^bytes (cas/get-bytes (:cas-dir store)
                                              (get-in run [:outputs "semantic"])) "UTF-8"))]
    (swap! stages conj (assoc run :fact key :rule? true))
    value))

(defn- contribution-parts [subject]
  (str/split subject #"/" 2))

(defn- validate-view! [source observations as-of]
  (records/date! as-of)
  (doseq [finding (get source "findings")]
    (when (pos? (compare (get finding "effective_date") as-of))
      (records/fail! :future-assessment-finding {:finding (get finding "id")})))
  (doseq [observation (get source "observations")]
    (let [id (get observation "id") value (get observations id)]
      (when-not (and (contains? observations id)
                     (or (and (not= "retained-evidence" (get observation "selector"))
                              (= missing-selected-work value))
                         (case (get observation "selector")
                           "catalog-contributors" (and (vector? value) (seq value)
                                                       (every? #(and (string? %)
                                                                     (re-matches #"(author|translator|editor|reviser|other):[0-9]{6}" %)) value)
                                                       (= value (vec (sort (distinct value)))))
                           (and (string? value) (re-matches #"sha256:[0-9a-f]{64}" value)))))
        (records/fail! :invalid-captured-observation {:id id}))))
  (let [ids (set (map #(get % "id") (get source "identities")))
        by-observation (into {} (map (juxt #(get % "id") identity))
                             (get source "observations"))]
    (doseq [finding (get source "findings")
            :when (= "contribution-set" (get-in finding ["fact" "predicate"]))]
      (let [slug (get-in finding ["fact" "subject"])
            captures (set (for [p (get finding "premises")
                                :when (= "observation" (get p "kind"))
                                :let [o (get by-observation (get p "ref"))]
                                :when (= slug (get o "slug"))]
                            (get o "selector")))]
        (when-not (every? captures ["canonical-source-bundle" "catalog-contributors"])
          (records/fail! :incomplete-contribution-set-premises {:finding (get finding "id")}))
        (doseq [cid (get finding "value")
                :let [coordinate (second (str/split cid #":" 2))]
                :when (str/starts-with? coordinate "soranoha-")]
          (when-not (ids coordinate)
            (records/fail! :missing-minted-person {:contribution-id cid}))
          (when-not (some #(and (= "identity" (get % "kind"))
                                (= coordinate (get % "ref")))
                          (get finding "premises"))
            (records/fail! :missing-minted-person-premise {:contribution-id cid})))))))

(defn- validate-acyclic! [source candidates]
  (let [by-fact (group-by #(get % "fact") (get source "findings"))
        done (atom #{}) active (atom #{})
        key-for records/fact-key]
    (letfn [(edges [key]
              (let [subject (get key "subject")
                    authored (mapcat #(get % "premises") (get by-fact key))
                    explicit (keep #(when (= "fact" (get % "kind")) (get % "ref")) authored)
                    implicit
                    (case (get key "predicate")
                      "work-status"
                      (cons (key-for subject "contribution-set")
                            (for [finding (get by-fact (key-for subject "contribution-set"))
                                  cid (get finding "value")]
                              (key-for (records/contribution-subject subject cid) "contribution-status")))
                      "contribution-status"
                      (let [[slug cid] (contribution-parts subject)
                            coordinate (second (str/split (or cid "") #":" 2))]
                        (concat [(key-for (str "person:" coordinate) "death-year")
                                 (key-for subject "attribution-form")
                                 (key-for subject "no-wartime-addition")
                                 (key-for slug "work-type")
                                 (key-for slug "publication-timing")
                                 (key-for slug "derivative-chain")]
                                (for [finding (get by-fact (key-for slug "derivative-chain"))
                                      underlying (get finding "value")]
                                  (key-for underlying "work-status"))))
                      [])]
                (concat explicit implicit)))
            (visit [key]
              (when (@active key) (records/fail! :assessment-cycle {:fact key}))
              (when-not (@done key)
                (swap! active conj key)
                (doseq [next-key (edges key)] (visit next-key))
                (swap! active disj key)
                (swap! done conj key)))]
      (doseq [key (concat (keys by-fact)
                          (map #(key-for % "work-status") (keys candidates)))]
        (visit key)))))

(defn- restrictive-facts-by-work [facts]
  (reduce-kv
   (fn [index fact result]
     (let [predicate (get fact "predicate")
           subject (get fact "subject")]
       (if (and (#{"work-status" "contribution-status"} predicate)
                (= :assessment/available (:state result))
                (#{"in-copyright" "undetermined"} (:value result)))
         (update index (if (= predicate "work-status") subject (first (str/split subject #"/" 2)))
                 (fnil conj []) {"fact" fact "semantic" (semantic-value result)})
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
  (validate-view! source observations as-of)
  (doseq [reliance (get source "reliances")]
    (when (pos? (compare (get reliance "decision_date") as-of))
      (records/fail! :future-reliance-decision {:slug (get reliance "slug")})))
  (validate-acyclic! source candidates)
  (let [findings (get source "findings")
        by-fact (group-by #(get % "fact") findings)
        revoked (set (map #(get % "target") (get source "controls")))
        states (atom {}) facts (atom {}) visiting (atom #{}) stages (atom [])
        obs (into {} (map (fn [[id value]]
                            [id (if (= missing-selected-work value)
                                  {:state :assessment/unavailable :reason :assessment/missing-selected-work}
                                  {:state :assessment/available :value value :version (records/fingerprint value)})])) observations)]
    (letfn [(resolve-premise [premise]
              (let [kind (get premise "kind") ref (get premise "ref")
                    projection (get premise "projection" "evidence-version")
                    result (when (= kind "fact") (resolve-fact ref))
                    identity (when (= kind "identity")
                               (first (filter #(= ref (get % "id")) (get source "identities"))))
                    actual (case kind
                             "identity" (when (every? #(= :assessment/available (get-in obs [% :state]))
                                                      (get identity "evidence"))
                                          (identity-fingerprint identity observations))
                             "observation"
                             (do
                               (when (and (= :assessment/available (get-in obs [ref :state]))
                                          (= projection "set-membership")
                                          (not (vector? (get-in obs [ref :value]))))
                                 (records/fail! :non-set-membership-premise {:premise premise}))
                               (get-in obs [ref :version]))
                             (when (= :assessment/available (:state result))
                               (when (and (= projection "set-membership")
                                          (not (vector? (:value result))))
                                 (records/fail! :non-set-membership-premise {:premise premise}))
                               (premise-fingerprint result projection)))
                    matches? (= actual (get premise "fingerprint"))]
                {:premise premise :state (if matches? :assessment/available :assessment/unavailable)
                 :actual actual :reason (when-not matches?
                                          (or (:reason result)
                                              (if identity
                                                (some #(get-in obs [% :reason]) (get identity "evidence"))
                                                (get-in obs [ref :reason]))
                                              :assessment/stale-premise))
                 :dependencies (vec (:dependencies result))}))
            (resolve-finding [finding]
              (let [id (get finding "id")
                    edges (mapv resolve-premise (get finding "premises"))
                    bad (filter #(= :assessment/unavailable (:state %)) edges)
                    result (cond
                             (revoked id) (unavailable :assessment/revoked-support edges)
                             (seq bad) (unavailable :assessment/stale-premise edges)
                             :else (available (get finding "value")
                                              (get finding "effective_date")
                                              [{"id" id "version" (records/fingerprint finding)
                                                "text" (get finding "basis")}]
                                              edges))]
                (swap! states assoc id (assoc result :record finding))
                result))
            (dependency [key result]
              {:fact key :state (:state result) :reason (:reason result)
               :semantic (semantic-value result)
               :dependencies (:dependencies result)})
            (derive-contribution [key]
              (let [[slug cid] (contribution-parts (get key "subject"))
                    coordinate (second (str/split (or cid "") #":" 2))
                    keys [(records/fact-key (str "person:" coordinate) "death-year")
                          (records/fact-key (get key "subject") "attribution-form")
                          (records/fact-key (get key "subject") "no-wartime-addition")
                          (records/fact-key slug "work-type")
                          (records/fact-key slug "publication-timing")
                          (records/fact-key slug "derivative-chain")]
                    results (mapv resolve-fact keys)
                    edges (mapv dependency keys results)
                    [death attribution wartime type timing chain] (map :value results)
                    chain-keys (mapv #(records/fact-key % "work-status") (or chain []))
                    chain-results (mapv resolve-fact chain-keys)
                    all-results (into results chain-results)
                    all-edges (into edges (mapv dependency chain-keys chain-results))
                    failure-reason (fn []
                                     (cond
                                       (some #(not= :assessment/available (:state %)) all-results) :assessment/unavailable-term-premise
                                       (or (not (integer? death)) (> death 1967)) :assessment/death-year-outside-conservative-rule
                                       (not (#{"real-name" "well-known-pseudonym" "registered-real-name"} attribution)) :assessment/attribution-not-death-based
                                       (not (true? wartime)) :assessment/wartime-addition-not-discharged
                                       (not= "non-film-non-photo" type) :assessment/work-type-outside-conservative-rule
                                       (not (#{"lifetime" "posthumous"} timing)) :assessment/publication-timing-unestablished
                                       (some #(not= "public-domain" (:value %)) chain-results) :assessment/underlying-work-not-public-domain
                                       (neg? (compare as-of rule-date)) :assessment/before-term-rule-effective-date))
                    semantic (run-rule!
                              store toolchain-id key
                              {"premises" (mapv semantic-value all-results)
                               "rule-applicable" (not (neg? (compare as-of rule-date)))}
                              #(if-let [reason (failure-reason)]
                                 {"state" "unavailable" "reason" (reason->wire reason)}
                                 {"state" "available" "value" "public-domain"
                                  "effective_date" (last (sort [rule-date (latest-date all-results)]))}) stages)]
                (if (= "available" (get semantic "state"))
                  (available (get semantic "value")
                             (get semantic "effective_date")
                             (concat [{"id" (str "jp-conservative-term/" rule-version)
                                       "text" (str "jp-conservative-term/" rule-version
                                                   ": Japanese death-based term: death by 1967; established attribution, work type, publication timing, rights chain and absence of wartime addition; status on or after 2018-12-29.")}]
                                     (mapcat :basis all-results))
                             all-edges)
                  (unavailable (wire->reason (get semantic "reason")) all-edges))))
            (derive-work [key]
              (let [slug (get key "subject")
                    set-key (records/fact-key slug "contribution-set")
                    members (resolve-fact set-key)]
                (if (= :assessment/available (:state members))
                  (let [keys (mapv #(records/fact-key (records/contribution-subject slug %)
                                                      "contribution-status") (:value members))
                        results (mapv resolve-fact keys)
                        edges (into [(dependency set-key members)] (mapv dependency keys results))]
                    (if (every? #(and (= :assessment/available (:state %))
                                      (= "public-domain" (:value %))) results)
                      (available "public-domain" (latest-date (cons members results))
                                 (mapcat :basis (cons members results)) edges)
                      (unavailable :assessment/unavailable-contribution-assessment edges)))
                  (unavailable :assessment/unavailable-contribution-completeness
                               [(dependency set-key members)]))))
            (resolve-fact [key]
              (or (get @facts key)
                  (do
                    (when (@visiting key)
                      (records/fail! :assessment-cycle {:fact key}))
                    (swap! visiting conj key)
                    (let [reviewed (mapv resolve-finding (get by-fact key))
                          predicate (get key "predicate")
                          derived (case predicate
                                    "contribution-status" (derive-contribution key)
                                    "work-status" (derive-work key)
                                    nil)
                          supports (filter #(= :assessment/available (:state %))
                                           (cond-> reviewed derived (conj derived)))
                          values (set (map :value supports))
                          _ (when (> (count values) 1)
                              (records/fail! :conflicting-assessment-support
                                             {:fact key :justifications (vec (mapcat :basis supports))}))
                          result (if (seq supports)
                                   (available (:value (first supports)) (latest-date supports)
                                              (distinct (mapcat :basis supports))
                                              (mapcat :dependencies supports))
                                   (unavailable (or (:reason (first reviewed))
                                                    (:reason derived) :assessment/absent-assessment)
                                                (mapcat :dependencies
                                                        (cond-> reviewed derived (conj derived)))))
                          result (if (and (= predicate "work-status")
                                          (not (#{"in-copyright" "undetermined"} (:value result)))
                                          (not= :assessment/available
                                                (:state (resolve-fact
                                                         (records/fact-key (get key "subject")
                                                                           "contribution-set")))))
                                   (unavailable :assessment/unavailable-contribution-completeness
                                                (:dependencies derived)) result)
                          result (stage-result! store toolchain-id key result stages)]
                      (swap! visiting disj key)
                      (swap! facts assoc key result)
                      result))))]
      ;; Evaluate every authored key, including findings outside the selected
      ;; publication population, so inconsistent source authority cannot hide.
      (doseq [key (sort-by records/fingerprint (keys by-fact))] (resolve-fact key))
      (doseq [[slug _] (sort-by key candidates)]
        (resolve-fact (records/fact-key slug "work-status")))
      (let [reliances (evaluate-reliances! store source candidates reliance-observations
                                           @facts toolchain-id stages)]
        {:source source :observations obs :facts @facts :candidates candidates
         :findings (mapv #(get @states (get % "id")) findings)
         :reliances reliances :stages @stages}))))
