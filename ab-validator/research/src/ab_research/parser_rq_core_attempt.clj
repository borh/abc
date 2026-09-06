(ns ab-research.parser-rq-core-attempt
  "Authenticate core-attempt captures and derive predicates 1, 7, and 9."
  (:require [ab-research.hash :as hash]
            [charred.api :as charred]
            [ab-research.parser-rq-capture :as capture]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def ^:private elapsed-pattern #"(?:0|[1-9][0-9]*)(?:\.[0-9]+)?\n?")
(def ^:private measured-dispositions
  #{"parsed" "fatal_error" "adapter_timeout"})

(defn- keywordize [value]
  (walk/keywordize-keys value))

(defn- canonical-json-value [value]
  (walk/postwalk
   (fn [node]
     (cond
       (map? node) (into {} (map (fn [[k v]]
                                   [(if (keyword? k) (name k) k) v])) node)
       (keyword? node) (name node)
       :else node))
   value))

(defn policy-hash
  "Compute the JCS identity of a core-attempt policy without its self-reference."
  [policy]
  (-> policy
      canonical-json-value
      hash/sha256-json-jcs
      hash/format-sha256))

(defn- unavailable [reason]
  {:status :unavailable :reason reason})

(defn- parse-elapsed [bytes]
  (let [text (String. ^bytes bytes "UTF-8")]
    (when-not (re-matches elapsed-pattern text)
      (throw (ex-info "elapsed record is not one plain non-negative decimal line"
                      {:value text})))
    (let [value (Double/parseDouble (string/trim-newline text))]
      (when-not (Double/isFinite value)
        (throw (ex-info "elapsed record is not finite" {:value text})))
      value)))

(defn- authenticated-bytes [blob-reader blob label]
  (let [result (blob-reader blob)]
    (if (and (= :ok (:status result)) (bytes? (:bytes result)))
      (:bytes result)
      (throw (ex-info (str label " is unavailable")
                      {:label label :result result})))))

(defn- authenticated-json [blob-reader blob label]
  (-> (authenticated-bytes blob-reader blob label)
      (String. "UTF-8")
      charred/read-json
      keywordize))

(defn- duplicate-values [values]
  (->> (frequencies values)
       (keep (fn [[value n]] (when (> n 1) value)))
       set))

(defn- index-errors [policy candidate index]
  (let [expected-work-ids (:expected_work_ids policy)
        expected-source-work-ids (mapv :work_id (:expected_sources policy))
        expected-pairs (set (for [repetition (range 1 4)
                                  work-id expected-work-ids]
                              [repetition work-id]))
        record-pairs (mapv (juxt :repetition :work_id) (:records index))
        attempt-repetitions (mapv :repetition (:attempts index))]
    (cond-> []
      (not= 3 (:repetitions policy))
      (conj "policy does not require exactly three repetitions")

      (not= "maximum" (:reduction policy))
      (conj "policy does not require maximum reduction")

      (not= (:policy_hash policy)
            (policy-hash (dissoc policy :policy_hash)))
      (conj "policy hash does not authenticate its reviewed values")

      (not= (set expected-work-ids) (set expected-source-work-ids))
      (conj "policy source membership differs from expected work membership")

      (seq (duplicate-values expected-source-work-ids))
      (conj "policy contains duplicate source work IDs")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref index))
      (conj "index qualification identity differs from candidate")

      (and (:qualification_identity_ref policy)
           (not= (:qualification_identity_ref candidate)
                 (:qualification_identity_ref policy)))
      (conj "policy qualification identity differs from candidate")

      (not= (:candidate_ref candidate) (:candidate_ref index))
      (conj "index candidate reference differs from candidate")

      (not= (:policy_hash policy) (:policy_hash index))
      (conj "index policy identity differs from policy")

      (not= expected-work-ids (:expected_work_ids index))
      (conj "index work membership differs from policy")

      (not= 3 (:repetitions index))
      (conj "index repetition count differs from policy")

      (not= "maximum" (:reduction index))
      (conj "index reduction differs from policy")

      (not= #{1 2 3} (set attempt-repetitions))
      (conj "attempt membership is not exactly repetitions 1, 2, and 3")

      (seq (duplicate-values attempt-repetitions))
      (conj "attempt index contains duplicate repetitions")

      (not= expected-pairs (set record-pairs))
      (conj "work record membership is not the closed work-by-repetition product")

      (seq (duplicate-values record-pairs))
      (conj "work record index contains duplicate work-by-repetition pairs"))))

(defn- record-errors [policy candidate descriptor record]
  (let [source-hashes (into {} (map (juxt :work_id :source_sha256)
                                    (:expected_sources policy)))
        status (:status record)]
    (cond-> []
      (not= (:work_id descriptor) (:work_id record))
      (conj "work record identity differs from its index descriptor")

      (not= (:repetition descriptor) (:repetition record))
      (conj "work record repetition differs from its index descriptor")

      (not= (get source-hashes (:work_id record)) (:source_sha256 record))
      (conj "work record source hash differs from policy")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref record))
      (conj "work record qualification identity differs from candidate")

      (not= (:candidate_ref candidate) (:candidate_ref record))
      (conj "work record candidate reference differs from candidate")

      (not= (:policy_hash policy) (:policy_hash record))
      (conj "work record policy identity differs from policy")

      (not= "measured" status)
      (conj (str "work record is not measured: " status))

      (and (= "measured" status)
           (not (contains? measured-dispositions (:disposition record))))
      (conj (str "work record has unknown disposition: " (:disposition record))))))

(defn authenticate-index
  "Authenticate a closed core index and every referenced timing/report/record blob.

  `blob-reader` receives one logical blob reference and must return the exact
  authenticated bytes as `{:status :ok :bytes byte-array}`."
  [policy candidate index blob-reader]
  (try
    (let [policy (keywordize policy)
          candidate (keywordize candidate)
          index (keywordize index)
          errors (index-errors policy candidate index)]
      (if (seq errors)
        (unavailable (string/join "; " errors))
        (let [attempts
              (mapv (fn [attempt]
                      (when-not (zero? (:exit_status attempt))
                        (throw (ex-info "core attempt command failed" attempt)))
                      (when-not (true? (:lock_retained attempt))
                        (throw (ex-info "core attempt lock was not retained" attempt)))
                      {:repetition (:repetition attempt)
                       :elapsed_seconds
                       (parse-elapsed
                        (authenticated-bytes blob-reader
                                             (:elapsed_record attempt)
                                             "elapsed record"))})
                    (:attempts index))
              records
              (mapv (fn [descriptor]
                      (let [record (authenticated-json blob-reader
                                                       (:record descriptor)
                                                       "work record")
                            errors (record-errors policy candidate descriptor record)]
                        (when (seq errors)
                          (throw (ex-info (string/join "; " errors)
                                          {:record record})))
                        (authenticated-bytes blob-reader (:report record)
                                             "ab-check report")
                        record))
                    (:records index))]
          {:status :ok
           :qualification_identity_ref (:qualification_identity_ref candidate)
           :candidate_ref (:candidate_ref candidate)
           :policy_hash (:policy_hash policy)
           :expected_work_ids (:expected_work_ids policy)
           :attempts attempts
           :records records})))
    (catch Exception error
      (unavailable (.getMessage error)))))

(defn derive-aggregate
  "Reduce authenticated repetition values with the policy's fixed maximum."
  [authenticated]
  (if-not (= :ok (:status authenticated))
    (unavailable (:reason authenticated))
    (let [records-by-repetition (group-by :repetition (:records authenticated))
          elapsed-by-repetition (into {} (map (juxt :repetition :elapsed_seconds)
                                              (:attempts authenticated)))
          repetition-values
          (mapv (fn [repetition]
                  (let [records (get records-by-repetition repetition)]
                    {:repetition repetition
                     :fatal_failures (count (filter #(= "fatal_error"
                                                        (:disposition %)) records))
                     :wall_time_seconds (get elapsed-by-repetition repetition)
                     :timeouts (count (filter #(= "adapter_timeout"
                                                  (:disposition %)) records))}))
                (range 1 4))]
      {:status :measured
       :qualification_identity_ref (:qualification_identity_ref authenticated)
       :candidate_ref (:candidate_ref authenticated)
       :policy_hash (:policy_hash authenticated)
       :work_count (count (:expected_work_ids authenticated))
       :repetition_count 3
       :fatal_failures (double (apply max (map :fatal_failures repetition-values)))
       :wall_time_seconds (double (apply max (map :wall_time_seconds repetition-values)))
       :timeouts (double (apply max (map :timeouts repetition-values)))
       :repetition_values repetition-values})))

(defn observation-envelopes
  "Project exactly the three identity-bound observations owned by this instrument."
  [candidate aggregate]
  (let [candidate (keywordize candidate)
        identity-ref (:qualification_identity_ref candidate)]
    (if (= :measured (:status aggregate))
      (into {}
            (map (fn [observed-key]
                   [observed-key
                    (capture/observation-envelope
                     identity-ref (double (get aggregate observed-key))
                     {:instrument :parser-rq-core-attempt-v1
                      :repetitions 3
                      :reduction :maximum})]))
            [:fatal_failures :wall_time_seconds :timeouts])
      (into {}
            (map (fn [observed-key]
                   [observed-key
                    (capture/observation-envelope
                     identity-ref :unavailable
                     {:instrument :parser-rq-core-attempt-v1
                      :reason (:reason aggregate)})]))
            [:fatal_failures :wall_time_seconds :timeouts]))))
