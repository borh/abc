(ns abc.tools.parser-rq-campaign
  "Pure identity, composition, selection, and promotion rules for one parser
  release-qualification campaign. Execution and governance edits stay outside
  this namespace."
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:import [java.time Instant]))

(def predicate-ids
  [:fatal-failures :source-span-coverage :silent-drops
   :diagnostic-completeness :parser-ir-schema-validation
   :publication-structure :wall-time :memory :timeout-policy])

(def observed-keys
  #{:fatal_failures :source_span_coverage :silent_drops
    :diagnostic_completeness :parser_ir_schema_validation
    :publication_structure :wall_time_seconds :peak_cgroup_memory_bytes
    :timeouts})

(def member-observed-keys
  {:core_attempt #{:fatal_failures :wall_time_seconds :timeouts}
   :source_recognition #{:source_span_coverage}
   :diagnostic_gap #{:silent_drops}
   :diagnostic_completeness #{:diagnostic_completeness}
   :parser_ir_conformance #{:parser_ir_schema_validation}
   :publication_structure #{:publication_structure}
   :resource #{:peak_cgroup_memory_bytes}})

(def candidate-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref
    :qualification_identity :executable_provenance_ref})

(def authorization-keys
  #{:schema_id :schema_version :authorization_ref :authorization_ordinal
    :candidate_ref :qualification_identity_ref :not_before_utc :not_after_utc
    :repetitions :reduction :host_policy_ref})

(defn- canonical-value [value]
  (walk/postwalk (fn [x]
                   (cond
                     (keyword? x) (name x)
                     (map? x) (into {} (map (fn [[k v]] [(if (keyword? k) (name k) k) v])) x)
                     :else x))
                 value))

(defn- content-ref [value self-field]
  (-> value
      (dissoc self-field)
      canonical-value
      hash/sha256-json-jcs
      hash/format-sha256))

(defn candidate-ref [candidate]
  (content-ref candidate :candidate_ref))

(defn authorization-ref [authorization]
  (content-ref authorization :authorization_ref))

(defn capture-generation-ref [capture-index]
  (content-ref capture-index :capture_generation_ref))

(defn evaluation-generation-ref [evaluation-index]
  (content-ref evaluation-index :evaluation_generation_ref))

(defn- parse-instant [value]
  (try
    (Instant/parse value)
    (catch Exception _ nil)))

(defn verify-authorization
  [candidate authorization now]
  (let [candidate-identity-ref
        (qualification/qualification-identity-ref (:qualification_identity candidate))
        not-before (parse-instant (:not_before_utc authorization))
        not-after (parse-instant (:not_after_utc authorization))
        execution-time (parse-instant now)]
    (cond-> []
      (not= candidate-keys (set (keys candidate)))
      (conj "candidate violates its closed key contract")

      (not= (:candidate_ref candidate) (candidate-ref candidate))
      (conj "candidate_ref does not authenticate the candidate")

      (not= candidate-identity-ref (:qualification_identity_ref candidate))
      (conj "qualification_identity_ref does not authenticate the candidate identity")

      (not= authorization-keys (set (keys authorization)))
      (conj "authorization violates its closed key contract")

      (not= (:authorization_ref authorization) (authorization-ref authorization))
      (conj "authorization_ref does not authenticate the authorization")

      (not= 1 (:authorization_ordinal authorization))
      (conj "authorization ordinal must be one")

      (not= (:candidate_ref candidate) (:candidate_ref authorization))
      (conj "authorization candidate_ref does not match the candidate")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref authorization))
      (conj "authorization qualification identity does not match the candidate")

      (or (nil? not-before) (nil? not-after) (nil? execution-time)
          (.isAfter ^Instant not-before ^Instant not-after)
          (.isBefore ^Instant execution-time ^Instant not-before)
          (.isAfter ^Instant execution-time ^Instant not-after))
      (conj "execution time is outside the inclusive authorization interval")

      (not= 3 (:repetitions authorization))
      (conj "authorization must require three repetitions")

      (not= "maximum" (:reduction authorization))
      (conj "authorization must require maximum reduction"))))

(defn compose-measurements
  [candidate _capture members]
  (let [member-errors
        (reduce-kv
         (fn [errors member expected]
           (let [actual (get members member)]
             (cond-> errors
               (not= expected (set (keys actual)))
               (conj (str (name member) " has missing or extra observation keys"))

               (some #(seq (capture/envelope-errors %)) (vals actual))
               (conj (str (name member) " contains a scalar or invalid envelope"))

               (some #(not= (:qualification_identity_ref candidate)
                            (:identity_ref %))
                     (vals actual))
               (conj (str (name member) " contains a cross-candidate envelope")))))
         (cond-> []
           (not= (set (keys member-observed-keys)) (set (keys members)))
           (conj "capture members are missing, extra, or duplicated"))
         member-observed-keys)
        composed (apply merge (vals members))]
    (when (or (seq member-errors)
              (not= observed-keys (set (keys composed))))
      (throw (ex-info "campaign measurement composition failed"
                      {:errors (cond-> member-errors
                                 (not= observed-keys (set (keys composed)))
                                 (conj "composition does not contain exactly nine observations"))})))
    composed))

(defn- resolve-one [label values predicate]
  (let [matching (filterv predicate values)]
    (when-not (= 1 (count matching))
      (throw (ex-info (str label " resolution requires exactly one matching generation")
                      {:matching (count matching) :available (count values)})))
    (first matching)))

(defn resolve-capture-values [authorization captures]
  (resolve-one "capture" captures
               #(= (:authorization_ref authorization) (:authorization_ref %))))

(defn resolve-current-evaluation-values [registry-ref evaluations]
  (resolve-one "evaluation" evaluations #(= registry-ref (:registry_ref %))))

(defn- read-indexes [directory]
  (->> (files/list-files-if-directory directory)
       (filter #(string/ends-with? (str %) ".edn"))
       (mapv files/read-edn)))

(defn resolve-capture [candidate-dir]
  (let [authorizations (read-indexes (str candidate-dir "/authorizations"))
        captures (read-indexes (str candidate-dir "/captures"))]
    (when-not (= 1 (count authorizations))
      (throw (ex-info "candidate must have exactly one authorization"
                      {:count (count authorizations)})))
    (resolve-capture-values (first authorizations) captures)))

(defn resolve-current-evaluation [candidate-dir registry]
  (let [registry-ref (-> registry canonical-value hash/sha256-json-jcs hash/format-sha256)
        evaluations (read-indexes (str candidate-dir "/evaluations"))]
    (resolve-current-evaluation-values registry-ref evaluations)))

(defn promotion-errors
  [{:keys [candidate authorization capture evaluation current_registry_ref
           qualification_report provenance replication adr_0040_status
           adr_0041_status capture_count canonical_equal]}]
  (let [verdicts (:predicate_verdicts qualification_report)
        ids (mapv :predicate_id verdicts)]
    (cond-> []
      (seq (verify-authorization candidate authorization
                                 (:not_before_utc authorization)))
      (conj "candidate authorization is invalid")

      (not= (:candidate_ref candidate) (:candidate_ref capture))
      (conj "capture is not bound to the candidate")

      (not= (:authorization_ref authorization) (:authorization_ref capture))
      (conj "capture is not bound to the authorization")

      (not= (:candidate_ref candidate) (:candidate_ref evaluation))
      (conj "evaluation is not bound to the candidate")

      (not= current_registry_ref (:registry_ref evaluation))
      (conj "evaluation registry is stale")

      (not= :release-qualified (:gate_status qualification_report))
      (conj "qualification gate is not release-qualified")

      (not= :admitted (get-in qualification_report [:admission :status]))
      (conj "qualification admission is not admitted")

      (or (not= (set predicate-ids) (set ids))
          (not= (count predicate-ids) (count ids))
          (some #(not= :pass (:verdict %)) verdicts))
      (conj "qualification report does not contain exactly nine passing predicates")

      (not= :reproducible (:status provenance))
      (conj "candidate executable provenance is not reproducible")

      (not= :replicated (:status replication))
      (conj "capture blobs are not replicated")

      (not= "Accepted" adr_0040_status)
      (conj "ADR 0040 is not Accepted")

      (not= "Accepted" adr_0041_status)
      (conj "ADR 0041 is not Accepted")

      (not= 1 capture_count)
      (conj "candidate has zero or sibling capture generations")

      (not (true? canonical_equal))
      (conj "canonical projections differ from immutable generation members"))))

(defn- usage []
  (str "usage: parser-rq-campaign <candidate-ref|authorization-ref|capture-ref|evaluate-ref> <input.edn>\n"
       "       parser-rq-campaign verify-authorization <candidate.edn> <authorization.edn> <utc>"))

(defn -main [& args]
  (let [[command & command-args] args]
    (case command
      "candidate-ref" (println (candidate-ref (files/read-edn (first command-args))))
      "authorization-ref" (println (authorization-ref (files/read-edn (first command-args))))
      "capture-ref" (println (capture-generation-ref (files/read-edn (first command-args))))
      "evaluate-ref" (println (evaluation-generation-ref (files/read-edn (first command-args))))
      "verify-authorization"
      (let [[candidate-path authorization-path now] command-args
            errors (verify-authorization (files/read-edn candidate-path)
                                         (files/read-edn authorization-path) now)]
        (when (seq errors) (throw (ex-info "authorization invalid" {:errors errors})))
        (println "ok"))
      (throw (ex-info (usage) {:args args})))))
