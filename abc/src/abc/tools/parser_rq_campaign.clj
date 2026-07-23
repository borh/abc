(ns abc.tools.parser-rq-campaign
  "Pure identity, composition, selection, and promotion rules for one parser
  release-qualification campaign. Execution and governance edits stay outside
  this namespace."
  (:require [abc.tools.aat-parser-ir-compat :as compat]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as json]
            [abc.tools.parser-release-qualification :as qualification]
            [abc.tools.parser-rq-capture :as capture]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:import [java.nio.charset StandardCharsets]
           [java.time Instant]))

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

(def capture-member-keys
  (conj (set (keys member-observed-keys)) :measurements))

(def capture-keys
  #{:schema_id :schema_version :capture_generation_ref :capture_started_at_utc
    :authorization_ref :candidate_ref :qualification_identity_ref :members})

(def blob-keys #{:sha256 :bytes :media_type :locator})

(def candidate-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref
    :qualification_identity :executable_provenance_ref})

(def authorization-keys
  #{:schema_id :schema_version :authorization_ref :authorization_ordinal
    :candidate_ref :qualification_identity_ref :readiness_receipt_ref
    :not_before_utc :not_after_utc :repetitions :reduction})

(def readiness-receipt-keys
  #{:schema_id :schema_version :readiness_receipt_ref
    :candidate_ref :qualification_identity_ref :provenance_core_ref
    :production_graph_hash :production_graph_version :candidate_git_rev
    :candidate_tree_clean :evidence_base_git_rev :evidence_tree_clean
    :corpus_snapshot_hash :corpus_list_hash})

(def executable-provenance-schema-id
  "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json")

(def executable-provenance-envelope-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref})

(def bound-executable-provenance-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref
    :provenance_core_ref :status :builds :executables})

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

(defn- sha256? [value]
  (and (string? value) (boolean (re-matches hash/hash-pattern value))))

(defn- named= [expected actual]
  (= (name expected) (if (keyword? actual) (name actual) actual)))

(defn- canonical-bytes [value]
  (jcs/canonical-json-bytes (canonical-value value)))

(defn candidate-ref [candidate]
  (content-ref candidate :candidate_ref))

(defn authorization-ref [authorization]
  (content-ref authorization :authorization_ref))

(defn readiness-receipt-ref [receipt]
  (content-ref receipt :readiness_receipt_ref))

(defn production-graph-ref [graph]
  (content-ref graph :policy_hash))

(defn production-graph-errors
  "Authenticate the committed production graph value."
  [graph]
  (let [graph (walk/keywordize-keys graph)
        expected-members ["core_attempt" "source_recognition" "diagnostic_gap"
                          "diagnostic_completeness" "parser_ir_conformance"
                          "publication_structure" "resource"]
        operation-members ["core_attempt" "predicate_hardening"
                           "source_recognition" "diagnostic_gap"
                           "publication_structure" "resource"]
        executables (:executables graph)
        executable-keys #{:name :adapter :adapter_version :argv_template}]
    (cond-> []
      (not= (:policy_hash graph) (production-graph-ref graph))
      (conj "parser RQ production graph hash has drifted")

      (not= expected-members (:installed_members graph))
      (conj "parser RQ production graph installed membership has drifted")

      (not= operation-members (mapv :name (:members graph)))
      (conj "parser RQ production operation membership has drifted")

      (or (not (vector? executables))
          (empty? executables)
          (not= (count executables) (count (set (map :name executables))))
          (some #(or (not= executable-keys (set (keys %)))
                     (some (fn [key]
                             (not (and (string? (get % key))
                                       (not (string/blank? (get % key))))))
                           [:name :adapter :adapter_version])
                     (not (and (vector? (:argv_template %))
                               (seq (:argv_template %))
                               (every? string? (:argv_template %)))))
                executables))
      (conj "parser RQ production executable coordinates are not closed"))))

(defn provenance-core-ref [provenance]
  (-> provenance
      (dissoc :schema_id :schema_version :candidate_ref
              :qualification_identity_ref :provenance_core_ref)
      canonical-value
      hash/sha256-json-jcs
      hash/format-sha256))

(defn capture-generation-ref [capture-index]
  (content-ref capture-index :capture_generation_ref))

(defn evaluation-generation-ref [evaluation-index]
  (content-ref evaluation-index :evaluation_generation_ref))

(defn receipt-ref [receipt]
  (content-ref receipt :receipt_ref))

(defn candidate-provenance-value [provenance]
  (apply dissoc provenance executable-provenance-envelope-keys))

(defn executable-provenance-ref [provenance]
  (-> provenance
      candidate-provenance-value
      canonical-value
      hash/sha256-json-jcs
      hash/format-sha256))

(declare provenance-errors verify-provenance-errors)

(defn build-candidate [repo parser-git-rev provenance]
  (let [abc-root (fs/file repo "abc")
        corpus (qualification/load-corpus
                (files/read-edn (fs/file abc-root
                                         "data/parser-release-qualification-corpus.edn")))
        predicates (qualification/load-predicates
                    (files/read-edn (fs/file abc-root
                                             "data/parser-release-qualification-predicates.edn")))
        mapping-path (fs/file repo "ab-validator/data/aat-to-parser-ir-mapping-v2.json")
        mapping (files/read-json mapping-path)
        parser (or (some #(when (= "ab-aozora" (:name %)) %) (:executables provenance))
                   (first (:executables provenance)))
        identity {:aat_version (get mapping "source_aat_version")
                  :aat_adapter (:adapter parser)
                  :aat_adapter_version (:adapter_version parser)
                  :mapping_id (get mapping "mapping_id")
                  :mapping_version (get mapping "mapping_version")
                  :mapping_hash (hash/sha256-json-abc-legacy-v0 mapping)
                  :mapping_schema_hash (get mapping "mapping_schema_hash")
                  :parser_ir_schema_id (get mapping "target_parser_ir_schema_id")
                  :parser_ir_schema_hash (get mapping "target_parser_ir_schema_hash")
                  :parser_git_rev parser-git-rev
                  :corpus_snapshot_hash (:corpus_snapshot_hash corpus)
                  :corpus_list_hash (:list_hash corpus)
                  :predicate_set_hash (:predicate_set_hash predicates)
                  :instrument_versions
                  (into (sorted-map)
                        (map (juxt (comp name :observed_key) :instrument))
                        (:predicates predicates))}
        identity-ref (qualification/qualification-identity-ref identity)]
    (when (or (seq (provenance-errors provenance))
              (not= parser-git-rev (:parser_git_rev parser)))
      (throw (ex-info "executable provenance does not bind the requested parser revision"
                      {:parser_git_rev parser-git-rev})))
    (let [candidate {:schema_id "https://w3id.org/abc/schemas/parser-rq-candidate.schema.json"
                     :schema_version "1.0.0"
                     :qualification_identity_ref identity-ref
                     :qualification_identity identity
                     :executable_provenance_ref (executable-provenance-ref provenance)}]
      (assoc candidate :candidate_ref (candidate-ref candidate)))))

(defn- blob-errors [label blob value]
  (let [bytes (canonical-bytes value)
        actual-hash (hash/format-sha256 (hash/sha256-bytes bytes))]
    (cond-> []
      (not= blob-keys (set (keys blob)))
      (conj (str label " blob reference violates its closed key contract"))

      (not= "application/json" (:media_type blob))
      (conj (str label " blob is not canonical JSON"))

      (or (not (string? (:locator blob)))
          (string/blank? (:locator blob))
          (string/starts-with? (:locator blob) "/")
          (some #{".."} (string/split (or (:locator blob) "") #"/")))
      (conj (str label " blob locator is invalid"))

      (not= (alength bytes) (:bytes blob))
      (conj (str label " blob byte length does not authenticate its value"))

      (not= actual-hash (:sha256 blob))
      (conj (str label " blob hash does not authenticate its value")))))

(defn capture-errors [candidate capture-index members]
  (let [capture-members (:members capture-index)]
    (vec
     (concat
      (cond-> []
        (not= capture-keys (set (keys capture-index)))
        (conj "capture index violates its closed key contract")

        (not= (:capture_generation_ref capture-index)
              (capture-generation-ref capture-index))
        (conj "capture_generation_ref does not authenticate the capture index")

        (not= (:candidate_ref candidate) (:candidate_ref capture-index))
        (conj "capture index is not bound to the candidate")

        (not= (:qualification_identity_ref candidate)
              (:qualification_identity_ref capture-index))
        (conj "capture index qualification identity does not match the candidate")

        (not= capture-member-keys (set (keys capture-members)))
        (conj "capture index member set is not closed")

        (not= (set (keys member-observed-keys)) (set (keys members)))
        (conj "supplied capture member set is not closed"))
      (mapcat (fn [[member value]]
                (blob-errors (name member) (get capture-members member) value))
              members)))))

(defn- parse-instant [value]
  (try
    (Instant/parse value)
    (catch Exception _ nil)))

(defn verify-readiness-receipt
  [candidate provenance graph receipt]
  (let [identity (:qualification_identity candidate)]
    (cond-> []
      (not= readiness-receipt-keys (set (keys receipt)))
      (conj "readiness receipt violates its closed key contract")

      (or (not= "https://w3id.org/abc/schemas/parser-rq-readiness-receipt.schema.json"
                (:schema_id receipt))
          (not= "2.0.0" (:schema_version receipt)))
      (conj "readiness receipt schema identity is invalid")

      (not= (:readiness_receipt_ref receipt) (readiness-receipt-ref receipt))
      (conj "readiness_receipt_ref does not authenticate the readiness receipt")

      (not= (:candidate_ref candidate) (:candidate_ref receipt))
      (conj "readiness receipt candidate_ref does not match the candidate")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref receipt))
      (conj "readiness receipt qualification identity does not match the candidate")

      (not= (:candidate_ref candidate) (:candidate_ref provenance))
      (conj "bound provenance candidate_ref does not match the candidate")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref provenance))
      (conj "bound provenance qualification identity does not match the candidate")

      (not= (:provenance_core_ref provenance) (:provenance_core_ref receipt))
      (conj "readiness receipt provenance core does not match")

      (not= (:provenance_core_ref provenance) (provenance-core-ref provenance))
      (conj "bound provenance core does not authenticate its evidence")

      (not= (:policy_hash graph) (:production_graph_hash receipt))
      (conj "readiness receipt production graph hash does not match")

      (not= (:policy_hash graph) (production-graph-ref graph))
      (conj "production graph policy hash does not authenticate the graph")

      (not= (:schema_version graph) (:production_graph_version receipt))
      (conj "readiness receipt production graph version does not match")

      (not= (:parser_git_rev identity) (:candidate_git_rev receipt))
      (conj "readiness receipt candidate revision does not match")

      (not= (:corpus_snapshot_hash identity) (:corpus_snapshot_hash receipt))
      (conj "readiness receipt corpus snapshot does not match")

      (not= (:corpus_list_hash identity) (:corpus_list_hash receipt))
      (conj "readiness receipt corpus list does not match")

      (not (and (:candidate_tree_clean receipt) (:evidence_tree_clean receipt)))
      (conj "readiness receipt does not bind clean candidate and evidence trees"))))

(defn- authorization-record-errors
  [candidate authorization]
  (let [candidate-identity-ref
        (qualification/qualification-identity-ref (:qualification_identity candidate))
        not-before (parse-instant (:not_before_utc authorization))
        not-after (parse-instant (:not_after_utc authorization))]
    (cond-> []
      (not= candidate-keys (set (keys candidate)))
      (conj "candidate violates its closed key contract")

      (not= (:candidate_ref candidate) (candidate-ref candidate))
      (conj "candidate_ref does not authenticate the candidate")

      (not= candidate-identity-ref (:qualification_identity_ref candidate))
      (conj "qualification_identity_ref does not authenticate the candidate identity")

      (not= authorization-keys (set (keys authorization)))
      (conj "authorization violates its closed key contract")

      (or (not= "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
                (:schema_id authorization))
          (not= "3.0.0" (:schema_version authorization)))
      (conj "authorization schema identity is invalid")

      (not= (:authorization_ref authorization) (authorization-ref authorization))
      (conj "authorization_ref does not authenticate the authorization")

      (not= 1 (:authorization_ordinal authorization))
      (conj "authorization ordinal must be one")

      (not= (:candidate_ref candidate) (:candidate_ref authorization))
      (conj "authorization candidate_ref does not match the candidate")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref authorization))
      (conj "authorization qualification identity does not match the candidate")

      (or (nil? not-before) (nil? not-after)
          (.isAfter ^Instant not-before ^Instant not-after))
      (conj "authorization interval is invalid")

      (not= 3 (:repetitions authorization))
      (conj "authorization must require three repetitions")

      (not= "maximum" (:reduction authorization))
      (conj "authorization must require maximum reduction"))))

(defn verify-authorization-record
  [candidate provenance graph receipt authorization]
  (vec
   (concat
    (authorization-record-errors candidate authorization)
    (verify-provenance-errors candidate provenance)
    (verify-readiness-receipt candidate provenance graph receipt)
    (when (not= (:readiness_receipt_ref receipt)
                (:readiness_receipt_ref authorization))
      ["authorization readiness receipt does not match the sealed receipt"]))))

(defn- temporal-authorization-errors [authorization now clock-synchronized?]
  (let [not-before (parse-instant (:not_before_utc authorization))
        not-after (parse-instant (:not_after_utc authorization))
        execution-time (parse-instant now)]
    (cond-> []
      (not= true clock-synchronized?)
      (conj "authorization clock is not synchronized")

      (or (nil? not-before) (nil? not-after) (nil? execution-time)
          (.isAfter ^Instant not-before ^Instant not-after)
          (.isBefore ^Instant execution-time ^Instant not-before)
          (.isAfter ^Instant execution-time ^Instant not-after))
      (conj "execution time is outside the inclusive authorization interval"))))

(defn verify-authorization
  ([candidate provenance graph receipt authorization now clock-synchronized?]
   (vec (concat (verify-authorization-record candidate provenance graph receipt authorization)
                (temporal-authorization-errors authorization now clock-synchronized?))))
  ([candidate authorization now]
   (vec (concat (authorization-record-errors candidate authorization)
                (temporal-authorization-errors authorization now true)))))

(defn runtime-inputs
  "Project the EDN authorities into the closed JSON value consumed by capture
  processes after structural authorization succeeds."
  [candidate authorization corpus]
  {:schema_version "abc/parser-rq-runtime-inputs/v1"
   :candidate (select-keys candidate
                           [:candidate_ref :qualification_identity_ref
                            :qualification_identity])
   :corpus corpus
   :source_accountability_corpus
   (mapv (fn [entry]
           (-> (select-keys entry [:work_id :source_path])
               (assoc :original_sha256 (:source_sha256 entry))))
         (:entries corpus))
   :authorization (select-keys authorization
                               [:authorization_ref :authorization_ordinal
                                :candidate_ref :qualification_identity_ref
                                :not_before_utc :not_after_utc :repetitions
                                :reduction])})

(defn build-authorization [candidate receipt ordinal not-before not-after]
  (let [authorization {:schema_id
                       "https://w3id.org/abc/schemas/parser-rq-capture-authorization.schema.json"
                       :schema_version "3.0.0"
                       :authorization_ordinal ordinal
                       :candidate_ref (:candidate_ref candidate)
                       :qualification_identity_ref (:qualification_identity_ref candidate)
                       :readiness_receipt_ref (:readiness_receipt_ref receipt)
                       :not_before_utc not-before
                       :not_after_utc not-after
                       :repetitions 3
                       :reduction "maximum"}]
    (assoc authorization :authorization_ref (authorization-ref authorization))))

(defn compose-measurements
  [candidate capture-index members]
  (let [authentication-errors (capture-errors candidate capture-index members)
        member-errors
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
        composed (apply merge (vals members))
        measurement-errors (blob-errors "measurements"
                                        (get-in capture-index [:members :measurements])
                                        composed)]
    (when (or (seq authentication-errors)
              (seq member-errors)
              (seq measurement-errors)
              (not= observed-keys (set (keys composed))))
      (throw (ex-info "campaign measurement composition failed"
                      {:errors (cond-> (vec (concat authentication-errors
                                                    member-errors
                                                    measurement-errors))
                                 (not= observed-keys (set (keys composed)))
                                 (conj "composition does not contain exactly nine observations"))})))
    composed))

(defn- load-json-blob [root blob]
  (let [authenticated (capture/authenticated-read {:root root} blob (:locator blob))]
    (when-not (= :ok (:status authenticated))
      (throw (ex-info "campaign member blob is unavailable"
                      {:blob blob :reason (:reason authenticated)})))
    (let [bytes (:bytes authenticated)
          value (-> (String. ^bytes bytes StandardCharsets/UTF_8)
                    json/read-json-str
                    walk/keywordize-keys)]
      (when-not (java.util.Arrays/equals ^bytes bytes ^bytes (canonical-bytes value))
        (throw (ex-info "campaign member is not canonical JSON" {:blob blob})))
      value)))

(defn load-capture-generation [candidate capture-root]
  (let [capture-index (files/read-edn (fs/file capture-root "capture-index.edn"))
        capture-members (:members capture-index)
        members (into {}
                      (for [member (keys member-observed-keys)]
                        [member (load-json-blob capture-root (get capture-members member))]))
        measurements (load-json-blob capture-root (get capture-members :measurements))
        composed (compose-measurements candidate capture-index members)]
    (when-not (= composed measurements)
      (throw (ex-info "stored measurements differ from authenticated composition" {})))
    {:capture_index capture-index
     :members members
     :measurements measurements}))

(declare canonical-file-equal? blob-for-value write-canonical-json! write-edn!)

(defn assemble-capture-generation!
  [candidate authorization capture-root capture-started-at-utc]
  (let [members
        (into {}
              (for [member (keys member-observed-keys)
                    :let [path (fs/file capture-root (str (name member) ".json"))
                          value (-> (files/read-json path) walk/keywordize-keys)]]
                (do
                  (when-not (canonical-file-equal? path value)
                    (throw (ex-info "capture member is not canonical JSON"
                                    {:member member :path (str path)})))
                  [member value])))
        measurements (apply merge (vals members))
        member-refs (reduce-kv
                     (fn [refs member value]
                       (assoc refs member
                              (blob-for-value (str (name member) ".json") value)))
                     {} members)
        measurements-ref (blob-for-value "measurements.json" measurements)
        capture-base
        {:schema_id "https://w3id.org/abc/schemas/parser-rq-capture-index.schema.json"
         :schema_version "1.0.0"
         :capture_started_at_utc capture-started-at-utc
         :authorization_ref (:authorization_ref authorization)
         :candidate_ref (:candidate_ref candidate)
         :qualification_identity_ref (:qualification_identity_ref candidate)
         :members (assoc member-refs :measurements measurements-ref)}
        capture-index (assoc capture-base :capture_generation_ref
                             (capture-generation-ref capture-base))]
    (compose-measurements candidate capture-index members)
    (write-canonical-json! (fs/file capture-root "measurements.json") measurements)
    (write-edn! (fs/file capture-root "capture-index.edn") capture-index)
    {:capture_index capture-index :members members :measurements measurements}))

(defn verify-capture-errors [candidate authorization capture-root]
  (try
    (let [{:keys [capture_index]} (load-capture-generation candidate capture-root)]
      (cond-> (vec (verify-authorization candidate authorization
                                         (:capture_started_at_utc capture_index)))
        (not= (:authorization_ref authorization) (:authorization_ref capture_index))
        (conj "capture index is not bound to the authorization")))
    (catch Exception error
      [(or (some-> error ex-data :errors first)
           (.getMessage error))])))

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

(defn- read-generation-indexes [directory index-name]
  (if (fs/directory? directory)
    (->> (fs/list-dir directory)
         (filter #(and (fs/directory? %) (not (fs/sym-link? %))))
         (sort-by str)
         (map #(fs/file % index-name))
         (filter fs/regular-file?)
         (mapv files/read-edn))
    []))

(defn resolve-capture [candidate-dir]
  (let [authorizations (read-indexes (str candidate-dir "/authorizations"))
        captures (read-generation-indexes (str candidate-dir "/captures")
                                          "capture-index.edn")]
    (when-not (= 1 (count authorizations))
      (throw (ex-info "candidate must have exactly one authorization"
                      {:count (count authorizations)})))
    (resolve-capture-values (first authorizations) captures)))

(defn resolve-current-evaluation [candidate-dir registry]
  (let [registry-ref (-> registry canonical-value hash/sha256-json-jcs hash/format-sha256)
        evaluations (read-generation-indexes (str candidate-dir "/evaluations")
                                             "evaluation-index.edn")]
    (resolve-current-evaluation-values registry-ref evaluations)))

(def evaluation-keys
  #{:schema_id :schema_version :evaluation_generation_ref :candidate_ref
    :qualification_identity_ref :capture_generation_ref :registry_ref :members})

(def evaluation-member-keys
  #{:admission_candidate :admission_report :qualification_report})

(defn- evaluation-errors [candidate capture-index registry-ref evaluation members]
  (vec
   (concat
    (cond-> []
      (not= evaluation-keys (set (keys evaluation)))
      (conj "evaluation index violates its closed key contract")

      (not= (:evaluation_generation_ref evaluation)
            (evaluation-generation-ref evaluation))
      (conj "evaluation_generation_ref does not authenticate the evaluation index")

      (not= (:candidate_ref candidate) (:candidate_ref evaluation))
      (conj "evaluation is not bound to the candidate")

      (not= (:qualification_identity_ref candidate)
            (:qualification_identity_ref evaluation))
      (conj "evaluation qualification identity does not match the candidate")

      (not= (:capture_generation_ref capture-index)
            (:capture_generation_ref evaluation))
      (conj "evaluation is not bound to the capture generation")

      (not= registry-ref (:registry_ref evaluation))
      (conj "evaluation registry is stale")

      (not= evaluation-member-keys (set (keys (:members evaluation))))
      (conj "evaluation member set is not closed")

      (not= evaluation-member-keys (set (keys members)))
      (conj "supplied evaluation member set is not closed"))
    (mapcat (fn [[member value]]
              (blob-errors (name member) (get-in evaluation [:members member]) value))
            members))))

(defn load-evaluation-generation [candidate capture-index registry-ref evaluation-root]
  (let [evaluation (files/read-edn (fs/file evaluation-root "evaluation-index.edn"))
        members (into {}
                      (for [member evaluation-member-keys]
                        [member (load-json-blob evaluation-root
                                                (get-in evaluation [:members member]))]))
        errors (evaluation-errors candidate capture-index registry-ref evaluation members)]
    (when (seq errors)
      (throw (ex-info "campaign evaluation authentication failed" {:errors errors})))
    {:evaluation_index evaluation :members members}))

(defn provenance-errors
  "Authenticate the evidence behind a reproducible status.

  A status word alone has no authority: the two independently named builds
  must resolve to the same output identity and the executable coordinates must
  be closed, unique, and content-addressed."
  [{:keys [status builds executables]}]
  (let [build-ids (mapv :build_id builds)
        output-refs (mapv :output_ref builds)
        names (mapv :name executables)
        executable-keys #{:name :nix_output :nar_hash :sha256 :bytes :adapter
                          :adapter_version :parser_git_rev :argv_template}]
    (cond-> []
      (not (named= :reproducible status))
      (conj "provenance status is not reproducible")

      (not= #{"build-a" "build-b"} (set build-ids))
      (conj "provenance does not contain exactly the two independent builds")

      (or (not= 2 (count output-refs))
          (not= 1 (count (set output-refs)))
          (some #(not (sha256? %)) output-refs))
      (conj "independent build output identities differ or are invalid")

      (or (empty? executables) (not= (count names) (count (set names))))
      (conj "executable membership is empty or duplicated")

      (some #(or (not= executable-keys (set (keys %)))
                 (not (sha256? (:nar_hash %)))
                 (not (sha256? (:sha256 %)))
                 (not (pos-int? (:bytes %)))
                 (not (and (string? (:parser_git_rev %))
                           (re-matches #"[0-9a-f]{40}" (:parser_git_rev %)))))
            executables)
      (conj "an executable provenance record is malformed"))))

(defn verify-provenance-errors [candidate provenance]
  (cond-> (vec (provenance-errors provenance))
    (or (not= bound-executable-provenance-keys (set (keys provenance)))
        (not= executable-provenance-schema-id (:schema_id provenance))
        (not= "2.0.0" (:schema_version provenance)))
    (conj "bound provenance envelope is invalid")

    (not= (:provenance_core_ref provenance)
          (provenance-core-ref provenance))
    (conj "bound provenance core does not authenticate its evidence")

    (not= (:executable_provenance_ref candidate)
          (executable-provenance-ref provenance))
    (conj "executable provenance does not authenticate the candidate")

    (not= (:candidate_ref candidate) (:candidate_ref provenance))
    (conj "executable provenance candidate_ref does not match")

    (not= (:qualification_identity_ref candidate)
          (:qualification_identity_ref provenance))
    (conj "executable provenance qualification identity does not match")))

(defn evidence-integrity-errors
  "Authenticate one closed set of manifest-referenced evidence bytes."
  [{:keys [status blobs] :as receipt} manifest-blobs]
  (let [receipt-blobs (mapv :blob blobs)
        identity #(select-keys % [:sha256 :bytes :media_type :locator])]
    (cond-> []
      (not= #{:schema_id :schema_version :receipt_ref :candidate_ref
              :capture_generation_ref :status :blobs}
            (set (keys receipt)))
      (conj "evidence integrity receipt violates its closed key contract")

      (not (named= :verified status))
      (conj "evidence integrity status is not verified")

      (or (empty? receipt-blobs)
          (not= (set (map identity manifest-blobs))
                (set (map identity receipt-blobs)))
          (not= (count manifest-blobs) (count receipt-blobs)))
      (conj "evidence integrity membership differs from capture manifests")

      (some (fn [{:keys [blob rehash observed_bytes] :as row}]
              (or (not= #{:blob :rehash :observed_bytes} (set (keys row)))
                  (not= (:sha256 blob) rehash)
                  (not= (:bytes blob) observed_bytes)
                  (not (sha256? (:sha256 blob)))
                  (not (nat-int? (:bytes blob)))
                  (not (and (string? (:media_type blob))
                            (not (string/blank? (:media_type blob)))))))
            blobs)
      (conj "evidence integrity receipt does not authenticate stored bytes"))))

(defn evidence-integrity-receipt-errors
  [receipt candidate-reference capture-reference manifest-blobs]
  (cond-> (vec (evidence-integrity-errors receipt manifest-blobs))
    (or (not= "https://w3id.org/abc/schemas/parser-rq-evidence-integrity-receipt.schema.json"
              (:schema_id receipt))
        (not= "1.0.0" (:schema_version receipt)))
    (conj "evidence integrity receipt schema identity is invalid")

    (not= (:receipt_ref receipt) (receipt-ref receipt))
    (conj "evidence integrity receipt self-reference is invalid")

    (not= candidate-reference (:candidate_ref receipt))
    (conj "evidence integrity receipt is not bound to the candidate")

    (not= capture-reference (:capture_generation_ref receipt))
    (conj "evidence integrity receipt is not bound to the capture")))

(defn- promotion-value-errors
  [{:keys [candidate authorization capture evaluation current_registry_ref
           qualification_report evidence_integrity adr_0040_status
           adr_0041_status capture_count canonical_equal manifest_blobs]}]
  (let [verdicts (:predicate_verdicts qualification_report)
        ids (mapv :predicate_id verdicts)]
    (cond-> []
      (seq (verify-authorization candidate authorization
                                 (:capture_started_at_utc capture)))
      (conj "candidate authorization is invalid")

      (not= (:candidate_ref candidate) (:candidate_ref capture))
      (conj "capture is not bound to the candidate")

      (not= (:authorization_ref authorization) (:authorization_ref capture))
      (conj "capture is not bound to the authorization")

      (not= (:candidate_ref candidate) (:candidate_ref evaluation))
      (conj "evaluation is not bound to the candidate")

      (not= current_registry_ref (:registry_ref evaluation))
      (conj "evaluation registry is stale")

      (not (named= :release-qualified (:gate_status qualification_report)))
      (conj "qualification gate is not release-qualified")

      (not (named= :admitted (get-in qualification_report [:admission :status])))
      (conj "qualification admission is not admitted")

      (or (not= (set (map name predicate-ids))
                (set (map #(if (keyword? %) (name %) %) ids)))
          (not= (count predicate-ids) (count ids))
          (some #(not (named= :pass (:verdict %))) verdicts))
      (conj "qualification report does not contain exactly nine passing predicates")

      (seq (evidence-integrity-receipt-errors
            evidence_integrity (:candidate_ref candidate)
            (:capture_generation_ref capture) manifest_blobs))
      (into (evidence-integrity-receipt-errors
             evidence_integrity (:candidate_ref candidate)
             (:capture_generation_ref capture) manifest_blobs))

      (not= "Accepted" adr_0040_status)
      (conj "ADR 0040 is not Accepted")

      (not= "Accepted" adr_0041_status)
      (conj "ADR 0041 is not Accepted")

      (not= 1 capture_count)
      (conj "candidate has zero or sibling capture generations")

      (not (true? canonical_equal))
      (conj "canonical projections differ from immutable generation members"))))

(defn- ref-directory-name [reference]
  (hash/parse-sha256 reference))

(defn- read-json-value [path]
  (walk/keywordize-keys (files/read-json path)))

(defn- adr-status [path]
  (some->> (string/split-lines (files/read-text path))
           (keep #(second (re-matches #"Status:\s*(\S+)" %)))
           first))

(defn- current-registry-ref [registry]
  (-> registry canonical-value hash/sha256-json-jcs hash/format-sha256))

(defn- canonical-file-equal? [path value]
  (try
    (java.util.Arrays/equals ^bytes (files/read-bytes path)
                             ^bytes (canonical-bytes value))
    (catch Exception _ false)))

(defn- blob-for-value [locator value]
  (let [bytes (canonical-bytes value)]
    {:sha256 (hash/format-sha256 (hash/sha256-bytes bytes))
     :bytes (alength bytes)
     :media_type "application/json"
     :locator locator}))

(defn- write-canonical-json! [path value]
  (files/create-parent-dirs! path)
  (files/write-bytes! path (canonical-bytes value)))

(defn- read-provenance [path]
  (-> (files/read-json path)
      walk/keywordize-keys))

(declare write-edn!)

(defn evaluate-generation!
  [{:keys [candidate_path capture_root registry_path admission_candidate_path
           output_root]}]
  (let [candidate (files/read-edn candidate_path)
        capture-generation (load-capture-generation candidate capture_root)
        capture-index (:capture_index capture-generation)
        registry (files/read-edn registry_path)
        admission-candidate (files/read-edn admission_candidate_path)
        admission-report (compat/admission-report registry admission-candidate)
        qualification-report
        (qualification/build-report
         {:report_id (str "abc/parser-release-qualification/"
                          (ref-directory-name (:candidate_ref candidate)))
          :corpus (qualification/load-corpus)
          :predicate-set (qualification/load-predicates)
          :registry registry
          :identity (:qualification_identity candidate)
          :measurements (:measurements capture-generation)
          :admission_candidate admission-candidate})
        member-values {:admission_candidate admission-candidate
                       :admission_report admission-report
                       :qualification_report qualification-report}
        member-refs (reduce-kv
                     (fn [refs member value]
                       (assoc refs member (blob-for-value (str (name member) ".json") value)))
                     {} member-values)
        evaluation-base
        {:schema_id "https://w3id.org/abc/schemas/parser-rq-evaluation-index.schema.json"
         :schema_version "1.0.0"
         :candidate_ref (:candidate_ref candidate)
         :qualification_identity_ref (:qualification_identity_ref candidate)
         :capture_generation_ref (:capture_generation_ref capture-index)
         :registry_ref (current-registry-ref registry)
         :members member-refs}
        evaluation (assoc evaluation-base :evaluation_generation_ref
                          (evaluation-generation-ref evaluation-base))]
    (when (fs/exists? output_root)
      (throw (ex-info "evaluation output root already exists" {:path output_root})))
    (fs/create-dirs output_root)
    (doseq [[member value] member-values]
      (write-canonical-json! (fs/file output_root (get-in member-refs [member :locator])) value))
    (write-edn! (fs/file output_root "evaluation-index.edn") evaluation)
    evaluation))

(defn publish-evaluation! [candidate-root evaluation-root]
  (let [evaluation (files/read-edn (fs/file evaluation-root "evaluation-index.edn"))
        expected (evaluation-generation-ref evaluation)
        reference (:evaluation_generation_ref evaluation)
        target (fs/file candidate-root "evaluations" (ref-directory-name reference))]
    (when-not (= expected reference)
      (throw (ex-info "evaluation self-reference is invalid" {})))
    (when (fs/exists? target)
      (throw (ex-info "evaluation generation already exists" {:path (str target)})))
    (fs/create-dirs (fs/parent target))
    (fs/copy-tree evaluation-root target)
    reference))

(defn promotion-errors
  "Resolve and authenticate a promotion solely from committed campaign paths.

  Callers provide coordinates, never verdict booleans, capture counts, ADR
  statuses, or canonical-equality claims. Those facts are derived here from
  the immutable generation directories and referenced bytes."
  [{:keys [runs_root candidate_ref registry_path measurements_path report_path
           provenance_path adr_0040_path adr_0041_path]}]
  (try
    (let [candidate-dir (fs/file runs_root (ref-directory-name candidate_ref))
          candidate (files/read-edn (fs/file candidate-dir "candidate.edn"))
          authorizations (read-indexes (fs/file candidate-dir "authorizations"))
          authorization (resolve-one "authorization" authorizations
                                     #(= candidate_ref (:candidate_ref %)))
          capture-indexes (read-generation-indexes (fs/file candidate-dir "captures")
                                                   "capture-index.edn")
          capture-index (resolve-capture-values authorization capture-indexes)
          capture-root (fs/file candidate-dir "captures"
                                (ref-directory-name (:capture_generation_ref capture-index)))
          capture-generation (load-capture-generation candidate capture-root)
          registry (files/read-edn registry_path)
          registry-ref (current-registry-ref registry)
          evaluation-indexes (read-generation-indexes (fs/file candidate-dir "evaluations")
                                                      "evaluation-index.edn")
          evaluation-index (resolve-current-evaluation-values registry-ref evaluation-indexes)
          evaluation-root (fs/file candidate-dir "evaluations"
                                   (ref-directory-name
                                    (:evaluation_generation_ref evaluation-index)))
          evaluation-generation (load-evaluation-generation
                                 candidate capture-index registry-ref evaluation-root)
          evaluation-members (:members evaluation-generation)
          qualification-report (:qualification_report evaluation-members)
          provenance (read-provenance provenance_path)
          evidence-integrity
          (read-json-value (fs/file capture-root "evidence-integrity-receipt.json"))
          manifest-blobs (vec (vals (:members capture-index)))
          binding-errors
          (cond-> (vec (verify-provenance-errors candidate provenance))
            (not= candidate_ref (:candidate_ref candidate))
            (conj "candidate path does not match its authenticated candidate_ref")

            (not= candidate_ref (candidate-ref candidate))
            (conj "candidate_ref does not authenticate the committed candidate"))

          derived {:candidate candidate
                   :authorization authorization
                   :capture capture-index
                   :evaluation evaluation-index
                   :current_registry_ref registry-ref
                   :qualification_report qualification-report
                   :evidence_integrity evidence-integrity
                   :manifest_blobs manifest-blobs
                   :adr_0040_status (adr-status adr_0040_path)
                   :adr_0041_status (adr-status adr_0041_path)
                   :capture_count (count capture-indexes)
                   :canonical_equal
                   (and (canonical-file-equal? measurements_path
                                               (:measurements capture-generation))
                        (canonical-file-equal? report_path qualification-report))}]
      (vec (concat binding-errors (promotion-value-errors derived))))
    (catch Exception error
      [(or (some-> error ex-data :errors first)
           (.getMessage error))])))

(defn- parse-options [args]
  (loop [remaining args options {}]
    (if (empty? remaining)
      options
      (let [[option value & tail] remaining]
        (when-not (and (string/starts-with? (or option "") "--") value)
          (throw (ex-info "campaign options must be --name value pairs" {:args args})))
        (recur tail (assoc options
                           (-> option (subs 2) (string/replace "-" "_") keyword)
                           value))))))

(defn- required-option [options key]
  (or (get options key)
      (throw (ex-info (str "missing required option --"
                           (string/replace (name key) "_" "-"))
                      {:option key}))))

(defn- write-edn! [path value]
  (files/create-parent-dirs! path)
  (files/write-text! path (str (pr-str value) "\n")))

(defn- candidate-directory [runs-root reference]
  (fs/file runs-root (ref-directory-name reference)))

(defn- project! [options]
  (let [runs-root (required-option options :runs_root)
        candidate-reference (required-option options :candidate_ref)
        candidate-dir (candidate-directory runs-root candidate-reference)
        candidate (files/read-edn (fs/file candidate-dir "candidate.edn"))
        capture-index (resolve-capture candidate-dir)
        capture-root (fs/file candidate-dir "captures"
                              (ref-directory-name (:capture_generation_ref capture-index)))
        capture-generation (load-capture-generation candidate capture-root)]
    (when-let [path (:measurements_out options)]
      (files/create-parent-dirs! path)
      (files/write-bytes! path (canonical-bytes (:measurements capture-generation))))
    (when-let [path (:report_out options)]
      (let [registry (files/read-edn (required-option options :registry))
            evaluation-index (resolve-current-evaluation candidate-dir registry)
            evaluation-root (fs/file candidate-dir "evaluations"
                                     (ref-directory-name
                                      (:evaluation_generation_ref evaluation-index)))
            evaluation-generation
            (load-evaluation-generation candidate capture-index
                                        (current-registry-ref registry)
                                        evaluation-root)]
        (files/create-parent-dirs! path)
        (files/write-bytes! path
                            (canonical-bytes
                             (get-in evaluation-generation
                                     [:members :qualification_report])))))
    {:capture_generation_ref (:capture_generation_ref capture-index)}))

(defn- usage []
  (str "usage: parser-rq-campaign <candidate-ref|qualification-identity-ref|authorization-ref|capture-ref|evaluate-ref> --<kind> PATH\n"
       "       parser-rq-campaign verify-authorization-record --candidate PATH --provenance PATH --graph PATH --receipt PATH --authorization PATH\n"
       "       parser-rq-campaign verify-authorization --candidate PATH --provenance PATH --graph PATH --receipt PATH --authorization PATH --utc TIME --clock-synchronized true|false\n"
       "       parser-rq-campaign runtime-inputs --candidate PATH --authorization PATH --out PATH\n"
       "       parser-rq-campaign compose|verify-capture --candidate PATH --capture-root DIR [--authorization PATH] [--out PATH]\n"
       "       parser-rq-campaign project --runs-root DIR --candidate-ref HASH --registry PATH [--measurements-out PATH] [--report-out PATH]\n"
       "       parser-rq-campaign verify-promotion --runs-root DIR --candidate-ref HASH --registry PATH --measurements PATH --report PATH --provenance PATH --adr-0040 PATH --adr-0041 PATH"))

(defn -main [& args]
  (let [[command & command-args] args]
    (case command
      "candidate-ref"
      (let [options (parse-options command-args)]
        (println (candidate-ref (files/read-edn (required-option options :candidate)))))
      "qualification-identity-ref"
      (let [options (parse-options command-args)]
        (println (qualification/qualification-identity-ref
                  (:qualification_identity
                   (files/read-edn (required-option options :candidate))))))
      "authorization-ref"
      (let [options (parse-options command-args)]
        (println (authorization-ref
                  (files/read-edn (required-option options :authorization)))))
      "capture-ref"
      (let [options (parse-options command-args)]
        (println (capture-generation-ref
                  (files/read-edn (required-option options :capture_index)))))
      "evaluate-ref"
      (let [options (parse-options command-args)]
        (println (evaluation-generation-ref
                  (files/read-edn (required-option options :evaluation_index)))))
      "candidate"
      (let [options (parse-options command-args)
            value (build-candidate (required-option options :repo)
                                   (required-option options :parser_git_rev)
                                   (read-provenance
                                    (required-option options :provenance)))]
        (write-edn! (required-option options :out) value)
        (println (:candidate_ref value)))
      "authorize"
      (let [options (parse-options command-args)
            candidate (files/read-edn (required-option options :candidate))
            receipt (-> (files/read-json (required-option options :receipt))
                        walk/keywordize-keys)
            value (build-authorization candidate receipt
                                       (parse-long (required-option options :ordinal))
                                       (required-option options :not_before)
                                       (required-option options :not_after))]
        (write-edn! (required-option options :out) value)
        (println (:authorization_ref value)))
      "verify-authorization-record"
      (let [options (parse-options command-args)
            errors (verify-authorization-record
                    (files/read-edn (required-option options :candidate))
                    (-> (files/read-json (required-option options :provenance))
                        walk/keywordize-keys)
                    (-> (files/read-json (required-option options :graph))
                        walk/keywordize-keys)
                    (-> (files/read-json (required-option options :receipt))
                        walk/keywordize-keys)
                    (files/read-edn (required-option options :authorization)))]
        (when (seq errors) (throw (ex-info "authorization record invalid" {:errors errors})))
        (println "ok"))
      "verify-authorization"
      (let [options (parse-options command-args)
            errors (verify-authorization
                    (files/read-edn (required-option options :candidate))
                    (-> (files/read-json (required-option options :provenance))
                        walk/keywordize-keys)
                    (-> (files/read-json (required-option options :graph))
                        walk/keywordize-keys)
                    (-> (files/read-json (required-option options :receipt))
                        walk/keywordize-keys)
                    (files/read-edn (required-option options :authorization))
                    (required-option options :utc)
                    (= "true" (required-option options :clock_synchronized)))]
        (when (seq errors) (throw (ex-info "authorization invalid" {:errors errors})))
        (println "ok"))
      "verify-provenance"
      (let [options (parse-options command-args)
            errors (verify-provenance-errors
                    (files/read-edn (required-option options :candidate))
                    (read-provenance (required-option options :provenance)))]
        (when (seq errors) (throw (ex-info "provenance invalid" {:errors errors})))
        (println "ok"))
      "runtime-inputs"
      (let [options (parse-options command-args)]
        (write-canonical-json!
         (required-option options :out)
         (runtime-inputs
          (files/read-edn (required-option options :candidate))
          (files/read-edn (required-option options :authorization))
          (qualification/load-corpus)))
        (println "ok"))
      "compose"
      (let [options (parse-options command-args)
            candidate (files/read-edn (required-option options :candidate))
            capture-root (required-option options :capture_root)
            generation (if (fs/regular-file? (fs/file capture-root "capture-index.edn"))
                         (load-capture-generation candidate capture-root)
                         (assemble-capture-generation!
                          candidate
                          (files/read-edn (required-option options :authorization))
                          capture-root
                          (required-option options :capture_started_at)))]
        (write-edn! (required-option options :out) (:measurements generation))
        (println "ok"))
      "verify-capture"
      (let [options (parse-options command-args)
            errors (verify-capture-errors
                    (files/read-edn (required-option options :candidate))
                    (files/read-edn (required-option options :authorization))
                    (required-option options :capture_root))]
        (when (seq errors) (throw (ex-info "capture invalid" {:errors errors})))
        (println "ok"))
      "project"
      (do (project! (parse-options command-args)) (println "ok"))
      "evaluate"
      (let [options (parse-options command-args)
            value (evaluate-generation!
                   {:candidate_path (required-option options :candidate)
                    :capture_root (required-option options :capture_root)
                    :registry_path (required-option options :registry)
                    :admission_candidate_path
                    (required-option options :admission_candidate)
                    :output_root (required-option options :out)})]
        (println (:evaluation_generation_ref value)))
      "publish-evaluation"
      (let [options (parse-options command-args)]
        (println (publish-evaluation! (required-option options :candidate_root)
                                      (required-option options :evaluation_root))))
      "verify-promotion"
      (let [options (parse-options command-args)
            errors (promotion-errors
                    {:runs_root (required-option options :runs_root)
                     :candidate_ref (required-option options :candidate_ref)
                     :registry_path (required-option options :registry)
                     :measurements_path (required-option options :measurements)
                     :report_path (required-option options :report)
                     :provenance_path (required-option options :provenance)
                     :adr_0040_path (required-option options :adr_0040)
                     :adr_0041_path (required-option options :adr_0041)})]
        (when (seq errors) (throw (ex-info "promotion invalid" {:errors errors})))
        (println "ok"))
      (throw (ex-info (usage) {:args args})))))
