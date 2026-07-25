(ns abc.tools.parser-release-authority
  "The single boundary that authenticates the release parser identity: a
  decision-bound, content-addressed governed record.

  Reads `data/release-parser-identity-v1.edn`, recomputes BOTH of its content
  refs (the qualification-identity ref and the candidate ref) from its own
  bytes, and requires the one Accepted `release-parser-identity-approval`
  governance decision to BIND that exact candidate_ref (plus schema_version and
  the record path being authenticated). Integrity plus approval. Every failure
  is reported as one closed vector of problem maps rather than ad hoc booleans
  or bare strings. Later tasks (source trust, release admissibility) depend on
  this value and must not re-derive it."
  (:require [abc.tools.decisions :as decisions]
            [abc.tools.hash :as hash]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.walk :as walk]))

(def release-qualification-slug
  "release-parser-identity-approval")

(defn- problem [kind message & {:as data}]
  (merge {:kind kind :message message} data))

;; --- content-ref recompute ---------------------------------------------------
;; The record carries keyword keys; JCS canonicalization
;; (abc.tools.jcs/canonical-json-object) throws on keyword keys, so every value
;; is keyword-stringified before hashing. This is the identical recipe Task 3
;; used to MINT the committed refs, so recompute is byte-exact.

(defn- canonical-value [v]
  (walk/postwalk
   (fn [x]
     (cond
       (keyword? x) (name x)
       (map? x) (into {} (map (fn [[k vv]] [(if (keyword? k) (name k) k) vv])) x)
       :else x))
   v))

(defn- ref-of [v]
  (hash/format-sha256 (hash/sha256-json-jcs (canonical-value v))))

;; --- record loader + shape ---------------------------------------------------

(def ^:private required-record-keys
  [:schema_version :adapter_id :adapter_version :executables
   :qualification_identity :qualification_identity_ref :candidate_ref])

(defn- record-problems
  "Shape-validate the parsed release-parser-identity record: the required keys
  are present, the two content refs are sha256:… shaped, and every executable
  is a {:name :sha256} map with a sha256:… hash. Reports
  :invalid-release-parser-identity problems, mirroring the decisions loader."
  [record]
  (vec
   (concat
    (for [k required-record-keys :when (nil? (get record k))]
      (problem :invalid-release-parser-identity
               (str "release parser identity record is missing " k) :key k))
    (for [k [:qualification_identity_ref :candidate_ref]
          :let [v (get record k)]
          :when (and (some? v) (not (re-matches hash/hash-pattern (str v))))]
      (problem :invalid-release-parser-identity
               (str k " is not a sha256:… ref") :key k :value v))
    (when (some? (:executables record))
      (for [e (:executables record)
            :when (not (and (map? e)
                            (string? (:name e))
                            (string? (:sha256 e))
                            (re-matches hash/hash-pattern (str (:sha256 e)))))]
        (problem :invalid-release-parser-identity
                 "each executable must be {:name string :sha256 sha256:…}"
                 :value e))))))

(defn- load-shape-valid-record!
  "Read and EDN-parse the release-parser-identity record at path. Throws the
  standard authentication failure on an unreadable/unparseable file (the record
  is the content authority; without it there is nothing to authenticate), and
  ALSO on a parsed value whose outermost shape is unsafe for downstream
  consumers: a non-map record (authenticate dissocs :candidate_ref from it) or a
  non-sequential :executables (record-problems iterates it). Guarding both here
  means every failure mode of this loader — not just the readable-EDN one —
  reaches callers as the closed :problems vector instead of a raw
  ClassCastException/IllegalArgumentException escaping past it. Returns the
  parsed value; the remaining shape validation is `record-problems`, gathered
  alongside the decision, integrity, and binding problems."
  [path]
  (let [fail (fn [msg]
               (throw (ex-info "parser release authority authentication failed"
                               {:problems [(problem :invalid-release-parser-identity msg)]})))]
    (cond
      (or (nil? path) (not (fs/exists? path)))
      (fail (str "release parser identity record does not exist: " path))
      (not (fs/regular-file? path))
      (fail (str "release parser identity record is not a file: " path))
      :else
      (let [parsed (try
                     (edn/read-string (slurp (fs/file path)))
                     (catch Exception e
                       (fail (str "release parser identity record is not readable EDN: "
                                  (.getMessage e)))))]
        (cond
          (not (map? parsed))
          (fail (str "release parser identity record is not a map: " path))

          (not (or (nil? (:executables parsed)) (sequential? (:executables parsed))))
          (fail "release parser identity record :executables is not a sequence")

          :else parsed)))))

;; --- decision resolution + authority -----------------------------------------

(defn- decision-resolution
  "Resolve the release-parser-identity-approval decision from decisions-path via
  the shared strict corpus boundary (abc.tools.decisions/load-shape-valid-corpus!
  and decision-by-slug!). Never trusts a decision whose corpus failed to load or
  shape-validate, and never invents a status for a missing slug.

  Returns {:decision record :content-hash sha256} on success, or
  {:problems [...]} — never both."
  [decisions-path]
  (try
    (let [{:keys [corpus content-hash]}
          (decisions/load-shape-valid-corpus! decisions-path)]
      (try
        {:decision (decisions/decision-by-slug! corpus release-qualification-slug)
         :content-hash content-hash}
        (catch clojure.lang.ExceptionInfo error
          {:problems (mapv #(problem :missing-decision %) (:errors (ex-data error)))})))
    (catch clojure.lang.ExceptionInfo error
      {:problems (mapv #(problem :invalid-decisions-corpus %) (:errors (ex-data error)))})))

(defn- authority-problems
  "The release-authority decision must be exactly {:status :accepted
  :release-authority :publication}. :release-authority is the sole authorization
  field here; a future scope change requires a deliberate superseding decision."
  [decision]
  (cond-> []
    (not= :accepted (:status decision))
    (conj (problem :decision-not-accepted
                   (str "decision " release-qualification-slug " is not accepted")
                   :status (:status decision)))

    (not= :publication (:release-authority decision))
    (conj (problem :release-authority-not-publication
                   (str "decision " release-qualification-slug
                        " release-authority is not :publication")
                   :release-authority (:release-authority decision)))))

(defn- binding-problems
  "The decision must BIND the exact content authority (candidate_ref) + protocol
  discriminator (schema_version) + object selector (record_path == the file being
  authenticated). candidate_ref already commits transitively to the executables,
  mapping, schema, adapter, and converter, so re-checking those here would be
  duplicated authority."
  [decision record record-path]
  (let [b (:release-parser-identity decision)]
    (cond-> []
      (nil? b)
      (conj (problem :decision-has-no-release-parser-identity-binding
                     (str "decision " release-qualification-slug
                          " carries no :release-parser-identity binding")))

      (and b (not= (:candidate_ref b) (:candidate_ref record)))
      (conj (problem :decision-does-not-bind-record-candidate-ref
                     "decision does not bind the record's candidate_ref"
                     :bound (:candidate_ref b) :record (:candidate_ref record)))

      (and b (not= (:schema_version b) (:schema_version record)))
      (conj (problem :decision-schema-version-mismatch
                     "decision schema_version disagrees with the record"
                     :bound (:schema_version b) :record (:schema_version record)))

      (and b (not= (:record_path b) record-path))
      (conj (problem :decision-does-not-approve-this-record-path
                     "decision approves a different record path than the one authenticated"
                     :bound (:record_path b) :authenticating record-path)))))

(defn authenticate
  "The single authenticated release-parser-identity value: the content-addressed
  governed record, bound to the Accepted release-parser-identity-approval
  decision that grants it publication authority.

  opts: {:release_parser_identity_path <edn record path>
         :decisions_path <decisions.edn path>}.

  Loads the record, recomputes both of its content refs from its own bytes
  (qualification_identity_ref and candidate_ref), resolves the governance
  decision, and requires it to be Accepted/publication AND to bind this exact
  candidate_ref + schema_version + record path.

  Returns {:candidate-ref :qualification-identity-ref :qualification-identity
  :executable-provenance :decision :authority-hashes}. Throws ex-info
  \"parser release authority authentication failed\" carrying
  {:problems [problem-map ...]} on any failure."
  [{:keys [release_parser_identity_path decisions_path]}]
  (let [record (load-shape-valid-record! release_parser_identity_path)
        {:keys [decision content-hash problems]} (decision-resolution decisions_path)
        qi (:qualification_identity record)
        recompute-qi (ref-of qi)
        recompute-cand (ref-of (dissoc record :candidate_ref))
        integrity (cond-> []
                    (not= recompute-qi (:qualification_identity_ref record))
                    (conj (problem :qualification-identity-ref-mismatch
                                   "qualification_identity_ref does not recompute from the record"
                                   :recomputed recompute-qi
                                   :record (:qualification_identity_ref record)))
                    (not= recompute-cand (:candidate_ref record))
                    (conj (problem :candidate-ref-mismatch
                                   "candidate_ref does not recompute from the record"
                                   :recomputed recompute-cand
                                   :record (:candidate_ref record))))
        authority (if decision
                    (into (authority-problems decision)
                          (binding-problems decision record release_parser_identity_path))
                    [])
        all (vec (concat (record-problems record) problems integrity authority))]
    (if (seq all)
      (throw (ex-info "parser release authority authentication failed"
                      {:problems all}))
      {:candidate-ref (:candidate_ref record)
       :qualification-identity-ref (:qualification_identity_ref record)
       :qualification-identity qi
       :executable-provenance {:executables (:executables record)}
       :decision decision
       :authority-hashes {:decisions-file content-hash
                          :record-file (hash/format-sha256
                                        (hash/sha256-file release_parser_identity_path))}})))
