(ns abc.tools.analysis-identity
  (:require [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [clojure.set :as set]
            [clojure.string :as string]))

(def allowed-missing-policies
  #{"require-existing" "build-missing-only" "record-missing-status"})

(def allowed-input-view-kinds
  #{"parser-ir-plaintext-body-v1" "parser-ir-body-annotations-v1"})

;; The identity (no-op) input-normalization policy hash — produced by Rust
;; (ab_ortho_detect::NormalizationPolicy::identity, spec Issue 2 P1) and used
;; verbatim here (U1: compute stays in Rust, ABC reads/records the hash). This
;; is the value the input view records when no ortho normalization was applied
;; (the default for every source-identity flow).
(def identity-normalization-policy-hash
  "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813")

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(def analysis-recipe-hash hash-json-value)

(def tokenizer-profile-hash hash-json-value)

(def pack-policy-hash hash-json-value)

(def annotation-policy-hash hash-json-value)

(defn- required-string [label value]
  (when (or (not (string? value)) (string/blank? value))
    (throw (ex-info (str label " must be a non-empty string")
                    {:label label
                     :value value})))
  value)

(defn- subject-sort-key [subject]
  [(required-string "source_id" (get subject "source_id"))
   (required-string "work_content_hash" (get subject "work_content_hash"))
   (or (get subject "metadata_record_hash") "")])

(defn assert-input-normalization-agreement!
  "Enforce the F1 agreement: the tokenizer profile *declares* the input
  normalization it expects (`declared`), and the run *applied* (`applied`, from
  the Rust run-provenance / recorded on the input view) must match it. A
  mismatch means the run normalized its analyzer input differently than the
  profile promised — a hard error. Returns `applied` on success."
  [{:keys [declared applied context]}]
  (when-not (= declared applied)
    (throw (ex-info "input-normalization policy mismatch: the run applied a different normalization than the tokenizer profile declared"
                    {:declared_input_normalization_policy_hash declared
                     :applied_input_normalization_policy_hash applied
                     :context context})))
  applied)

(defn- normalize-subject [subject]
  (assoc subject "metadata_record_hash" (get subject "metadata_record_hash")))

(defn canonical-subjects [subjects]
  (->> subjects
       (map normalize-subject)
       (sort-by subject-sort-key)
       distinct
       vec))

(defn- canonical-input-views [input-views]
  (->> input-views
       (map (fn [input-view]
              (let [input-view-kind (get input-view "input_view_kind")]
                (when-not (contains? allowed-input-view-kinds input-view-kind)
                  (throw (ex-info "Invalid input view kind"
                                  {:input_view_kind input-view-kind
                                   :allowed_input_view_kinds allowed-input-view-kinds})))
                input-view)))
       (sort-by (juxt #(get % "input_view_kind")
                      #(get % "policy_hash")
                      #(get % "input_normalization_policy_hash")))
       distinct
       vec))

(defn assert-input-view-coverage!
  "Machine-checks the agreement between analysis-recipe
  `supported_input_view_kinds` and a request-set definition's `input_views`
  (ADR 0028 D6 follow-through).

  Consumer model:
  - Provided kinds are the declared input-view kinds, plus the derived
    \"token-stream-v1\" view when tokenizer profiles are present.
  - Direction A (recipe runnability): every referenced recipe must support at
    least one provided kind — otherwise the recipe could never run.
  - Direction B (no dead views): every declared view kind must have a
    consumer — a recipe that supports it, the tokenizer (consumes
    \"parser-ir-plaintext-body-v1\" when profiles are present), or the
    annotation materializer (\"parser-ir-body-annotations-v1\" is materialized
    directly from parser IR per ADR 0028, not recipe-mediated) — otherwise
    the view changes the request_set_id without affecting any output.

  Takes {:input-views ..., :recipes <raw recipe registry values>,
  :tokenizer-profile-ids ...}; returns nil, throws ex-info on violation."
  [{:keys [input-views recipes tokenizer-profile-ids]}]
  (let [view-kinds (into #{} (map #(get % "input_view_kind")) input-views)
        tokenized? (boolean (seq tokenizer-profile-ids))
        provided (cond-> view-kinds
                   tokenized? (conj "token-stream-v1"))]
    (doseq [recipe recipes]
      (let [supported (set (get recipe "supported_input_view_kinds"))]
        (when (empty? (set/intersection supported provided))
          (throw (ex-info "Analysis recipe cannot consume any provided input view"
                          {:recipe_id (get recipe "recipe_id")
                           :supported_input_view_kinds (vec (sort supported))
                           :provided_input_view_kinds (vec (sort provided))})))))
    (let [consumed (cond-> (into #{"parser-ir-body-annotations-v1"}
                                 (mapcat #(get % "supported_input_view_kinds"))
                                 recipes)
                     tokenized? (conj "parser-ir-plaintext-body-v1"))]
      (doseq [kind (sort view-kinds)]
        (when-not (contains? consumed kind)
          (throw (ex-info "Request-set input view has no consumer"
                          {:input_view_kind kind
                           :consumed_input_view_kinds (vec (sort consumed))})))))
    nil))

(defn- canonical-hashes [values]
  (->> values
       sort
       distinct
       vec))

(defn request-set-identity-object
  [{:keys [schema-hash corpus-snapshot-hash subjects input-views
           tokenizer-profile-hashes analysis-recipe-hashes missing-policy
           pack-policy-hash]}]
  (when-not (contains? allowed-missing-policies missing-policy)
    (throw (ex-info "Invalid missing policy"
                    {:missing_policy missing-policy})))
  {"schema_hash" schema-hash
   "corpus_snapshot_hash" corpus-snapshot-hash
   "subjects" (canonical-subjects subjects)
   "input_views" (canonical-input-views input-views)
   "tokenizer_profile_hashes" (canonical-hashes tokenizer-profile-hashes)
   "analysis_recipe_hashes" (canonical-hashes analysis-recipe-hashes)
   "missing_policy" missing-policy
   "pack_policy_hash" pack-policy-hash})

(defn request-set-id [request-set]
  (manifest/artifact-id (get request-set "request_set_identity_object")))

(defn resolved-recipe-label
  [{:keys [recipe-id analysis-recipe-hash registry-entry-hash resolved-at]}]
  {"recipe_id" recipe-id
   "analysis_recipe_hash" analysis-recipe-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})

(defn resolved-tokenizer-profile-label
  [{:keys [profile-id tokenizer-profile-hash registry-entry-hash resolved-at]}]
  {"profile_id" profile-id
   "tokenizer_profile_hash" tokenizer-profile-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})

(defn resolved-pack-policy-label
  [{:keys [policy-id pack-policy-hash registry-entry-hash resolved-at]}]
  {"policy_id" policy-id
   "pack_policy_hash" pack-policy-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})
