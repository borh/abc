(ns abc.tools.request-set-resolver
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def request-set-schema-id
  "https://w3id.org/abc/schemas/request-set.schema.json")

(def request-set-schema-path
  "schemas/request-set.schema.json")

(def request-set-schema-version
  "request-set-v1")

(def definitions-dir
  "data/request-set-definitions")

(def analysis-recipes-dir
  "data/analysis-recipes")

(def tokenizer-profiles-dir
  "data/tokenizer-profiles")

(def pack-policies-dir
  "data/pack-policies")

(def resolver-id
  "abc.tools.request-set-resolver/v1")

(def default-resolved-at
  "2026-07-07T00:00:00Z")

(def source-snapshot-kind
  "source-corpus-snapshot-v0")

(def source-snapshot-workset-file-name
  "source-snapshot.workset.edn")

(defn- json-file? [file]
  (and (.isFile file)
       (string/ends-with? (.getName file) ".json")))

(defn request-set-labels []
  (let [dir (io/file definitions-dir)]
    (->> (file-seq dir)
         (filter json-file?)
         (map #(.getName %))
         (map #(subs % 0 (- (count %) (count ".json"))))
         sort
         vec)))

(defn request-set-definition-path [label]
  (str definitions-dir "/" label ".json"))

(defn read-request-set-definition [label]
  (let [path (request-set-definition-path label)
        file (io/file path)]
    (when-not (.isFile file)
      (throw (ex-info "Unknown request set"
                      {:label label
                       :path path})))
    (files/read-json path)))

(defn- registry-value-path [dir semantic-id]
  (str dir "/" semantic-id ".json"))

(defn- read-registry-value
  [{:keys [dir value-kind semantic-key semantic-id]}]
  (let [path (registry-value-path dir semantic-id)
        file (io/file path)]
    (when-not (.isFile file)
      (throw (ex-info (str "Unknown " value-kind)
                      {semantic-key semantic-id
                       :path path})))
    (files/read-json path)))

(defn- semantic-registry-entry [semantic-id content-hash resolved-at]
  {"semantic_id" semantic-id
   "content_hash" content-hash
   "valid_from" resolved-at})

(defn- resolve-semantic-registry-value
  [{:keys [semantic-id resolved-at hash-fn label-fn] :as opts}]
  (let [value (read-registry-value opts)
        value-hash (hash-fn value)
        registry-entry-hash (analysis-identity/hash-json-value
                             (semantic-registry-entry semantic-id
                                                      value-hash
                                                      resolved-at))]
    {:hash value-hash
     :label (label-fn (assoc opts
                             :content-hash value-hash
                             :registry-entry-hash registry-entry-hash))}))

(defn- resolve-analysis-recipe [recipe-id resolved-at]
  (resolve-semantic-registry-value
   {:dir analysis-recipes-dir
    :value-kind "analysis recipe"
    :semantic-key :recipe_id
    :semantic-id recipe-id
    :resolved-at resolved-at
    :hash-fn analysis-identity/analysis-recipe-hash
    :label-fn (fn [{:keys [semantic-id content-hash
                           registry-entry-hash resolved-at]}]
                (analysis-identity/resolved-recipe-label
                 {:recipe-id semantic-id
                  :analysis-recipe-hash content-hash
                  :registry-entry-hash registry-entry-hash
                  :resolved-at resolved-at}))}))

(defn- resolve-tokenizer-profile [profile-id resolved-at]
  (resolve-semantic-registry-value
   {:dir tokenizer-profiles-dir
    :value-kind "tokenizer profile"
    :semantic-key :profile_id
    :semantic-id profile-id
    :resolved-at resolved-at
    :hash-fn analysis-identity/tokenizer-profile-hash
    :label-fn (fn [{:keys [semantic-id content-hash
                           registry-entry-hash resolved-at]}]
                (analysis-identity/resolved-tokenizer-profile-label
                 {:profile-id semantic-id
                  :tokenizer-profile-hash content-hash
                  :registry-entry-hash registry-entry-hash
                  :resolved-at resolved-at}))}))

(defn- resolve-pack-policy [policy-id resolved-at]
  (resolve-semantic-registry-value
   {:dir pack-policies-dir
    :value-kind "pack policy"
    :semantic-key :policy_id
    :semantic-id policy-id
    :resolved-at resolved-at
    :hash-fn analysis-identity/pack-policy-hash
    :label-fn (fn [{:keys [semantic-id content-hash
                           registry-entry-hash resolved-at]}]
                (analysis-identity/resolved-pack-policy-label
                 {:policy-id semantic-id
                  :pack-policy-hash content-hash
                  :registry-entry-hash registry-entry-hash
                  :resolved-at resolved-at}))}))

(defn- resolved-tokenizer-profile-labels [definition resolved-at]
  (mapv #(resolve-tokenizer-profile % resolved-at)
        (get definition "tokenizer_profile_ids" [])))

(defn- required-string [label value context]
  (when (or (not (string? value)) (string/blank? value))
    (throw (ex-info (str label " must be a non-empty string")
                    (assoc context
                           :label label
                           :value value))))
  value)

(defn- source-snapshot-hash [snapshot path]
  (let [identity-object (get snapshot "snapshot_identity_object")
        declared-hash (get snapshot "snapshot_hash")
        expected-hash (analysis-identity/hash-json-value identity-object)]
    (when-not (= declared-hash expected-hash)
      (throw (ex-info "Source snapshot hash mismatch"
                      {:path path
                       :declared_hash declared-hash
                       :expected_hash expected-hash})))
    declared-hash))

(defn- source-snapshot-subject [subject-source input]
  (let [work-id (required-string "work_id" (get input "work_id")
                                 {:subject_source subject-source
                                  :snapshot_input input})
        source-id-prefix (get subject-source "source_id_prefix" "")
        work-id-prefix (get subject-source "work_id_prefix" "")
        work-content-hash (required-string "work_content_hash"
                                           (get input "work_content_hash")
                                           {:subject_source subject-source
                                            :snapshot_input input})]
    {"source_id" (or (get input "source_id")
                     (str source-id-prefix work-id))
     "work_id" (or (get input "subject_work_id")
                   (str work-id-prefix work-id))
     "work_content_hash" work-content-hash
     "metadata_record_hash" (get input "metadata_record_hash")}))

(defn- resolve-source-snapshot-subjects [subject-source]
  (let [kind (get subject-source "kind")
        path (required-string "subject_source.path"
                              (get subject-source "path")
                              {:subject_source subject-source})]
    (when-not (= source-snapshot-kind kind)
      (throw (ex-info "Unsupported request-set subject source kind"
                      {:kind kind
                       :allowed_kinds [source-snapshot-kind]})))
    (let [snapshot (files/read-json path)
          snapshot-hash (source-snapshot-hash snapshot path)
          inputs (get-in snapshot ["snapshot_identity_object" "snapshot_inputs"])]
      (when-not (seq inputs)
        (throw (ex-info "Source snapshot has no snapshot_inputs"
                        {:path path})))
      {:corpus-snapshot-hash snapshot-hash
       :subjects (mapv #(source-snapshot-subject subject-source %)
                       inputs)})))

(defn- definition-with-source-snapshot-path [definition subject-source-path]
  (if subject-source-path
    (if (get definition "subject_source")
      (assoc-in definition ["subject_source" "path"] subject-source-path)
      (throw (ex-info "Source snapshot path override requires a subject_source definition"
                      {:label (get definition "label")
                       :subject_source_path subject-source-path})))
    definition))

(defn- sibling-workset-path [subject-source-path]
  (when-let [parent (some-> subject-source-path io/file .getParentFile)]
    (str (io/file parent source-snapshot-workset-file-name))))

(defn- resolve-subject-coordinate [definition]
  (let [inline-subjects (get definition "subjects")
        subject-source (get definition "subject_source")]
    (cond
      (and (seq inline-subjects) subject-source)
      (throw (ex-info "Request-set definition must not define both subjects and subject_source"
                      {:label (get definition "label")}))

      subject-source
      (let [{:keys [corpus-snapshot-hash subjects]}
            (resolve-source-snapshot-subjects subject-source)
            declared-hash (get definition "corpus_snapshot_hash")]
        (when (and declared-hash (not= declared-hash corpus-snapshot-hash))
          (throw (ex-info "Request-set definition corpus_snapshot_hash does not match subject source"
                          {:label (get definition "label")
                           :declared_hash declared-hash
                           :subject_source_hash corpus-snapshot-hash})))
        {:corpus-snapshot-hash corpus-snapshot-hash
         :subjects subjects})

      (seq inline-subjects)
      {:corpus-snapshot-hash (required-string "corpus_snapshot_hash"
                                              (get definition "corpus_snapshot_hash")
                                              {:label (get definition "label")})
       :subjects inline-subjects}

      :else
      (throw (ex-info "Request-set definition must define subjects or subject_source"
                      {:label (get definition "label")})))))

(defn resolve-request-set
  ([label]
   (resolve-request-set label {:resolved-at default-resolved-at}))
  ([label {:keys [resolved-at subject-source-path]
           :or {resolved-at default-resolved-at}}]
   (let [definition (definition-with-source-snapshot-path
                      (read-request-set-definition label)
                      subject-source-path)
         definition-label (get definition "label")
         subject-coordinate (resolve-subject-coordinate definition)
         resolved-recipes (mapv #(resolve-analysis-recipe % resolved-at)
                                (get definition "analysis_recipe_ids" []))
         resolved-tokenizer-profiles (resolved-tokenizer-profile-labels
                                      definition
                                      resolved-at)
         resolved-pack-policy (resolve-pack-policy
                               (required-string "pack_policy_id"
                                                (get definition "pack_policy_id")
                                                {:label definition-label})
                               resolved-at)
         identity-object (analysis-identity/request-set-identity-object
                          {:schema-hash (manifest/schema-hash request-set-schema-path)
                           :corpus-snapshot-hash (:corpus-snapshot-hash
                                                  subject-coordinate)
                           :subjects (:subjects subject-coordinate)
                           :input-views (get definition "input_views")
                           :tokenizer-profile-hashes (mapv :hash
                                                           resolved-tokenizer-profiles)
                           :analysis-recipe-hashes (mapv :hash resolved-recipes)
                           :missing-policy (get definition "missing_policy")
                           :pack-policy-hash (:hash resolved-pack-policy)})
         source-workset-path (sibling-workset-path subject-source-path)
         request-set {"schema_id" request-set-schema-id
                      "schema_hash" (manifest/schema-hash request-set-schema-path)
                      "request_set_schema_version" request-set-schema-version
                      "label" definition-label
                      "request_set_identity_object" identity-object
                      "resolved_recipe_labels" (mapv :label resolved-recipes)
                      "resolved_tokenizer_profile_labels" (mapv :label
                                                                resolved-tokenizer-profiles)
                      "resolved_pack_policy_label" (:label resolved-pack-policy)
                      "resolution" (cond-> {"source_definition_path" (request-set-definition-path label)
                                            "resolved_at" resolved-at
                                            "resolver_id" resolver-id}
                                     subject-source-path
                                     (assoc "subject_source_path"
                                            subject-source-path)

                                     source-workset-path
                                     (assoc "source_snapshot_workset_path"
                                            source-workset-path))}]
     (when-not (= label definition-label)
       (throw (ex-info "Request-set definition label does not match path label"
                       {:path_label label
                        :definition_label definition-label})))
     (assoc request-set
            "request_set_id" (analysis-identity/request-set-id request-set)))))

(defn write-resolved-request-set! [label output-dir]
  (let [request-set (resolve-request-set label)
        output-path (io/file output-dir (str label ".json"))]
    (manifest/write-json-file! output-path request-set)
    output-path))
