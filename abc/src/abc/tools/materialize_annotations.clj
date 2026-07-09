(ns abc.tools.materialize-annotations
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def activity-id "https://w3id.org/abc/activity/materialize-annotations")
(def tool-agent "abc.tools.materialize-annotations")

(def ^:private copied-parser-ir-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(def ^:private coordinate-system "unicode-scalar-value-plaintext-spans")
(def ^:private input-view-kind "parser-ir-plaintext-body-v1")

(defn annotation-identity-object
  [{:keys [producer-manifest annotation-policy]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" (get producer-identity "metadata_record_hash")
      "tei_profile_hash" nil
      "tokenizer_build_hash" nil
      "tokenizer_dictionary_hash" nil
      "tokenizer_profile_hash" nil
      "analysis_recipe_hash" nil
      "annotation_policy_hash" (analysis-identity/annotation-policy-hash annotation-policy)
      "output_format_spec_hash" (manifest/schema-hash "schemas/annotation-output.schema.json")}
     (select-keys producer-identity copied-parser-ir-fields))))

(defn annotation-value
  [{:keys [producer-manifest annotation-policy input-plaintext-policy-hash
           annotations warnings]}]
  {"schema_id" "https://w3id.org/abc/schemas/annotation-output.schema.json"
   "schema_hash" (manifest/schema-hash "schemas/annotation-output.schema.json")
   "annotation_policy_hash" (analysis-identity/annotation-policy-hash annotation-policy)
   "producer" {"input_view_kind" input-view-kind
               "artifact_id" (get producer-manifest "artifact_id")
               "content_hash" (get-in producer-manifest ["content" "content_hash"])}
   "input_plaintext_policy_hash" input-plaintext-policy-hash
   "coordinate_system" coordinate-system
   "annotations" (vec annotations)
   "warnings" (vec (or warnings []))})

(defn- sidecar [file]
  {"role" "body-annotations"
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" "application/json"
   "path_hint" "body-annotations.json"})

(defn annotation-manifest
  [{:keys [producer-manifest annotation-policy input-plaintext-policy-hash
           annotations-file generated-at]}]
  (let [identity-object (annotation-identity-object
                         {:producer-manifest producer-manifest
                          :annotation-policy annotation-policy})
        producer-artifact-id (get producer-manifest "artifact_id")
        policy-hash (analysis-identity/annotation-policy-hash annotation-policy)]
    (manifest/artifact-manifest
     {:artifact-kind "annotation"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content annotations-file
                                 "application/json"
                                 "body-annotations.json"
                                 files/sha256-file)
      :sidecars [(sidecar annotations-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent tool-agent
      :plan-hash nil
      :used [producer-artifact-id
             (get-in producer-manifest ["content" "content_hash"])
             policy-hash
             input-plaintext-policy-hash]
      :was-derived-from [producer-artifact-id]
      :notes "Ruby/gaiji annotation view aligned to parser-ir-plaintext-body-v1."})))

(defn materialize-annotations!
  [{:keys [producer-manifest parser-ir annotation-policy
           input-plaintext-policy-hash output-dir generated-at]}]
  (let [output-dir (io/file output-dir)
        annotations-file (io/file output-dir "body-annotations.json")
        manifest-file (io/file output-dir "annotation.manifest.json")
        {:keys [annotations]} (plaintext/render-with-annotations parser-ir)]
    (manifest/write-json-file! annotations-file
                               (annotation-value
                                {:producer-manifest producer-manifest
                                 :annotation-policy annotation-policy
                                 :input-plaintext-policy-hash input-plaintext-policy-hash
                                 :annotations annotations}))
    (manifest/write-json-file! manifest-file
                               (annotation-manifest
                                {:producer-manifest producer-manifest
                                 :annotation-policy annotation-policy
                                 :input-plaintext-policy-hash input-plaintext-policy-hash
                                 :annotations-file annotations-file
                                 :generated-at generated-at}))
    {:annotations annotations-file
     :manifest manifest-file}))

(def annotation-input-view-kind "parser-ir-body-annotations-v1")

(defn- registry-policies [registry-dir]
  (->> (.listFiles (io/file registry-dir))
       (filter #(and (.isFile %) (string/ends-with? (.getName %) ".json")))
       (sort-by #(.getName %))
       (mapv (fn [f] {:path (str f)
                      :policy (files/read-json f)}))))

(defn- resolve-policy-by-hash [registry-dir policy-hash]
  (let [matches (filterv #(= policy-hash
                             (analysis-identity/annotation-policy-hash
                              (:policy %)))
                         (registry-policies registry-dir))]
    (case (count matches)
      0 (throw (ex-info "Unknown annotation policy hash"
                        {:policy_hash policy-hash
                         :registry_dir registry-dir}))
      1 (:policy (first matches))
      (throw (ex-info "Duplicate annotation policy hash in registry"
                      {:policy_hash policy-hash
                       :paths (mapv :path matches)})))))

(defn- aligned-plaintext-policy-hash [input-views policy]
  (let [aligns-to (get policy "aligns_to")
        aligned (filterv #(= aligns-to (get % "input_view_kind")) input-views)]
    (when-not (= 1 (count aligned))
      (throw (ex-info "Annotation view requires exactly one aligned input view"
                      {:aligns_to aligns-to
                       :aligned_view_count (count aligned)})))
    (get (first aligned) "policy_hash")))

(defn resolve-annotation-materialization
  "Resolves a request set's annotation input view to the concrete
  materialization inputs, ONCE per run (spec A1-A3). Returns nil when the
  request set declares no annotation view; otherwise
  {:view <input-view> :policy <registry value>
   :input-plaintext-policy-hash <the aligned view's policy_hash>}.
  Fails closed on: unknown/duplicate policy hash in the registry, more than
  one annotation view (v1 layout is single-view), and anything but exactly
  one input view of the policy's aligns_to kind."
  [{:keys [request-set registry-dir]}]
  (let [input-views (get-in request-set
                            ["request_set_identity_object" "input_views"])
        annotation-views (filterv #(= annotation-input-view-kind
                                      (get % "input_view_kind"))
                                  input-views)]
    (when (seq annotation-views)
      (when (> (count annotation-views) 1)
        (throw (ex-info "Multiple annotation input views are not supported"
                        {:label (get request-set "label")
                         :annotation_view_count (count annotation-views)})))
      (let [view (first annotation-views)
            policy (resolve-policy-by-hash registry-dir
                                           (get view "policy_hash"))]
        {:view view
         :policy policy
         :input-plaintext-policy-hash (aligned-plaintext-policy-hash
                                       input-views policy)}))))
