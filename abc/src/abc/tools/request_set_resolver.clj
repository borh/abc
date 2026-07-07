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

(def resolver-id
  "abc.tools.request-set-resolver/v1")

(def default-resolved-at
  "2026-07-07T00:00:00Z")

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

(defn- analysis-recipe-path [recipe-id]
  (str analysis-recipes-dir "/" recipe-id ".json"))

(defn- read-analysis-recipe [recipe-id]
  (let [path (analysis-recipe-path recipe-id)
        file (io/file path)]
    (when-not (.isFile file)
      (throw (ex-info "Unknown analysis recipe"
                      {:recipe_id recipe-id
                       :path path})))
    (files/read-json path)))

(defn- recipe-registry-entry [recipe-id recipe-hash resolved-at]
  {"semantic_id" recipe-id
   "content_hash" recipe-hash
   "valid_from" resolved-at})

(defn- resolve-analysis-recipe [recipe-id resolved-at]
  (let [recipe (read-analysis-recipe recipe-id)
        recipe-hash (analysis-identity/analysis-recipe-hash recipe)
        registry-entry-hash (analysis-identity/hash-json-value
                             (recipe-registry-entry recipe-id
                                                    recipe-hash
                                                    resolved-at))]
    {:hash recipe-hash
     :label (analysis-identity/resolved-recipe-label
             {:recipe-id recipe-id
              :analysis-recipe-hash recipe-hash
              :registry-entry-hash registry-entry-hash
              :resolved-at resolved-at})}))

(defn- resolved-tokenizer-profile-labels [definition]
  (let [profile-ids (get definition "tokenizer_profile_ids" [])]
    (when (seq profile-ids)
      (throw (ex-info "Tokenizer profile resolution is blocked until tokenizer_profile_hash is in manifest identity"
                      {:tokenizer_profile_ids profile-ids})))
    []))

(defn resolve-request-set
  ([label]
   (resolve-request-set label {:resolved-at default-resolved-at}))
  ([label {:keys [resolved-at]
           :or {resolved-at default-resolved-at}}]
   (let [definition (read-request-set-definition label)
         definition-label (get definition "label")
         resolved-recipes (mapv #(resolve-analysis-recipe % resolved-at)
                                (get definition "analysis_recipe_ids" []))
         tokenizer-labels (resolved-tokenizer-profile-labels definition)
         identity-object (analysis-identity/request-set-identity-object
                          {:schema-hash (manifest/schema-hash request-set-schema-path)
                           :corpus-snapshot-hash (get definition "corpus_snapshot_hash")
                           :subjects (get definition "subjects")
                           :input-views (get definition "input_views")
                           :tokenizer-profile-hashes []
                           :analysis-recipe-hashes (mapv :hash resolved-recipes)
                           :missing-policy (get definition "missing_policy")
                           :pack-policy-hash (analysis-identity/hash-json-value
                                              (get definition "pack_policy"))})
         request-set {"schema_id" request-set-schema-id
                      "schema_hash" (manifest/schema-hash request-set-schema-path)
                      "request_set_schema_version" request-set-schema-version
                      "label" definition-label
                      "request_set_identity_object" identity-object
                      "resolved_recipe_labels" (mapv :label resolved-recipes)
                      "resolved_tokenizer_profile_labels" tokenizer-labels
                      "resolution" {"source_definition_path" (request-set-definition-path label)
                                    "resolved_at" resolved-at
                                    "resolver_id" resolver-id}}]
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
