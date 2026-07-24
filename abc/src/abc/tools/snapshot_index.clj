(ns abc.tools.snapshot-index
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def snapshot-index-schema-id
  "https://w3id.org/abc/schemas/snapshot-index.schema.json")

(def snapshot-index-schema-version "0.2.0")

(def snapshot-index-schema-path "schemas/snapshot-index.schema.json")

(def manifest-schema-path "schemas/manifest.schema.json")

(def successful-statuses #{"passed" "warning"})

;; ── Governed value hashes ───────────────────────────────────────────────────

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(defn policy-hash [value]
  (hash-json-value value))

(defn- require-hash! [label value]
  (when-not (and (string? value) (re-matches hash/hash-pattern value))
    (throw (ex-info (str label " must be a sha256 hash")
                    {:label label :value value})))
  value)

(defn- require-nullable-hash! [label value]
  (when-not (or (nil? value)
                (and (string? value) (re-matches hash/hash-pattern value)))
    (throw (ex-info (str label " must be a sha256 hash or null")
                    {:label label :value value})))
  value)

(defn- canonical-hashes [label values]
  (when-not (vector? values)
    (throw (ex-info "snapshot_index_identity_object array fields must use empty arrays, not null"
                    {:field label :value values})))
  (->> values
       (map #(require-hash! label %))
       sort
       distinct
       vec))

;; ── Source selection identity ───────────────────────────────────────────────

(defn- source-sort-key [source]
  [(get source "work_id")
   (get source "person_id")
   (get source "text_zip_relpath")])

(defn canonical-source-selection [source-selection]
  (update source-selection "sources"
          (fn [sources]
            (->> (or sources [])
                 (sort-by source-sort-key)
                 vec))))

(defn source-selection-hash [source-selection]
  (hash-json-value (canonical-source-selection source-selection)))

;; ── Parser runtime identity ─────────────────────────────────────────────────

(defn parser-config-hash [parser-runtime-identity-object]
  (hash-json-value parser-runtime-identity-object))

;; ── Failure identity ────────────────────────────────────────────────────────

(defn- failure-identity-row [failure]
  {"stage" (get failure "stage")
   "work_slug" (get failure "work_slug")
   "code" (get failure "code")})

(defn- failure-sort-key [row]
  [(str (get row "stage"))
   (str (get row "work_slug"))
   (str (get row "code"))])

(defn failure-identity [failures]
  (->> (or failures [])
       (map failure-identity-row)
       (sort-by failure-sort-key)
       vec))

(defn failure-set-hash [failures]
  (hash-json-value (failure-identity failures)))

;; ── Artifact-reference identity (locator excluded) ──────────────────────────

(defn- artifact-reference-identity [reference]
  {"artifact_id" (require-hash! "artifact_id" (get reference "artifact_id"))
   "artifact_kind" (get reference "artifact_kind")
   "work_slug" (get reference "work_slug")
   "sidecar_role" (get reference "sidecar_role")
   "validation_status" (get reference "validation_status")
   "manifest_content_hash" (require-hash! "manifest_content_hash"
                                          (get reference "manifest_content_hash"))
   "content_hash" (when-let [content-hash (get reference "content_hash")]
                    (require-hash! "content_hash" content-hash))})

(defn- artifact-sort-key [reference]
  [(get reference "artifact_id")
   (get reference "artifact_kind")
   (or (get reference "work_slug") "")
   (or (get reference "sidecar_role") "")])

(defn canonical-artifact-references [references]
  (->> references
       (map artifact-reference-identity)
       (sort-by artifact-sort-key)
       distinct
       vec))

(defn sort-artifact-references [references]
  (->> references
       (sort-by artifact-sort-key)
       distinct
       vec))

(defn artifact-set-hash [artifact-references]
  (hash-json-value (canonical-artifact-references artifact-references)))

;; ── Summary ─────────────────────────────────────────────────────────────────

(defn snapshot-summary [artifact-references]
  (let [total-artifacts (count artifact-references)
        success-count (count (filter #(contains? successful-statuses
                                                 (get % "validation_status"))
                                     artifact-references))
        failure-count (- total-artifacts success-count)]
    {"total_artifacts" total-artifacts
     "success_count" success-count
     "failure_count" failure-count
     "failure_rate" (if (zero? total-artifacts)
                      0.0
                      (double (/ failure-count total-artifacts)))}))

;; ── Identity object + full index ────────────────────────────────────────────

(defn snapshot-index-identity-object
  [{:keys [snapshot-index-schema-hash source-selection parser-runtime-identity
           candidate-ref qualification-identity-ref
           artifact-references failures
           failure-policy layout-policy schema-hashes]}]
  {"snapshot_index_schema_hash" (require-hash! "snapshot_index_schema_hash"
                                               snapshot-index-schema-hash)
   "source_selection_hash" (source-selection-hash source-selection)
   "candidate_ref" (require-nullable-hash! "candidate_ref" candidate-ref)
   "qualification_identity_ref" (require-nullable-hash! "qualification_identity_ref"
                                                        qualification-identity-ref)
   "parser_config_hash" (parser-config-hash parser-runtime-identity)
   "artifact_set_hash" (artifact-set-hash artifact-references)
   "failure_set_hash" (failure-set-hash failures)
   "failure_policy_hash" (policy-hash failure-policy)
   "layout_policy_hash" (policy-hash layout-policy)
   "schema_hashes" (canonical-hashes "schema_hashes" schema-hashes)})

(defn build-snapshot-index
  [{:keys [snapshot-date generated-at source-selection parser-runtime-identity
           candidate-ref qualification-identity-ref
           failure-policy layout-policy schema-hashes failures
           artifact-references]}]
  (let [snapshot-index-schema-hash (manifest/schema-hash snapshot-index-schema-path)
        source-selection (canonical-source-selection source-selection)
        artifact-references (sort-artifact-references artifact-references)
        failures (vec (or failures []))
        identity-object (snapshot-index-identity-object
                         {:snapshot-index-schema-hash snapshot-index-schema-hash
                          :source-selection source-selection
                          :parser-runtime-identity parser-runtime-identity
                          :candidate-ref candidate-ref
                          :qualification-identity-ref qualification-identity-ref
                          :artifact-references artifact-references
                          :failures failures
                          :failure-policy failure-policy
                          :layout-policy layout-policy
                          :schema-hashes schema-hashes})]
    {"schema_id" snapshot-index-schema-id
     "schema_version" snapshot-index-schema-version
     "schema_hash" snapshot-index-schema-hash
     "snapshot_date" snapshot-date
     "generated_at" generated-at
     "snapshot_identity_hash" (manifest/artifact-id identity-object)
     "snapshot_index_identity_object" identity-object
     "source_selection_identity_object" source-selection
     "parser_runtime_identity_object" parser-runtime-identity
     "failure_policy" failure-policy
     "layout_policy" layout-policy
     "failures" failures
     "artifact_references" artifact-references
     "summary" (snapshot-summary artifact-references)}))

(defn write-snapshot-index! [snapshot-index output-path]
  (let [output-file (fs/file output-path)]
    (when-let [parent (fs/parent output-path)]
      (fs/create-dirs parent))
    (manifest/write-json-file! output-file snapshot-index)
    output-file))

(defn snapshot-identity-hash [snapshot-index-or-identity-object]
  (manifest/artifact-id
   (or (get snapshot-index-or-identity-object "snapshot_index_identity_object")
       snapshot-index-or-identity-object)))

;; ── Value validation ────────────────────────────────────────────────────────

(defn- compare-hash! [label expected actual]
  (when-not (= expected actual)
    (throw (ex-info (str label " mismatch")
                    {:label label :expected expected :actual actual}))))

(defn validate-snapshot-index-identity-object! [identity-object]
  (doseq [field ["snapshot_index_schema_hash" "source_selection_hash"
                 "parser_config_hash" "artifact_set_hash" "failure_set_hash"
                 "failure_policy_hash" "layout_policy_hash"]]
    (require-hash! field (get identity-object field)))
  (doseq [field ["candidate_ref" "qualification_identity_ref"]]
    (require-nullable-hash! field (get identity-object field)))
  (canonical-hashes "schema_hashes" (get identity-object "schema_hashes"))
  true)

(defn validate-snapshot-index! [snapshot-index]
  (when-not (= snapshot-index-schema-version (get snapshot-index "schema_version"))
    (throw (ex-info "Unsupported snapshot index schema_version"
                    {:schema_version (get snapshot-index "schema_version")})))
  (let [identity-object (get snapshot-index "snapshot_index_identity_object")]
    (validate-snapshot-index-identity-object! identity-object)
    (compare-hash! "snapshot_identity_hash"
                   (snapshot-identity-hash identity-object)
                   (get snapshot-index "snapshot_identity_hash"))
    (compare-hash! "source_selection_hash"
                   (source-selection-hash
                    (get snapshot-index "source_selection_identity_object"))
                   (get identity-object "source_selection_hash"))
    (compare-hash! "parser_config_hash"
                   (parser-config-hash
                    (get snapshot-index "parser_runtime_identity_object"))
                   (get identity-object "parser_config_hash"))
    (compare-hash! "artifact_set_hash"
                   (artifact-set-hash (get snapshot-index "artifact_references"))
                   (get identity-object "artifact_set_hash"))
    (compare-hash! "failure_set_hash"
                   (failure-set-hash (get snapshot-index "failures"))
                   (get identity-object "failure_set_hash"))
    (compare-hash! "failure_policy_hash"
                   (policy-hash (get snapshot-index "failure_policy"))
                   (get identity-object "failure_policy_hash"))
    (compare-hash! "layout_policy_hash"
                   (policy-hash (get snapshot-index "layout_policy"))
                   (get identity-object "layout_policy_hash")))
  true)

;; ── Closed-reference verification ───────────────────────────────────────────
;;
;; `closure-problems` proves a completed publication root closes over exactly
;; the artifact references its index names: relative, root-contained locators;
;; manifests whose bytes rehash to their declared manifest_content_hash and
;; validate against the manifest schema; content and sidecar bytes that rehash
;; to the hashes their manifest records; reference fields that agree with the
;; manifest; and referenced per-work directories with no unreferenced regular
;; files. Only the derived publications/publications-report.json is exempt.
;; It reads bytes but never renders, repairs, or mutates; it returns a vector
;; of problem maps with :code and :message plus optional :path/:expected/:actual.

(defn- problem [code message extra]
  (merge {:code code :message message} extra))

(defn- normalized-relative-locator
  "Return the normalized relative locator path when it is relative and stays
  under the root, otherwise nil."
  [locator-path]
  (when (string? locator-path)
    (let [normalized (str (fs/normalize locator-path))]
      (when (and (not (fs/absolute? locator-path))
                 (not (= ".." normalized))
                 (not (string/starts-with? normalized "../")))
        normalized))))

(defn- canonical-path [file]
  (str (fs/normalize (fs/absolutize file))))

(defn- sidecar-files [manifest-dir manifest-value]
  (for [sidecar (get manifest-value "sidecars" [])
        :let [path-hint (get sidecar "path_hint")]
        :when path-hint]
    {:sidecar sidecar
     :file (io/file manifest-dir path-hint)}))

(defn- content-file [manifest-dir manifest-value]
  (when-let [path-hint (get-in manifest-value ["content" "path_hint"])]
    (io/file manifest-dir path-hint)))

(defn- reference-file-problems
  "Verify a single loose reference against its manifest, content, and sidecars.
  Returns {:problems [...] :expected-files #{canonical-path...}
           :scan-dirs #{canonical-dir...}}."
  [root manifest-schema reference]
  (let [locator (get reference "locator")
        locator-kind (get locator "kind")]
    (if (not= "loose" locator-kind)
      {:problems [(problem "closure-locator-unsupported"
                           "Closed-reference verification requires loose locators"
                           {:actual locator-kind})]
       :expected-files #{} :scan-dirs #{}}
      (let [locator-path (normalized-relative-locator (get locator "path"))]
        (if (nil? locator-path)
          {:problems [(problem "closure-locator-escapes-root"
                               "Artifact locator must be a relative path contained by the root"
                               {:actual (get locator "path")})]
           :expected-files #{} :scan-dirs #{}}
          (let [manifest-file (io/file root locator-path)]
            (if-not (fs/regular-file? manifest-file)
              {:problems [(problem "closure-manifest-missing"
                                   "Referenced manifest does not exist under the root"
                                   {:path locator-path})]
               :expected-files #{} :scan-dirs #{}}
              (let [manifest-dir (.getParentFile ^java.io.File manifest-file)
                    actual-manifest-hash (manifest/file-hash manifest-file)
                    manifest-value (files/read-json manifest-file)
                    content-f (content-file manifest-dir manifest-value)
                    sidecars (sidecar-files manifest-dir manifest-value)
                    schema-errors (schema/validation-errors manifest-schema
                                                            manifest-value)
                    problems
                    (vec
                     (concat
                      (when (not= (get reference "manifest_content_hash")
                                  actual-manifest-hash)
                        [(problem "closure-manifest-content-hash-mismatch"
                                  "Referenced manifest bytes do not match manifest_content_hash"
                                  {:path locator-path
                                   :expected (get reference "manifest_content_hash")
                                   :actual actual-manifest-hash})])
                      (when schema-errors
                        [(problem "closure-manifest-schema-invalid"
                                  "Referenced manifest does not validate against the manifest schema"
                                  {:path locator-path})])
                      (for [[field ref-field manifest-field]
                            [["artifact_id" (get reference "artifact_id")
                              (get manifest-value "artifact_id")]
                             ["artifact_kind" (get reference "artifact_kind")
                              (get manifest-value "artifact_kind")]
                             ["validation_status" (get reference "validation_status")
                              (get manifest-value "validation_status")]
                             ["content_hash" (get reference "content_hash")
                              (get-in manifest-value ["content" "content_hash"])]]
                            :when (not= ref-field manifest-field)]
                        (problem "closure-reference-field-mismatch"
                                 (str "Reference " field
                                      " disagrees with its manifest")
                                 {:path locator-path
                                  :expected ref-field
                                  :actual manifest-field}))
                      (when (and content-f (fs/regular-file? content-f))
                        (let [actual (str "sha256:" (files/sha256-file content-f))]
                          (when (not= (get-in manifest-value ["content" "content_hash"])
                                      actual)
                            [(problem "closure-content-hash-mismatch"
                                      "Content bytes do not match the manifest content_hash"
                                      {:path locator-path
                                       :expected (get-in manifest-value
                                                         ["content" "content_hash"])
                                       :actual actual})])))
                      (when (and content-f (not (fs/regular-file? content-f)))
                        [(problem "closure-content-missing"
                                  "Manifest content file does not exist"
                                  {:path locator-path})])
                      (mapcat
                       (fn [{:keys [sidecar file]}]
                         (cond
                           (not (fs/regular-file? file))
                           [(problem "closure-sidecar-missing"
                                     "Manifest sidecar file does not exist"
                                     {:path locator-path
                                      :actual (get sidecar "path_hint")})]
                           :else
                           (let [actual (str "sha256:" (files/sha256-file file))]
                             (when (not= (get sidecar "hash") actual)
                               [(problem "closure-sidecar-hash-mismatch"
                                         "Sidecar bytes do not match the manifest sidecar hash"
                                         {:path locator-path
                                          :expected (get sidecar "hash")
                                          :actual actual})]))))
                       sidecars)))]
                {:problems problems
                 :expected-files (into #{(canonical-path manifest-file)}
                                       (concat
                                        (when content-f [(canonical-path content-f)])
                                        (map (comp canonical-path :file) sidecars)))
                 :scan-dirs #{(canonical-path manifest-dir)}}))))))))

(defn- unreferenced-file-problems
  [root scan-dirs expected-files]
  (let [allowed (conj expected-files
                      (canonical-path (io/file root "publications"
                                               "publications-report.json")))]
    (vec
     (for [dir scan-dirs
           :let [dir-file (io/file dir)]
           :when (fs/directory? dir-file)
           child (sort (map str (fs/list-dir dir-file)))
           :when (fs/regular-file? child)
           :let [canonical (canonical-path child)]
           :when (not (contains? allowed canonical))]
       (problem "closure-unreferenced-file"
                "Referenced per-work directory contains an unreferenced regular file"
                {:path (str (fs/relativize (canonical-path root) canonical))})))))

(defn closure-problems [root index]
  (let [root (io/file root)
        manifest-schema (files/read-json manifest-schema-path)
        per-reference (mapv #(reference-file-problems root manifest-schema %)
                            (get index "artifact_references" []))
        reference-problems (vec (mapcat :problems per-reference))
        expected-files (reduce into #{} (map :expected-files per-reference))
        scan-dirs (reduce into #{} (map :scan-dirs per-reference))]
    (vec (concat reference-problems
                 (unreferenced-file-problems root scan-dirs expected-files)))))

(defn assert-closed-root!
  "Throw when the completed root does not close over its index references.
  Returns true on a clean closure."
  [root index]
  (let [problems (closure-problems root index)]
    (when (seq problems)
      (throw (ex-info "Snapshot root does not close over its artifact references"
                      {:problems problems})))
    true))

;; ── File-loading boundary (single reader for snapshot indexes) ──────────────

(defn snapshot-index-path
  "Resolve a snapshot-index locator: a directory yields its snapshot-index.json
  (or index.json for a staged root), a file yields itself."
  [path]
  (let [file (io/file path)]
    (if (fs/directory? file)
      (let [snapshot-index-file (io/file file "snapshot-index.json")]
        (if (fs/regular-file? snapshot-index-file)
          snapshot-index-file
          (io/file file "index.json")))
      file)))

(defn read-valid-snapshot-index
  "The single file-loading boundary for snapshot indexes: read the JSON, check
  it against the JSON Schema, then check internal identity coherence."
  [path]
  (let [snapshot-path (snapshot-index-path path)
        snapshot (files/read-json snapshot-path)
        snapshot-schema (files/read-json snapshot-index-schema-path)]
    (when-let [errors (schema/validation-errors snapshot-schema snapshot)]
      (throw (ex-info "Snapshot index schema validation failed"
                      {:path (str snapshot-path) :errors errors})))
    (validate-snapshot-index! snapshot)
    snapshot))
