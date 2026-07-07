(ns abc.tools.materialize-source-snapshot
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [taoensso.telemere :as tel]))

(def default-generated-at "2026-07-04T00:00:00Z")
(def snapshot-schema-id "https://w3id.org/abc/source-corpus-snapshot-v0.json")
(def snapshot-hash-algorithm "sha256-rfc8785-jcs-v0")

(defn- map-value [m k]
  (or (get m k)
      (get m (name k))))

(defn- work-value [work k]
  (map-value work k))

(defn- required-work-value [work k]
  (or (work-value work k)
      (throw (ex-info (str "workset entry missing required key " k)
                      {:key k :work work}))))

(defn- read-workset [path]
  (let [value (edn/read-string (slurp (io/file path)))]
    (when-not (seq (map-value value :works))
      (throw (ex-info "workset must contain non-empty :works"
                      {:workset-path path})))
    value))

(defn- relative-path [from-file to-file]
  (let [from-parent (.getCanonicalFile (.getParentFile (io/file from-file)))
        to (.getCanonicalFile (io/file to-file))]
    (-> (str (.relativize (.toPath from-parent) (.toPath to)))
        (string/replace "\\" "/"))))

(defn- compact-hashes [& values]
  (vec (keep identity values)))

(defn- source-media-type [encoding]
  (if (seq encoding)
    (str "text/plain; charset=" encoding)
    "text/plain"))

(defn- snapshot-input [work]
  (let [aat-path (required-work-value work :aat_path)
        parser-ir-path (required-work-value work :parser_ir_path)
        metadata-record-path (required-work-value work :metadata_record_path)
        parser-ir (files/read-json parser-ir-path)
        metadata-record (files/read-json metadata-record-path)]
    {"slug" (required-work-value work :slug)
     "title" (required-work-value work :title)
     "work_id" (required-work-value work :work_id)
     "person_id" (required-work-value work :person_id)
     "card_url" (or (work-value work :card_url)
                    (get-in metadata-record ["work" "card_url"]))
     "aat_path" aat-path
     "aat_file_hash" (manifest/file-hash aat-path)
     "aat_adapter" (get-in parser-ir ["derived_from" "aat_adapter"])
     "aat_adapter_version" (get-in parser-ir ["derived_from" "aat_adapter_version"])
     "aat_version" (get-in parser-ir ["derived_from" "aat_version"])
     "mapping_id" (get-in parser-ir ["derived_from" "mapping_id"])
     "mapping_schema_hash" (get-in parser-ir ["derived_from" "mapping_schema_hash"])
     "mapping_version" (get-in parser-ir ["derived_from" "mapping_version"])
     "parser_ir_schema_hash" (get parser-ir "schema_hash")
     "work_content_hash" (get-in parser-ir ["source" "work_content_hash"])
     "source_encoding" (get-in parser-ir ["source" "encoding"])
     "source_normalization" (get-in parser-ir ["source" "normalization"])
     "metadata_record_hash" (metadata-record/record-hash metadata-record)}))

(defn- snapshot-identity-object [workset]
  {"snapshot_scope" (map-value workset :snapshot_scope)
   "snapshot_date" (map-value workset :snapshot_date)
   "snapshot_inputs" (->> (map-value workset :works)
                          (map snapshot-input)
                          (sort-by (juxt #(get % "work_id")
                                         #(get % "slug")))
                          vec)})

(defn- source-identity-object
  [{:keys [snapshot-hash work-content-hash metadata-record-hash]}]
  (assoc
   (manifest/identity-object
    {"corpus_snapshot_hash" snapshot-hash
     "work_content_hash" work-content-hash
     "metadata_record_hash" metadata-record-hash
     "parser_build_hash" nil
     "parser_config_hash" nil
     "mapping_hash" nil
     "parser_ir_schema_hash" nil}
    {:manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
     :output-format-spec-hash (manifest/schema-hash "schemas/manifest.schema.json")})
   "metadata_record_hash" metadata-record-hash))

(defn- source-manifest [work snapshot snapshot-file generated-at]
  (let [source-manifest-path (required-work-value work :source_manifest_path)
        parser-ir (files/read-json (required-work-value work :parser_ir_path))
        metadata-record (files/read-json (required-work-value work :metadata_record_path))
        title (required-work-value work :title)
        work-content-hash (get-in parser-ir ["source" "work_content_hash"])
        metadata-record-hash (metadata-record/record-hash metadata-record)
        snapshot-hash (get snapshot "snapshot_hash")
        snapshot-file-hash (manifest/file-hash snapshot-file)
        source-encoding (get-in parser-ir ["source" "encoding"])
        identity-object (source-identity-object
                         {:snapshot-hash snapshot-hash
                          :work-content-hash work-content-hash
                          :metadata-record-hash metadata-record-hash})]
    (assoc
     (manifest/artifact-manifest
      {:artifact-kind "source"
       :validation-status "passed"
       :identity-object identity-object
       :content {"content_hash" work-content-hash
                 "media_type" (source-media-type source-encoding)
                 "path_hint" "source.txt"}
       :sidecars [{"role" "index-entry"
                   "hash" snapshot-file-hash
                   "media_type" "application/json"
                   "path_hint" (relative-path source-manifest-path snapshot-file)}]
       :generated-at generated-at
       :activity-id "https://w3id.org/abc/activity/source-ingest-snapshot"
       :agent "abc.tools.materialize-source-snapshot"
       :plan-hash nil
       :used (compact-hashes snapshot-hash
                             work-content-hash
                             metadata-record-hash
                             snapshot-file-hash)
       :was-derived-from (compact-hashes snapshot-hash work-content-hash)
       :notes (str "Source manifest for " title
                   ". corpus_snapshot_hash points to the source corpus snapshot descriptor.")})
     "license" {"label" "Aozora Bunko"
                "source_url" (or (work-value work :card_url)
                                 (get-in metadata-record ["work" "card_url"]))})))

(defn- validate-manifest! [manifest-value path]
  (let [errors (schema/validation-errors
                (files/read-json "schemas/manifest.schema.json")
                manifest-value)]
    (when (seq errors)
      (throw (ex-info "generated source manifest fails schema validation"
                      {:path path
                       :errors errors}))))
  :ok)

(defn materialize-source-snapshot!
  [{:keys [workset-path output-path generated-at]
    :or {generated-at default-generated-at}}]
  (let [workset (read-workset workset-path)
        identity-object (snapshot-identity-object workset)
        snapshot-hash (hash/format-sha256
                       (hash/sha256-json-jcs identity-object))
        snapshot {"snapshot_schema_id" snapshot-schema-id
                  "snapshot_hash_algorithm" snapshot-hash-algorithm
                  "snapshot_hash" snapshot-hash
                  "snapshot_identity_object" identity-object
                  "notes" "Source corpus snapshot over selected AAT JSON files. The hash is SHA-256 over the RFC8785/JCS canonical snapshot_identity_object."}
        output-file (io/file output-path)]
    (manifest/write-json-file! output-file snapshot)
    (let [works (mapv (fn [work]
                        (let [manifest-path (required-work-value
                                             work :source_manifest_path)
                              manifest-value (source-manifest
                                              work snapshot output-file
                                              generated-at)]
                          (validate-manifest! manifest-value manifest-path)
                          (manifest/write-json-file! manifest-path
                                                     manifest-value)
                          {:slug (required-work-value work :slug)
                           :source-manifest (io/file manifest-path)}))
                      (map-value workset :works))]
      {:snapshot output-file
       :snapshot-hash snapshot-hash
       :works works})))

(defn usage []
  (tel/log! :warn
            "Usage: clojure -M:abc/materialize-source-snapshot --workset workset.edn --output snapshot.json [--generated-at instant]"))

(def cli-options
  [["-w" "--workset FILE" "EDN workset describing source snapshot inputs."
    :id :workset-path]
   ["-o" "--output FILE" "Output source corpus snapshot JSON path."
    :id :output-path]
   [nil "--generated-at INSTANT" "UTC generation timestamp for deterministic manifests."
    :id :generated-at]])

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)
        {:keys [workset-path output-path generated-at]} options]
    (if (or (seq errors) (nil? workset-path) (nil? output-path))
      (do
        (doseq [error errors]
          (tel/log! :error error))
        (usage)
        (System/exit 2))
      (let [{:keys [snapshot snapshot-hash]} (materialize-source-snapshot!
                                              {:workset-path workset-path
                                               :output-path output-path
                                               :generated-at generated-at})]
        (tel/log! :info (str "materialized source snapshot " snapshot
                             " with hash " snapshot-hash))))))
