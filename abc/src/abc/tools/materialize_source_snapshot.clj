(ns abc.tools.materialize-source-snapshot
  (:require [abc.tools.cli :as abc-cli]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [abc.tools.source-bundle :as source-bundle]
            [abc.tools.source-snapshot-workset :as source-snapshot-workset]
            [clojure.java.io :as io]
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

(defn- work-file-path [work k]
  (or (work-value work (source-snapshot-workset/resolved-path-key k))
      (required-work-value work k)))

(defn- relative-path [from-file to-file]
  (files/relative-path (.getCanonicalFile (.getParentFile (io/file from-file)))
                       (.getCanonicalFile (io/file to-file))))

(defn- compact-hashes [& values]
  (vec (keep identity values)))

(defn- source-media-type [encoding]
  (if (seq encoding)
    (str "text/plain; charset=" encoding)
    "text/plain"))

(defn- key-present? [m k]
  (or (contains? m k)
      (contains? m (name k))))

(defn- identity-mode [work official-source parser-ir]
  (let [source (get parser-ir "source")
        fields [[:workset :source_bundle_path
                 (key-present? work :source_bundle_path)]
                [:official-source :archive_hash
                 (key-present? official-source :archive_hash)]
                [:official-source :bundle_hash
                 (key-present? official-source :bundle_hash)]
                [:official-source :primary_text_member
                 (key-present? official-source :primary_text_member)]
                [:official-source :primary_text_hash
                 (key-present? official-source :primary_text_hash)]
                [:parser-ir :primary_text_hash
                 (key-present? source :primary_text_hash)]]
        present (filterv #(nth % 2) fields)]
    (cond
      (empty? present) :legacy
      (= (count fields) (count present)) :source-bundle-v1
      :else
      (throw (ex-info "workset entry mixes legacy and source-bundle identity fields"
                      {:reason :mixed-source-identity-mode
                       :work (required-work-value work :slug)
                       :present (mapv #(subvec % 0 2) present)
                       :missing (->> fields
                                     (remove #(nth % 2))
                                     (mapv #(subvec % 0 2)))})))))

(defn- valid-hash? [value]
  (and (string? value) (re-matches hash/hash-pattern value)))

(defn- identity-error! [message role work data]
  (throw (ex-info message
                  (merge {:identity-role role
                          :work (required-work-value work :slug)}
                         data))))

(defn- member-hash [source-bundle-value primary-text-member]
  (some #(when (= primary-text-member (get % "path"))
           (get % "member_hash"))
        (get source-bundle-value "members")))

(defn- validate-legacy-identities!
  [work official-source parser-ir]
  (let [work-content-hash (get-in parser-ir ["source" "work_content_hash"])
        official-source-hash (get official-source "source_hash")]
    (when-not (= work-content-hash official-source-hash)
      (identity-error!
       "official-source source_hash differs from parser-IR work_content_hash"
       :legacy-source-hash work
       {:work-content-hash work-content-hash
        :official-source-hash official-source-hash}))
    {:work-content-hash work-content-hash}))

(defn- validate-source-bundle-identities!
  [work official-source parser-ir source-bundle-value]
  (let [source-bundle-errors
        (schema/validation-errors
         (files/read-json "schemas/source-bundle.schema.json")
         source-bundle-value)]
    (when (seq source-bundle-errors)
      (identity-error! "source-bundle fails schema validation"
                       :source-bundle-schema work
                       {:errors source-bundle-errors})))
  (let [source-hash (get official-source "source_hash")
        archive-hash (get official-source "archive_hash")
        bundle-archive-hash (get source-bundle-value "archive_hash")
        official-bundle-hash (get official-source "bundle_hash")
        source-bundle-hash (get source-bundle-value "bundle_hash")
        identity-object (get source-bundle-value "identity_object")
        recomputed-bundle-hash
        (hash/format-sha256 (hash/sha256-json-jcs identity-object))
        parser-bundle-hash (get-in parser-ir ["source" "work_content_hash"])
        official-primary-hash (get official-source "primary_text_hash")
        parser-primary-hash (get-in parser-ir ["source" "primary_text_hash"])
        primary-text-member (get official-source "primary_text_member")
        identity-primary-member (get identity-object "primary_text_member")
        primary-member-hash (member-hash source-bundle-value
                                         primary-text-member)]
    (when-not (= source-bundle/construction
                 (get identity-object "construction"))
      (identity-error! "source-bundle has unsupported construction"
                       :source-bundle-construction work
                       {:construction (get identity-object "construction")}))
    (when-not (and (valid-hash? source-hash)
                   (valid-hash? archive-hash)
                   (= source-hash archive-hash))
      (identity-error! "official-source source_hash must alias archive_hash"
                       :archive-alias work
                       {:source-hash source-hash :archive-hash archive-hash}))
    (when-not (= archive-hash bundle-archive-hash)
      (identity-error! "source-bundle archive_hash differs from official source"
                       :source-bundle-archive work
                       {:official-archive-hash archive-hash
                        :source-bundle-archive-hash bundle-archive-hash}))
    (when-not (= source-bundle-hash recomputed-bundle-hash)
      (identity-error! "source-bundle bundle_hash does not match identity_object"
                       :bundle-construction work
                       {:bundle-hash source-bundle-hash
                        :recomputed-bundle-hash recomputed-bundle-hash}))
    (when-not (= official-bundle-hash source-bundle-hash)
      (identity-error! "official-source bundle_hash differs from source-bundle"
                       :official-bundle work
                       {:official-bundle-hash official-bundle-hash
                        :source-bundle-hash source-bundle-hash}))
    (when-not (= parser-bundle-hash source-bundle-hash)
      (identity-error! "parser-IR work_content_hash differs from source-bundle"
                       :parser-bundle work
                       {:parser-work-content-hash parser-bundle-hash
                        :source-bundle-hash source-bundle-hash}))
    (when-not (= parser-primary-hash official-primary-hash)
      (identity-error! "parser-IR primary_text_hash differs from official source"
                       :parser-primary-text work
                       {:parser-primary-text-hash parser-primary-hash
                        :official-primary-text-hash official-primary-hash}))
    (when-not (and (= primary-text-member identity-primary-member)
                   (= official-primary-hash primary-member-hash))
      (identity-error! "primary text identity differs from source-bundle member"
                       :primary-member work
                       {:official-primary-text-member primary-text-member
                        :identity-primary-text-member identity-primary-member
                        :official-primary-text-hash official-primary-hash
                        :primary-member-hash primary-member-hash}))
    {:archive-hash archive-hash
     :work-content-hash source-bundle-hash
     :primary-text-hash official-primary-hash
     :primary-text-member primary-text-member}))

(defn- snapshot-input [work]
  (let [aat-path (required-work-value work :aat_path)
        parser-ir-path (work-file-path work :parser_ir_path)
        metadata-record-path (work-file-path work :metadata_record_path)
        official-source-path (required-work-value work :official_source_path)
        official-source-file (work-file-path work :official_source_path)
        parser-ir (files/read-json parser-ir-path)
        metadata-record (files/read-json metadata-record-path)
        official-source (files/read-json official-source-file)
        mode (identity-mode work official-source parser-ir)
        source-bundle-path (when (= :source-bundle-v1 mode)
                             (required-work-value work :source_bundle_path))
        source-bundle-file (when source-bundle-path
                             (work-file-path work :source_bundle_path))
        source-bundle-value (when source-bundle-file
                              (files/read-json source-bundle-file))
        identities (if (= :legacy mode)
                     (validate-legacy-identities! work official-source parser-ir)
                     (validate-source-bundle-identities!
                      work official-source parser-ir source-bundle-value))]
    (cond->
     {"slug" (required-work-value work :slug)
      "title" (required-work-value work :title)
      "work_id" (required-work-value work :work_id)
      "person_id" (required-work-value work :person_id)
      "card_url" (or (work-value work :card_url)
                     (get-in metadata-record ["work" "card_url"]))
      "aat_path" aat-path
      "aat_file_hash" (manifest/file-hash (work-file-path work :aat_path))
      "official_source_path" official-source-path
      "official_source_file_hash" (manifest/file-hash official-source-file)
      "official_text_zip_relpath" (get official-source "text_zip_relpath")
      "official_text_zip_member" (get official-source "zip_member")
      "aat_adapter" (get-in parser-ir ["derived_from" "aat_adapter"])
      "aat_adapter_version" (get-in parser-ir ["derived_from" "aat_adapter_version"])
      "aat_version" (get-in parser-ir ["derived_from" "aat_version"])
      "mapping_id" (get-in parser-ir ["derived_from" "mapping_id"])
      "mapping_schema_hash" (get-in parser-ir ["derived_from" "mapping_schema_hash"])
      "mapping_version" (get-in parser-ir ["derived_from" "mapping_version"])
      "parser_ir_schema_hash" (get parser-ir "schema_hash")
      "work_content_hash" (:work-content-hash identities)
      "source_encoding" (get-in parser-ir ["source" "encoding"])
      "source_normalization" (get-in parser-ir ["source" "normalization"])
      "metadata_record_hash" (metadata-record/record-hash metadata-record)}
      (= :source-bundle-v1 mode)
      (assoc "source_bundle_path" source-bundle-path
             "source_bundle_file_hash" (manifest/file-hash source-bundle-file)
             "archive_hash" (:archive-hash identities)
             "primary_text_hash" (:primary-text-hash identities)
             "primary_text_member" (:primary-text-member identities)))))

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
  (let [source-manifest-path (work-file-path work :source_manifest_path)
        parser-ir (files/read-json (work-file-path work :parser_ir_path))
        metadata-record (files/read-json (work-file-path work :metadata_record_path))
        title (required-work-value work :title)
        work-content-hash (get-in parser-ir ["source" "work_content_hash"])
        metadata-record-hash (metadata-record/record-hash metadata-record)
        snapshot-hash (get snapshot "snapshot_hash")
        snapshot-file-hash (manifest/file-hash snapshot-file)
        source-encoding (get-in parser-ir ["source" "encoding"])
        source-bundle-path (work-value work :source_bundle_path)
        source-bundle-file (when source-bundle-path
                             (work-file-path work :source_bundle_path))
        source-content-hash (if source-bundle-file
                              (manifest/file-hash source-bundle-file)
                              work-content-hash)
        identity-object (source-identity-object
                         {:snapshot-hash snapshot-hash
                          :work-content-hash work-content-hash
                          :metadata-record-hash metadata-record-hash})]
    (assoc
     (manifest/artifact-manifest
      {:artifact-kind "source"
       :validation-status "passed"
       :identity-object identity-object
       :content (if source-bundle-file
                  {"content_hash" source-content-hash
                   "media_type" "application/json"
                   "path_hint" "source-bundle.json"}
                  {"content_hash" work-content-hash
                   "media_type" (source-media-type source-encoding)
                   "path_hint" "source.txt"})
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
                             (when source-bundle-file source-content-hash)
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
  (let [workset (source-snapshot-workset/read-workset workset-path)
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
                        (let [manifest-path (work-file-path
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

(defn usage [_summary]
  "Usage: clojure -M:abc/materialize-source-snapshot --workset workset.edn --output snapshot.json [--generated-at instant]")

(def cli-options
  [["-w" "--workset FILE" "EDN workset describing source snapshot inputs."
    :id :workset-path]
   ["-o" "--output FILE" "Output source corpus snapshot JSON path."
    :id :output-path]
   [nil "--generated-at INSTANT" "UTC generation timestamp for deterministic manifests."
    :id :generated-at]])

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required    [:workset-path :output-path]
    :usage-fn    usage
    :run         (fn [{:keys [options]}]
                   (let [{:keys [workset-path output-path generated-at]} options
                         {:keys [snapshot snapshot-hash]}
                         (materialize-source-snapshot! {:workset-path workset-path
                                                        :output-path output-path
                                                        :generated-at generated-at})]
                     (tel/log! :info (str "materialized source snapshot " snapshot
                                          " with hash " snapshot-hash))))}))
