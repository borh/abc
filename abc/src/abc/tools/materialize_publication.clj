(ns abc.tools.materialize-publication
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.parallel :as parallel]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.parser-ir-sentence-policy :as sentence-policy]
            [abc.tools.parser-ir-tei :as parser-ir-tei]
            [abc.tools.schematron :as schematron]
            [abc.tools.tei :as tei]
            [abc.tools.tei-header :as tei-header]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [taoensso.telemere :as tel]))

(def default-generated-at "2026-07-03T00:00:00Z")
(def publication-policy-path "data/parser-ir-publication-policy-v0.json")
(def tei-odd-path "schemas/tei-profile.odd")
(def tei-rng-path "schemas/tei-profile.rng")
(def tei-schematron-path "schemas/tei-profile.sch")
(def preservation-schema-path "schemas/parser-ir-publication-preservation.schema.json")
(def preservation-schema-id "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json")
(def preservation-schema-version "0.3.0")

(defn- write-string-file! [file value]
  (io/make-parents file)
  (spit file value)
  file)

(defn- resolve-header-input [metadata-record persons-dir]
  (let [persons-by-id (into {}
                            (for [contributor (get metadata-record "contributors")
                                  :let [person-id (get contributor "person_id")
                                        person (files/read-json
                                                (io/file persons-dir
                                                         (str person-id ".json")))]]
                              [person-id person]))]
    {:work (get metadata-record "work")
     :contributors (mapv (fn [contributor]
                           {:relation-to-work (get contributor "relation_to_work")
                            :person (get persons-by-id
                                         (get contributor "person_id"))})
                         (get metadata-record "contributors"))}))

(defn- tei-document [header body]
  [:TEI {:xmlns/abc "https://w3id.org/abc/ns/tei"
         :abc/vocab-version "0"}
   header
   body])

;; The TEI profile trio (odd/rng/sch) is fixed for a process lifetime but was
;; re-hashed for every work (profile-hash 3x per work). Cache by canonical
;; path + mtime, mirroring the schematron/tei schema caches.
(defonce ^:private static-file-hash-cache (atom {}))

(defn- static-file-hash [path]
  (let [file (io/file path)
        cache-key [(.getCanonicalPath file) (.lastModified file)]]
    (or (get @static-file-hash-cache cache-key)
        (let [hash (str "sha256:" (files/sha256-file path))]
          (swap! static-file-hash-cache assoc cache-key hash)
          hash))))

(defn- profile-hash []
  (static-file-hash tei-odd-path))

(defn- file-hash [path]
  (str "sha256:" (files/sha256-file path)))

(defn- validation-layer [status validator message]
  {"status" status
   "validator" validator
   "message" message})

(defn- finding-severity [severity]
  (case severity
    :fatal "error"
    :error "error"
    :warning "warning"
    :info "info"
    (name severity)))

(defn- rng-validation [tei-file]
  (let [{:keys [violations]} (tei/validate! {:schema-path tei-rng-path
                                             :xml-path (str tei-file)
                                             :label (str tei-file)})
        failures (filter #(#{:error :fatal} (:severity %)) violations)]
    (if (seq failures)
      {:status "failed"
       :layer (validation-layer "failed" "jing"
                                (string/join "\n" (map :message failures)))
       :findings (mapv (fn [violation]
                         {"rule_id" "relax-ng"
                          "severity" (finding-severity (:severity violation))
                          "layer" "relax_ng"
                          "message" (:message violation)
                          "location" (when-let [line (:line violation)]
                                       (str line))
                          "allowed" false})
                       failures)}
      {:status "passed"
       :layer (validation-layer "passed" "jing"
                                "Validated against ODD-derived project Relax NG target.")
       :findings []})))

(defn- schematron-validation [tei-file]
  (let [{:keys [findings]} (schematron/validate! {:schema-path tei-schematron-path
                                                  :xml-path (str tei-file)
                                                  :label (str tei-file)})
        errors (filter #(= :error (:severity %)) findings)
        warnings (filter #(= :warning (:severity %)) findings)
        status (cond
                 (seq errors) "failed"
                 (seq warnings) "warning"
                 :else "passed")]
    {:status status
     :layer (validation-layer status "abc.tools.schematron"
                              (case status
                                "passed" "No project Schematron findings."
                                "warning" "Only project Schematron warnings were reported."
                                "failed" "Project Schematron errors were reported."))
     :findings (mapv (fn [finding]
                       {"rule_id" (:rule-id finding)
                        "severity" (name (:severity finding))
                        "layer" "schematron"
                        "message" (:message finding)
                        "location" (:location finding)
                        "allowed" (not= :error (:severity finding))})
                     findings)}))

(defn tei-validation-result [tei-file]
  (let [tei-profile-hash (profile-hash)
        rng-result (rng-validation tei-file)
        schematron-result (schematron-validation tei-file)
        status (cond
                 (some #{"failed"} [(:status rng-result)
                                    (:status schematron-result)])
                 "failed"

                 (some #{"warning"} [(:status rng-result)
                                     (:status schematron-result)])
                 "warning"

                 :else "passed")]
    {"validated_artifact" (file-hash tei-file)
     "tei_profile_hash" tei-profile-hash
     "status" status
     "layers" {"well_formed_xml" (validation-layer
                                  "passed" "clojure.data.xml"
                                  "Generated by clojure.data.xml.")
               "relax_ng" (:layer rng-result)
               "schematron" (:layer schematron-result)}
     "toolchain" {"odd_path" tei-odd-path
                  "rng_path" tei-rng-path
                  "schematron_path" tei-schematron-path
                  "odd_hash" tei-profile-hash
                  "rng_hash" (static-file-hash tei-rng-path)
                  "schematron_hash" (static-file-hash tei-schematron-path)
                  "generator" nil
                  "generator_build_hash" nil}
     "findings" (vec (concat (:findings rng-result)
                             (:findings schematron-result)))}))

(defn- adjacent-source-manifest-path [parser-ir-path]
  (let [parent (some-> (io/file parser-ir-path) .getParentFile)
        source-manifest (when parent
                          (io/file parent "source.manifest.json"))]
    (when (and source-manifest (.exists source-manifest))
      (str source-manifest))))

(defn- read-source-manifest [parser-ir-path source-manifest-path]
  (when-let [path (or source-manifest-path
                      (adjacent-source-manifest-path parser-ir-path))]
    (files/read-json path)))

(defn- orthographic-sentence-normalization? [parser-ir]
  (boolean
   (some (fn [sentence]
           (some #{"orthographic-katakana"}
                 (get sentence "tags" [])))
         (get parser-ir "sentences" []))))

(defn- corpus-snapshot-hash [source-manifest]
  (or (get-in source-manifest ["manifest_identity_object" "corpus_snapshot_hash"])
      (throw (ex-info "Publication materialization requires a source manifest with corpus_snapshot_hash"
                      {:source-manifest-present? (some? source-manifest)}))))

(defn- manifest-inputs [parser-ir metadata-record source-manifest]
  {"corpus_snapshot_hash" (corpus-snapshot-hash source-manifest)
   "work_content_hash" (get-in parser-ir ["source" "work_content_hash"])
   "metadata_record_hash" (metadata-record/record-hash metadata-record)
   "parser_build_hash" nil
   "parser_config_hash" nil
   "mapping_hash" nil
   "parser_ir_schema_hash" (get parser-ir "schema_hash")})

(defn- publication-identity-object
  [manifest-inputs {:keys [output-format-spec-hash tei-profile-hash]}]
  (cond-> (assoc (manifest/identity-object
                  manifest-inputs
                  {:manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
                   :output-format-spec-hash output-format-spec-hash})
                 "metadata_record_hash" (get manifest-inputs "metadata_record_hash"))
    tei-profile-hash
    (assoc "tei_profile_hash" tei-profile-hash)))

(defn- compact-hashes [& values]
  (vec (keep identity values)))

(defn- json-pointer [path]
  (str "/" (string/join "/" (map str path))))

(defn- preservation-record [index {:keys [ir-pointer tei-pointer construct
                                          source-pointer source-inventory-row
                                          message value record-class]
                                   :or {record-class "custom_sidecar"}}]
  {"record_id" (format "r%06d" index)
   "ir_pointer" ir-pointer
   "tei_pointer" tei-pointer
   "class" record-class
   "construct" construct
   "source_pointer" source-pointer
   "source_inventory_row" source-inventory-row
   "message" message
   "count" 1
   "first_path" ir-pointer
   "value" value})

(defn- span-records [collection-name values]
  (mapcat
   (fn [index value]
     (let [span (get value "span")]
       (when (map? span)
         [{:ir-pointer (json-pointer [collection-name index "span"])
           :tei-pointer nil
           :construct "span_coordinates"
           :source-pointer (get value "source_pointer")
           :source-inventory-row nil
           :message "Parser-IR span coordinates are preserved outside TEI publication XML."
           :value (pr-str span)}])))
   (range)
   values))

(defn- paragraph-preservation-records [paragraphs]
  (apply concat
         (map-indexed
          (fn [index paragraph]
            (concat
             (when-let [node-range (get paragraph "node_range")]
               [{:ir-pointer (json-pointer ["paragraphs" index "node_range"])
                 :tei-pointer nil
                 :construct "paragraph.node_range"
                 :source-pointer (get paragraph "source_pointer")
                 :source-inventory-row nil
                 :message "Paragraph node ranges are ABC traceability data, not TEI publication text."
                 :value (pr-str node-range)}])
             (when-let [source-pointer (get paragraph "source_pointer")]
               [{:ir-pointer (json-pointer ["paragraphs" index "source_pointer"])
                 :tei-pointer nil
                 :construct "paragraph.source_pointer"
                 :source-pointer source-pointer
                 :source-inventory-row nil
                 :message "Paragraph source pointers preserve source-authority traceability."
                 :value source-pointer}])
             (when (= "source-note" (get paragraph "role"))
               [{:ir-pointer (json-pointer ["paragraphs" index "classification"])
                 :tei-pointer nil
                 :construct "source_note.classification"
                 :source-pointer (get paragraph "source_pointer")
                 :source-inventory-row nil
                 :message "Source-note classification records whether routing was direct or heuristic."
                 :value (get paragraph "classification")}])))
          paragraphs)))

(defn- source-identity-records [parser-ir]
  (let [source (get parser-ir "source")]
    (for [field ["work_content_hash" "source_path" "encoding" "normalization"]
          :when (contains? source field)]
      {:ir-pointer (json-pointer ["source" field])
       :tei-pointer nil
       :construct "source_identity"
       :source-pointer nil
       :source-inventory-row nil
       :message "Parser-IR source identity is preserved in the ABC preservation sidecar and manifests."
       :value (get source field)})))

(defn- mapping-identity-records [parser-ir]
  (when-let [derived-from (get parser-ir "derived_from")]
    (for [field ["mapping_id" "mapping_version" "mapping_schema_hash"]
          :when (contains? derived-from field)]
      {:ir-pointer (json-pointer ["derived_from" field])
       :tei-pointer nil
       :construct "mapping_identity"
       :source-pointer nil
       :source-inventory-row nil
       :message "AAT-to-parser-IR mapping identity is preserved outside TEI publication XML."
       :value (get derived-from field)})))

(defn- diagnostic-records [parser-ir]
  (mapcat
   (fn [collection-name]
     (map-indexed
      (fn [index diagnostic]
        {:ir-pointer (json-pointer [collection-name index])
         :tei-pointer nil
         :construct "diagnostic"
         :source-pointer nil
         :source-inventory-row nil
         :message "Parser-IR diagnostics are preservation evidence, not TEI body text."
         :value (get diagnostic "code")})
      (get parser-ir collection-name [])))
   ["warnings" "errors"]))

(defn- gaiji-records [parser-ir]
  (keep-indexed
   (fn [index node]
     (when (= "gaiji" (get node "type"))
       {:ir-pointer (json-pointer ["nodes" index "gaiji" "resolved"])
        :tei-pointer nil
        :construct "gaiji_resolution"
        :source-pointer nil
        :source-inventory-row nil
        :message "Gaiji resolution status and diagnostics are preserved outside TEI publication XML."
        :value (get-in node ["gaiji" "resolved"])}))
   (get parser-ir "nodes" [])))

(defn- style-projection-records [parser-ir]
  (keep-indexed
   (fn [index node]
     (when (and (= "emphasis" (get node "type"))
                (some? (get node "style")))
       {:record-class "tei_profile_projection"
        :ir-pointer (json-pointer ["nodes" index "style"])
        :tei-pointer nil
        :construct "style_rendition"
        :source-pointer (get node "source_pointer")
        :source-inventory-row nil
        :message "Parser-IR emphasis style is projected into the ABC TEI profile rendition vocabulary."
        :value (get node "style")}))
   (get parser-ir "nodes" [])))

(defn- layout-projection-records [parser-ir]
  (concat
   (keep-indexed
    (fn [index node]
      (when (and (= "layout-span" (get node "type"))
                 (map? (get node "layout")))
        {:record-class "tei_profile_projection"
         :ir-pointer (json-pointer ["nodes" index "layout"])
         :tei-pointer nil
         :construct "style_rendition"
         :source-pointer (get node "source_pointer")
         :source-inventory-row nil
         :message "Parser-IR layout-span metadata is projected into ABC TEI profile attributes."
         :value (pr-str (get node "layout"))}))
    (get parser-ir "nodes" []))
   (keep-indexed
    (fn [index paragraph]
      (when (map? (get paragraph "layout"))
        {:record-class "tei_profile_projection"
         :ir-pointer (json-pointer ["paragraphs" index "layout"])
         :tei-pointer nil
         :construct "heading_jisage_structure"
         :source-pointer (get paragraph "source_pointer")
         :source-inventory-row nil
         :message "Parser-IR paragraph layout is projected into ABC TEI profile attributes."
         :value (pr-str (get paragraph "layout"))}))
    (get parser-ir "paragraphs" []))))

(defn- heading-projection-records [parser-ir]
  (keep-indexed
   (fn [index node]
     (when (= "heading" (get node "type"))
       {:record-class "tei_profile_projection"
        :ir-pointer (json-pointer ["nodes" index])
        :tei-pointer nil
        :construct "heading_jisage_structure"
        :source-pointer (get node "source_pointer")
        :source-inventory-row nil
        :message "Parser-IR heading structure is projected to TEI head under the ABC TEI profile."
        :value (get node "level")}))
   (get parser-ir "nodes" [])))

(defn- figure-projection-records [parser-ir]
  (keep-indexed
   (fn [index node]
     (when (= "image" (get node "type"))
       (let [figure-value (select-keys node ["src" "width" "height" "css_class"])]
         {:record-class "tei_profile_projection"
          :ir-pointer (json-pointer ["nodes" index])
          :tei-pointer nil
          :construct "figure_metadata"
          :source-pointer (get node "source_pointer")
          :source-inventory-row nil
          :message "Parser-IR image metadata is projected to TEI figure/graphic under the ABC TEI profile."
          :value (pr-str figure-value)})))
   (get parser-ir "nodes" [])))

(defn- sentence-segmentation-records [parser-ir]
  (when-let [segmentation (get parser-ir "sentence_segmentation")]
    [{:ir-pointer (json-pointer ["sentence_segmentation"])
      :tei-pointer nil
      :construct "sentence_segmentation"
      :source-pointer nil
      :source-inventory-row nil
      :message (str "Sentence <s> boundaries and node/paragraph ranges were rewritten "
                    "for segmentation by splitter " (get segmentation "splitter_id")
                    " over " (get segmentation "coverage")
                    "; source text is preserved in the TEI body.")
      :value (get segmentation "splitter_id")}]))

(defn- orthographic-annotation-records [parser-ir]
  (let [ortho (get parser-ir "orthographic_annotations")
        detector-id (get ortho "detector_id")
        detector-label (if (string? detector-id) detector-id (pr-str detector-id))
        sentences (get parser-ir "sentences" [])
        tagging-sentence-ids
        (fn [annotation-index]
          (->> sentences
               (filter #(some #{annotation-index}
                              (get % "orthographic_annotation_indices")))
               (mapv #(get % "id"))))]
    (map-indexed
     (fn [index annotation]
       (let [ids (tagging-sentence-ids index)]
         {:ir-pointer (json-pointer ["orthographic_annotations" "annotations" index])
          :tei-pointer nil
          :construct "orthographic_annotation"
          :source-pointer nil
          :source-inventory-row nil
          :message (str "Orthographic detector " detector-label " flagged "
                        (get annotation "kind") " over source bytes "
                        (get-in annotation ["source_byte_range" "start"]) "-"
                        (get-in annotation ["source_byte_range" "end"])
                        (if (seq ids)
                          (str "; tags sentence(s) " (string/join ", " ids))
                          "; tags no sentence")
                        ". Source text is preserved in the TEI body.")
          :value (get annotation "kind")}))
     (get ortho "annotations" []))))

(defn- preservation-records [parser-ir]
  (let [raw-records (vec
                     (concat
                      (source-identity-records parser-ir)
                      (mapping-identity-records parser-ir)
                      (span-records "nodes" (get parser-ir "nodes" []))
                      (span-records "paragraphs" (get parser-ir "paragraphs" []))
                      (paragraph-preservation-records (get parser-ir "paragraphs" []))
                      (diagnostic-records parser-ir)
                      (gaiji-records parser-ir)
                      (style-projection-records parser-ir)
                      (layout-projection-records parser-ir)
                      (heading-projection-records parser-ir)
                      (figure-projection-records parser-ir)
                      (sentence-segmentation-records parser-ir)
                      (orthographic-annotation-records parser-ir)))]
    (mapv preservation-record (range) raw-records)))

(defn- coverage-classes [records]
  (->> records
       (keep #(get % "class"))
       set
       sort
       vec))

(defn publication-preservation
  [{:keys [parser-ir source-manifest generated-at]}]
  (let [records (preservation-records parser-ir)
        derived-from (get parser-ir "derived_from")]
    {"schema_id" preservation-schema-id
     "schema_version" preservation-schema-version
     "schema_hash" (manifest/schema-hash preservation-schema-path)
     "parser_ir" {"schema_id" (get parser-ir "schema_id")
                  "schema_hash" (get parser-ir "schema_hash")
                  "work_id" (get parser-ir "work_id")}
     "tei" {"profile_id" "abc-tei-profile-v0"
            "profile_hash" (profile-hash)}
     "source" {"corpus_snapshot_hash" (corpus-snapshot-hash source-manifest)
               "work_content_hash" (get-in parser-ir ["source" "work_content_hash"])
               "source_path" (get-in parser-ir ["source" "source_path"])
               "encoding" (get-in parser-ir ["source" "encoding"])
               "normalization" (get-in parser-ir ["source" "normalization"])}
     "producer" {"agent" "abc.tools.materialize-publication"
                 "generated_at" generated-at}
     "mapping" (when derived-from
                 {"mapping_id" (get derived-from "mapping_id")
                  "mapping_version" (get derived-from "mapping_version")
                  "mapping_schema_hash" (get derived-from "mapping_schema_hash")})
     "coverage" {"record_count" (count records)
                 "classes" (coverage-classes records)}
     "records" records}))

(defn- artifact-manifest
  [{:keys [artifact-kind validation-status identity-object content-file
           media-type path-hint sidecars generated-at activity-id notes used]}]
  (manifest/artifact-manifest
   {:artifact-kind artifact-kind
    :validation-status validation-status
    :identity-object identity-object
    :content (manifest/content content-file media-type path-hint files/sha256-file)
    :sidecars sidecars
    :generated-at generated-at
    :activity-id activity-id
    :agent "abc.tools.materialize-publication"
    :plan-hash nil
    :used used
    :was-derived-from (compact-hashes (get identity-object "corpus_snapshot_hash")
                                      (get identity-object "work_content_hash"))
    :notes notes}))

(defn plaintext-manifest
  [{:keys [plain-file parser-ir metadata-record source-manifest generated-at]}]
  (let [inputs (manifest-inputs parser-ir metadata-record source-manifest)
        policy-hash (policy/policy-hash publication-policy-path)
        identity-object (publication-identity-object
                         inputs
                         {:output-format-spec-hash policy-hash})]
    (artifact-manifest
     {:artifact-kind "plaintext"
      :validation-status "passed"
      :identity-object identity-object
      :content-file plain-file
      :media-type "text/plain; charset=UTF-8"
      :path-hint "plain.txt"
      :sidecars []
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-parser-ir-plaintext"
      :used (compact-hashes (get inputs "corpus_snapshot_hash")
                            (get inputs "work_content_hash")
                            (get inputs "metadata_record_hash")
                            (get inputs "parser_ir_schema_hash")
                            policy-hash)
      :notes "Generated from parser-IR by ABC plaintext publication renderer."})))

(defn tei-manifest
  [{:keys [tei-file validation-result-file validation-result
           preservation-file parser-ir metadata-record source-manifest generated-at]}]
  (let [inputs (manifest-inputs parser-ir metadata-record source-manifest)
        tei-profile-hash (profile-hash)
        identity-object (publication-identity-object
                         inputs
                         {:output-format-spec-hash tei-profile-hash
                          :tei-profile-hash tei-profile-hash})]
    (artifact-manifest
     {:artifact-kind "tei"
      :validation-status (get validation-result "status")
      :identity-object identity-object
      :content-file tei-file
      :media-type "application/tei+xml"
      :path-hint "tei.xml"
      :sidecars [{"role" "validation-result"
                  "hash" (file-hash validation-result-file)
                  "media_type" "application/json"
                  "path_hint" "tei-validation-result.json"}
                 {"role" "preservation"
                  "hash" (file-hash preservation-file)
                  "media_type" "application/json"
                  "path_hint" "preservation.json"}]
      :generated-at generated-at
      :activity-id "https://w3id.org/abc/activity/materialize-parser-ir-tei"
      :used (compact-hashes (get inputs "corpus_snapshot_hash")
                            (get inputs "work_content_hash")
                            (get inputs "metadata_record_hash")
                            (get inputs "parser_ir_schema_hash")
                            tei-profile-hash)
      :notes "Generated from parser-IR by ABC TEI publication renderer."})))

(defn materialize-publication!
  [{:keys [parser-ir-path metadata-record-path persons-dir output-dir
           source-manifest-path generated-at]
    :or {generated-at default-generated-at}}]
  (let [output-dir (io/file output-dir)
        _ (.mkdirs output-dir)
        parser-ir (files/read-json parser-ir-path)
        parser-ir (sentence-policy/ensure-publication-sentence-evidence! parser-ir)
        metadata-record (files/read-json metadata-record-path)
        source-manifest (read-source-manifest parser-ir-path source-manifest-path)
        plaintext-result (plaintext/render parser-ir)
        tei-result (parser-ir-tei/render parser-ir)
        header (tei-header/build
                (assoc (resolve-header-input metadata-record persons-dir)
                       :char-declarations (:char_declarations tei-result)
                       :orthographic-sentence-normalization?
                       (orthographic-sentence-normalization? parser-ir)))
        plain-file (io/file output-dir "plain.txt")
        tei-file (io/file output-dir "tei.xml")
        preservation-file (io/file output-dir "preservation.json")
        plaintext-manifest-file (io/file output-dir "plaintext.manifest.json")
        tei-manifest-file (io/file output-dir "tei.manifest.json")
        tei-validation-result-file (io/file output-dir "tei-validation-result.json")]
    (write-string-file! plain-file (:text plaintext-result))
    (write-string-file! tei-file
                        (tei-header/hiccup->pretty-xml-string
                         (tei-document header (:body tei-result))))
    (manifest/write-json-file!
     preservation-file
     (publication-preservation {:parser-ir parser-ir
                                :source-manifest source-manifest
                                :generated-at generated-at}))
    (let [validation-result (tei-validation-result tei-file)]
      (manifest/write-json-file! tei-validation-result-file validation-result)
      (manifest/write-json-file!
       plaintext-manifest-file
       (plaintext-manifest {:plain-file plain-file
                            :parser-ir parser-ir
                            :metadata-record metadata-record
                            :source-manifest source-manifest
                            :generated-at generated-at}))
      (manifest/write-json-file!
       tei-manifest-file
       (tei-manifest {:tei-file tei-file
                      :validation-result-file tei-validation-result-file
                      :validation-result validation-result
                      :preservation-file preservation-file
                      :parser-ir parser-ir
                      :metadata-record metadata-record
                      :source-manifest source-manifest
                      :generated-at generated-at})))
    {:plaintext plain-file
     :tei tei-file
     :preservation preservation-file
     :plaintext-manifest plaintext-manifest-file
     :tei-manifest tei-manifest-file
     :tei-validation-result tei-validation-result-file}))

(defn materialize-release-publication!
  "Materialize release-facing artifacts after the global rights gate.
  Development fixture validation calls materialize-publication! directly."
  [opts]
  (publication-policy/assert-release-allowed!)
  (materialize-publication! opts))

(defn- batch-job-value [job key]
  (or (get job key)
      (get job (name key))))

(defn- materialize-batch-job! [job]
  (let [job-id (or (batch-job-value job :id)
                   (batch-job-value job "id"))
        output-dir (batch-job-value job :output_dir)]
    (try
      (let [result (materialize-publication!
                    {:parser-ir-path (batch-job-value job :parser_ir_path)
                     :metadata-record-path (batch-job-value job :metadata_record_path)
                     :persons-dir (batch-job-value job :persons_dir)
                     :output-dir output-dir
                     :source-manifest-path (batch-job-value job :source_manifest_path)
                     :generated-at (or (batch-job-value job :generated_at)
                                       default-generated-at)})
            validation (files/read-json (:tei-validation-result result))]
        {"id" job-id
         "status" (get validation "status")
         "output_dir" (str output-dir)
         "plain_text" (str (:plaintext result))
         "tei" (str (:tei result))
         "preservation" (str (:preservation result))
         "tei_validation_result" (str (:tei-validation-result result))
         "findings_count" (count (get validation "findings" []))})
      (catch Throwable t
        {"id" job-id
         "status" "failed"
         "output_dir" (str output-dir)
         "error" (.getMessage t)}))))

(defn- batch-concurrency [requested job-count]
  (let [requested (or requested 1)
        requested (if (pos-int? requested)
                    requested
                    (.availableProcessors (Runtime/getRuntime)))]
    (max 1 (min requested (max 1 job-count)))))

(defn- materialize-batch-jobs! [jobs concurrency]
  (parallel/ordered-pmap concurrency materialize-batch-job! jobs))

(def workflow-run-schema-id
  "https://w3id.org/abc/schemas/workflow-run.schema.json")

(defn- now-utc []
  (str (java.time.Instant/now)))

(defn- batch-step-status [result]
  (case (get result "status")
    "passed" "passed"
    "partial" "partial"
    "skipped" "skipped"
    "failed"))

(defn- batch-step-record [started-at ended-at result]
  {"id" (str (get result "id"))
   "status" (batch-step-status result)
   "started_at" started-at
   "ended_at" ended-at
   "duration_ms" 0
   "requires" []
   "produces" ["publication-output"]
   "inputs" []
   "outputs" (cond-> []
               (get result "tei")
               (conj {"role" "tei"
                      "path" (get result "tei")})
               (get result "plain_text")
               (conj {"role" "plain-text"
                      "path" (get result "plain_text")}))
   "messages" []})

(defn- batch-workflow-run [results]
  (let [started-at (now-utc)
        ended-at started-at
        failed (count (filter #(= "failed" (batch-step-status %)) results))
        partial (count (filter #(= "partial" (batch-step-status %)) results))
        passed (count (filter #(= "passed" (batch-step-status %)) results))]
    {"schema_id" workflow-run-schema-id
     "schema_version" "soranoha-workflow-run-v1"
     "workflow_id" "soranoha.materialize-publications-batch.v1"
     "run_id" "local-batch"
     "status" (cond
                (pos? failed) "failed"
                (pos? partial) "partial"
                :else "passed")
     "started_at" started-at
     "ended_at" ended-at
     "duration_ms" 0
     "step_count" (count results)
     "steps_passed" passed
     "steps_failed" failed
     "steps" (mapv #(batch-step-record started-at ended-at %) results)}))

(defn materialize-publications-batch!
  [{:keys [batch-path summary-path jobs]}]
  (let [batch (files/read-json batch-path)
        batch-jobs (vec (get batch "jobs" []))
        concurrency (batch-concurrency jobs (count batch-jobs))
        results (materialize-batch-jobs! batch-jobs concurrency)
        passed (count (filter #(= "passed" (get % "status")) results))
        summary-file (some-> summary-path io/file)
        summary-dir (some-> summary-file .getParentFile)
        summary-dir (or summary-dir (some-> summary-file .getAbsoluteFile .getParentFile))
        workflow-run-file (some-> summary-dir (io/file "workflow-run.json"))
        summary (cond-> {"schema_version" "abc-materialize-publications-batch-v1"
                         "jobs_total" (count results)
                         "jobs_concurrency" concurrency
                         "jobs_succeeded" passed
                         "jobs_failed" (- (count results) passed)
                         "jobs" results}
                  workflow-run-file
                  (assoc "workflow_run_path" "workflow-run.json"))]
    (when workflow-run-file
      (abc-json/write-deterministic-json-file!
       workflow-run-file
       (batch-workflow-run results)))
    (when summary-path
      (abc-json/write-deterministic-json-file! summary-path summary))
    summary))

(defn usage []
  (tel/log! :warn "Usage: clojure -M:abc/materialize-publication <parser-ir.json> <metadata-record.json> <persons-dir> <output-dir> [--source-manifest source.manifest.json] [--generated-at instant]")
  (tel/log! :warn "   or: clojure -M:abc/materialize-publications-batch --batch jobs.json --summary summary.json"))

(def cli-options
  [[nil "--generated-at INSTANT" "UTC generation timestamp for deterministic fixtures"
    :id :generated-at]
   [nil "--source-manifest PATH" "Source artifact manifest carrying corpus snapshot identity"
    :id :source-manifest]
   [nil "--batch PATH" "Batch materialization input JSON with a top-level jobs array"
    :id :batch]
   [nil "--summary PATH" "Batch materialization summary JSON path"
    :id :summary]
   [nil "--jobs N" "Batch materialization concurrency"
    :id :jobs
    :parse-fn #(Integer/parseInt %)]])

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        [parser-ir-path metadata-record-path persons-dir output-dir
         positional-generated-at & extra] arguments
        generated-at (or (:generated-at options)
                         positional-generated-at
                         default-generated-at)]
    (if (:batch options)
      (let [_ (publication-policy/assert-release-allowed!)
            summary (materialize-publications-batch!
                     {:batch-path (:batch options)
                      :summary-path (:summary options)
                      :jobs (:jobs options)})]
        (tel/log! :info (str "materialized " (get summary "jobs_succeeded")
                             "/" (get summary "jobs_total")
                             " publication batch job(s)")))
      (if (or (seq errors)
              (nil? parser-ir-path)
              (nil? metadata-record-path)
              (nil? persons-dir)
              (nil? output-dir)
              (seq extra))
        (do
          (doseq [error errors]
            (tel/log! :error error))
          (usage)
          (System/exit 2))
        (do
          (materialize-release-publication!
           {:parser-ir-path parser-ir-path
            :metadata-record-path metadata-record-path
            :persons-dir persons-dir
            :output-dir output-dir
            :source-manifest-path (:source-manifest options)
            :generated-at generated-at})
          (tel/log! :info (str "materialized publication artifacts to "
                               output-dir)))))))
