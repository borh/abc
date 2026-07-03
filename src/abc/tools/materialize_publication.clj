(ns abc.tools.materialize-publication
  (:require [abc.tools.files :as files]
            [abc.tools.logging :as logging]
            [abc.tools.manifest :as manifest]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-publication-policy :as policy]
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
  [:TEI header body])

(defn- profile-hash []
  (str "sha256:" (files/sha256-file tei-odd-path)))

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
                  "rng_hash" (file-hash tei-rng-path)
                  "schematron_hash" (file-hash tei-schematron-path)
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
  (cond-> (manifest/identity-object
           manifest-inputs
           {:manifest-schema-hash (manifest/schema-hash "schemas/manifest.schema.json")
            :output-format-spec-hash output-format-spec-hash})
    true
    (assoc "metadata_record_hash" (get manifest-inputs "metadata_record_hash"))

    tei-profile-hash
    (assoc "tei_profile_hash" tei-profile-hash)))

(defn- compact-hashes [& values]
  (vec (keep identity values)))

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
           parser-ir metadata-record source-manifest generated-at]}]
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
                  "path_hint" "tei-validation-result.json"}]
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
        metadata-record (files/read-json metadata-record-path)
        source-manifest (read-source-manifest parser-ir-path source-manifest-path)
        plaintext-result (plaintext/render parser-ir)
        tei-result (parser-ir-tei/render parser-ir)
        header (tei-header/build
                (assoc (resolve-header-input metadata-record persons-dir)
                       :char-declarations (:char_declarations tei-result)))
        plain-file (io/file output-dir "plain.txt")
        tei-file (io/file output-dir "tei.xml")
        plaintext-manifest-file (io/file output-dir "plaintext.manifest.json")
        tei-manifest-file (io/file output-dir "tei.manifest.json")
        tei-validation-result-file (io/file output-dir "tei-validation-result.json")]
    (write-string-file! plain-file (:text plaintext-result))
    (write-string-file! tei-file
                        (tei-header/hiccup->xml-string
                         (tei-document header (:body tei-result))))
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
                      :parser-ir parser-ir
                      :metadata-record metadata-record
                      :source-manifest source-manifest
                      :generated-at generated-at})))
    {:plaintext plain-file
     :tei tei-file
     :plaintext-manifest plaintext-manifest-file
     :tei-manifest tei-manifest-file
     :tei-validation-result tei-validation-result-file}))

(defn usage []
  (tel/log! :warn "Usage: clojure -M:abc/materialize-publication <parser-ir.json> <metadata-record.json> <persons-dir> <output-dir> [--source-manifest source.manifest.json] [--generated-at instant]"))

(def cli-options
  [[nil "--generated-at INSTANT" "UTC generation timestamp for deterministic fixtures"
    :id :generated-at]
   [nil "--source-manifest PATH" "Source artifact manifest carrying corpus snapshot identity"
    :id :source-manifest]])

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        [parser-ir-path metadata-record-path persons-dir output-dir
         positional-generated-at & extra] arguments
        generated-at (or (:generated-at options)
                         positional-generated-at
                         default-generated-at)]
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
        (materialize-publication! {:parser-ir-path parser-ir-path
                                   :metadata-record-path metadata-record-path
                                   :persons-dir persons-dir
                                   :output-dir output-dir
                                   :source-manifest-path (:source-manifest options)
                                   :generated-at generated-at})
        (tel/log! :info (str "materialized publication artifacts to "
                             output-dir))))))
