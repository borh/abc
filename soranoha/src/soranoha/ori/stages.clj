;; Per-work derivations: parse, convert, render,
;; validate-tei, plus the kernel's extract and metadata stages that feed
;; them. Every stage is engine-shaped ({:stage-id :stage-version
;; :toolchain-id :f}); the engine owns caching and the CAS, stages own
;; nothing but their function. Toolchain identity: subprocess stages hash the
;; actual binary (+ mapping) bytes; validate-tei hashes the TEI profile trio;
;; pure Clojure stages carry the run-supplied runtime/dependency identity.
(ns soranoha.ori.stages
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [soranoha.core.hash :as core-hash]
            [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.validate :as validate]
            [soranoha.aozora.ingest :as ingest]
            [soranoha.core.json :as record-json]
            [soranoha.aozora.source-bundle :as source-bundle]
            [soranoha.core.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- utf8 ^bytes [^String s] (.getBytes s "UTF-8"))
(defn- json-bytes ^bytes [value] (utf8 (record-json/write-deterministic-json-str value)))

(defn- env-value [k]
  (let [v (System/getenv k)]
    (when-not (string/blank? v) v)))

(defn- require-env [k what]
  (or (env-value k)
      (throw (ex-info (str what " unavailable; set " k) {:env_var k}))))

(defn resolve-adapter
  "Resolve the ab-aozora adapter, converter, and v2 mapping once from the
  environment the flake wrapper supplies (never per work)."
  []
  {:aozora-bin (require-env "AB_AOZORA_BIN" "ab-aozora parser")
   :convert-bin (require-env "AB_AAT_TO_PARSER_IR_BIN"
                             "ab-aat-to-parser-ir converter")
   :mapping (require-env "AB_AAT_TO_PARSER_IR_MAPPING_V2"
                         "aat->parser-IR v2 mapping document")})

(defn- run-process! [{:keys [args stdin-bytes]}]
  (let [{:keys [exit out err]}
        @(process/process args {:in stdin-bytes :out :bytes :err :string})]
    {:exit exit :out-bytes out :err err}))

(defn- with-temp-dir [f]
  (let [dir (fs/create-temp-dir {:prefix "soranoha-stage"})]
    (try (f dir)
         (finally (fs/delete-tree dir)))))

(defn extract-stage
  "Work ZIP (by content hash) -> primary text bytes + source facts."
  [clj-toolchain-id]
  {:stage-id "extract"
   :stage-version "1"
   :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob-path]} inputs]
        (let [inspection (source-bundle/inspect-zip
                          (io/file (str (blob-path (get inputs "zip")))))]
          {"primary-text" (:primary-text-bytes inspection)
           "source-facts" (json-bytes
                           {"work_content_hash" (:bundle-hash inspection)
                            "archive_hash" (:archive-hash inspection)
                            "primary_text_member" (:primary-text-member inspection)
                            "primary_text_hash" (:primary-text-hash inspection)})}))})

(defn metadata-stage
  "Work-local catalog rows and work id -> validated metadata and person records.
  Schema documents are captured once and are part of this stage's identity."
  [clj-toolchain-id assets-root]
  (let [schemas {:metadata (schema/read-schema (str (fs/path assets-root "schemas/metadata-record.schema.json")))
                 :person (schema/read-schema (str (fs/path assets-root "schemas/person-record.schema.json")))}]
    {:stage-id "metadata"
     :stage-version "3"
     :toolchain-id (core-hash/sha256-canonical-json
                    {"clj" clj-toolchain-id
                     "metadata-schema" (core-hash/sha256-canonical-json (:metadata schemas))
                     "person-schema" (core-hash/sha256-canonical-json (:person schemas))})
     :f (fn [{:keys [blob]} inputs]
          (let [{:keys [metadata-rec person-records]}
                (ingest/build-records
                 {:rows (json/read-json (String. ^bytes (blob (get inputs "catalog-rows")) "UTF-8"))
                  :work-id (get inputs "work_id") :schemas schemas})]
            {"metadata-record" (utf8 (str (record-json/write-deterministic-json-str metadata-rec) "\n"))
             "persons" (json-bytes person-records)}))}))

(defn parse-stage
  "Primary text bytes -> AAT JSON via the ab-aozora adapter."
  [{:keys [aozora-bin]}]
  {:stage-id "parse"
   :stage-version "1"
   :toolchain-id (core-hash/sha256-file aozora-bin)
   :f (fn [{:keys [blob]} inputs]
        (let [{:keys [exit out-bytes err]}
              (run-process! {:args [aozora-bin "--mode" "aat"]
                             :stdin-bytes (blob (get inputs "source"))})]
          (when-not (zero? exit)
            (throw (ex-info "ab-aozora adapter failed"
                            {:exit exit :stderr err})))
          {"aat" out-bytes}))})

(defn convert-stage
  "AAT JSON -> parser-IR + divergence via ab-aat-to-parser-ir."
  [{:keys [convert-bin mapping]}]
  {:stage-id "convert"
   :stage-version "1"
   :toolchain-id (core-hash/sha256-canonical-json
                  {"convert_bin" (core-hash/sha256-file convert-bin)
                   "mapping" (core-hash/sha256-file mapping)})
   :f (fn [{:keys [blob-path]} inputs]
        (with-temp-dir
          (fn [dir]
            (let [parser-ir-file (str (fs/path dir "parser-ir.json"))
                  divergence-file (str (fs/path dir "divergence.json"))
                  {:keys [exit err]}
                  (run-process!
                   {:args [convert-bin "convert"
                           "--aat" (str (blob-path (get inputs "aat")))
                           "--mapping" mapping
                           "--work-content-hash" (get inputs "work_content_hash")
                           "--parser-ir-out" parser-ir-file
                           "--divergence-out" divergence-file]})]
              (when-not (zero? exit)
                (throw (ex-info "ab-aat-to-parser-ir convert failed"
                                {:exit exit :stderr err})))
              {"parser-ir" (fs/read-all-bytes parser-ir-file)
               "divergence" (fs/read-all-bytes divergence-file)}))))})

(defn render-stage
  "parser-IR + metadata record + persons -> TEI XML bytes."
  [clj-toolchain-id]
  {:stage-id "render"
   :stage-version "24"
   :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [read-json (fn [name]
                          (json/read-json
                           (String. ^bytes (blob (get inputs name)) "UTF-8")))
              rendered (render/render-work
                        {:parser-ir (read-json "parser-ir")
                         :metadata-record (read-json "metadata-record")
                         :persons-by-id (read-json "persons")})]
          {"tei" (utf8 (:tei rendered))}))})

(defn plaintext-stage [clj-toolchain-id]
  {:stage-id "plaintext" :stage-version "4" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [reading (view/from-tei (String. ^bytes (blob (get inputs "tei")) "UTF-8"))]
          {"plaintext" (utf8 (projection/plaintext reading))
           "plaintext-projection" (json-bytes (projection/report :projection/plaintext reading))}))})

(defn markdown-stage [clj-toolchain-id]
  {:stage-id "markdown" :stage-version "4" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [reading (view/from-tei (String. ^bytes (blob (get inputs "tei")) "UTF-8"))]
          {"markdown" (utf8 (projection/markdown reading))
           "markdown-projection" (json-bytes (projection/report :projection/markdown reading))}))})

(defn validate-tei-stage
  "TEI bytes -> validation record. Include-and-flag: a failed
  validation is an artifact, never an exclusion. The stage runs
  in-process, so its toolchain identity binds the Clojure runtime
  identity alongside the TEI profile trio — a JVM validation-dependency
  change must invalidate its traces."
  [clj-toolchain-id profile]
  {:stage-id "validate-tei"
   :stage-version "3"
   :toolchain-id (core-hash/sha256-canonical-json
                  {"clj" clj-toolchain-id
                   "odd" (core-hash/sha256-file (:odd profile))
                   "rng" (core-hash/sha256-file (:rng profile))
                   "sch" (core-hash/sha256-file (:sch profile))
                   "generation" (when-let [path (:generation profile)]
                                  (when (fs/exists? path)
                                    (core-hash/sha256-file path)))})
   :f (fn [{:keys [blob]} inputs]
        (with-temp-dir
          (fn [dir]
            (let [tei-file (fs/file (fs/path dir "tei.xml"))]
              (io/copy ^bytes (blob (get inputs "tei")) tei-file)
              {"tei-validation" (json-bytes
                                 (validate/tei-validation-result
                                  profile tei-file))}))))})
