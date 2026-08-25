;; The registered stages (design component 4): parse, convert, render,
;; validate-tei, plus the kernel's extract and metadata stages that feed
;; them. Every stage is engine-shaped ({:stage-id :stage-version
;; :toolchain-id :f}); the engine owns caching and the CAS, stages own
;; nothing but their function. Toolchain identity: subprocess stages hash the
;; actual binary (+ mapping) bytes; validate-tei hashes the TEI profile trio;
;; pure Clojure stages carry the run-supplied clj toolchain id (over- rather
;; than under-invalidation, R6).
(ns soranoha.ori.stages
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [charred.api :as json]
            [soranoha.core.hash :as core-hash]
            [soranoha.ori.render :as render]
            [soranoha.ori.validate :as validate]
            [soranoha.ported.aozora-ingest :as ingest]
            [soranoha.ported.json :as abc-json]
            [soranoha.ported.source-bundle :as source-bundle]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn- utf8 ^bytes [^String s] (.getBytes s "UTF-8"))
(defn- json-bytes ^bytes [value] (utf8 (abc-json/write-deterministic-json-str value)))

(defn- env-value [k]
  (let [v (System/getenv k)]
    (when-not (string/blank? v) v)))

(defn- require-env [k what]
  (or (env-value k)
      (throw (ex-info (str what " unavailable; set " k) {:env_var k}))))

(defn resolve-adapter
  "Resolve the ab-aozora adapter, converter, and v2 mapping ONCE from the
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

;; ── Stage constructors ──────────────────────────────────────────────────────

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
  "Catalog (by content hash) + work id -> metadata record + person records.
  `rows-for` returns the parsed catalog rows for a catalog hash (run-scoped
  parse-once cache; deterministic because it is keyed by content)."
  [clj-toolchain-id rows-for catalog-provenance]
  {:stage-id "metadata"
   :stage-version "1"
   :toolchain-id clj-toolchain-id
   :f (fn [_resolve inputs]
        (let [rows (rows-for (get inputs "catalog"))
              work-id (get inputs "work_id")]
          (with-temp-dir
            (fn [dir]
              (let [record-file (str (fs/path dir "metadata-record.json"))
                    persons-dir (str (fs/path dir "persons"))]
                (ingest/run-from-rows!
                 {:rows rows
                  :work-id work-id
                  :output record-file
                  :persons-output-dir persons-dir
                  :overwrite true
                  :source-csv-provenance catalog-provenance})
                (let [record-bytes (fs/read-all-bytes record-file)
                      record (json/read-json (String. ^bytes record-bytes "UTF-8"))
                      persons (into (sorted-map)
                                    (for [contributor (get record "contributors")
                                          :let [pid (get contributor "person_id")]]
                                      [pid (json/read-json
                                            (String. ^bytes (fs/read-all-bytes
                                                             (fs/path persons-dir
                                                                      (str pid ".json")))
                                                     "UTF-8"))]))]
                  {"metadata-record" record-bytes
                   "persons" (json-bytes persons)}))))))})

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
  "parser-IR + metadata record + persons -> TEI XML + plaintext bytes."
  [clj-toolchain-id]
  {:stage-id "render"
   :stage-version "1"
   :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [read-json (fn [name]
                          (json/read-json
                           (String. ^bytes (blob (get inputs name)) "UTF-8")))
              rendered (render/render-work
                        {:parser-ir (read-json "parser-ir")
                         :metadata-record (read-json "metadata-record")
                         :persons-by-id (read-json "persons")})]
          {"tei" (utf8 (:tei rendered))
           "plaintext" (utf8 (:plaintext rendered))}))})

(defn validate-tei-stage
  "TEI bytes -> validation record. Include-and-flag (R7): a failed
  validation is an artifact, never an exclusion."
  [profile]
  {:stage-id "validate-tei"
   :stage-version "1"
   :toolchain-id (core-hash/sha256-canonical-json
                  {"odd" (core-hash/sha256-file (:odd profile))
                   "rng" (core-hash/sha256-file (:rng profile))
                   "sch" (core-hash/sha256-file (:sch profile))})
   :f (fn [{:keys [blob]} inputs]
        (with-temp-dir
          (fn [dir]
            (let [tei-file (fs/file (fs/path dir "tei.xml"))]
              (io/copy ^bytes (blob (get inputs "tei")) tei-file)
              {"tei-validation" (json-bytes
                                 (validate/tei-validation-result
                                  profile tei-file))}))))})
