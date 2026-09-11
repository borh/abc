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
            [soranoha.core.rights :as core-rights]
            [soranoha.annotations.view :as view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.tei :as tei]
            [soranoha.ori.tei-header :as tei-header]
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
   :stage-version "3"
   :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob-path]} inputs]
        (let [inspection (source-bundle/inspect-zip
                          (io/file (str (blob-path (get inputs "zip")))))]
          {"primary-text" (:primary-text-bytes inspection)
           "source-facts"
           (json-bytes
            (cond-> {"work_content_hash" (:bundle-hash inspection)
                     "archive_hash" (:archive-hash inspection)
                     "primary_text_member" (:primary-text-member inspection)
                     "primary_text_hash" (:primary-text-hash inspection)}
              ;; How many bytes follow the archive proper. The bundle reader
              ;; finds them by retrying at earlier end-of-central-directory
              ;; records, so the member comes out whole; unzip trusts the
              ;; decoy record among them and refuses the file. Recorded here
              ;; because the count is known only while the archive is being
              ;; read, and the work page needs it to explain the refusal.
              (:trailing-garbage-trimmed inspection)
              (assoc "trailing_bytes_after_archive"
                     (:trailing-garbage-trimmed inspection))))}))})

(defn- notation-as-text
  "`field` read the way a text is read: parsed, rendered to TEI, projected to
  plaintext. The one-line document the parser sees has no header, so the
  first line is body text and not a title."
  [{:keys [aozora-bin convert-bin mapping]} field]
  (with-temp-dir
    (fn [dir]
      (let [parsed (run-process! {:args [aozora-bin "--mode" "aat"]
                                  :stdin-bytes (utf8 (str field "\n"))})
            _ (when-not (zero? (:exit parsed))
                (throw (ex-info "ab-aozora adapter failed on a catalog field"
                                {:exit (:exit parsed) :stderr (:err parsed) :field field})))
            aat-file (str (fs/path dir "aat.json"))
            parser-ir-file (str (fs/path dir "parser-ir.json"))
            _ (fs/write-bytes aat-file (:out-bytes parsed))
            converted (run-process!
                       {:args [convert-bin "convert"
                               "--aat" aat-file
                               "--mapping" mapping
                               "--work-content-hash" (core-hash/format-sha256
                                                      (core-hash/sha256-bytes (utf8 field)))
                               "--parser-ir-out" parser-ir-file
                               "--divergence-out" (str (fs/path dir "divergence.json"))]})
            _ (when-not (zero? (:exit converted))
                (throw (ex-info "ab-aat-to-parser-ir convert failed on a catalog field"
                                {:exit (:exit converted) :stderr (:err converted) :field field})))
            parser-ir (json/read-json (slurp parser-ir-file))
            document (render/text-document (:body (tei/render parser-ir)))
            text (string/trimr (projection/plaintext
                                (view/from-tei (tei-header/hiccup->pretty-xml-string document))))]
        (when (string/blank? text)
          (throw (ex-info "a catalog field read as nothing"
                          {:field field})))
        text))))

(defn catalog-text-reader
  "The reader the metadata stage hands to the catalog boundary.

  Aozora Bunko writes ※［＃…］ into a title or a publisher's name when the
  catalog cannot type a character, the same notation it writes into a text,
  and the boundary used to publish it as it stood: 八※［＃小書き片仮名ガ］
  岳登山記 as a page heading, a search row and a citation. A field that
  carries the notation is read the way the text is read, so the title holds
  what the body holds and follows it when the parser changes: ガ from Aozora
  Bunko's own gaiji dictionary, 𫝹 where the annotation names the code point,
  the digits without the instruction where 指数 asks for a superscript. A
  field without it is returned as it is, and never reaches the parser, which
  reads a bare 《》 as a ruby reading with no base and drops it; four subtitles
  are written with those brackets.

  The identity is the same three files the parse and convert stages hash."
  [{:keys [aozora-bin convert-bin mapping] :as adapter}]
  {:toolchain-id (core-hash/sha256-canonical-json
                  {"aozora_bin" (core-hash/sha256-file aozora-bin)
                   "convert_bin" (core-hash/sha256-file convert-bin)
                   "mapping" (core-hash/sha256-file mapping)})
   :read (fn [field]
           (if (string/includes? field "［＃")
             (notation-as-text adapter field)
             field))})

(defn metadata-stage
  "Work-local catalog rows and work id -> validated metadata and person records.
  Schema documents are captured once and are part of this stage's identity,
  and so is `text-reader`, a `catalog-text-reader`: the records it reads
  hold what the reader made of the catalog's prose."
  [clj-toolchain-id assets-root text-reader]
  (let [schemas {:metadata (schema/read-schema (str (fs/path assets-root "schemas/metadata-record.schema.json")))
                 :person (schema/read-schema (str (fs/path assets-root "schemas/person-record.schema.json")))}]
    {:stage-id "metadata"
     :stage-version "5"
     :toolchain-id (core-hash/sha256-canonical-json
                    {"clj" clj-toolchain-id
                     "metadata-schema" (core-hash/sha256-canonical-json (:metadata schemas))
                     "person-schema" (core-hash/sha256-canonical-json (:person schemas))
                     "catalog-text" (:toolchain-id text-reader)})
     :f (fn [{:keys [blob]} inputs]
          (let [{:keys [metadata-rec person-records]}
                (ingest/build-records
                 {:rows (json/read-json (String. ^bytes (blob (get inputs "catalog-rows")) "UTF-8"))
                  :work-id (get inputs "work_id") :schemas schemas
                  :read-text (:read text-reader)})]
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
  "parser-IR + metadata record + persons + publication identifier -> TEI bytes.

  `rights` is the grant read from the publication policy. It is fail-closed
  and part of the stage's toolchain identity: the terms are published inside
  every TEI file, so a policy change has to invalidate the cached TEI rather
  than leave works stating superseded terms.

  The `slug` and `works-standing` inputs are fail-closed for the same reason.
  Both are published in the header, and both are scalar stage inputs rather
  than blobs so that they enter the derivation key: a work rendered under one
  identifier, or under one set of rights terms, must not be served from cache
  under another."
  [clj-toolchain-id rights]
  (when-not (map? rights)
    (throw (ex-info "render stage requires the publication rights grant"
                    {:reason :missing-rights-grant})))
  {:stage-id "render"
   :stage-version "50"
   :toolchain-id (core-hash/sha256-canonical-json
                  {"clj" clj-toolchain-id "rights" rights})
   :f (fn [{:keys [blob]} inputs]
        (let [read-json (fn [name]
                          (json/read-json
                           (String. ^bytes (blob (get inputs name)) "UTF-8")))
              slug (get inputs "slug")
              works-standing (get inputs "works-standing")
              rendered (do
                         (when (string/blank? slug)
                           (throw (ex-info "render stage requires the publication identifier"
                                           {:reason :missing-publication-identifier})))
                         (when (string/blank? works-standing)
                           (throw (ex-info "render stage requires the work's rights standing"
                                           {:reason :missing-works-standing :slug slug})))
                         (render/render-work
                          {:parser-ir (read-json "parser-ir")
                           :metadata-record (read-json "metadata-record")
                           :persons-by-id (read-json "persons")
                           :slug slug
                           :rights (core-rights/work-terms rights works-standing)}))]
          {"tei" (utf8 (:tei rendered))}))})

(defn plaintext-stage [clj-toolchain-id]
  {:stage-id "plaintext" :stage-version "7" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [reading (view/from-tei (String. ^bytes (blob (get inputs "tei")) "UTF-8"))]
          {"plaintext" (utf8 (projection/plaintext reading))
           "plaintext-projection" (json-bytes (projection/report :projection/plaintext reading))}))})

(defn markdown-stage [clj-toolchain-id]
  {:stage-id "markdown" :stage-version "8" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [reading (view/from-tei (String. ^bytes (blob (get inputs "tei")) "UTF-8"))]
          {"markdown" (utf8 (projection/markdown reading))
           "markdown-projection" (json-bytes (projection/report :projection/markdown reading))}))})

(defn validate-tei-stage
  "TEI bytes -> validation record. Include-and-flag: a failed
  validation is an artifact, never an exclusion. The stage runs
  in-process, so its toolchain identity binds the Clojure runtime
  identity alongside the TEI profile trio: a JVM validation-dependency
  change must invalidate its traces."
  [clj-toolchain-id profile]
  {:stage-id "validate-tei"
   :stage-version "4"
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
