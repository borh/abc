(ns abc.sim.content-sim-test
  "P16 content-side evolution properties (spec
  2026-07-12-content-side-evolution-simulation-design.md): cross-snapshot
  skip/reuse/rebuild of soranoha-build-publication over generated content
  trees, pin-chain composition (D7), and sampled integrity faults.

  The parser adapter is stubbed with the REAL hash contract: parser-IR
  source.work_content_hash = sha256 of the member bytes the adapter
  receives (ab-aozora-aat decode_source_bytes hashes stdin;
  ab-aat-to-parser-ir copies meta.source_hash). official-source.json
  source_hash is the raw-ZIP hash, so the two can never agree — the D7
  divergence gated in the pin-chain property."
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as snapshot]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.source-snapshot-workset :as workset]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.properties :as prop])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- realistic-stub
  "Adapter-chain double with the real hash contract (member-bytes hash;
  see ns docstring and D7). Other fields mirror soranoha_test.clj's stub."
  [{:keys [source-bytes aat-file parser-ir-file divergence-file]}]
  (let [member-hash (hash/format-sha256 (hash/sha256-bytes source-bytes))]
    (abc-json/write-deterministic-json-file!
     aat-file
     {"version" 1 "work_id" "stub" "blocks" []
      "meta" {"adapter" "stub" "adapter_version" "test"
              "source_encoding" "utf-8" "source_hash" member-hash
              "parse_complete" true "warnings" []}})
    (abc-json/write-deterministic-json-file!
     parser-ir-file
     {"schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")
      "source" {"work_content_hash" member-hash
                "encoding" "utf-8" "normalization" "source"}
      "derived_from" {"aat_adapter" "stub" "aat_adapter_version" "test-stub"
                      "aat_version" 1
                      "mapping_id" (str "https://w3id.org/abc/mappings/"
                                        "aat-v1-to-parser-ir-v1/generated-probe")
                      "mapping_schema_hash" (files/example-hash "38")
                      "mapping_version" "0.2.0"}
      "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                               "splitter_id" "ab-plaintext-japanese-v1"
                               "coordinate_system" "decoded_utf8"
                               "coverage" "body-paragraphs"}
      "nodes" [] "warnings" [] "errors" []})
    (abc-json/write-deterministic-json-file! divergence-file {"stub" true})))

(defn- write-config! [dir continue-on-failure?]
  (let [f (io/file dir "config.json")]
    (abc-json/write-deterministic-json-file!
     f
     {"config_schema_id" "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json"
      "request_set_label" "sim-content"
      "snapshot_scope" "sim"
      "parser_profile" "aozora2html"
      "publication_profile" "tei-publication-basic-ja-v1"
      "continue_on_failure" continue-on-failure?
      "materialization_scope" "smoke"})
    (str f)))

(defn- run-build! [{:keys [aozora-root out-root config-path snapshot-date replace?]}]
  (binding [build-publication/*derive-parser-ir!* realistic-stub
            *out* (java.io.StringWriter.)]
    (build-publication/build-publication!
     (cond-> ["--aozora-root" (str aozora-root) "--config" config-path
              "--output-root" (str out-root) "--snapshot-date" snapshot-date]
       replace? (conj "--replace"))))
  {:selection (abc-json/read-json-file (io/file out-root "source-selection-report.json"))
   :publications (abc-json/read-json-file
                  (io/file out-root "publications" "publications-report.json"))})

(defn- statuses [reports]
  (into (sorted-map)
        (map (juxt #(get % "slug") #(get % "status")))
        (get-in reports [:publications "publications"])))

(defn- marker [out-root slug]
  (string/trim (slurp (io/file out-root "publications" slug
                               "source_work_content_hash.txt"))))

(defn- official-source [out-root slug]
  (abc-json/read-json-file
   (io/file out-root "materialized-root" "works" slug "official-source.json")))

(defn- pub-files [out-root slug]
  (into (sorted-map)
        (keep (fn [^java.io.File f]
                (when (.isFile f)
                  [(.getName f) (vec (Files/readAllBytes (.toPath f)))])))
        (file-seq (io/file out-root "publications" slug))))

(defn- ex-chain [t]
  (take-while some? (iterate #(.getCause ^Throwable %) t)))

(defn- chain-clean-ex-info? [t ks]
  (boolean (some #(harness/clean-ex-info? % ks) (ex-chain t))))

(defmacro ^:private with-temp-dirs [[aozora-sym out-sym cfg-sym] & body]
  `(let [~aozora-sym (render/temp-dir "sim-aozora")
         out-parent# (render/temp-dir "sim-out")
         ~out-sym (io/file out-parent# "out")
         ~cfg-sym (render/temp-dir "sim-cfg")]
     (try
       ~@body
       (finally
         (render/delete-tree! ~aozora-sym)
         (render/delete-tree! out-parent#)
         (render/delete-tree! ~cfg-sym)))))

(def ^:private text-a "作品000101 本文 春")
(def ^:private text-b "作品000102 本文 秋")
(def ^:private slug-a "000101_000001_000101_t")
(def ^:private slug-b "000102_000002_000102_t")

(defn- synthetic-state []
  (-> (model/bootstrap 2)
      (assoc-in [:contents "000101"] {:text text-a})
      (assoc-in [:contents "000102"] {:text text-b})))

(defn- overwrite-zip! [aozora-root m wid ^bytes zip-bytes]
  (let [rel (get-in (render/content-sources m) [wid :relpath])
        f (io/file aozora-root rel)]
    (with-open [o (io/output-stream f)] (.write o zip-bytes))))

(defn- no-text-zip-bytes []
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (doto (ZipEntry. "cover.png") (.setTime 0)))
      (.write zip (.getBytes "png" StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

(deftest p16-4-tamper-rebuild-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)]
      (render/write-aozora-root! aozora m)
      (run-build! {:aozora-root aozora :out-root out :config-path config
                   :snapshot-date "2026-07-12"})
      (let [tampered (render/text->zip-bytes "作品000101 本文 改変" "000101")
            tampered-hash (hash/format-sha256 (hash/sha256-bytes tampered))]
        (overwrite-zip! aozora m "000101" tampered)
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-13" :replace? true}))]
          (is (= "passed" (get st slug-a)))
          (is (= "reused" (get st slug-b)))
          (is (= tampered-hash (marker out slug-a)))
          (is (= tampered-hash (get (official-source out slug-a) "source_hash"))))))))

(deftest p16-4-prior-marker-fault-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)
          marker-file #(io/file out "publications" slug-a "source_work_content_hash.txt")]
      (render/write-aozora-root! aozora m)
      (run-build! {:aozora-root aozora :out-root out :config-path config
                   :snapshot-date "2026-07-12"})
      (testing "corrupt marker → rebuild, never reuse"
        (spit (marker-file) "sha256:0000000000000000000000000000000000000000000000000000000000000000")
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-13" :replace? true}))]
          (is (= "passed" (get st slug-a)))
          (is (= "reused" (get st slug-b)))
          (is (= (get-in (render/content-sources m) ["000101" :source-hash])
                 (marker out slug-a)))))
      (testing "missing marker → rebuild"
        (is (.delete (marker-file)))
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-14" :replace? true}))]
          (is (= "passed" (get st slug-a))))))))

(deftest p16-4-no-text-member-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)]
      (render/write-aozora-root! aozora m)
      (overwrite-zip! aozora m "000101" (no-text-zip-bytes))
      (let [t (try (run-build! {:aozora-root aozora :out-root out
                                :config-path config :snapshot-date "2026-07-12"})
                   nil
                   (catch Throwable t t))]
        (is (some? t))
        (is (not (harness/forbidden-throw? t)))
        (is (chain-clean-ex-info? t [:path]))
        (is (some #(string/includes? (str (ex-message %)) "no .txt member")
                  (ex-chain t)))))))
