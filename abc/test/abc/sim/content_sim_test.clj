(ns abc.sim.content-sim-test
  "P16 content-side evolution properties (spec
  2026-07-12-content-side-evolution-simulation-design.md): cross-snapshot
  re-render/artifact-identity evolution of soranoha-build-publication over
  generated content trees (every selected work re-renders in a fresh
  temporary root — there is no publication cache, so identity evolution is
  observed through artifact_id, never a reused/skipped status), pin-chain
  composition (D7), and sampled integrity faults.

  The parser adapter is stubbed with the REAL split identity contract:
  parser-IR source.work_content_hash is the bundle hash supplied by ABC,
  while source.primary_text_hash is the independently computed member hash.
  official-source.json source_hash remains the raw-ZIP compatibility alias;
  snapshot validation checks every role independently."
  (:require [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as snapshot]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.source-bundle :as source-bundle]
            [abc.tools.source-snapshot-workset :as workset]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.properties :as prop])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- realistic-stub
  "Adapter-chain double with the real split bundle/member hash contract."
  [{:keys [source-bytes work-content-hash aat-file parser-ir-file
           divergence-file]}]
  (let [member-hash (hash/format-sha256 (hash/sha256-bytes source-bytes))]
    (abc-json/write-deterministic-json-file!
     aat-file
     {"version" 1 "work_id" "stub" "blocks" []
      "meta" {"adapter" "stub" "adapter_version" "test"
              "source_encoding" "utf-8"
              "source_hash" member-hash
              "primary_text_hash" member-hash
              "parse_complete" true "warnings" []}})
    (abc-json/write-deterministic-json-file!
     parser-ir-file
     {"schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")
      "source" {"work_content_hash" work-content-hash
                "primary_text_hash" member-hash
                "encoding" "utf-8" "normalization" "source"}
      "derived_from" {"aat_adapter" "stub" "aat_adapter_version" "test-stub"
                      "aat_version" 1
                      "mapping_id" (str "https://w3id.org/abc/mappings/"
                                        "aat-v1-to-parser-ir-v1/generated-probe")
                      "mapping_schema_hash" (files/example-hash "38")
                      "mapping_version" "0.3.0"}
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
      "config_schema_version" "0.2.0"
      ;; Temp-directory fixtures cannot prove official Git provenance, so they
      ;; explicitly request the recorded non-release `fixture` trust mode.
      "source_trust_mode" "fixture"
      "parser_profile" "aozora2html"
      "parser_candidate_ref" nil
      "publication_profile" "tei-publication-basic-ja-v1"
      "continue_on_failure" continue-on-failure?
      "materialization_scope" "smoke"})
    (str f)))

;; Parser resolution is independent of source trust; the temp-directory fixtures
;; have no adapter binaries, so the build's parser-runtime boundary is bound to a
;; well-formed (but unauthenticated) canonical runtime identity. Source trust
;; (fixture) and rights still make the resulting root non-admissible.
(def ^:private fixture-parser-runtime-identity
  {"adapter_id" "aozora2html"
   "adapter_argv_template" build-publication/parser-argv-template
   "converter_argv_template" build-publication/converter-argv-template
   "parser_build_hash" (files/example-hash "70")
   "converter_build_hash" (files/example-hash "71")
   "aat_parser_ir_mapping_hash" (files/example-hash "72")
   "parser_ir_schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")})

(defn- stub-parser-runtime [_]
  {:adapter nil
   :parser-runtime-identity fixture-parser-runtime-identity
   :parser-config-hash (hash/format-sha256
                        (hash/sha256-json-jcs fixture-parser-runtime-identity))
   :candidate-ref nil
   :qualification-identity-ref nil
   :problems []})

(defn- run-build! [{:keys [aozora-root out-root config-path snapshot-date replace?]}]
  (let [exit (with-redefs [publication-policy/assert-release-allowed!
                           (constantly :ok)]
               (binding [build-publication/*derive-parser-ir!* realistic-stub
                         build-publication/*resolve-parser-runtime!* stub-parser-runtime
                         *out* (java.io.StringWriter.)]
                 (build-publication/build-publication!
                  {:aozora-root (str aozora-root)
                   :config config-path
                   :output-root (str out-root)
                   :snapshot-date snapshot-date
                   :replace (boolean replace?)})))]
    {:exit exit
     :selection (abc-json/read-json-file (io/file out-root "source-selection-report.json"))
     :publications (abc-json/read-json-file
                    (io/file out-root "publications" "publications-report.json"))}))

(defn- statuses [reports]
  (into (sorted-map)
        (map (juxt #(get % "slug") #(get % "status")))
        (get-in reports [:publications "publications"])))

(defn- official-source [out-root slug]
  (abc-json/read-json-file
   (io/file out-root "materialized-root" "works" slug "official-source.json")))

(defn- source-bundle [out-root slug]
  (abc-json/read-json-file
   (io/file out-root "materialized-root" "works" slug "source-bundle.json")))

(defn- root-file-bytes
  "relative-path -> byte vector for every file under root. Used to pin that a
  root's byte content is (or is not) touched by a subsequent build attempt."
  [root]
  (let [root-file (io/file root)]
    (into (sorted-map)
          (keep (fn [^java.io.File f]
                  (when (.isFile f)
                    [(str (.relativize (.toPath root-file) (.toPath f)))
                     (vec (Files/readAllBytes (.toPath f)))])))
          (file-seq root-file))))

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
      (assoc-in [:contents "000101"] {:text text-a :images (sorted-map)})
      (assoc-in [:contents "000102"] {:text text-b :images (sorted-map)})))

(defn- overwrite-zip! [aozora-root m wid ^bytes zip-bytes]
  (let [rel (get-in (render/content-sources m) [wid :relpath])
        f (io/file aozora-root rel)]
    (files/write-bytes! f zip-bytes)))

(defn- no-text-zip-bytes []
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (doto (ZipEntry. "cover.png") (.setTime 0)))
      (.write zip (.getBytes "png" StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

(defn- unsafe-path-zip-bytes []
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (doto (ZipEntry. "../000101.txt") (.setTime 0)))
      (.write zip (.getBytes text-a StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

(defn- named-text-zip-bytes [entries]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (doseq [[path content] entries]
        (.putNextEntry zip (doto (ZipEntry. path) (.setTime 0)))
        (.write zip (.getBytes content StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray out)))

(deftest p16-admission-failure-disposition-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora strict-out cfg]
    (let [m (synthetic-state)
          best-out (io/file (.getParentFile strict-out) "best-effort")]
      (render/write-aozora-root! aozora m)
      (overwrite-zip! aozora m "000101" (unsafe-path-zip-bytes))
      (testing "strict admission failure is atomic"
        (let [strict-config (write-config! cfg false)
              thrown (try
                       (run-build! {:aozora-root aozora
                                    :out-root strict-out
                                    :config-path strict-config
                                    :snapshot-date "2026-07-12"})
                       nil
                       (catch Throwable t t))]
          (is (some? thrown))
          (is (some #(= :unsafe-member-path (:reason (ex-data %)))
                    (ex-chain thrown)))
          (is (not (.exists strict-out)))))
      (testing "best-effort promotes explicit non-releaseable evidence"
        (let [best-config (write-config! cfg true)
              reports (run-build! {:aozora-root aozora
                                   :out-root best-out
                                   :config-path best-config
                                   :snapshot-date "2026-07-12"})
              selection (:selection reports)
              failure (first (get selection "derive_failures"))
              workflow (abc-json/read-json-file
                        (io/file best-out "workflow-run.json"))]
          (is (= 1 (:exit reports)))
          (is (= 1 (get selection "selected_source_count")))
          (is (= 1 (get selection "derive_failed_count")))
          (is (= 1 (count (get selection "derive_failures"))))
          (is (not (contains? selection "release_admissible"))
              "the locally-derived verdict is gone from source-selection-report")
          (is (false? (get-in reports [:publications "release_admissible"]))
              "admissibility is the recorded verifier result")
          (is (= {"work_id" "000101"
                  "person_id" "000001"
                  "text_zip_relpath" "cards/000001/files/000101_t.zip"
                  "reason" "unsafe-member-path"
                  "decoded_path" "../000101.txt"
                  "normalized_path" "../000101.txt"}
                 (select-keys failure
                              ["work_id" "person_id" "text_zip_relpath"
                               "reason" "decoded_path" "normalized_path"])))
          (is (string/includes? (get failure "error")
                                "source bundle admission failed"))
          (is (string/ends-with? (get failure "archive_path")
                                 "cards/000001/files/000101_t.zip"))
          (is (= {slug-b "passed"} (statuses reports)))
          (is (= "partial" (get workflow "status")))
          (is (= "partial" (get-in workflow ["steps" 0 "status"])))
          (is (.exists (io/file best-out "source-selection-report.json")))
          (is (.exists (io/file best-out "publications"
                                "publications-report.json"))))))))

(deftest strict-admission-failure-aborts-atomically-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "strict-output")
          config-root (fs/file root "config")
          state (synthetic-state)]
      (render/write-aozora-root! aozora state)
      (overwrite-zip! aozora state "000101" (unsafe-path-zip-bytes))
      (let [failure (try
                      (run-build! {:aozora-root aozora :out-root output
                                   :config-path (write-config! config-root false)
                                   :snapshot-date "2026-07-12"})
                      nil
                      (catch Throwable throwable throwable))]
        (is (some? failure))
        (is (some #(= :unsafe-member-path (:reason (ex-data %)))
                  (ex-chain failure)))
        (is (not (files/exists? output)))))))

(deftest best-effort-admission-failures-are-counted-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "best-output")
          config-root (fs/file root "config")
          state (synthetic-state)]
      (render/write-aozora-root! aozora state)
      (overwrite-zip! aozora state "000101" (unsafe-path-zip-bytes))
      (let [reports (run-build! {:aozora-root aozora :out-root output
                                 :config-path (write-config! config-root true)
                                 :snapshot-date "2026-07-12"})]
        (is (= 1 (get-in reports [:selection "derive_failed_count"])))
        (is (= 1 (count (get-in reports [:selection "derive_failures"]))))))))

(deftest nonzero-derive-failures-are-not-release-admissible-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "best-output")
          config-root (fs/file root "config")
          state (synthetic-state)]
      (render/write-aozora-root! aozora state)
      (overwrite-zip! aozora state "000101" (unsafe-path-zip-bytes))
      (let [reports (run-build! {:aozora-root aozora :out-root output
                                 :config-path (write-config! config-root true)
                                 :snapshot-date "2026-07-12"})
            workflow (abc-json/read-json-file (fs/file output "workflow-run.json"))]
        (is (= 1 (:exit reports)))
        (is (false? (get-in reports [:publications "release_admissible"])))
        (is (= "partial" (get workflow "status")))
        ;; A best-effort (continue_on_failure) derive failure does not abort
        ;; the build: the current, still-not-release-admissible partial root
        ;; is installed at the configured output path, and the run exits
        ;; nonzero rather than silently succeeding.
        (is (.exists output))
        (is (.exists (io/file output "source-selection-report.json")))
        (is (.exists (io/file output "publications" "publications-report.json")))
        (is (= {slug-b "passed"} (statuses reports)))))))

(deftest strict-derive-failure-preserves-existing-root-byte-for-byte-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "output")
          config-root (fs/file root "config")
          state (synthetic-state)
          strict-config (write-config! config-root false)]
      (render/write-aozora-root! aozora state)
      (run-build! {:aozora-root aozora :out-root output :config-path strict-config
                   :snapshot-date "2026-07-12"})
      (let [sentinel (root-file-bytes output)]
        (overwrite-zip! aozora state "000101" (unsafe-path-zip-bytes))
        (let [thrown (try
                       (run-build! {:aozora-root aozora :out-root output
                                    :config-path strict-config
                                    :snapshot-date "2026-07-13" :replace? true})
                       nil
                       (catch Throwable t t))]
          (is (some? thrown))
          (is (some #(= :unsafe-member-path (:reason (ex-data %))) (ex-chain thrown)))
          ;; A strict (non-continue_on_failure) derive failure never reaches
          ;; promote-output-root!, even when --replace was requested: the
          ;; already-installed sentinel root is left byte-for-byte intact.
          (is (.exists output))
          (is (= sentinel (root-file-bytes output))))))))

(deftest build-replaces-existing-root-only-with-replace-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "output")
          config-root (fs/file root "config")
          state (synthetic-state)
          config (write-config! config-root false)]
      (render/write-aozora-root! aozora state)
      (run-build! {:aozora-root aozora :out-root output :config-path config
                   :snapshot-date "2026-07-12"})
      (let [sentinel (root-file-bytes output)]
        (testing "without --replace, a build over an existing root is rejected and leaves it untouched"
          (let [thrown (try
                         (run-build! {:aozora-root aozora :out-root output
                                      :config-path config
                                      :snapshot-date "2026-07-13"})
                         nil
                         (catch clojure.lang.ExceptionInfo t t))]
            (is (some? thrown))
            (is (string/includes? (ex-message thrown) "output-root already exists"))
            (is (= sentinel (root-file-bytes output)))))
        (testing "with --replace, a successful build replaces the existing root"
          (run-build! {:aozora-root aozora :out-root output :config-path config
                       :snapshot-date "2026-07-13" :replace? true})
          (let [plan (abc-json/read-json-file (io/file output "build-plan.json"))]
            (is (= "2026-07-13" (get plan "snapshot_date")))
            (is (not= sentinel (root-file-bytes output)))))))))

(deftest build-plan-records-only-relative-locators-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "output")
          config-root (fs/file root "config")
          state (synthetic-state)]
      (render/write-aozora-root! aozora state)
      (run-build! {:aozora-root aozora :out-root output
                   :config-path (write-config! config-root false)
                   :snapshot-date "2026-07-12"})
      (let [plan (abc-json/read-json-file (io/file output "build-plan.json"))
            recorded-materialized-root (get plan "materialized_root")
            recorded-selection-report (get plan "source_selection_report")
            recorded-publications (get plan "publications")]
        ;; Fixes the place/value defect Task 1 pinned: the promoted
        ;; build-plan.json now names every place as a RELATIVE locator under
        ;; the (eventual) output-root, never the discarded temporary root the
        ;; build actually wrote to and never the source aozora-root.
        (is (= "materialized-root" recorded-materialized-root))
        (is (= "source-selection-report.json" recorded-selection-report))
        (is (= "publications" recorded-publications))
        (is (not (string/starts-with? recorded-materialized-root "/")))
        (is (not (string/includes? recorded-materialized-root ".tmp-")))
        (is (not (contains? plan "aozora_root")))
        (is (.exists (io/file output recorded-materialized-root)))
        (is (.exists (io/file output recorded-selection-report)))
        (is (.exists (io/file output recorded-publications)))))))

(deftest p16-best-effort-promotes-all-rejected-evidence-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora unsafe-out cfg]
    (let [m (synthetic-state)
          damaged-out (io/file (.getParentFile unsafe-out) "all-damaged")
          config (write-config! cfg true)]
      (testing "all unsafe works produce an empty partial publication set"
        (render/write-aozora-root! aozora m)
        (doseq [wid ["000101" "000102"]]
          (overwrite-zip! aozora m wid (unsafe-path-zip-bytes)))
        (let [reports (run-build! {:aozora-root aozora
                                   :out-root unsafe-out
                                   :config-path config
                                   :snapshot-date "2026-07-12"})
              workflow (abc-json/read-json-file
                        (io/file unsafe-out "workflow-run.json"))]
          (is (= 1 (:exit reports)))
          (is (= 0 (get-in reports [:selection "selected_source_count"])))
          (is (= 2 (get-in reports [:selection "derive_failed_count"])))
          (is (= #{"unsafe-member-path"}
                 (set (map #(get % "reason")
                           (get-in reports [:selection "derive_failures"])))))
          (is (false? (get-in reports [:publications "release_admissible"])))
          (is (= 0 (get-in reports [:publications "publication_count"])))
          (is (= "partial" (get workflow "status")))))
      (testing "all damaged works follow the same counted disposition"
        (render/write-aozora-root! aozora m)
        (doseq [wid ["000101" "000102"]]
          (overwrite-zip! aozora m wid
                          (.getBytes "damaged ZIP" StandardCharsets/UTF_8)))
        (let [reports (run-build! {:aozora-root aozora
                                   :out-root damaged-out
                                   :config-path config
                                   :snapshot-date "2026-07-13"})]
          (is (= 1 (:exit reports)))
          (is (= 0 (get-in reports [:selection "selected_source_count"])))
          (is (= 2 (get-in reports [:selection "derive_failed_count"])))
          (is (= #{"unreadable-zip"}
                 (set (map #(get % "reason")
                           (get-in reports [:selection "derive_failures"])))))
          (is (= 0 (get-in reports [:publications "publication_count"]))))))))

(deftest p16-best-effort-preserves-cardinality-and-collision-diagnostics-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora collision-out cfg]
    (let [m (synthetic-state)
          multiple-root (render/temp-dir "sim-multiple-aozora")
          multiple-out (io/file (.getParentFile collision-out) "multiple")
          config (write-config! cfg true)]
      (try
        (render/write-aozora-root! aozora m)
        (overwrite-zip! aozora m "000101"
                        (named-text-zip-bytes [["é.txt" "lower"]
                                               ["É.txt" "upper"]]))
        (let [failure (first (get-in (run-build! {:aozora-root aozora
                                                  :out-root collision-out
                                                  :config-path config
                                                  :snapshot-date "2026-07-12"})
                                     [:selection "derive_failures"]))]
          (is (= "case-fold-member-path-collision" (get failure "reason")))
          (is (= "é.txt" (get failure "folded_path")))
          (is (= ["É.txt" "é.txt"] (get failure "paths"))))
        (render/write-aozora-root! multiple-root m)
        (overwrite-zip! multiple-root m "000101"
                        (named-text-zip-bytes [["two.txt" "two"]
                                               ["one.txt" "one"]]))
        (let [failure (first (get-in (run-build! {:aozora-root multiple-root
                                                  :out-root multiple-out
                                                  :config-path config
                                                  :snapshot-date "2026-07-12"})
                                     [:selection "derive_failures"]))]
          (is (= "multiple-primary-text-members" (get failure "reason")))
          (is (= ["one.txt" "two.txt"] (get failure "candidates"))))
        (finally
          (render/delete-tree! multiple-root))))))

(deftest p16-best-effort-recognizes-wrapped-admission-cause-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg true)
          inspect source-bundle/inspect-zip]
      (render/write-aozora-root! aozora m)
      (overwrite-zip! aozora m "000101" (unsafe-path-zip-bytes))
      (let [reports
            (with-redefs [source-bundle/inspect-zip
                          (fn [& args]
                            (try
                              (apply inspect args)
                              (catch Throwable admission
                                (throw (ex-info "workflow wrapper"
                                                {:wrapper true}
                                                admission)))))]
              (run-build! {:aozora-root aozora
                           :out-root out
                           :config-path config
                           :snapshot-date "2026-07-12"}))
            failures (get-in reports [:selection "derive_failures"])
            failure (first failures)]
        (is (= 1 (:exit reports)))
        (is (= 1 (count failures)))
        (is (= "unsafe-member-path" (get failure "reason")))
        (is (= "../000101.txt" (get failure "decoded_path")))
        (is (= "../000101.txt" (get failure "normalized_path")))
        (is (string/includes? (get failure "error")
                              "source bundle admission failed"))
        (is (not= "workflow wrapper" (get failure "error")))
        (is (= {slug-b "passed"} (statuses reports)))))))

;; A tampered work re-renders with its new content hash; the untouched
;; sibling work re-renders too (never reused/skipped — every selected work
;; renders in the fresh temporary root, so tamper on one work cannot be
;; masked by, nor mask, a stale publication for another).
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
          (is (= "passed" (get st slug-b)))
          (is (= tampered-hash (get (official-source out slug-a) "source_hash"))))))))

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
        (is (chain-clean-ex-info? t [:reason :archive-path]))
        (is (some #(= :no-primary-text-member (:reason (ex-data %)))
                  (ex-chain t)))))))

(deftest p16-image-repack-and-text-evolution-test
  ^{:clj-kondo/ignore [:unresolved-symbol]}
  (with-temp-dirs [aozora out cfg]
    (let [config (write-config! cfg false)
          m0 (-> (synthetic-state)
                 (assoc-in [:contents "000101" :images]
                           (sorted-map "images/表紙.png" "image-v1"
                                       "__MACOSX/._notes.txt" "finder")))
          m-image (assoc-in m0 [:contents "000101" :images "images/表紙.png"]
                            "image-v2")
          m-text (assoc-in m-image [:contents "000101" :text]
                           "作品000101 本文 夏")]
      (render/write-aozora-root! aozora m0)
      (let [r0 (run-build! {:aozora-root aozora :out-root out
                            :config-path config :snapshot-date "2026-07-01"})
            base (official-source out slug-a)]
        (is (= {slug-a "passed" slug-b "passed"} (statuses r0)))
        (is (= "000101.txt" (get base "primary_text_member")))
        (is (some #(= "__MACOSX/._notes.txt" (get % "path"))
                  (get (source-bundle out slug-a) "members")))

        (render/write-aozora-root! aozora m-image)
        (let [r-image (run-build! {:aozora-root aozora :out-root out
                                   :config-path config :snapshot-date "2026-07-02"
                                   :replace? true})
              image (official-source out slug-a)]
          (testing "image change rebuilds through conservative bundle invalidation"
            (is (= {slug-a "passed" slug-b "passed"} (statuses r-image)))
            (is (not= (get base "bundle_hash") (get image "bundle_hash")))
            (is (= (get base "primary_text_hash")
                   (get image "primary_text_hash"))))

          (render/write-aozora-root!
           aozora m-image
           {:zip-layouts {"000101" {:order :reverse
                                    :mtime 1700000000000
                                    :comment "metadata-only repack"
                                    :compression :stored}}})
          (let [r-repack (run-build! {:aozora-root aozora :out-root out
                                      :config-path config
                                      :snapshot-date "2026-07-03"
                                      :replace? true})
                repacked (official-source out slug-a)]
            (testing "metadata-only repack changes archive identity but re-renders"
              (is (= {slug-a "passed" slug-b "passed"} (statuses r-repack)))
              (is (not= (get image "archive_hash")
                        (get repacked "archive_hash")))
              (is (= (get image "bundle_hash") (get repacked "bundle_hash")))
              (is (= (get image "primary_text_hash")
                     (get repacked "primary_text_hash"))))

            (render/write-aozora-root! aozora m-text)
            (let [r-text (run-build! {:aozora-root aozora :out-root out
                                      :config-path config
                                      :snapshot-date "2026-07-04"
                                      :replace? true})
                  text-edited (official-source out slug-a)]
              (testing "text edit changes bundle and primary text and rebuilds"
                (is (= {slug-a "passed" slug-b "passed"} (statuses r-text)))
                (is (not= (get repacked "bundle_hash")
                          (get text-edited "bundle_hash")))
                (is (not= (get repacked "primary_text_hash")
                          (get text-edited "primary_text_hash")))))))))))

(deftest repack-and-image-identity-evolution-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "output")
          config-root (fs/file root "config")
          config (write-config! config-root false)
          initial (assoc-in (synthetic-state) [:contents "000101" :images]
                            (sorted-map "images/表紙.png" "image-v1"))
          changed (assoc-in initial [:contents "000101" :images "images/表紙.png"]
                            "image-v2")]
      (render/write-aozora-root! aozora initial)
      (run-build! {:aozora-root aozora :out-root output :config-path config
                   :snapshot-date "2026-07-01"})
      (let [before (official-source output slug-a)]
        (render/write-aozora-root! aozora changed)
        (run-build! {:aozora-root aozora :out-root output :config-path config
                     :snapshot-date "2026-07-02" :replace? true})
        (let [after (official-source output slug-a)]
          (is (not= (get before "bundle_hash") (get after "bundle_hash")))
          (is (= (get before "primary_text_hash")
                 (get after "primary_text_hash"))))))))

;; --- P16.1 build ---------------------------------------------------------

(defn- build-checks
  "Boolean checks for one build against the oracle. All keys must be true."
  [out reports expected]
  (let [sel-actual (get-in reports [:selection "selected_sources"])
        st (statuses reports)]
    {:relpaths (= (mapv :text_zip_relpath (:selected expected))
                  (mapv #(get % "text_zip_relpath") sel-actual))
     :identities (= (mapv (juxt :work_id :person_id :slug) (:selected expected))
                    (mapv (juxt #(get % "work_id") #(get % "person_id")
                                #(get % "slug"))
                          sel-actual))
     :rejected (= (:rejected expected)
                  (set (map (juxt #(get % "path") #(get % "reason"))
                            (get-in reports [:selection "rejected_sources"]))))
     :pins (every? (fn [{:keys [slug archive_hash bundle_hash]}]
                     (let [official (official-source out slug)
                           bundle (source-bundle out slug)]
                       (and (= archive_hash (get official "source_hash"))
                            (= archive_hash (get official "archive_hash"))
                            (= bundle_hash (get official "bundle_hash"))
                            (= bundle_hash (get bundle "bundle_hash"))
                            (= bundle_hash
                               (get-in (abc-json/read-json-file
                                        (io/file out "publications" slug
                                                 "source.manifest.json"))
                                       ["manifest_identity_object" "work_content_hash"])))))
                   (:selected expected))
     :statuses (and (every? #(= "passed" %) (vals st))
                    (= (count (:selected expected)) (count st))
                    (zero? (get-in reports [:publications "failed"])))}))

(deftest p16-1-build-sim-test
  (let [counter (harness/ratio-counter)]
    (harness/check!
     "P16.1 build" 10
     (prop/for-all [hist (sgen/content-history-gen {})]
                   (let [m (peek (:states (model/fold-history hist)))
                         expected (oracle/expected-selection (render/model->rows m)
                                                             (render/content-sources m))]
                     (with-temp-dirs [aozora out cfg]
                       (render/write-aozora-root! aozora m)
                       (let [config (write-config! cfg false)
                             args {:aozora-root aozora :out-root out :config-path config
                                   :snapshot-date "2026-07-12"}]
                         (if (harness/tick! counter (seq (:selected expected)))
                           (let [checks (build-checks out (run-build! args) expected)]
                             (when-not (every? val checks)
                               (println "P16.1 failing checks:"
                                        (vec (keep (fn [[k v]] (when-not v k)) checks))))
                             (every? val checks))
               ;; empty selection: the SUT must refuse loudly
                           (let [t (try (run-build! args) nil (catch Throwable t t))]
                             (and (some? t)
                                  (not (harness/forbidden-throw? t))
                                  (chain-clean-ex-info? t [:aozora_root])
                                  (some #(= "no catalog-backed work ZIPs were successfully derived"
                                            (ex-message %))
                                        (ex-chain t))))))))))
    (harness/assert-applied-ratio! "P16.1 build" counter)))

;; --- P16.3 pin-chain (D7) -------------------------------------------------

(defn- pin-chain-work-checks [aozora out snapshot-input selected]
  (let [{:keys [slug text_zip_relpath archive_hash bundle_hash
                primary_text_hash primary_text_member members identity_object]}
        selected
        official (official-source out slug)
        bundle (source-bundle out slug)
        parser-ir (abc-json/read-json-file
                   (io/file out "materialized-root" "works" slug "parser-ir.json"))
        source-manifest (abc-json/read-json-file
                         (io/file out "materialized-root" "works" slug
                                  "source.manifest.json"))
        primary-member-hash (get (some #(when (= primary_text_member
                                                 (get % "path")) %)
                                       members)
                                 "member_hash")]
    {:actual-archive (= archive_hash
                        (hash/format-sha256
                         (files/sha256-file (io/file aozora text_zip_relpath))))
     :archive-alias (= archive_hash
                       (get official "source_hash")
                       (get official "archive_hash")
                       (get bundle "archive_hash")
                       (get snapshot-input "archive_hash"))
     :independent-bundle (= bundle_hash
                            (hash/format-sha256
                             (hash/sha256-bytes
                              (jcs/rfc8785-string-domain-json-bytes
                               identity_object)))
                            (get official "bundle_hash")
                            (get bundle "bundle_hash")
                            (get-in parser-ir ["source" "work_content_hash"])
                            (get snapshot-input "work_content_hash")
                            (get-in source-manifest
                                    ["manifest_identity_object"
                                     "work_content_hash"]))
     :identity-object (= identity_object (get bundle "identity_object"))
     :members (= members
                 (mapv #(select-keys % ["path" "member_hash"])
                       (get bundle "members")))
     :primary-member (= primary_text_member
                        (get official "primary_text_member")
                        (get-in bundle ["identity_object" "primary_text_member"])
                        (get snapshot-input "primary_text_member"))
     :primary-integrity (= primary_text_hash primary-member-hash
                           (get official "primary_text_hash")
                           (get-in parser-ir ["source" "primary_text_hash"])
                           (get snapshot-input "primary_text_hash"))}))

(deftest p16-3-pin-chain-sim-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [aozora (fs/file root "aozora")
          output (fs/file root "output")
          config-root (fs/file root "config")
          state (synthetic-state)
          expected (oracle/expected-selection (render/model->rows state)
                                              (render/content-sources state))]
      (render/write-aozora-root! aozora state)
      (run-build! {:aozora-root aozora :out-root output
                   :config-path (write-config! config-root false)
                   :snapshot-date "2026-07-12"})
      (let [workset-file (fs/file config-root "workset.edn")
            snapshot-file (fs/file config-root "snapshot.json")]
        (workset/write-workset!
         {:input-root (str (fs/file output "materialized-root"))
          :output-path (str workset-file)
          :snapshot-scope "sim" :snapshot-date "2026-07-12"})
        (snapshot/materialize-source-snapshot!
         {:workset-path (str workset-file) :output-path (str snapshot-file)})
        (let [inputs (reduce (fn [result item]
                               (assoc result (get item "slug") item))
                             {}
                             (get-in (abc-json/read-json-file snapshot-file)
                                     ["snapshot_identity_object" "snapshot_inputs"]))
              checks (for [selected (:selected expected)
                           [_ holds?] (pin-chain-work-checks
                                       aozora output
                                       (get inputs (:slug selected)) selected)]
                       holds?)]
          (is (= (count (:selected expected)) (count inputs)))
          (is (every? true? checks)))))))

;; --- P16.2 evolution (the core) -------------------------------------------

(defn- exact-status-map? [slugs expected-status actual]
  (= (zipmap slugs (repeat expected-status)) actual))

(deftest exact-status-map-rejects-vacuous-or-partial-results-test
  (let [slugs ["a" "b"]]
    (is (exact-status-map? slugs "passed" {"a" "passed" "b" "passed"}))
    (is (not (exact-status-map? slugs "passed" {})))
    (is (not (exact-status-map? slugs "passed" {"a" "passed"})))
    (is (not (exact-status-map? slugs "passed"
                                {"a" "passed" "b" "passed" "c" "passed"})))))

(defn- artifact-id [out-root slug]
  (get (abc-json/read-json-file
        (io/file out-root "publications" slug "tei.manifest.json"))
       "artifact_id"))

(defn- rendered-work-content-hash
  "The rendered artifact's own record of the source it was rendered from
  (independent of corpus_snapshot_hash/snapshot-date, which legitimately
  rotate the whole artifact_id every dated leg — snapshot-date is release
  identity too, not a cache key)."
  [out-root slug]
  (get-in (abc-json/read-json-file
           (io/file out-root "publications" slug "tei.manifest.json"))
          ["manifest_identity_object" "work_content_hash"]))

(defn- evolution-checks
  "Runs three build legs and returns boolean checks (all must be true).
  s-before/s-after per the spec: around the seeded edit. No leg reuses or
  skips: every selected work re-renders in a fresh temporary root, every leg,
  so every status is \"passed\". leg1→leg2 advances the snapshot-date as
  content evolves (unchanged works must still keep the SAME rendered
  work_content_hash; edited works must rotate it — content identity, not the
  date-carrying artifact_id). leg2→leg3 is a literal identical rebuild — the
  SAME snapshot-date, same content — so it is held to the stronger claim:
  the SAME artifact_id (this is exactly the property that the old cache would
  have called \"reused\" instead of independently re-verifying)."
  [s-before s-after]
  (with-temp-dirs [aozora out cfg]
    (let [aozora2 (render/temp-dir "sim-aozora2")]
      (try
        (let [config (write-config! cfg false)
              selected-before (:selected (oracle/expected-selection
                                          (render/model->rows s-before)
                                          (render/content-sources s-before)))
              selected-after (:selected (oracle/expected-selection
                                         (render/model->rows s-after)
                                         (render/content-sources s-after)))
              before-slugs (mapv :slug selected-before)
              after-slugs (mapv :slug selected-after)
              before-bundle (into {} (map (juxt :slug :bundle_hash)) selected-before)
              after-bundle (into {} (map (juxt :slug :bundle_hash)) selected-after)
              unchanged-slugs (set (filter (fn [slug]
                                             (and (contains? before-bundle slug)
                                                  (= (get before-bundle slug)
                                                     (get after-bundle slug))))
                                           after-slugs))
              changed-slugs (remove unchanged-slugs after-slugs)]
          (render/write-aozora-root! aozora s-before)
          (let [r1 (run-build! {:aozora-root aozora :out-root out
                                :config-path config :snapshot-date "2026-07-01"})
                st1 (statuses r1)
                content1 (into {}
                               (map (fn [slug]
                                      [slug (rendered-work-content-hash out slug)]))
                               before-slugs)]
            (render/write-aozora-root! aozora2 s-after)
            (let [r2 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-02"
                                  :replace? true})
                  st2 (statuses r2)
                  content2 (into {}
                                 (map (fn [slug]
                                        [slug (rendered-work-content-hash out slug)]))
                                 after-slugs)
                  ids2 (into {} (map (fn [slug] [slug (artifact-id out slug)]))
                             after-slugs)
                  ;; Same snapshot-date and same content as leg2 — a literal
                  ;; identical rebuild, not the next day's evolution.
                  r3 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-02"
                                  :replace? true})
                  st3 (statuses r3)
                  ids3 (into {} (map (fn [slug] [slug (artifact-id out slug)]))
                             after-slugs)]
              {:leg1-all-passed (exact-status-map? before-slugs "passed" st1)
               :leg1-counts (zero? (get-in r1 [:publications "failed"]))
               :leg2-all-passed (exact-status-map? after-slugs "passed" st2)
               :leg2-counts (zero? (get-in r2 [:publications "failed"]))
               :leg3-all-passed (exact-status-map? after-slugs "passed" st3)
               :leg3-counts (zero? (get-in r3 [:publications "failed"]))
               :unchanged-content-keeps-content-identity
               (every? (fn [slug] (= (get content1 slug) (get content2 slug)))
                       unchanged-slugs)
               :changed-content-rotates-content-identity
               (every? (fn [slug] (not= (get content1 slug) (get content2 slug)))
                       changed-slugs)
               :identical-rebuild-keeps-artifact-id
               (every? (fn [slug] (= (get ids2 slug) (get ids3 slug)))
                       after-slugs)})))
        (finally (render/delete-tree! aozora2))))))

(deftest p16-2-evolution-sim-test
  (let [counter (harness/ratio-counter)]
    (harness/check!
     "P16.2 evolution" 5
     (prop/for-all [hist (sgen/content-history-gen {})]
                   (let [fold (model/fold-history hist)
                         applied-edit (sgen/find-applied fold :edit-content)]
                     (if (nil? applied-edit)
                       (do (harness/tick! counter false) true)
                       (let [i (.indexOf ^java.util.List (:events hist) (:event applied-edit))
                             s-before (nth (:states fold) i)
                             s-after (nth (:states fold) (inc i))
                             sel-b (oracle/expected-selection (render/model->rows s-before)
                                                              (render/content-sources s-before))
                             sel-a (oracle/expected-selection (render/model->rows s-after)
                                                              (render/content-sources s-after))]
                         (if (empty? (:selected sel-b))
                           (do (harness/tick! counter false) true)
                           (let [before-bundle (into {} (map (juxt :slug :bundle_hash))
                                                     (:selected sel-b))
                                 after-bundle (into {} (map (juxt :slug :bundle_hash))
                                                    (:selected sel-a))
                                 unchanged? (some (fn [[slug hash]]
                                                    (= hash (get before-bundle slug)))
                                                  after-bundle)
                                 changed? (some (fn [[slug hash]]
                                                  (not= hash (get before-bundle slug)))
                                                after-bundle)]
                             (harness/tick! counter (boolean (and unchanged? changed?)))
                             (let [checks (evolution-checks s-before s-after)]
                               (when-not (every? val checks)
                                 (println "P16.2 failing checks:"
                                          (vec (keep (fn [[k v]] (when-not v k)) checks))))
                               (every? val checks)))))))))
    (harness/assert-applied-ratio! "P16.2 evolution" counter)))
