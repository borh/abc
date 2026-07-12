(ns abc.sim.content-sim-test
  "P16 content-side evolution properties (spec
  2026-07-12-content-side-evolution-simulation-design.md): cross-snapshot
  skip/reuse/rebuild of soranoha-build-publication over generated content
  trees, pin-chain composition (D7), and sampled integrity faults.

  The parser adapter is stubbed with the REAL split identity contract:
  parser-IR source.work_content_hash is the bundle hash supplied by ABC,
  while source.primary_text_hash is the independently computed member hash.
  official-source.json source_hash remains the raw-ZIP compatibility alias,
  so old snapshot validation still fails across roles — the D7 divergence
  gated in the pin-chain property."
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
            [abc.tools.source-bundle :as source-bundle]
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
      "request_set_label" "sim-content"
      "snapshot_scope" "sim"
      "parser_profile" "aozora2html"
      "publication_profile" "tei-publication-basic-ja-v1"
      "continue_on_failure" continue-on-failure?
      "materialization_scope" "smoke"})
    (str f)))

(defn- run-build! [{:keys [aozora-root out-root config-path snapshot-date replace?]}]
  (let [exit (binding [build-publication/*derive-parser-ir!* realistic-stub
                       *out* (java.io.StringWriter.)]
               (build-publication/build-publication!
                (cond-> ["--aozora-root" (str aozora-root) "--config" config-path
                         "--output-root" (str out-root) "--snapshot-date" snapshot-date]
                  replace? (conj "--replace"))))]
    {:exit exit
     :selection (abc-json/read-json-file (io/file out-root "source-selection-report.json"))
     :publications (abc-json/read-json-file
                    (io/file out-root "publications" "publications-report.json"))}))

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

(defn- source-bundle [out-root slug]
  (abc-json/read-json-file
   (io/file out-root "materialized-root" "works" slug "source-bundle.json")))

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
          (is (false? (get selection "release_admissible")))
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
          (is (false? (get-in reports [:selection "release_admissible"])))
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
          (is (= tampered-hash (get (official-source out slug-a) "source_hash")))
          (is (= (get (official-source out slug-a) "bundle_hash")
                 (marker out slug-a))))))))

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
          (is (= (get (official-source out slug-a) "bundle_hash")
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
        (is (chain-clean-ex-info? t [:reason :archive-path]))
        (is (some #(= :no-primary-text-member (:reason (ex-data %)))
                  (ex-chain t)))))))

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
     :pins (every? (fn [{:keys [slug source_hash]}]
                     (let [official (official-source out slug)
                           bundle (source-bundle out slug)]
                       (and (= source_hash (get official "source_hash"))
                            (= source_hash (get official "archive_hash"))
                            (= (get official "bundle_hash")
                               (get bundle "bundle_hash"))
                            (= (get official "bundle_hash")
                               (get-in (abc-json/read-json-file
                                        (io/file out "materialized-root" "works" slug
                                                 "source.manifest.json"))
                                       ["manifest_identity_object" "work_content_hash"]))
                            (= (get official "bundle_hash") (marker out slug)))))
                   (:selected expected))
     :statuses (and (every? #(= "passed" %) (vals st))
                    (= (count (:selected expected)) (count st))
                    (zero? (get-in reports [:publications "failed"]))
                    (zero? (get-in reports [:publications "skipped"])))}))

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

(deftest p16-3-pin-chain-sim-test
  (harness/check!
   "P16.3 pin-chain" 5
   (prop/for-all [hist (sgen/content-history-gen {})]
                 (let [m (peek (:states (model/fold-history hist)))
                       expected (oracle/expected-selection (render/model->rows m)
                                                           (render/content-sources m))]
                   (if (empty? (:selected expected))
                     true ;; vacuous run; generation non-vacuity is enforced by P16.1's ratio
                     (with-temp-dirs [aozora out cfg]
                       (render/write-aozora-root! aozora m)
                       (run-build! {:aozora-root aozora :out-root out
                                    :config-path (write-config! cfg false)
                                    :snapshot-date "2026-07-12"})
                       (let [ws (io/file cfg "workset.edn")
                             _ (workset/write-workset!
                                {:input-root (str (io/file out "materialized-root"))
                                 :output-path (str ws)
                                 :snapshot-scope "sim" :snapshot-date "2026-07-12"})
                             res (try {:ok (snapshot/materialize-source-snapshot!
                                            {:workset-path (str ws)
                                             :output-path (str (io/file cfg "snapshot.json"))})}
                                      (catch Throwable t {:thrown t}))
                 ;; workset works sort by [work_id slug]; snapshot-input
                 ;; throws on the first mismatch
                             first-sel (first (sort-by (juxt :work_id :slug)
                                                       (:selected expected)))
                             bundle-hash (get (source-bundle out (:slug first-sel)) "bundle_hash")
                             hard-ok?
                             (if-let [t (:thrown res)]
                               (let [d (some #(let [dd (ex-data %)]
                                                (when (contains? dd :work-content-hash) dd))
                                             (ex-chain t))]
                                 (and (not (harness/forbidden-throw? t))
                                      (chain-clean-ex-info?
                                       t [:work :parser-ir-path :official-source-path
                                          :work-content-hash :official-source-hash])
                                      ;; pin WHY it fails: bundle hash vs raw-ZIP hash
                                      (= (:work d) (:slug first-sel))
                                      (= (:work-content-hash d) bundle-hash)
                                      (= (:official-source-hash d) (:source_hash first-sel))))
                               true)]
                         (and hard-ok?
                              (div/expected-failure*
                               :D7
                               "P16.3: build-publication output composes with materialize-source-snapshot!"
                               (fn [] (contains? res :ok)))))))))))

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

(defn- evolution-checks
  "Runs the three build legs and returns boolean checks (all must be true).
  s-before/s-after per the spec: around the seeded edit."
  [s-before s-after exp-status]
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
              expected-slugs (set (map :slug selected-after))]
          (render/write-aozora-root! aozora s-before)
          (let [r1 (run-build! {:aozora-root aozora :out-root out
                                :config-path config :snapshot-date "2026-07-01"})
                st1 (statuses r1)
                prior (into {} (map (fn [s] [s (pub-files out s)])) (keys st1))]
            (render/write-aozora-root! aozora2 s-after)
            (let [r2 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-02"
                                  :replace? true})
                  st2 (statuses r2)
                  cur-hash (into {}
                                 (map (juxt #(get % "slug")
                                            #(get % "bundle_hash")))
                                 (filter #(contains? expected-slugs (get % "slug"))
                                         (get-in r2 [:selection "selected_sources"])))
                  r2-markers (into {} (map (fn [slug] [slug (marker out slug)]))
                                   (keys cur-hash))
                  r2-files (into {} (map (fn [slug] [slug (pub-files out slug)]))
                                 (keys st2))
                  r3 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-03"
                                  :replace? true})
                  st3 (statuses r3)]
              {:leg1-all-passed (exact-status-map? before-slugs "passed" st1)
               :leg1-counts (and (zero? (get-in r1 [:publications "failed"]))
                                 (zero? (get-in r1 [:publications "skipped"])))
               :leg2-statuses (= exp-status st2)
               :leg2-counts (and (zero? (get-in r2 [:publications "failed"]))
                                 (zero? (get-in r2 [:publications "skipped"])))
               :markers (every? (fn [[slug h]] (= h (get r2-markers slug))) cur-hash)
               :reused-bytes (every? (fn [[slug status]]
                                       (or (not= "reused" status)
                                           (= (get prior slug) (get r2-files slug))))
                                     st2)
               :leg3-all-reused (exact-status-map? after-slugs "reused" st3)
               :leg3-counts (and (zero? (get-in r3 [:publications "failed"]))
                                 (zero? (get-in r3 [:publications "skipped"])))})))
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
                           (let [exp (oracle/expected-statuses sel-b sel-a)
                                 statuses-set (set (vals exp))]
                             (harness/tick! counter (and (contains? statuses-set "passed")
                                                         (contains? statuses-set "reused")))
                             (let [checks (evolution-checks s-before s-after exp)]
                               (when-not (every? val checks)
                                 (println "P16.2 failing checks:"
                                          (vec (keep (fn [[k v]] (when-not v k)) checks))))
                               (every? val checks)))))))))
    (harness/assert-applied-ratio! "P16.2 evolution" counter)))
