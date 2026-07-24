(ns abc.tools.snapshot-index-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn h [suffix] (files/example-hash suffix))

(def slug "0005_1234_rashomon")

(defn artifact-ref
  [{:keys [kind sidecar-role status aid mch ch path]
    :or {sidecar-role nil status "passed"}}]
  {"artifact_id" aid
   "artifact_kind" kind
   "work_slug" slug
   "sidecar_role" sidecar-role
   "validation_status" status
   "manifest_content_hash" mch
   "content_hash" ch
   "locator" {"kind" "loose" "path" path}})

(def source-selection
  {"trust_mode" "official-git"
   "aozora_git_commit" "0123456789abcdef0123456789abcdef01234567"
   "catalog_csv_hash" (h "c1")
   "snapshot_date" "2026-07-07"
   "sources"
   [{"work_id" "0005" "person_id" "1234" "slug" slug
     "text_zip_relpath" "cards/000005/files/1234_ruby.zip"
     "archive_hash" (h "a1") "bundle_hash" (h "b1")
     "primary_text_member" "1234_ruby.txt"
     "primary_text_hash" (h "d1") "metadata_record_hash" (h "e5")}]})

(def parser-runtime-identity
  {"adapter_id" "ab-aozora"
   "adapter_argv_template" ["{executable}" "--mode" "{mode}"]
   "converter_argv_template" ["{executable}" "convert"]
   "parser_build_hash" (h "f1")
   "converter_build_hash" (h "f2")
   "aat_parser_ir_mapping_hash" (h "f3")
   "parser_ir_schema_hash" (h "f4")})

(def failure-policy
  {"allow_nonzero_failures" false
   "max_failure_rate" 0
   "per_diagnostic_tolerances" {}})

(def layout-policy
  {"loose_artifact_kinds" ["source" "parser-ir" "tei" "plaintext"]
   "batched_artifact_kinds" []
   "batch_target_work_count" 250
   "archive_format" "tar.zst"})

(def base-references
  [(artifact-ref {:kind "source" :aid (h "51") :mch (h "52") :ch (h "53")
                  :path "materialized-root/works/0005_1234_rashomon/source.manifest.json"})
   (artifact-ref {:kind "parser-ir" :aid (h "61") :mch (h "62") :ch (h "63")
                  :path "materialized-root/works/0005_1234_rashomon/parser-ir.manifest.json"})
   (artifact-ref {:kind "plaintext" :aid (h "71") :mch (h "72") :ch (h "73")
                  :path "publications/0005_1234_rashomon/plaintext.manifest.json"})
   (artifact-ref {:kind "tei" :aid (h "81") :mch (h "82") :ch (h "83") :status "warning"
                  :path "publications/0005_1234_rashomon/tei.manifest.json"})])

(defn build-args [& {:as overrides}]
  (merge
   {:snapshot-date "2026-07-07"
    :generated-at "2026-07-07T00:00:00Z"
    :source-selection source-selection
    :parser-runtime-identity parser-runtime-identity
    :candidate-ref (h "ca")
    :qualification-identity-ref (h "cb")
    :failure-policy failure-policy
    :layout-policy layout-policy
    :schema-hashes [(manifest/schema-hash "schemas/snapshot-index.schema.json")]
    :failures []
    :artifact-references base-references}
   overrides))

;; ── Value construction ──────────────────────────────────────────────────────

(deftest build-produces-closed-v0_2_0-value-test
  (let [index (snapshot-index/build-snapshot-index (build-args))
        schema-json (files/read-json "schemas/snapshot-index.schema.json")]
    (is (= "0.2.0" (get index "schema_version")))
    (is (nil? (schema/validation-errors schema-json index)))
    (is (true? (snapshot-index/validate-snapshot-index! index)))
    (is (= "0.2.0" (get schema-json "version"))
        "the schema document version annotation is 0.2.0")))

(deftest sources-sort-canonically-test
  (let [shuffled (update source-selection "sources"
                         (fn [sources]
                           (conj sources
                                 {"work_id" "0001" "person_id" "0002" "slug" "a"
                                  "text_zip_relpath" "cards/000001/files/a.zip"
                                  "archive_hash" (h "a2") "bundle_hash" (h "b2")
                                  "primary_text_member" "a.txt"
                                  "primary_text_hash" (h "d2")
                                  "metadata_record_hash" (h "e6")})))
        index (snapshot-index/build-snapshot-index
               (build-args :source-selection shuffled))]
    (is (= ["0001" "0005"]
           (mapv #(get % "work_id")
                 (get-in index ["source_selection_identity_object" "sources"])))
        "sources are sorted by [work_id person_id text_zip_relpath]")
    (is (= (snapshot-index/source-selection-hash shuffled)
           (snapshot-index/source-selection-hash (update shuffled "sources" reverse)))
        "source_selection_hash is order-independent")))

(deftest failure-set-hash-is-stable-and-projects-only-identity-test
  (let [f1 {"stage" "derive" "work_slug" "w1" "code" "invalid-zip"
            "message" "host path /tmp/a" "path" "/tmp/a"}
        f2 {"stage" "render" "work_slug" nil "code" "tei-failed"
            "message" "different message"}]
    (is (= (snapshot-index/failure-set-hash [f1 f2])
           (snapshot-index/failure-set-hash [f2 f1]))
        "failure_set_hash is order-independent")
    (is (= (snapshot-index/failure-set-hash [f1])
           (snapshot-index/failure-set-hash
            [(assoc f1 "message" "totally other" "path" "/var/x")]))
        "diagnostic message and host path do not enter failure_set_hash")))

(deftest artifact-identity-excludes-locator-test
  (let [moved (mapv #(assoc-in % ["locator" "path"]
                               (str "staged/" (get-in % ["locator" "path"])))
                    base-references)
        index (snapshot-index/build-snapshot-index (build-args))
        moved-index (snapshot-index/build-snapshot-index
                     (build-args :artifact-references moved))]
    (is (= (snapshot-index/artifact-set-hash base-references)
           (snapshot-index/artifact-set-hash moved))
        "changing a locator does not change artifact_set_hash")
    (is (= (get index "snapshot_identity_hash")
           (get moved-index "snapshot_identity_hash"))
        "changing a locator does not change snapshot_identity_hash")))

(deftest artifact-identity-includes-work-slug-test
  (let [rekeyed (mapv #(assoc % "work_slug" "other_work") base-references)]
    (is (not= (snapshot-index/artifact-set-hash base-references)
              (snapshot-index/artifact-set-hash rekeyed))
        "work_slug participates in artifact identity")))

(deftest nullable-candidate-coordinates-test
  (let [index (snapshot-index/build-snapshot-index
               (build-args :candidate-ref nil :qualification-identity-ref nil))]
    (is (nil? (get-in index ["snapshot_index_identity_object" "candidate_ref"])))
    (is (nil? (get-in index ["snapshot_index_identity_object"
                             "qualification_identity_ref"])))
    (is (true? (snapshot-index/validate-snapshot-index! index)))))

(deftest runtime-parser-hashes-are-non-null-test
  (let [index (snapshot-index/build-snapshot-index (build-args))
        runtime (get index "parser_runtime_identity_object")]
    (doseq [field ["parser_build_hash" "converter_build_hash"
                   "aat_parser_ir_mapping_hash" "parser_ir_schema_hash"]]
      (is (re-matches #"^sha256:[0-9a-f]{64}$" (get runtime field))
          (str field " is a non-null sha256")))
    (is (= (get-in index ["snapshot_index_identity_object" "parser_config_hash"])
           (snapshot-index/parser-config-hash runtime))
        "parser_config_hash is the JCS hash of the whole runtime identity object")))

(deftest identity-rotates-on-every-identity-field-test
  (let [base (get (snapshot-index/build-snapshot-index (build-args))
                  "snapshot_identity_hash")
        rotations
        {:source-selection (assoc-in source-selection ["sources" 0 "bundle_hash"] (h "99"))
         :parser-runtime-identity (assoc parser-runtime-identity "parser_build_hash" (h "98"))
         :candidate-ref (h "97")
         :qualification-identity-ref (h "96")
         :failure-policy (assoc failure-policy "allow_nonzero_failures" true)
         :layout-policy (assoc layout-policy "batch_target_work_count" 7)
         :schema-hashes [(h "95")]
         :failures [{"stage" "derive" "work_slug" "w" "code" "x"}]
         :artifact-references (conj base-references
                                    (artifact-ref {:kind "plaintext" :aid (h "41")
                                                   :mch (h "42") :ch (h "43")
                                                   :path "publications/other/plaintext.manifest.json"}))}]
    (doseq [[k v] rotations]
      (is (not= base
                (get (snapshot-index/build-snapshot-index (build-args k v))
                     "snapshot_identity_hash"))
          (str "rotating " k " rotates snapshot_identity_hash")))))

;; ── Closed schema rejects retired keys ──────────────────────────────────────

(deftest closed-schema-rejects-retired-keys-test
  (let [schema-json (files/read-json "schemas/snapshot-index.schema.json")
        index (snapshot-index/build-snapshot-index (build-args))]
    (testing "top-level retired keys are rejected"
      (doseq [k ["snapshot_label" "request_set_label" "manifest_index"]]
        (is (some? (schema/validation-errors schema-json (assoc index k "x")))
            (str "top-level " k " is rejected"))))
    (testing "identity-object retired keys are rejected"
      (doseq [k ["request_set_id" "source_snapshot_hash" "manifest_index_hash"
                 "tokenizer_profile_hashes" "analysis_recipe_hashes"
                 "parser_evidence_hashes"]]
        (is (some? (schema/validation-errors
                    schema-json
                    (assoc-in index ["snapshot_index_identity_object" k]
                              (h "aa"))))
            (str "identity key " k " is rejected"))))
    (testing "artifact references require work_slug"
      (is (some? (schema/validation-errors
                  schema-json
                  (update-in index ["artifact_references" 0] dissoc "work_slug")))))))

;; ── Checked-in example ──────────────────────────────────────────────────────

(deftest checked-in-example-validates-test
  (let [fixture (files/read-json "examples/v0/snapshot/snapshot-index.json")
        schema-json (files/read-json "schemas/snapshot-index.schema.json")]
    (is (nil? (schema/validation-errors schema-json fixture)))
    (is (true? (snapshot-index/validate-snapshot-index! fixture)))))

;; ── Completed-root fixture + closed-reference verification ───────────────────

(defn- write-manifest-and-content!
  "Write a content file plus its artifact manifest into work-dir, returning the
  built artifact reference with hashes coherent with the written bytes."
  [root work-dir {:keys [kind status content-name content-bytes sidecars slug-value]}]
  (let [content-file (io/file work-dir content-name)
        _ (io/make-parents content-file)
        _ (files/write-text! content-file content-bytes)
        sidecar-entries
        (mapv (fn [{:keys [role path-hint bytes]}]
                (let [sc-file (io/file work-dir path-hint)]
                  (files/write-text! sc-file bytes)
                  {"role" role
                   "hash" (str "sha256:" (files/sha256-file sc-file))
                   "media_type" "application/json"
                   "path_hint" path-hint}))
              sidecars)
        identity-object (manifest/identity-object
                         {"corpus_snapshot_hash" (h "c1")
                          "work_content_hash" (h "b1")}
                         {:manifest-schema-hash (manifest/schema-hash
                                                 "schemas/manifest.schema.json")
                          :output-format-spec-hash (h "0f")})
        manifest-value (manifest/artifact-manifest
                        {:artifact-kind kind
                         :validation-status status
                         :identity-object identity-object
                         :content (manifest/content content-file
                                                    "text/plain; charset=UTF-8"
                                                    content-name
                                                    files/sha256-file)
                         :sidecars sidecar-entries
                         :generated-at "2026-07-07T00:00:00Z"
                         :activity-id "https://w3id.org/abc/activity/test"
                         :agent "abc.tools.snapshot-index-test"
                         :plan-hash nil
                         :used []
                         :was-derived-from []
                         :notes nil})
        manifest-name (str kind ".manifest.json")
        manifest-file (io/file work-dir manifest-name)]
    (manifest/write-json-file! manifest-file manifest-value)
    {:manifest-file manifest-file
     :reference {"artifact_id" (get manifest-value "artifact_id")
                 "artifact_kind" kind
                 "work_slug" (or slug-value slug)
                 "sidecar_role" nil
                 "validation_status" status
                 "manifest_content_hash" (manifest/file-hash manifest-file)
                 "content_hash" (get-in manifest-value ["content" "content_hash"])
                 "locator" {"kind" "loose"
                            "path" (str (fs/relativize
                                         (fs/canonicalize root)
                                         (fs/canonicalize manifest-file)))}}}))

(defn build-completed-root!
  "Build a v0.2.0 completed publication root with source/parser-ir/plaintext/tei
  manifests, content, and a TEI sidecar under a single referenced per-work
  directory, plus a coherent snapshot-index.json. Returns {:root :index}."
  [root]
  (let [root (io/file root)
        work-dir (io/file root "publications" slug)
        _ (io/make-parents (io/file work-dir "x"))
        entries
        (mapv #(write-manifest-and-content! root work-dir %)
              [{:kind "source" :status "passed" :content-name "source-bundle.json"
                :content-bytes "{\"source\":true}" :sidecars []}
               {:kind "parser-ir" :status "passed" :content-name "parser-ir.json"
                :content-bytes "{\"ir\":true}" :sidecars []}
               {:kind "plaintext" :status "passed" :content-name "plain.txt"
                :content-bytes "本文" :sidecars []}
               {:kind "tei" :status "warning" :content-name "tei.xml"
                :content-bytes "<TEI/>"
                :sidecars [{:role "validation-result"
                            :path-hint "tei-validation-result.json"
                            :bytes "{\"status\":\"warning\"}"}]}])
        references (mapv :reference entries)
        index (snapshot-index/build-snapshot-index
               (build-args :artifact-references references))]
    (snapshot-index/write-snapshot-index!
     index (str (io/file root "snapshot-index.json")))
    {:root root :index index :work-dir work-dir}))

(deftest closure-problems-passes-for-completed-root-test
  (with-temp-dir [dir]
    (let [{:keys [root index]} (build-completed-root! (io/file dir "root"))]
      (is (true? (snapshot-index/validate-snapshot-index! index)))
      (is (empty? (snapshot-index/closure-problems root index))
          "a coherent completed root closes with no problems"))))

(deftest closure-problems-detects-tampered-content-test
  (with-temp-dir [dir]
    (let [{:keys [root index work-dir]} (build-completed-root! (io/file dir "root"))]
      (files/write-text! (io/file work-dir "plain.txt") "tampered")
      (is (some #(= "closure-content-hash-mismatch" (:code %))
                (snapshot-index/closure-problems root index))))))

(deftest closure-problems-detects-tampered-manifest-test
  (with-temp-dir [dir]
    (let [{:keys [root index work-dir]} (build-completed-root! (io/file dir "root"))
          manifest-file (io/file work-dir "source.manifest.json")
          tampered (assoc (files/read-json manifest-file) "notes" "tampered")]
      (manifest/write-json-file! manifest-file tampered)
      (is (some #(= "closure-manifest-content-hash-mismatch" (:code %))
                (snapshot-index/closure-problems root index))))))

(deftest closure-problems-detects-unreferenced-file-test
  (with-temp-dir [dir]
    (let [{:keys [root index work-dir]} (build-completed-root! (io/file dir "root"))]
      (files/write-text! (io/file work-dir "stray.txt") "stray")
      (is (some #(= "closure-unreferenced-file" (:code %))
                (snapshot-index/closure-problems root index))))))

(deftest closure-problems-detects-missing-manifest-test
  (with-temp-dir [dir]
    (let [{:keys [root index work-dir]} (build-completed-root! (io/file dir "root"))]
      (io/delete-file (io/file work-dir "tei.manifest.json"))
      (is (some #(= "closure-manifest-missing" (:code %))
                (snapshot-index/closure-problems root index))))))

(deftest closure-problems-allows-publications-report-test
  (with-temp-dir [dir]
    (let [{:keys [root index]} (build-completed-root! (io/file dir "root"))]
      ;; The derived report lives at the collection root, outside any referenced
      ;; per-work directory, so it is not scanned regardless.
      (files/write-text! (io/file root "publications" "publications-report.json")
                         "{\"report\":true}")
      (is (empty? (snapshot-index/closure-problems root index))))))
