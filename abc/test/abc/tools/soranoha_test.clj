(ns abc.tools.soranoha-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-publication :as materialize-publication]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.publication-release :as publication-release]
            [abc.tools.request-set-resolver :as resolver]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.soranoha :as soranoha]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.snapshot-index-test :as six]
            [abc.tools.source-bundle :as source-bundle]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]])
  (:import [java.io FileNotFoundException IOException InterruptedIOException]
           [java.nio.charset StandardCharsets]
           [java.nio.file AccessDeniedException NoSuchFileException]
           [java.util.zip ZipEntry ZipOutputStream]))

;; --replace is solely atomic installation policy: it governs whether an
;; existing output-root may be replaced by a successful build's temporary
;; root, never publication content reuse.
(deftest publication-filesystem-contract-test
  (fs/with-temp-dir [root {}]
    (let [output (fs/file root "out")
          tmp (#'build-publication/prepare-output-root! output false)]
      (is (instance? java.io.File tmp))
      (is (fs/directory? root))
      (fs/create-dirs output)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"output-root already exists"
                            (#'build-publication/prepare-output-root! output false)))
      (is (instance? java.io.File
                     (#'build-publication/prepare-output-root! output true))))
    (let [tmp (fs/file root "promotion.tmp")
          target (fs/file root "promotion")]
      (fs/create-dirs tmp)
      (spit (fs/file tmp "artifact") "ok")
      (let [result (#'build-publication/promote-output-root! tmp target false)]
        (is (instance? java.io.File result))
        (is (= "ok" (slurp (fs/file target "artifact"))))))))

(deftest work-zip-files-does-not-descend-through-directory-symlinks-test
  (fs/with-temp-dir [base {}]
    (let [root (fs/path base "root")
          external (fs/path base "external")]
      (fs/create-dirs root)
      (fs/create-dirs external)
      (spit (fs/file external "work.zip") "zip")
      (fs/create-sym-link (fs/path root "linked") external)
      (is (= []
             (#'build-publication/work-zip-files root))))))

(defn- delete-tree! [file]
  (fixture/delete-tree! file))

(defn- with-release-policy-allowed [f]
  (with-redefs [publication-policy/assert-release-allowed! (constantly :ok)]
    (f)))

(def ^:private build-publication-csv
  (str "作品ID,作品名,作品名読み,ソート用読み,副題,副題読み,原題,初出,"
       "分類番号,文字遣い種別,作品著作権フラグ,公開日,最終更新日,図書カードURL,"
       "人物ID,姓,名,姓読み,名読み,姓読みソート用,名読みソート用,"
       "姓ローマ字,名ローマ字,役割フラグ,生年月日,没年月日,人物著作権フラグ,"
       "底本名1,底本出版社名1,底本初版発行年1,入力に使用した版1,校正に使用した版1,"
       "底本の親本名1,底本の親本出版社名1,底本の親本初版発行年1,"
       "底本名2,底本出版社名2,底本初版発行年2,入力に使用した版2,校正に使用した版2,"
       "底本の親本名2,底本の親本出版社名2,底本の親本初版発行年2,"
       "入力者,校正者,テキストファイルURL,テキストファイル最終更新日,"
       "テキストファイル符号化方式,テキストファイル文字集合,テキストファイル修正回数,"
       "XHTML/HTMLファイルURL,XHTML/HTMLファイル最終更新日,"
       "XHTML/HTMLファイル符号化方式,XHTML/HTMLファイル文字集合,"
       "XHTML/HTMLファイル修正回数\n"
       "\"000001\",\"羅生門\",\"らしょうもん\",\"らしようもん\",\"\",\"\",\"\","
       "\"\",\"NDC 913\",\"新字新仮名\",\"なし\",\"1997-10-29\","
       "\"2022-07-16\",\"https://www.aozora.gr.jp/cards/000879/card1.html\","
       "\"000879\",\"芥川\",\"竜之介\",\"あくたがわ\",\"りゅうのすけ\","
       "\"あくたかわ\",\"りゆうのすけ\",\"Akutagawa\",\"Ryunosuke\","
       "\"著者\",\"1892-03-01\",\"1927-07-24\",\"なし\","
       "\"羅生門\",\"テスト出版社\",\"\",\"\",\"\",\"\",\"\",\"\","
       "\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"野口英司\",\"校正者\","
       "\"https://www.aozora.gr.jp/cards/000879/files/000001_ruby_fixture.zip\","
       "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\","
       "\"https://www.aozora.gr.jp/cards/000879/files/000001_15260.html\","
       "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\"\n"))

(defn- write-zip!
  [file entries]
  (.mkdirs (.getParentFile (io/file file)))
  (with-open [out (ZipOutputStream. (io/output-stream file))]
    (doseq [[name content] entries]
      (.putNextEntry out (ZipEntry. name))
      (.write out (.getBytes (str content) StandardCharsets/UTF_8))
      (.closeEntry out))))

(defn- write-repacked-zip!
  [file entries comment]
  (.mkdirs (.getParentFile (io/file file)))
  (with-open [out (ZipOutputStream. (io/output-stream file))]
    (.setComment out comment)
    (doseq [[name content] entries]
      (.putNextEntry out (doto (ZipEntry. name) (.setTime 0)))
      (.write out (.getBytes (str content) StandardCharsets/UTF_8))
      (.closeEntry out))))

(defn- official-aozora-fixture! [root]
  (let [catalog (io/file root "index_pages" "list_person_all_extended_utf8.zip")
        work-zip (io/file root "cards" "000879" "files"
                          "000001_ruby_fixture.zip")
        non-text-zip (io/file root "cards" "000879" "files"
                              "000001_images.zip")
        support-zip (io/file root "support" "tools.zip")]
    (write-zip! catalog {"list_person_all_extended_utf8.csv"
                         build-publication-csv})
    (write-zip! work-zip {"000001.txt" "本文です。"})
    (write-zip! non-text-zip {"cover.png" "not text"})
    (write-zip! support-zip {"README.txt" "support"})
    (.mkdirs (io/file root ".git"))
    (spit (io/file root ".git" "HEAD") "fixture-head\n")
    root))

(defn- stub-derive-parser-ir!
  "Test double for the adapter chain: writes a minimal, schema-valid parser-IR
  (empty body) so build-publication can be exercised without the aozora2html /
  ab-aat-to-parser-ir binaries."
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
                "encoding" "Shift_JIS" "normalization" "source"}
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

;; Parser resolution is independent of source trust: real-resolve-parser-runtime!
;; now resolves the real adapter for every trust mode, so a build with no adapter
;; binaries present must inject a well-formed (but unauthenticated) runtime
;; identity. Every build test binds the parser-runtime boundary to this stub;
;; the default is installed for the whole namespace by a fixture so individual
;; tests only bind the source→parser-IR derivation double.
(def ^:private fixture-parser-runtime-identity
  {"adapter_id" "aozora2html"
   "adapter_argv_template" build-publication/parser-argv-template
   "converter_argv_template" build-publication/converter-argv-template
   "parser_build_hash" (files/example-hash "70")
   "converter_build_hash" (files/example-hash "71")
   "aat_parser_ir_mapping_hash" (files/example-hash "72")
   "parser_ir_schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")})

(defn- fixed-parser-runtime
  "Injectable *resolve-parser-runtime!* double carrying a caller-supplied,
  authenticated-shaped CANONICAL runtime identity object: lets a test rotate
  exactly the parser/mapping coordinates a real profile/mapping/parser-build
  change would rotate, without needing the real adapter binaries for a second
  profile."
  [runtime-identity]
  (fn [_options]
    {:adapter nil
     :parser-runtime-identity runtime-identity
     :parser-config-hash (hash/format-sha256
                          (hash/sha256-json-jcs runtime-identity))
     :candidate-ref nil
     :qualification-identity-ref nil
     :problems []}))

(def ^:private stub-parser-runtime
  (fixed-parser-runtime fixture-parser-runtime-identity))

(use-fixtures :each
  (fn [f]
    (binding [build-publication/*resolve-parser-runtime!* stub-parser-runtime]
      (f))))

(deftest list-request-sets-prints-checked-in-labels-test
  (let [out (with-out-str
              (is (zero? (soranoha/run! ["list-request-sets"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out "full-corpus-basic-ja"))))

(deftest explain-request-set-prints-request-set-id-test
  (let [resolved (resolver/resolve-request-set "smoke-basic-ja")
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-request-set"
                                         "smoke-basic-ja"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out (get resolved "request_set_id")))
    (is (not (string/includes? out "fixture_role")))))

;; The producer commands (snapshot-index/reproduce/materialize-snapshot-root!)
;; were retired with the competing publication composition. The read-only
;; explain/validate projections stay and are characterized here against the
;; checked-in 0.2.0 example index (a read-only value, not a materialized root).
(def ^:private example-snapshot-index-path
  "examples/v0/snapshot/snapshot-index.json")

(deftest explain-snapshot-command-explains-checked-in-example-index-test
  (let [snapshot (files/read-json example-snapshot-index-path)
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-snapshot"
                                         example-snapshot-index-path]))))]
    (is (string/includes? out (get snapshot "snapshot_date")))
    (is (string/includes? out (get snapshot "snapshot_identity_hash")))
    (is (string/includes?
         out (get-in snapshot ["snapshot_index_identity_object"
                               "source_selection_hash"])))
    (is (string/includes? out "failure_rate:"))))

(deftest validate-command-validates-checked-in-example-index-test
  (let [snapshot (files/read-json example-snapshot-index-path)
        out (with-out-str
              (is (zero? (soranoha/run! ["validate"
                                         example-snapshot-index-path]))))]
    (is (string/includes? out "snapshot_valid: true"))
    (is (string/includes? out (get snapshot "snapshot_date")))
    (is (string/includes? out (get snapshot "snapshot_identity_hash")))))

(deftest validate-on-a-root-recomputes-admissibility-and-ignores-report-test
  ;; The retained validate/explain-snapshot projections recompute current
  ;; admissibility over the installed root; they never trust or rewrite the
  ;; build-time publications-report.json.
  (with-temp-dir [dir]
    (let [{:keys [root]} (six/build-completed-root! (io/file dir "root"))]
      (files/write-text! (io/file root "publications" "publications-report.json")
                         "{\"admissible?\":true,\"problems\":[]}")
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["validate" (str root)]))))]
        (is (string/includes? out "snapshot_valid: true"))
        (is (string/includes? out "release_admissible: false")
            "the fixture root is recomputed as inadmissible despite the asserted report")
        (is (string/includes? out "rights_policy_file_hash:")))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["explain-snapshot" (str root)]))))]
        (is (string/includes? out "release_admissible: false"))
        (is (string/includes? out "decisions_file_hash:"))))))

(deftest source-snapshot-command-generates-workset-and-snapshot-test
  (let [root (fixture/temp-dir "abc-soranoha-source-snapshot")
        input-root (io/file root "materialized")
        output-root (io/file root "source-snapshot")]
    (try
      (fixture/materialized-work! input-root
                                  {:slug "alpha"
                                   :title "一"
                                   :work-id "000001"
                                   :person-id "000101"
                                   :work-hash (fixture/example-hash "a1")})
      (let [out (with-out-str
                  (is (zero? (soranoha/run!
                              ["source-snapshot"
                               (str input-root)
                               (str output-root)
                               "unit-test-source-snapshot"
                               "2026-07-07"]))))
            workset-file (io/file output-root "source-snapshot.workset.edn")
            snapshot-file (io/file output-root "source-snapshot.json")
            source-manifest (io/file input-root
                                     "works"
                                     "alpha"
                                     "source.manifest.json")]
        (is (.exists workset-file))
        (is (.exists snapshot-file))
        (is (.exists source-manifest))
        (let [snapshot (files/read-json snapshot-file)]
          (is (= "unit-test-source-snapshot"
                 (get-in snapshot ["snapshot_identity_object"
                                   "snapshot_scope"])))
          (is (= "source"
                 (get (files/read-json source-manifest)
                      "artifact_kind")))
          (is (string/includes? out (str snapshot-file)))
          (is (string/includes? out (get snapshot "snapshot_hash")))))
      (finally
        (delete-tree! root)))))

(deftest resolve-request-set-command-uses-generated-source-snapshot-test
  (let [root (fixture/temp-dir "abc-soranoha-resolve-source-snapshot")
        input-root (io/file root "materialized")
        source-snapshot-root (io/file root "source-snapshot")
        request-set-file (io/file root "full-corpus-basic-ja.json")]
    (try
      (fixture/materialized-work! input-root
                                  {:slug "alpha"
                                   :title "一"
                                   :work-id "000001"
                                   :person-id "000101"
                                   :work-hash (fixture/example-hash "a1")})
      (with-out-str
        (is (zero? (soranoha/run!
                    ["source-snapshot"
                     (str input-root)
                     (str source-snapshot-root)
                     "unit-test-source-snapshot"
                     "2026-07-07"]))))
      (let [snapshot-file (io/file source-snapshot-root "source-snapshot.json")
            out (with-out-str
                  (is (zero? (soranoha/run!
                              ["resolve-request-set"
                               "full-corpus-basic-ja"
                               (str request-set-file)
                               (str snapshot-file)]))))
            resolved (files/read-json request-set-file)
            snapshot (files/read-json snapshot-file)]
        (is (.exists request-set-file))
        (is (= (get snapshot "snapshot_hash")
               (get-in resolved ["request_set_identity_object"
                                 "corpus_snapshot_hash"])))
        (is (= 1
               (count (get-in resolved ["request_set_identity_object"
                                        "subjects"]))))
        (is (string/includes? out (str request-set-file)))
        (is (string/includes? out (get resolved "request_set_id")))
        (is (string/includes? out "subjects_count: 1")))
      (finally
        (delete-tree! root)))))

(deftest build-publication-command-materializes-real-publications-test
  (let [root (fixture/temp-dir "abc-soranoha-build-publication")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")]
    (try
      (let [out (with-redefs [publication-policy/assert-release-allowed!
                              (constantly :ok)]
                  (binding [build-publication/*derive-parser-ir!*
                            stub-derive-parser-ir!]
                    (with-out-str
                      ;; A fixture-trust build is NOT release-admissible; it
                      ;; installs an inspectable diagnostic index-v2 root and
                      ;; exits 1.
                      (is (= 1 (soranoha/run!
                                ["build-publication"
                                 "--aozora-root" (str aozora-root)
                                 "--config" "abc/config/publication-basic-ja.json"
                                 "--snapshot-date" "2026-07-08"
                                 "--output-root" (str output-root)]))))))
            slug "000001_000879_000001_ruby_fixture"
            work-dir (io/file output-root "materialized-root" "works" slug)
            pub-dir (io/file output-root "publications" slug)
            official-source-file (io/file work-dir "official-source.json")
            source-bundle-file (io/file work-dir "source-bundle.json")
            parser-ir-file (io/file work-dir "parser-ir.json")
            source-manifest-file (io/file pub-dir "source.manifest.json")
            source-selection-report-file (io/file output-root
                                                  "source-selection-report.json")
            build-workflow-run-file (io/file output-root "workflow-run.json")
            pub-dir (io/file output-root "publications" slug)
            tei-file (io/file pub-dir "tei.xml")
            tei-validation-file (io/file pub-dir "tei-validation-result.json")
            publications-report-file (io/file output-root "publications"
                                              "publications-report.json")]
        (is (string/includes? out "build_publication_root:"))
        (is (string/includes? out "release_admissible: false"))
        (is (.exists official-source-file))
        (is (.exists source-bundle-file))
        (is (.exists parser-ir-file))
        (is (.exists source-manifest-file))
        (is (.exists source-selection-report-file))
        (is (.exists build-workflow-run-file))
        (is (.exists tei-file))
        ;; The source manifest publication materialization requires now exists.
        (let [source-manifest (files/read-json source-manifest-file)]
          (is (string/starts-with?
               (get-in source-manifest
                       ["manifest_identity_object" "corpus_snapshot_hash"])
               "sha256:")))
        ;; The TEI is really validated, not stubbed away.
        (let [validation (files/read-json tei-validation-file)]
          (is (= "passed" (get validation "status"))))
        (let [report (files/read-json source-selection-report-file)
              selection (first (get report "selected_sources"))
              official-source (files/read-json official-source-file)
              source-bundle (files/read-json source-bundle-file)
              parser-ir (files/read-json parser-ir-file)
              primary-member (get official-source "primary_text_member")
              primary-member-hash
              (some #(when (= primary-member (get % "path"))
                       (get % "member_hash"))
                    (get source-bundle "members"))]
          (is (= 1 (get report "selected_source_count")))
          (is (not (contains? report "release_admissible"))
              "source-selection-report no longer carries a locally-derived verdict")
          (is (<= 2 (get report "rejected_source_count")))
          (is (= ["cards/000879/files/000001_ruby_fixture.zip"]
                 (mapv #(get % "text_zip_relpath")
                       (get report "selected_sources"))))
          (is (= "cards/000879/files/000001_ruby_fixture.zip"
                 (get official-source "text_zip_relpath")))
          (is (= (get official-source "archive_hash")
                 (get official-source "source_hash")))
          (is (= (get source-bundle "archive_hash")
                 (get official-source "archive_hash")
                 (get selection "archive_hash")
                 (get selection "source_hash")))
          (is (= (get source-bundle "bundle_hash")
                 (get official-source "bundle_hash")
                 (get selection "bundle_hash")
                 (get-in parser-ir ["source" "work_content_hash"])
                 (get-in (files/read-json source-manifest-file)
                         ["manifest_identity_object" "work_content_hash"])))
          (is (= (get official-source "primary_text_hash")
                 (get selection "primary_text_hash")
                 (get-in parser-ir ["source" "primary_text_hash"])
                 primary-member-hash))
          (is (= primary-member
                 (get-in source-bundle ["identity_object" "primary_text_member"])
                 (get selection "primary_text_member"))))
        (let [publications-report (files/read-json publications-report-file)]
          (is (= 1 (get publications-report "publication_count")))
          (is (= 1 (get publications-report "passed")))
          (is (= 0 (get publications-report "failed")))
          ;; The verifier result is recorded in publications-report.json: a
          ;; fixture-trust build is inadmissible on source trust and rights.
          (is (false? (get publications-report "release_admissible")))
          (is (contains? (set (map #(get % "code")
                                   (get publications-report "release_problems")))
                         "release-source-not-official")))
        (let [workflow-run (files/read-json build-workflow-run-file)]
          (is (= "soranoha.build-publication.v1"
                 (get workflow-run "workflow_id")))
          (is (= "passed" (get workflow-run "status")))
          (is (= ["materialize-source-selection"
                  "write-build-records"
                  "materialize-publications"]
                 (mapv #(get % "id") (get workflow-run "steps"))))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-installs-and-exits-zero-when-admissible-test
  ;; A fully-authorized release installs the root atomically and exits 0. The
  ;; release-facing exit is controlled solely by the fresh verify-release-root!
  ;; result; here it is an in-memory admissible verdict.
  (let [root (fixture/temp-dir "abc-soranoha-build-admissible")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")]
    (try
      (let [out (binding [build-publication/*derive-parser-ir!* stub-derive-parser-ir!]
                  (with-redefs
                   [publication-release/verify-release-root!
                    (fn [_]
                      {:admissible? true
                       :problems []
                       :authority-hashes
                       {:decisions-file (files/example-hash "d1")
                        :record-file (files/example-hash "d2")
                        :rights-policy-file (files/example-hash "d3")}})]
                    (with-out-str
                      (is (= 0 (soranoha/run!
                                ["build-publication"
                                 "--aozora-root" (str aozora-root)
                                 "--config" "abc/config/publication-basic-ja.json"
                                 "--snapshot-date" "2026-07-08"
                                 "--output-root" (str output-root)]))))))]
        (is (string/includes? out "release_admissible: true"))
        (is (.exists output-root))
        (let [report (files/read-json (io/file output-root "publications"
                                               "publications-report.json"))]
          (is (true? (get report "release_admissible")))
          (is (empty? (get report "release_problems")))
          (is (= (files/example-hash "d3")
                 (get-in report ["authority_hashes" "rights-policy-file"]))))
        (is (true? (snapshot-index/validate-snapshot-index!
                    (files/read-json (io/file output-root
                                              "snapshot-index.json"))))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-report-records-and-recomputes-verifier-result-test
  ;; Re-establishes publication-report coverage: the build-time verifier result
  ;; is recorded in publications/publications-report.json AND independently
  ;; recomputable from the installed root. For a fixture-trust build both agree
  ;; it is inadmissible, with the same problem codes.
  (let [root (fixture/temp-dir "abc-soranoha-build-report-coverage")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")]
    (try
      (binding [build-publication/*derive-parser-ir!* stub-derive-parser-ir!]
        (with-out-str
          (is (= 1 (soranoha/run!
                    ["build-publication"
                     "--aozora-root" (str aozora-root)
                     "--config" "abc/config/publication-basic-ja.json"
                     "--snapshot-date" "2026-07-08"
                     "--output-root" (str output-root)])))))
      (let [report (files/read-json (io/file output-root "publications"
                                             "publications-report.json"))
            recomputed (publication-release/verify-release-root!
                        {:root (str output-root)
                         :parser-authority-sources
                         build-publication/release-authority-sources
                         :rights-policy-path publication-policy/policy-path})]
        (is (false? (get report "release_admissible")))
        (is (= (:admissible? recomputed) (get report "release_admissible"))
            "recomputing admissibility from the installed root agrees with the report")
        (is (= (set (map #(get % "code") (get report "release_problems")))
               (set (map :code (:problems recomputed))))
            "the recorded and recomputed problem codes agree")
        (is (contains? (set (map :code (:problems recomputed)))
                       "release-source-not-official")))
      (finally
        (delete-tree! root)))))

(defn- strip-generated-at
  "Removes the run-varying generated_at provenance field (and, for the TEI
  manifest, the preservation-sidecar content hash that transitively varies
  with it, since preservation.json itself embeds generated_at) from a
  manifest or preservation JSON value, so two independently-timestamped
  renders can be compared for structural equivalence."
  [m]
  (cond-> m
    (contains? m "provenance") (update "provenance" dissoc "generated_at")
    (contains? m "producer") (update "producer" dissoc "generated_at")
    (contains? m "sidecars")
    (update "sidecars"
            (fn [sidecars]
              (mapv (fn [sidecar]
                      (cond-> sidecar
                        (= "preservation" (get sidecar "role"))
                        (dissoc "hash")))
                    sidecars)))))

(deftest build-publication-equals-direct-materialize-publication-invocation-test
  (let [root (fixture/temp-dir "abc-soranoha-build-vs-materialize")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        direct-output (io/file root "direct-publication")]
    (try
      (with-release-policy-allowed
        #(binding [build-publication/*derive-parser-ir!* stub-derive-parser-ir!]
           (with-out-str
             (is (= 1 (soranoha/run!
                       ["build-publication"
                        "--aozora-root" (str aozora-root)
                        "--config" "abc/config/publication-basic-ja.json"
                        "--snapshot-date" "2026-07-08"
                        "--output-root" (str output-root)]))))))
      (let [slug "000001_000879_000001_ruby_fixture"
            work-dir (io/file output-root "materialized-root" "works" slug)
            build-pub-dir (io/file output-root "publications" slug)
            build-plan (files/read-json (io/file output-root "build-plan.json"))
            parser-runtime (get build-plan "parser_runtime")
            runtime-identity (get parser-runtime "parser_runtime_identity_object")
            ;; The authenticated parser runtime identity is computed ONCE and the
            ;; build injects it into every work's manifest; a bare
            ;; materialize-publication! call must be given the SAME (canonical)
            ;; value to stay byte-equivalent to what the build wrote.
            parser-identity {:parser-build-hash (get runtime-identity
                                                     "parser_build_hash")
                             :parser-config-hash (get parser-runtime
                                                      "parser_config_hash")
                             :mapping-hash (get runtime-identity
                                                "aat_parser_ir_mapping_hash")
                             :parser-ir-schema-hash (get runtime-identity
                                                         "parser_ir_schema_hash")}]
        ;; The direct build never invokes a private/duplicated rendering path:
        ;; feeding materialize-publication! the same inputs (including parser
        ;; identity) the build wrote for this work reproduces the same
        ;; publication artifact set, up to the generated-at provenance
        ;; timestamp each invocation was given.
        (materialize-publication/materialize-publication!
         {:parser-ir-path (str (io/file work-dir "parser-ir.json"))
          :source-manifest-path (str (io/file output-root "publications" slug
                                              "source.manifest.json"))
          :metadata-record-path (str (io/file work-dir "metadata-record.json"))
          :persons-dir (str (io/file output-root "materialized-root" "persons"))
          :output-dir (str direct-output)
          :parser-identity parser-identity})
        (is (= (slurp (io/file build-pub-dir "plain.txt"))
               (slurp (io/file direct-output "plain.txt"))))
        (is (= (slurp (io/file build-pub-dir "tei.xml"))
               (slurp (io/file direct-output "tei.xml"))))
        (is (= (strip-generated-at
                (files/read-json (io/file build-pub-dir "plaintext.manifest.json")))
               (strip-generated-at
                (files/read-json (io/file direct-output "plaintext.manifest.json")))))
        (is (= (strip-generated-at
                (files/read-json (io/file build-pub-dir "tei.manifest.json")))
               (strip-generated-at
                (files/read-json (io/file direct-output "tei.manifest.json")))))
        (is (= (strip-generated-at
                (files/read-json (io/file build-pub-dir "preservation.json")))
               (strip-generated-at
                (files/read-json (io/file direct-output "preservation.json")))))
        (is (= (files/read-json (io/file build-pub-dir "tei-validation-result.json"))
               (files/read-json (io/file direct-output "tei-validation-result.json"))))
        ;; The two invocations were in fact given different generated-at
        ;; values, so the strip above is load-bearing, not a no-op.
        (is (not= (get-in (files/read-json (io/file build-pub-dir "tei.manifest.json"))
                          ["provenance" "generated_at"])
                  (get-in (files/read-json (io/file direct-output "tei.manifest.json"))
                          ["provenance" "generated_at"]))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-second-identical-build-rerenders-same-artifact-ids-test
  ;; Same snapshot-date on both builds (a literal identical-build retry, e.g.
  ;; after an unrelated downstream failure): corpus_snapshot_hash — and
  ;; therefore generated_at, which this build derives from snapshot-date —
  ;; coincide too, so a passing SUT must reproduce the FULL manifest
  ;; byte-for-byte, not merely its artifact_id. Before this task, the second
  ;; build would have returned "skipped" from the manifest-reuse cache
  ;; instead of re-rendering at all.
  (let [root (fixture/temp-dir "abc-soranoha-build-identical-rebuild")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        slug "000001_000879_000001_ruby_fixture"
        build! (fn [replace?]
                 (with-release-policy-allowed
                   #(binding [build-publication/*derive-parser-ir!*
                              stub-derive-parser-ir!]
                      (with-out-str
                        (is (= 1 (soranoha/run!
                                  (cond-> ["build-publication"
                                           "--aozora-root" (str aozora-root)
                                           "--config" "abc/config/publication-basic-ja.json"
                                           "--snapshot-date" "2026-07-08"
                                           "--output-root" (str output-root)]
                                    replace? (conj "--replace")))))))))]
    (try
      (build! false)
      (let [first-report (files/read-json
                          (io/file output-root "publications"
                                   "publications-report.json"))
            first-manifest (files/read-json
                            (io/file output-root "publications" slug
                                     "tei.manifest.json"))]
        (build! true)
        (let [second-report (files/read-json
                             (io/file output-root "publications"
                                      "publications-report.json"))
              second-manifest (files/read-json
                               (io/file output-root "publications" slug
                                        "tei.manifest.json"))]
          (testing "both builds re-render — never reused or skipped"
            (is (= #{"passed"}
                   (set (map #(get % "status") (get first-report "publications")))
                   (set (map #(get % "status") (get second-report "publications")))))
            (is (= 0 (get first-report "failed") (get second-report "failed")))
            (is (not (contains? (set (keys first-report)) "reused")))
            (is (not (contains? (set (keys first-report)) "skipped"))))
          (testing "an identical rebuild reproduces the same artifact_id (and full manifest)"
            (is (= (get first-manifest "artifact_id")
                   (get second-manifest "artifact_id")))
            (is (= first-manifest second-manifest)))))
      (finally
        (delete-tree! root)))))

(deftest materialize-publication-artifact-id-is-independent-of-generated-at-test
  ;; Isolates the "normalizing generated_at" half of the same claim at the
  ;; renderer boundary, where generated_at is free to vary independently of
  ;; every other identity input (in the CLI build it is pinned to
  ;; snapshot-date, so it cannot vary on its own there): re-rendering the
  ;; SAME upstream artifacts with a different generated_at must reproduce the
  ;; same artifact_id, with the full manifest equal once generated_at (and the
  ;; provenance-derived preservation-sidecar hash) is stripped.
  (let [root (fixture/temp-dir "abc-soranoha-materialize-generated-at")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        second-output (io/file root "second-render")
        slug "000001_000879_000001_ruby_fixture"]
    (try
      (with-release-policy-allowed
        #(binding [build-publication/*derive-parser-ir!* stub-derive-parser-ir!]
           (with-out-str
             (is (= 1 (soranoha/run!
                       ["build-publication"
                        "--aozora-root" (str aozora-root)
                        "--config" "abc/config/publication-basic-ja.json"
                        "--snapshot-date" "2026-07-08"
                        "--output-root" (str output-root)]))))))
      (let [work-dir (io/file output-root "materialized-root" "works" slug)
            first-manifest (files/read-json
                            (io/file output-root "publications" slug
                                     "tei.manifest.json"))
            build-plan (files/read-json (io/file output-root "build-plan.json"))
            parser-runtime (get build-plan "parser_runtime")
            runtime-identity (get parser-runtime "parser_runtime_identity_object")
            parser-identity {:parser-build-hash (get runtime-identity
                                                     "parser_build_hash")
                             :parser-config-hash (get parser-runtime
                                                      "parser_config_hash")
                             :mapping-hash (get runtime-identity
                                                "aat_parser_ir_mapping_hash")
                             :parser-ir-schema-hash (get runtime-identity
                                                         "parser_ir_schema_hash")}]
        (materialize-publication/materialize-publication!
         {:parser-ir-path (str (io/file work-dir "parser-ir.json"))
          :source-manifest-path (str (io/file output-root "publications" slug
                                              "source.manifest.json"))
          :metadata-record-path (str (io/file work-dir "metadata-record.json"))
          :persons-dir (str (io/file output-root "materialized-root" "persons"))
          :output-dir (str second-output)
          :generated-at "2099-01-01T00:00:00Z"
          :parser-identity parser-identity})
        (let [second-manifest (files/read-json
                               (io/file second-output "tei.manifest.json"))]
          (is (not= (get-in first-manifest ["provenance" "generated_at"])
                    (get-in second-manifest ["provenance" "generated_at"])))
          (is (= (get first-manifest "artifact_id")
                 (get second-manifest "artifact_id")))
          (is (= (strip-generated-at first-manifest)
                 (strip-generated-at second-manifest)))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-parser-identity-change-rotates-artifact-id-test
  (let [root (fixture/temp-dir "abc-soranoha-build-parser-identity-rotation")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        slug "000001_000879_000001_ruby_fixture"
        base-identity {"adapter_id" "aozora2html"
                       "adapter_argv_template" build-publication/parser-argv-template
                       "converter_argv_template"
                       build-publication/converter-argv-template
                       "parser_build_hash" (files/example-hash "70")
                       "converter_build_hash" (files/example-hash "71")
                       "aat_parser_ir_mapping_hash" (files/example-hash "72")
                       "parser_ir_schema_hash"
                       (manifest/schema-hash "schemas/parser-ir.schema.json")}
        ;; Represents what changes for a different parser build, a different
        ;; aat->parser-IR mapping pin, or a different profile: the mapping
        ;; coordinate rotates while the source content is untouched.
        changed-identity (assoc base-identity "aat_parser_ir_mapping_hash"
                                (files/example-hash "73"))
        build! (fn [output-root runtime-identity]
                 (with-release-policy-allowed
                   #(binding [build-publication/*derive-parser-ir!*
                              stub-derive-parser-ir!
                              build-publication/*resolve-parser-runtime!*
                              (fixed-parser-runtime runtime-identity)]
                      (build-publication/build-publication!
                       {:aozora-root (str aozora-root)
                        :config "abc/config/publication-basic-ja.json"
                        :snapshot-date "2026-07-08"
                        :output-root (str output-root)}))))
        base-out (io/file root "build-base")
        changed-out (io/file root "build-changed")]
    (try
      (build! base-out base-identity)
      (build! changed-out changed-identity)
      (let [base-manifest (files/read-json
                           (io/file base-out "publications" slug
                                    "tei.manifest.json"))
            changed-manifest (files/read-json
                              (io/file changed-out "publications" slug
                                       "tei.manifest.json"))]
        (testing "identical content"
          (is (= (get-in base-manifest ["manifest_identity_object"
                                        "work_content_hash"])
                 (get-in changed-manifest ["manifest_identity_object"
                                           "work_content_hash"]))))
        (testing "a changed parser/mapping coordinate rotates the identity object and artifact_id"
          (is (not= (get-in base-manifest ["manifest_identity_object"
                                           "aat_parser_ir_mapping_hash"])
                    (get-in changed-manifest ["manifest_identity_object"
                                              "aat_parser_ir_mapping_hash"])))
          (is (not= (get base-manifest "artifact_id")
                    (get changed-manifest "artifact_id")))))
      (finally
        (delete-tree! root)))))

(defn- two-work-aozora-fixture!
  "official-aozora-fixture! plus a second catalog-backed work so parallel
  ordering has something to scramble."
  [root]
  (official-aozora-fixture! root)
  (let [second-row (str "\"000002\",\"鼻\",\"はな\",\"はな\",\"\",\"\",\"\","
                        "\"\",\"NDC 913\",\"新字新仮名\",\"なし\",\"1997-10-29\","
                        "\"2022-07-16\",\"https://www.aozora.gr.jp/cards/000879/card2.html\","
                        "\"000879\",\"芥川\",\"竜之介\",\"あくたがわ\",\"りゅうのすけ\","
                        "\"あくたかわ\",\"りゆうのすけ\",\"Akutagawa\",\"Ryunosuke\","
                        "\"著者\",\"1892-03-01\",\"1927-07-24\",\"なし\","
                        "\"鼻\",\"テスト出版社\",\"\",\"\",\"\",\"\",\"\",\"\","
                        "\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"野口英司\",\"校正者\","
                        "\"https://www.aozora.gr.jp/cards/000879/files/000002_ruby_fixture.zip\","
                        "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\","
                        "\"https://www.aozora.gr.jp/cards/000879/files/000002_15261.html\","
                        "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\"\n")]
    (write-zip! (io/file root "index_pages" "list_person_all_extended_utf8.zip")
                {"list_person_all_extended_utf8.csv"
                 (str build-publication-csv second-row)})
    (write-zip! (io/file root "cards" "000879" "files"
                         "000002_ruby_fixture.zip")
                {"000002.txt" "第二の本文です。"}))
  root)

(defn- tree-file-hashes
  "relative-path -> sha256 for every file under root, excluding the run-varying
  records (concurrency in build-plan.json; timestamps in workflow-*.json)."
  [root]
  (let [root-file (io/file root)
        excluded #{"build-plan.json" "workflow-run.json" "workflow-plan.json"}]
    (->> (file-seq root-file)
         (filter #(.isFile ^java.io.File %))
         (remove #(excluded (.getName ^java.io.File %)))
         (map (fn [^java.io.File f]
                [(str (.relativize (.toPath root-file) (.toPath f)))
                 (files/sha256-file (str f))]))
         (into (sorted-map)))))

(deftest build-publication-concurrency-is-recorded-and-deterministic-test
  (let [root (fixture/temp-dir "abc-soranoha-build-concurrency")
        aozora-root (two-work-aozora-fixture! (io/file root "aozorabunko"))
        run! (fn [output-root concurrency-arg]
               (with-redefs [publication-policy/assert-release-allowed!
                             (constantly :ok)]
                 (binding [build-publication/*derive-parser-ir!*
                           stub-derive-parser-ir!]
                   (with-out-str
                     (is (= 1 (soranoha/run!
                               (cond-> ["build-publication"
                                        "--aozora-root" (str aozora-root)
                                        "--config" "abc/config/publication-basic-ja.json"
                                        "--snapshot-date" "2026-07-12"
                                        "--output-root" (str output-root)]
                                 concurrency-arg
                                 (into ["--concurrency" concurrency-arg])))))))))
        sequential-root (io/file root "out-sequential")
        parallel-root (io/file root "out-parallel")
        default-root (io/file root "out-default")]
    (try
      (run! sequential-root "1")
      (run! parallel-root "4")
      (run! default-root nil)
      (testing "resolved concurrency is recorded in build-plan.json"
        (is (= 1 (get (files/read-json (io/file sequential-root "build-plan.json"))
                      "concurrency")))
        (is (= 4 (get (files/read-json (io/file parallel-root "build-plan.json"))
                      "concurrency")))
        (is (= (.availableProcessors (Runtime/getRuntime))
               (get (files/read-json (io/file default-root "build-plan.json"))
                    "concurrency"))
            "absent flag must resolve to all cores"))
      (testing "parallel output is byte-identical to sequential output"
        (let [sequential-tree (tree-file-hashes sequential-root)
              parallel-tree (tree-file-hashes parallel-root)]
          (is (= 2 (get (files/read-json
                         (io/file sequential-root "publications"
                                  "publications-report.json"))
                        "publication_count")))
          (is (= (keys sequential-tree) (keys parallel-tree)))
          (is (= sequential-tree parallel-tree))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-metadata-repack-preserves-logical-identity-test
  (let [root (fixture/temp-dir "abc-soranoha-build-repack")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        work-zip (io/file aozora-root "cards" "000879" "files"
                          "000001_ruby_fixture.zip")
        output-a (io/file root "build-a")
        output-b (io/file root "build-b")
        build! (fn [output-root snapshot-date]
                 (with-release-policy-allowed
                   #(binding [build-publication/*derive-parser-ir!*
                              stub-derive-parser-ir!]
                      (is (= 1 (soranoha/run!
                                ["build-publication"
                                 "--aozora-root" (str aozora-root)
                                 "--config" "abc/config/publication-basic-ja.json"
                                 "--snapshot-date" snapshot-date
                                 "--output-root" (str output-root)]))))))
        identity (fn [output-root]
                   (let [work-dir (io/file output-root "materialized-root" "works"
                                           "000001_000879_000001_ruby_fixture")]
                     (files/read-json (io/file work-dir "official-source.json"))))]
    (try
      (write-repacked-zip! work-zip {"000001.txt" "本文です。"} "repack-a")
      (build! output-a "2026-07-08")
      (write-repacked-zip! work-zip {"000001.txt" "本文です。"} "repack-b")
      (build! output-b "2026-07-09")
      (let [a (identity output-a)
            b (identity output-b)]
        (is (not= (get a "archive_hash") (get b "archive_hash")))
        (is (= (get a "bundle_hash") (get b "bundle_hash")))
        (is (= (get a "primary_text_hash") (get b "primary_text_hash"))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-best-effort-does-not-suppress-parser-identity-mismatch-test
  (let [root (fixture/temp-dir "abc-soranoha-build-identity-mismatch")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        config-file (io/file root "strict-config.json")
        mismatching-stub
        (fn [opts]
          (stub-derive-parser-ir! opts)
          (let [parser-ir-file (:parser-ir-file opts)
                parser-ir (files/read-json parser-ir-file)]
            (abc-json/write-deterministic-json-file!
             parser-ir-file
             (-> parser-ir
                 (assoc-in ["source" "work_content_hash"]
                           (files/example-hash "31"))
                 (assoc-in ["source" "primary_text_hash"]
                           (files/example-hash "32"))))))]
    (try
      (abc-json/write-deterministic-json-file!
       config-file
       (assoc (files/read-json (io/file "config" "publication-basic-ja.json"))
              "continue_on_failure" true))
      (let [thrown (try
                     (with-release-policy-allowed
                       #(binding [build-publication/*derive-parser-ir!*
                                  mismatching-stub]
                          (build-publication/build-publication!
                           {:aozora-root (str aozora-root)
                            :config (str config-file)
                            :snapshot-date "2026-07-08"
                            :output-root (str output-root)})))
                     nil
                     (catch clojure.lang.ExceptionInfo t t))
            identity-data (some #(let [data (ex-data %)]
                                   (when (contains? data
                                                    :expected-work-content-hash)
                                     data))
                                (take-while some?
                                            (iterate #(.getCause ^Throwable %)
                                                     thrown)))]
        (is (some? thrown))
        (is (some? identity-data))
        (is (= #{:expected-work-content-hash :actual-work-content-hash
                 :expected-primary-text-hash :actual-primary-text-hash}
               (set (keys (select-keys identity-data
                                       [:expected-work-content-hash
                                        :actual-work-content-hash
                                        :expected-primary-text-hash
                                        :actual-primary-text-hash]))))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-best-effort-propagates-non-admission-failures-test
  (let [root (fixture/temp-dir "abc-soranoha-build-error-taxonomy")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        config-file (io/file root "best-effort-config.json")]
    (try
      (abc-json/write-deterministic-json-file!
       config-file
       (assoc (files/read-json (io/file "config" "publication-basic-ja.json"))
              "continue_on_failure" true))
      (doseq [failure [(AssertionError. "programming")
                       (LinkageError. "linkage")
                       (InterruptedException. "interrupted")
                       (InterruptedIOException. "interrupted")
                       (FileNotFoundException. "missing")
                       (NoSuchFileException. "missing")
                       (AccessDeniedException. "denied")
                       (RuntimeException. "programming")
                       (ex-info "spoofed admission reason"
                                {:reason :unsafe-member-path
                                 :archive-path "not-from-inspector.zip"})
                       (ex-info "wrapper" {:wrapper true}
                                (ex-info "wrapped spoofed admission reason"
                                         {:reason :unsafe-member-path
                                          :archive-path
                                          "not-from-inspector.zip"}))]]
        (is (identical?
             failure
             (try
               (with-redefs [source-bundle/inspect-zip
                             (fn [& _] (throw failure))]
                 (with-release-policy-allowed
                   #(build-publication/build-publication!
                     {:aozora-root (str aozora-root)
                      :config (str config-file)
                      :snapshot-date "2026-07-08"
                      :output-root (str output-root)})))
               (catch Throwable t t)))))
      (is (not (.exists output-root)))
      (finally
        (delete-tree! root)))))

(deftest build-publication-best-effort-does-not-traverse-protected-wrappers-test
  (let [root (fixture/temp-dir "abc-soranoha-build-protected-wrappers")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        unsafe-zip (io/file root "unsafe.zip")
        output-root (io/file root "build-output")
        config-file (io/file root "best-effort-config.json")]
    (try
      (write-zip! unsafe-zip {"../work.txt" "unsafe"})
      (abc-json/write-deterministic-json-file!
       config-file
       (assoc (files/read-json (io/file "config" "publication-basic-ja.json"))
              "continue_on_failure" true))
      (let [admission (try
                        (source-bundle/inspect-zip unsafe-zip)
                        nil
                        (catch clojure.lang.ExceptionInfo t t))
            wrap (fn [outer]
                   (.initCause ^Throwable outer admission)
                   outer)
            protected-outers
            [(wrap (AssertionError. "assertion"))
             (wrap (Error. "error"))
             (wrap (LinkageError. "linkage"))
             (wrap (InterruptedException. "interrupted"))
             (wrap (InterruptedIOException. "interrupted IO"))
             (wrap (IOException. "filesystem IO"))
             (wrap (FileNotFoundException. "missing"))
             (wrap (NoSuchFileException. "missing"))
             (wrap (AccessDeniedException. "denied"))
             (RuntimeException. "programming" admission)]]
        (is (some? admission))
        (is (true? (::source-bundle/admission-error (ex-data admission))))
        (doseq [outer protected-outers]
          (is (identical?
               outer
               (try
                 (with-redefs [source-bundle/inspect-zip
                               (fn [& _] (throw outer))]
                   (with-release-policy-allowed
                     #(build-publication/build-publication!
                       {:aozora-root (str aozora-root)
                        :config (str config-file)
                        :snapshot-date "2026-07-08"
                        :output-root (str output-root)})))
                 (catch Throwable t t))))))
      (is (not (.exists output-root)))
      (finally
        (delete-tree! root)))))

(deftest build-publication-best-effort-still-rejects-zero-attempted-candidates-test
  (let [root (fixture/temp-dir "abc-soranoha-build-no-candidates")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        work-zip (io/file aozora-root "cards" "000879" "files"
                          "000001_ruby_fixture.zip")
        output-root (io/file root "build-output")]
    (try
      (is (.delete work-zip))
      (let [thrown (try
                     (with-release-policy-allowed
                       #(build-publication/build-publication!
                         {:aozora-root (str aozora-root)
                          :config "abc/config/publication-basic-ja.json"
                          :snapshot-date "2026-07-08"
                          :output-root (str output-root)}))
                     nil
                     (catch clojure.lang.ExceptionInfo t t))]
        (is (some? thrown))
        (is (= "no catalog-backed work ZIPs were successfully derived"
               (.getMessage thrown)))
        (is (= 0 (:derive_failed_count (ex-data thrown))))
        (is (not (.exists output-root))))
      (finally
        (delete-tree! root)))))

(deftest publication-run-process-preserves-bytes-env-and-nonzero-test
  (let [run-process (ns-resolve 'abc.tools.soranoha-build-publication
                                'run-process!)
        stdin-bytes (.getBytes "\u0000\u00ffA" StandardCharsets/ISO_8859_1)
        result (@run-process
                {:args ["sh" "-c"
                        "cat; printf '%s|%s' \"$ABC_TEST_ENV\" \"$HOME\" >&2; exit 6"]
                 :stdin-bytes stdin-bytes
                 :extra-env {"ABC_TEST_ENV" "kept"}})]
    (is (= #{:exit :out-bytes :err} (set (keys result))))
    (is (= 6 (:exit result)))
    (is (= (seq stdin-bytes) (seq (:out-bytes result))))
    (is (= (str "kept|" (System/getenv "HOME")) (:err result)))))

(deftest build-publication-damaged-zip-does-not-invoke-process-recovery-test
  (let [root (fixture/temp-dir "abc-soranoha-build-damaged-zip")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        work-zip (io/file aozora-root "cards" "000879" "files"
                          "000001_ruby_fixture.zip")
        output-root (io/file root "build-output")
        config-file (io/file root "strict-config.json")
        process-calls (atom 0)
        process-var (ns-resolve 'abc.tools.soranoha-build-publication
                                'run-process!)]
    (try
      (with-open [out (io/output-stream work-zip)]
        (.write out (.getBytes "damaged, not a ZIP" StandardCharsets/UTF_8)))
      (abc-json/write-deterministic-json-file!
       config-file
       (assoc (files/read-json (io/file "config" "publication-basic-ja.json"))
              "continue_on_failure" false))
      (let [thrown (with-redefs-fn
                     {process-var (fn [& _]
                                    (swap! process-calls inc)
                                    (throw (ex-info "process recovery invoked" {})))}
                     #(try
                        (with-release-policy-allowed
                          (fn []
                            (build-publication/build-publication!
                             {:aozora-root (str aozora-root)
                              :config (str config-file)
                              :snapshot-date "2026-07-08"
                              :output-root (str output-root)})))
                        nil
                        (catch Throwable t t)))]
        (is (some? thrown))
        (is (some #(= :unreadable-zip (:reason (ex-data %)))
                  (take-while some?
                              (iterate #(.getCause ^Throwable %) thrown))))
        (is (zero? @process-calls))
        (is (not (.exists output-root))))
      (finally
        (delete-tree! root)))))

(deftest build-publication-command-requires-snapshot-date-for-publication-config-test
  (let [root (fixture/temp-dir "abc-soranoha-build-publication-date")
        aozora-root (official-aozora-fixture! (io/file root "aozorabunko"))
        output-root (io/file root "build-output")
        err (java.io.StringWriter.)]
    (try
      (binding [*err* err]
        (is (= 2 (soranoha/run!
                  ["build-publication"
                   "--aozora-root" (str aozora-root)
                   "--config" "abc/config/publication-basic-ja.json"
                   "--output-root" (str output-root)]))))
      (is (string/includes? (str err) "Required option"))
      (is (string/includes? (str err) "--snapshot-date"))
      (is (not (.exists output-root)))
      (finally
        (delete-tree! root)))))

(deftest build-publication-accepts-parsed-option-map-test
  (let [parse-var (ns-resolve 'abc.tools.soranoha-build-publication 'parse-args)
        resolve-var (ns-resolve 'abc.tools.soranoha-build-publication
                                'resolve-invocation-path)]
    (is (some? parse-var))
    (is (some? resolve-var))
    (is (= {:aozora-root "/invocation/a"
            :config "/invocation/c.json"
            :snapshot-date "2026-07-13"
            :output-root "/invocation/o"
            :replace false
            :concurrency 3}
           (with-redefs-fn {resolve-var #(str "/invocation/" %)}
             #(@parse-var {:aozora-root "a"
                           :config "c.json"
                           :snapshot-date "2026-07-13"
                           :output-root "o"
                           :replace false
                           :concurrency 3}))))))

(deftest cli-status-and-stream-contract-test
  (testing "global help is successful stdout"
    (doseq [args [[] ["help"] ["--help"]]]
      (let [out (java.io.StringWriter.)
            err (java.io.StringWriter.)]
        (binding [*out* out *err* err]
          (is (= 0 (soranoha/run! args))))
        (is (string/includes? (str out) "Usage: soranoha"))
        (is (string/blank? (str err))))))
  (testing "unknown command is status 2 on stderr"
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 2 (soranoha/run! ["nope"]))))
      (is (string/blank? (str out)))
      (is (string/includes? (str err) "Unknown command: nope"))))
  (testing "fixed positional arity is status 2 on stderr"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! ["explain-request-set"]))))
      (is (string/includes? (str err) "Required option")))))

(deftest generated-command-help-test
  (doseq [args [["help" "publication-report"]
                ["publication-report" "--help"]
                ["publication-report" "-h"]]]
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 0 (soranoha/run! args))))
      (is (string/includes? (str out) "Usage: soranoha publication-report"))
      (is (string/includes? (str out) "<snapshot-root>"))
      (is (string/includes? (str out) "<output-path>"))
      (is (string/includes? (str out) "--help"))
      (is (string/blank? (str err))))))

(deftest generated-build-publication-help-test
  (let [out (with-out-str
              (is (= 0 (soranoha/run! ["build-publication" "--help"]))))]
    (doseq [fragment ["--aozora-root" "--config" "--snapshot-date"
                      "--output-root" "--replace" "--concurrency" "--help"]]
      (is (string/includes? out fragment)))))

(deftest generated-global-help-lists-commands-test
  (let [out (with-out-str
              (is (= 0 (soranoha/run! ["--help"]))))]
    (is (string/includes? out "Usage: soranoha"))
    (is (string/includes? out "Commands:"))
    (doseq [command ["explain-request-set" "build-publication"
                     "annotation-join-stats-run"]]
      (is (string/includes? out command)))))

(deftest command-help-does-not-run-command-test
  (let [ran? (atom false)]
    (with-redefs [soranoha/publication-report! (fn [& _] (reset! ran? true))]
      (is (= 0 (soranoha/run! ["publication-report" "--help"]))))
    (is (false? @ran?))))

(deftest malformed-help-requests-are-usage-errors-test
  (doseq [args [["help" "nope"] ["help" "explain-request-set" "extra"]]]
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! args))))
      (is (not (string/blank? (str err)))))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "Unknown command"))))

;; Executable proof that the competing publication composition is gone: the four
;; retired dispatcher commands no longer resolve, and the five retired producer
;; vars are absent from the namespace.
(deftest retired-publication-producer-surface-is-absent-test
  (testing "retired dispatcher commands return the unknown-command exit"
    (doseq [command ["snapshot-index" "reproduce"
                     "publication-rehearsal" "validate-workflow"]]
      (let [err (java.io.StringWriter.)]
        (binding [*err* err]
          (is (= 2 (soranoha/run! [command])) command))
        (is (string/includes? (str err) "Unknown command") command))))
  (testing "retired producer vars no longer resolve"
    (doseq [var-name ['build-snapshot-index 'snapshot-index!
                      'materialize-snapshot-root! 'reproduce!
                      'publication-rehearsal!]]
      (is (nil? (ns-resolve 'abc.tools.soranoha var-name))
          (str var-name)))))
