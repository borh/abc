(ns abc.tools.soranoha-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-evidence :as parser-evidence]
            [abc.tools.publication-policy :as publication-policy]
            [abc.tools.request-set-resolver :as resolver]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha :as soranoha]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.source-bundle :as source-bundle]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.io FileNotFoundException IOException InterruptedIOException]
           [java.nio.charset StandardCharsets]
           [java.nio.file AccessDeniedException Files NoSuchFileException]
           [java.util.zip ZipEntry ZipOutputStream]))

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
        (is (= "ok" (slurp (fs/file target "artifact"))))))
    (let [publication (fs/file root "publication")]
      (fs/create-dirs publication)
      (spit (fs/file publication "tei.manifest.json") "{}")
      (spit (fs/file publication "source_work_content_hash.txt") " hash\n")
      (is (#'build-publication/publication-up-to-date? publication "hash"))
      (fs/delete (fs/file publication "tei.manifest.json"))
      (is (not (#'build-publication/publication-up-to-date? publication "hash"))))
    (let [from (fs/file root "from")
          to (fs/file root "to")]
      (fs/create-dirs (fs/file from "nested"))
      (spit (fs/file from "b") "b")
      (spit (fs/file from "a") "a")
      (spit (fs/file from "nested" "ignored") "ignored")
      (is (= (fs/file to) (#'build-publication/copy-dir-files! from to)))
      (is (= ["a" "b"] (->> (fs/list-dir to)
                            (map (comp str fs/file-name))
                            sort vec))))))

(defn- temp-json-path [prefix]
  (let [file (java.io.File/createTempFile prefix ".json")]
    (.delete file)
    (str file)))

(deftest archive-summary-follows-directory-symlinks-in-stable-path-order-test
  (let [base (fixture/temp-dir "abc-soranoha-symlinked-archives")
        root (io/file base "root")
        external (io/file base "external")]
    (.mkdirs root)
    (.mkdirs external)
    (spit (io/file root "z.tar.zst") "z")
    (spit (io/file external "b.tar.zst") "b")
    (spit (io/file external "a.tar.zst") "a")
    (Files/createSymbolicLink (.toPath (io/file root "linked"))
                              (.toPath external)
                              (make-array java.nio.file.attribute.FileAttribute 0))
    (is (= ["linked/a.tar.zst" "linked/b.tar.zst" "z.tar.zst"]
           (get (#'soranoha/archive-summary root) "paths")))))

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

(defn- write-generated-source-request-set!
  [root label]
  (let [input-root (io/file root "materialized")
        source-snapshot-root (io/file root "source-snapshot")
        request-set-file (io/file root (str label ".json"))]
    (fixture/materialized-work! input-root
                                {:slug "alpha"
                                 :title "一"
                                 :work-id "000001"
                                 :person-id "000879"
                                 :work-hash (fixture/example-hash "a1")})
    (with-out-str
      (is (zero? (soranoha/run!
                  ["source-snapshot"
                   (str input-root)
                   (str source-snapshot-root)
                   "unit-test-source-snapshot"
                   "2026-07-07"]))))
    (with-out-str
      (is (zero? (soranoha/run!
                  ["resolve-request-set"
                   label
                   (str request-set-file)
                   (str (io/file source-snapshot-root
                                 "source-snapshot.json"))]))))
    request-set-file))

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

(deftest snapshot-index-command-generates-index-from-plan-test
  (let [output-path (temp-json-path "abc-soranoha-snapshot-index")
        request-set (resolver/resolve-request-set "smoke-basic-ja")
        out (with-out-str
              (is (zero? (soranoha/run! ["snapshot-index"
                                         "smoke-basic-ja"
                                         output-path]))))
        snapshot (files/read-json output-path)]
    (try
      (is (string/includes? out output-path))
      (is (= (get request-set "request_set_id")
             (get-in snapshot ["snapshot_index_identity_object"
                               "request_set_id"])))
      (is (= (manifest/schema-hash "schemas/snapshot-index.schema.json")
             (get snapshot "schema_hash")))
      (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
      (finally
        (.delete (io/file output-path))))))

(deftest snapshot-index-command-accepts-resolved-request-set-file-test
  (let [root (fixture/temp-dir "abc-soranoha-request-set-file-index")
        request-set-file (io/file root "request-set.json")
        output-path (io/file root "snapshot-index.json")
        request-set (resolver/resolve-request-set "smoke-basic-ja")]
    (try
      (manifest/write-json-file! request-set-file request-set)
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["snapshot-index"
                                             (str request-set-file)
                                             (str output-path)]))))
            snapshot (files/read-json output-path)]
        (is (string/includes? out (str output-path)))
        (is (= (get request-set "request_set_id")
               (get-in snapshot ["snapshot_index_identity_object"
                                 "request_set_id"])))
        (is (= (get request-set "label")
               (get snapshot "request_set_label")))
        (is (true? (snapshot-index/validate-snapshot-index! snapshot))))
      (finally
        (delete-tree! root)))))

(deftest explain-snapshot-command-validates-and-prints-identity-test
  (let [output-path (temp-json-path "abc-soranoha-explain-snapshot")]
    (try
      (with-out-str
        (is (zero? (soranoha/run! ["snapshot-index" "smoke-basic-ja" output-path]))))
      (let [snapshot (files/read-json output-path)
            out (with-out-str
                  (is (zero? (soranoha/run! ["explain-snapshot"
                                             output-path]))))]
        (is (string/includes? out (get snapshot "snapshot_label")))
        (is (string/includes? out (get snapshot "snapshot_identity_hash")))
        (is (string/includes? out "failure_rate:")))
      (finally
        (.delete (io/file output-path))))))

(deftest validate-command-accepts-snapshot-root-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["validate" (str root)]))))]
        (is (string/includes? out "snapshot_valid: true")))
      (finally
        (delete-tree! root)))))

(deftest validate-command-rejects-missing-referenced-manifest-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")
        missing-manifest (io/file root
                                  "artifacts/analysis/analysis.manifest.json")]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))
      (is (.delete missing-manifest))
      (let [err (java.io.StringWriter.)]
        (binding [*err* err]
          (is (= 1 (soranoha/run! ["validate" (str root)]))))
        (is (string/includes? (str err)
                              "Referenced snapshot manifest does not exist")))
      (finally
        (delete-tree! root)))))

(deftest reproduce-command-writes-default-smoke-snapshot-index-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")
        output-path (io/file root "snapshot-index.json")
        expected-files ["artifacts/parser-ir/parser-ir.json"
                        "artifacts/parser-ir/parser-ir.manifest.json"
                        "artifacts/plaintext/plain.txt"
                        "artifacts/plaintext/plaintext.manifest.json"
                        "artifacts/tei/tei.xml"
                        "artifacts/tei/tei.manifest.json"
                        "artifacts/analysis/analysis-result.json"
                        "artifacts/analysis/analysis.manifest.json"]]
    (try
      (delete-tree! root)
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))]
        (is (.exists output-path))
        (doseq [path expected-files]
          (is (.exists (io/file root path))
              (str path " should be materialized into the smoke snapshot root")))
        (is (string/includes? out (str output-path)))
        (let [snapshot (files/read-json output-path)
              references (get snapshot "artifact_references")
              reference-kinds (set (map #(get % "artifact_kind") references))
              locators (set (map #(get-in % ["locator" "path"]) references))]
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (is (contains? reference-kinds "parser-ir"))
          (is (contains? reference-kinds "plaintext"))
          (is (contains? reference-kinds "tei"))
          (is (contains? reference-kinds "analysis"))
          (is (contains? locators "artifacts/analysis/analysis.manifest.json"))))
      (finally
        (delete-tree! root)))))

(deftest reproduce-command-accepts-resolved-request-set-file-test
  (let [request-set-root (fixture/temp-dir "abc-soranoha-request-set-file-reproduce")
        request-set-file (io/file request-set-root "request-set.json")
        snapshot-root (io/file "target/soranoha/smoke-basic-ja")]
    (try
      (delete-tree! snapshot-root)
      (manifest/write-json-file! request-set-file
                                 (resolver/resolve-request-set "smoke-basic-ja"))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["reproduce"
                                             (str request-set-file)]))))
            output-path (io/file snapshot-root "snapshot-index.json")]
        (is (.exists output-path))
        (is (string/includes? out (str output-path)))
        (is (= "smoke-basic-ja"
               (get (files/read-json output-path) "request_set_label")))
        (with-out-str
          (is (zero? (soranoha/run! ["validate" (str snapshot-root)])))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! request-set-root)))))

(deftest reproduce-command-materializes-demo-request-set-test
  (let [root (io/file "target/soranoha/demo-basic-ja")
        output-path (io/file root "snapshot-index.json")
        expected-work-hashes #{"sha256:2323232323232323232323232323232323232323232323232323232323232323"
                               "sha256:2424242424242424242424242424242424242424242424242424242424242424"}
        expected-files ["artifacts/works/demo-fixture-a/parser-ir/parser-ir.manifest.json"
                        "artifacts/works/demo-fixture-a/plaintext/plaintext.manifest.json"
                        "artifacts/works/demo-fixture-a/tei/tei.manifest.json"
                        "artifacts/works/demo-fixture-a/analysis/analysis.manifest.json"
                        "artifacts/works/demo-fixture-b/parser-ir/parser-ir.manifest.json"
                        "artifacts/works/demo-fixture-b/plaintext/plaintext.manifest.json"
                        "artifacts/works/demo-fixture-b/tei/tei.manifest.json"
                        "artifacts/works/demo-fixture-b/analysis/analysis.manifest.json"]]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "demo-basic-ja"]))))
      (is (.exists output-path))
      (doseq [path expected-files]
        (is (.exists (io/file root path))
            (str path " should be materialized into the demo snapshot root")))
      (when (.exists output-path)
        (let [snapshot (files/read-json output-path)
              references (get snapshot "artifact_references")
              reference-kinds (frequencies (map #(get % "artifact_kind")
                                                references))
              manifest-work-hashes (set
                                    (for [path expected-files]
                                      (get-in (files/read-json
                                               (io/file root path))
                                              ["manifest_identity_object"
                                               "work_content_hash"])))]
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (is (= {"parser-ir" 2
                  "plaintext" 2
                  "tei" 2
                  "analysis" 2}
                 reference-kinds))
          (is (= expected-work-hashes manifest-work-hashes))
          (with-out-str
            (is (zero? (soranoha/run! ["validate" (str root)]))))))
      (finally
        (delete-tree! root)))))

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

(deftest reproduce-command-uses-generated-full-corpus-request-set-test
  (let [root (fixture/temp-dir "abc-soranoha-reproduce-source-snapshot")
        snapshot-root (io/file "target/soranoha/full-corpus-basic-ja")]
    (try
      (delete-tree! snapshot-root)
      (let [request-set-file (write-generated-source-request-set!
                              root
                              "full-corpus-basic-ja")
            out (with-out-str
                  (is (zero? (soranoha/run!
                              ["reproduce" (str request-set-file)]))))
            snapshot-file (io/file snapshot-root "snapshot-index.json")
            run-summary-file (io/file snapshot-root "run-summary.json")
            parser-manifest (io/file snapshot-root
                                     "artifacts/works/alpha/parser-ir/parser-ir.manifest.json")]
        (is (string/includes? out (str snapshot-file)))
        (is (.exists parser-manifest))
        (is (.exists snapshot-file))
        (is (.exists run-summary-file))
        (let [snapshot (files/read-json snapshot-file)]
          (is (= "full-corpus-basic-ja" (get snapshot "request_set_label")))
          (is (= 4 (get-in snapshot ["summary" "total_artifacts"])))
          (is (= (parser-evidence/citable-hashes)
                 (get-in snapshot ["snapshot_index_identity_object"
                                   "parser_evidence_hashes"])))
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (let [run-summary (files/read-json run-summary-file)]
            (is (= (get snapshot "snapshot_identity_hash")
                   (get run-summary "snapshot_identity_hash")))
            (is (= (get snapshot "summary")
                   (get run-summary "snapshot_summary")))
            (is (= (get-in snapshot ["snapshot_index_identity_object"
                                     "parser_evidence_hashes"])
                   (get run-summary "parser_evidence_hashes"))))
          (with-out-str
            (is (zero? (soranoha/run! ["validate" (str snapshot-root)]))))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! root)))))

(deftest publication-report-command-writes-citable-reproduction-evidence-test
  (let [root (fixture/temp-dir "abc-soranoha-publication-report")
        snapshot-root (io/file "target/soranoha/full-corpus-basic-ja")
        report-file (io/file root "publication-report.json")]
    (try
      (delete-tree! snapshot-root)
      (let [request-set-file (write-generated-source-request-set!
                              root
                              "full-corpus-basic-ja")]
        (with-out-str
          (is (zero? (soranoha/run!
                      ["reproduce" (str request-set-file)]))))
        (let [out (with-out-str
                    (is (zero? (soranoha/run!
                                ["publication-report"
                                 (str snapshot-root)
                                 (str report-file)]))))
              snapshot (files/read-json (io/file snapshot-root
                                                 "snapshot-index.json"))
              run-summary (files/read-json (io/file snapshot-root
                                                    "run-summary.json"))
              report (files/read-json report-file)]
          (is (string/includes? out (str report-file)))
          (is (= "https://w3id.org/abc/soranoha-publication-report-v0.json"
                 (get report "schema_id")))
          (is (= "0.1.0" (get report "report_version")))
          (is (= (get snapshot "snapshot_label")
                 (get report "snapshot_label")))
          (is (= (get snapshot "snapshot_identity_hash")
                 (get report "snapshot_identity_hash")))
          (is (= (get snapshot "request_set_label")
                 (get report "request_set_label")))
          (is (= (get-in snapshot ["snapshot_index_identity_object"
                                   "request_set_id"])
                 (get report "request_set_id")))
          (is (= (get-in snapshot ["snapshot_index_identity_object"
                                   "source_snapshot_hash"])
                 (get report "source_snapshot_hash")))
          (is (= (get snapshot "summary")
                 (get report "snapshot_summary")))
          (is (= {"analysis" 1
                  "parser-ir" 1
                  "plaintext" 1
                  "tei" 1}
                 (get report "artifact_kind_counts")))
          (is (= 4 (get report "manifest_reference_count")))
          (is (= (get run-summary "artifact_manifest_count")
                 (get report "artifact_manifest_count")))
          (is (= (get run-summary "materialization_count")
                 (get report "materialization_count")))
          (is (= (parser-evidence/citable-hashes)
                 (get report "parser_evidence_hashes")))
          (is (= (get run-summary "runtime_environment")
                 (get report "runtime_environment")))
          (is (= {"snapshot_root_valid" true
                  "checked_manifest_references" 4
                  "run_summary_valid" true}
                 (get report "validation")))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! root)))))

(deftest layout-report-command-compares-static-layout-strategies-test
  (let [root (fixture/temp-dir "abc-soranoha-layout-report")
        snapshot-root (io/file "target/soranoha/full-corpus-basic-ja")
        report-file (io/file root "layout-report.json")]
    (try
      (delete-tree! snapshot-root)
      (let [request-set-file (write-generated-source-request-set!
                              root
                              "full-corpus-basic-ja")]
        (with-out-str
          (is (zero? (soranoha/run!
                      ["reproduce" (str request-set-file)]))))
        (let [out (with-out-str
                    (is (zero? (soranoha/run!
                                ["layout-report"
                                 (str snapshot-root)
                                 (str report-file)]))))
              snapshot (files/read-json (io/file snapshot-root
                                                 "snapshot-index.json"))
              report (files/read-json report-file)
              strategies (into {}
                               (map (juxt #(get % "strategy_id") identity))
                               (get report "strategy_estimates"))]
          (is (string/includes? out (str report-file)))
          (is (= "https://w3id.org/abc/soranoha-layout-report-v0.json"
                 (get report "schema_id")))
          (is (= "0.1.0" (get report "report_version")))
          (is (= (get snapshot "snapshot_identity_hash")
                 (get report "snapshot_identity_hash")))
          (is (= (get snapshot "summary")
                 (get report "snapshot_summary")))
          (is (= {"analysis" 1
                  "parser-ir" 1
                  "plaintext" 1
                  "tei" 1}
                 (get report "artifact_kind_counts")))
          (is (= {"snapshot_root_valid" true
                  "checked_manifest_references" 4}
                 (get report "validation")))
          (is (= 4 (get-in report ["actual_root" "referenced_manifest_count"])))
          (is (pos? (get-in report ["actual_root" "file_count"])))
          (is (pos? (get-in report ["actual_root" "byte_count"])))
          (is (= {"loose_artifact_kinds" ["annotation" "plaintext" "tei"]
                  "batched_artifact_kinds" ["analysis" "tokenized"]}
                 (select-keys (get strategies "mixed-default-v1")
                              ["loose_artifact_kinds"
                               "batched_artifact_kinds"])))
          (is (= 2 (get-in strategies ["mixed-default-v1"
                                       "loose_content_file_count"])))
          (is (= 1 (get-in strategies ["mixed-default-v1"
                                       "archive_count"])))
          (is (= 4 (get-in strategies ["all-loose-v1"
                                       "loose_content_file_count"])))
          (is (= 0 (get-in strategies ["all-loose-v1"
                                       "archive_count"])))
          (is (= 0 (get-in strategies ["all-batched-v1"
                                       "loose_content_file_count"])))
          (is (= 4 (get-in strategies ["all-batched-v1"
                                       "archive_count"])))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! root)))))

(deftest stage-publication-command-writes-mixed-static-layout-test
  (let [root (fixture/temp-dir "abc-soranoha-stage-publication")
        snapshot-root (io/file "target/soranoha/full-corpus-basic-ja")
        staged-root (io/file root "published")]
    (try
      (delete-tree! snapshot-root)
      (let [request-set-file (write-generated-source-request-set!
                              root
                              "full-corpus-basic-ja")]
        (with-out-str
          (is (zero? (soranoha/run!
                      ["reproduce" (str request-set-file)]))))
        (let [staged-index-file (io/file staged-root "index.json")
              analysis-archive-file (io/file staged-root
                                             "artifacts/analysis/batches/analysis-batch-0001.tar.zst")
              out (with-redefs [publication-policy/assert-release-allowed!
                                (constantly :ok)]
                    (with-out-str
                      (is (zero? (soranoha/run!
                                  ["stage-publication"
                                   (str snapshot-root)
                                   (str staged-root)])))))]
          (is (string/includes? out (str staged-index-file)))
          (is (.exists staged-index-file))
          (when (.exists staged-index-file)
            (let [staged-index (files/read-json staged-index-file)
                  references (get staged-index "artifact_references")
                  reference-by-kind (into {}
                                          (map (juxt #(get % "artifact_kind")
                                                     identity))
                                          references)
                  plaintext-ref (get reference-by-kind "plaintext")
                  tei-ref (get reference-by-kind "tei")
                  analysis-ref (get reference-by-kind "analysis")]
              (is (.exists (io/file staged-root "run-summary.json")))
              (is (= (get (files/read-json (io/file snapshot-root
                                                    "snapshot-index.json"))
                          "snapshot_identity_hash")
                     (get staged-index "snapshot_identity_hash")))
              (is (= "loose" (get-in plaintext-ref ["locator" "kind"])))
              (is (= "manifests/by-work/alpha/plaintext.manifest.json"
                     (get-in plaintext-ref ["locator" "path"])))
              (is (.exists (io/file staged-root
                                    "artifacts/plaintext/by-work/alpha/plain.txt")))
              (is (= "loose" (get-in tei-ref ["locator" "kind"])))
              (is (= "manifests/by-work/alpha/tei.manifest.json"
                     (get-in tei-ref ["locator" "path"])))
              (is (.exists (io/file staged-root
                                    "artifacts/tei/by-work/alpha/tei.xml")))
              (is (= "archive-member" (get-in analysis-ref ["locator"
                                                            "kind"])))
              (is (= "tar.zst" (get-in staged-index ["layout_policy"
                                                     "archive_format"])))
              (is (= "artifacts/analysis/batches/analysis-batch-0001.tar.zst"
                     (get-in analysis-ref ["locator" "archive_path"])))
              (is (= "alpha/analysis.manifest.json"
                     (get-in analysis-ref ["locator" "member_path"])))
              (is (pos? (.length analysis-archive-file)))
              (is (true? (snapshot-index/validate-snapshot-index! staged-index)))
              (with-out-str
                (is (zero? (soranoha/run! ["validate" (str staged-root)]))))
              (spit analysis-archive-file "")
              (let [err (java.io.StringWriter.)]
                (binding [*err* err]
                  (is (= 1 (soranoha/run! ["validate" (str staged-root)]))))
                (is (string/includes? (str err)
                                      "Referenced snapshot archive does not contain member")))))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! root)))))

(deftest publication-rehearsal-command-runs-full-chain-test
  (let [root (fixture/temp-dir "abc-soranoha-publication-rehearsal")
        input-root (io/file root "materialized")
        output-root (io/file root "rehearsal")]
    (try
      (fixture/materialized-work! input-root
                                  {:slug "alpha"
                                   :title "一"
                                   :work-id "000001"
                                   :person-id "000879"
                                   :work-hash (fixture/example-hash "a1")})
      (let [out (with-out-str
                  (is (zero? (soranoha/run!
                              ["publication-rehearsal"
                               (str input-root)
                               (str output-root)
                               "full-corpus-basic-ja"
                               "unit-test-source-snapshot"
                               "2026-07-07"]))))
            report-file (io/file output-root "rehearsal-report.json")
            request-set-file (io/file output-root
                                      "request-sets"
                                      "full-corpus-basic-ja.json")
            snapshot-root (io/file output-root "snapshot-root")
            staged-root (io/file output-root "publication")
            publication-report-file (io/file output-root
                                             "reports"
                                             "publication-report.json")
            layout-report-file (io/file output-root
                                        "reports"
                                        "layout-report.json")
            workflow-plan-file (io/file output-root "workflow-plan.json")
            workflow-run-file (io/file output-root "workflow-run.json")]
        (is (string/includes? out (str report-file)))
        (is (.exists request-set-file))
        (is (.exists (io/file snapshot-root "snapshot-index.json")))
        (is (.exists (io/file staged-root "index.json")))
        (is (.exists publication-report-file))
        (is (.exists layout-report-file))
        (is (.exists workflow-plan-file))
        (is (.exists workflow-run-file))
        (with-out-str
          (is (zero? (soranoha/run! ["validate" (str staged-root)]))))
        (let [workflow-run (files/read-json workflow-run-file)
              report (files/read-json report-file)
              staged-index (files/read-json (io/file staged-root "index.json"))
              request-set (files/read-json request-set-file)]
          (is (= "soranoha.publication-rehearsal.v1"
                 (get workflow-run "workflow_id")))
          (is (= "passed" (get workflow-run "status")))
          (is (= 8 (get workflow-run "step_count")))
          (is (= ["source-snapshot"
                  "resolve-request-set"
                  "materialize-snapshot-root"
                  "validate-snapshot-root"
                  "publication-report"
                  "layout-report"
                  "stage-publication"
                  "validate-staged-publication"]
                 (mapv #(get % "id") (get workflow-run "steps"))))
          (is (= "https://w3id.org/abc/soranoha-publication-rehearsal-report-v0.json"
                 (get report "schema_id")))
          (is (= "0.1.0" (get report "report_version")))
          (is (= (get request-set "request_set_id")
                 (get report "request_set_id")))
          (is (= (get staged-index "snapshot_identity_hash")
                 (get report "snapshot_identity_hash")))
          (is (= 1 (get report "work_count")))
          (is (= 4 (get report "manifest_reference_count")))
          (is (= "tar.zst" (get report "archive_format")))
          (is (= "soranoha publication-rehearsal <materialized-root> <output-root> <request-set-label> <snapshot-scope> <snapshot-date>"
                 (get report "rehearsal_command")))
          (is (= ["source-snapshot"
                  "resolve-request-set"
                  "materialize-snapshot-root"
                  "publication-report"
                  "layout-report"
                  "stage-publication"
                  "validate-staged"]
                 (mapv #(get % "step") (get report "internal_steps"))))
          (is (nil? (get report "commands")))
          (is (= true (get-in report ["validation"
                                      "snapshot_root_valid"])))
          (is (= true (get-in report ["validation"
                                      "staged_root_valid"])))))
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
                      (is (zero? (soranoha/run!
                                  ["build-publication"
                                   "--aozora-root" (str aozora-root)
                                   "--config" "abc/config/publication-basic-ja.json"
                                   "--snapshot-date" "2026-07-08"
                                   "--output-root" (str output-root)]))))))
            slug "000001_000879_000001_ruby_fixture"
            work-dir (io/file output-root "materialized-root" "works" slug)
            official-source-file (io/file work-dir "official-source.json")
            source-bundle-file (io/file work-dir "source-bundle.json")
            parser-ir-file (io/file work-dir "parser-ir.json")
            source-manifest-file (io/file work-dir "source.manifest.json")
            source-selection-report-file (io/file output-root
                                                  "source-selection-report.json")
            build-workflow-run-file (io/file output-root "workflow-run.json")
            pub-dir (io/file output-root "publications" slug)
            tei-file (io/file pub-dir "tei.xml")
            tei-validation-file (io/file pub-dir "tei-validation-result.json")
            publications-report-file (io/file output-root "publications"
                                              "publications-report.json")]
        (is (string/includes? out "build_publication_root:"))
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
          (is (true? (get report "release_admissible")))
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
          (is (= 0 (get publications-report "failed"))))
        (let [workflow-run (files/read-json build-workflow-run-file)]
          (is (= "soranoha.build-publication.v1"
                 (get workflow-run "workflow_id")))
          (is (= "passed" (get workflow-run "status")))
          (is (= ["materialize-source-selection"
                  "write-build-records"
                  "materialize-publications"]
                 (mapv #(get % "id") (get workflow-run "steps"))))
          (with-out-str
            (is (zero? (soranoha/run!
                        ["validate-workflow"
                         (str build-workflow-run-file)]))))))
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
                     (is (zero? (soranoha/run!
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
                      (is (zero? (soranoha/run!
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

(deftest reproduce-command-skips-analysis-for-generated-publication-request-set-test
  (let [root (fixture/temp-dir "abc-soranoha-reproduce-publication-source")
        snapshot-root (io/file "target/soranoha/full-corpus-publication-basic-ja")]
    (try
      (delete-tree! snapshot-root)
      (let [request-set-file (write-generated-source-request-set!
                              root
                              "full-corpus-publication-basic-ja")
            out (with-out-str
                  (is (zero? (soranoha/run!
                              ["reproduce" (str request-set-file)]))))
            snapshot-file (io/file snapshot-root "snapshot-index.json")
            analysis-manifest (io/file snapshot-root
                                       "artifacts/works/alpha/analysis/analysis.manifest.json")]
        (is (string/includes? out (str snapshot-file)))
        (is (not (.exists analysis-manifest)))
        (let [snapshot (files/read-json snapshot-file)]
          (is (= "full-corpus-publication-basic-ja"
                 (get snapshot "request_set_label")))
          (is (= 3 (get-in snapshot ["summary" "total_artifacts"])))
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (with-out-str
            (is (zero? (soranoha/run! ["validate" (str snapshot-root)]))))))
      (finally
        (delete-tree! snapshot-root)
        (delete-tree! root)))))

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
        (is (= 2 (soranoha/run! ["snapshot-index"]))))
      (is (string/includes? (str err) "Required option")))))

(deftest generated-command-help-test
  (doseq [args [["help" "snapshot-index"]
                ["snapshot-index" "--help"]
                ["snapshot-index" "-h"]]]
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 0 (soranoha/run! args))))
      (is (string/includes? (str out) "Usage: soranoha snapshot-index"))
      (is (string/includes? (str out) "<label-or-request-set-json>"))
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
    (doseq [command ["snapshot-index" "build-publication"
                     "annotation-join-stats-run"]]
      (is (string/includes? out command)))))

(deftest command-help-does-not-run-command-test
  (let [ran? (atom false)]
    (with-redefs [soranoha/snapshot-index! (fn [& _] (reset! ran? true))]
      (is (= 0 (soranoha/run! ["snapshot-index" "--help"]))))
    (is (false? @ran?))))

(deftest malformed-help-requests-are-usage-errors-test
  (doseq [args [["help" "nope"] ["help" "snapshot-index" "extra"]]]
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! args))))
      (is (not (string/blank? (str err)))))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "Unknown command"))))
