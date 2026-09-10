(ns soranoha.ori.metadata-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [charred.api :as json]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.accountability :as accountability]
            [soranoha.core.json :as record-json]
            [soranoha.main :as main]
            [soranoha.za.corpus :as corpus]))

(deftest selected-ragged-row-is-refused-before-the-metadata-build
  (let [work {:work-id "000100" :person-id "000001" :card "000001"
              :title "試験" :text "本文"}
        root (corpus/init-corpus! [work])
        store (fs/create-temp-dir {:prefix "metadata-rejection"})]
    (try
      (corpus/write-zip! (fs/path root "index_pages" "list_person_all_extended_utf8.zip")
                         [["catalog.csv"
                           ;; ragged in 姓 only: the work still has to be
                           ;; selected for the metadata build to reach it, and
                           ;; selection refuses a row that states no copyright
                           ;; flag before the ragged check ever runs
                           (str "作品ID,人物ID,役割フラグ,作品名,テキストファイルURL,作品著作権フラグ,姓\n"
                                "000100,000001,著者,試験,https://example.org/cards/000001/files/"
                                (corpus/work-basename work) ".zip,なし\n")]])
      (corpus/commit-corpus! root)
      (is (= :ragged-metadata-row
             (try (main/build! {:root (str store) :aozora-root root
                                :assets-root "unused" :clj-toolchain-id "test" :concurrency 1})
                  :accepted
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (finally (fs/delete-tree root) (fs/delete-tree store)))))

(defn- catalog-row [id title]
  {"作品ID" id "作品名" title "人物ID" "000001" "姓" "著者"
   "役割フラグ" "著者" "文字遣い種別" "新字新仮名"
   "作品著作権フラグ" "なし" "人物著作権フラグ" "なし"
   "公開日" "2026-01-01" "最終更新日" "2026-01-01"
   "図書カードURL" (str "https://www.aozora.gr.jp/cards/000001/card" id ".html")})

(deftest metadata-traces-use-captured-schema-documents
  (let [dir (fs/create-temp-dir {:prefix "metadata-inputs"})
        assets (fs/path dir "assets")
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})]
    (try
      (fs/create-dirs (fs/path assets "schemas"))
      (doseq [filename ["metadata-record.schema.json" "person-record.schema.json"]]
        (spit (str (fs/path assets "schemas" filename))
              (slurp (str (fs/path "schemas" filename)))))
      (let [stage (stages/metadata-stage "test" assets)
            run (fn [stage rows]
                  (engine/run-stage! store stage
                                     {"catalog-rows" (cas/put-bytes!
                                                      (:cas-dir store)
                                                      (canonical/rfc8785-safe-integer-json-bytes-v1 rows))
                                      "work_id" (get (first rows) "作品ID")}))
            read-output (fn [result kind]
                          (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir store)
                                                                         (get-in result [:outputs kind])) "UTF-8")))
            first-row (catalog-row "000100" "最初")
            second-row (catalog-row "000101" "次")
            second-run (run stage [second-row])
            old-metadata (read-output second-run "metadata-record")
            old-person (get (read-output second-run "persons") "000001")
            mutate-schema! (fn [filename]
                             (let [path (str (fs/path assets "schemas" filename))
                                   doc (assoc (record-json/read-json-file path) "description" "changed schema document")]
                               (spit path (record-json/write-deterministic-json-str doc))
                               (str "sha256:" (hash/sha256-canonical-json doc))))]
        (testing "same-process metadata schema edits bind both stage identity and embedded schema hash"
          (let [schema-hash (mutate-schema! "metadata-record.schema.json")
                current (run (stages/metadata-stage "test" assets) [second-row])]
            (is (not (:cached? current)))
            (is (= schema-hash (get (read-output current "metadata-record") "metadata_record_schema_hash")))
            (is (not= (get old-metadata "metadata_record_schema_hash") schema-hash))))
        (testing "person schema edits also change the referenced person identity"
          (let [schema-hash (mutate-schema! "person-record.schema.json")
                current (run (stages/metadata-stage "test" assets) [second-row])
                person (get (read-output current "persons") "000001")]
            (is (not (:cached? current)))
            (is (= schema-hash (get person "person_record_schema_hash")))
            (is (not= (get old-metadata "contributors")
                      (get (read-output current "metadata-record") "contributors")))))
        (testing "an already-constructed stage keeps the schema documents its identity describes"
          (let [fresh (run stage [(catalog-row "000102" "新規")])]
            (is (= (get old-metadata "metadata_record_schema_hash")
                   (get (read-output fresh "metadata-record") "metadata_record_schema_hash")))
            (is (= (get old-person "person_record_schema_hash")
                   (get-in (read-output fresh "persons") ["000001" "person_record_schema_hash"])))))
        (testing "explicit schema injection retains impossible-calendar rejection"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a valid calendar date"
                                (run stage [(assoc first-row "没年月日" "2020-02-31")])))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))

(deftest catalog-changes-invalidate-only-the-works-that-consume-the-changed-rows
  (let [first-work {:work-id "000100" :person-id "000001" :card "000001"
                    :book "100" :n "1" :title "最初" :text "本文"}
        second-work (assoc first-work :work-id "000101" :book "101" :n "2" :title "次")
        corpus-root (corpus/init-corpus! [first-work second-work])
        dir (fs/create-temp-dir {:prefix "metadata-catalog"})
        row (fn [work]
              (assoc (catalog-row (:work-id work) (:title work))
                     "テキストファイルURL"
                     (str "https://www.aozora.gr.jp/cards/000001/files/" (corpus/work-basename work) ".zip")))
        first-row (row first-work)
        second-row (row second-work)
        write-catalog! (fn [rows reverse-header?]
                         (let [header (cond->> (sort (keys first-row)) reverse-header? reverse)
                               writer (java.io.StringWriter.)]
                           (json/write-csv writer (cons header (map #(mapv % header) rows)) :close-writer? true)
                           (corpus/write-zip! (fs/path corpus-root "index_pages" "list_person_all_extended_utf8.zip")
                                              [["catalog.csv" (str writer)]])
                           (corpus/commit-corpus! corpus-root)))
        opts {:root (str dir) :aozora-root corpus-root :assets-root "."
              :clj-toolchain-id "test" :concurrency 1}
        build #(binding [*out* (java.io.StringWriter.)] (main/build! opts))]
    (try
      (with-redefs [stages/resolve-adapter (constantly {})
                    accountability/source-stage (constantly (:accountability corpus/stage-set))
                    accountability/coverage-stage (constantly (:coverage corpus/stage-set))
                    stages/parse-stage (constantly (:parse corpus/stage-set))
                    stages/convert-stage (constantly (:convert corpus/stage-set))
                    stages/render-stage (constantly (:render corpus/stage-set))
                    stages/validate-tei-stage (constantly (:validate corpus/stage-set))]
        (write-catalog! [first-row second-row] false)
        (let [original (build)
              changed-row (assoc first-row "作品名" "訂正" "姓" "訂正著者")
              unrelated (assoc first-row "作品ID" "000999" "作品名" "未選択"
                               "テキストファイルURL" "https://www.aozora.gr.jp/cards/000001/files/999_ruby_999.zip")]
          (write-catalog! [changed-row second-row unrelated] false)
          (let [changed (build)
                a (corpus/work-slug first-work)
                b (corpus/work-slug second-work)]
            (is (= #{a b} (set (keys (get changed "works")))))
            (is (false? (get-in changed ["works" a "cached" "metadata"])))
            (is (true? (get-in changed ["works" b "cached" "metadata"])))
            (is (= (get-in original ["works" b "trace_keys" "metadata"])
                   (get-in changed ["works" b "trace_keys" "metadata"])))
            (is (true? (get-in changed ["works" a "cached" "parse"])))
            (write-catalog! [second-row changed-row unrelated] true)
            (let [reordered (build)]
              (is (every? #(get-in reordered ["works" % "cached" "metadata"]) [a b]))))))
      (finally (fs/delete-tree corpus-root) (fs/delete-tree dir)))))
