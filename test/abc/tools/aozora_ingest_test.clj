(ns abc.tools.aozora-ingest-test
  (:require [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.person-record :as pr]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

;; A minimal synthetic CSV row covering the fields the parser reads.
;; Unknown columns are empty strings; the parser tolerates that.
(defn- row [overrides]
  (merge {"作品ID" "000127"
          "人物ID" "000879"
          "役割フラグ" "著者"
          "作品名" "羅生門"
          "作品名読み" "らしょうもん"
          "ソート用読み" "らしようもん"
          "副題" "" "副題読み" "" "原題" "" "初出" ""
          "分類番号" "NDC 913" "文字遣い種別" "新字新仮名"
          "作品著作権フラグ" "なし" "公開日" "1997-10-29"
          "最終更新日" "2022-07-16"
          "図書カードURL" "https://www.aozora.gr.jp/cards/000879/card127.html"
          "底本名1" "" "底本出版社名1" ""
          "底本名2" "" "底本出版社名2" ""
          "底本初版発行年1" "" "底本初版発行年2" ""
          "入力に使用した版1" "" "入力に使用した版2" ""
          "校正に使用した版1" "" "校正に使用した版2" ""
          "底本の親本名1" "" "底本の親本名2" ""
          "底本の親本出版社名1" "" "底本の親本出版社名2" ""
          "底本の親本初版発行年1" "" "底本の親本初版発行年2" ""
          "姓" "芥川" "名" "竜之介"
          "姓読み" "あくたがわ" "名読み" "りゅうのすけ"
          "姓読みソート用" "あくたかわ" "名読みソート用" "りゆうのすけ"
          "姓ローマ字" "Akutagawa" "名ローマ字" "Ryunosuke"
          "生年月日" "1892-03-01" "没年月日" "1927-07-24"
          "人物著作権フラグ" "なし"}
         overrides))

;; The work fixture in the synthetic CSV needs at least one source-edition
;; row to satisfy the metadata-record schema's minItems: 1. Add one.
(def synthetic-rows-with-edition
  [(row {"底本名1" "羅生門" "底本出版社名1" "テスト出版社"})])

(deftest ingest-emits-n-plus-1-files-test
  (testing "run-from-rows! writes the work record + each person file"
    (let [work-dir (temp-dir "abc-ingest-work")
          persons-dir (temp-dir "abc-ingest-persons")]
      (try
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file work-dir "metadata-record.json"))
          :persons-output-dir (str persons-dir)})
        (is (.exists (io/file work-dir "metadata-record.json")))
        (is (.exists (io/file persons-dir "000879.json")))
        (let [person (files/read-json (str (io/file persons-dir "000879.json")))]
          (is (= :ok (pr/validate! person)))
          (is (= "000879" (get person "person_id"))))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-deterministic-test
  (testing "two runs with the same inputs produce byte-identical outputs"
    (let [d1-work (temp-dir "abc-ingest-w1")
          d1-persons (temp-dir "abc-ingest-p1")
          d2-work (temp-dir "abc-ingest-w2")
          d2-persons (temp-dir "abc-ingest-p2")
          opts (fn [w p] {:rows synthetic-rows-with-edition
                          :work-id "000127"
                          :output (str (io/file w "metadata-record.json"))
                          :persons-output-dir (str p)})]
      (try
        (ingest/run-from-rows! (opts d1-work d1-persons))
        (ingest/run-from-rows! (opts d2-work d2-persons))
        (is (= (slurp (io/file d1-work "metadata-record.json"))
               (slurp (io/file d2-work "metadata-record.json"))))
        (is (= (slurp (io/file d1-persons "000879.json"))
               (slurp (io/file d2-persons "000879.json"))))
        (finally
          (delete-recursive d1-work) (delete-recursive d1-persons)
          (delete-recursive d2-work) (delete-recursive d2-persons))))))

(deftest ingest-refuse-overwrite-on-divergent-person-test
  (testing "with a divergent on-disk person file and no --overwrite, ingest fails"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json")
              (str "{\"person_record_schema_id\":\"https://w3id.org/abc/schemas/person-record.schema.json\","
                   "\"person_record_schema_hash\":\"sha256:0000000000000000000000000000000000000000000000000000000000000000\","
                   "\"person_id\":\"000879\","
                   "\"family_name\":\"divergent\","
                   "\"given_name\":\"divergent\","
                   "\"person_copyright_expired\":true,"
                   "\"external_links\":[]}"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"000879"
             (ingest/run-from-rows!
              {:rows synthetic-rows-with-edition
               :work-id "000127"
               :output (str (io/file work-dir "metadata-record.json"))
               :persons-output-dir (str persons-dir)})))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-overwrite-flag-replaces-divergent-test
  (testing "with --overwrite, a divergent on-disk person file is replaced"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json")
              (str "{\"person_record_schema_id\":\"https://w3id.org/abc/schemas/person-record.schema.json\","
                   "\"person_record_schema_hash\":\"sha256:0000000000000000000000000000000000000000000000000000000000000000\","
                   "\"person_id\":\"000879\","
                   "\"family_name\":\"divergent\","
                   "\"given_name\":\"divergent\","
                   "\"person_copyright_expired\":true,"
                   "\"external_links\":[]}"))
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file work-dir "metadata-record.json"))
          :persons-output-dir (str persons-dir)
          :overwrite true})
        (is (= "芥川" (get (files/read-json (str (io/file persons-dir "000879.json")))
                         "family_name")))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))

(deftest ingest-corruption-safe-fails-on-unparseable-test
  (testing "an unparseable on-disk person file fails ingest with a loud error"
    (let [work-dir (temp-dir "abc-ingest-w")
          persons-dir (temp-dir "abc-ingest-p")]
      (try
        (spit (io/file persons-dir "000879.json") "not-json{{{")
        (is (thrown? clojure.lang.ExceptionInfo
                     (ingest/run-from-rows!
                      {:rows synthetic-rows-with-edition
                       :work-id "000127"
                       :output (str (io/file work-dir "metadata-record.json"))
                       :persons-output-dir (str persons-dir)})))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))
