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

(defn- person-row [overrides]
  ;; Variant of `row` letting us vary work_id / person_id / names in one place.
  (row overrides))

(def synthetic-corpus-rows
  ;; Works 000127 and 000128 share person 000879 (Akutagawa).
  ;; Work 000129 has a different person 000888 (Soseki).
  [(person-row {"作品ID" "000127"
                "底本名1" "羅生門" "底本出版社名1" "テスト出版社"})
   (person-row {"作品ID" "000128"
                "作品名" "鼻" "作品名読み" "はな" "ソート用読み" "はな"
                "底本名1" "鼻" "底本出版社名1" "テスト出版社"})
   (person-row {"作品ID" "000129"
                "人物ID" "000888"
                "作品名" "吾輩は猫である" "作品名読み" "わがはいはねこである"
                "ソート用読み" "わかはいはねこてある"
                "姓" "夏目" "名" "漱石"
                "姓読み" "なつめ" "名読み" "そうせき"
                "姓読みソート用" "なつめ" "名読みソート用" "そうせき"
                "姓ローマ字" "Natsume" "名ローマ字" "Soseki"
                "生年月日" "1867-02-09" "没年月日" "1916-12-09"
                "底本名1" "吾輩は猫である" "底本出版社名1" "テスト出版社"})])

(deftest ingest-corpus-emits-work-and-person-files-test
  (testing "run-corpus! writes one metadata-record per work and dedups persons"
    (let [out-dir (temp-dir "abc-ingest-corpus")]
      (try
        (let [{:keys [works-written persons-written]}
              (ingest/run-corpus!
               {:rows synthetic-corpus-rows
                :output-dir (str out-dir)})]
          (is (= 3 works-written))
          (is (= 2 persons-written)))
        (is (.exists (io/file out-dir "works" "000127.json")))
        (is (.exists (io/file out-dir "works" "000128.json")))
        (is (.exists (io/file out-dir "works" "000129.json")))
        (is (.exists (io/file out-dir "persons" "000879.json")))
        (is (.exists (io/file out-dir "persons" "000888.json")))
        ;; Two works that share a person reference the same person_record_hash.
        (let [m1 (files/read-json (str (io/file out-dir "works" "000127.json")))
              m2 (files/read-json (str (io/file out-dir "works" "000128.json")))
              h1 (-> m1 (get "contributors") first (get "person_record_hash"))
              h2 (-> m2 (get "contributors") first (get "person_record_hash"))]
          (is (= h1 h2)))
        (finally
          (delete-recursive out-dir))))))

(deftest ingest-corpus-deterministic-test
  (testing "two corpus runs produce byte-identical output"
    (let [d1 (temp-dir "abc-ingest-corpus-1")
          d2 (temp-dir "abc-ingest-corpus-2")]
      (try
        (ingest/run-corpus! {:rows synthetic-corpus-rows :output-dir (str d1)})
        (ingest/run-corpus! {:rows synthetic-corpus-rows :output-dir (str d2)})
        (doseq [rel ["works/000127.json" "works/000128.json" "works/000129.json"
                     "persons/000879.json" "persons/000888.json"]]
          (is (= (slurp (io/file d1 rel)) (slurp (io/file d2 rel)))
              (str rel " differs between runs")))
        (finally
          (delete-recursive d1)
          (delete-recursive d2))))))

(deftest ingest-corpus-byte-identical-to-single-work-test
  (testing "running corpus over the single-work fixture matches the single-work output"
    (let [single-work-dir (temp-dir "abc-single")
          single-persons-dir (temp-dir "abc-single-persons")
          corpus-dir (temp-dir "abc-corpus-single")]
      (try
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file single-work-dir "metadata-record.json"))
          :persons-output-dir (str single-persons-dir)})
        (ingest/run-corpus!
         {:rows synthetic-rows-with-edition
          :output-dir (str corpus-dir)})
        (is (= (slurp (io/file single-work-dir "metadata-record.json"))
               (slurp (io/file corpus-dir "works" "000127.json"))))
        (is (= (slurp (io/file single-persons-dir "000879.json"))
               (slurp (io/file corpus-dir "persons" "000879.json"))))
        (finally
          (delete-recursive single-work-dir)
          (delete-recursive single-persons-dir)
          (delete-recursive corpus-dir))))))

(deftest ingest-refresh-manifest-roundtrip-test
  (testing "--refresh-manifest rewrites metadata_record_hash and recomputes artifact_id; re-run is byte-identical"
    (let [work-dir (temp-dir "abc-ingest-rm-w")
          persons-dir (temp-dir "abc-ingest-rm-p")
          manifest-path (str (io/file work-dir "manifest.json"))]
      (try
        ;; Seed a manifest stub with a stale metadata_record_hash and artifact_id.
        (require '[abc.tools.json :as j])
        (let [seed-base (files/read-json "examples/v0/example-work/manifest.json")
              seed (-> seed-base
                       (assoc-in ["manifest_identity_object" "metadata_record_hash"]
                                 "sha256:0000000000000000000000000000000000000000000000000000000000000000")
                       (assoc "artifact_id"
                              "sha256:0000000000000000000000000000000000000000000000000000000000000000"))]
          ((resolve 'abc.tools.json/write-deterministic-json-file!)
           (io/file manifest-path) seed))
        (ingest/run-from-rows!
         {:rows synthetic-rows-with-edition
          :work-id "000127"
          :output (str (io/file work-dir "metadata-record.json"))
          :persons-output-dir (str persons-dir)
          :refresh-manifest manifest-path})
        (let [m (files/read-json manifest-path)
              new-mr-hash (get-in m ["manifest_identity_object" "metadata_record_hash"])
              new-artifact (get m "artifact_id")]
          (is (re-matches #"^sha256:[0-9a-f]{64}$" new-mr-hash))
          (is (re-matches #"^sha256:[0-9a-f]{64}$" new-artifact))
          (is (not= "sha256:0000000000000000000000000000000000000000000000000000000000000000"
                    new-artifact)))
        ;; Re-run is byte-identical.
        (let [first-bytes (slurp manifest-path)]
          (ingest/run-from-rows!
           {:rows synthetic-rows-with-edition
            :work-id "000127"
            :output (str (io/file work-dir "metadata-record.json"))
            :persons-output-dir (str persons-dir)
            :refresh-manifest manifest-path})
          (is (= first-bytes (slurp manifest-path))))
        (finally
          (delete-recursive work-dir)
          (delete-recursive persons-dir))))))
