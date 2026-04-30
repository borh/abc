(ns abc.tools.aozora-history-audit-test
  (:require [abc.tools.aozora-history-audit :as audit]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipEntry ZipOutputStream]
           [org.eclipse.jgit.api Git]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(defn- row [overrides]
  (merge {"作品ID" "000100"
          "人物ID" "000001"
          "役割フラグ" "著者"
          "作品名" "テスト作品"
          "作品名読み" "てすとさくひん"
          "ソート用読み" "てすとさくひん"
          "副題" "" "副題読み" "" "原題" "" "初出" ""
          "分類番号" "NDC 913" "文字遣い種別" "新字新仮名"
          "作品著作権フラグ" "なし" "公開日" "1997-10-29"
          "最終更新日" "2022-07-16"
          "図書カードURL" "https://www.aozora.gr.jp/cards/000001/card100.html"
          "底本名1" "テスト作品" "底本出版社名1" "テスト出版社"
          "底本名2" "" "底本出版社名2" ""
          "底本初版発行年1" "" "底本初版発行年2" ""
          "入力に使用した版1" "" "入力に使用した版2" ""
          "校正に使用した版1" "" "校正に使用した版2" ""
          "底本の親本名1" "" "底本の親本名2" ""
          "底本の親本出版社名1" "" "底本の親本出版社名2" ""
          "底本の親本初版発行年1" "" "底本の親本初版発行年2" ""
          "姓" "旧" "名" "人"
          "姓読み" "きゅう" "名読み" "ひと"
          "姓読みソート用" "きゆう" "名読みソート用" "ひと"
          "姓ローマ字" "Old" "名ローマ字" "Person"
          "生年月日" "1900-01-01" "没年月日" "1970-01-01"
          "人物著作権フラグ" "なし"}
         overrides))

(defn- csv-text [rows]
  (let [headers (vec (sort (keys (first rows))))]
    (->> (cons headers
               (map (fn [row] (mapv #(get row % "") headers)) rows))
         (map #(string/join "," %))
         (string/join "\n"))))

(defn- zip-bytes [csv]
  (let [out (java.io.ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (ZipEntry. "list_person_all_extended_utf8.csv"))
      (.write zip (.getBytes csv StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

(defn- commit-zip! [^Git git root bytes message]
  (let [file (io/file root "index_pages/list_person_all_extended_utf8.zip")]
    (io/make-parents file)
    (with-open [out (io/output-stream file)]
      (.write out bytes))
    (-> git .add (.addFilepattern "index_pages/list_person_all_extended_utf8.zip") .call)
    (-> git
        .commit
        (.setMessage message)
        (.setAuthor "ABC Test" "abc@example.test")
        (.setCommitter "ABC Test" "abc@example.test")
        .call)))

(deftest audit-history-uses-git-refs-and-flags-real-split-evidence-test
  (testing "two upstream refs are extracted, ingested, validated, and compared"
    (let [repo-dir (temp-dir "abc-history-audit-repo")
          work-dir (temp-dir "abc-history-audit-work")
          git (-> (Git/init) (.setDirectory repo-dir) .call)]
      (try
        (let [old-commit (commit-zip!
                          git repo-dir
                          (zip-bytes (csv-text [(row {})]))
                          "old corpus")
              new-commit (commit-zip!
                          git repo-dir
                          (zip-bytes
                           (csv-text [(row {"人物ID" "abc-000000000001"
                                            "姓" "新" "名" "一"
                                            "姓読み" "しん" "名読み" "いち"
                                            "姓読みソート用" "しん" "名読みソート用" "いち"
                                            "姓ローマ字" "New" "名ローマ字" "One"})
                                      (row {"人物ID" "abc-000000000002"
                                            "姓" "新" "名" "二"
                                            "姓読み" "しん" "名読み" "に"
                                            "姓読みソート用" "しん" "名読みソート用" "に"
                                            "姓ローマ字" "New" "名ローマ字" "Two"})]))
                          "new corpus")
              result (audit/audit! {:aozora-repo (str repo-dir)
                                    :previous-ref (.getName old-commit)
                                    :current-ref (.getName new-commit)
                                    :work-dir (str work-dir)})]
          (is (= "ok" (get result "status")))
          (is (= 0 (get-in result ["validation" "current" "failed"])))
          (is (= 1 (get-in result ["drift" "summary" "split_candidates"])))
          (is (= [{"work_id" "000100"
                   "relation_to_work" "著者"
                   "source_person_ids" ["000001"]
                   "target_person_ids" ["abc-000000000001" "abc-000000000002"]}]
                 (get-in result ["drift" "split_candidates"]))))
        (finally
          (.close git)
          (delete-recursive repo-dir)
          (delete-recursive work-dir))))))
