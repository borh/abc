(ns abc.tools.aozora-history-audit-test
  (:require [abc.tools.aozora-history-audit :as audit]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as drift]
            [abc.tools.person-record :as person-record]
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

(defn- commit-text! [^Git git root rel content message]
  (let [file (io/file root rel)]
    (io/make-parents file)
    (spit file content)
    (-> git .add (.addFilepattern rel) .call)
    (-> git
        .commit
        (.setMessage message)
        (.setAuthor "ABC Test" "abc@example.test")
        (.setCommitter "ABC Test" "abc@example.test")
        .call)))

(defn- write-drift-sidecars! [persons-dir event]
  (let [event-with-id (drift/materialize-event-id event)
        event-id (get event-with-id "drift_event_id")
        events-dir (io/file persons-dir "_events")
        indexes-dir (io/file persons-dir "_indexes")]
    (.mkdirs events-dir)
    (.mkdirs indexes-dir)
    (json/write-deterministic-json-file!
     (io/file events-dir (str event-id ".json"))
     event-with-id)
    (doseq [participant (get event-with-id "participants")]
      (json/write-deterministic-json-file!
       (io/file indexes-dir (str (get participant "person_id") ".json"))
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" (get participant "person_id")
        "drift_event_ids" [event-id]}))
    event-with-id))

(defn- synthetic-person-record [person-id family-name]
  {"person_record_schema_id" "https://w3id.org/abc/schemas/person-record.schema.json"
   "person_record_schema_hash" (manifest/schema-hash "schemas/person-record.schema.json")
   "person_id" person-id
   "family_name" family-name
   "given_name" "人"
   "family_name_reading" "せい"
   "given_name_reading" "ひと"
   "family_name_sort" "せい"
   "given_name_sort" "ひと"
   "family_name_romaji" "Sei"
   "given_name_romaji" "Hito"
   "date_of_birth" "1900-01-01"
   "date_of_death" "1970-01-01"
   "person_copyright_expired" true
   "external_links" []})

(defn- write-corpus! [root persons-by-id]
  (let [persons-dir (io/file root "persons")
        works-dir (io/file root "works")]
    (.mkdirs persons-dir)
    (.mkdirs works-dir)
    (doseq [[person-id record] persons-by-id]
      (json/write-deterministic-json-file!
       (io/file persons-dir (str person-id ".json"))
       record))
    (json/write-deterministic-json-file!
     (io/file works-dir "000100.json")
     {"metadata_record_schema_id" "https://w3id.org/abc/schemas/metadata-record.schema.json"
      "metadata_record_schema_hash" (manifest/schema-hash "schemas/metadata-record.schema.json")
      "work" {"work_id" "000100"
              "title" "テスト作品"
              "title_reading" "てすとさくひん"
              "title_sort" "てすとさくひん"
              "subtitle" nil
              "subtitle_reading" nil
              "original_title" nil
              "first_appearance" nil
              "ndc" "NDC 913"
              "orthography" "新字新仮名"
              "work_copyright_expired" true
              "publication_date" "1997-10-29"
              "last_updated" "2022-07-16"
              "card_url" "https://www.aozora.gr.jp/cards/000001/card100.html"
              "source_editions" [{"edition_name" "テスト作品"
                                  "publisher" "テスト出版社"
                                  "first_edition_year" nil
                                  "input_edition" nil
                                  "proofing_edition" nil
                                  "parent_edition_name" nil
                                  "parent_publisher" nil
                                  "parent_first_edition_year" nil}]}
      "contributors" (vec
                      (for [[person-id record] (sort-by key persons-by-id)]
                        {"person_id" person-id
                         "person_record_hash" (person-record/record-hash record)
                         "relation_to_work" "著者"}))})))

(deftest drift-participant-updates-empty-without-sidecars-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (is (= []
             (audit/drift-participant-updates
              {:previous-dir (str previous-dir)
               :current-dir (str current-dir)
               :drift-persons-dir (str drift-dir)})))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))

(deftest drift-participant-updates-report-hash-changes-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川改")})
      (let [previous-person (files/read-json (io/file previous-dir "persons" "000879.json"))
            current-person (files/read-json (io/file current-dir "persons" "000879.json"))
            first-successor (synthetic-person-record "abc-000000000001" "芥川一")
            second-successor (synthetic-person-record "abc-000000000002" "芥川二")
            event (write-drift-sidecars!
                   drift-dir
                   {"schema_id" drift/event-schema-id
                    "schema_hash" (manifest/schema-hash drift/event-schema-path)
                    "drift_event_type" "split"
                    "date" "2026-04-30"
                    "participants" [{"snapshot_id" "post-abc-000000000001"
                                     "person_id" "abc-000000000001"
                                     "person_record_hash" (person-record/record-hash first-successor)}
                                    {"snapshot_id" "post-abc-000000000002"
                                     "person_id" "abc-000000000002"
                                     "person_record_hash" (person-record/record-hash second-successor)}
                                    {"snapshot_id" "pre-000879"
                                     "person_id" "000879"
                                     "person_record_hash" (person-record/record-hash previous-person)}]
                    "evidence" ["https://example.org/drift-evidence"]
                    "prov" {"used" ["pre-000879"]
                            "was_generated_by" ["post-abc-000000000001"
                                                "post-abc-000000000002"]
                            "qualified_association" {"agent" "https://w3id.org/abc/agents/test"
                                                     "had_role" "abc:DriftEditor"}}})
            updates (audit/drift-participant-updates
                     {:previous-dir (str previous-dir)
                      :current-dir (str current-dir)
                      :drift-persons-dir (str drift-dir)})]
        (is (= [{"person_id" "000879"
                 "change_type" "hash_changed"
                 "previous_hash" (person-record/record-hash previous-person)
                 "current_hash" (person-record/record-hash current-person)
                 "drift_event_ids" [(get event "drift_event_id")]}]
               updates)))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))

(deftest drift-participant-updates-reject-invalid-sidecars-test
  (let [previous-dir (temp-dir "abc-audit-prev")
        current-dir (temp-dir "abc-audit-cur")
        drift-dir (temp-dir "abc-audit-drift")
        indexes-dir (io/file drift-dir "_indexes")]
    (try
      (write-corpus! previous-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (write-corpus! current-dir {"000879" (synthetic-person-record "000879" "芥川")})
      (.mkdirs indexes-dir)
      (json/write-deterministic-json-file!
       (io/file indexes-dir "000879.json")
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" "000879"
        "drift_event_ids" ["sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]})
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"drift sidecars failed validation"
           (audit/drift-participant-updates
            {:previous-dir (str previous-dir)
             :current-dir (str current-dir)
             :drift-persons-dir (str drift-dir)})))
      (finally
        (delete-recursive previous-dir)
        (delete-recursive current-dir)
        (delete-recursive drift-dir)))))

(deftest audit-history-uses-git-refs-and-flags-real-split-evidence-test
  (testing "two upstream refs are extracted, ingested, validated, and compared"
    (let [repo-dir (temp-dir "abc-history-audit-repo")
          work-dir (temp-dir "abc-history-audit-work")
          drift-dir (temp-dir "abc-history-audit-drift")
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
              event (write-drift-sidecars!
                     drift-dir
                     {"schema_id" drift/event-schema-id
                      "schema_hash" (manifest/schema-hash drift/event-schema-path)
                      "drift_event_type" "split"
                      "date" "2026-04-30"
                      "participants" [{"snapshot_id" "post-abc-000000000001"
                                       "person_id" "abc-000000000001"
                                       "person_record_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"}
                                      {"snapshot_id" "post-abc-000000000002"
                                       "person_id" "abc-000000000002"
                                       "person_record_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"}
                                      {"snapshot_id" "pre-000001"
                                       "person_id" "000001"
                                       "person_record_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000000"}]
                      "evidence" ["https://example.org/drift-evidence"]
                      "prov" {"used" ["pre-000001"]
                              "was_generated_by" ["post-abc-000000000001"
                                                  "post-abc-000000000002"]
                              "qualified_association" {"agent" "https://w3id.org/abc/agents/test"
                                                       "had_role" "abc:DriftEditor"}}})
              result (audit/audit! {:aozora-repo (str repo-dir)
                                    :previous-ref (.getName old-commit)
                                    :current-ref (.getName new-commit)
                                    :drift-persons-dir (str drift-dir)
                                    :work-dir (str work-dir)})]
          (is (= "ok" (:status result)))
          (is (= 0 (get-in result [:validation :current :failed])))
          (is (= 1 (get-in result [:drift "summary" "split_candidates"])))
          (is (= [{"work_id" "000100"
                   "relation_to_work" "著者"
                   "source_person_ids" ["000001"]
                   "target_person_ids" ["abc-000000000001" "abc-000000000002"]}]
                 (get-in result [:drift "split_candidates"])))
          (let [updates (:drift_participant_updates result)]
            (is (= #{"000001" "abc-000000000001" "abc-000000000002"}
                   (set (map #(get % "person_id") updates))))
            (is (= #{"removed" "added"}
                   (set (map #(get % "change_type") updates))))
            (is (every? #(= [(get event "drift_event_id")]
                            (get % "drift_event_ids"))
                        updates))))
        (finally
          (.close git)
          (delete-recursive repo-dir)
          (delete-recursive work-dir)
          (delete-recursive drift-dir))))))

(deftest scan-history-audits-adjacent-zip-changing-commits-test
  (testing "scan mode walks adjacent commits that changed the upstream CSV ZIP"
    (let [repo-dir (temp-dir "abc-history-scan-repo")
          work-dir (temp-dir "abc-history-scan-work")
          git (-> (Git/init) (.setDirectory repo-dir) .call)]
      (try
        (let [old-commit (commit-zip!
                          git repo-dir
                          (zip-bytes (csv-text [(row {})]))
                          "old corpus")
              _unrelated (commit-text! git repo-dir "README.md" "not a csv change"
                                       "unrelated")
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
              result (audit/scan-history! {:aozora-repo (str repo-dir)
                                           :from-ref (.getName old-commit)
                                           :to-ref (.getName new-commit)
                                           :max-pairs 1
                                           :work-dir (str work-dir)})]
          (is (= "ok" (:status result)))
          (is (= {"pairs_scanned" 1
                  "validation_failed" 0
                  "split_candidates" 1
                  "merge_candidates" 0
                  "drift_participant_updates" 0}
                 (:summary result)))
          (is (= [{:previous-ref (.getName old-commit)
                   :current-ref (.getName new-commit)
                   :status "ok"
                   :split-candidates 1
                   :merge-candidates 0
                   :drift-participant-updates 0}]
                 (mapv #(select-keys % [:previous-ref :current-ref :status
                                         :split-candidates :merge-candidates
                                         :drift-participant-updates])
                       (:pairs result)))))
        (finally
          (.close git)
          (delete-recursive repo-dir)
          (delete-recursive work-dir))))))
