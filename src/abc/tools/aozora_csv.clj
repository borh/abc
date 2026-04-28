(ns abc.tools.aozora-csv
  "Read Aozora's list_person_all_extended_utf8.csv (one row per
  work-author-role tuple) into JSON-shaped record fragments. Pure;
  no I/O beyond reading the supplied path or string. NFC normalization
  applied at the parse boundary."
  (:require [charred.api :as charred]
            [clojure.java.io :as io])
  (:import [java.text Normalizer Normalizer$Form]))

(def ^:private bom-char (char 0xFEFF))

(defn- nfc [^String s]
  (when s (Normalizer/normalize s Normalizer$Form/NFC)))

(defn- strip-bom [^String s]
  (if (and s (pos? (.length s)) (= bom-char (.charAt s 0)))
    (.substring s 1)
    s))

(defn- read-rows*
  "Internal: takes a charred result (vector of vectors, first row is
  header) and returns a seq of maps keyed by header column. Strips
  the UTF-8 BOM from the first header cell."
  [rows]
  (when (seq rows)
    (let [header (mapv (fn [c] (strip-bom (or c ""))) (first rows))]
      (mapv (fn [r]
              (into {}
                    (map (fn [k v] [k (or v "")]) header r)))
            (rest rows)))))

(defn read-rows-from-string [^String s]
  (read-rows* (charred/read-csv s)))

(defn read-rows [^String path]
  (with-open [r (io/reader path)]
    (read-rows* (charred/read-csv r))))

(defn- nullable
  "Empty CSV cell -> JSON null. Non-empty -> NFC-normalized."
  [s]
  (when (and s (not= "" s)) (nfc s)))

(defn- nonblank [s]
  (when (and s (not= "" s)) s))

(defn- parse-bool-flag
  "Aozora's *著作権フラグ uses 'なし' (no copyright = expired)
  and 'あり' (has copyright = active)."
  [s]
  (case s
    "なし" true
    "あり" false
    nil))

(defn- source-edition-from
  "Build a source-edition map from the columns indexed `n` (1 or 2).
  Returns nil when both title and publisher are blank."
  [row n]
  (let [k (fn [base] (str base n))
        title (nonblank (get row (k "底本名")))
        publisher (nonblank (get row (k "底本出版社名")))]
    (when (and title publisher)
      (cond-> {"title" (nfc title)
               "publisher" (nfc publisher)}
        (nonblank (get row (k "底本初版発行年")))
        (assoc "first_edition_year" (nfc (get row (k "底本初版発行年"))))
        (nonblank (get row (k "入力に使用した版")))
        (assoc "input_edition" (nfc (get row (k "入力に使用した版"))))
        (nonblank (get row (k "校正に使用した版")))
        (assoc "proof_edition" (nfc (get row (k "校正に使用した版"))))
        (nonblank (get row (k "底本の親本名")))
        (assoc "parent_title" (nfc (get row (k "底本の親本名"))))
        (nonblank (get row (k "底本の親本出版社名")))
        (assoc "parent_publisher" (nfc (get row (k "底本の親本出版社名"))))
        (nonblank (get row (k "底本の親本初版発行年")))
        (assoc "parent_first_edition_year"
               (nfc (get row (k "底本の親本初版発行年"))))))))

(defn parse-work-fields-from-row [row]
  (let [editions (filterv some? [(source-edition-from row 1)
                                 (source-edition-from row 2)])]
    {"work_id" (get row "作品ID")
     "title" (nfc (get row "作品名"))
     "title_reading" (nullable (get row "作品名読み"))
     "sort_reading" (nullable (get row "ソート用読み"))
     "subtitle" (nullable (get row "副題"))
     "subtitle_reading" (nullable (get row "副題読み"))
     "original_title" (nullable (get row "原題"))
     "first_published" (nullable (get row "初出"))
     "ndc" (get row "分類番号")
     "orthographic_style" (get row "文字遣い種別")
     "copyright_expired" (parse-bool-flag (get row "作品著作権フラグ"))
     "aozora_available" (get row "公開日")
     "aozora_modified" (get row "最終更新日")
     "card_url" (get row "図書カードURL")
     "source_editions" editions}))

(defn parse-person-fields-from-row [row]
  {"person_id" (get row "人物ID")
   "family_name" (nfc (get row "姓"))
   "given_name" (nfc (get row "名"))
   "family_name_reading" (nullable (get row "姓読み"))
   "given_name_reading" (nullable (get row "名読み"))
   "family_name_sort" (nullable (get row "姓読みソート用"))
   "given_name_sort" (nullable (get row "名読みソート用"))
   "family_name_romaji" (nullable (get row "姓ローマ字"))
   "given_name_romaji" (nullable (get row "名ローマ字"))
   "date_of_birth" (nullable (get row "生年月日"))
   "date_of_death" (nullable (get row "没年月日"))
   "person_copyright_expired" (parse-bool-flag (get row "人物著作権フラグ"))
   "external_links" []})

(defn parse-contributor-from-row [row]
  {"person_id" (get row "人物ID")
   "relation_to_work" (get row "役割フラグ")})

(defn build-record-fragment-from-rows
  "Given multiple CSV rows for the same work_id (one per author/role),
  return {:work, :persons-by-id, :contributors}. Asserts work-level
  fields are consistent across rows; persons-by-id is keyed by
  person_id; contributors are sorted by person_id and de-duplicated.
  Throws if a single person_id appears with divergent body fields."
  [rows]
  (assert (seq rows) "build-record-fragment-from-rows requires at least one row")
  (let [works (mapv parse-work-fields-from-row rows)
        work-ids (distinct (map #(get % "work_id") works))]
    (assert (= 1 (count work-ids))
            (str "rows must share work_id; got: " (vec work-ids)))
    (let [person-bodies-by-id
          (->> rows
               (mapv parse-person-fields-from-row)
               (group-by #(get % "person_id"))
               (into {}
                     (map (fn [[pid xs]]
                            (let [unique (distinct xs)]
                              (when (< 1 (count unique))
                                (throw (ex-info
                                        (str "person_id " pid
                                             " has divergent bodies across CSV rows")
                                        {:person-id pid
                                         :bodies unique})))
                              [pid (first unique)])))))
          contributors (->> rows
                            (mapv parse-contributor-from-row)
                            distinct
                            (sort-by #(get % "person_id"))
                            vec)]
      {:work (first works)
       :persons-by-id person-bodies-by-id
       :contributors contributors})))
