(ns abc.tools.aozora-csv
  "Read Aozora's list_person_all_extended_utf8.csv (one row per
  work-author-role tuple) into JSON-shaped record fragments. Pure;
  no I/O beyond reading the supplied path or string. NFC normalization
  applied at the parse boundary."
  (:require [charred.api :as charred]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.text Normalizer Normalizer$Form]
           [java.time DateTimeException LocalDate YearMonth]))

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

(def ^:private bce-pattern #"^前(\d+)$")
(def ^:private partial-date-pattern
  #"^(-?\d{1,4})(?:-\s*(\d{1,2})(?:-(\d{1,2}))?)?$")
(def ^:private unknown-marker-set
  "Aozora date sentinels that mean 'not known'. Mapped to null at parse
  time; preserved in parse_corrections under rule `unknown-marker`."
  #{"不詳" "未詳"})
(def ^:private multi-dash-pattern #"--+")
(def ^:private century-prose-pattern
  ;; Japanese BCE century prose with optional sub-century qualifier.
  ;; The qualifier (`初頭`/`初`/`末`/`半ば`/`前半`/`後半`) is captured
  ;; only for the audit trail — EDTF Level 1 has no sub-century
  ;; precision, so the qualifier does not influence the canonical
  ;; lexical output. ADR 0016.
  #"^紀元前(\d+)世紀(初頭|初|末|半ば|前半|後半)?$")
(def ^:private decade-pattern
  ;; EDTF Level 1 decade marker (capital-X digit placeholder), with
  ;; optional leading `-` for BCE. ADR 0016.
  #"^-?\d{3}X$")

(defn- pad4-year [^String s]
  (let [neg? (string/starts-with? s "-")
        digits (if neg? (subs s 1) s)
        padded (format "%04d" (Integer/parseInt digits))]
    (if neg? (str "-" padded) padded)))

(defn- pad2 [^String s]
  (format "%02d" (Integer/parseInt s)))

(defn- bce->astronomical
  "前N (N BCE) → astronomical year lexical, per ISO 8601-2 / XSD 1.1.
  N=1 → '0000' (1 BCE = year 0); N≥2 → '-NNNN' (N BCE = year -(N-1))."
  [^String n-str]
  (let [n (Integer/parseInt n-str)]
    (if (= n 1) "0000" (format "-%04d" (dec n)))))

(defn- bce-century->edtf
  "紀元前N世紀 → EDTF Level 1 century marker `-{N-1:02d}XX`. Lossless
  at century precision under ISO 8601-2 / XSD 1.1 astronomical year
  numbering: 紀元前N世紀 covers astronomical interval
  `[-(100N - 1), -((N - 1) · 100)]`, which matches the EDTF lexical
  `-{N-1:02d}XX`. Two-digit zero-padding holds for N ≤ 100; deeper
  history is not exercised by the corpus. ADR 0016."
  [^String n-str]
  (format "-%02dXX" (dec (Integer/parseInt n-str))))

(defn- valid-calendar-shape?
  "Reject impossible calendar dates (Feb 31, etc.) for YYYY-MM-DD /
  YYYY-MM lexical forms — including negative-year (BCE) forms, which
  java.time accepts as a leading `-` in `LocalDate.parse` /
  `YearMonth.parse`. The schema regex rules out month ≤ 0, ≥ 13,
  day ≤ 0, ≥ 32; this catches month/day pairs that the regex can't
  (e.g. 2020-02-31, -0426-02-31). YYYY-only forms have no
  day-of-year to validate."
  [^String s]
  (cond
    (nil? s) true
    (re-matches #"^-?\d{4}-\d{2}-\d{2}$" s)
    (try (LocalDate/parse s) true (catch DateTimeException _ false))
    (re-matches #"^-?\d{4}-\d{2}$" s)
    (try (YearMonth/parse s) true (catch DateTimeException _ false))
    :else true))

(defn parse-date
  "Normalize an Aozora date-cell string to the v0.1 EDTF lexical union
  (Level 0: YYYY[-MM[-DD]] with optional leading '-'; Level 1: decade
  YYYX and century YYXX, both with optional leading '-'). Returns
  `[normalized corrections]` where `corrections` is a vector of
  `{raw, corrected, rule}` maps (the caller adds `field`).

  Cosmetic rules: `pad-year`, `pad-month`, `pad-day`,
  `strip-whitespace`, `collapse-multi-dash`. Semantic rules:
  `bce-astronomical`, `unknown-marker`, `century-prose`. Decade
  markers (`192X`) are admitted verbatim with no rule entry — no
  rewrite occurs. ADR 0015 / ADR 0016."
  [raw]
  (cond
    (or (nil? raw) (= "" raw))
    [nil []]

    (contains? unknown-marker-set (string/trim raw))
    [nil [{"raw" raw "corrected" nil "rule" "unknown-marker"}]]

    :else
    (let [raw-str (string/trim raw)]
      (cond
        ;; 前N (CE-relative BCE notation) → astronomical year. ADR 0015.
        (re-matches bce-pattern raw-str)
        (let [n-str (second (re-matches bce-pattern raw-str))
              n (Integer/parseInt n-str)]
          (if (pos? n)
            (let [corrected (bce->astronomical n-str)]
              [corrected
               [{"raw" raw "corrected" corrected "rule" "bce-astronomical"}]])
            ;; 前0 / 前000 has no astronomical equivalent (the BCE
            ;; calendar starts at 1 BCE). Pass raw through; downstream
            ;; schema validation will reject the Japanese-character
            ;; lexical form.
            [raw-str []]))

        ;; 紀元前N世紀(初頭|初|末|半ば|前半|後半)? → EDTF Level 1
        ;; century marker. The qualifier is preserved in the raw
        ;; field of the audit entry but does not influence the
        ;; corrected lexical form (no sub-century precision in EDTF
        ;; Level 1). ADR 0016.
        (re-matches century-prose-pattern raw-str)
        (let [n-str (second (re-matches century-prose-pattern raw-str))
              corrected (bce-century->edtf n-str)]
          [corrected
           [{"raw" raw "corrected" corrected "rule" "century-prose"}]])

        ;; EDTF Level 1 decade marker (`192X`) — admitted verbatim.
        ;; No rewrite, no audit entry. ADR 0016.
        (re-matches decade-pattern raw-str)
        [raw-str []]

        :else
        (let [had-interior-space? (boolean (re-find #"\s" raw-str))
              despaced (string/replace raw-str #"\s+" "")
              had-multi-dash? (boolean (re-find multi-dash-pattern despaced))
              cleaned (string/replace despaced multi-dash-pattern "-")]
          (if-let [m (re-matches partial-date-pattern cleaned)]
            (let [[_ y mo d] m
                  y-digits (count (cond-> y (string/starts-with? y "-") (subs 1)))
                  rules (cond-> []
                          had-interior-space? (conj "strip-whitespace")
                          had-multi-dash? (conj "collapse-multi-dash")
                          (< y-digits 4) (conj "pad-year")
                          (and mo (= 1 (count mo))) (conj "pad-month")
                          (and d (= 1 (count d))) (conj "pad-day"))
                  corrected (cond
                              (and y mo d) (str (pad4-year y) "-" (pad2 mo) "-" (pad2 d))
                              (and y mo)   (str (pad4-year y) "-" (pad2 mo))
                              :else        (pad4-year y))]
              (if (valid-calendar-shape? corrected)
                [corrected
                 (mapv (fn [r] {"raw" raw "corrected" corrected "rule" r}) rules)]
                ;; Regex-shaped but impossible calendar date (e.g. 2020-02-31).
                ;; Pass through verbatim; downstream validation will reject it.
                [raw-str []]))

            ;; Unparseable shape; pass through verbatim. Schema
            ;; validation will reject it downstream.
            [raw-str []]))))))

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
     "ndc" (let [v (some-> (get row "分類番号") nfc string/trim)]
             (when (and v (re-matches #"NDC [0-9A-Z]+( [0-9A-Z]+)*" v))
               v))
     "orthographic_style" (get row "文字遣い種別")
     "copyright_expired" (parse-bool-flag (get row "作品著作権フラグ"))
     "aozora_available" (get row "公開日")
     "aozora_modified" (get row "最終更新日")
     "card_url" (get row "図書カードURL")
     "source_editions" editions}))

(defn parse-person-fields-from-row
  "Returns `{:fields, :corrections}`. `:fields` is the JSON-shaped
  person body (keys per the person-record schema). `:corrections` is
  a vector of `{field, raw, corrected, rule}` audit entries from
  date normalization. ADR 0015."
  [row]
  (let [[dob dob-corrs] (parse-date (get row "生年月日"))
        [dod dod-corrs] (parse-date (get row "没年月日"))
        fields {"person_id" (get row "人物ID")
                "family_name" (nfc (get row "姓"))
                "given_name" (nullable (get row "名"))
                "family_name_reading" (nullable (get row "姓読み"))
                "given_name_reading" (nullable (get row "名読み"))
                "family_name_sort" (nullable (get row "姓読みソート用"))
                "given_name_sort" (nullable (get row "名読みソート用"))
                "family_name_romaji" (nullable (get row "姓ローマ字"))
                "given_name_romaji" (nullable (get row "名ローマ字"))
                "date_of_birth" dob
                "date_of_death" dod
                "person_copyright_expired" (parse-bool-flag (get row "人物著作権フラグ"))
                "external_links" []}
        corrections (-> []
                        (into (mapv #(assoc % "field" "date_of_birth") dob-corrs))
                        (into (mapv #(assoc % "field" "date_of_death") dod-corrs)))]
    {:fields fields :corrections corrections}))

(defn parse-contributor-from-row [row]
  {"person_id" (get row "人物ID")
   "relation_to_work" (get row "役割フラグ")})

(defn build-record-fragment-from-rows
  "Given multiple CSV rows for the same work_id (one per author/role),
  return {:work, :persons-by-id, :contributors, :corrections-by-pid}.
  Asserts work-level fields are consistent across rows; persons-by-id
  is keyed by person_id; contributors are sorted by person_id and
  de-duplicated. Throws if a single person_id appears with divergent
  body fields. `:corrections-by-pid` maps person_id → vector of
  {field, raw, corrected, rule} audit entries (deduplicated)."
  [rows]
  (assert (seq rows) "build-record-fragment-from-rows requires at least one row")
  (let [works (mapv parse-work-fields-from-row rows)
        work-ids (distinct (map #(get % "work_id") works))]
    (assert (= 1 (count work-ids))
            (str "rows must share work_id; got: " (vec work-ids)))
    (let [person-results (mapv parse-person-fields-from-row rows)
          per-pid (group-by #(get-in % [:fields "person_id"]) person-results)
          person-bodies-by-id
          (into {}
                (map (fn [[pid xs]]
                       (let [unique (distinct (mapv :fields xs))]
                         (when (< 1 (count unique))
                           (throw (ex-info
                                   (str "person_id " pid
                                        " has divergent bodies across CSV rows")
                                   {:person-id pid
                                    :bodies unique})))
                         [pid (first unique)])))
                per-pid)
          corrections-by-pid
          (into {}
                (map (fn [[pid xs]]
                       [pid (vec (distinct (mapcat :corrections xs)))]))
                per-pid)
          contributors (->> rows
                            (mapv parse-contributor-from-row)
                            distinct
                            (sort-by #(get % "person_id"))
                            vec)]
      {:work (first works)
       :persons-by-id person-bodies-by-id
       :contributors contributors
       :corrections-by-pid corrections-by-pid})))
