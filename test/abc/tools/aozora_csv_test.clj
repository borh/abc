(ns abc.tools.aozora-csv-test
  (:require [abc.tools.aozora-csv :as ac]
            [clojure.test :refer [deftest is testing]]))

(def ^:private header-line
  (str "﻿作品ID,作品名,作品名読み,ソート用読み,副題,副題読み,原題,初出,"
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
       "XHTML/HTMLファイル修正回数"))

(def ^:private rashomon-row
  (str
   "\"000127\",\"羅生門\",\"らしょうもん\",\"らしようもん\",\"\",\"\",\"\","
   "\"「帝国文学」1915（大正4）年11月号\",\"NDC 913\",\"新字新仮名\","
   "\"なし\",1997-10-29,2022-07-16,"
   "\"https://www.aozora.gr.jp/cards/000879/card127.html\",\"000879\","
   "\"芥川\",\"竜之介\",\"あくたがわ\",\"りゅうのすけ\","
   "\"あくたかわ\",\"りゆうのすけ\",\"Akutagawa\",\"Ryunosuke\","
   "\"著者\",\"1892-03-01\",\"1927-07-24\",\"なし\","
   "\"芥川龍之介全集1\",\"ちくま文庫、筑摩書房\","
   "\"1986（昭和61）年9月24日\",\"1997（平成9）年4月15日第14刷\",\"\","
   "\"筑摩全集類聚　芥川龍之介全集第一巻\",\"筑摩書房\","
   "\"1971（昭和46）年3月5日\",\"\",\"\","
   "\"\",\"\",\"\",\"\",\"\",\"\","
   "\"野口英司、平山誠\",\"もりみつじゅんじ\","
   "\"https://www.aozora.gr.jp/cards/000879/files/127_ruby_150.zip\","
   "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"4\","
   "\"https://www.aozora.gr.jp/cards/000879/files/127_15260.html\","
   "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"2\""))

(def ^:private csv-text
  (str header-line "\n" rashomon-row "\n"))

(deftest read-rows-strips-bom-and-keys-by-header-test
  (testing "read-rows returns column-name-keyed maps; BOM stripped"
    (let [rows (ac/read-rows-from-string csv-text)]
      (is (= 1 (count rows)))
      (is (= "000127" (get (first rows) "作品ID"))
          "BOM must not infect the first column key")
      (is (= "芥川" (get (first rows) "姓"))))))

(deftest parse-work-fields-test
  (testing "parse-work-fields-from-row maps Aozora columns to JSON shape"
    (let [row (first (ac/read-rows-from-string csv-text))
          work (ac/parse-work-fields-from-row row)]
      (is (= "000127" (get work "work_id")))
      (is (= "羅生門" (get work "title")))
      (is (= "らしょうもん" (get work "title_reading")))
      (is (= "NDC 913" (get work "ndc")))
      (is (= "新字新仮名" (get work "orthographic_style")))
      (is (true? (get work "copyright_expired")))
      (is (= "1997-10-29" (get work "aozora_available")))
      (is (= "2022-07-16" (get work "aozora_modified")))
      (is (= "https://www.aozora.gr.jp/cards/000879/card127.html"
             (get work "card_url")))
      (is (vector? (get work "source_editions")))
      (is (= "芥川龍之介全集1" (get-in work ["source_editions" 0 "title"])))
      (is (= "ちくま文庫、筑摩書房" (get-in work ["source_editions" 0 "publisher"]))))))

(deftest parse-person-fields-test
  (testing "parse-person-fields-from-row returns {:fields :corrections}; role is NOT in body"
    (let [row (first (ac/read-rows-from-string csv-text))
          {:keys [fields corrections]} (ac/parse-person-fields-from-row row)]
      (is (= "000879" (get fields "person_id")))
      (is (= "芥川" (get fields "family_name")))
      (is (= "竜之介" (get fields "given_name")))
      (is (= "あくたがわ" (get fields "family_name_reading")))
      (is (= "りゅうのすけ" (get fields "given_name_reading")))
      (is (= "Akutagawa" (get fields "family_name_romaji")))
      (is (= "Ryunosuke" (get fields "given_name_romaji")))
      (is (not (contains? fields "relation_to_work"))
          "relation_to_work is a Work-Person edge, not a person body field")
      (is (= "1892-03-01" (get fields "date_of_birth")))
      (is (= "1927-07-24" (get fields "date_of_death")))
      (is (true? (get fields "person_copyright_expired")))
      (is (= [] (get fields "external_links")))
      (is (= [] corrections)
          "ISO dates produce no corrections"))))

(deftest parse-contributor-test
  (testing "parse-contributor-from-row carries person_id + role only"
    (let [row (first (ac/read-rows-from-string csv-text))]
      (is (= {"person_id" "000879" "relation_to_work" "著者"}
             (ac/parse-contributor-from-row row))))))

(deftest build-record-fragment-test
  (testing "build-record-fragment-from-rows returns work, persons, contributors, corrections"
    (let [rows (ac/read-rows-from-string csv-text)
          frag (ac/build-record-fragment-from-rows rows)]
      (is (= "000127" (get-in frag [:work "work_id"])))
      (is (contains? (:persons-by-id frag) "000879"))
      (is (= "芥川" (get-in frag [:persons-by-id "000879" "family_name"])))
      (is (not (contains? (get-in frag [:persons-by-id "000879"])
                          "relation_to_work")))
      (is (= [{"person_id" "000879" "relation_to_work" "著者"}]
             (:contributors frag)))
      (is (= {"000879" []} (:corrections-by-pid frag))
          "ISO-clean rows produce a corrections-by-pid map with empty entries"))))

(deftest parse-date-iso-test
  (testing "parse-date passes through ISO-shaped values without correction"
    (is (= ["1892-03-01" []] (ac/parse-date "1892-03-01")))
    (is (= ["1904-01" []] (ac/parse-date "1904-01")))
    (is (= ["1941" []] (ac/parse-date "1941")))
    (is (= [nil []] (ac/parse-date nil)))
    (is (= [nil []] (ac/parse-date "")))))

(deftest parse-date-pad-month-test
  (testing "parse-date zero-pads single-digit month"
    (let [[norm corrs] (ac/parse-date "1888-6-12")]
      (is (= "1888-06-12" norm))
      (is (= [{"raw" "1888-6-12" "corrected" "1888-06-12" "rule" "pad-month"}]
             corrs)))))

(deftest parse-date-pad-day-test
  (testing "parse-date zero-pads single-digit day"
    (let [[norm corrs] (ac/parse-date "1888-06-1")]
      (is (= "1888-06-01" norm))
      (is (= [{"raw" "1888-06-1" "corrected" "1888-06-01" "rule" "pad-day"}]
             corrs)))))

(deftest parse-date-pad-year-test
  (testing "parse-date zero-pads short years"
    (let [[norm corrs] (ac/parse-date "723-08-15")]
      (is (= "0723-08-15" norm))
      (is (= [{"raw" "723-08-15" "corrected" "0723-08-15" "rule" "pad-year"}]
             corrs)))))

(deftest parse-date-strip-whitespace-test
  (testing "parse-date strips interior whitespace"
    (let [[norm corrs] (ac/parse-date "1869- 02-22")]
      (is (= "1869-02-22" norm))
      (is (= [{"raw" "1869- 02-22" "corrected" "1869-02-22" "rule" "strip-whitespace"}]
             corrs)))))

(deftest parse-date-multi-rule-test
  (testing "parse-date emits one correction per rule when several apply"
    (let [[norm corrs] (ac/parse-date "1869- 6-1")]
      (is (= "1869-06-01" norm))
      (is (= #{"strip-whitespace" "pad-month" "pad-day"}
             (set (map #(get % "rule") corrs))))
      (is (every? #(= "1869- 6-1" (get % "raw")) corrs))
      (is (every? #(= "1869-06-01" (get % "corrected")) corrs)))))

(deftest parse-date-bce-astronomical-test
  (testing "parse-date converts 前N (N BCE) to ISO 8601-2 astronomical year"
    (is (= ["-0426" [{"raw" "前427" "corrected" "-0426" "rule" "bce-astronomical"}]]
           (ac/parse-date "前427"))
        "前427 (427 BCE) → -0426 in astronomical year numbering")
    (is (= ["-0001" [{"raw" "前2" "corrected" "-0001" "rule" "bce-astronomical"}]]
           (ac/parse-date "前2"))
        "前2 (2 BCE) → -0001")
    (is (= ["0000" [{"raw" "前1" "corrected" "0000" "rule" "bce-astronomical"}]]
           (ac/parse-date "前1"))
        "前1 (1 BCE) → 0000")))

(deftest parse-date-passthrough-on-unparseable-test
  (testing "parse-date passes unparseable shapes through verbatim"
    (let [[norm corrs] (ac/parse-date "19xx")]
      (is (= "19xx" norm)
          "unparseable shape is preserved; schema validation will reject it")
      (is (= [] corrs)))))

(deftest parse-date-decade-marker-admitted-verbatim-test
  (testing "EDTF Level 1 decade markers (\\d{3}X) are admitted verbatim with no audit correction"
    ;; Per ADR 0016, decade markers are first-class v0.1 lexical
    ;; values. No transformation occurs, so no audit entry is written;
    ;; downstream schema/SHACL accept them.
    (doseq [in ["192X" "200X" "-019X"]]
      (let [[norm corrs] (ac/parse-date in)]
        (is (= in norm) (str in " admitted verbatim"))
        (is (= [] corrs) (str in " produces no audit entry"))))))

(deftest parse-date-bce-century-prose-test
  (testing "Japanese BCE century prose translates to EDTF Level 1 century markers under astronomical year numbering (ADR 0016)"
    (is (= ["-06XX" [{"raw" "紀元前7世紀末"
                      "corrected" "-06XX"
                      "rule" "century-prose"}]]
           (ac/parse-date "紀元前7世紀末"))
        "紀元前7世紀末 → -06XX; the qualifier 末 lives in the audit raw field only")
    (is (= ["-05XX" [{"raw" "紀元前6世紀初"
                      "corrected" "-05XX"
                      "rule" "century-prose"}]]
           (ac/parse-date "紀元前6世紀初"))
        "紀元前6世紀初 → -05XX")
    (is (= ["-00XX" [{"raw" "紀元前1世紀"
                      "corrected" "-00XX"
                      "rule" "century-prose"}]]
           (ac/parse-date "紀元前1世紀"))
        "紀元前1世紀 (the 100..1 BCE century) maps to astronomical -00XX")
    (doseq [q ["初頭" "半ば" "前半" "後半"]]
      (let [raw (str "紀元前7世紀" q)
            [norm corrs] (ac/parse-date raw)]
        (is (= "-06XX" norm)
            (str "qualifier " q " does not influence the canonical lexical form"))
        (is (= [{"raw" raw "corrected" "-06XX" "rule" "century-prose"}] corrs)
            (str "qualifier " q " is preserved verbatim in audit raw field"))))))

(deftest parse-date-passthrough-out-of-grammar-test
  (testing "EDTF Level 1 shapes still out of v0.1 grammar pass through verbatim"
    ;; Per ADR 0016, uncertainty/approximation qualifiers and CE
    ;; century prose remain deferred. They must not be silently
    ;; rewritten — they pass through and fail downstream.
    (doseq [in ["1892?" "1892~" "1892%" "7世紀" "6世紀後半"]]
      (let [[norm corrs] (ac/parse-date in)]
        (is (= in norm) (str in " passes through verbatim"))
        (is (= [] corrs) (str in " produces no audit entry"))))))

(deftest parse-date-rejects-impossible-calendar-day-test
  (testing "parse-date passes through values whose regex shape fits but whose calendar day is impossible (Feb 31, etc.)"
    (let [[norm corrs] (ac/parse-date "2020-02-31")]
      (is (= "2020-02-31" norm)
          "shape-valid but calendar-impossible date is preserved verbatim, not normalized")
      (is (= [] corrs)
          "no correction is recorded — downstream validation will reject this"))))

(deftest parse-date-rejects-impossible-bce-day-test
  (testing "BCE full dates with impossible day pass through verbatim"
    ;; 前427 → -0426 (year), so a 前427-02-31 shape would convert to
    ;; -0426-02-31 — Feb 31 doesn't exist in any year. The parser
    ;; doesn't construct that shape from the BCE side (年月日 columns
    ;; are independent), but a curated -0426-02-31 string in the date
    ;; column should fall through. Verifying via the parser's
    ;; partial-date-pattern / valid-calendar-shape? path.
    (let [[norm corrs] (ac/parse-date "-0426-02-31")]
      (is (= "-0426-02-31" norm))
      (is (= [] corrs)))))

(deftest parse-date-bce-zero-rejects-test
  (testing "前0 / 前000 has no astronomical equivalent — pass raw through with no correction"
    (let [[norm corrs] (ac/parse-date "前0")]
      (is (= "前0" norm))
      (is (= [] corrs)
          "no bogus 'corrected' value (the previous bug emitted '--001')"))
    (let [[norm corrs] (ac/parse-date "前000")]
      (is (= "前000" norm))
      (is (= [] corrs)))))

(deftest parse-date-unknown-marker-test
  (testing "parse-date maps Japanese 'unknown' sentinels to null with an unknown-marker correction"
    (is (= [nil [{"raw" "不詳" "corrected" nil "rule" "unknown-marker"}]]
           (ac/parse-date "不詳")))
    (is (= [nil [{"raw" "未詳" "corrected" nil "rule" "unknown-marker"}]]
           (ac/parse-date "未詳"))
        "未詳 (not yet ascertained) is a synonym for 不詳 in Aozora date columns")))

(deftest parse-date-collapse-multi-dash-test
  (testing "parse-date collapses repeated dashes from obvious typos"
    (let [[norm corrs] (ac/parse-date "1850-08--18")]
      (is (= "1850-08-18" norm))
      (is (= [{"raw" "1850-08--18" "corrected" "1850-08-18" "rule" "collapse-multi-dash"}]
             corrs)))))

(deftest parse-date-normalizes-dot-separators-test
  (testing "parse-date normalizes unambiguous dot-separated Y.M.D dates"
    (let [[norm corrs] (ac/parse-date "1839.1.1")]
      (is (= "1839-01-01" norm))
      (is (= [{"raw" "1839.1.1" "corrected" "1839-01-01" "rule" "normalize-date-separator"}
              {"raw" "1839.1.1" "corrected" "1839-01-01" "rule" "pad-month"}
              {"raw" "1839.1.1" "corrected" "1839-01-01" "rule" "pad-day"}]
             corrs)))))

(deftest parse-date-rejects-out-of-range-month-day-test
  (testing "parse-date passes through values with out-of-range month/day rather than emitting them as 'corrected'"
    ;; partial-date-pattern accepts \d{1,2} for month/day. The
    ;; calendar validity check then rejects the corrected form, so
    ;; the parser falls through to verbatim passthrough; downstream
    ;; schema validation rejects.
    (let [[norm corrs] (ac/parse-date "2020-99-99")]
      (is (= "2020-99-99" norm))
      (is (= [] corrs)))
    (let [[norm corrs] (ac/parse-date "2020-13-01")]
      (is (= "2020-13-01" norm))
      (is (= [] corrs)))))
