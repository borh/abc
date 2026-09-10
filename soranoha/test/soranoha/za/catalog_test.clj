(ns soranoha.za.catalog-test
  "The bibliographic facts a release's catalog carries. `archive_stem` is the
  one derived from a filename rather than from a record, so it is the one that
  can arrive outside the character set the catalog's schema admits."
  (:require [clojure.test :refer [deftest is testing]]
            [soranoha.za.catalog :as catalog]))

(def ^:private stem #'catalog/archive-stem)

(deftest an-archive-stem-is-the-primary-text-members-name-without-its-extension
  (is (= "kumono_ito" (stem "000092_000879" "kumono_ito.txt")))
  (is (= "kumono_ito" (stem "000092_000879" "files/kumono_ito.txt")))
  (is (= "ippon'ashino_heitai" (stem "x" "ippon'ashino_heitai.txt"))
      "Hepburn's n' disambiguation is a word boundary and survives"))

(deftest an-archive-stem-outside-the-catalogs-character-set-is-folded
  (testing "a space is the word boundary these names otherwise write as an underscore"
    ;; Six archives in the corpus: four with a trailing space, two interior.
    (is (= "usukuchi_shoyu" (stem "059641_001403" "usukuchi_shoyu .txt")))
    (is (= "hakubutsushi_atogaki" (stem "043596_001154" "hakubutsushi _atogaki.txt")))
    (is (= "I_am_not" (stem "052884_000183" "I am_not.txt"))))
  (testing "the typographic quotation mark is the same apostrophe"
    ;; Four archives spell n' this way and 204 spell it with an apostrophe.
    (is (= "kan'yaku_igakushi" (stem "052528_001574" "kan’yaku_igakushi.txt")))
    (is (= "aoi_tamato_gin'irono_fue" (stem "051496_001475" "aoi_tamato_gin’irono_fue.txt")))))

(deftest a-stem-that-cannot-be-folded-is-refused-by-name
  ;; The whole catalog is schema-checked at the boundary, which reports a
  ;; position in the works array. A stem this cannot rescue is named with its
  ;; slug here instead, while the work it came from is still in hand.
  (let [failure (try (stem "000001_000001" "蜘蛛の糸.txt")
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :archive-stem-outside-catalog-character-set (:reason failure)))
    (is (= "000001_000001" (:slug failure)))
    (is (= "蜘蛛の糸" (:stem failure)))))

(deftest one-person-in-two-relations-is-two-entries-ordered-by-the-pair
  (testing "the entry's identity is the pair, so both relations survive and sort"
    ;; 海潮音 is Ueda Bin's, as its 著者 and as its 翻訳者 both. Fifteen works
    ;; in the corpus record one person in two relations; keying on the person
    ;; refused every one of them.
    (let [entry (#'catalog/work-entry
                 "002259_000235" (apply str (repeat 64 "a")) "public-domain"
                 {"work" {"title" "海潮音" "orthographic_style" "旧字旧仮名"
                          "card_url" "https://www.aozora.gr.jp/cards/000235/card2259.html"
                          "source_editions" []}
                  "contributors" [{"person_id" "000235" "relation_to_work" "翻訳者"}
                                  {"person_id" "000235" "relation_to_work" "著者"}]}
                 {"000235" {"family_name" "上田" "given_name" "敏"
                            "family_name_romaji" "Ueda" "given_name_romaji" "Bin"}}
                 "kaichoon.txt" nil)]
      (is (= [["000235" "翻訳者"] ["000235" "著者"]]
             (mapv (juxt #(get % "person_id") #(get % "relation_to_work"))
                   (get entry "contributors")))
          ;; code-point order, the same comparator the semantic check applies,
          ;; which puts 翻 (U+7FFB) ahead of 著 (U+8457)
          "sorted by the pair, so two relations of one person have an order"))))

(deftest an-entry-carries-the-standing-the-work-is-published-under
  ;; A client selecting works by their terms reads this column. Without it the
  ;; only published copy of the standing is inside each work's TEI header, so
  ;; the question costs one file per work instead of one file.
  (testing "the standing selection admitted the work under reaches the catalog"
    (doseq [standing ["public-domain" "CC-BY-2.1-JP" "CC-BY-4.0"]]
      (is (= standing
             (get (#'catalog/work-entry
                   "054333_001657" (apply str (repeat 64 "a")) standing
                   {"work" {"title" "食品の変造"
                            "orthographic_style" "新字新仮名"
                            "card_url" "https://www.aozora.gr.jp/cards/001657/card54333.html"
                            "source_editions" []}
                    "contributors" []}
                   {}
                   "shokuhin.txt" nil)
                  "rights"))))))

(deftest an-archive-with-bytes-after-its-end-says-so-on-the-entry
  ;; unzip and Python's zipfile refuse 058100_001505's archive. The bundle
  ;; reader retries at an earlier end-of-central-directory record and the
  ;; members come out whole, so nothing published is wrong; what a reader
  ;; verifying against the source meets is a refusal with no explanation.
  (let [entry #(#'catalog/work-entry
                "058100_001505" (apply str (repeat 64 "a")) "public-domain"
                {"work" {"title" "試験" "orthographic_style" "新字新仮名"
                         "card_url" "https://www.aozora.gr.jp/cards/001505/card58100.html"
                         "source_editions" []}
                 "contributors" []}
                {} "shiken.txt" %)]
    (is (= 984 (get (entry 984) "trailing_bytes_after_archive")))
    (testing "and an archive with none carries no field at all, which is almost every work"
      (is (not (contains? (entry nil) "trailing_bytes_after_archive"))))))
