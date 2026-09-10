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
