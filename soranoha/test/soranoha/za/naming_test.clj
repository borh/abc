(ns soranoha.za.naming-test
  "Download filenames are a rendering of the signed catalog, so their contract
  is the one the catalog cannot carry: a name that identifies the work, never
  collides, survives a filesystem, and is the same on every activation."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.za.naming :as naming]))

(defn- person
  [id family given family-romaji given-romaji & [relation]]
  {"person_id" id
   "family_name" family
   "given_name" given
   "family_name_romaji" family-romaji
   "given_name_romaji" given-romaji
   "relation_to_work" (or relation "著者")})

(defn- work [stem slug & contributors]
  {"slug" slug
   "archive_stem" stem
   "title" "作品"
   "contributors" (vec contributors)})

(deftest a-filename-names-the-work-it-holds
  (testing "the decided shape: author, Aozora's own stem, then the identifier"
    (is (= "Shiraki_Shizu-sanjusanno_shi-000002_000012.xml"
           (naming/filename (work "sanjusanno_shi" "000002_000012"
                                  (person "000012" "白木" "しづ" "Shiraki" "Shizu"))
                            "tei"))))

  (testing "each artifact type gets the extension its bytes deserve"
    (let [w (work "kumono_ito" "000092_000879"
                  (person "000879" "芥川" "龍之介" "Akutagawa" "Ryunosuke"))]
      (is (= ["Akutagawa_Ryunosuke-kumono_ito-000092_000879.xml"
              "Akutagawa_Ryunosuke-kumono_ito-000092_000879.txt"
              "Akutagawa_Ryunosuke-kumono_ito-000092_000879.md"
              "Akutagawa_Ryunosuke-kumono_ito-000092_000879.validation.json"]
             (mapv #(naming/filename w %)
                   ["tei" "plaintext" "markdown" "tei-validation"])))))

  (testing "an unknown artifact type is refused rather than guessed at"
    (is (= :unknown-artifact-type
           (try (naming/filename (work "a" "000001_000001" (person "000001" "あ" nil "A" nil))
                                 "epub")
                nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(deftest a-filename-collides-with-nothing
  ;; 802 Aozora stems are shared by 1908 works and 470 author-and-title pairs
  ;; by 2357 works, so uniqueness has to come from the identifier component
  ;; and from nowhere else
  (let [author (person "000879" "芥川" "龍之介" "Akutagawa" "Ryunosuke")
        names (map #(naming/filename (work "yume" % author) "tei")
                   ["000001_000879" "000002_000879" "000003_000879"])]
    (is (= 3 (count (distinct names))))
    (is (every? #(string/includes? % "-yume-") names))))

(deftest a-name-part-with-no-ascii-in-it-still-produces-a-name
  (testing "diacritics fold to the letter they decorate"
    (is (= "Droste_Hulshoff_Andre-hito-000003_000003.xml"
           (naming/filename (work "hito" "000003_000003"
                                  (person "000003" "ドロステ" nil "Droste-Hülshoff" "André"))
                            "tei"))))

  (testing "a letter NFKD leaves alone is transliterated rather than dropped"
    (is (= "Boleslaw-x-000004_000004.xml"
           (naming/filename (work "x" "000004_000004"
                                  (person "000004" "ボレスワフ" nil "Bolesław" nil))
                            "tei"))))

  (testing "apostrophes are kept: 231 stems use Hepburn's n' disambiguation"
    (is (= "O_Henry_Ken'ichiro-ippon'ashino_heitai-000005_000005.txt"
           (naming/filename (work "ippon'ashino_heitai" "000005_000005"
                                  (person "000005" "ヘンリー" nil "O. Henry" "Ken'ichiro "))
                            "plaintext"))))

  (testing "a name with nothing Latin in it falls back to the person id"
    ;; person 000361, Толстой, is the only one in the whole Aozora catalog
    (is (= "000361-sensou-000006_000006.xml"
           (naming/filename (work "sensou" "000006_000006"
                                  (person "000361" "トルストイ" "レオ" "Толстой" "Лев"))
                            "tei")))))

(deftest a-filename-stays-inside-what-a-filesystem-accepts
  (let [long-name (apply str (repeat 300 "a"))
        ^String name (naming/filename
                      (work long-name "000007_000007"
                            (person "000007" "長" nil long-name long-name))
                      "tei-validation")]
    (is (< (count (.getBytes name "UTF-8")) 255))
    (is (string/ends-with? name "-000007_000007.validation.json")
        "the identifier component is never the part that gets truncated")))

(deftest the-author-component-is-one-person-chosen-by-rule
  (testing "the first 著者 in the catalog's order, not the first contributor"
    ;; contributors arrive sorted by person id, so a collator can come first
    (is (= "Chosha-x-000008_000008.xml"
           (naming/filename (work "x" "000008_000008"
                                  (person "000001" "校" nil "Kousei" nil "校訂者")
                                  (person "000002" "著" nil "Chosha" nil "著者"))
                            "tei"))))

  (testing "and the first contributor when a work records no 著者 at all"
    (is (= "Kousei-x-000009_000009.xml"
           (naming/filename (work "x" "000009_000009"
                                  (person "000001" "校" nil "Kousei" nil "校訂者"))
                            "tei"))))

  (testing "a missing given-name romaji leaves the family name alone"
    ;; 115 works in the catalog have one
    (is (= "Shiken-x-000010_000010.xml"
           (naming/filename (work "x" "000010_000010"
                                  (person "000010" "試験" nil "Shiken" nil))
                            "tei")))))

(deftest bulk-archive-paths-say-what-they-hold
  (is (= "bulk/soranoha-tei.zip" (naming/corpus-bundle-path "tei")))
  (is (= "bulk/soranoha-plaintext.zip" (naming/corpus-bundle-path "plaintext")))
  (is (= "bulk/ndc/soranoha-ndc-9-tei.zip" (naming/ndc-bundle-path "9" "tei")))
  (is (= "bulk/authors/soranoha-Akutagawa_Ryunosuke-000879-plaintext.zip"
         (naming/author-bundle-path (person "000879" "芥川" "龍之介" "Akutagawa" "Ryunosuke")
                                    "000879" "plaintext")))
  (testing "a person with no romanized name is still addressable by id"
    (is (= "bulk/authors/soranoha-000361-tei.zip"
           (naming/author-bundle-path (person "000361" "トルストイ" "レオ" "Толстой" "Лев")
                                      "000361" "tei")))))
