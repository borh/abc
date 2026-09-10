(ns soranoha.za.bundle-test
  "Bulk archives are pre-built, so what they promise is checkable without a
  server: the readable names are inside, `catalog.csv` maps every one of them
  back to its identifier, the bytes are the release's own, and the same
  release produces the same archive on any machine at any time."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.za.bundle :as bundle]
            [soranoha.za.citation :as citation]
            [soranoha.za.naming :as naming])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.nio.charset StandardCharsets)
           (java.util Arrays TimeZone)
           (java.util.zip ZipInputStream)))

(defn- person [id family given family-romaji given-romaji & [relation]]
  {"person_id" id "family_name" family "given_name" given
   "family_name_romaji" family-romaji "given_name_romaji" given-romaji
   "relation_to_work" (or relation "著者")})

(defn- work [slug stem title ndc & contributors]
  {"slug" slug
   "archive_stem" stem
   "title" title
   "ndc" ndc
   "orthographic_style" "新字新仮名"
   "first_published" "「赤い鳥」1918（大正7）年7月"
   ;; Aozora's 初版発行年 is a publication history, not a year, and every
   ;; recorded value looks like this one
   "source_editions" [{"title" "芥川龍之介全集　第三巻"
                       "publisher" "筑摩書房"
                       "first_edition_year" "1971（昭和46）年8月10日改版"}]
   "source_content_hash" (apply str (repeat 64 "1"))
   "contributors" (vec contributors)})

(def ^:private release
  {:head-hex (apply str (repeat 64 "d")) :doi "10.5281/zenodo.1234567"})

(def ^:private akutagawa (person "000879" "芥川" "龍之介" "Akutagawa" "Ryunosuke"))
(def ^:private dazai (person "000035" "太宰" "治" "Dazai" "Osamu"))
(def ^:private collator (person "000001" "校" nil "Kousei" nil "校訂者"))

(def ^:private catalog
  {"schema" "snh-catalog/1"
   "works" [(work "000092_000879" "kumono_ito" "蜘蛛の糸" "NDC 913" akutagawa)
            (work "000035_001567" "hashire_merosu" "走れメロス" "NDC 913" dazai collator)
            (work "000001_000001" "ronbun" "論文, 「一」" "NDC 002" akutagawa)]})

(defn- artifact [slug artifact-type]
  (.getBytes (str "bytes:" artifact-type ":" slug) StandardCharsets/UTF_8))

(defn- build
  "One archive's bytes, by the path the export would write it to."
  ^bytes [path]
  (let [produce (some (fn [[p produce]] (when (= p path) produce))
                      (bundle/archives {:catalog catalog :artifact artifact
                                        :release release}))
        out (ByteArrayOutputStream.)]
    (is (some? produce) (str "no archive at " path))
    (produce out)
    (.toByteArray out)))

(defn- entries
  "Archive member name -> its bytes, plus the order they appear in."
  [^bytes archive]
  (with-open [zip (ZipInputStream. (ByteArrayInputStream. archive))]
    (loop [named [] content {}]
      (if-let [entry (.getNextEntry zip)]
        (recur (conj named (.getName entry))
               (assoc content (.getName entry) (.readAllBytes zip)))
        {:order named :content content}))))

(defn- text [{:keys [content]} name]
  (String. ^bytes (get content name) StandardCharsets/UTF_8))

(deftest an-archive-holds-the-readable-names-and-a-map-back-to-the-identifiers
  (let [archive (entries (build (naming/corpus-bundle-path "tei")))]
    (testing "catalog.csv sits at the root, beside the files it describes"
      (is (= "catalog.csv" (first (:order archive))))
      (is (every? #(not (string/includes? % "/")) (:order archive))))

    (testing "every work is inside, under the name its page offers"
      (is (= #{"catalog.csv"
               "Akutagawa_Ryunosuke-kumono_ito-000092_000879.xml"
               "Dazai_Osamu-hashire_merosu-000035_001567.xml"
               "Akutagawa_Ryunosuke-ronbun-000001_000001.xml"}
             (set (:order archive)))))

    (testing "and holds the release's own bytes, not a copy made for the archive"
      (is (= "bytes:tei:000092_000879"
             (text archive "Akutagawa_Ryunosuke-kumono_ito-000092_000879.xml"))))))

(deftest catalog-csv-maps-a-file-to-its-work-without-a-program
  (let [csv (text (entries (build (naming/corpus-bundle-path "plaintext"))) "catalog.csv")
        lines (string/split csv #"\r\n")]
    (testing "it opens as a spreadsheet: a UTF-8 mark, CRLF rows, quoted fields"
      (is (string/starts-with? csv "﻿"))
      (is (string/includes? csv "\r\n"))
      (is (= (str "﻿" (string/join "," (map #(str "\"" % "\"") citation/csv-columns)))
             (first lines))))

    (testing "one row per work, in the archive's own order"
      (is (= 4 (count lines)))
      (is (string/starts-with? (second lines) "\"000092_000879\",\"蜘蛛の糸\",")))

    (testing "the filename column names the file that is actually in the archive"
      (is (string/includes? csv "\"Dazai_Osamu-hashire_merosu-000035_001567.txt\"")))

    (testing "a title carrying a comma or a quotation mark survives the round trip"
      (is (string/includes? csv "\"論文, 「一」\"")))

    (testing "and the citable columns are there, hash prefix included"
      (is (string/includes? csv (str "\"sha256:" (apply str (repeat 64 "1")) "\"")))
      (is (string/includes? csv (str "\"" (:head-hex release) "\"")))
      (is (string/includes? csv "\"10.5281/zenodo.1234567\"")))

    (testing "a whole selection becomes a bibliography without opening a TEI file"
      (let [row (zipmap citation/csv-columns
                        (map #(subs % 1 (dec (count %)))
                             (re-seq #"\"[^\"]*\"" (second lines))))]
        (is (= {"author" "芥川 龍之介"
                "source_edition_title" "芥川龍之介全集　第三巻"
                "source_edition_publisher" "筑摩書房"
                "orthographic_style" "新字新仮名"
                "url" "https://soranoha.org/works/000092_000879/"}
               (select-keys row ["author" "source_edition_title"
                                 "source_edition_publisher" "orthographic_style" "url"])))
        (is (= "1971" (get row "source_edition_year"))
            "the Gregorian year is extracted; 底本初版発行年 is a publication history")))))

(deftest the-same-release-produces-the-same-archive
  (let [default (TimeZone/getDefault)]
    (try
      (let [utc (do (TimeZone/setDefault (TimeZone/getTimeZone "UTC"))
                    (build (naming/corpus-bundle-path "tei")))
            elsewhere (do (TimeZone/setDefault (TimeZone/getTimeZone "Pacific/Kiritimati"))
                          (build (naming/corpus-bundle-path "tei")))]
        (is (Arrays/equals utc elsewhere)
            "no clock and no timezone reaches the bytes, so the exporter's reuse check holds"))
      (finally (TimeZone/setDefault default)))))

(deftest the-selections-mirror-the-axes-the-site-browses-by
  (let [paths (set (map :path (bundle/selections catalog)))]
    (testing "the whole corpus, for both bulk-published types"
      (is (contains? paths "bulk/soranoha-tei.zip"))
      (is (contains? paths "bulk/soranoha-plaintext.zip")))

    (testing "one per person, in any role, so an author page can link its own"
      (is (contains? paths "bulk/authors/soranoha-Akutagawa_Ryunosuke-000879-tei.zip"))
      (is (contains? paths "bulk/authors/soranoha-Kousei-000001-plaintext.zip")))

    (testing "and one per NDC class, including the classes this release empties"
      (is (contains? paths "bulk/ndc/soranoha-ndc-9-tei.zip"))
      (is (contains? paths "bulk/ndc/soranoha-ndc-other-tei.zip"))))

  (testing "a person's archive holds their works and no others"
    (let [dazai-archive (entries (build "bulk/authors/soranoha-Dazai_Osamu-000035-tei.zip"))]
      (is (= #{"catalog.csv" "Dazai_Osamu-hashire_merosu-000035_001567.xml"}
             (set (:order dazai-archive))))))

  (testing "an NDC archive holds that class and no others"
    (let [general (entries (build "bulk/ndc/soranoha-ndc-0-plaintext.zip"))]
      (is (= #{"catalog.csv" "Akutagawa_Ryunosuke-ronbun-000001_000001.txt"}
             (set (:order general))))))

  (testing "a work reaches a person's archive once even when both roles are theirs"
    (let [twice (assoc-in catalog ["works" 0 "contributors"]
                          [akutagawa (assoc akutagawa "relation_to_work" "校訂者")])
          produce (some (fn [[p produce]]
                          (when (= p "bulk/authors/soranoha-Akutagawa_Ryunosuke-000879-tei.zip")
                            produce))
                        (bundle/archives {:catalog twice :artifact artifact
                                          :release release}))
          out (ByteArrayOutputStream.)]
      (produce out)
      (is (= 3 (count (:order (entries (.toByteArray out)))))
          "catalog.csv plus two works, with 蜘蛛の糸 counted once"))))

(deftest a-person-named-twice-for-one-work-holds-it-once
  (testing "an author bundle lists a work once however many rows name the person"
    ;; A catalog can name one person twice for one work, repeating a relation
    ;; or holding two of them, and neither makes it two works. The author page
    ;; and the archive it links to derive this from one definition, so they
    ;; cannot disagree about what the works of a person are.
    (let [repeated (work "000092_000879" "kumono_ito" "蜘蛛の糸" "NDC 913"
                         akutagawa akutagawa (assoc akutagawa "relation_to_work" "校訂者"))
          selections (bundle/selections {"schema" "snh-catalog/1" "works" [repeated]})
          author (first (filter #(string/includes? (:path %) "000879") selections))]
      (is (= 1 (count (:works author))))
      (is (= ["000092_000879"] (mapv #(get % "slug") (:works author)))))))
