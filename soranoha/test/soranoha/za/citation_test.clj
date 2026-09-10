(ns soranoha.za.citation-test
  "A citation is the one thing a reader takes away from this site and pastes
  somewhere it cannot be checked against the source. So the tests are about
  what each form must survive: an importer that parses it, a bibliography
  built from it, and a later reader trying to get back to the exact bytes."
  (:require [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.za.citation :as citation]))

(def ^:private release
  {:head-hex "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
   :doi "10.5281/zenodo.1234567"})

(def ^:private kumo
  {"slug" "000092_000879"
   "title" "蜘蛛の糸"
   "title_reading" "くものいと"
   "subtitle" nil
   "original_title" nil
   "first_published" "「赤い鳥」1918（大正7）年7月"
   "orthographic_style" "新字新仮名"
   "ndc" "NDC 913"
   "source_content_hash" (apply str (repeat 64 "1"))
   "source_editions" [{"title" "芥川龍之介全集　第三巻"
                       "publisher" "筑摩書房"
                       "first_edition_year" "1971（昭和46）年8月10日改版"}
                      {"title" "無視されるべき第二底本"
                       "publisher" "×"
                       "first_edition_year" "1999（平成11）年1月1日"}]
   "contributors" [{"person_id" "000879"
                    "family_name" "芥川" "given_name" "龍之介"
                    "family_name_romaji" "Akutagawa" "given_name_romaji" "Ryunosuke"
                    "relation_to_work" "著者"}]})

(deftest the-rendered-line-carries-everything-a-reader-needs-to-come-back
  (let [line (citation/rendered release kumo)]
    (testing "the bibliographic content, in Japanese, in the decided order"
      (is (string/starts-with? line "芥川 龍之介「蜘蛛の糸」（新字新仮名）、底本『芥川龍之介全集　第三巻』筑摩書房、1971年。")))

    (testing "and then the two things that make it identify a work"
      ;; author and title leave 2357 works ambiguous, and the corpus is
      ;; versioned, so neither the identifier nor the release is optional
      (is (string/includes? line "000092_000879"))
      ;; the twelve-character prefix is the release's own short name, which
      ;; /releases/short/<prefix>.json resolves; an ellipsis here would paste
      ;; into a bibliography as an identifier that resolves to nothing
      (is (string/includes? line "release dddddddddddd"))
      (is (not (string/includes? line "…")))
      (is (string/includes? line "https://doi.org/10.5281/zenodo.1234567")))

    (testing "no romanized title anywhere: the reading has no word boundaries"
      (is (not (re-find #"[Kk]umo" line))))))

(deftest a-release-with-no-doi-yet-still-renders-every-form
  ;; the first release is exported before it has been deposited anywhere
  (let [pre {:head-hex (:head-hex release)}]
    (is (string/ends-with? (citation/rendered pre kumo) "release dddddddddddd"))
    (is (not (contains? (citation/csl-json-value pre kumo) "DOI")))
    (is (not (string/includes? (citation/biblatex pre kumo) "doi")))
    (is (not (string/includes? (citation/coins pre kumo) "info:doi")))
    (testing "the release is still named, because that part is never optional"
      (is (string/includes? (citation/biblatex pre kumo) "version = {d"))
      (is (string/includes? (citation/biblatex pre kumo)
                            "addendum = {Release dddddddddddd},")))))

(deftest csl-json-imports-as-a-work-inside-its-source-edition
  (let [record (citation/csl-json-value release kumo)]
    (testing "a chapter of the 底本, not a book of its own"
      (is (= "chapter" (get record "type")))
      (is (= "芥川龍之介全集　第三巻" (get record "container-title")))
      (is (= "筑摩書房" (get record "publisher"))))

    (testing "issued holds a number; a string there is an uninterpretable literal"
      (is (= {"date-parts" [[1971]]} (get record "issued")))
      (is (integer? (ffirst (get (get record "issued") "date-parts")))))

    (testing "the author is a structured name, not a literal"
      (is (= [{"family" "芥川" "given" "龍之介"}] (get record "author"))))

    (testing "the identifier is a field a reader can act on, not buried in a note"
      (is (= "000092_000879" (get record "archive_location")))
      (is (= citation/corpus-name (get record "archive"))))

    (testing "and it survives being written and read back as JSON"
      (let [round (json/read-json (json/write-json-str record))]
        (is (= "10.5281/zenodo.1234567" (get round "DOI")))
        (is (= 1971 (ffirst (get (get round "issued") "date-parts"))))))))

(deftest biblatex-names-the-collection-in-fields-a-style-will-print
  (let [entry (citation/biblatex release kumo)]
    (is (string/starts-with? entry "@incollection{soranoha-000092_000879,"))
    (is (string/includes? entry "author = {芥川, 龍之介},"))
    (is (string/includes? entry "booktitle = {芥川龍之介全集　第三巻},"))
    (is (string/includes? entry "date = {1971},"))
    (testing "the archive idiom rather than a note, so ordinary styles print it"
      (is (string/includes? entry (str "eprinttype = {" citation/corpus-name "},")))
      (is (string/includes? entry "eprint = {000092_000879},")))
    (testing "every brace is balanced, or the .bib file will not parse at all"
      (is (= (count (filter #{\{} entry)) (count (filter #{\}} entry)))))
    (is (string/ends-with? entry "}\n"))))

(def ^:private printed-by-the-incollection-driver
  "The fields BibLaTeX's standard @incollection driver puts on the page.

  Read from `bbx/standard.bbx` in biblatex 3.21 and confirmed by compiling an
  entry with biber under authoryear, numeric, authortitle and verbose.
  `version` and `origtitle` are not in it: `version` is printed by the
  dataset, manual, misc, online and report drivers, and `origtitle` only
  through the `related` mechanism, which needs a second entry to point at."
  #{"author" "translator" "editor" "editora" "editoratype" "title" "subtitle"
    "titleaddon" "language" "booktitle" "maintitle" "edition" "volume"
    "volumes" "series" "number" "note" "publisher" "location" "date"
    "chapter" "pages" "isbn" "doi" "eprint" "eprinttype" "url" "addendum"
    "pubstate"})

(defn- entry-fields [entry]
  (into {} (map (fn [[_ k v]] [k v])) (re-seq #"(?m)^  ([a-z]+) = \{(.*)\},$" entry)))

(deftest the-release-reaches-the-bibliography-and-not-only-the-file
  ;; A field the .bib carries and no style prints is not a citation that
  ;; names the release. The whole hash stays for biber and Zotero, which
  ;; import it, and the short name goes where the driver will print it.
  (let [fields (entry-fields (citation/biblatex release kumo))
        printed (vals (select-keys fields printed-by-the-incollection-driver))]
    (is (= (:head-hex release) (get fields "version")))
    (is (some #(string/includes? % "dddddddddddd") printed)
        "no field a bibliography style prints carries the release")))

(deftest an-original-title-reaches-the-bibliography-too
  (let [translated (assoc kumo "original_title" "The Spider's Thread")
        fields (entry-fields (citation/biblatex release translated))
        printed (vals (select-keys fields printed-by-the-incollection-driver))]
    (is (= "The Spider's Thread" (get fields "origtitle")))
    (is (some #(string/includes? % "原題: The Spider's Thread") printed)
        "no field a bibliography style prints carries the original title")
    (testing "and a work that was not translated says nothing about one"
      (is (not (string/includes? (citation/biblatex release kumo) "原題"))))))

(deftest biblatex-escapes-what-tex-would-otherwise-interpret
  (let [hostile (assoc kumo
                       "title" "100% #1 {の} $x$ & _y_ \\z~"
                       "slug" "000001_000001")
        entry (citation/biblatex release hostile)]
    (is (string/includes? entry "100\\% \\#1 \\{の\\} \\$x\\$ \\& \\_y\\_ \\textbackslash{}z\\textasciitilde{}"))
    (testing "and the escaping keeps the braces balanced"
      (is (= (count (filter #{\{} entry)) (count (filter #{\}} entry)))))))

(deftest coins-saves-a-typed-record-rather-than-a-web-page
  (let [ctx (citation/coins release kumo)
        pairs (into {} (map #(let [[k v] (string/split % #"=" 2)] [k v]))
                    (string/split ctx #"&"))]
    (is (= "Z39.88-2004" (get pairs "ctx_ver")))
    (testing "a section of a book, which is the subtype Highwire tags cannot express"
      (is (= "info%3Aofi%2Ffmt%3Akev%3Amtx%3Abook" (get pairs "rft_val_fmt")))
      (is (= "bookitem" (get pairs "rft.genre"))))
    (testing "the work is the article-level title and the 底本 the book-level one"
      (is (string/includes? ctx "rft.atitle=%E8%9C%98%E8%9B%9B%E3%81%AE%E7%B3%B8"))
      (is (string/includes? ctx "rft.btitle=")))
    (testing "spaces are percent-encoded; a form-encoded plus is not an OpenURL"
      (is (not (string/includes? ctx "+"))))
    (is (= "1971" (get pairs "rft.date")))))

(deftest the-first-source-edition-is-the-one-cited
  ;; a work can record two 底本; the transcription was made from the first,
  ;; and a citation names one book
  (is (not (string/includes? (citation/rendered release kumo) "無視される")))
  (is (= "芥川龍之介全集　第三巻"
         (get (citation/csl-json-value release kumo) "container-title"))))

(deftest an-edition-year-is-taken-from-a-publication-history-or-omitted
  (let [year (fn [recorded]
               (-> (citation/csl-json-value
                    release
                    (assoc kumo "source_editions"
                           [{"title" "本" "first_edition_year" recorded}]))
                   (get "issued")))]
    (testing "the shapes Aozora actually records"
      (is (= {"date-parts" [[1981]]} (year "1981（昭和56）年3月20日")))
      (is (= {"date-parts" [[1971]]} (year "1971(昭和46)年11月30日改版")))
      (is (= {"date-parts" [[2015]]} (year "2015（平成27）年4月16日第1刷")))
      (is (= {"date-parts" [[1936]]} (year "昭和11（1936）年1月1日"))))

    (testing "a printing history gives the first edition, which is the field's name"
      (is (= {"date-parts" [[1948]]}
             (year "1948（昭和23）年5月15日、1963（昭和38）年5月16日第20刷改版"))))

    (testing "nothing is claimed when the recorded value holds no year"
      (is (nil? (year "")))
      (is (nil? (year "　")))
      (is (nil? (year "発行年不明"))))

    (testing "and a work with no 底本 at all still cites"
      (is (string/includes?
           (citation/rendered release (assoc kumo "source_editions" []))
           "000092_000879")))))

(deftest a-work-with-several-roles-cites-each-of-them-as-itself
  (let [many (assoc kumo "contributors"
                    [{"person_id" "000879" "family_name" "芥川" "given_name" "龍之介"
                      "family_name_romaji" "Akutagawa" "given_name_romaji" "Ryunosuke"
                      "relation_to_work" "著者"}
                     {"person_id" "000001" "family_name" "森" "given_name" "鴎外"
                      "relation_to_work" "翻訳者"}
                     {"person_id" "000002" "family_name" "校" "given_name" "訂"
                      "relation_to_work" "校訂者"}])
        record (citation/csl-json-value release many)
        entry (citation/biblatex release many)]
    (is (= 1 (count (get record "author"))))
    (is (= [{"family" "森" "given" "鴎外"}] (get record "translator")))
    (testing "校訂 has no CSL counterpart, so it is not promoted to editor"
      (is (nil? (get record "editor")))
      (is (= [{"family" "校" "given" "訂"}] (get record "contributor"))))
    (testing "BibLaTeX says it with a word from its own closed set of roles"
      (is (string/includes? entry "editora = {校, 訂},"))
      (is (string/includes? entry "editoratype = {reviser},")))))

(deftest the-subtitle-joins-the-title-the-way-japanese-typography-joins-it
  (let [with-sub (assoc kumo "subtitle" "序")]
    (is (string/includes? (citation/rendered release with-sub) "「蜘蛛の糸──序」"))
    (is (= "蜘蛛の糸──序" (get (citation/csl-json-value release with-sub) "title")))
    (testing "BibLaTeX has a subtitle field, so it keeps them apart"
      (let [entry (citation/biblatex release with-sub)]
        (is (string/includes? entry "title = {蜘蛛の糸},"))
        (is (string/includes? entry "subtitle = {序},"))))))

(deftest a-citation-note-runs-several-first-publications-into-one-line
  ;; A citation form has nowhere to put a line break, so the statements are
  ;; joined. What must not happen is that they are run together with no
  ;; separator at all, which would read as one publication.
  (let [collection (assoc kumo "first_published" "甲「新潮」1918年\n乙「改造」1922年")]
    (is (string/includes? (get (citation/csl-json-value release collection) "note")
                          "初出: 甲「新潮」1918年; 乙「改造」1922年."))
    (is (string/includes? (citation/biblatex release collection)
                          "初出: 甲「新潮」1918年; 乙「改造」1922年"))
    (testing "and the spreadsheet column holds one line, not 414"
      (let [row (citation/csv-values release collection "kumo.md")
            cell (nth row (.indexOf ^java.util.List citation/csv-columns "first_published"))]
        (is (= "甲「新潮」1918年; 乙「改造」1922年" cell))
        (is (not (string/includes? cell "\n")))))))
