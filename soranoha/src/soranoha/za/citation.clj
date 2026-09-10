(ns soranoha.za.citation
  "Per-work citations, in the forms a humanities writer actually pastes into
  something: a rendered line, CSL-JSON for Zotero and Pandoc, BibLaTeX for
  people who write TeX directly, and COinS so a browser connector can save the
  record in one click.

  All four are projections. The TEI header is the authoritative bibliographic
  record; the signed catalog carries the same facts so that the serving layer
  and bulk consumers do not have to parse every TEI file to build a
  bibliography. Nothing here is a second source of metadata, and nothing here
  is signed: a citation form can be corrected in a later release, which is
  exactly why it does not live in the chain.

  Two things every form carries. The **identifier**, because a description
  does not identify a work: measured over the Aozora Bunko catalog this release
  builds from, author and title leave 2357 works ambiguous, and author, title,
  副題, 文字遣い種別, 底本名 and 初出 together still leave 16. Both counts
  move with the upstream catalog. And the **release**, because the corpus is
  versioned and a citation that does not say which release was read does not
  name the bytes that were read.

  Romanized titles are not published, here or anywhere else. The kana reading
  carries no word boundaries, so a mechanical transliteration is not correct
  Hepburn; the Japanese title and its reading are given, and the romanization
  style is the citing journal's business.

  Pure, like the rest of the browse layer: same catalog entry, release head
  and DOI in, same bytes out."
  (:require [clojure.string :as string]
            [soranoha.za.naming :as naming])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))

(def corpus-name "Soranoha Aozora Bunko TEI Corpus")

(def ^:private site-origin
  "The origin a citation's landing-page URL points at.

  The permanent identifier `w3id.org/soranoha/works/<id>` is claimed for
  exactly this in the registration prepared under `docs/w3id/`, and would
  survive a change of serving location, but it
  resolves only by redirecting here, and the citation already carries two
  identifiers that outlive the site: the work identifier and the release DOI.
  A second indirection on the convenience field would buy nothing and add a
  registration this URL would depend on."
  "https://soranoha.org")

(defn- blank->nil [s] (when-not (string/blank? s) s))

(defn- short-release
  "The release's short name: the first twelve characters of its head hash,
  which `/releases/short/<prefix>.json` resolves. The forms a reader sees
  carry this; the machine-readable fields carry all sixty-four characters."
  [head-hex]
  (subs head-hex 0 12))

(def ^:private person-name-ja naming/person-name-ja)
(def ^:private person-name-romaji naming/person-name-romaji)

(defn- contributors-by-role
  "Aozora Bunko's role strings grouped as the citation formats need them. A person
  can hold more than one role on a work, and each role is cited separately."
  [work]
  (group-by #(get % "relation_to_work") (get work "contributors")))

(defn- primary-edition
  "The first recorded 底本. A work can list two; the first is the one the
  transcription was made from, and a citation names one book."
  [work]
  (first (get work "source_editions")))

(defn- edition-year
  "The Gregorian year of the 底本's first edition, as a number.

  Aozora Bunko's 底本初版発行年 is not a year. It is a free-form publication
  history: 17747 of the 18780 recorded values read `1981（昭和56）年3月20日`,
  and of the 1033 that do not, 667 append a printing history, as in
  `1948（昭和23）年5月15日、1963（昭和38）年5月16日第20刷改版`. Every one of
  the 18886 values across both edition slots contains a four-digit Gregorian
  year, and in all but one it leads the string.

  Taking the first year is the semantically correct reading, not just the
  convenient one: the field is 初版発行年, so where a printing history lists
  several years the earliest is the first edition's, which is the year a
  bibliography wants. Month and day are dropped: a book is cited by year,
  and the 1033 irregular values have no dependable month to take.

  The full string is not discarded; it is what the work page and the TEI
  header show. This is only for the fields that must hold a date."
  [work]
  (when-let [recorded (blank->nil (get (primary-edition work) "first_edition_year"))]
    (when-let [year (some-> (re-find #"\d{4}" recorded) parse-long)]
      ;; Recorded years run 1820-2025. A number outside that is not a year
      ;; this field can plausibly hold, so nothing is claimed about it.
      (when (<= 1600 year 2100) year))))

(defn- work-url [work]
  (str site-origin "/works/" (get work "slug") "/"))

(defn- full-title
  "Title and 副題 as one string, joined the way Japanese typography joins
  them. CSL and COinS have no subtitle field; BibLaTeX does and uses it."
  [work]
  (let [title (get work "title")
        subtitle (blank->nil (get work "subtitle"))]
    (if subtitle (str title "──" subtitle) title)))

(defn rendered
  "The citation to copy, in the decided order: author, title with 副題,
  orthography, 底本 with publisher and year, corpus, identifier, release,
  DOI. Japanese punctuation throughout, because the bibliographic content is
  Japanese and a reader pastes this into a Japanese bibliography."
  [{:keys [head-hex doi]} work]
  (let [{:strs [slug orthographic_style]} work
        edition (primary-edition work)
        edition-part (when edition
                       (str "底本『" (get edition "title") "』"
                            (blank->nil (get edition "publisher"))
                            (when-let [year (edition-year work)] (str "、" year "年"))))]
    (str (naming/byline work)
         "「" (full-title work) "」"
         (when-not (string/blank? orthographic_style)
           (str "（" orthographic_style "）"))
         (when edition-part (str "、" edition-part))
         "。" corpus-name ", " slug ", release " (short-release head-hex)
         (when doi (str ". https://doi.org/" doi)))))

(defn rendered-en
  "The same citation for a bibliography written in English: the author in
  Latin script from the romanized name parts the catalog publishes, Western
  ordering and punctuation, and every title left in Japanese.

  The titles stay Japanese because the alternative would be invented. The
  kana reading carries no word boundaries, so a mechanical transliteration is
  not correct Hepburn, and the 文字遣い種別 values are Aozora Bunko's own
  classification rather than terms with settled English equivalents. A writer
  who needs romanized titles has the reading and their journal's style; a
  writer who does not gets a form that is true.

  When a contributor has no romanized name at all (one person in the whole
  Aozora Bunko catalog), this falls back to the Japanese name rather than dropping
  the author."
  [{:keys [head-hex doi]} work]
  (let [{:strs [slug orthographic_style]} work
        edition (primary-edition work)
        authors (or (seq (naming/authors-of work)) (get work "contributors"))
        by (->> authors
                (map #(or (person-name-romaji %) (person-name-ja %)))
                (remove string/blank?)
                (string/join ", "))]
    (str (when-not (string/blank? by) (str by ". "))
         "“" (full-title work) "”"
         (when-not (string/blank? orthographic_style)
           (str " (" orthographic_style ")"))
         (when edition
           (str ". In " (get edition "title")
                (when-let [publisher (blank->nil (get edition "publisher"))]
                  (str ". " publisher))
                (when-let [year (edition-year work)] (str ", " year))))
         ". " corpus-name ", " slug ", release " (short-release head-hex)
         (when doi (str ". https://doi.org/" doi)))))

(def ^:private csl-roles
  "Aozora Bunko's roles as CSL contributor variables. 校訂者 has no CSL counterpart,
  since CSL models editors and translators but not collators, so it is
  recorded as a plain contributor rather than promoted to editor, which would
  say something the source does not."
  {"著者" "author" "翻訳者" "translator" "編者" "editor" "校訂者" "contributor"})

(defn- csl-name [person]
  (let [family (blank->nil (get person "family_name"))
        given (blank->nil (get person "given_name"))]
    (cond-> {}
      family (assoc "family" family)
      given (assoc "given" given)
      (and (nil? family) (nil? given))
      (assoc "literal" (naming/person-label person)))))

(defn csl-json-value
  "One CSL-JSON record, as data. `chapter` because these are works inside a
  transcribed 底本, which is also why BibLaTeX gets @incollection.

  The identifier goes in `archive` and `archive_location` rather than into a
  note: those are the CSL variables for `which collection, and where in it`,
  and Zotero surfaces them as fields a reader can act on."
  [{:keys [head-hex doi]} work]
  (let [{:strs [slug title_reading original_title first_published
                orthographic_style ndc]} work
        edition (primary-edition work)
        by-role (contributors-by-role work)]
    (cond-> {"id" (str "soranoha-" slug)
             "type" "chapter"
             "title" (full-title work)
             "language" "ja"
             "archive" corpus-name
             "archive_location" slug
             "URL" (work-url work)
             "note" (string/join
                     " "
                     (remove nil?
                             [(when-let [reading (blank->nil title_reading)]
                                (str "作品名読み: " reading "."))
                              (when-let [style (blank->nil orthographic_style)]
                                (str "文字遣い種別: " style "."))
                              (when-let [first-pub (blank->nil first_published)]
                                (str "初出: " first-pub "."))
                              (str "Release " head-hex ".")]))}
      edition (assoc "container-title" (get edition "title"))
      (blank->nil (get edition "publisher"))
      (assoc "publisher" (get edition "publisher"))
      ;; CSL date-parts holds numbers; a string here makes Zotero and Pandoc
      ;; treat the year as an uninterpretable literal.
      (edition-year work) (assoc "issued" {"date-parts" [[(edition-year work)]]})
      (blank->nil original_title) (assoc "original-title" original_title)
      (blank->nil ndc) (assoc "call-number" ndc)
      doi (assoc "DOI" doi)
      true (merge (into {}
                        (keep (fn [[role people]]
                                (when-let [variable (get csl-roles role)]
                                  [variable (mapv csl-name people)])))
                        by-role)))))

(def ^:private latex-escapes
  {\\ "\\textbackslash{}" \{ "\\{" \} "\\}" \& "\\&" \% "\\%"
   \$ "\\$" \# "\\#" \_ "\\_" \~ "\\textasciitilde{}"
   \^ "\\textasciicircum{}"})

(defn- tex [value]
  (string/escape (str value) latex-escapes))

(defn- bib-name [person]
  (let [family (blank->nil (get person "family_name"))
        given (blank->nil (get person "given_name"))]
    (if (and family given)
      (str (tex family) ", " (tex given))
      (tex (naming/person-label person)))))

(defn- bib-names [people]
  (string/join " and " (map bib-name people)))

(defn biblatex
  "One @incollection entry. Works are items inside a 底本, which is what
  @incollection means; @book would say the transcription is the whole volume.

  The identifier uses BibLaTeX's own archive idiom rather than a note:
  `eprinttype` names the collection and `eprint` the identifier inside it,
  which the standard drivers print as `Soranoha Aozora Bunko TEI Corpus:
  000092_000879`.

  The release and the original title are each written twice, because the
  entry has two readers that want different things from it. `version` and
  `origtitle` are what biber and Zotero import, and no standard style prints
  either of them for an @incollection. `addendum` and `note` are printed by
  every standard driver, so the release the transcription came from and the
  title it was translated from reach the citation a reader sees, which is
  what makes that citation name the bytes that were read."
  ^String [{:keys [head-hex doi]} work]
  (let [{:strs [slug title subtitle title_reading original_title first_published
                orthographic_style]} work
        edition (primary-edition work)
        by-role (contributors-by-role work)
        field (fn [k v] (when-not (string/blank? (str v)) (str "  " k " = {" v "},\n")))
        names (fn [k role] (when-let [people (seq (get by-role role))]
                             (field k (bib-names people))))]
    (str "@incollection{soranoha-" slug ",\n"
         (names "author" "著者")
         (names "translator" "翻訳者")
         (names "editor" "編者")
         (when-let [collators (seq (get by-role "校訂者"))]
           ;; BibLaTeX's editorial roles are a closed set with no collator in
           ;; it, and a value outside the set prints as itself: `collator
           ;; Suzuki Gyozo` where a value inside it prints `Rev. by Suzuki
           ;; Gyozo`. 校訂 is revision of a text against its sources, so
           ;; `reviser` is the entry in that set that says the nearest true
           ;; thing. The Japanese role itself is on the work page and in the
           ;; TEI header.
           (str (field "editora" (bib-names collators))
                (field "editoratype" "reviser")))
         (field "title" (tex title))
         (field "subtitle" (tex subtitle))
         (field "titleaddon" (tex title_reading))
         (field "origtitle" (tex original_title))
         (field "booktitle" (tex (get edition "title")))
         (field "publisher" (tex (get edition "publisher")))
         (field "date" (tex (edition-year work)))
         (field "language" "japanese")
         (field "langid" "japanese")
         (field "eprinttype" (tex corpus-name))
         (field "eprint" slug)
         (field "version" head-hex)
         (field "addendum" (str "Release " (short-release head-hex)))
         (field "url" (work-url work))
         (when doi (field "doi" doi))
         (field "note" (tex (string/join "; " (remove string/blank?
                                                      [orthographic_style
                                                       (when original_title
                                                         (str "原題: " original_title))
                                                       (when first_published
                                                         (str "初出: " first_published))]))))
         "}\n")))

(defn- urlencode [s]
  (-> (URLEncoder/encode (str s) StandardCharsets/UTF_8)
      ;; OpenURL readers expect percent-encoding, not the form-encoding plus
      (string/replace "+" "%20")))

(defn coins
  "The OpenURL context object a Zotero-style connector reads out of the page.

  `genre=bookitem` against the book metadata format, so a work is saved as a
  section of the 底本 rather than as a standalone book. That subtype is the
  reason this is COinS and not Highwire `citation_*` meta tags: Highwire
  cannot express it, so a connector reading those would record every work in
  the corpus as its own book and lose the source edition."
  ^String [{:keys [head-hex doi]} work]
  (let [edition (primary-edition work)
        authors (get (contributors-by-role work) "著者")
        pair (fn [k v] (when-not (string/blank? (str v))
                         (str "&" k "=" (urlencode v))))]
    (str "ctx_ver=Z39.88-2004"
         "&rft_val_fmt=" (urlencode "info:ofi/fmt:kev:mtx:book")
         "&rft.genre=bookitem"
         (pair "rft.atitle" (full-title work))
         (pair "rft.btitle" (get edition "title"))
         (pair "rft.pub" (get edition "publisher"))
         (pair "rft.date" (edition-year work))
         (apply str (map #(pair "rft.au" (naming/person-label %)) authors))
         (pair "rft.language" "jpn")
         (pair "rft_id" (work-url work))
         (when doi (pair "rft_id" (str "info:doi/" doi)))
         (pair "rft.description"
               (str corpus-name ", " (get work "slug") ", release " head-hex)))))

(def csv-columns
  "The bulk archives' `catalog.csv` header. Enough to build a bibliography
  for a whole selection in a spreadsheet without opening a single TEI file,
  and enough to get back to the record: `identifier` and `release` together
  name the bytes, and `url` resolves to the page that serves them."
  ["identifier" "title" "subtitle" "title_reading" "author"
   "orthographic_style" "ndc" "first_published"
   "source_edition_title" "source_edition_publisher" "source_edition_year"
   "filename" "source_content_hash" "url" "release" "doi"])

(defn csv-values
  "One work as `csv-columns`, in that order. `filename` is the archive member
  this row describes, which the caller knows and the catalog does not."
  [{:keys [head-hex doi]} work filename]
  (let [{:strs [slug title subtitle title_reading orthographic_style ndc
                first_published source_content_hash]} work
        edition (primary-edition work)]
    [slug title subtitle title_reading (naming/byline work)
     orthographic_style ndc first_published
     (get edition "title") (get edition "publisher") (edition-year work)
     filename (str "sha256:" source_content_hash) (work-url work)
     head-hex doi]))
