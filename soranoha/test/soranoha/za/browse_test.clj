(ns soranoha.za.browse-test
  "The browse layer is presentation, so its contract is narrow but strict:
  the same release must produce the same bytes, corpus metadata must reach
  the page as text and never as markup, and every work in the release must be
  reachable without scripting."
  (:require [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.za.browse :as browse]
            [soranoha.za.docs :as docs]
            [soranoha.za.html :as html]))

(def ^:private head-hex (apply str (repeat 64 "a")))
(def ^:private prev-hex (apply str (repeat 64 "b")))
(def ^:private event-hex (apply str (repeat 64 "c")))

(defn- work
  [slug title reading & {:keys [ndc contributors]
                         :or {ndc "NDC 913"
                              contributors [{"person_id" "000879"
                                             "family_name" "芥川"
                                             "given_name" "龍之介"
                                             "family_name_romaji" "Akutagawa"
                                             "given_name_romaji" "Ryunosuke"
                                             "relation_to_work" "著者"}]}}]
  {"slug" slug
   "source_content_hash" (apply str (repeat 64 "1"))
   "title" title
   "title_reading" reading
   "subtitle" nil
   "original_title" nil
   "first_published" "「新思潮」1918(大正7)年"
   "orthographic_style" "新字新仮名"
   "ndc" ndc
   "card_url" "https://www.aozora.gr.jp/cards/000879/card92.html"
   "archive_stem" "92_ruby_164"
   "contributors" contributors
   "source_editions" [{"title" "芥川龍之介全集　第三巻"
                       "publisher" "筑摩書房"
                       "first_edition_year" "1971"}]})

(def ^:private rights
  {"works" "public-domain"
   "encoding" "CC0-1.0"
   "statement_url" "https://soranoha.org/rights"})

(defn- tei-for
  "Published TEI for a fixture work, rendered by the same path that produces
  the artifact a manifest names, so the reading pages under test are built
  from the shape the release actually carries."
  [slug]
  (.getBytes
   ^String (:tei (render/render-work
                  {:rights @fixture/grant
                   :parser-ir {"nodes" [{"type" "text" "text" (str "本文 " slug)}
                                        {"type" "ruby"
                                         "ruby" {"base" "池" "reading" "いけ"
                                                 "scope" "explicit"}}]}
                   :metadata-record {"work" {"title" "試験"} "contributors" []}
                   :persons-by-id {}}))
   "UTF-8"))

(defn- inputs
  ([works] (inputs works []))
  ([works withdrawn]
   {:head-hex head-hex
    :tei tei-for
    :manifests [[head-hex {"works" (mapv #(select-keys % ["slug"]) works)
                           "withdrawn" withdrawn
                           "rights" rights}]
                [prev-hex {"works" [{"slug" "000092_000879"}]
                           "withdrawn" []
                           "rights" rights}]]
    :catalog {"schema" "snh-catalog/1" "works" works}
    :events {event-hex {"schema" "snh-governance-event/1"
                        "kind" "withdrawal"
                        "entries" [{"slug" "000092_000879"
                                    "reason_code" "rights"
                                    "statement" "Rights holder came forward."}]}}}))

(defn- pages
  "`browse/pages` yields `[path bytes]` pairs lazily, because one reading page
  per work is more than a map should hold at once. Tests want random access,
  so they realize the sequence they asked for."
  [inputs]
  (into {} (browse/pages inputs)))

(defn- page [pages path]
  (String. ^bytes (get pages path) "UTF-8"))

(deftest every-work-is-reachable-without-scripting-test
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")
               (work "000035_001567" "走れメロス" "はしれめろす")]
        pages (pages (inputs works))]
    (testing "the entry point and each index exist as ordinary files"
      (doseq [path ["index.html" "authors/index.html" "titles/index.html"
                    "ndc/index.html" "rights.html" "citation.html"
                    "style.css" "search.js" "search-index.json"]]
        (is (contains? pages path) path)))

    (testing "so is every document, and every file the documents send a reader to"
      (doseq [{:keys [route]} docs/documents]
        (is (contains? pages (str route ".html")) route))
      (doseq [{:keys [route]} docs/verbatim]
        (is (contains? pages route) route)))

    (testing "a work is reachable from the title index by the row of its reading"
      (is (string/includes? (page pages "titles/index.html") "/titles/ka"))
      (is (string/includes? (page pages "titles/ka.html") "蜘蛛の糸"))
      (is (string/includes? (page pages "titles/ha.html") "走れメロス"))
      (is (not (string/includes? (page pages "titles/ha.html") "蜘蛛の糸"))))

    (testing "and from the author index, which links the person's own page"
      (is (string/includes? (page pages "authors/index.html") "/authors/000879"))
      (let [author (page pages "authors/000879.html")]
        (is (string/includes? author "芥川 龍之介"))
        (is (string/includes? author "著者"))
        (is (string/includes? author "/works/000092_000879/"))))

    (testing "and from the NDC index, under the main class of its code"
      (is (string/includes? (page pages "ndc/index.html") "/ndc/9"))
      (is (string/includes? (page pages "ndc/9.html") "蜘蛛の糸")))

    (testing "the work page carries the facts a citation needs"
      (let [work-page (page pages "works/000092_000879/index.html")]
        (doseq [fact ["蜘蛛の糸" "くものいと" "芥川 龍之介" "新字新仮名" "NDC 913"
                      "芥川龍之介全集　第三巻" "筑摩書房" "1971"
                      "https://www.aozora.gr.jp/cards/000879/card92.html"
                      "000092_000879" head-hex]]
          (is (string/includes? work-page fact) fact))
        (doseq [artifact ["tei" "plaintext" "markdown" "tei-validation"]]
          (is (string/includes? work-page (str "/works/000092_000879/" artifact))
              artifact))))))

(deftest a-download-is-offered-under-a-name-that-identifies-the-work
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")]
        pages (pages (inputs works))
        work-page (page pages "works/000092_000879/index.html")]
    (testing "each artifact links its readable name, which is what gets saved"
      (doseq [[artifact extension] [["tei" "xml"] ["plaintext" "txt"]
                                    ["markdown" "md"] ["tei-validation" "validation.json"]]]
        (is (string/includes?
             work-page
             (str "/works/000092_000879/Akutagawa_Ryunosuke-92_ruby_164-000092_000879."
                  extension))
            artifact)))

    (testing "and the type route stays on the page as the citable one"
      (is (string/includes? work-page "/works/000092_000879/tei"))
      (is (string/includes? work-page "識別子であって、ファイル名ではありません")))))

(deftest bulk-selections-are-linked-from-the-pages-they-mirror
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")]
        pages (pages (inputs works))]
    (testing "the whole corpus, from the front door"
      (let [landing (page pages "index.html")]
        (is (string/includes? landing "/bulk/soranoha-tei.zip"))
        (is (string/includes? landing "/bulk/soranoha-plaintext.zip"))))

    (testing "one person's works, from their own page"
      (let [author (page pages "authors/000879.html")]
        (is (string/includes?
             author "/bulk/authors/soranoha-Akutagawa_Ryunosuke-000879-tei.zip"))))

    (testing "one NDC class, from that class's page"
      (is (string/includes? (page pages "ndc/9.html") "/bulk/ndc/soranoha-ndc-9-tei.zip")))

    (testing "and every archive says what catalog.csv is for"
      (is (string/includes? (page pages "index.html") "catalog.csv")))))

(deftest a-work-can-be-read-in-the-browser-without-scripting-test
  (let [slug "000092_000879"
        pages (pages (inputs [(work slug "蜘蛛の糸" "くものいと")]))
        read-page (page pages (str "works/" slug "/read.html"))]
    (testing "the work page sends a reader to the reading view"
      (is (string/includes? (page pages (str "works/" slug "/index.html"))
                            (str "/works/" slug "/read"))))

    (testing "and the reading view renders the work's own published TEI"
      (is (string/includes? read-page (str "本文 " slug)))
      (is (string/includes? read-page "<rt>いけ</rt>")))

    (testing "the orientation toggle is a checkbox, so it works with scripting off"
      (is (string/includes? read-page "type=\"checkbox\""))
      (is (string/includes? read-page "id=\"tategaki\""))
      (is (not (string/includes? read-page "<script"))))

    (testing "and the page says which release it renders and how to cite it"
      (is (string/includes? read-page head-hex))
      (is (string/includes? read-page (str "/works/" slug "/tei")))
      (is (string/includes? read-page "/citation")))))

(deftest an-unclassified-or-k-coded-work-still-has-a-class-page-test
  (let [pages (pages (inputs [(work "000001_000001" "無分類" "むぶんるい" :ndc nil)
                              (work "000002_000002" "児童書" "じどうしょ" :ndc "NDC K913")]))]
    (is (string/includes? (page pages "ndc/other.html") "無分類"))
    (is (string/includes? (page pages "ndc/other.html") "児童書")
        "Aozora's K-prefixed children's codes are not a main class and are not invented into one")))

(deftest a-work-with-no-recorded-reading-is-still-listed-test
  (let [pages (pages (inputs [(work "000003_000003" "読みなし" nil)]))]
    (is (string/includes? (page pages "titles/other.html") "読みなし"))
    (is (string/includes? (page pages "titles/index.html") "/titles/other"))))

(deftest a-katakana-reading-files-under-the-same-row-as-hiragana-test
  (let [pages (pages (inputs [(work "000004_000004" "カタカナ" "カタカナ")]))]
    (is (string/includes? (page pages "titles/ka.html") "カタカナ"))))

(deftest corpus-metadata-reaches-the-page-as-text-test
  (testing "a title carrying markup characters is escaped, not interpreted"
    (let [hostile "<script>&\"'"
          pages (pages (inputs [(work "000005_000005" hostile "あ")]))
          work-page (page pages "works/000005_000005/index.html")]
      (is (string/includes? work-page "&lt;script&gt;&amp;"))
      (is (not (string/includes? work-page "<script>&\"")))
      (is (string/includes? (page pages "titles/a.html") "&lt;script&gt;"))))

  (testing "and reaches the search index as JSON string content"
    (let [pages (pages (inputs [(work "000005_000005" "引用\"符" "あ")]))
          index (json/read-json (page pages "search-index.json"))]
      (is (= [["000005_000005" "引用\"符" "あ" "芥川 龍之介"]]
             (get index "works")))
      (is (= head-hex (get index "release"))))))

(deftest a-withdrawn-work-explains-itself-and-names-its-last-release-test
  (let [pages (pages (inputs [(work "000035_001567" "走れメロス" "はしれめろす")]
                             [{"slug" "000092_000879"
                               "event" (str "snh:1:governance-event:" event-hex)}]))
        withdrawn (page pages "works/000092_000879/index.html")]
    (is (string/includes? withdrawn "rights"))
    (is (string/includes? withdrawn "Rights holder came forward."))
    (is (string/includes? withdrawn "/withdrawn/000092_000879.json"))
    (is (string/includes? withdrawn prev-hex)
        "the release that last contained it stays reachable")
    (is (string/includes? (page pages "index.html") "取り下げ")
        "the landing page counts withdrawals rather than hiding them")))

(deftest the-same-release-produces-the-same-bytes-test
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")
               (work "000035_001567" "走れメロス" "はしれめろす")]
        once (vec (browse/pages (inputs works)))
        twice (vec (browse/pages (inputs works)))]
    (is (= (mapv first once) (mapv first twice))
        "the same release yields the same paths in the same order")
    (is (every? (fn [[[_ a] [_ b]]] (java.util.Arrays/equals ^bytes a ^bytes b))
                (map vector once twice))
        "generation is a pure function of the release, which is what lets the
         exporter's reuse check treat these files like chain content")))

(deftest the-pages-point-at-the-signed-record-rather-than-standing-in-for-it-test
  (let [pages (pages (inputs [(work "000092_000879" "蜘蛛の糸" "くものいと")]))]
    (doseq [path ["index.html" "works/000092_000879/index.html"]]
      (is (string/includes? (page pages path) "/catalog.json") path)
      (is (string/includes? (page pages path) (str "/releases/" head-hex ".json")) path))
    (testing "a document page carries the document, its links and its neighbours"
      (let [glossary (page pages "glossary.html")]
        (is (string/includes? glossary "<h1"))
        (is (string/includes? glossary "id=\"reliance\"")
            "headings carry the identifier the documents link to")
        (is (string/includes? glossary "href=\"/identifiers\"")
            "a link to a served document becomes a link to its route")
        (is (string/includes? glossary "class=\"siblings\""))
        (is (not (string/includes? glossary ".md\""))
            "no page links a Markdown file the site does not serve"))
      (let [rights-doc (page pages "start-here.html")]
        (is (string/includes? rights-doc "href=\"/glossary\"")))
      (testing "and a repository file the site does not serve is named, not linked"
        (let [validation (page pages "validation.html")]
          (is (string/includes? validation "href=\"/schemas/tei-profile.odd\"")
              "the profile is served, so it is a link")
          (is (string/includes? (page pages "assessment.html")
                                "soranoha/test/soranoha/assessment/evaluate_test.clj")
              "a test file is not served, so its path is shown instead"))))

    (testing "and the rights page states the grant the manifest carries"
      (let [rights-page (page pages "rights.html")]
        (is (string/includes? rights-page "https://creativecommons.org/publicdomain/zero/1.0/"))
        (is (string/includes? rights-page "https://creativecommons.org/publicdomain/mark/1.0/"))
        (is (string/includes? rights-page "https://soranoha.org/rights"))))))

(deftest html-rendering-escapes-every-untrusted-position-test
  (is (= "<p class=\"a&quot;b\">&lt;x&gt;&amp;</p>"
         (html/render [:p {:class "a\"b"} "<x>&"])))
  (is (= "<br>" (html/render [:br])) "void elements have no closing tag")
  (is (= "<input disabled>" (html/render [:input {:disabled true :value nil}]))
      "true renders bare, nil renders nothing")
  (is (= "ab" (html/render (list "a" "b"))) "sequences splice"))
