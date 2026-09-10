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
            [soranoha.snh.verify :as verify]
            [soranoha.za.browse :as browse]
            [soranoha.za.docs :as docs]
            [soranoha.za.naming :as naming]
            [soranoha.za.html :as html]))

(def ^:private head-hex (apply str (repeat 64 "a")))
(def ^:private prev-hex (apply str (repeat 64 "b")))
(def ^:private event-hex (apply str (repeat 64 "c")))
(def ^:private head-rev (apply str (repeat 40 "1")))
(def ^:private prev-rev (apply str (repeat 40 "0")))

(defn- corpus
  "The corpus projection a manifest carries. `covers_from` is the
  predecessor's `upstream_rev`, or nil at genesis, which is the producer
  side of the invariant the verifier checks."
  [rev covers-from]
  {"upstream_origin" "https://github.com/aozorabunko/aozorabunko"
   "upstream_rev" rev
   "covers_from" covers-from})

(defn- work
  [slug title reading & {:keys [ndc contributors work-rights trailing-bytes]
                         :or {ndc "NDC 913"
                              work-rights "public-domain"
                              contributors [{"person_id" "000879"
                                             "family_name" "芥川"
                                             "given_name" "龍之介"
                                             "family_name_romaji" "Akutagawa"
                                             "given_name_romaji" "Ryunosuke"
                                             "relation_to_work" "著者"}]}}]
  (cond-> {"slug" slug
           "source_content_hash" (apply str (repeat 64 "1"))
           "rights" work-rights
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
                               "first_edition_year" "1971"}]}
    trailing-bytes (assoc "trailing_bytes_after_archive" trailing-bytes)))

(def ^:private rights
  {"encoding" "CC0-1.0"
   "statement_url" "https://w3id.org/soranoha/rights"})

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
    ;; a function, and called afresh each time, exactly as the exporter
    ;; supplies it: `pages` must not depend on getting one sequence back
    :manifest-seq
    (fn []
      [[head-hex {"works" (mapv #(select-keys % ["slug" "source_content_hash" "rights"])
                                works)
                  "withdrawn" withdrawn
                  "governance_event" (when (seq withdrawn)
                                       (str "snh:1:governance-event:" event-hex))
                  "corpus" (corpus head-rev prev-rev)
                  "rights" rights}]
       ;; the predecessor holds one of the same works under different
       ;; source bytes, so a history row has an addition and a change
       ;; to count rather than only additions
       [prev-hex {"works" [{"slug" "000092_000879"
                            "source_content_hash" (apply str (repeat 64 "2"))
                            "rights" "public-domain"}]
                  "withdrawn" []
                  "governance_event" nil
                  "corpus" (corpus prev-rev nil)
                  "rights" rights}]])
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
                    "ndc/index.html" "rights.html" "citation.html" "history.html"
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
        (doseq [artifact naming/artifact-kinds]
          (is (string/includes? work-page (str "/works/000092_000879/" artifact))
              artifact))))))

(deftest a-download-is-offered-under-a-name-that-identifies-the-work
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")]
        pages (pages (inputs works))
        work-page (page pages "works/000092_000879/index.html")]
    (testing "each artifact links its readable name, which is what gets saved"
      ;; against the set the release carries rather than a list of its own, so
      ;; that a page offering three of four downloads, or a fourth nothing
      ;; answers, fails here
      (is (= (set (map naming/extensions naming/artifact-kinds))
             (into #{}
                   (keep #(second (re-matches
                                   #"Akutagawa_Ryunosuke-92_ruby_164-000092_000879\.(.+)"
                                   (second %))))
                   (re-seq #"download=\"([^\"]+)\"" work-page)))
          "the download list offers exactly the artifact types a work carries")
      (doseq [artifact naming/artifact-kinds]
        (is (string/includes?
             work-page
             (str "/works/000092_000879/Akutagawa_Ryunosuke-92_ruby_164-000092_000879."
                  (get naming/extensions artifact)))
            artifact)))

    (testing "and the type route stays on the page as the citable one"
      (is (string/includes? work-page "/works/000092_000879/tei"))
      (is (string/includes? work-page "引用には識別子を使ってください")))))

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
      ;; the page may load progressive enhancement, but nothing scripted may
      ;; touch the toggle: with scripting off the checkbox and the stylesheet
      ;; still switch the setting between them
      (is (not (re-find #"(?s)<script[^>]*>.*tategaki" read-page)))
      (is (not (string/includes? read-page "<script>"))
          "every script is an external file the reader can decline"))

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

(deftest withdrawn-works-find-their-own-last-release-in-one-pass
  ;; Two works withdrawn at different points, so a lookup that stopped at the
  ;; first release still listing anything would name the wrong one for the
  ;; older withdrawal. Reading the chain once for all of them rather than once
  ;; per withdrawal is what keeps an activation from re-walking it, so the
  ;; per-slug answer has to survive that sharing.
  (let [kept (work "000035_001567" "走れメロス" "はしれめろす")
        older "000092_000879"
        newer "000005_000005"
        event-of (fn [slug] (str "snh:1:governance-event:"
                                 (apply str (repeat 64 (if (= slug older) "a" "b")))))
        release (fn [hex slugs]
                  [hex {"works" (mapv (fn [slug]
                                        {"slug" slug
                                         "source_content_hash" (apply str (repeat 64 "1"))
                                         "rights" "public-domain"})
                                      slugs)
                        "withdrawn" []
                        "governance_event" nil
                        "corpus" (corpus head-rev prev-rev)
                        "rights" rights}])
        chain [(assoc-in (release head-hex [(get kept "slug")]) [1 "withdrawn"]
                         [{"slug" older "event" (event-of older)}
                          {"slug" newer "event" (event-of newer)}])
               (release prev-hex [(get kept "slug") newer])
               (release (apply str (repeat 64 "3")) [(get kept "slug") newer older])]
        rendered (pages {:head-hex head-hex
                         :tei tei-for
                         :manifest-seq (fn [] chain)
                         :catalog {"schema" "snh-catalog/1" "works" [kept]}
                         :events (into {}
                                       (map (fn [slug]
                                              [(verify/id->hex (event-of slug))
                                               {"schema" "snh-governance-event/1"
                                                "kind" "withdrawal"
                                                "entries" [{"slug" slug
                                                            "reason_code" "rights"
                                                            "statement" "Withdrawn."}]}]))
                                       [older newer])})]
    (is (string/includes? (page rendered (str "works/" newer "/index.html")) prev-hex)
        "the newer withdrawal was last published one release back")
    (is (string/includes? (page rendered (str "works/" older "/index.html"))
                          (apply str (repeat 64 "3")))
        "the older one two releases back, not wherever the walk first stopped")))

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
    (testing "the one served document carries its text and resolves its links"
      (let [vocabulary (page pages "ns/tei.html")]
        (is (string/includes? vocabulary "<h1"))
        (is (string/includes? vocabulary "href=\"/rights\"")
            "a link to a served route becomes a link, even to a generated page")
        (is (string/includes? vocabulary "href=\"/schemas/tei-profile.odd\"")
            "the profile is served, so it is a link")
        (is (not (string/includes? vocabulary ".md\""))
            "no page links a Markdown file the site does not serve")
        (is (string/includes? vocabulary "soranoha/docs/tei-validation.md")
            "a held-back document is named as a repository path, not linked")))

    (testing "the held-back documents have no page at all"
      (doseq [rel ["glossary.html" "start-here.html" "identifiers.html"
                   "validation.html" "assessment.html" "protocol.html"]]
        (is (nil? (get pages rel)) rel)))

    ;; Every manifest carries rights.statement_url and every published TEI
    ;; header carries a <ptr> to the same URL, so this page is where a reader
    ;; holding a detached file has to arrive at an answer. It stands on its
    ;; own: naming a repository document is background, not the answer.
    (testing "and the rights page is the whole statement the signed bytes cite"
      (let [rights-page (page pages "rights.html")]
        (testing "the grant, read from this release's own manifest"
          (is (string/includes? rights-page "https://creativecommons.org/publicdomain/zero/1.0/"))
          (is (string/includes? rights-page "https://creativecommons.org/publicdomain/mark/1.0/"))
          (is (string/includes? rights-page "https://w3id.org/soranoha/rights")))
        (testing "both rights layers, because a reader has to know which covers what"
          (is (string/includes? rights-page "Two rights layers"))
          (is (string/includes? rights-page "Aozora Bunko"))
          (is (string/includes? rights-page "CC0-1.0"))
          (is (string/includes? rights-page "Apache-2.0")
              "redistributing the toolchain is not redistributing the corpus"))
        (testing "and how to ask for a withdrawal, which is what a rights holder needs"
          (is (string/includes? rights-page "https://orcid.org/0000-0003-2246-8774"))
          (is (string/includes? rights-page "governance event"))
          (is (string/includes? rights-page "No warranty")))
        (is (string/includes? rights-page "docs/rights.md")
            "the long treatment is named as background")))

    (testing "and the citation page states this release's own forms"
      (let [citation-page (page pages "citation.html")]
        (is (string/includes? citation-page (subs head-hex 0 12))
            "abbreviated the way a bibliography carries it")
        (is (not (string/includes? citation-page "id=\"cite-a-release-not-the-corpus\"")))
        (is (string/includes? citation-page "docs/citation.md"))))))

(deftest the-release-history-counts-what-each-release-changed
  ;; The history is a projection over the manifests the export has already
  ;; written, so every number on it has to be derivable from those bytes by a
  ;; reader who does not trust the page. These assertions are that derivation
  ;; done independently: the head publishes two works where its predecessor
  ;; published one, and that one's source bytes differ.
  (let [works [(work "000092_000879" "蜘蛛の糸" "くものいと")
               (work "000035_001567" "走れメロス" "はしれめろす")]
        history (page (pages (inputs works)) "history.html")
        row (fn [hex] (some #(when (string/includes? % (subs hex 0 12)) %)
                            (string/split history #"<tr")))]
    (testing "every release in the chain has a row that links its manifest"
      (is (string/includes? history (str "/releases/" head-hex ".json")))
      (is (string/includes? history (str "/releases/" prev-hex ".json"))))

    (testing "the head's row states what it changed against its predecessor"
      (let [head-row (row head-hex)]
        (is (some? head-row))
        (is (string/includes? head-row "<td>2</td>") "two works published")
        ;; one work is new and one kept its slug under different source bytes,
        ;; so the row reads 2 works, 1 added, 0 removed, 1 changed
        (is (string/includes? head-row "<td>1</td><td>0</td><td>1</td>"))))

    (testing "the oldest release states no change, having nothing to differ from"
      (let [prev-row (row prev-hex)]
        (is (some? prev-row))
        (is (string/includes? prev-row "<td>1</td><td></td><td></td><td></td>"))))

    (testing "each row names the upstream revisions the release stands for"
      (is (string/includes? (row head-hex)
                            (str (subs prev-rev 0 12) " .. " (subs head-rev 0 12))))
      (is (not (string/includes? (row prev-hex) " .. "))
          "genesis has no predecessor revision to open its range"))

    (testing "a release carrying a governance event links the event beside it"
      (let [withdrawn [{"slug" "000092_000879"
                        "event" (str "snh:1:governance-event:" event-hex)}]
            with-event (page (pages (inputs works withdrawn)) "history.html")]
        (is (string/includes? with-event (str "/governance/" event-hex ".json")))
        (is (string/includes? with-event "withdrawal"))))

    (testing "and the landing page sends a reader to it"
      (is (string/includes? (page (pages (inputs works)) "index.html") "\"/history\"")))))

(deftest html-rendering-escapes-every-untrusted-position-test
  (is (= "<p class=\"a&quot;b\">&lt;x&gt;&amp;</p>"
         (html/render [:p {:class "a\"b"} "<x>&"])))
  (is (= "<br>" (html/render [:br])) "void elements have no closing tag")
  (is (= "<input disabled>" (html/render [:input {:disabled true :value nil}]))
      "true renders bare, nil renders nothing")
  (is (= "ab" (html/render (list "a" "b"))) "sequences splice"))

(deftest a-person-index-holds-a-work-once-per-relation-and-once-overall
  (testing "repeated contributor rows are rows, not works"
    (let [akutagawa {"person_id" "000879" "family_name" "芥川" "given_name" "龍之介"
                     "relation_to_work" "著者"}
          work {"slug" "000092_000879" "title" "蜘蛛の糸"
                "contributors" [akutagawa
                                akutagawa
                                (assoc akutagawa "relation_to_work" "校訂者")]}
          {:keys [works by-relation person]} (get (browse/people [work]) "000879")]
      (is (= ["000092_000879"] (mapv #(get % "slug") works))
          "one work, however many rows named the person in it")
      (is (= {"著者" ["000092_000879"] "校訂者" ["000092_000879"]}
             (update-vals by-relation #(mapv (fn [w] (get w "slug")) %)))
          "and once in each relation it was named under")
      (is (= "著者" (get person "relation_to_work"))
          "the person record is the first row that named them"))))

(deftest a-work-page-states-the-terms-that-work-is-under-test
  (let [pd (work "000092_000879" "蜘蛛の糸" "くものいと")
        by (work "054333_001657" "食品の変造" "しょくひんのへんぞう"
                 :work-rights "CC-BY-2.1-JP")
        pages (pages (inputs [pd by]))
        page-of (fn [w] (page pages (str "works/" (get w "slug") "/index.html")))]
    (testing "the public-domain work says the term expired"
      (is (string/includes? (page-of pd) "著作権の存続期間が満了しています"))
      (is (string/includes? (page-of pd)
                            "https://creativecommons.org/publicdomain/mark/1.0/")))
    (testing "the licensed work names its licence and states the condition"
      (is (string/includes? (page-of by) "CC-BY-2.1-JP"))
      (is (string/includes? (page-of by)
                            "https://creativecommons.org/licenses/by/2.1/jp/"))
      (is (string/includes? (page-of by) "クレジットの表示はこのライセンスの条件です"))
      (is (string/includes? (page-of by) "Attribution is a condition of this licence.")))
    (testing "neither page claims the other's terms"
      (is (not (string/includes? (page-of pd) "condition of this licence")))
      (is (not (string/includes? (page-of by) "著作権の存続期間が満了しています"))))
    (testing "the rights page lists both regimes rather than one"
      (let [rights-page (page pages "rights.html")]
        (is (string/includes? rights-page "public-domain"))
        (is (string/includes? rights-page "CC-BY-2.1-JP"))
        (is (not (string/includes?
                  rights-page
                  "クレジットの表示はお願いであって、利用の条件ではありません"))
            "the corpus-wide claim is false once one work is licensed")))))

(deftest a-work-page-names-the-bytes-that-follow-its-source-archive
  ;; Standard zip readers refuse 058100_001505's archive: 984 bytes follow
  ;; the archive proper, and the decoy record in them is what the reader
  ;; trusts and then rejects. The members are intact and are what the
  ;; edition was built from, so the page has to say that where a reader
  ;; checking against the source will meet it.
  (let [trailing (work "058100_001505" "ちょび髭サミュエルの話" "ちょびひげさみゅえるのはなし"
                       :trailing-bytes 984)
        ordinary (work "000092_000879" "蜘蛛の糸" "くものいと")
        pages (pages (inputs [trailing ordinary]))
        page-of (fn [w] (page pages (str "works/" (get w "slug") "/index.html")))]
    (testing "the affected work states the count and that the text survived it"
      (is (string/includes? (page-of trailing) "底本アーカイブ"))
      (is (string/includes? (page-of trailing) "984 バイトが続いています"))
      (is (string/includes? (page-of trailing) "984 bytes after the archive proper")))
    (testing "and every other work says nothing, because nothing is wrong with theirs"
      (is (not (string/includes? (page-of ordinary) "底本アーカイブ")))
      (is (not (string/includes? (page-of ordinary) "Source archive"))))))
