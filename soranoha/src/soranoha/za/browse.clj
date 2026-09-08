(ns soranoha.za.browse
  "The browse layer: static pages generated from one release's own verified
  bytes.

  Presentation, not publication. Nothing here is named by a manifest, hashed
  into the chain, or checked by a verifier — a reader who wants the published
  record follows the links these pages carry to `/catalog.json`, the
  manifests and the blobs. What the pages add is a way in: without them the
  only documented entry point is a 64-character hex string.

  Every page is a pure function of the head manifest, its catalog and the
  governance events the chain already contains, so re-exporting the same
  commit produces the same bytes and the exporter's reuse check still holds.
  The pages carry no state and no runtime; serving stays a static tree.

  Site text is bilingual, Japanese first: the corpus is Japanese literature
  read by scholars in Japan, and the encoding and protocol work is aimed at
  an international digital-humanities audience. Titles, personal names and
  source editions are never translated."
  (:require [clojure.string :as string]
            [soranoha.core.rights :as rights]
            [soranoha.snh.verify :as verify]
            [soranoha.za.html :as html]
            [soranoha.za.reading :as reading]))

;; ---------------------------------------------------------------- chrome

(def ^:private site-name "Soranoha")

(def ^:private stylesheet
  (string/join
   "\n"
   [":root { --ink: #1a1a1a; --muted: #5a5a5a; --rule: #d8d4cc; --bg: #fbfaf7; --link: #1c4f7c; }"
    "* { box-sizing: border-box; }"
    "body { margin: 0; background: var(--bg); color: var(--ink);"
    "  font-family: \"Hiragino Mincho ProN\", \"Yu Mincho\", \"Noto Serif JP\", Georgia, serif;"
    "  line-height: 1.7; }"
    "main, header, footer { max-width: 48rem; margin: 0 auto; padding: 0 1.25rem; }"
    "header { border-bottom: 1px solid var(--rule); padding-top: 1.5rem; padding-bottom: 1rem; }"
    "header a.site { font-size: 1.15rem; font-weight: bold; text-decoration: none; color: var(--ink); }"
    "nav { margin-top: .5rem; font-size: .9rem; }"
    "nav a { margin-right: 1rem; }"
    "a { color: var(--link); }"
    "main { padding-top: 1.5rem; padding-bottom: 3rem; }"
    "h1 { font-size: 1.6rem; line-height: 1.35; margin: 0 0 .25rem; }"
    "h2 { font-size: 1.15rem; margin: 2rem 0 .5rem; border-bottom: 1px solid var(--rule); padding-bottom: .25rem; }"
    ".en { color: var(--muted); font-weight: normal; }"
    ".reading { color: var(--muted); margin: 0 0 1rem; }"
    ".stats { list-style: none; padding: 0; display: flex; flex-wrap: wrap; gap: 1.5rem; }"
    ".stats li { margin: 0; }"
    ".stats .n { font-size: 1.5rem; display: block; }"
    "dl.facts { display: grid; grid-template-columns: max-content 1fr; gap: .35rem 1.25rem; margin: 0; }"
    "dl.facts dt { color: var(--muted); font-size: .9rem; }"
    "dl.facts dd { margin: 0; }"
    "ul.works, ul.plain { list-style: none; padding: 0; }"
    "ul.works li { padding: .35rem 0; border-bottom: 1px solid var(--rule); }"
    "ul.works .by { color: var(--muted); font-size: .9rem; }"
    "ul.cols { list-style: none; padding: 0; columns: 2; }"
    "code { font-family: ui-monospace, Menlo, Consolas, monospace; font-size: .85em;"
    "  overflow-wrap: anywhere; }"
    "footer { border-top: 1px solid var(--rule); padding-top: 1rem; padding-bottom: 3rem;"
    "  font-size: .85rem; color: var(--muted); }"
    "#q { width: 100%; padding: .6rem .8rem; font-size: 1rem; font-family: inherit;"
    "  border: 1px solid var(--rule); background: #fff; }"
    "#results:empty { display: none; }"
    "@media (max-width: 32rem) { dl.facts { grid-template-columns: 1fr; } ul.cols { columns: 1; } }"

    ;; the reading view. Everything below styles TEI that has been projected
    ;; into HTML by soranoha.za.reading; the class names are that projection's
    ;; vocabulary, and a class with no rule here renders as ordinary text
    ;; rather than disappearing.
    "p.controls { margin: 1rem 0 2rem; font-size: .9rem; }"
    "p.controls label { cursor: pointer; }"
    "details.front { margin: 0 0 2rem; font-size: .9rem; color: var(--muted); }"
    "details.front summary { cursor: pointer; }"
    ".colophon, .provenance { font-size: .9rem; }"
    ".tei { line-height: 2; }"
    ".tei section { margin: 0 0 1.5rem; }"
    ".tei p { margin: 0 0 1em; }"
    ;; a source heading is part of the work, not part of the site's chrome, so
    ;; it takes none of the section-heading rule above
    ".tei h2, .tei h3, .tei h4 { border: none; padding: 0; margin: 2rem 0 1rem; }"
    ".tei h2 { font-size: 1.3rem; }"
    ".tei h3 { font-size: 1.15rem; }"
    ".tei h4 { font-size: 1.05rem; }"
    ".tei ruby { ruby-align: center; }"
    ".tei rt { font-size: .5em; font-weight: normal; }"
    ".tei .gaiji { border-bottom: 1px dotted var(--muted); }"
    ".tei .gaiji-unmapped { color: var(--muted); }"
    ".tei .app, .tei .choice { border-bottom: 1px dotted var(--rule); }"
    ".tei .note { font-size: .85em; color: var(--muted); }"
    ".tei .gap { color: var(--muted); }"
    ".tei .pb { display: block; block-size: 1px; background: var(--rule); margin: 1.5rem 0; }"
    ".tei figure { margin: 1.5rem 0; padding: .75rem; border: 1px dashed var(--rule);"
    "  color: var(--muted); font-size: .9em; }"
    ".tei .graphic-url { font-size: .8em; }"
    ".tei .rend-bold { font-weight: bold; }"
    ".tei .rend-italic { font-style: italic; }"
    ".tei .rend-gothic, .tei .rend-textbook {"
    "  font-family: \"Hiragino Kaku Gothic ProN\", \"Yu Gothic\", \"Noto Sans JP\", sans-serif; }"
    ".tei .rend-dots { text-emphasis: filled sesame; }"
    ".tei .rend-dots-open { text-emphasis: open sesame; }"
    ".tei .rend-dots-circle { text-emphasis: filled circle; }"
    ".tei .rend-dots-circle-open { text-emphasis: open circle; }"
    ".tei .rend-dots-double-circle { text-emphasis: open double-circle; }"
    ".tei .rend-dots-cross { text-emphasis: filled \"\\00d7\"; }"
    ".tei .rend-dots-triangle { text-emphasis: filled triangle; }"
    ".tei .rend-dots-triangle-open { text-emphasis: open triangle; }"
    ".tei .rend-line { text-decoration: underline; }"
    ".tei .rend-line-double { text-decoration: underline double; }"
    ".tei .rend-line-wavy { text-decoration: underline wavy; }"
    ".tei .rend-line-dashed { text-decoration: underline dashed; }"
    ".tei .rend-left { text-emphasis-position: under left; text-underline-position: left; }"
    ".tei .layout-chitsuki { display: block; text-align: end; }"
    ".tei .layout-jizume { display: block; }"
    ".tei .layout-exponent { vertical-align: super; font-size: .75em; }"
    ".tei .layout-baseline-position { vertical-align: sub; font-size: .85em; }"
    ".tei .layout-small-script { font-size: .75em; }"
    ".tei .layout-tcy { text-combine-upright: all; }"
    ".tei .layout-keigakomi { border: 1px solid var(--muted); padding: 0 .2em; }"
    ".tei .layout-yokogumi { writing-mode: horizontal-tb; display: inline-block; }"
    ".tei .layout-fraction { font-size: .85em; }"
    ".tei .type-warichu { font-size: .7em; }"
    ".tei .type-upper, .tei .type-lower { display: block; line-height: 1.2; }"
    ".tei .type-heading { font-weight: bold; }"

    ;; Vertical is a checkbox rather than a script. Horizontal is the default
    ;; because it is what a browser lays out correctly without help and what a
    ;; narrow screen can show; vertical is what the source was set in, so it
    ;; is one click away rather than unavailable. Logical properties carry the
    ;; indents across: padding-inline-start is a left margin horizontally and
    ;; a top margin vertically, which is what 字下げ means in each direction.
    "body:has(#tategaki:checked) main { max-width: none; }"
    "body:has(#tategaki:checked) #reading { writing-mode: vertical-rl;"
    "  text-orientation: mixed; height: 78vh; overflow-x: auto; overflow-y: hidden;"
    "  border-block: 1px solid var(--rule); padding-block: 1rem; }"]))

(def ^:private search-js
  ;; Progressive enhancement only. Every work is reachable through the
  ;; generated author, title and NDC indexes with scripting off; this makes
  ;; 17308 works findable by typing instead of by navigating.
  (string/join
   "\n"
   ["(function () {"
    "  var box = document.getElementById('q'), out = document.getElementById('results'), rows = null;"
    "  if (!box || !out) { return; }"
    "  box.disabled = false;"
    "  box.placeholder = box.getAttribute('data-ready');"
    "  function draw() {"
    "    var q = box.value.trim().toLowerCase();"
    "    if (!rows || q === '') { out.textContent = ''; return; }"
    "    var hits = [], i;"
    "    for (i = 0; i < rows.length && hits.length < 50; i++) {"
    "      if (rows[i][4].indexOf(q) !== -1) { hits.push(rows[i]); }"
    "    }"
    "    out.innerHTML = '';"
    "    for (i = 0; i < hits.length; i++) {"
    "      var li = document.createElement('li');"
    "      var a = document.createElement('a');"
    "      a.href = '/works/' + hits[i][0] + '/';"
    "      a.textContent = hits[i][1];"
    "      li.appendChild(a);"
    "      if (hits[i][3]) {"
    "        var by = document.createElement('span');"
    "        by.className = 'by';"
    "        by.textContent = ' — ' + hits[i][3];"
    "        li.appendChild(by);"
    "      }"
    "      out.appendChild(li);"
    "    }"
    "  }"
    "  function load() {"
    "    if (rows) { return; }"
    "    rows = [];"
    "    fetch('/search-index.json').then(function (r) { return r.json(); }).then(function (d) {"
    "      rows = d.works.map(function (w) {"
    "        return [w[0], w[1], w[2], w[3], (w[1] + ' ' + w[2] + ' ' + w[3] + ' ' + w[0]).toLowerCase()];"
    "      });"
    "      draw();"
    "    });"
    "  }"
    "  box.addEventListener('focus', load);"
    "  box.addEventListener('input', function () { load(); draw(); });"
    "})();"]))

(defn- bilingual
  "Japanese label with its English counterpart marked as English, so a screen
  reader and a translation tool both get the language right."
  [ja en]
  (list ja " " [:span {:class "en" :lang "en"} (str "/ " en)]))

(defn- head-nodes [title]
  [[:meta {:charset "utf-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
   [:title (str title " — " site-name)]
   [:link {:rel "stylesheet" :href "/style.css"}]])

(defn- chrome
  ([title body] (chrome title {} body))
  ([title {:keys [main-class]} body]
   (html/document
    {:lang "ja"}
    (head-nodes title)
    [[:header
      [:a {:class "site" :href "/"} site-name]
      [:nav
       [:a {:href "/authors/"} (bilingual "著者" "Authors")]
       [:a {:href "/titles/"} (bilingual "作品名" "Titles")]
       [:a {:href "/ndc/"} (bilingual "分類" "NDC")]
       [:a {:href "/rights"} (bilingual "権利" "Rights")]
       [:a {:href "/citation"} (bilingual "引用" "Citation")]]]
     (into [:main (cond-> {} main-class (assoc :class main-class))] body)
     [:footer
      [:p (bilingual
           "本文は青空文庫の公有作品、符号化は CC0 1.0。表示は署名された記録の投影であり、記録そのものではありません。"
           "Texts are public-domain works from Aozora Bunko; the encoding is CC0 1.0. These pages are a projection of the signed record, not the record itself.")]
      [:p [:a {:href "/catalog.json"} "/catalog.json"] " · "
       [:a {:href "/releases/HEAD"} "/releases/HEAD"] " · "
       [:a {:href "https://www.aozora.gr.jp/"} "青空文庫"]]]])))

;; -------------------------------------------------------------- metadata

(defn- person-name-ja [{:strs [family_name given_name]}]
  (let [parts (remove string/blank? [family_name given_name])]
    (when (seq parts) (string/join " " parts))))

(defn- person-name-romaji [{:strs [family_name_romaji given_name_romaji]}]
  (let [parts (remove string/blank? [family_name_romaji given_name_romaji])]
    (when (seq parts) (string/join " " parts))))

(defn- person-label
  "Japanese name where there is one, romaji otherwise. A catalog entry can
  carry either alone — 105 works have no given-name romaji, and some
  contributors are recorded only in one script — so neither is required."
  [person]
  (or (person-name-ja person) (person-name-romaji person) "—"))

(defn- authors-of [work]
  (filterv #(= "著者" (get % "relation_to_work")) (get work "contributors")))

(defn- byline [work]
  (let [authors (authors-of work)]
    (when (seq authors)
      (string/join "、" (map person-label authors)))))

(def ^:private kana-rows
  [["a" "あ行" "あいうえおぁぃぅぇぉゔ"]
   ["ka" "か行" "かきくけこがぎぐげごゕゖ"]
   ["sa" "さ行" "さしすせそざじずぜぞ"]
   ["ta" "た行" "たちつてとだぢづでどっ"]
   ["na" "な行" "なにぬねの"]
   ["ha" "は行" "はひふへほばびぶべぼぱぴぷぺぽ"]
   ["ma" "ま行" "まみむめも"]
   ["ya" "や行" "やゆよゃゅょ"]
   ["ra" "ら行" "らりるれろ"]
   ["wa" "わ行" "わをんゎ"]
   ["other" "その他" ""]])

(defn- hiragana
  "Katakana folded to hiragana so one reading form drives the index."
  [^String s]
  (apply str (map (fn [c]
                    (let [n (int c)]
                      (if (<= 0x30A1 n 0x30F6) (char (- n 0x60)) c)))
                  s)))

(defn- kana-row-key [work]
  (let [reading (get work "title_reading")
        head (when-not (string/blank? reading) (first (hiragana reading)))]
    (or (some (fn [[key _ members]]
                (when (and head (string/includes? members (str head))) key))
              kana-rows)
        "other")))

(def ^:private ndc-classes
  [["0" "総記" "General works"]
   ["1" "哲学" "Philosophy"]
   ["2" "歴史" "History"]
   ["3" "社会科学" "Social sciences"]
   ["4" "自然科学" "Natural sciences"]
   ["5" "技術・工学" "Technology"]
   ["6" "産業" "Industry"]
   ["7" "芸術・美術" "The arts"]
   ["8" "言語" "Language"]
   ["9" "文学" "Literature"]
   ["other" "その他・分類なし" "Other or unclassified"]])

(defn- ndc-class-key
  "First character of the first NDC code. Aozora records children's material
  as K-prefixed codes and leaves some works unclassified, so anything that is
  not a main-class digit collects under one key rather than inventing a
  class for it."
  [work]
  (let [code (some-> (get work "ndc") (string/split #"\s+") second)
        head (when-not (string/blank? code) (str (first code)))]
    (if (some #(= head (first %)) (butlast ndc-classes)) head "other")))

;; ----------------------------------------------------------------- parts

(defn- work-link [work]
  (let [slug (get work "slug")]
    [:li
     [:a {:href (str "/works/" slug "/")} (get work "title")]
     (when-let [by (byline work)]
       [:span {:class "by"} (str " — " by)])]))

(defn- work-list [works]
  (if (seq works)
    (into [:ul {:class "works"}] (map work-link works))
    [:p (bilingual "該当する作品はありません。" "No works here.")]))

(defn- release-note [head-hex work-count]
  [:section
   [:h2 (bilingual "この版" "This release")]
   [:dl {:class "facts"}
    [:dt (bilingual "版" "Release")]
    [:dd [:a {:href (str "/releases/" head-hex ".json")} [:code head-hex]]]
    (when work-count
      (list [:dt (bilingual "作品数" "Works")] [:dd (str work-count)]))
    [:dt (bilingual "目録" "Catalog")]
    [:dd [:a {:href "/catalog.json"} "/catalog.json"]]]])

;; ----------------------------------------------------------------- pages

(defn- landing [head-hex works withdrawn-count]
  (chrome
   "青空文庫 TEI コーパス"
   [[:h1 (bilingual "青空文庫 TEI コーパス" "Aozora Bunko TEI corpus")]
    [:p (bilingual
         (str "青空文庫の公有作品を TEI P5 に変換し、内容アドレスで署名された版として公開しています。"
              "各作品に TEI、プレーンテキスト、Markdown、検証レポートが付きます。")
         (str "Public-domain works from Aozora Bunko converted to TEI P5 and published as "
              "signed, content-addressed releases. Every work carries TEI, plain text, "
              "Markdown and a validation report."))]
    [:ul {:class "stats"}
     [:li [:span {:class "n"} (str (count works))] (bilingual "作品" "works")]
     [:li [:span {:class "n"} "4"] (bilingual "形式" "formats")]
     [:li [:span {:class "n"} "CC0"] (bilingual "符号化の権利" "encoding licence")]
     (when (pos? withdrawn-count)
       [:li [:span {:class "n"} (str withdrawn-count)] (bilingual "取り下げ" "withdrawn")])]

    [:section
     [:h2 (bilingual "作品を探す" "Find a work")]
     [:p [:input {:id "q" :type "search" :disabled true
                  :placeholder "作品名・著者名で検索するにはスクリプトが必要です / Searching here needs scripting"
                  :data-ready "作品名・著者名 / Title or author"
                  :autocomplete "off"}]]
     [:ul {:class "works" :id "results"}]
     [:p (bilingual "一覧から辿ることもできます:" "Or browse the indexes:")
      " "
      [:a {:href "/authors/"} (bilingual "著者" "authors")] "、"
      [:a {:href "/titles/"} (bilingual "作品名の読み" "title readings")] "、"
      [:a {:href "/ndc/"} (bilingual "NDC 分類" "NDC classes")] "。"]]

    [:section
     [:h2 (bilingual "そのまま使う" "Use it directly")]
     [:dl {:class "facts"}
      [:dt [:a {:href "/catalog.json"} "/catalog.json"]]
      [:dd (bilingual "全作品の書誌。署名された版の一部です。"
                      "Bibliography of every work in this release; part of the signed record.")]
      [:dt [:a {:href "/releases/HEAD"} "/releases/HEAD"]]
      [:dd (bilingual "現在の版のハッシュ。" "The current release head.")]
      [:dt [:a {:href "/releases/latest"} "/releases/latest"]]
      [:dd (bilingual "現在の版のマニフェスト。" "The current release manifest.")]
      [:dt [:code "/works/<識別子>/tei"]]
      [:dd (bilingual "作品ごとの成果物。plaintext・markdown・tei-validation も同様。"
                      "Per-work artifacts; likewise plaintext, markdown and tei-validation.")]]]

    [:section
     [:h2 (bilingual "権利と引用" "Rights and citation")]
     [:p (bilingual
          "底本は著作権の消滅した作品、Soranoha の符号化は CC0 1.0。表示は要望であって条件ではありません。"
          "The underlying works are out of copyright and Soranoha's encoding is CC0 1.0. Attribution is requested, not required.")]
     [:p [:a {:href "/rights"} (bilingual "権利について" "Rights statement")] " · "
      [:a {:href "/citation"} (bilingual "引用のしかた" "How to cite")]]]

    (release-note head-hex (count works))
    [:script {:src "/search.js"}]]))

(defn- author-index [people]
  (chrome
   "著者一覧"
   [[:h1 (bilingual "著者一覧" "Authors")]
    [:p (bilingual
         (str "この版に登場する人物 " (count people) " 名。ローマ字表記の頭文字順です。")
         (str (count people) " people in this release, ordered by the initial of the romanized name."))]
    (into [:ul {:class "cols"}]
          (map (fn [{:keys [person-id label count*]}]
                 [:li [:a {:href (str "/authors/" person-id)} label]
                  [:span {:class "by"} (str " (" count* ")")]])
               people))]))

(defn- author-page [{:keys [label romaji person-id]} entries]
  (chrome
   label
   [[:h1 label]
    (when romaji [:p {:class "reading" :lang "ja-Latn"} romaji])
    [:dl {:class "facts"}
     [:dt (bilingual "人物 ID" "Person ID")]
     [:dd [:code person-id]]]
    (mapcat (fn [[relation works]]
              [[:h2 (str relation " (" (count works) ")")]
               (work-list works)])
            entries)]))

(defn- title-index [rows]
  (chrome
   "作品名から探す"
   [[:h1 (bilingual "作品名から探す" "Titles by reading")]
    [:p (bilingual
         "作品名の読みの頭文字で分けています。読みのない作品は「その他」に入ります。"
         "Grouped by the first kana of the title reading; works with no recorded reading fall under その他.")]
    (into [:ul {:class "cols"}]
          (map (fn [{:keys [key label count*]}]
                 [:li [:a {:href (str "/titles/" key)} label]
                  [:span {:class "by"} (str " (" count* ")")]])
               rows))]))

(defn- title-row-page [label works]
  (chrome
   (str "作品名 " label)
   [[:h1 label]
    [:p (bilingual (str (count works) " 作品。読みの順です。")
                   (str (count works) " works, ordered by reading."))]
    (work-list works)]))

(defn- ndc-index [classes]
  (chrome
   "NDC 分類から探す"
   [[:h1 (bilingual "NDC 分類から探す" "By NDC class")]
    [:p (bilingual
         "日本十進分類法の第一次区分です。分類のない作品と児童書の K 記号は「その他」にまとめています。"
         "Nippon Decimal Classification main classes. Unclassified works and Aozora's K-prefixed children's codes collect under その他.")]
    (into [:ul {:class "plain"}]
          (map (fn [{:keys [key ja en count*]}]
                 [:li [:a {:href (str "/ndc/" key)} (bilingual (str key " " ja) en)]
                  [:span {:class "by"} (str " (" count* ")")]])
               classes))]))

(defn- ndc-class-page [{:keys [key ja en]} works]
  (chrome
   (str "NDC " key " " ja)
   [[:h1 (bilingual (str key " " ja) en)]
    [:p (bilingual (str (count works) " 作品。") (str (count works) " works."))]
    (work-list works)]))

(defn- source-edition-line [{:strs [title publisher first_edition_year]}]
  (string/join "、" (remove string/blank?
                           [(when-not (string/blank? title) (str "『" title "』"))
                            publisher
                            (when-not (string/blank? first_edition_year)
                              (str first_edition_year "年"))])))

(defn- citation-line
  "The citation a reader should copy: author, title, orthography, identifier
  and release. Title and author alone do not identify a work — 1966 works in
  the Aozora catalog share an author-and-title pair — so the identifier is
  not optional and neither is the release."
  [head-hex work]
  (let [{:strs [slug title orthographic_style]} work]
    (str (or (byline work) "") "「" title "」"
         (when-not (string/blank? orthographic_style)
           (str "（" orthographic_style "）"))
         "。" site-name " Aozora TEI Corpus, " slug ", release "
         (subs head-hex 0 12) "…")))

(defn- work-page [head-hex work]
  (let [{:strs [slug title title_reading subtitle original_title first_published
                orthographic_style ndc card_url source_content_hash
                contributors source_editions]} work]
    (chrome
     title
     [[:h1 title]
      (when-not (string/blank? title_reading)
        [:p {:class "reading"} title_reading])
      (when-not (string/blank? subtitle) [:p subtitle])

      [:dl {:class "facts"}
       (mapcat (fn [contributor]
                 [[:dt (get contributor "relation_to_work")]
                  [:dd [:a {:href (str "/authors/" (get contributor "person_id"))}
                        (person-label contributor)]
                   (when-let [romaji (person-name-romaji contributor)]
                     [:span {:class "by" :lang "ja-Latn"} (str " " romaji)])]])
               contributors)
       (when-not (string/blank? original_title)
         (list [:dt (bilingual "原題" "Original title")] [:dd original_title]))
       (when-not (string/blank? first_published)
         (list [:dt (bilingual "初出" "First published")] [:dd first_published]))
       (when-not (string/blank? orthographic_style)
         (list [:dt (bilingual "文字遣い" "Orthography")] [:dd orthographic_style]))
       (when-not (string/blank? ndc)
         (list [:dt (bilingual "分類" "NDC")] [:dd ndc]))
       (when (seq source_editions)
         (list [:dt (bilingual "底本" "Source edition")]
               (into [:dd] (interpose [:br] (map source-edition-line source_editions)))))
       (when-not (string/blank? card_url)
         (list [:dt (bilingual "青空文庫" "Aozora card")]
               [:dd [:a {:href card_url} card_url]]))
       [:dt (bilingual "識別子" "Identifier")]
       [:dd [:code slug]]
       [:dt (bilingual "底本ハッシュ" "Source hash")]
       [:dd [:code (str "sha256:" source_content_hash)]]]

      [:section
       [:h2 (bilingual "本文を読む" "Read the text")]
       [:p [:a {:class "read-link" :href (str "/works/" slug "/read")}
            (bilingual "この作品を読む" "Read this work")]]
       [:p (bilingual
            "ルビ、外字、傍点、字下げを表示したまま読めます。縦書きにも切り替えられます。表示は下の TEI ファイルの投影であり、記録そのものではありません。"
            "Ruby, gaiji, emphasis marks and indentation are shown as encoded, and the text can be switched to vertical. The page is a rendering of the TEI file below, not the record itself.")]]

      [:section
       [:h2 (bilingual "ダウンロード" "Downloads")]
       [:ul {:class "plain"}
        [:li [:a {:href (str "/works/" slug "/tei")} "TEI XML"]
         [:span {:class "by"} " — "]
         (bilingual "本文と符号化の全体。" "The full encoding.")]
        [:li [:a {:href (str "/works/" slug "/plaintext")} (bilingual "プレーンテキスト" "Plain text")]]
        [:li [:a {:href (str "/works/" slug "/markdown")} "Markdown"]]
        [:li [:a {:href (str "/works/" slug "/tei-validation")}
              (bilingual "検証レポート" "Validation report")]
         [:span {:class "by"} " JSON"]]]
       [:p (bilingual
            "これらの URL は識別子から組み立てられ、版をまたいで変わりません。"
            "These URLs are constructible from the identifier and do not move between releases.")]]

      [:section
       [:h2 (bilingual "引用" "Citation")]
       [:p (bilingual
            "引用には識別子と版を含めてください。作品名と著者名だけでは一意に定まりません。"
            "Cite the identifier and the release: title and author alone do not identify a work.")]
       [:p [:code (citation-line head-hex work)]]
       [:p [:a {:href "/citation"} (bilingual "引用のしかた" "How to cite")]]]

      (release-note head-hex nil)])))

(defn- reading-page
  "One work rendered for reading. The page is a projection: it says so, names
  the release it was rendered from, and links the TEI file it was rendered
  out of, because a reader who thinks the rendering is wrong needs the bytes
  to check it against."
  [head-hex work {:keys [front body back]}]
  (let [{:strs [slug title title_reading]} work]
    (chrome
     title
     {:main-class "read"}
     [[:h1 title]
      (when-not (string/blank? title_reading)
        [:p {:class "reading"} title_reading])
      (when-let [by (byline work)]
        [:p {:class "reading"} by])

      ;; a checkbox, not a script: the toggle has to keep working with
      ;; scripting off, like every other way into this site
      [:p {:class "controls"}
       [:input {:type "checkbox" :id "tategaki"}]
       " "
       [:label {:for "tategaki"} (bilingual "縦書きで読む" "Read vertically")]]

      (when front
        [:details {:class "front"}
         [:summary (bilingual "テキストについての注記" "Notes on the text")]
         (into [:div {:class "tei"}] front)])

      (into [:article {:id "reading" :class "tei" :lang "ja"}] body)

      (when back
        [:section {:class "colophon"}
         [:h2 (bilingual "底本" "Source edition")]
         (into [:div {:class "tei"}] back)])

      [:section {:class "provenance"}
       [:h2 (bilingual "この表示について" "About this rendering")]
       [:p (bilingual
            "この頁は署名された TEI ファイルを読みやすく表示したものです。記録は TEI ファイルであって、この頁ではありません。校異は一つの読みを選んで示し、退けられた読みは要素の title に残しています。"
            "This page renders the signed TEI file. The file is the record; this page is a projection of it. Where the encoding carries an apparatus, one reading is shown and the rejected witness stays available as the element's title.")]
       [:dl {:class "facts"}
        [:dt (bilingual "版" "Release")]
        [:dd [:a {:href (str "/releases/" head-hex ".json")} [:code head-hex]]]
        [:dt "TEI"]
        [:dd [:a {:href (str "/works/" slug "/tei")} (str "/works/" slug "/tei")]]
        [:dt (bilingual "プレーンテキスト" "Plain text")]
        [:dd [:a {:href (str "/works/" slug "/plaintext")} (str "/works/" slug "/plaintext")]]
        [:dt (bilingual "書誌" "Bibliography")]
        [:dd [:a {:href (str "/works/" slug "/")} (str "/works/" slug "/")]]]
       [:p [:code (citation-line head-hex work)]]
       [:p [:a {:href "/citation"} (bilingual "引用のしかた" "How to cite")]]]])))

(defn- withdrawn-page [slug {:strs [reason_code statement]} last-release]
  (chrome
   (str slug " — 取り下げ")
   [[:h1 (bilingual "取り下げられた作品" "Withdrawn work")]
    [:p [:code slug]]
    [:dl {:class "facts"}
     [:dt (bilingual "理由区分" "Reason code")]
     [:dd [:code (or reason_code "—")]]
     [:dt (bilingual "説明" "Statement")]
     [:dd (or statement "—")]
     [:dt (bilingual "取り下げ記録" "Governance event")]
     [:dd [:a {:href (str "/withdrawn/" slug ".json")} (str "/withdrawn/" slug ".json")]]]
    [:p (bilingual
         "この作品は現在の版では配布していません。取り下げは追記のみの記録であり、過去の版を書き換えることはできません。"
         "This work is not distributed in the current release. Withdrawal is an append-only act: it stops current distribution and cannot rewrite earlier releases.")]
    (when last-release
      [:p (bilingual "最後に収録された版:" "Last release that contained it:")
       " " [:a {:href (str "/releases/" last-release ".json")} [:code last-release]]])
    [:p [:a {:href "/rights"} (bilingual "権利について" "Rights statement")]]]))

(defn- rights-page [{:strs [works encoding statement_url]}]
  (chrome
   "権利について"
   [[:h1 (bilingual "権利について" "Rights")]
    [:p (bilingual
         "複製、再配布、翻案、翻訳、機械可読な解析、再公開のいずれも、営利非営利を問わず自由に行えます。許諾も支払いも不要です。表示は要望であって条件ではありません。"
         "You may copy, redistribute, adapt, translate, mine and republish everything here, commercially or not, without asking and without payment. Attribution is requested, not required.")]

    [:section
     [:h2 (bilingual "二つの層" "Two layers")]
     [:p (bilingual
          "底本となる青空文庫の作品は著作権が消滅しており、Soranoha は何の権利も主張しません。青空文庫の取り扱い規準も、複製・再配布・改変を自由とし、表示を「希望」としています。"
          "The underlying Aozora Bunko works are out of copyright, and Soranoha claims no right in them. Aozora's own handling rules permit free copying, redistribution and modification, and treat attribution as requested rather than required.")]
     [:p (bilingual
          "TEI 符号化、プレーンテキストと Markdown の投影、検証レポート、目録、マニフェストは Soranoha の成果物であり、CC0 1.0 で公有に供します。"
          "The TEI encoding, the plain-text and Markdown projections, the validation reports, the catalog and the manifests are Soranoha's own work, dedicated to the public domain under CC0 1.0.")]
     [:dl {:class "facts"}
      [:dt (bilingual "底本の状態" "Underlying works")]
      [:dd [:a {:href (rights/works-uri works)} works]]
      [:dt (bilingual "符号化の licence" "Encoding licence")]
      [:dd [:a {:href (rights/licence-uri encoding)} encoding]]
      [:dt (bilingual "この文書" "This statement")]
      [:dd [:a {:href statement_url} statement_url]]]
     [:p (bilingual
          "同じ内容は各版のマニフェストの rights 欄と、各 TEI ファイルの publicationStmt/availability にも記録されています。署名されているのはマニフェストです。"
          "The same grant is recorded in every manifest's rights field and in every TEI file's publicationStmt/availability. The manifest is the signed one.")]]

    [:section
     [:h2 (bilingual "変換ソフトウェア" "The toolchain")]
     [:p (bilingual
          "コードは Apache-2.0、TEI カスタマイズとプロトコルスキーマは CC0 1.0 です。解析器の中核は P4suta/aozora の fork で、MIT または Apache-2.0 の二重ライセンスです。コーパスを再配布する場合にこれらの義務は生じませんが、ソフトウェアを再配布する場合は生じます。"
          "The code is Apache-2.0; the TEI customisation and the protocol schemas are CC0 1.0. The parser core is a fork of P4suta/aozora, dual-licensed MIT or Apache-2.0. Redistributing the corpus does not carry the toolchain's obligations; redistributing the software does.")]]

    [:section
     [:h2 (bilingual "権利者の方へ" "If you hold rights in a work")]
     [:p (bilingual
          "公開しているのは権利が消滅したと判断した作品のみですが、この規模では判断を誤ることがあります。作品の識別子（URL に現れる 000092_000879 の形）または青空文庫のカードと、主張の根拠をお知らせください。正式な法的通知である必要はありません。"
          "Only works assessed as free of subsisting rights are published, but a corpus this size will eventually be wrong about one. Send the work identifier — the 000092_000879 form visible in the URL — or the Aozora card, together with the basis of the claim. It does not need to be a formal legal notice.")]
     [:p [:a {:href "https://orcid.org/0000-0003-2246-8774"} "https://orcid.org/0000-0003-2246-8774"]]
     [:p (bilingual
          "取り下げは署名された記録として公開されます。作品は現在の版から外れ、目録からも消え、取り下げ記録が /withdrawn/<識別子>.json に残ります。追記のみの記録なので、過去の版は書き換えません。"
          "A withdrawal is published as a signed record: the work leaves the current release and its catalog, and the governing statement stays at /withdrawn/<identifier>.json. The record is append-only, so earlier releases are not rewritten.")]]]))

(defn- citation-page [head-hex]
  (chrome
   "引用のしかた"
   [[:h1 (bilingual "引用のしかた" "How to cite")]
    [:p (bilingual
         "すべて CC0 なので引用は義務ではありません。それでも、版を明示した引用をお願いします。"
         "Everything here is CC0, so citation is not required. Please cite anyway, and name the release.")]

    [:section
     [:h2 (bilingual "版を引く" "Cite a release")]
     [:p (bilingual
          "コーパスは版ごとに変わります。作品が加わり、符号化が改良され、取り下げも起こります。「Soranoha」とだけ書いた引用は、読んだバイト列を特定しません。"
          "The corpus changes between releases: works are added, the encoding improves, and a work can be withdrawn. A citation naming only the corpus does not identify the bytes you read.")]
     [:p [:code (str site-name " Aozora TEI Corpus. Release " head-hex ".")]]]

    [:section
     [:h2 (bilingual "作品を引く" "Cite one work")]
     [:p (bilingual
          "作品名と著者名だけでは足りません。青空文庫の目録では、著者名と作品名が一致する作品が 1966 件あります。識別子を必ず含めてください。"
          "Title and author are not enough: 1966 works in the Aozora catalog share an author-and-title pair. Always include the identifier.")]
     [:p [:code "芥川龍之介「蜘蛛の糸」（新字新仮名）、底本『芥川龍之介全集　第三巻』筑摩書房、1971年。Soranoha Aozora TEI Corpus, 000092_000879, release …"]]
     [:p (bilingual
          "作品名のローマ字表記は提供していません。読みには語の切れ目がなく、機械的な変換では正しいヘボン式になりません。日本語の作品名と読みをそのまま示し、翻字は投稿先の様式に従ってください。"
          "Romanized titles are not published: the reading carries no word boundaries, so a mechanical transliteration is not correct Hepburn. Take the Japanese title and its reading, and follow your journal's style.")]]

    [:section
     [:h2 (bilingual "バイト列を引く" "Cite exact bytes")]
     [:p (bilingual
          "再現性が要る場合は成果物を内容アドレスで引いてください。各成果物には snh:1:<種別>:<sha256> 形式の識別子があり、それを収めるマニフェストは署名されています。"
          "For work that must be reproducible, cite the artifact by content address: every artifact has an id of the form snh:1:<type>:<sha256>, and the manifest naming it is signed.")]
     [:p [:a {:href "/catalog.json"} "/catalog.json"] " · "
      [:a {:href (str "/releases/" head-hex ".json")} (str "/releases/" head-hex ".json")]]]]))

;; ------------------------------------------------------------- assembly

(defn- utf8 ^bytes [^String s] (.getBytes s "UTF-8"))

(defn- json-escape [^String s]
  (str "\""
       (apply str (map (fn [c]
                         (case c
                           \" "\\\""
                           \\ "\\\\"
                           \newline "\\n"
                           \return "\\r"
                           \tab "\\t"
                           (if (< (int c) 0x20)
                             (format "\\u%04x" (int c))
                             c)))
                       s))
       "\""))

(defn- search-index
  "A compact array-of-arrays index: slug, title, reading, byline. Written
  directly rather than through the protocol canonicalizer, which exists to
  make signed bytes comparable and has no business shaping a presentation
  file."
  [head-hex works]
  (str "{\"release\":" (json-escape head-hex) ",\"works\":["
       (string/join
        ","
        (map (fn [work]
               (str "[" (json-escape (get work "slug"))
                    "," (json-escape (or (get work "title") ""))
                    "," (json-escape (or (get work "title_reading") ""))
                    "," (json-escape (or (byline work) ""))
                    "]"))
             works))
       "]}"))

(defn- people
  "Every person in the release, keyed by person_id, with the works they
  contributed to grouped by the relation the catalog records."
  [works]
  (reduce
   (fn [acc work]
     (reduce (fn [acc contributor]
               (let [id (get contributor "person_id")]
                 (-> acc
                     (assoc-in [id :person] (get-in acc [id :person] contributor))
                     (update-in [id :by-relation (get contributor "relation_to_work")]
                                (fnil conj []) work))))
             acc
             (get work "contributors")))
   (sorted-map)
   works))

(defn pages
  "Every browse file for one release, as a sequence of `[path bytes]` pairs.

  Deterministic: the same manifest, catalog, events and TEI bytes in, the
  same pairs out in the same order, which is what lets the exporter's reuse
  check treat these files exactly like chain content.

  A sequence rather than a map because of the reading pages. There is one per
  work, each holding a whole rendered text, and the corpus is large enough
  that materialising them together would cost more memory than the whole
  serving tree costs on disk. The caller consumes them one at a time and
  writes each out; nothing here holds a page after it has been handed over.

  `tei` is a function from slug to that work's published TEI bytes. It is
  what makes a reading page possible without a second copy of the text: the
  bytes it returns are the artifact the manifest names and the export has
  already written."
  [{:keys [head-hex manifests catalog events tei]}]
  (let [head (second (first manifests))
        works (get catalog "works")
        by-slug (into {} (map (juxt #(get % "slug") identity)) works)
        withdrawn (get head "withdrawn")
        people-index (people works)
        reading-key (fn [work] [(or (some-> (get work "title_reading") hiragana) "")
                                (get work "slug")])
        page (fn [path ^String content] [path (utf8 content)])
        by-kana (group-by kana-row-key works)
        by-ndc (group-by ndc-class-key works)]
    (concat
     [(page "style.css" stylesheet)
      (page "search.js" search-js)
      (page "search-index.json" (search-index head-hex works))
      (page "index.html" (landing head-hex works (count withdrawn)))
      (page "rights.html" (rights-page (get head "rights")))
      (page "citation.html" (citation-page head-hex))

      (page "authors/index.html"
            (author-index (mapv (fn [[id {:keys [person by-relation]}]]
                                  {:person-id id
                                   :label (person-label person)
                                   :count* (count (distinct (mapcat val by-relation)))})
                                (sort-by (fn [[id {:keys [person]}]]
                                           [(or (person-name-romaji person) "￿")
                                            (or (person-name-ja person) "")
                                            id])
                                         people-index))))

      (page "titles/index.html"
            (title-index (mapv (fn [[key label _]]
                                 {:key key :label label :count* (count (get by-kana key))})
                               kana-rows)))

      (page "ndc/index.html"
            (ndc-index (mapv (fn [[key ja en]]
                               {:key key :ja ja :en en :count* (count (get by-ndc key))})
                             ndc-classes)))]

     (map (fn [[id {:keys [person by-relation]}]]
            (page (str "authors/" id ".html")
                  (author-page {:label (person-label person)
                                :romaji (person-name-romaji person)
                                :person-id id}
                               (sort-by key
                                        (update-vals by-relation
                                                     #(vec (sort-by reading-key %)))))))
          people-index)

     (map (fn [[key label _]]
            (page (str "titles/" key ".html")
                  (title-row-page label (vec (sort-by reading-key (get by-kana key))))))
          kana-rows)

     (map (fn [[key ja en]]
            (page (str "ndc/" key ".html")
                  (ndc-class-page {:key key :ja ja :en en}
                                  (vec (sort-by reading-key (get by-ndc key))))))
          ndc-classes)

     ;; the works, each with its bibliography page and its reading page. This
     ;; is the part that must stay lazy.
     (mapcat (fn [work]
               (let [slug (get work "slug")]
                 [(page (str "works/" slug "/index.html") (work-page head-hex work))
                  (page (str "works/" slug "/read.html")
                        (reading-page
                         head-hex work
                         (reading/render (String. ^bytes (tei slug) "UTF-8"))))]))
             works)

     ;; then the withdrawn works, which no longer have one
     (keep (fn [{:strs [slug event]}]
             (when-not (contains? by-slug slug)
               (let [entry (->> (get (get events (verify/id->hex event)) "entries")
                                (filter #(= slug (get % "slug")))
                                first)
                     last-release (some (fn [[hex manifest]]
                                          (when (some #(= slug (get % "slug"))
                                                      (get manifest "works"))
                                            hex))
                                        manifests)]
                 (page (str "works/" slug "/index.html")
                       (withdrawn-page slug entry last-release)))))
           withdrawn))))
