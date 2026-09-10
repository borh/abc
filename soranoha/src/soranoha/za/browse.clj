(ns soranoha.za.browse
  "The browse layer: static pages generated from one release's own verified
  bytes.

  Presentation, not publication. Nothing here is named by a manifest, hashed
  into the chain, or checked by a verifier: a reader who wants the published
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
            [soranoha.core.json :as json]
            [soranoha.core.rights :as rights]
            [soranoha.snh.verify :as verify]
            [soranoha.za.citation :as citation]
            [soranoha.za.docs :as docs]
            [soranoha.za.html :as html]
            [soranoha.za.markdown :as markdown]
            [soranoha.za.naming :as naming]
            [soranoha.za.reading :as reading]))

(def ^:private site-name "Soranoha")

(def ^:private orcid-url
  "Where a rights claim is sent. An ORCID record rather than a mail address:
  the served rights statement is what every manifest and every detached TEI
  file points at, so its contact route has to outlive any one mailbox, and
  ORCID is the identifier the project already publishes in `CITATION.cff`."
  "https://orcid.org/0000-0003-2246-8774")

(def ^:private example-identifier
  "A work identifier shown as a shape rather than as a link, so a reader
  recognises the form to quote when naming a work in a claim."
  "000092_000879")

(def ^:private stylesheet
  (string/join
   "\n"
   ;; Pastel light blue and green, which is what the name is: 空 for the
   ;; blue surfaces and 葉 for the green ones. Green carries the page and its
   ;; rules, blue carries what a reader acts on (links) and the table headings
   ;; that separate a grid from the prose around it. Every text colour here
   ;; clears WCAG AA against every surface it is used on: --muted, the
   ;; smallest text, is 5.66:1 on --bg and 4.86:1 on --sky.
   [(str ":root { --ink: #152a28; --muted: #4a6a66; --rule: #bfdcd5;"
         " --bg: #f5fbf9; --panel: #e2f0ed; --sky: #dcebf4; --link: #15607f; }")
    "* { box-sizing: border-box; }"
    "body { margin: 0; background: var(--bg); color: var(--ink);"
    "  font-family: \"Hiragino Mincho ProN\", \"Yu Mincho\", \"Noto Serif JP\", Georgia, serif;"
    "  line-height: 1.7; }"
    "main, header, footer { max-inline-size: 48rem; margin-block: 0; margin-inline: auto;"
    "  padding-inline: 1.25rem; }"
    "header { border-block-end: 1px solid var(--rule);"
    "  padding-block: 1.5rem 1rem; }"
    "header a.site { font-size: 1.15rem; font-weight: bold; text-decoration: none; color: var(--ink); }"
    "nav { margin-top: .5rem; font-size: .9rem; }"
    "nav a { margin-right: 1rem; }"
    "a { color: var(--link); }"
    "main { padding-block: 1.5rem 3rem; }"
    "h1 { font-size: 1.6rem; line-height: 1.35; margin-block: 0 .25rem; }"
    "h2 { font-size: 1.15rem; margin-block: 2rem .5rem;"
    "  border-block-end: 1px solid var(--rule); padding-block-end: .25rem; }"
    ".en { color: var(--muted); font-weight: normal; }"
    ".reading { color: var(--muted); margin-block: 0 1rem; }"
    ".stats { list-style: none; padding: 0; display: flex; flex-wrap: wrap; gap: 1.5rem; }"
    ".stats li { margin: 0; }"
    ".stats .n { font-size: 1.5rem; display: block; }"
    "dl.facts { display: grid; grid-template-columns: max-content 1fr; gap: .35rem 1.25rem; margin: 0; }"
    "dl.facts dt { color: var(--muted); font-size: .9rem; }"
    "dl.facts dd { margin: 0; }"
    "ul.works, ul.plain { list-style: none; padding: 0; }"
    "ul.works li { padding: .35rem 0; border-bottom: 1px solid var(--rule); }"
    "ul.works .by { color: var(--muted); font-size: .9rem; }"
    ;; smaller than the byline, since it is only needed on the rows that
    ;; would otherwise look identical
    "ul.works .variant { color: var(--muted); font-size: .85rem; }"
    "ul.cols { list-style: none; padding: 0; columns: 2; }"
    "ul.plain li.evidence { margin-block-start: .5rem; padding-block-start: .5rem;"
    "  border-block-start: 1px solid var(--rule); }"
    "code.citation { display: block; white-space: pre-wrap; overflow-wrap: anywhere;"
    "  padding: .6em .8em; background: var(--panel); border-radius: 3px; }"
    "code { font-family: ui-monospace, Menlo, Consolas, monospace; font-size: .85em;"
    "  overflow-wrap: anywhere; }"
    "footer { border-block-start: 1px solid var(--rule); padding-block: 1rem 3rem;"
    "  font-size: .85rem; color: var(--muted); }"
    "#q { width: 100%; padding: .6rem .8rem; font-size: 1rem; font-family: inherit;"
    "  border: 1px solid var(--rule); background: #fff; }"
    "#results:empty { display: none; }"
    ;; the copy button sits beside the value it copies, quiet until wanted:
    ;; this is a convenience next to the text, not a control competing with it
    "button.copy { margin-inline-start: .5em; padding: .1em .5em; font: inherit;"
    "  font-size: .75em; line-height: 1.6; color: var(--muted); cursor: pointer;"
    "  background: var(--bg); border: 1px solid var(--rule); border-radius: 3px;"
    "  vertical-align: middle; }"
    "button.copy:hover, button.copy:focus-visible { color: var(--ink);"
    "  background: var(--panel); }"
    "button.copy.done { color: var(--link); border-color: var(--link); }"
    ;; beside a short value the button sits inline, but these two are blocks
    ;; the full width of the column, so the button goes under the block it
    ;; copies and against its trailing edge, where it still reads as belonging
    ;; to that block rather than floating loose beneath it
    "pre + button.copy, code.citation + button.copy { display: block;"
    "  width: fit-content; margin: .4rem 0 0; margin-inline-start: auto; }"
    "@media (max-width: 32rem) { dl.facts { grid-template-columns: 1fr; } ul.cols { columns: 1; } }"

    ;; the documentation pages. Prose the project wrote, so unlike the reading
    ;; view these are ordinary elements and take ordinary rules.
    "main.doc h3 { font-size: 1.05rem; margin: 1.5rem 0 .5rem; }"
    "main.doc table { border-collapse: collapse; width: 100%; font-size: .9rem;"
    "  display: block; overflow-x: auto; }"
    "main.doc th, main.doc td { border: 1px solid var(--rule); padding: .3rem .5rem;"
    "  text-align: start; vertical-align: top; }"
    "main.doc th { background: var(--sky); }"
    "main.doc pre { background: var(--panel); padding: .6rem .8rem; border-radius: 3px;"
    "  overflow-x: auto; font-size: .85rem; line-height: 1.5; }"
    "main.doc pre code { font-size: 1em; overflow-wrap: normal; }"
    "main.doc blockquote { margin: 1rem 0; padding-inline-start: 1rem;"
    "  border-inline-start: 3px solid var(--rule); color: var(--muted); }"
    "nav.siblings { margin-top: 3rem; border-top: 1px solid var(--rule); padding-top: 1rem;"
    "  font-size: .9rem; }"

    ;; the reading view. Everything below styles TEI that has been projected
    ;; into HTML by soranoha.za.reading; the class names are that projection's
    ;; vocabulary, and a class with no rule here renders as ordinary text
    ;; rather than disappearing.
    "p.controls { margin-block: 1rem 2rem; font-size: .9rem; }"
    "p.controls label { cursor: pointer; }"
    "details.front { margin-block: 0 2rem; font-size: .9rem; color: var(--muted); }"
    "details.front summary { cursor: pointer; }"
    "details.bibtex summary { cursor: pointer; color: var(--muted); font-size: .9rem; }"
    ;; the entry wraps rather than scrolls: a reader is here to select it, and
    ;; a line running off the edge is the one thing that makes that harder
    "details.bibtex pre { margin: .5rem 0 0; padding: .6em .8em; background: var(--panel);"
    "  border-radius: 3px; font-family: ui-monospace, Menlo, Consolas, monospace;"
    "  font-size: .85em; white-space: pre-wrap; overflow-wrap: anywhere; }"
    ".colophon, .provenance { font-size: .9rem; }"
    ".tei { line-height: 2; }"
    ".tei section { margin-block: 0 1.5rem; }"
    ".tei p { margin-block: 0 1em; }"
    ;; a source heading is part of the work, not part of the site's chrome, so
    ;; it takes none of the section-heading rule above
    ".tei h2, .tei h3, .tei h4 { border: none; padding: 0; margin-block: 2rem 1rem; }"
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
    ".tei .pb { display: block; block-size: 1px; background: var(--rule); margin-block: 1.5rem; }"
    ".tei figure { margin-block: 1.5rem; margin-inline: 0; padding: .75rem;"
    "  border: 1px dashed var(--rule);"
    "  color: var(--muted); font-size: .9em; }"
    ".tei .graphic-url { font-size: .8em; }"
    ".tei .ref-target { font-size: .8em; color: var(--muted); }"
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
    ;;
    ;; The mode is set on body, which propagates it to the viewport, so the
    ;; page's own scroller is the one that runs right to left. Giving the text
    ;; a scrolling box of its own instead is what the earlier rule did, and it
    ;; opened every work at its last line: a nested scroller starts at its left
    ;; edge, and in vertical-rl the left edge is the end. Measured on 小説総論
    ;; at 1280px, the first paragraph sat at x=2821 and had to be scrolled back
    ;; to; on the document scroller it sits at x=859, on screen at load. The
    ;; fixed height that box needed also clipped the last line of every work.
    "body:has(#tategaki:checked) { writing-mode: vertical-rl; text-orientation: mixed; }"]))

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

(def ^:private copy-js
  ;; Progressive enhancement, like the search box: the button is created here
  ;; rather than emitted into every page, so with scripting off a reader gets
  ;; selectable text and no dead control. The button is a sibling of the code
  ;; rather than a child, so selecting the code by hand never picks it up, and
  ;; a code inside a link gets its button after the link so the anchor stays
  ;; whole.
  (string/join
   "\n"
   ["(function () {"
    "  var clip = navigator.clipboard;"
    "  if (!clip || !document.querySelectorAll) { return; }"
    "  var ja = document.documentElement.lang === 'ja';"
    "  var idle = ja ? 'コピー' : 'Copy', done = ja ? 'コピーしました' : 'Copied';"
    "  function attach(node, text) {"
    "    var anchor = node.parentNode;"
    "    if (!anchor) { return; }"
    "    if (anchor.tagName === 'A') { node = anchor; anchor = anchor.parentNode; }"
    "    var button = document.createElement('button');"
    "    button.type = 'button';"
    "    button.className = 'copy';"
    "    button.textContent = idle;"
    "    button.setAttribute('aria-label', idle);"
    "    button.addEventListener('click', function () {"
    "      clip.writeText(text).then(function () {"
    "        button.textContent = done;"
    "        button.classList.add('done');"
    "        setTimeout(function () {"
    "          button.textContent = idle;"
    "          button.classList.remove('done');"
    "        }, 1500);"
    "      });"
    "    });"
    "    anchor.insertBefore(button, node.nextSibling);"
    "  }"
    "  var blocks = document.querySelectorAll('pre'), i;"
    "  for (i = 0; i < blocks.length; i++) {"
    "    attach(blocks[i], blocks[i].textContent);"
    "  }"
    ;; A button belongs beside a value a reader would rather copy than retype,
    ;; and not beside every `code` in a sentence. Two exclusions do that.
    ;; Reference tables are prose about the vocabulary rather than values to
    ;; take away: on the vocabulary page 74 of 113 codes are table cells.
    ;; Twelve characters is where the rest divides: every value a work page
    ;; presents is 13 or longer, the shortest being the identifier itself,
    ;; while the inline mentions left over are `@rend` and `snh:src`.
    "  var codes = document.querySelectorAll('code');"
    "  for (i = 0; i < codes.length; i++) {"
    "    var code = codes[i];"
    "    if (code.closest('pre') || code.closest('table')) { continue; }"
    "    if (code.textContent.length < 12) { continue; }"
    "    attach(code, code.textContent);"
    "  }"
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
           "本文は青空文庫の作品、符号化データは CC0 1.0。作品ごとの権利の状態は各作品のページにあります。署名はマニフェストと各ファイルにかかり、このサイトはそれを表示しています。"
           "Texts are works from Aozora Bunko; the encoding is CC0 1.0. Each work's page states the rights standing of that work. The signature covers the manifest and the artifact files, and this site displays them.")]
      [:p [:a {:href "/catalog.json"} "/catalog.json"] " · "
       [:a {:href "/releases/HEAD"} "/releases/HEAD"] " · "
       [:a {:href "https://www.aozora.gr.jp/"} "青空文庫"]]]
     [:script {:src "/copy.js"}]])))

;; Personal names, bylines and download filenames are rendered by
;; soranoha.za.naming, which the bulk archives share: a name on a page and
;; the name inside a ZIP must agree.
(def ^:private person-name-ja naming/person-name-ja)
(def ^:private person-name-romaji naming/person-name-romaji)
(def ^:private person-label naming/person-label)
(def ^:private byline naming/byline)

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

(def ndc-classes
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

(defn ndc-class-key
  "First character of the first NDC code. Aozora Bunko records children's material
  as K-prefixed codes and leaves some works unclassified, so anything that is
  not a main-class digit collects under one key rather than inventing a
  class for it."
  [work]
  (let [code (some-> (get work "ndc") (string/split #"\s+") second)
        head (when-not (string/blank? code) (str (first code)))]
    (if (some #(= head (first %)) (butlast ndc-classes)) head "other")))

(defn- source-edition-title [work]
  (get (first (get work "source_editions")) "title"))

(defn- variant-facts
  "The facts that distinguish works a list would otherwise print identically.

  Aozora Bunko carries several transcriptions of one text: 樋口一葉's わかれ道
  is keyed three times. They differ in the edition they were keyed from and in
  whether that edition modernized the orthography, so those are the two facts
  that separate them.

  Orthography alone separates most groups; the source edition is added only
  where it does not."
  [works]
  (let [styles (mapv #(get % "orthographic_style") works)]
    (if (or (apply distinct? styles) (= 1 (count styles)))
      [:orthographic_style]
      [:orthographic_style :source-edition])))

(defn- variant-label [work facts]
  (let [parts (keep (fn [fact]
                      (case fact
                        :orthographic_style (get work "orthographic_style")
                        :source-edition (source-edition-title work)))
                    facts)]
    (when (seq parts)
      (str "（" (string/join "・" parts) "）"))))

(defn- work-link [work facts]
  (let [slug (get work "slug")]
    [:li
     [:a {:href (str "/works/" slug "/")} (get work "title")]
     (when-let [by (byline work)]
       [:span {:class "by"} (str " — " by)])
     (when-let [label (and facts (variant-label work facts))]
       [:span {:class "variant"} (str " " label)])]))

(defn- work-list
  "One row per work. A title and byline do not identify a work uniquely.
  Where the same pair names more than one work in a list, each row also shows
  what separates it, so that three rows reading わかれ道 — 樋口 一葉 are not
  read as one row repeated three times."
  [works]
  (if (seq works)
    (let [ambiguous (into {}
                          (keep (fn [[key group]]
                                  (when (< 1 (count group))
                                    [key (variant-facts group)])))
                          (group-by (juxt #(get % "title") byline) works))]
      (into [:ul {:class "works"}]
            (map #(work-link % (get ambiguous [(get % "title") (byline %)])) works)))
    [:p (bilingual "該当する作品はありません。" "No works here.")]))

(defn- archive-label [artifact-type]
  (case artifact-type
    "tei" "TEI XML"
    "plaintext" (bilingual "プレーンテキスト" "Plain text")
    artifact-type))

(defn- bulk-section
  "The pre-built archives for one selection. Serving is a static tree with no
  runtime, so a selection cannot be assembled on request: these are the
  selections, and they mirror the axis of the page they appear on."
  [scope-ja scope-en paths]
  [:section
   [:h2 (bilingual "まとめてダウンロード" "Download in bulk")]
   [:p (bilingual scope-ja scope-en)]
   (into [:ul {:class "plain"}]
         (map (fn [[path artifact-type]]
                [:li [:a {:href (str "/" path)} (archive-label artifact-type)]
                 [:span {:class "by"} " ZIP"]])
              paths))
   [:p (bilingual
        (str "どの ZIP にも直下に catalog.csv が入っています。識別子、作品名、著者、底本、初出、分類、版など、"
             "引用に必要な項目が列に分かれているので、表計算ソフトでそのまま開けます。"
             "TEI ファイルを一つも開かずに、まとめた作品全体の文献表を作れます。"
             "引用には識別子・底本ハッシュ・版の列を使ってください。ファイル名は便宜のためのものです。")
        (str "Every archive carries catalog.csv at its root, with the structured citation "
             "fields in columns: identifier, title, author, source edition, first publication, "
             "class and release among them. It opens directly in a spreadsheet, so a whole "
             "selection becomes a bibliography without opening a single TEI file. Cite the "
             "identifier, source hash and release columns; the filename is a convenience."))]])

(defn- corpus-archives []
  (map (juxt naming/corpus-bundle-path identity) naming/bulk-artifact-types))

(defn- release-note [head-hex work-count]
  [:section
   [:h2 (bilingual "この版" "This release")]
   [:dl {:class "facts"}
    [:dt (bilingual "版" "Release")]
    [:dd [:a {:href (str "/releases/" head-hex ".json")} [:code head-hex]]]
    (when work-count
      (list [:dt (bilingual "作品数" "Works")] [:dd (str work-count)]))
    [:dt (bilingual "目録" "Catalog")]
    [:dd [:a {:href "/catalog.json"} "/catalog.json"]]
    [:dt (bilingual "履歴" "History")]
    [:dd [:a {:href "/history"} (bilingual "この版までのすべての版" "Every release up to this one")]]]])

(defn- document-entries
  "Every served document with its own text and its own English title.

  Read once per activation and passed around, because the landing page, the
  page itself and every page's sibling list all name the same set, and a
  document read twice could be read twice differently."
  []
  (mapv (fn [document]
          (let [text (docs/read-text (:path document))]
            (assoc document :text text :en (markdown/title text))))
        docs/documents))

(defn- document-page [{:keys [path text ja en]}]
  (chrome
   ja
   {:main-class "doc"}
   (concat [[:h1 (bilingual ja en)]]
           (markdown/render {:text text
                             :source path
                             :link (partial docs/resolve-link path)}))))

(defn- landing [head-hex works withdrawn-count]
  (chrome
   "青空文庫 TEI コーパス"
   [[:h1 (bilingual "青空文庫 TEI コーパス" "Aozora Bunko TEI corpus")]
    [:p (bilingual
         (str "著作権の消滅した青空文庫の作品を TEI P5 に変換し、内容のハッシュで特定できる署名付きの版として公開しています。"
              "各作品に TEI、プレーンテキスト、Markdown、検証レポートが付きます。")
         (str "Public-domain works from Aozora Bunko converted to TEI P5 and published as "
              "signed, content-addressed releases. Every work carries TEI, plain text, "
              "Markdown and a validation report."))]
    [:ul {:class "stats"}
     [:li [:span {:class "n"} (str (count works))] (bilingual "作品" "works")]
     [:li [:span {:class "n"} "4"] (bilingual "形式" "formats")]
     [:li [:span {:class "n"} "CC0"] (bilingual "符号化のライセンス" "encoding licence")]
     (when (pos? withdrawn-count)
       [:li [:span {:class "n"} (str withdrawn-count)] (bilingual "取り下げ" "withdrawn")])]

    [:section
     [:h2 (bilingual "作品を探す" "Find a work")]
     [:p [:input {:id "q" :type "search" :disabled true
                  :placeholder "検索には JavaScript が必要です / Search requires JavaScript"
                  :data-ready "作品名・著者名 / Title or author"
                  :autocomplete "off"}]]
     [:ul {:class "works" :id "results"}]
     [:p (bilingual "一覧から探すこともできます:" "Or browse the indexes:")
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
      [:dd (bilingual "作品ごとのファイル。plaintext・markdown・tei-validation も同じ形です。"
                      "Per-work artifacts; plaintext, markdown and tei-validation take the same form.")]]]

    (bulk-section
     "この版の全作品を一つの ZIP にまとめてあります。著者ごと・分類ごとの ZIP は、それぞれのページにあります。"
     "Every work in this release, in one archive. Per-author and per-class archives are on the author and NDC pages."
     (corpus-archives))

    [:section
     [:h2 (bilingual "権利と引用" "Rights and citation")]
     [:p (bilingual
          "底本は、著作権の消滅した作品か、権利者がクリエイティブ・コモンズ 表示ライセンスの下で公開している作品です。Soranoha の符号化データは CC0 1.0。作品ごとの権利の状態は catalog.csv の rights 列にあります。"
          "Each underlying work is either out of copyright or published by its rightsholder under a Creative Commons Attribution licence; Soranoha's encoding is CC0 1.0. The rights column of catalog.csv carries each work's own standing.")]
     [:p [:a {:href "/rights"} (bilingual "権利について" "Rights statement")] " · "
      [:a {:href "/citation"} (bilingual "引用のしかた" "How to cite")]]]

    (release-note head-hex (count works))
    [:script {:src "/search.js"}]]))

(defn- author-index [people]
  (chrome
   "著者一覧"
   [[:h1 (bilingual "著者一覧" "Authors")]
    [:p (bilingual
         (str "この版の作品に関わった人物 " (count people) " 名。ローマ字表記の頭文字順です。")
         (str (count people) " people in this release, ordered by the initial of the romanized name."))]
    (into [:ul {:class "cols"}]
          (map (fn [{:keys [person-id label count*]}]
                 [:li [:a {:href (str "/authors/" person-id)} label]
                  [:span {:class "by"} (str " (" count* ")")]])
               people))]))

(defn- author-page [{:keys [label romaji person-id person]} entries]
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
            entries)
    (bulk-section
     "この人物が関わった作品を、役割を問わずまとめて取得できます。"
     "Every work this person contributed to, in any role, in one archive."
     (map (juxt #(naming/author-bundle-path person person-id %) identity)
          naming/bulk-artifact-types))]))

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
    [:p (bilingual (str (count works) " 作品。読みの順に並べています。")
                   (str (count works) " works, ordered by reading."))]
    (work-list works)]))

(defn- ndc-index [classes]
  (chrome
   "NDC 分類から探す"
   [[:h1 (bilingual "NDC 分類から探す" "By NDC class")]
    [:p (bilingual
         "日本十進分類法の第一次区分です。分類のない作品と児童書の K 記号は「その他」にまとめています。"
         "Nippon Decimal Classification main classes. Unclassified works and Aozora Bunko's K-prefixed children's codes are grouped under その他.")]
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
    (work-list works)
    (bulk-section
     (str "この分類の " (count works) " 作品をまとめて取得できます。")
     (str "All " (count works) " works in this class, in one archive.")
     (map (juxt #(naming/ndc-bundle-path key %) identity)
          naming/bulk-artifact-types))]))

(defn- source-edition-line
  "One 底本, shown the way Aozora Bunko recorded it. 初版発行年 is a free-form
  publication history rather than a year, as `1981（昭和56）年3月20日` is, and
  914 values carry a printing history after that, so it is shown verbatim and
  nothing is appended to it. Every recorded value already ends in its own
  年, 月 or 日."
  [{:strs [title publisher first_edition_year]}]
  (string/join "、" (remove string/blank?
                           [(when-not (string/blank? title) (str "『" title "』"))
                            publisher
                            first_edition_year])))

(defn- work-page
  "One work's bibliography page. `release` carries the head hex and the
  release DOI, which every citation form on the page has to name."
  [release work]
  (let [{:keys [head-hex]} release
        {:strs [slug title title_reading subtitle original_title first_published
                orthographic_style ndc card_url source_content_hash
                contributors source_editions]} work
        works-standing (get work "rights")]
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
       ;; the site-wide statement covers almost every work, and a reader who
       ;; saw only it would take attribution for a request on the works where
       ;; it is a licence condition. So the condition is stated here, on the
       ;; page of the work it binds.
       (when-not (string/blank? works-standing)
         (list [:dt (bilingual "底本の権利" "Rights in the source text")]
               [:dd [:a {:href (rights/works-uri works-standing)} works-standing] " "
                (if (= "public-domain" works-standing)
                  (bilingual "著作権の存続期間が満了しています。"
                             "The copyright term has expired.")
                  (bilingual "クレジットの表示はこのライセンスの条件です。"
                             "Attribution is a condition of this licence."))]))
       (when-not (string/blank? card_url)
         (list [:dt (bilingual "青空文庫" "Aozora Bunko card")]
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
            "ルビ、外字、傍点、字下げをそのまま表示して読めます。縦書きに切り替えることもできます。この表示は下の TEI ファイルから組み立てています。"
            "Ruby, gaiji, emphasis marks and indentation are shown as encoded, and the text can be set vertically. The reading view is built from the TEI file below.")]]

      [:section
       [:h2 (bilingual "ダウンロード" "Downloads")]
       (into [:ul {:class "plain"}]
             (map (fn [[artifact-type label class]]
                    (let [name (naming/filename work artifact-type)]
                      [:li (cond-> {} class (assoc :class class))
                       [:a {:href (str "/works/" slug "/" name) :download name} label]
                       [:span {:class "by"} " " [:code name]]]))
                  ;; the first three are the work itself in three forms; the
                  ;; report is evidence about them, so it sits below a rule
                  ;; rather than reading as a fourth copy of the text
                  [["tei" "TEI XML"]
                   ["plaintext" (bilingual "プレーンテキスト" "Plain text")]
                   ["markdown" "Markdown"]
                   ["tei-validation" (bilingual "検証レポート" "Validation report") "evidence"]]))
       [:p (bilingual
            (str "引用には識別子を使ってください。ファイル名は便宜のためのものです。"
                 "同じバイト列は、末尾に種別名を置いた URL からも取得できます。この URL は識別子から組み立てられ、版をまたいでも同じです。")
            (str "Cite the identifier; the filename is a convenience. The same bytes are also "
                 "served at URLs named after the artifact type, which are constructible from "
                 "the identifier and stable across releases."))]
       [:p (->> naming/artifact-kinds
                (map (fn [artifact-type]
                       [:code (str "/works/" slug "/" artifact-type)]))
                (interpose " · "))]]

      [:section
       [:h2 (bilingual "引用" "Citation")]
       [:p (bilingual
            "引用には識別子と版を含めてください。作品名と著者名だけでは一意に定まりません。"
            "Cite the identifier and the release: title and author alone do not identify a work.")]
       [:p [:code {:class "citation" :lang "ja"} (citation/rendered release work)]]
       [:p [:code {:class "citation"} (citation/rendered-en release work)]]
       [:p {:class "by"}
        (bilingual
         "上が日本語の文献表用、下が英語の文献表用です。作品名はどちらも日本語のままです。読みには語の切れ目がないため、ヘボン式への翻字には編集上の判断が要ります。翻字は投稿先の様式に従ってください。"
         "The first form is for a Japanese bibliography, the second for an English one. Titles stay in Japanese in both: the reading carries no word boundaries, so Hepburn romanization takes editorial judgement. Romanize to your journal's style.")]
       [:p (bilingual "文献管理ソフト向け:" "For reference managers:")
        " "
        [:a {:href (str "/works/" slug "/citation.json") :download (str slug ".json")}
         "CSL-JSON"]
        " · "
        [:a {:href (str "/works/" slug "/citation.bib") :download (str slug ".bib")}
         "BibLaTeX"]]
       ;; a download is the wrong shape for the common case, which is pasting
       ;; one entry into a .bib file already open in an editor. Folded away,
       ;; because a reader who wants the file still wants the link above.
       [:details {:class "bibtex"}
        [:summary (bilingual "BibLaTeX をそのままコピーする" "Copy the BibLaTeX entry")]
        [:pre (citation/biblatex release work)]]
       ;; COinS: an empty span whose title is an OpenURL context object. This
       ;; is the one metadata form a browser connector reads without being
       ;; told the site exists, and `genre=bookitem` is what makes it save the
       ;; work as a section of its 底本 rather than as a book of its own.
       [:span {:class "Z3988" :title (citation/coins release work)}]
       [:p [:a {:href "/citation"} (bilingual "引用のしかた" "How to cite")]]]

      (release-note head-hex nil)])))

(defn- reading-page
  "One work rendered for reading. The page is a projection: it says so, names
  the release it was rendered from, and links the TEI file it was rendered
  out of, because a reader who thinks the rendering is wrong needs the bytes
  to check it against."
  [release work {:keys [front body back]}]
  (let [{:keys [head-hex]} release
        {:strs [slug title title_reading]} work]
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
            "記録は下にリンクした署名済みの TEI ファイルです。このページはそれを読むための一つの形です。読みが複数ある箇所は一つだけを本文に示し、もう一方は語の title に残しています。"
            "The record is the signed TEI file linked below. This page is one way of reading it. Where the text has more than one reading, the page shows one of them and keeps the other on the word itself, in its title text.")]
       [:dl {:class "facts"}
        [:dt (bilingual "版" "Release")]
        [:dd [:a {:href (str "/releases/" head-hex ".json")} [:code head-hex]]]
        [:dt "TEI"]
        [:dd [:a {:href (str "/works/" slug "/tei")} (str "/works/" slug "/tei")]]
        [:dt (bilingual "プレーンテキスト" "Plain text")]
        [:dd [:a {:href (str "/works/" slug "/plaintext")} (str "/works/" slug "/plaintext")]]
        [:dt (bilingual "書誌" "Bibliography")]
        [:dd [:a {:href (str "/works/" slug "/")} (str "/works/" slug "/")]]]
       [:p [:code {:class "citation"} (citation/rendered release work)]]
       ;; the same OpenURL record the bibliography page carries, because this
       ;; is the page a reader is on when they reach for their reference
       ;; manager
       [:span {:class "Z3988" :title (citation/coins release work)}]
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
     ;; the two rows above are read out of this file, so the label names the
     ;; document a reader would be opening rather than its role in the protocol
     [:dt (bilingual "取り下げの記録" "Withdrawal record")]
     [:dd [:a {:href (str "/withdrawn/" slug ".json")} (str "/withdrawn/" slug ".json")]]]
    [:p (bilingual
         "この作品は現在の版では配布していません。取り下げても記録を消すことはなく、以後の配布を止めるだけで、過去の版は署名の連鎖にそのまま残ります。"
         "Soranoha no longer distributes this work in the current release. Withdrawing a work removes nothing: it stops further distribution, and the signed chain keeps every earlier release exactly as published.")]
    (when last-release
      [:p (bilingual "最後に収録された版:" "Last release that contained it:")
       " " [:a {:href (str "/releases/" last-release ".json")} [:code last-release]]])
    [:p [:a {:href "/rights"} (bilingual "権利について" "Rights statement")]]]))

(defn- covered-range
  "The upstream revisions a release covers. A release is minted when the
  corpus moved rather than on every upstream commit, so one release spans
  `(covers_from, upstream_rev]`, which can be several revisions. Genesis
  has no predecessor revision to open its range and shows only its own."
  [{:strs [covers_from upstream_rev]}]
  (if covers_from
    (str (subs covers_from 0 12) " .. " (subs upstream_rev 0 12))
    (subs upstream_rev 0 12)))

(defn- published-change
  "What a release did to the published set against its predecessor: works
  added, works removed, and works whose source bytes changed.

  Counts rather than lists. A corpus release changes a median of two works
  out of 17,602, but a toolchain change moves every one of them, and a page
  naming them all would be larger than the manifests it summarises.

  `source_content_hash` is what decides `changed`, because it is the fact
  about the work rather than about the toolchain: a release that reruns an
  unchanged source through a new parser publishes different artifacts, and
  saying that every work changed would bury the two that did."
  [manifest predecessor]
  (let [source-of (fn [m]
                    (into {} (map (juxt #(get % "slug") #(get % "source_content_hash")))
                          (get m "works")))
        before (source-of predecessor)
        now (source-of manifest)]
    {:added (count (remove before (keys now)))
     :removed (count (remove now (keys before)))
     :changed (count (filter (fn [[slug hash]]
                               (when-let [was (get before slug)] (not= was hash)))
                             now))}))

(defn- history-row [[hex manifest] predecessor events]
  (let [event-id (get manifest "governance_event")
        event (some->> event-id verify/id->hex (get events))
        {:keys [added removed changed]} (when predecessor
                                          (published-change manifest predecessor))]
    [:tr
     [:td [:a {:href (str "/releases/" hex ".json")} [:code (subs hex 0 12)]]
      (when event
        (list " "
              [:a {:href (str "/governance/" (verify/id->hex event-id) ".json")}
               (get event "kind")]))]
     [:td [:code (covered-range (get manifest "corpus"))]]
     [:td (str (count (get manifest "works")))]
     [:td (if predecessor (str added) "")]
     [:td (if predecessor (str removed) "")]
     [:td (if predecessor (str changed) "")]]))

(defn- history-page
  "Every release in the chain, newest first, with what it changed.

  Assembled here rather than published as an artifact for the reason
  `adr/0001` gave the citation forms and the catalog CSV: it is a projection
  over signed bytes, and a projection stays correctable. Every number on it
  is recomputed from the manifests the export has already written, so a
  reader who distrusts the page can derive it from the same files.

  The oldest release has no predecessor and so has no change to state; its
  cells are left empty rather than filled with the count of the whole
  corpus, which would read as a release that added everything at once."
  [manifest-seq events]
  (chrome
   "版の履歴"
   {:main-class "doc"}
   [[:h1 (bilingual "版の履歴" "Release history")]
    [:p (bilingual
         (str "署名された連鎖にあるすべての版です。新しいものから順に並んでいます。"
              "各行の数字は、その版が一つ前の版に対して何を変えたかを、公開されたマニフェストから数え直したものです。")
         (str "Every release in the signed chain, newest first. The counts on each row are "
              "what that release changed against the one before it, recomputed from the "
              "published manifests."))]
    [:table
     [:thead
      [:tr
       [:th (bilingual "版" "Release")]
       [:th (bilingual "底本の範囲" "Upstream range")]
       [:th (bilingual "作品数" "Works")]
       [:th (bilingual "追加" "Added")]
       [:th (bilingual "削除" "Removed")]
       [:th (bilingual "変更" "Changed")]]]
     (into [:tbody]
           (map (fn [[entry predecessor]]
                  (history-row entry (second predecessor) events))
                (partition-all 2 1 (manifest-seq))))]
    [:p (bilingual
         "「底本の範囲」は、その版が対象とする青空文庫のリビジョンの範囲です。作品の書庫が動いたときに版を作るので、一つの版が複数のリビジョンにまたがることがあります。最初の版には比較対象となる前の版がないので、変更の欄は空です。"
         (str "The upstream range is the span of Aozora Bunko revisions a release covers. "
              "A release is minted when a work archive moved, so one release can cover "
              "several revisions. The oldest release has no predecessor to compare against, so its "
              "change columns are empty."))]]))

(defn- last-release-of
  "The newest release still listing each of `slugs`, in one newest-first pass.

  A withdrawn work's page has to name the release it was last published in,
  and the chain is ordered newest first, so the answer for every slug is
  found by walking until none are outstanding. Done once for all of them
  rather than once per slug: the alternative reads the chain again for every
  withdrawal, and reading the chain is the expensive part."
  [manifest-seq slugs]
  (loop [remaining (manifest-seq)
         wanted (set slugs)
         found {}]
    (let [[hex manifest] (first remaining)]
      (if (or (nil? hex) (empty? wanted))
        found
        (let [listed (into #{} (comp (map #(get % "slug")) (filter wanted))
                           (get manifest "works"))]
          (recur (rest remaining)
                 (reduce disj wanted listed)
                 (into found (map (fn [slug] [slug hex])) listed)))))))

(defn- generated-page
  "One of the two pages that state facts read from the release.

  The facts come from the head manifest rather than from a document that could
  disagree with it, which is the whole reason these two pages are assembled
  here instead of being served from Markdown.

  `:source` names the repository document that treats the subject at length.
  The page stands on its own without it: a reader who followed
  `rights.statement_url` out of a detached TEI file has to find the answer
  here, not a forwarding address."
  [{:keys [ja source]} title lead]
  (chrome
   ja
   {:main-class "doc"}
   (concat [[:h1 (bilingual ja title)]] lead
           [[:p {:class "by"}
             (bilingual (str "背景と詳細はリポジトリの " source " にあります。")
                        (str "Background and detail are in " source
                             " in the repository."))]])))

(defn- works-standings
  "The standings the release actually publishes under, in the order the rights
  page lists them: the public domain first, because it is almost every work,
  then the licences, so a reader meets the common case before the exception."
  [manifest]
  (let [present (into #{} (map #(get % "rights")) (get manifest "works"))]
    (into (filterv present ["public-domain"])
          (sort (disj present "public-domain")))))

(defn- rights-page [document standings {:strs [encoding statement_url]}]
  (generated-page
   document
   "Rights and licensing"
   [[:p (bilingual
         "複製、再頒布、翻案、翻訳、情報解析、公衆送信のいずれも、営利非営利を問わず自由に行えます。許諾を得る必要も、対価を支払う必要もありません。ほとんどの作品では、クレジットの表示はお願いであって利用の条件ではありません。権利者がクリエイティブ・コモンズ 表示ライセンスの下で公開している作品に限り、クレジットの表示はそのライセンスの条件です。どの作品がどちらにあたるかは、その作品のページに書いてあります。"
         "You may copy, redistribute, adapt, translate, mine and republish everything here, commercially or not, without asking and without payment. For almost every work, attribution is requested rather than required. For the works whose rightsholder publishes them under a Creative Commons Attribution licence, attribution is a condition of that licence. Each work's own page says which of the two it is.")]
    ;; the grant as this release states it, rather than as this document
    ;; describes it: a served page that disagreed with the signed manifest
    ;; would be the one thing a rights statement may not do
    [:dl {:class "facts"}
     [:dt (bilingual "底本の権利状態" "Underlying works")]
     ;; one <dd> per standing: <dl> allows several for one <dt>, and a corpus
     ;; with two rights regimes has to show both under the one term
     (map (fn [standing]
            [:dd [:a {:href (rights/works-uri standing)} standing]])
          standings)
     [:dt (bilingual "符号化のライセンス" "Encoding licence")]
     [:dd [:a {:href (rights/licence-uri encoding)} encoding]]
     [:dt (bilingual "この文書" "This statement")]
     [:dd [:a {:href statement_url} statement_url]]]
    [:p (bilingual
         "上の各項目は、この版のマニフェストに書かれている値です。底本の権利状態は作品ごとに記録しているので、この版に含まれるものをすべて挙げています。"
         "Every row above is read from this release's own manifest. The standing of the underlying works is recorded per work, so the row lists each one the release publishes under.")]

    [:h2 (bilingual "権利の二つの層" "Two rights layers")]
    [:p (bilingual
         "底本のほとんどは、著作権の存続期間が満了して権利が消滅した青空文庫の作品です。Soranoha はこれらについて著作権その他の権利を有しておらず、主張もしません。青空文庫の「収録ファイルの取り扱い規準」は、著作権の消滅した作品のファイルを、有償無償を問わず自由に複製・再頒布・翻案してよいとしています。"
         "Most of the underlying texts are Aozora Bunko works whose copyright term has expired and whose rights have therefore lapsed. Soranoha neither holds nor claims any right in them. Aozora Bunko's handling rules allow files for expired works to be copied, redistributed and adapted freely, whether for payment or not.")]
    ;; the second regime. Aozora Bunko has no licence column, so these works
    ;; are identified from the notice their rightsholder wrote into the
    ;; colophon, and that notice is what the standing beside each work records.
    [:p (bilingual
         "残りは、著作権が存続しており、権利者がクリエイティブ・コモンズ 表示ライセンス（CC BY）の下で青空文庫に公開している作品です。これらも同じく自由に利用できますが、クレジットの表示は権利者が付した条件です。どのバージョンのライセンスによるかは、作品ページと TEI ファイルの双方に記載しています。Soranoha が公開するのは、表示のみを条件とするライセンスの作品に限られます。非営利、改変禁止、継承のいずれかを課すライセンスの作品は収録していません。"
         "The rest are works whose copyright subsists and whose rightsholder publishes them on Aozora Bunko under a Creative Commons Attribution licence. They may be used just as freely, but attribution is a condition their rightsholder set rather than a request. Each work's page and its TEI header name the version of the licence it is under. Soranoha publishes such a work only where attribution is the sole condition: a work under a licence adding NonCommercial, NoDerivatives or ShareAlike is not in the corpus.")]
    [:p (bilingual
         "Soranoha 自身の符号化（TEI マークアップ、プレーンテキストと Markdown への投影、検証レポート、目録、リリースマニフェスト）は、CC0-1.0 によりパブリックドメインで提供します。符号化に著作権および関連する権利（データベースに関する権利を含む）が生じる範囲では、これを放棄します。"
         "Soranoha's own encoding (the TEI markup, the plaintext and Markdown projections, the validation reports, the catalog and the release manifests) is dedicated to the public domain under CC0-1.0. Where that encoding attracts copyright or a database right at all, those rights are waived.")]
    ;; the two requests are not conditions on the grant, so they belong on
    ;; the served page as what they are: a redistributor who reads only this
    ;; page would otherwise never learn that anything was asked
    [:p (bilingual
         "青空文庫は義務ではなく二つのことを求めています。作品名・著者・底本・入力者・校正者を記したクレジット表記を削らないこと、そして底本や表記を変更した場合はその記録を添えることです。Soranoha はどちらも行っており、クレジットは各 TEI ファイルの back と各作品ページに、変更の記録は検証レポートと底本との差異に残しています。再頒布される方にも同じ扱いをお願いします。"
         "Aozora Bunko asks two things without requiring them: that the credit block naming the work, its author, its source edition and the people who keyed and proofread it not be removed, and that a change of source edition or notation come with a record of what changed. Soranoha does both: we keep the credit block in every TEI file's back matter and on every work page, and we publish the record as the validation report and the divergences from the source that accompany each work. We ask the same of anyone redistributing these files.")]

    ;; the split a redistributor actually needs: the corpus and the program
    ;; that made it are under different terms, and the page a manifest points
    ;; at is where someone checks before redistributing
    [:p (bilingual
         "公開されたコーパスを再頒布しても、ツールチェーンのライセンス上の義務は生じません。ツールチェーン自体を再頒布する場合には生じます。自作のソースコードは Apache-2.0、分岐した解析器クレートは上流から承継した MIT OR Apache-2.0 です。"
         "Redistributing the published corpus does not carry the toolchain's obligations. Redistributing the toolchain does: the locally authored source is Apache-2.0, and the forked parser crates carry MIT OR Apache-2.0 as an inherited obligation.")]

    [:h2 (bilingual "公開作品に権利をお持ちの方へ" "If you hold rights in a published work")]
    [:p (bilingual
         "Soranoha が公開するのは、著作権が消滅したと判断した作品と、権利者が表示のみを条件とするライセンスの下で公開している作品だけです。ただし、この規模のコーパスであれば、いずれどれか一つは判断を誤ります。"
         "Soranoha publishes only works its assessment finds to be out of copyright, and works their rightsholder publishes under a licence whose sole condition is attribution. Over a corpus this size we will eventually get one wrong.")]
    [:p (bilingual
         (str "作品識別子（URL に見える " example-identifier " の形）または青空文庫の図書カードと、権利主張の根拠を添えて、"
              orcid-url " に記載の連絡先までご連絡ください。正式な法的通知の形式による必要はありません。")
         (str "Write to the address on " orcid-url ", naming the work identifier (the "
              example-identifier " form, visible in the URL) or the Aozora Bunko card, and the "
              "basis of the claim. A claim does not need to be a formal legal notice to be acted on."))]
    ;; the event's own release is the one that removes the work, so a
    ;; rights holder is not waiting on a corpus release that may be weeks
    ;; away; `transact/successor-for-event` builds that manifest
    [:p (bilingual
         "公開を取り下げる場合は、その理由を記したガバナンスイベントを公開します。イベントの公開それ自体が一つの版で、その版では作品を works[] と目録から外し、根拠となるイベントとともに withdrawn[] に記録します。次の定期の版を待たずに公開します。署名済みの履歴は追記しかできないので、過去の版はそのまま残ります。"
         "To withdraw a work, Soranoha publishes a governance event stating why. Publishing it is itself a release. That release omits the work from works[] and from the catalog, and records it in withdrawn[] together with the event the removal rests on. Soranoha publishes such a release as soon as it is needed, rather than at the next corpus release. Nothing can be removed from the signed history, so earlier releases keep the work exactly as they published it.")]

    [:h2 (bilingual "無保証" "No warranty")]
    [:p (bilingual
         "Soranoha が公開するのは、検証レポートと底本との差異を添えた翻刻であって、翻刻の正確性を保証するものではありません。コーパスは現状有姿で提供します。法律・医療・安全に関わる用途では、各作品の sourceDesc に記録された底本に照らして独自に検証することなく依拠しないでください。"
         "Soranoha publishes transcriptions with their validation reports and their recorded divergences from the source, not a guarantee of fidelity. The corpus is provided as-is. Do not rely on it for a legal, medical or safety purpose without independent verification against the source edition recorded in each work's sourceDesc.")]]))

(def ^:private example-work
  "The work the how-to-cite page is worked through. A literal rather than a
  lookup into the release: the page has to render the same example on every
  activation, and a real catalog entry would change when that work's record
  is corrected. The facts are 蜘蛛の糸's own, so the example matches the page
  a reader reaches by following it."
  {"slug" "000092_000879"
   "title" "蜘蛛の糸"
   "title_reading" "くものいと"
   "orthographic_style" "新字新仮名"
   "source_editions" [{"title" "芥川龍之介全集2"
                       "publisher" "ちくま文庫、筑摩書房"
                       "first_edition_year" "1986（昭和61）年10月28日"}]
   "contributors" [{"person_id" "000879"
                    "family_name" "芥川" "given_name" "竜之介"
                    "family_name_romaji" "Akutagawa" "given_name_romaji" "Ryunosuke"
                    "relation_to_work" "著者"}]})

(defn- citation-page [document {:keys [head-hex doi] :as release}]
  (generated-page
   document
   "Citing Soranoha"
   [[:p (bilingual
         "すべて CC0 なので、引用はライセンス上の条件ではなく学術上の慣行です。版を明示して引用してください。"
         "Everything here is CC0, so citation is a scholarly norm rather than a licence condition. Cite the release by name.")]
    ;; the templates below are the document's; these are this release's, which
    ;; is the one thing a reader cannot fill in from a repository checkout.
    ;; The head is abbreviated the way every other citation on the site
    ;; abbreviates it and the way `docs/citation.md` says a bibliography
    ;; should carry it: twelve hex digits and nothing after them, because the
    ;; short name resolves and a trailing ellipsis would make a reader who
    ;; typed what they saw look up a release that does not exist
    [:p [:code {:class "citation"}
         (str site-name " Aozora TEI Corpus. Release " (subs head-hex 0 12)
              (when doi (str ". https://doi.org/" doi)))]]
    [:p [:code {:class "citation" :lang "ja"}
         (citation/rendered release example-work)]]
    [:p [:code {:class "citation"}
         (citation/rendered-en release example-work)]]
    [:p (bilingual
         "上はこの版のもので、作品の例は「蜘蛛の糸」です。各作品のページにも、その作品に合わせた同じ形が載っています。それぞれの項目の意味は以下のとおりです。"
         "Those are this release, with 蜘蛛の糸 as the worked example; every work page carries the same forms for its own work. Each component is explained below.")]
    [:p [:a {:href "/catalog.json"} "/catalog.json"] " · "
     [:a {:href (str "/releases/" head-hex ".json")} (str "/releases/" head-hex ".json")]]]))

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

(defn people
  "Every person in the release, keyed by person_id.

  `:works` is every work they contributed to and `:by-relation` is the same
  works split by the relation the catalog records; both hold a work once and
  keep catalog order. A catalog can name one person twice for one work,
  holding two relations or repeating one, and neither is a reason to list the
  work twice: the works of a person are works, not contributor rows.

  One definition because the author pages and the bulk selections describe the
  same release from the same catalog, and a person's works have to mean the
  same thing on the page and in the archive it links to. Contributors of a
  work are reached consecutively here, so the last entry in a list is enough
  to recognise a repeat."
  [works]
  (let [add (fn [ws work] (if (identical? (peek ws) work) ws (conj (or ws []) work)))]
    (reduce
     (fn [acc work]
       (reduce (fn [acc contributor]
                 (let [id (get contributor "person_id")]
                   (-> acc
                       (assoc-in [id :person] (get-in acc [id :person] contributor))
                       (update-in [id :works] add work)
                       (update-in [id :by-relation (get contributor "relation_to_work")]
                                  add work))))
               acc
               (get work "contributors")))
     (sorted-map)
     works)))

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
  already written.

  `manifest-seq` is a function of no arguments returning the chain newest
  first as `[hex manifest]`, and it is a function for the same reason `tei`
  is. A published manifest is about nine megabytes at the current corpus, and
  three places here read the chain, so a realized sequence would be held for
  the whole export and would cost the chain's length times that. Each caller
  asks for its own sequence, walks it once and lets it go, which bounds the
  chain's contribution to what one manifest costs."
  [{:keys [head-hex manifest-seq catalog events tei doi]}]
  (let [release {:head-hex head-hex :doi doi}
        head (second (first (manifest-seq)))
        works (get catalog "works")
        by-slug (into {} (map (juxt #(get % "slug") identity)) works)
        withdrawn (get head "withdrawn")
        people-index (people works)
        reading-key (fn [work] [(or (some-> (get work "title_reading") hiragana) "")
                                (get work "slug")])
        page (fn [path ^String content] [path (utf8 content)])
        by-kana (group-by kana-row-key works)
        by-ndc (group-by ndc-class-key works)
        entries (document-entries)
        by-route (into {} (map (juxt :route identity)) docs/generated)]
    (concat
     [(page "style.css" stylesheet)
      (page "search.js" search-js)
      (page "copy.js" copy-js)
      (page "search-index.json" (search-index head-hex works))
      (page "index.html" (landing head-hex works (count withdrawn)))
      (page "rights.html" (rights-page (by-route "rights")
                                       (works-standings head)
                                       (get head "rights")))
      (page "citation.html" (citation-page (by-route "citation") release))
      (page "history.html" (history-page manifest-seq events))

      (page "authors/index.html"
            (author-index (mapv (fn [[id {:keys [person works]}]]
                                  {:person-id id
                                   :label (person-label person)
                                   :count* (count works)})
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

     ;; the project's own documentation, rendered from the repository files it
     ;; is reviewed in, and the files those documents send a reader to
     (map (fn [entry] (page (str (:route entry) ".html") (document-page entry)))
          entries)

     (map (fn [{:keys [route path]}] [route (docs/read-bytes path)]) docs/verbatim)

     (map (fn [[id {:keys [person by-relation]}]]
            (page (str "authors/" id ".html")
                  (author-page {:label (person-label person)
                                :romaji (person-name-romaji person)
                                :person-id id
                                :person person}
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
                 [(page (str "works/" slug "/index.html") (work-page release work))
                  ;; the two records a reference manager imports. Files rather
                  ;; than a service because there is no runtime here, and they
                  ;; cost about a kilobyte each against a corpus whose texts
                  ;; are three orders of magnitude larger.
                  (page (str "works/" slug "/citation.json")
                        (json/write-deterministic-json-str
                         [(citation/csl-json-value release work)]))
                  (page (str "works/" slug "/citation.bib")
                        (citation/biblatex release work))
                  (page (str "works/" slug "/read.html")
                        (reading-page
                         release work
                         (reading/render (String. ^bytes (tei slug) "UTF-8"))))]))
             works)

     ;; then the withdrawn works, which no longer have one
     (let [gone (remove #(contains? by-slug (get % "slug")) withdrawn)
           last-release (delay (last-release-of manifest-seq (map #(get % "slug") gone)))]
       (map (fn [{:strs [slug event]}]
              (let [entry (->> (get (get events (verify/id->hex event)) "entries")
                               (filter #(= slug (get % "slug")))
                               first)]
                (page (str "works/" slug "/index.html")
                      (withdrawn-page slug entry (get @last-release slug)))))
            gone)))))
