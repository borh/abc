(ns soranoha.za.markdown
  "The Markdown subset the project's own reader-facing documents are written
  in, rendered to the browse layer's hiccup vectors.

  A subset rather than CommonMark, and hand-written rather than a dependency,
  for the same reason `soranoha.za.html` is: the exporter must produce these
  bytes identically on every activation, and a Markdown library brings a
  parser, its own escaping rules and its own version to a tree that is checked
  byte for byte. The subset covers what the documents actually use: ATX
  headings, paragraphs, fenced code, pipe tables, bullet lists, blockquotes,
  and inline code, links, autolinks, bold and italic. There are no images, no
  ordered lists, no raw HTML blocks and no reference links in any served
  document.

  An unterminated code fence throws rather than being tolerated, because the
  alternative is a page whose remainder is swallowed into a code block.

  How a link target becomes a URL is the caller's decision, passed in as
  `link`. When it answers with a string instead of attributes, the link is
  rendered as its label followed by that string in code, which is how a
  reference to a repository file the site does not serve reaches the reader:
  named, and not as an anchor to nothing."
  (:require [clojure.string :as string]))

(defn slug
  "The fragment identifier a heading gets, in the form the documents already
  write when they link to one: lowercased, punctuation dropped, runs of
  whitespace turned into hyphens. Characters outside ASCII are kept, so a
  Japanese heading gets a usable identifier rather than an empty one."
  [text]
  (-> text
      string/lower-case
      (string/replace #"[^\p{L}\p{N}\p{M}\s_-]" "")
      string/trim
      (string/replace #"\s+" "-")))

(defn- heading-line [line]
  (when-let [[_ hashes text] (re-matches #"(#{1,6}) (.*)" line)]
    [(count hashes) text]))

(defn- fence-line? [line]
  (string/starts-with? line "```"))

(defn- bullet-line [line]
  (second (re-matches #"[-*] (.*)" line)))

(defn- quote-line [line]
  (cond
    (= line ">") ""
    :else (second (re-matches #"> (.*)" line))))

(defn- table-row? [line]
  (string/starts-with? line "|"))

(defn- table-delimiter? [line]
  (and (table-row? line)
       (re-matches #"\|[\s:|-]+" line)))

(defn- table-cells [line]
  (->> (string/split (string/replace line #"^\||\|$" "") #"\|" -1)
       (mapv string/trim)))

;; ---------------------------------------------------------------------------
;; Inline
;; ---------------------------------------------------------------------------

(def ^:private cjk-boundary
  "Ranges whose characters do not take a space when a hard-wrapped line is
  rejoined. Japanese prose wraps without spaces, so joining two such lines with
  one would insert a space the author did not write."
  [[0x3000 0x303F] [0x3040 0x309F] [0x30A0 0x30FF] [0x4E00 0x9FFF]
   [0xFF00 0xFFEF] [0x2E80 0x2EFF] [0x3400 0x4DBF]])

(defn- cjk? [^Character c]
  (when c
    (let [code (int c)]
      (boolean (some (fn [[lo hi]] (<= lo code hi)) cjk-boundary)))))

(defn- join-wrapped
  "Rejoin the hard-wrapped lines of one paragraph. A space goes between them
  unless both sides of the break are CJK, where the author's line break carries
  no space."
  [lines]
  (reduce (fn [acc line]
            (if (string/blank? acc)
              line
              (let [left (last acc)
                    right (first line)]
                (str acc (when-not (and (cjk? left) (cjk? right)) " ") line))))
          ""
          lines))

(defn- inline-pattern []
  ;; Code spans first: their content is literal, so nothing inside one may be
  ;; reinterpreted as a link or as emphasis.
  #"`([^`]+)`|\[([^\]]+)\]\(([^)]+)\)|<(https?://[^>\s]+)>|\*\*([^*]+)\*\*|\*([^*]+)\*")

(defn- inline
  "Parse one run of text into hiccup. `link` resolves a Markdown link target to
  either a hiccup attribute map or a string to show instead of an anchor."
  [text link]
  (let [matcher (re-matcher (inline-pattern) text)]
    (loop [cursor 0 out []]
      (if (.find matcher)
        (let [start (.start matcher)
              end (.end matcher)
              before (subs text cursor start)
              [_ code label target autolink bold italic] (re-groups matcher)
              ;; A code span's content is literal; everything else may nest,
              ;; and the documents do nest, as in a link whose label is a path
              ;; in backticks. Recursion terminates because the inner text is
              ;; strictly shorter than the match that produced it.
              node (cond
                     code [:code code]
                     label (let [resolved (link target)
                                 shown (inline label link)]
                             (cond
                               (not (string? resolved)) [:a resolved shown]
                               ;; the label is already the path, as it is
                               ;; whenever a document names a file in
                               ;; backticks; saying it twice helps nobody
                               (= (string/replace label "`" "") resolved)
                               [:code resolved]
                               :else (list shown " (" [:code resolved] ")")))
                     autolink [:a {:href autolink} autolink]
                     bold (into [:strong] (inline bold link))
                     italic (into [:em] (inline italic link)))]
          (recur end (cond-> out
                       (seq before) (conj before)
                       true (conj node))))
        (let [tail (subs text cursor)]
          ;; A seq, never a vector: `soranoha.za.html` reads a vector as an
          ;; element and would take the first node as the tag name.
          (seq (cond-> out (seq tail) (conj tail))))))))

;; ---------------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------------

(defn- paragraph-line? [line]
  (and (not (string/blank? line))
       (not (heading-line line))
       (not (fence-line? line))
       (not (table-row? line))
       (not (bullet-line line))
       (not (quote-line line))))

(declare blocks)

(defn- take-fence [lines source]
  (let [info (string/trim (subs (first lines) 3))
        body (take-while (complement fence-line?) (rest lines))
        rest* (drop (inc (count body)) (rest lines))]
    (when (= (count body) (count (rest lines)))
      (throw (ex-info "unterminated code fence"
                      {:reason :unterminated-code-fence :source source})))
    [[:pre [:code (cond-> {} (seq info) (assoc :class (str "language-" info)))
            (str (string/join "\n" body) "\n")]]
     rest*]))

(defn- take-table [lines link]
  (let [header (first lines)
        body (take-while table-row? (drop 2 lines))]
    [[:table
      [:thead [:tr (for [cell (table-cells header)] [:th (inline cell link)])]]
      [:tbody
       (for [row body]
         [:tr (for [cell (table-cells row)] [:td (inline cell link)])])]]
     (drop (+ 2 (count body)) lines)]))

(defn- take-list
  "A bullet and the hard-wrapped continuation lines that belong to it.

  The documents wrap list items without indenting the continuation, so a
  continuation is any following line that starts no other block. Reading only
  the bullet's own line would split inline markup that spans the wrap, leaving
  a stray `**` or backtick on the page."
  [lines link]
  (loop [lines (vec lines) items []]
    (if-let [first-line (and (seq lines) (bullet-line (first lines)))]
      (let [continuation (take-while paragraph-line? (rest lines))]
        (recur (drop (inc (count continuation)) lines)
               (conj items (join-wrapped (cons first-line continuation)))))
      [(into [:ul] (for [item items] [:li (inline item link)]))
       lines])))

(defn- take-quote [lines link source]
  (let [quoted (take-while quote-line lines)]
    [(into [:blockquote] (blocks (mapv quote-line quoted) link source))
     (drop (count quoted) lines)]))

(defn- take-paragraph
  "Always consumes at least its first line. Without that, a block shape that
  reached here without matching any other rule would consume nothing and the
  loop would not terminate."
  [lines link]
  (let [text (cons (first lines) (take-while paragraph-line? (rest lines)))]
    [[:p (inline (join-wrapped text) link)]
     (drop (count text) lines)]))

(defn- blocks [lines link source]
  (loop [lines (vec lines) out []]
    (if-let [line (first lines)]
      (cond
        (string/blank? line) (recur (rest lines) out)

        (heading-line line)
        (let [[level text] (heading-line line)]
          (recur (rest lines)
                 ;; The id is what makes a link to a section resolve. Both the
                 ;; documents' own cross-references and a reader's bookmark
                 ;; depend on it, so it is derived from the heading rather
                 ;; than counted, and survives a section moving.
                 (conj out (into [(keyword (str "h" level)) {:id (slug text)}]
                                 (inline text link)))))

        (fence-line? line)
        (let [[node rest*] (take-fence lines source)]
          (recur rest* (conj out node)))

        (and (table-row? line)
             (table-delimiter? (str (second lines))))
        (let [[node rest*] (take-table lines link)]
          (recur rest* (conj out node)))

        (bullet-line line)
        (let [[node rest*] (take-list lines link)]
          (recur rest* (conj out node)))

        (quote-line line)
        (let [[node rest*] (take-quote lines link source)]
          (recur rest* (conj out node)))

        :else
        (let [[node rest*] (take-paragraph lines link)]
          (recur rest* (conj out node))))
      out)))

(defn render
  "Render `text` as hiccup. `link` resolves a Markdown link target; `source` is
  the document's repository path, used only in error data.

  Rendering starts after the document's own leading `# Title`, because the
  page renders that title as its `h1` and a document has one title rather than
  two. Heading levels below it are kept, so a document's `##` sections are the
  page's `h2` sections.

  `from` names a heading by its identifier and starts there instead, which is
  how a page that already states a document's opening in its own words carries
  the rest of that document without repeating it. A `from` that names no
  heading throws rather than silently rendering nothing."
  [{:keys [text link source from]}]
  (let [lines (string/split-lines text)
        after-title (rest (drop-while (complement heading-line) lines))]
    (if from
      (let [tail (drop-while #(not= from (some-> (heading-line %) second slug))
                             after-title)]
        (when (empty? tail)
          (throw (ex-info "document has no heading to start from"
                          {:reason :unknown-start-heading :source source :from from})))
        (blocks tail link source))
      (blocks after-title link source))))

(defn title
  "The document's first ATX heading, which is what the page is called."
  [text]
  (or (some #(second (heading-line %)) (string/split-lines text))
      (throw (ex-info "document has no heading to use as its title"
                      {:reason :document-without-title}))))
