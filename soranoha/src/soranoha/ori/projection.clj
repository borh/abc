(ns soranoha.ori.projection
  (:require [clojure.string :as string]
            [soranoha.annotations.view :as view])
  (:import [org.w3c.dom Element Node]))

(defn- paragraph [^Node node]
  (when node
    (if (= "p" (view/local-name node)) node
        (recur (.getParentNode node)))))

(defn- indent [^Element element]
  (when element
    (when-let [[_ width] (or (re-find #"(?:^| )first-line-indent\((\d+)\)(?: |$)" (.getAttribute element "rend"))
                             (re-find #"(?:^|;)\s*text-indent:\s*(\d+)em(?:;|$)" (.getAttribute element "style")))]
      (apply str (repeat (Long/parseLong width) "　")))))

(defn plaintext
  "Project body text, restoring encoded first-line indentation for plain display."
  [reading]
  (let [out (StringBuilder.)]
    (reduce (fn [seen segment]
              (let [p (paragraph (:view/node segment))]
                (when (and p (not (contains? seen p)))
                  (.append out ^String (or (indent p) "")))
                (.append out ^String (:view/text segment))
                (cond-> seen p (conj p))))
            #{} (:view/segments reading))
    (str out)))

(defn- html-text [text]
  (string/escape text {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))

(defn- markdown-text [text]
  (string/replace (html-text text) #"([\\`*_{}\[\]()#+.!|>~-])" (fn [[_ token]] (str "\\" token))))

(def ^:private dot-styles
  {"傍点" "filled sesame"
   "白ゴマ傍点" "open sesame"
   "丸傍点" "filled circle"
   "白丸傍点" "open circle"
   "白三角傍点" "open triangle"
   "黒三角傍点" "filled triangle"
   "二重丸傍点" "'◎'"
   "蛇の目傍点" "'◉'"
   "ばつ傍点" "'×'"})

(defn- rendition-style [^Element node]
  (when-not (.hasAttribute node "style")
    (let [rend (.getAttribute node "rend")]
      (or ({"text-combine-upright" "text-combine-upright: all"
            "baseline-lowered" "vertical-align: sub; font-size: inherit"
            "exponent" "vertical-align: super; font-size: inherit"
            "yokogumi horizontal" "writing-mode: horizontal-tb"
            "keigakomi" "border: 1px solid"} rend)
          (when-let [[_ family shape side] (re-matches #"(bouten|bosen) ([^ ]+) (right|left|both)" rend)]
            (case family
              "bouten" (when-let [style (dot-styles shape)]
                         (when-let [position ({"right" "over right" "left" "under left"} side)]
                           (str "text-emphasis-style: " style "; text-emphasis-position: " position)))
              "bosen" (when-let [style ({"傍線" "solid" "二重傍線" "double" "波線" "wavy"} shape)]
                        (str "text-decoration-line: " ({"right" "underline" "left" "overline"
                                                        "both" "underline overline"} side)
                             "; text-decoration-style: " style))))))))

(declare inline)

(defn- inline-children [node html?]
  (apply str (map #(inline % html?) (view/selected-children node))))

(defn- inline [^Node node html?]
  (let [tag (view/local-name node)
        content #(inline-children node html?)]
    (cond
      (= Node/TEXT_NODE (.getNodeType node))
      (markdown-text (.getNodeValue node))

      (= "ruby" tag)
      (str "<ruby>"
           (apply str (for [child (view/children node)
                            :let [name (view/local-name child)]
                            :when (#{"rb" "rt"} name)]
                        (str "<" name ">" (inline-children child true) "</" name ">")))
           "</ruby>")

      (#{"lb" "pb" "cb"} tag) (if html? "<br>" "  \n")
      (= "g" tag) (if (empty? (.getTextContent node)) "�" (content))
      (= "hi" tag) (let [rend (.getAttribute ^Element node "rend")]
                     (case rend
                       "bold" (str "<strong>" (inline-children node true) "</strong>")
                       "italic" (str "<em>" (inline-children node true) "</em>")
                       (if-let [style (rendition-style node)]
                         (str "<span data-tei-rend=\"" (html-text rend) "\" style=\"" (html-text style) "\">"
                              (inline-children node true) "</span>")
                         (content))))
      (#{Node/COMMENT_NODE Node/PROCESSING_INSTRUCTION_NODE} (.getNodeType node)) ""
      :else (content))))

(def ^:private block-tags #{"p" "head" "l" "ab" "item"})

(defn- blocks [node]
  (cond
    (block-tags (view/local-name node)) [node]
    (#{"note" "fw" "figDesc"} (view/local-name node)) []
    :else (into [] (mapcat blocks) (view/selected-children node))))

(defn markdown
  "Horizontal CommonMark with HTML ruby and generated inline rendition CSS."
  [reading]
  (let [^org.w3c.dom.Document document (:view/document reading)
        body (.item (.getElementsByTagNameNS document view/tei-namespace "body") 0)]
    (string/join "\n\n"
                 (map (fn [node]
                        (str (when (= "head" (view/local-name node)) "## ")
                             (inline-children node false)))
                      (blocks body)))))

(def ^:private structural-tags
  #{"TEI" "text" "body" "div" "floatingText" "p" "s" "seg" "ab" "l" "lg" "list" "item"
    "anchor" "rb" "rt" "lem" "corr" "sic" "orig" "reg" "abbr" "expan" "quote" "figure" "figDesc"})

(defn- report-children [profile node]
  (if (and (= :projection/markdown profile) (= "ruby" (view/local-name node)))
    (filterv #(#{"rb" "rt"} (view/local-name %)) (view/children node))
    (view/selected-children node)))

(defn- disposition [profile ^Element node]
  (let [tag (view/local-name node)]
    (cond
      (and (= "g" tag) (empty? (.getTextContent node))) :projection/unresolved
      (#{"note" "fw" "figDesc"} tag) :projection/omitted
      (= "graphic" tag) (if (= :projection/plaintext profile) :projection/omitted :projection/unsupported)
      (= "hi" tag) (if (or (= :projection/plaintext profile)
                           (and (not (.hasAttribute node "style"))
                                (#{"bold" "italic"} (.getAttribute node "rend")))
                           (rendition-style node))
                     :projection/transformed :projection/unsupported)
      (#{"ruby" "choice" "app" "head" "lb"} tag) :projection/transformed
      (#{"pb" "cb"} tag) (if (= :projection/plaintext profile) :projection/transformed :projection/omitted)
      (or (= "g" tag) (structural-tags tag)) :projection/represented
      :else :projection/unsupported)))

(defn- node-outcomes [profile ^Element node]
  (let [tag (view/local-name node)]
    (cond-> [[(or tag (.getNodeName node)) (disposition profile node)]]
      (and (= "ruby" tag) (= :projection/plaintext profile))
      (into (map (fn [_] ["ruby-reading" :projection/omitted])
                 (filter #(= "rt" (view/local-name %)) (view/children node))))

      (and (= "rt" tag) (.hasAttribute node "place"))
      (conj ["ruby-placement" :projection/unsupported])

      (and (not= "hi" tag) (or (.hasAttribute node "rend") (.hasAttribute node "style")))
      (conj ["layout" :projection/omitted])

      (#{"choice" "app"} tag)
      (into (map (fn [_] ["alternative-reading" :projection/omitted])
                 (remove (set (view/selected-children node))
                         (filter #(= Node/ELEMENT_NODE (.getNodeType ^Node %)) (view/children node))))))))

(defn report [profile reading]
  (when-not (#{:projection/plaintext :projection/markdown} profile)
    (throw (ex-info "Unknown projection profile" {:profile profile})))
  (let [^org.w3c.dom.Document document (:view/document reading)
        body (.item (.getElementsByTagNameNS document view/tei-namespace "body") 0)
        descend (partial report-children profile)
        outcomes (mapcat (fn [^Node node]
                           (when (= Node/ELEMENT_NODE (.getNodeType node))
                             (node-outcomes profile node)))
                         (tree-seq #(seq (descend %)) descend body))
        external-content (for [problem (:view/problems reading)
                               :when (= "content-outside-primary-input" (get-in problem [:view/evidence "code"]))]
                           ["external-content" :projection/unresolved])
        counts (frequencies (concat outcomes external-content))]
    {"profile" (str (name profile) (if (= :projection/markdown profile) "/2" "/1"))
     "view" (:view/id reading)
     "status" (if (some (fn [[[_ disposition] _]]
                          (#{:projection/unresolved :projection/unsupported} disposition)) counts)
                "limited" "complete-for-profile")
     "counts" (mapv (fn [[[family disposition] count]]
                      {"family" family "disposition" (name disposition) "count" count})
                    (sort-by key counts))}))
