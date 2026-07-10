(ns abc.tools.diagram.presentation-svg
  (:require [babashka.process :as process]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files]
           [java.util Base64 Locale]))

(def allowed-colors
  #{"#000000" "#F5F7FA" "#A7B0BE" "#48CAE4" "#F2B84B" "#7BC47F"
    "none" "transparent"})

(def svg-namespace "http://www.w3.org/2000/svg")

(def ^:private font-stylesheet-pattern
  #"^@font-face\{font-family:'Noto Sans CJK JP';font-style:normal;font-weight:400 700;src:url\(data:font/woff2;base64,([A-Za-z0-9+/]+={0,2})\) format\('woff2'\);\}text\{font-family:'Noto Sans CJK JP',sans-serif;\}$")

(def ^:private color-attributes
  ["fill" "stroke" "color" "stop-color" "flood-color" "lighting-color"
   "solid-color" "viewport-fill"])

(defn- parse-xml [label value]
  (try
    ;; Graphviz emits a remote SVG 1.1 DOCTYPE. The figure is self-contained;
    ;; remove it before parsing so validation never performs external lookup.
    (xml/parse-str (str/replace value #"(?is)<!DOCTYPE[^>]*>" ""))
    (catch Exception cause
      (throw (ex-info (str label " is not valid Graphviz SVG") {} cause)))))

(defn- parse-view-box [root]
  (let [parts (mapv parse-double
                    (str/split (get-in root [:attrs :viewBox]) #"\s+"))]
    (when-not (= 4 (count parts))
      (throw (ex-info "Graphviz SVG has no four-number viewBox"
                      {:viewBox (get-in root [:attrs :viewBox])})))
    parts))

(defn- element [tag attrs & content]
  (apply xml/element (xml/qname svg-namespace (name tag)) attrs content))

(defn- font-style [woff2-bytes]
  (str "@font-face{font-family:'Noto Sans CJK JP';font-style:normal;"
       "font-weight:400 700;src:url(data:font/woff2;base64,"
       (.encodeToString (Base64/getEncoder) woff2-bytes)
       ") format('woff2');}text{font-family:'Noto Sans CJK JP',sans-serif;}"))

(defn- root-format [pattern & arguments]
  (String/format Locale/ROOT pattern (to-array arguments)))

(defn normalize-svg [graph raw-svg woff2-bytes]
  (let [raw-root (parse-xml "Graphviz SVG" raw-svg)
        [_ _ raw-width raw-height] (parse-view-box raw-root)
        graph-x 96.0
        graph-y 195.0
        graph-width 1728.0
        graph-height 745.0
        scale (min (/ graph-width raw-width) (/ graph-height raw-height))
        tx (+ graph-x (/ (- graph-width (* raw-width scale)) 2.0))
        ty (+ graph-y (/ (- graph-height (* raw-height scale)) 2.0))
        graph-content (:content raw-root)
        _ (when (< scale 1.0)
            (throw (ex-info "Graphviz SVG requires a fit scale below 1"
                            {:scale scale
                             :viewBox (get-in raw-root [:attrs :viewBox])})))
        root
        (element :svg
                 {:width "1920" :height "1080"
                  :viewBox "0 0 1920 1080"
                  :role "img"
                  :aria-labelledby "figure-title figure-description"}
                 (element :title {:id "figure-title"} (:title graph))
                 (element :desc {:id "figure-description"} (:description graph))
                 (element :style {} (font-style woff2-bytes))
                 (element :rect {:x "0" :y "0" :width "1920" :height "1080"
                                 :fill "#000000"})
                 (element :text {:x "96" :y "148" :fill "#F5F7FA"
                                 :font-size "52" :font-weight "700"
                                 :class "figure-title"}
                          (:title graph))
                 (element :text {:x "96" :y "184" :fill "#A7B0BE"
                                 :font-size "22" :font-weight "400"
                                 :class "figure-subtitle"}
                          (:subtitle graph))
                 (apply element :g
                        {:class "figure-graph"
                         :data-graph-scale (root-format "%.6f" scale)
                         :transform (root-format
                                     "translate(%.4f %.4f) scale(%.6f)"
                                     tx ty scale)}
                        graph-content)
                 (element :text {:x "96" :y "976" :fill "#A7B0BE"
                                 :font-size "16" :font-weight "400"
                                 :class "figure-citation"}
                          (:footer graph)))]
    (str (xml/emit-str root) "\n")))

(defn- elements [root]
  (tree-seq #(and (map? %) (seq (:content %))) :content root))

(defn- tag-name [node]
  (some-> (:tag node) name))

(defn- attr [node wanted]
  (some (fn [[key value]]
          (when (= wanted (name key)) value))
        (:attrs node)))

(defn- parse-number [value]
  (when (string? value)
    (try
      (let [number (parse-double value)]
        (when (and (some? number) (Double/isFinite number)) number))
      (catch NumberFormatException _ nil))))

(defn- font-size-problem [node]
  (when-let [raw-size (attr node "font-size")]
    (let [size (parse-number raw-size)
          class (attr node "class")]
      (cond
        (nil? size)
        (str "presentation text has invalid font-size: " raw-size)

        (= "figure-title" class)
        (when (< size 52.0) "figure title is smaller than 52 px")

        (= "figure-citation" class)
        (when (not= size 16.0) "figure citation must be exactly 16 px")

        (< size 22.0)
        (str "presentation label is smaller than 22 px: " raw-size)))))

(defn- bold? [value]
  (or (= "bold" value)
      (some-> (parse-number value) (>= 700.0))))

(defn- transform-arguments [value]
  (let [raw-parts (remove str/blank? (str/split value #"[\s,]+"))
        numbers (mapv parse-number raw-parts)]
    (when (every? some? numbers) numbers)))

(defn- descendant-transform-problems [node]
  (when-let [transform (attr node "transform")]
    (let [commands (re-seq #"([A-Za-z]+)\s*\(([^)]*)\)" transform)
          remainder (-> transform
                        (str/replace #"[A-Za-z]+\s*\([^)]*\)" "")
                        (str/replace #"[\s,]" ""))]
      (vec
       (concat
        (when (or (empty? commands) (not (str/blank? remainder)))
          [(str "unsupported descendant SVG transform: " transform)])
        (mapcat
         (fn [[_ command raw-arguments]]
           (let [arguments (transform-arguments raw-arguments)
                 expected-arities (case command
                                    "scale" #{1 2}
                                    "translate" #{1 2}
                                    "rotate" #{1 3}
                                    nil)]
             (cond
               (or (nil? expected-arities)
                   (nil? arguments)
                   (not (expected-arities (count arguments))))
               [(str "unsupported descendant SVG transform: " command
                     "(" raw-arguments ")")]

               (and (= "scale" command) (some #(< % 1.0) arguments))
               [(str "descendant SVG transform scale must be at least 1: "
                     command "(" raw-arguments ")")]

               :else [])))
         commands))))))

(defn- graph-node-problems [graph-root]
  (letfn [(walk [node inherited]
            (if-not (map? node)
              []
              (let [stroke (or (attr node "stroke") (:stroke inherited))
                    stroke-width (or (attr node "stroke-width")
                                     (:stroke-width inherited)
                                     "1")
                    font-size (or (attr node "font-size")
                                  (:font-size inherited)
                                  "16")
                    font-weight (or (attr node "font-weight")
                                    (:font-weight inherited)
                                    "normal")
                    text? (= "text" (tag-name node))
                    size (parse-number font-size)
                    width (parse-number stroke-width)
                    current {:stroke stroke
                             :stroke-width stroke-width
                             :font-size font-size
                             :font-weight font-weight}]
                (concat
                 (descendant-transform-problems node)
                 (when text?
                   (cond
                     (nil? size)
                     [(str "graph text has invalid font-size: " font-size)]

                     (and (bold? font-weight) (< size 34.0))
                     [(str "bold graph text is smaller than 34 px: " font-size)]

                     (< size 24.0)
                     [(str "graph text is smaller than 24 px: " font-size)]))
                 (when (and stroke (not= "none" stroke))
                   (cond
                     (nil? width)
                     [(str "graph stroke has invalid stroke-width: " stroke-width)]

                     (< width 2.0)
                     [(str "graph stroke is thinner than 2 px: " stroke-width)]))
                 (mapcat #(walk % current) (:content node))))))]
    (vec (mapcat #(walk % {}) (:content graph-root)))))

(defn- graph-transform-scale [graph-root]
  (some->> (attr graph-root "transform")
           (re-find #"scale\(\s*([^\s)]+)\s*\)")
           second
           parse-number))

(defn- canonical-font-stylesheet? [style-node]
  (and (empty? (:attrs style-node))
       (every? string? (:content style-node))
       (when-let [[_ encoded] (re-matches font-stylesheet-pattern
                                          (apply str (:content style-node)))]
         (try
           (pos? (alength (.decode (Base64/getDecoder) encoded)))
           (catch IllegalArgumentException _ false)))))

(defn- allowed-resource? [value]
  (and (string? value)
       (or (= svg-namespace value)
           (str/starts-with? value "data:")
           (str/starts-with? value "#"))))

(defn- url-resources [value]
  (map second
       (re-seq #"(?i)url\(\s*['\"]?([^'\"\s)]+)['\"]?\s*\)" value)))

(defn- resource-problems [nodes svg-string]
  (let [attribute-resources
        (for [node nodes
              [key value] (:attrs node)
              :when (or (#{"href" "src" "base"} (name key))
                        (and (string? value)
                             (re-find #"(?i)^[a-z][a-z0-9+.-]*:" value)))]
          value)
        url-values (mapcat url-resources
                           (concat [svg-string]
                                   (mapcat (comp vals :attrs) nodes)))
        external (distinct
                  (remove allowed-resource?
                          (concat attribute-resources url-values)))]
    (concat
     (when (re-find #"(?i)@import\b" svg-string)
       ["CSS @import is forbidden"])
     (for [resource external]
       (str "external SVG resource " resource)))))

(defn svg-problems [svg-string]
  (try
    (let [root (parse-xml "presentation SVG" svg-string)
          nodes (filter map? (elements root))
          first-rect (first (filter #(= "rect" (tag-name %)) nodes))
          styles (filter #(= "style" (tag-name %)) nodes)
          graph-roots (filter #(= "figure-graph" (attr % "class")) nodes)
          graph-root (when (= 1 (count graph-roots)) (first graph-roots))
          graph-scale (some-> graph-root (attr "data-graph-scale") parse-number)
          transform-scale (some-> graph-root graph-transform-scale)
          attribute-colors
          (for [node nodes
                key-name color-attributes
                :let [value (attr node key-name)]
                :when value]
            (if (str/starts-with? value "#")
              (str/upper-case value)
              value))
          hex-colors (set (map str/upper-case
                               (re-seq #"#[0-9A-Fa-f]{6}" svg-string)))
          font-problems (keep font-size-problem
                              (filter #(= "text" (tag-name %)) nodes))
          wrong-namespace
          (for [node nodes
                :when (and (:tag node)
                           (not= svg-namespace (xml/qname-uri (:tag node))))]
            (tag-name node))
          style-attributes (for [node nodes
                                 :when (some? (attr node "style"))]
                             (tag-name node))]
      (vec
       (concat
        (when-not (= "0 0 1920 1080" (attr root "viewBox"))
          ["presentation SVG must use viewBox 0 0 1920 1080"])
        (when-not (and (= "1920" (attr root "width"))
                       (= "1080" (attr root "height")))
          ["presentation SVG must use width 1920 and height 1080"])
        (when-not (= svg-namespace (xml/qname-uri (:tag root)))
          ["presentation SVG root is not in the SVG namespace"])
        (for [tag wrong-namespace]
          (str "presentation element is not in the SVG namespace: " tag))
        (when-not (some #(= "title" (tag-name %)) nodes)
          ["presentation SVG needs title"])
        (when-not (some #(= "desc" (tag-name %)) nodes)
          ["presentation SVG needs description"])
        (when-not (and first-rect
                       (= "0" (attr first-rect "x"))
                       (= "0" (attr first-rect "y"))
                       (= "1920" (attr first-rect "width"))
                       (= "1080" (attr first-rect "height"))
                       (= "#000000" (some-> (attr first-rect "fill")
                                            str/upper-case)))
          ["presentation SVG needs a full 1920x1080 black background"])
        (when-not (= 1 (count styles))
          ["presentation SVG needs exactly one embedded-font stylesheet"])
        (when (and (= 1 (count styles))
                   (not (and (some #{(first styles)} (:content root))
                             (canonical-font-stylesheet? (first styles)))))
          ["presentation SVG embedded-font stylesheet is not canonical"])
        (for [tag style-attributes]
          (str "style attributes are forbidden on SVG element " tag))
        (when-not (= 1 (count graph-roots))
          ["presentation SVG needs exactly one figure graph"])
        (when (and graph-root (nil? graph-scale))
          ["presentation graph has invalid data-graph-scale"])
        (when (and graph-scale (< graph-scale 1.0))
          ["presentation graph data-graph-scale must be at least 1"])
        (when (and graph-root
                   (or (nil? transform-scale)
                       (< transform-scale 1.0)
                       (and graph-scale (not= graph-scale transform-scale))))
          ["presentation graph transform scale is invalid or disagrees with data-graph-scale"])
        (when graph-root (graph-node-problems graph-root))
        (for [color attribute-colors :when (not (allowed-colors color))]
          (str "unapproved SVG color " color))
        (for [color hex-colors :when (not (allowed-colors color))]
          (str "unapproved SVG color " color))
        (resource-problems nodes svg-string)
        font-problems)))
    (catch Exception ex
      [(or (ex-message ex) "presentation SVG validation failed")])))

(defn- required-env [name]
  (or (System/getenv name)
      (throw (ex-info (str name " is required; run in the ABC Nix environment")
                      {:environment name}))))

(defn- run-command! [args]
  (let [{:keys [exit out err]} @(process/process args {:out :string :err :string})]
    (when-not (zero? exit)
      (throw (ex-info (str "presentation renderer command failed: "
                           (str/join " " args))
                      {:exit exit :stdout out :stderr err :command args})))
    out))

(defn- glyph-text [graph]
  (str/join "\n"
            (remove nil?
                    (concat [(:title graph) (:subtitle graph)
                             (:description graph) (:footer graph) "·"]
                            (map :label (:groups graph))
                            (mapcat (juxt :label :subtitle) (:nodes graph))
                            (mapcat (fn [node]
                                      (map :label (:coordinates node)))
                                    (:nodes graph))
                            (map :label (:edges graph))))))

(defn render-svg! [graph dot-string temp-dir]
  (let [dot-path (io/file temp-dir (str (name (:id graph)) ".dot"))
        raw-path (io/file temp-dir (str (name (:id graph)) ".raw.svg"))
        glyph-path (io/file temp-dir (str (name (:id graph)) ".glyphs.txt"))
        font-path (io/file temp-dir (str (name (:id graph)) ".woff2"))]
    (spit dot-path dot-string)
    (spit glyph-path (glyph-text graph))
    (run-command! [(required-env "ABC_GRAPHVIZ_DOT") "-Tsvg"
                   (.getPath dot-path) "-o" (.getPath raw-path)])
    (run-command! [(required-env "ABC_FONTTOOLS_SUBSET")
                   (required-env "ABC_PRESENTATION_FONT")
                   "--font-number=0"
                   (str "--text-file=" (.getPath glyph-path))
                   "--flavor=woff2"
                   "--layout-features=*"
                   (str "--output-file=" (.getPath font-path))])
    (let [svg (normalize-svg graph (slurp raw-path)
                             (Files/readAllBytes (.toPath font-path)))
          failures (svg-problems svg)]
      (when (seq failures)
        (throw (ex-info "rendered presentation SVG is invalid"
                        {:figure (:id graph) :problems failures})))
      svg)))
