(ns soranoha.annotations.view
  "Body reading and UTF-8 alignment for analysis of TEI transcription.
  DOM mappings are local to a parsed document; only text and policy identify a view."
  (:require [charred.api :as json]
            [clojure.string :as str]
            [soranoha.core.hash :as hash])
  (:import [java.io ByteArrayInputStream]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Document Node]))

(def tei-namespace "http://www.tei-c.org/ns/1.0")
(def xml-namespace "http://www.w3.org/XML/1998/namespace")

(defn read-document ^Document [^String tei]
  (let [factory (doto (DocumentBuilderFactory/newInstance)
                  (.setNamespaceAware true)
                  (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
                  (.setFeature "http://xml.org/sax/features/external-general-entities" false)
                  (.setFeature "http://xml.org/sax/features/external-parameter-entities" false)
                  (.setXIncludeAware false)
                  (.setExpandEntityReferences false))
        doc (.parse (.newDocumentBuilder factory)
                    (ByteArrayInputStream. (.getBytes tei "UTF-8")))]
    (when-not (and (= tei-namespace (.getNamespaceURI (.getDocumentElement doc)))
                   (= "TEI" (.getLocalName (.getDocumentElement doc))))
      (throw (ex-info "Expected a TEI document" {})))
    doc))

(defn children [^Node node]
  (let [nodes (.getChildNodes node)]
    (mapv #(.item nodes %) (range (.getLength nodes)))))

(defn local-name [^Node node]
  (when (= tei-namespace (.getNamespaceURI node))
    (.getLocalName node)))

(defn selected-children
  "Select principal body text, excluding editorial notes and figure descriptions.
  Call on an rt directly to read its content."
  [node]
  (let [nodes (children node)]
    (case (local-name node)
      ("note" "fw" "figDesc") []
      "ruby" (filterv #(= "rb" (local-name %)) nodes)
      "app" (let [lemmas (filterv #(= "lem" (local-name %)) nodes)]
              (when-not (= 1 (count lemmas))
                (throw (ex-info "TEI apparatus requires one supplied reading" {})))
              lemmas)
      "choice" (or (some (fn [tag]
                           (let [matches (filterv #(= tag (local-name %)) nodes)]
                             (when (seq matches) matches)))
                         ["corr" "reg" "expan" "sic" "orig" "abbr"])
                   (throw (ex-info "No supported reading in TEI choice" {})))
      (filterv #(not= "rt" (local-name %)) nodes))))

(defn utf8-size [^String text]
  (alength (.getBytes text "UTF-8")))

(defn utf8-offsets
  "Map requested UTF-8 boundaries to Java offsets without indexing every character."
  [^String text offsets]
  (when-not (every? #(and (integer? %) (<= 0 %)) offsets)
    (throw (ex-info "Invalid UTF-8 offset" {})))
  (let [wanted (set offsets)]
    (loop [char-offset 0 byte-offset 0 result (transient (if (wanted 0) {0 0} {}))]
      (if (= char-offset (.length text))
        (let [result (persistent! result)]
          (when-not (= wanted (set (keys result)))
            (throw (ex-info "Offset is outside text or inside a UTF-8 sequence" {})))
          result)
        (let [cp (.codePointAt text char-offset)
              chars (Character/charCount cp)]
          (when (<= 0xd800 cp 0xdfff)
            (throw (ex-info "Unpaired surrogate in analysis text" {:offset char-offset})))
          (let [bytes (cond (< cp 0x80) 1 (< cp 0x800) 2 (< cp 0x10000) 3 :else 4)
                next-char (+ char-offset chars)
                next-byte (+ byte-offset bytes)]
            (recur next-char (long next-byte)
                   (if (wanted next-byte) (assoc! result next-byte next-char) result))))))))

(def ^:private block-tags #{"p" "head" "l" "ab" "item"})
(def ^:private structural-tags #{"body" "div" "lg" "list" "text" "floatingText" "figure"})

(defn- text-segments [^Node node]
  (let [tag (local-name node)]
    (cond
      (= Node/TEXT_NODE (.getNodeType node))
      (let [text (.getNodeValue node)]
        (if (and (structural-tags (local-name (.getParentNode node)))
                 (every? #(Character/isWhitespace ^char %) text))
          []
          [{:view/kind :view/text :view/text text :view/node node}]))

      (#{"lb" "pb" "cb"} tag)
      [{:view/kind :view/break :view/text "\n" :view/node node}]

      (and (= "g" tag) (empty? (.getTextContent node)))
      [{:view/kind :view/unresolved-glyph :view/text "\uFFFC" :view/node node}]

      (#{Node/COMMENT_NODE Node/PROCESSING_INSTRUCTION_NODE} (.getNodeType node)) []

      :else
      (let [segments (into [] (mapcat text-segments) (selected-children node))]
        (if (and (block-tags tag) (seq segments))
          (conj segments {:view/kind :view/block-end :view/text "\n" :view/node node})
          segments)))))

(defn- reading-segments [node]
  (let [raw (text-segments node)
        segments (reduce (fn [result [index segment]]
                           (if (and (= :view/block-end (:view/kind segment))
                                    (or (str/ends-with? (or (:view/text (peek result)) "") "\n")
                                        (str/starts-with? (or (:view/text (get raw (inc index))) "") "\n")))
                             result
                             (conj result segment)))
                         [] (map-indexed vector raw))]
    (if (= :view/block-end (:view/kind (peek segments)))
      (pop segments)
      segments)))

(defn visible-text [node]
  (apply str (map :view/text (reading-segments node))))

(defn- interpretation-problems [^Document document]
  (let [notes (.getElementsByTagNameNS document tei-namespace "note")]
    (into []
          (keep (fn [index]
                  (let [^org.w3c.dom.Element note (.item notes index)]
                    (when (= "interpretation-problem" (.getAttribute note "type"))
                      (let [problem (json/read-json (.getTextContent note))]
                        (when-not (and (map? problem) (vector? (get problem "aspects"))
                                       (every? string? (get problem "aspects")))
                          (throw (ex-info "Invalid TEI interpretation problem" {:problem problem})))
                        {:view/problem :view/interpretation-problem :view/evidence problem})))))
          (range (.getLength notes)))))

(defn from-document [^Document document]
  (let [texts (filterv #(= "text" (local-name %)) (children (.getDocumentElement document)))
        bodies (into [] (mapcat #(filter (fn [node] (= "body" (local-name node))) (children %))) texts)]
    (when-not (= 1 (count bodies))
      (throw (ex-info "Analysis requires exactly one outer TEI body" {:count (count bodies)})))
    (let [segments (reading-segments (first bodies))
          text (apply str (map :view/text segments))
          _ (utf8-offsets text [])
          [segments _] (reduce (fn [[result start] segment]
                                 (let [end (+ start (utf8-size (:view/text segment)))]
                                   [(conj result (assoc segment :view/start start :view/end end)) end]))
                               [[] 0] segments)
          problems (interpretation-problems document)
          content-uncertain? (some #(some #{"content"} (get-in % [:view/evidence "aspects"])) problems)
          excluded (filter #(= :view/unresolved-glyph (:view/kind %)) segments)
          [eligible offset] (reduce (fn [[spans offset] {:view/keys [start end]}]
                                      [(cond-> spans (< offset start) (conj [offset start])) end])
                                    [[] 0] excluded)
          eligible (cond-> eligible (< offset (utf8-size text)) (conj [offset (utf8-size text)]))]
      {:view/id (hash/format-sha256
                 (hash/sha256-canonical-json {"policy" "body-v1" "unit" "utf8-bytes" "text" text}))
       :view/policy :view/body-v1
       :view/text text
       :view/eligible-spans (if content-uncertain? [] eligible)
       :view/problems (into problems (map #(hash-map :view/problem :view/unresolved-glyph
                                                     :view/start (:view/start %) :view/end (:view/end %))) excluded)
       :view/segments segments
       :view/document document})))

(defn from-tei [tei]
  (from-document (read-document tei)))
