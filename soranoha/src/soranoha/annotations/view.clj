(ns soranoha.annotations.view
  "Body reading and UTF-8 alignment for analysis of TEI transcription.
  DOM mappings are local to a parsed document; only text and policy identify a view."
  (:require [soranoha.core.hash :as hash])
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
  "Select the established body reading. Call on an rt directly to read its content."
  [node]
  (let [nodes (children node)]
    (case (local-name node)
      ("note" "fw") []
      "ruby" (filterv #(= "rb" (local-name %)) nodes)
      "choice" (or (some (fn [tag]
                           (let [matches (filterv #(= tag (local-name %)) nodes)]
                             (when (seq matches) matches)))
                         ["corr" "reg" "expan" "sic" "orig" "abbr"])
                    (throw (ex-info "No supported reading in TEI choice" {})))
      (filterv #(not= "rt" (local-name %)) nodes))))

(defn utf8-size [^String text]
  (alength (.getBytes text "UTF-8")))

(defn utf8-boundaries
  "Map valid UTF-8 boundaries to Java string offsets; reject unpaired surrogates."
  [^String text]
  (loop [char-offset 0 byte-offset 0 result (transient {0 0})]
    (if (= char-offset (.length text))
      (persistent! result)
      (let [cp (.codePointAt text char-offset)
            chars (Character/charCount cp)]
        (when (<= 0xd800 cp 0xdfff)
          (throw (ex-info "Unpaired surrogate in analysis text" {:offset char-offset})))
        (let [bytes (cond (< cp 0x80) 1 (< cp 0x800) 2 (< cp 0x10000) 3 :else 4)
              next-char (+ char-offset chars)
              next-byte (+ byte-offset bytes)]
          (recur next-char next-byte (assoc! result next-byte next-char)))))))

(def ^:private block-tags #{"p" "head" "l" "ab" "item"})
(def ^:private structural-tags #{"body" "div" "lg" "list" "text"})

(defn- text-segments [^Node node]
  (let [tag (local-name node)]
    (cond
      (= Node/TEXT_NODE (.getNodeType node))
      (let [text (.getNodeValue node)]
        (if (and (structural-tags (local-name (.getParentNode node)))
                 (every? #(Character/isWhitespace ^char %) text))
          []
          [{:view/kind :view/text :view/text text :view/node node}]))

      (#{"lb" "pb"} tag)
      [{:view/kind :view/break :view/text "\n" :view/node node}]

      (#{Node/COMMENT_NODE Node/PROCESSING_INSTRUCTION_NODE} (.getNodeType node)) []

      :else
      (let [segments (into [] (mapcat text-segments) (selected-children node))]
        (if (and (block-tags tag) (seq segments))
          (conj segments {:view/kind :view/block-end :view/text "\n" :view/node node})
          segments)))))

(defn- reading-segments [node]
  (let [segments (text-segments node)]
    (if (= :view/block-end (:view/kind (peek segments)))
      (pop segments)
      segments)))

(defn visible-text [node]
  (apply str (map :view/text (reading-segments node))))

(defn from-document [^Document document]
  (let [bodies (.getElementsByTagNameNS document tei-namespace "body")]
    (when-not (= 1 (.getLength bodies))
      (throw (ex-info "Analysis requires exactly one TEI body" {:count (.getLength bodies)})))
    (let [segments (reading-segments (.item bodies 0))
          text (apply str (map :view/text segments))
          _ (utf8-boundaries text)
          [segments _] (reduce (fn [[result start] segment]
                                 (let [end (+ start (utf8-size (:view/text segment)))]
                                   [(conj result (assoc segment :view/start start :view/end end)) end]))
                               [[] 0] segments)]
      {:view/id (hash/format-sha256
                 (hash/sha256-canonical-json {"policy" "body-v1" "unit" "utf8-bytes" "text" text}))
       :view/policy :view/body-v1
       :view/text text
       :view/segments segments
       :view/document document})))

(defn from-tei [tei]
  (from-document (read-document tei)))
