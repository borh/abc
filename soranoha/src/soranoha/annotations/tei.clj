(ns soranoha.annotations.tei
  "Assemble selected analysis layers without replacing or flattening transcription."
  (:require [soranoha.annotations.layer :as layer]
            [soranoha.annotations.view :as view]
            [soranoha.core.json :as record-json])
  (:import [java.io StringWriter]
           [javax.xml.transform OutputKeys TransformerFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform.stream StreamResult]
           [org.w3c.dom Document Element Node Text]))

(defn- element ^Element [^Document doc name attributes]
  (let [el (.createElementNS doc view/tei-namespace name)]
    (doseq [[key value] attributes]
      (if (= key "xml:id")
        (.setAttributeNS el view/xml-namespace key value)
        (.setAttribute el key value)))
    el))

(defn- all-elements [^Document doc]
  (let [elements (.getElementsByTagName doc "*")]
    (map #(.item elements %) (range (.getLength elements)))))

(defn- source-points [segments wanted]
  (reduce
   (fn [points {:view/keys [kind node start end text]}]
     (case kind
       :view/text
       (reduce-kv (fn [points byte char]
                    (let [offset (+ start byte)
                          parent (.getParentNode ^Node node)]
                      (if-not (wanted offset)
                        points
                        (if (= "g" (view/local-name parent))
                          (cond
                            (= offset start) (assoc points offset {:node parent :side :before})
                            (= offset end) (assoc points offset {:node parent :side :after})
                            :else (throw (ex-info "Analysis boundary splits an atomic TEI glyph" {:offset offset})))
                          (assoc points offset {:node node :char char})))))
                  points (view/utf8-offsets text (map #(- % start) (subseq wanted >= start <= end))))
       (:view/break :view/unresolved-glyph)
       (cond-> points
         (wanted start) (assoc start {:node node :side :before})
         (wanted end) (assoc end {:node node :side :after}))
       :view/block-end
       (cond-> points
         (wanted start) (assoc start {:node node :side :inside-end})
         (wanted end) (assoc end {:node node :side :inside-end}))))
   {} segments))

(defn- insert-anchor! [^Document doc id {:keys [^Node node char side]}]
  (let [anchor (element doc "anchor" {"xml:id" id})]
    (if (some? char)
      (let [^Text text node
            parent (.getParentNode text)]
        (cond
          (zero? char) (.insertBefore parent anchor text)
          (= char (.getLength text)) (.insertBefore parent anchor (.getNextSibling text))
          :else (.insertBefore parent anchor (.splitText text char))))
      (case side
        :before (.insertBefore (.getParentNode node) anchor node)
        :after (.insertBefore (.getParentNode node) anchor (.getNextSibling node))
        :inside-end (.appendChild node anchor)))))

(defn- serialize [^Document doc]
  (let [transformer (.newTransformer (TransformerFactory/newInstance))
        out (StringWriter.)]
    (.setOutputProperty transformer OutputKeys/ENCODING "UTF-8")
    (.setOutputProperty transformer OutputKeys/INDENT "no")
    (.transform transformer (DOMSource. doc) (StreamResult. out))
    (str out)))

(defn enrich
  "Read immutable base TEI and append selected layers bound to its body-v1 text view.
  The result preserves all existing elements and their text; anchors split text nodes only.
  Layer payloads remain separately stored and are referenced by content identity."
  [tei layers]
  (if (empty? layers)
    tei
    (let [text-view (view/from-tei tei)
          ^Document doc (:view/document text-view)
          identities (mapv #(layer/layer-id text-view %) layers)
          _ (when-not (= (count identities) (count (set identities)))
              (throw (ex-info "Duplicate analysis layer selection" {})))
          wanted (into (sorted-set) (mapcat (fn [layer]
                                              (mapcat (juxt :annotation/start :annotation/end)
                                                      (:layer/records layer)))) layers)
          points (source-points (:view/segments text-view) wanted)
          prefix (str "analysis-" (subs (:view/id text-view) 7 23) "-")
          anchor-id #(str prefix %)
          group-id #(str "layer-" (subs % 7))
          existing (into #{} (keep (fn [^Element el]
                                     (let [id (.getAttributeNS el view/xml-namespace "id")]
                                       (when (seq id) id)))) (all-elements doc))
          ids (concat (map anchor-id wanted)
                      (map group-id identities)
                      (mapcat (fn [id layer]
                                (map #(str (group-id id) "-" (:annotation/id %)) (:layer/records layer)))
                              identities layers))]
      (when-let [collision (some existing ids)]
        (throw (ex-info "Analysis XML identifier already exists in base transcription" {:id collision})))
      (when-not (= wanted (set (keys points)))
        (throw (ex-info "Analysis boundary lacks a TEI correspondence" {})))
      ;; Descending positions keep earlier offsets valid when a DOM text node is split.
      (doseq [offset (reverse wanted)]
        (insert-anchor! doc (anchor-id offset) (get points offset)))
      (let [stand-off (element doc "standOff" {"type" "research-analysis"})]
        (doseq [[identity analysis] (map vector identities layers)]
          (let [group (element doc "spanGrp"
                               {"xml:id" (group-id identity)
                                "type" "analysis"
                                "corresp" (str "urn:" identity)
                                "n" (:producer/name (:layer/producer analysis))})]
            (.appendChild stand-off (.createTextNode doc "\n  "))
            (doseq [{:annotation/keys [id start end label features]} (:layer/records analysis)]
              (let [span (element doc "span"
                                  {"xml:id" (str (group-id identity) "-" id)
                                   "from" (str "#" (anchor-id start))
                                   "to" (str "#" (anchor-id end))
                                   "n" label})]
                (.appendChild span (.createTextNode doc (record-json/write-deterministic-json-str features)))
                (.appendChild group (.createTextNode doc "\n    "))
                (.appendChild group span)))
            (.appendChild group (.createTextNode doc "\n  "))
            (.appendChild stand-off group)))
        (.appendChild stand-off (.createTextNode doc "\n"))
        (.appendChild (.getDocumentElement doc) (.createTextNode doc "\n"))
        (.appendChild (.getDocumentElement doc) stand-off)
        (.appendChild (.getDocumentElement doc) (.createTextNode doc "\n")))
      (let [result (serialize doc)]
        (when-not (= (:view/id text-view) (:view/id (view/from-tei result)))
          (throw (ex-info "Analysis assembly changed the body reading" {})))
        result))))
