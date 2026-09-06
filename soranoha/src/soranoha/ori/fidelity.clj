(ns soranoha.ori.fidelity
  "Independent, bounded comparisons of retained Aozora text with its exports.
  Passing these checks does not certify unsupported markup or editorial fidelity."
  (:require [clojure.string :as str]
            [soranoha.core.hash :as hash])
  (:import [java.io ByteArrayInputStream]
           [java.nio ByteBuffer]
           [java.nio.charset Charset CodingErrorAction]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Node]))

(defn- decode [bytes encoding]
  (str (.decode (doto (.newDecoder (Charset/forName encoding))
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap bytes))))

(defn- source-text [bytes]
  (or (try {:text (decode bytes "UTF-8") :encoding "UTF-8"}
           (catch Exception _ nil))
      (try {:text (decode bytes "windows-31j") :encoding "windows-31j"}
           (catch Exception _ nil))))

(defn- children [^Node node]
  (let [xs (.getChildNodes node)]
    (map #(.item xs %) (range (.getLength xs)))))

(defn- elements [node name]
  (filter #(and (= name (.getLocalName ^Node %))
                (= "http://www.tei-c.org/ns/1.0" (.getNamespaceURI ^Node %)))
          (tree-seq #(seq (children %)) children node)))

(defn- attr [^Node node name]
  (when-let [attrs (.getAttributes node)]
    (when-let [a (.getNamedItem attrs name)] (.getNodeValue a))))

(defn- abc-attr [^Node node name]
  (when-let [attrs (.getAttributes node)]
    (when-let [a (.getNamedItemNS attrs "https://w3id.org/abc/ns/tei" name)]
      (.getNodeValue a))))

(defn- xml [bytes]
  (let [factory (doto (DocumentBuilderFactory/newInstance)
                  (.setNamespaceAware true)
                  (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true)
                  (.setFeature "http://xml.org/sax/features/external-general-entities" false)
                  (.setFeature "http://xml.org/sax/features/external-parameter-entities" false)
                  (.setXIncludeAware false)
                  (.setExpandEntityReferences false))]
    (.parse (.newDocumentBuilder factory) (ByteArrayInputStream. bytes))))

(defn- visible [node]
  (let [tag (.getLocalName ^Node node)]
    (cond
      (#{"rt" "note"} tag) ""
      (= Node/TEXT_NODE (.getNodeType ^Node node))
      (.getNodeValue ^Node node)
      :else (apply str (map #(visible %) (children node))))))

(defn- gaiji [row cell]
  (let [row (Long/parseLong row)
        cell (Long/parseLong cell)]
    (when (and (<= 1 row 94) (<= 1 cell 94))
      (let [lead (+ 0x81 (quot (dec row) 2))
            lead (if (> lead 0x9f) (+ lead 0x40) lead)
            trail (if (even? row) (+ cell 0x9e)
                      (+ cell (if (< cell 64) 0x3f 0x40)))]
        (decode (byte-array [(unchecked-byte lead) (unchecked-byte trail)])
                "x-SJIS_0213")))))

(def ruby-pattern #"(?:｜([^｜《》\n]+)|([\p{IsHan}々〆ヵヶ]+))《([^《》\n]+)》")

(defn- source-emphasis [text]
  (let [matcher (re-matcher #"［＃「([^「」\n]+)」に傍点］" text)]
    (loop [end 0 plain "" spans []]
      (if (.find matcher)
        (let [prefix (str plain (subs text end (.start matcher)))
              target (.group matcher 1)
              start (- (count prefix) (count target))
              [last-start last-text] (peek spans)]
          (when (and (str/ends-with? prefix target)
                     (>= start (+ (or last-start 0) (count last-text))))
            (recur (.end matcher) prefix (conj spans [start target "bouten"]))))
        {:plain (str plain (subs text end)) :emphasis spans}))))

(defn- export-emphasis [node]
  (letfn [(walk [node offset]
            (if (#{"rt" "note"} (.getLocalName ^Node node))
              [offset []]
              (if (= Node/TEXT_NODE (.getNodeType ^Node node))
                [(+ offset (count (.getNodeValue ^Node node))) []]
                (reduce (fn [[at spans] child]
                          (let [[next-at child-spans] (walk child at)]
                            [next-at (into spans child-spans)]))
                        [offset (if (= "hi" (.getLocalName ^Node node))
                                  [[offset (visible node) (attr node "rend")]] [])]
                        (children node)))))]
    (second (walk node 0))))

(defn- parse-line [line]
  (let [gaijis (atom [])
        mapped (str/replace line #"※［＃[^］]*第3水準1-([0-9]+)-([0-9]+)］"
                            (fn [[_ row cell]]
                              (let [s (or (gaiji row cell) "�")]
                                (swap! gaijis conj s) s)))
        heading (re-matches #"［＃[０-９0-9]+字下げ］(.+)［＃「(.+)」は中見出し］" mapped)
        closing? (str/starts-with? mapped "［＃地から１字上げ］")
        text (cond heading (if (= (nth heading 1) (nth heading 2))
                             (nth heading 1) mapped)
                   closing? (subs mapped (count "［＃地から１字上げ］"))
                   :else mapped)
        rubies (mapv (fn [[_ explicit implicit reading]] [(or explicit implicit) reading])
                     (re-seq ruby-pattern text))
        unpointed (str/replace text ruby-pattern (fn [[_ explicit implicit _]] (or explicit implicit)))
        emphasis (source-emphasis unpointed)
        plain (or (:plain emphasis) unpointed)
        unsupported? (boolean (re-find #"[［］《》｜※�]" plain))]
    {:plain plain :rubies rubies :gaijis @gaijis :emphasis (:emphasis emphasis)
     :heading (when heading (nth heading 1))
     :heading-indent (when heading
                       (Long/parseLong (apply str (map #(Character/digit ^char % 10)
                                                       (second (re-find #"^［＃([０-９0-9]+)字下げ］" mapped))))))
     :closing? closing?
     :indent (count (or (re-find #"^　+" plain) ""))
     :unsupported? unsupported?}))

(defn- source-parts [s]
  (let [s (str/replace s #"\r\n?" "\n")
        sections (str/split s #"(?m)^-{20,}\n" -1)]
    (when (= 3 (count sections))
      (let [parts (str/split (last sections) #"(?m)(?=^底本：)" -1)]
        (when (= 2 (count parts))
          {:lines (mapv parse-line (remove str/blank? (str/split-lines (first parts))))
           :notes (->> (str/split-lines (second parts))
                       (remove str/blank?) vec)})))))

(defn- result [id status message]
  {"id" id "status" status "message" message})

(defn- comparison [id expected actual]
  (result id (if (= expected actual) "passed" "failed")
          (if (= expected actual) "Source-derived values match."
              "Source-derived values differ from the export.")))

(defn- compare-exports [{:keys [lines notes]} doc plaintext]
  (let [mappings (into {} (map (fn [c]
                                 [(str "#" (attr c "xml:id"))
                                  (some #(when (= "unicode" (attr % "type"))
                                           (.getTextContent ^Node %))
                                        (elements c "mapping"))])
                               (elements doc "char")))
        body (first (elements doc "body"))
        blocks (->> (tree-seq #(seq (children %)) children body)
                    (filter #(and (= "http://www.tei-c.org/ns/1.0" (.getNamespaceURI ^Node %))
                                  (#{"p" "head"} (.getLocalName ^Node %))))
                    (remove #(str/blank? (visible %))))
        paragraphs (->> (elements body "p")
                        (map #(vector % (visible %)))
                        (remove #(str/blank? (second %))) vec)
        source-paragraphs (vec (remove :heading lines))
        source-notes (filter #(#{"source-attribution" "transcriber-note"} (attr % "type"))
                             (elements doc "note"))
        note-lines (vec (mapcat #(elements % "seg") source-notes))
        indent-style? (fn [node property n]
                        (let [style (or (attr node "style") "")]
                          (or (and (zero? n) (not (str/includes? style property)))
                              (boolean (re-find (re-pattern (str property "\\s*:\\s*" n "em(?:;|$)"))
                                                style)))))]
    [(comparison "plaintext-start" false (boolean (re-find #"^\r?\n" plaintext)))
     (comparison "plaintext-body"
                 (mapv :plain lines)
                 (vec (remove str/blank? (str/split-lines (str/replace plaintext #"\r\n?" "\n")))))
     (comparison "tei-body-text"
                 (mapv #(str/replace (:plain %) #"^　+" "") source-paragraphs)
                 (mapv second paragraphs))
     (comparison "tei-block-order"
                 (mapv #(vector (if (:heading %) "head" "p")
                                (str/replace (:plain %) #"^　+" "")) lines)
                 (mapv #(vector (.getLocalName ^Node %) (visible %)) blocks))
     (comparison "tei-headings" (vec (keep :heading lines))
                 (mapv #(visible %) (elements body "head")))
     (comparison "tei-heading-layout" true
                 (let [expected (filter :heading lines)
                       actual (elements body "head")]
                   (and (= (count expected) (count actual))
                        (every? true?
                                (map (fn [src h]
                                       (and (= "2" (attr h "n"))
                                            (indent-style? h "padding-inline-start" (:heading-indent src))))
                                     expected actual)))))
     (comparison "tei-ruby" (vec (mapcat :rubies lines))
                 (mapv (fn [r] [(visible (first (elements r "rb")))
                                (.getTextContent ^Node (first (elements r "rt")))])
                       (elements body "ruby")))
     (comparison "tei-gaiji" (mapv #(vector % %) (mapcat :gaijis lines))
                 (mapv #(vector (.getTextContent ^Node %)
                                (get mappings (attr % "ref") "�"))
                       (elements body "g")))
     (comparison "tei-emphasis"
                 (mapv (fn [line]
                         (mapv (fn [[start text rendition]]
                                 [(- start (:indent line)) text rendition])
                               (:emphasis line))) lines)
                 (mapv export-emphasis blocks))
     (comparison "tei-paragraph-indentation" true
                 (and (= (count source-paragraphs) (count paragraphs))
                      (every? true?
                              (map (fn [src [p text]]
                                     (and (not (str/starts-with? text "　"))
                                          (indent-style? p "text-indent" (:indent src))))
                                   source-paragraphs paragraphs))))
     (comparison "tei-source-note-layout" true
                 (and (= (count notes) (count note-lines))
                      (every? true?
                              (map (fn [s node]
                                     (let [n (count (or (re-find #"^　+" s) ""))]
                                       (and (= (subs s n) (.getTextContent ^Node node))
                                            (indent-style? node "padding-inline-start" n))))
                                   notes note-lines))))
     (comparison "closing-date-layout" true
                 (every? true?
                         (map (fn [src [p _]]
                                (or (not (:closing? src))
                                    (and (= "chitsuki" (abc-attr p "layout-kind"))
                                         (= "align=right;offset-from-end=1" (abc-attr p "layout-params")))))
                              source-paragraphs paragraphs)))]))

(defn check
  "Compare raw primary-text bytes against TEI and UTF-8 plaintext bytes.
  Unknown source boundaries, annotations or encodings remain not-evaluated;
  status covers only the listed checks and never controls rights admission."
  [source-bytes tei-bytes plaintext-bytes]
  (let [decoded (source-text source-bytes)
        parts (when decoded
                (try (source-parts (:text decoded))
                     (catch Exception _ nil)))
        supported? (and parts (seq (:lines parts)) (not-any? :unsupported? (:lines parts)))
        coverage (result "source-coverage" (if supported? "passed" "not-evaluated")
                         (if supported? "Recognized bounded source syntax and body boundaries."
                             "Unrecognized source encoding, body boundary or annotation; comparisons withheld."))
        checks (if supported?
                 (try (into [coverage] (compare-exports parts (xml tei-bytes) (decode plaintext-bytes "UTF-8")))
                      (catch Exception _
                        [coverage (result "export-decode" "failed" "Export XML or UTF-8 text cannot be compared.")]))
                 [coverage])
        statuses (set (map #(get % "status") checks))]
    {"schema" "soranoha-source-fidelity/1"
     "artifacts" (into {} (map (fn [[k v]] [k (str "sha256:" (hash/sha256-bytes v))])
                               [["source" source-bytes] ["tei" tei-bytes] ["plaintext" plaintext-bytes]]))
     "source_encoding" (:encoding decoded)
     "status" (cond (statuses "failed") "failed"
                    (statuses "not-evaluated") "not-evaluated" :else "passed")
     "checks" checks
     "limitations" ["Limited to separator-delimited Aozora prose with a 底本 colophon, basic ruby, non-overlapping retrospective emphasis dots, plane-1 third-level JIS gaiji, middle headings, and leading fullwidth indentation."
                    "Blank-line spacing and title/author metadata are not certified. Passing is scoped to these comparisons, not complete editorial fidelity."]}))
