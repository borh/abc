(ns abc.tools.schematron
  "Small Schematron evaluator for ABC's v0 profile subset.

  This is not a full ISO Schematron compiler. It evaluates the checked-in
  `schemas/tei-profile.sch` constructs ABC currently uses:
  sch:ns, sch:pattern, sch:rule, sch:assert, and sch:report. Each pattern
  must contain exactly one rule until ISO rule-claim semantics are implemented.
  XPath 2.0 expressions are evaluated with Saxon-HE."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.transform.stream StreamSource]
           [net.sf.saxon.s9api Processor XdmNode]))

(def ^:private schematron-ns "http://purl.oclc.org/dsdl/schematron")

(defn- namespace-aware-document [path]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.. factory newDocumentBuilder (parse (io/file path)))))

(defn- element-children [^org.w3c.dom.Element element local-name]
  (let [nodes (.getChildNodes element)]
    (->> (range (.getLength nodes))
         (map #(.item nodes %))
         (filter #(instance? org.w3c.dom.Element %))
         (filter #(and (= schematron-ns (.getNamespaceURI ^org.w3c.dom.Element %))
                       (= local-name (.getLocalName ^org.w3c.dom.Element %)))))))

(defn- attr [^org.w3c.dom.Element element attr-name]
  (let [v (.getAttribute element attr-name)]
    (when-not (string/blank? v) v)))

(defn- trim-text [^org.w3c.dom.Element element]
  (string/trim (.getTextContent element)))

(defn- parse-namespaces [^org.w3c.dom.Document document]
  (let [nodes (.getElementsByTagNameNS document schematron-ns "ns")]
    (into {}
          (for [i (range (.getLength nodes))
                :let [node (.item nodes i)
                      prefix (attr node "prefix")
                      uri (attr node "uri")]
                :when (and prefix uri)]
            [prefix uri]))))

(defn- parse-check [pattern-id rule-context kind ^org.w3c.dom.Element element]
  {:rule-id pattern-id
   :context rule-context
   :kind kind
   :severity (keyword (or (attr element "role")
                          (if (= kind :report) "warning" "error")))
   :test (or (attr element "test")
             (throw (ex-info "Schematron check is missing @test"
                             {:rule-id pattern-id
                              :context rule-context})))
   :message (trim-text element)})

(defn parse-schema [schema-path]
  (let [document (namespace-aware-document schema-path)
        patterns (.getElementsByTagNameNS document schematron-ns "pattern")]
    {:namespaces (parse-namespaces document)
     :checks
     (vec
      (mapcat
       (fn [pattern-index]
         (let [pattern (.item patterns pattern-index)
               pattern-id (attr pattern "id")
               rules (vec (element-children pattern "rule"))]
           (when-not (= 1 (count rules))
             (throw (ex-info "ABC v0 Schematron requires exactly one sch:rule per sch:pattern until ISO rule-claim semantics are implemented"
                             {:pattern-id pattern-id
                              :rule-count (count rules)})))
           (mapcat
            (fn [rule]
              (let [context (or (attr rule "context")
                                (throw (ex-info "Schematron rule is missing @context"
                                                {:pattern-id pattern-id})))]
                (concat
                 (map #(parse-check pattern-id context :assert %) (element-children rule "assert"))
                 (map #(parse-check pattern-id context :report %) (element-children rule "report")))))
            rules)))
       (range (.getLength patterns))))}))

(defn- processor []
  (Processor. false))

(defn- document-node [^Processor processor xml-path]
  (let [builder (.newDocumentBuilder processor)]
    (.setLineNumbering builder true)
    (.build builder (StreamSource. (io/file xml-path)))))

(defn- compiler [^Processor processor namespaces]
  (let [compiler (.newXPathCompiler processor)]
    (doseq [[prefix uri] namespaces]
      (.declareNamespace compiler prefix uri))
    compiler))

(defn- context-search-expr [context-expr]
  (->> (string/split context-expr #"\|")
       (map string/trim)
       (map #(if (string/starts-with? % "/") % (str "//" %)))
       (string/join " | ")))

(defn- select-nodes [compiler ^XdmNode document context-expr]
  (let [selector (.load (.compile compiler (context-search-expr context-expr)))]
    (.setContextItem selector document)
    (vec (iterator-seq (.iterator (.evaluate selector))))))

(defn- boolean-test [compiler node test-expr]
  (let [selector (.load (.compile compiler test-expr))]
    (.setContextItem selector node)
    (.effectiveBooleanValue selector)))

(defn- node-location [^XdmNode node]
  (let [line (.getLineNumber node)]
    (when (pos? line)
      (str line))))

(defn- finding [label check node]
  {:label label
   :rule-id (:rule-id check)
   :severity (:severity check)
   :kind (:kind check)
   :context (:context check)
   :test (:test check)
   :message (:message check)
   :location (node-location node)})

(defn validate!
  "Evaluate an ABC Schematron schema against one XML document.

  Returns {:label string, :findings [...]}. Assertion findings are emitted when
  the test is false. Report findings are emitted when the test is true."
  [{:keys [schema-path xml-path label]}]
  (let [{:keys [namespaces checks]} (parse-schema schema-path)
        proc (processor)
        doc (document-node proc xml-path)
        comp (compiler proc namespaces)
        findings
        (reduce
         (fn [acc check]
           (let [nodes (select-nodes comp doc (:context check))]
             (into acc
                   (keep
                    (fn [node]
                      (let [result (boolean-test comp node (:test check))]
                        (case (:kind check)
                          :assert (when-not result (finding label check node))
                          :report (when result (finding label check node)))))
                    nodes))))
         []
         checks)]
    {:label label
     :findings (vec findings)}))
