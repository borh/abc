(ns abc.tools.schematron
  "Schematron evaluator for ABC's TEI profile.

  Validation uses ph-schematron's ISO Schematron-to-XSLT engine and returns the
  ABC finding shape consumed by publication and design-bundle validation."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.transform.stream StreamSource]
           [com.helger.schematron.sch SchematronResourceSCH]
           [com.helger.schematron.svrl SVRLFailedAssert SVRLSuccessfulReport]
           [com.helger.schematron.svrl.jaxb ActivePattern FailedAssert FiredRule SuccessfulReport]))

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

(defn- severity [kind role]
  (keyword (or (when-not (string/blank? role) role)
               (if (= kind :report) "warning" "error"))))

(defn- finding [label rule-id context kind message]
  {:label label
   :rule-id rule-id
   :severity (severity kind (.getRole message))
   :kind kind
   :context context
   :test (.getTest message)
   :message (string/trim (.getText message))
   :location (.getLocation message)})

(defn- schematron-resource [schema-path]
  (let [resource (SchematronResourceSCH/fromFile (io/file schema-path))]
    (when-not (.isValidSchematron resource)
      (throw (ex-info "Invalid Schematron schema"
                      {:schema-path schema-path})))
    resource))

(defn- svrl-findings [label svrl]
  (loop [items (seq (.getActivePatternAndFiredRuleAndFailedAssert svrl))
         rule-id nil
         context nil
         findings []]
    (if-not items
      findings
      (let [item (first items)]
        (cond
          (instance? ActivePattern item)
          (recur (next items) (.getId ^ActivePattern item) nil findings)

          (instance? FiredRule item)
          (recur (next items) rule-id (.getContext ^FiredRule item) findings)

          (instance? FailedAssert item)
          (recur (next items) rule-id context
                 (conj findings
                       (finding label rule-id context :assert
                                (SVRLFailedAssert. ^FailedAssert item))))

          (instance? SuccessfulReport item)
          (recur (next items) rule-id context
                 (conj findings
                       (finding label rule-id context :report
                                (SVRLSuccessfulReport. ^SuccessfulReport item))))

          :else
          (recur (next items) rule-id context findings))))))

(defn validate!
  "Evaluate an ISO Schematron schema against one XML document.

  Returns {:label string, :findings [...]}. Assertion findings are emitted when
  the test is false. Report findings are emitted when the test is true."
  [{:keys [schema-path xml-path label]}]
  (let [resource (schematron-resource schema-path)
        svrl (.applySchematronValidationToSVRL resource
                                               (StreamSource. (io/file xml-path)))
        findings (svrl-findings label svrl)]
    {:label label
     :findings (vec findings)}))
