(ns abc.tools.schematron
  "Schematron evaluator for ABC's TEI profile.

  Validation uses ph-schematron's ISO Schematron-to-XSLT engine and returns the
  ABC finding shape consumed by publication and design-bundle validation."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.transform.stream StreamSource]
           [com.helger.schematron.pure SchematronResourcePure]
           [com.helger.schematron.sch SchematronResourceSCH]
           [com.helger.schematron.svrl SVRLFailedAssert SVRLSuccessfulReport]
           [com.helger.schematron.svrl.jaxb ActivePattern FailedAssert FiredRule SuccessfulReport]))

(def ^:private schematron-ns "http://purl.oclc.org/dsdl/schematron")

(defn- namespace-aware-document [path]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.. factory newDocumentBuilder (parse (io/file path)))))

(defn pattern-ids
  "Return Schematron pattern IDs in document order."
  [schema-path]
  (let [document (namespace-aware-document schema-path)
        patterns (.getElementsByTagNameNS document schematron-ns "pattern")]
    (mapv #(.getAttribute ^org.w3c.dom.Element (.item patterns %) "id")
          (range (.getLength patterns)))))

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

;; Compiling the ISO-Schematron schema to XSLT is by far the dominant per-call
;; cost (~0.4s vs ~0.03s for the actual validation). ph-schematron caches the
;; bound transformer inside a SchematronResourceSCH once it is used, so reusing
;; the resource across validations skips recompilation. Cache by canonical path
;; + mtime so an edited schema busts the entry. Safe for concurrent apply: the
;; cached SchematronResourceSCH is fully compiled by .isValidSchematron before
;; it is published into the atom, and each validate! call applies a fresh JAXP
;; Transformer from the thread-safe compiled Templates, with no shared
;; per-call mutable state.
(defonce ^:private resource-cache (atom {}))

(defn- schematron-resource [schema-path]
  (let [file (io/file schema-path)
        cache-key [(.getCanonicalPath file) (.lastModified file)]]
    (or (get @resource-cache cache-key)
        (let [resource (SchematronResourceSCH/fromFile file)]
          (when-not (.isValidSchematron resource)
            (throw (ex-info "Invalid Schematron schema"
                            {:schema-path schema-path})))
          (swap! resource-cache assoc cache-key resource)
          resource))))

(defn schema-valid?
  "Return whether ph-schematron accepts schema-path for the selected backend.

  :xslt is the runtime backend used by validate!. :pure is a stricter
  in-memory diagnostic model used by artifact-boundary tests."
  [backend schema-path]
  (case backend
    :xslt (.isValidSchematron (SchematronResourceSCH/fromFile (io/file schema-path)))
    :pure (.isValidSchematron (SchematronResourcePure/fromFile (io/file schema-path)))
    (throw (ex-info "Unsupported Schematron backend"
                    {:backend backend
                     :supported #{:xslt :pure}}))))

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
