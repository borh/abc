(ns soranoha.ori.schematron
  "Evaluate the TEI profile with ph-schematron and return structured findings."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [javax.xml.transform.stream StreamSource]
           [com.helger.schematron.sch SchematronResourceSCH]
           [com.helger.schematron.svrl SVRLFailedAssert SVRLSuccessfulReport]
           [com.helger.schematron.svrl.jaxb ActivePattern FailedAssert FiredRule SuccessfulReport]))

(defn- severity [kind role]
  (keyword (or (when-not (string/blank? role) role)
               (if (= kind :report) "warning" "error"))))

(defn- finding [label rule-id context kind ^com.helger.schematron.svrl.AbstractSVRLMessage message]
  {:label label
   :rule-id rule-id
   :severity (severity kind (.getRole message))
   :kind kind
   :context context
   :test (.getTest message)
   :message (string/trim (.getText message))
   :location (.getLocation message)})

;; Reuse compiled XSLT by canonical schema path and mtime. Fully compile before
;; sharing the resource; each validation creates a fresh Transformer from the
;; thread-safe Templates.
(defonce ^:private resource-cache (atom {}))

(defn- schematron-resource ^SchematronResourceSCH [schema-path]
  (let [path (fs/path schema-path)
        cache-key [(str (fs/canonicalize path)) (fs/last-modified-time path)]]
    (or (get @resource-cache cache-key)
        (let [resource (SchematronResourceSCH/fromFile (fs/file path))]
          (when-not (.isValidSchematron resource)
            (throw (ex-info "Invalid Schematron schema"
                            {:schema-path schema-path})))
          (swap! resource-cache assoc cache-key resource)
          resource))))

(defn- svrl-findings [label ^com.helger.schematron.svrl.jaxb.SchematronOutputType svrl]
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
