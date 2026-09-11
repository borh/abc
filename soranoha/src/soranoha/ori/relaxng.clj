(ns soranoha.ori.relaxng
  "Wrap Jing for TEI RelaxNG validation. validate! parses the XML against a
  cached parse of the schema (keyed by canonical path + mtime, mirroring
  soranoha.ori.schematron's XSLT cache) and returns structured per-file
  violations. Safe for concurrent use: the cached
  com.thaiopensource.validate.Schema is immutable; a fresh Validator and
  error handler are created per call."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.thaiopensource.util PropertyMapBuilder]
           [com.thaiopensource.validate Schema ValidateProperty]
           [com.thaiopensource.validate.auto AutoSchemaReader]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax InputSource SAXParseException]))

(defn- file->input-source ^InputSource [^String path]
  (InputSource. (.toString (.toURI (io/file path)))))

(defn- build-sax-error-handler [violations-atom]
  (let [record! (fn [severity ^SAXParseException e]
                  (swap! violations-atom conj
                         {:severity severity
                          :line (.getLineNumber e)
                          :column (.getColumnNumber e)
                          :message (.getMessage e)}))]
    (reify org.xml.sax.ErrorHandler
      (^void warning [_ ^SAXParseException e] (record! :warning e))
      (^void error [_ ^SAXParseException e] (record! :error e))
      (^void fatalError [_ ^SAXParseException e] (record! :fatal e)))))

(defn- error-handler-props ^com.thaiopensource.util.PropertyMap [handler]
  (let [builder (PropertyMapBuilder.)]
    (.put builder ValidateProperty/ERROR_HANDLER handler)
    (.toPropertyMap builder)))

;; Jing Schema objects can be shared; Validators cannot. Cache the parsed
;; schema by canonical path and mtime, and create a Validator per call.
(defonce ^:private schema-cache (atom {}))

(defn- load-schema ^Schema [^String schema-path]
  (let [violations (atom [])]
    (try
      (let [path (fs/path schema-path)
            cache-key [(str (fs/canonicalize path))
                       (fs/last-modified-time path)]]
        (or (get @schema-cache cache-key)
            (let [schema (.createSchema
                          (AutoSchemaReader.)
                          (file->input-source schema-path)
                          (error-handler-props
                           (build-sax-error-handler violations)))]
              (swap! schema-cache assoc cache-key schema)
              schema)))
      (catch Exception e
        (throw (ex-info (str "Failed to load TEI RelaxNG schema: " schema-path)
                        {:schema-path schema-path
                         :violations @violations}
                        e))))))

(defn validate!
  "Validate the XML at `xml-path` against the RelaxNG schema at
  `schema-path`. Returns {:label, :violations [{:severity, :line,
  :column, :message} ...]}. Does not throw on validation issues;
  severity classification preserved on each violation. Malformed XML is
  recorded as a :fatal violation and the parser's SAXParseException
  propagates."
  [{:keys [^String schema-path ^String xml-path label]}]
  (when (string/blank? schema-path)
    (throw (ex-info "TEI RelaxNG schema path must be set."
                    {:error :missing-schema-path})))
  (when (string/blank? xml-path)
    (throw (ex-info "TEI XML path must be set."
                    {:error :missing-xml-path})))
  (let [schema (load-schema schema-path)
        violations (atom [])
        handler (build-sax-error-handler violations)
        validator (.createValidator schema (error-handler-props handler))
        factory (doto (SAXParserFactory/newInstance)
                  (.setNamespaceAware true))
        xml-reader (.getXMLReader (.newSAXParser factory))]
    (.setContentHandler xml-reader (.getContentHandler validator))
    (.setErrorHandler xml-reader handler)
    (.parse xml-reader (file->input-source xml-path))
    {:label label
     :violations @violations}))
