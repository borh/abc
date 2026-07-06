(ns abc.tools.tei
  "Wrap Jing for TEI RelaxNG validation. validate! takes a schema
  path and an XML path, parses both, and returns structured per-file
  violations. v0 re-parses the schema each call; future caching can
  use Jing's Schema.createValidator if the per-file cost matters."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.thaiopensource.util PropertyMapBuilder]
           [com.thaiopensource.validate ValidateProperty ValidationDriver]
           [org.xml.sax InputSource SAXParseException]))

(defn- file->input-source ^InputSource [^String path]
  (InputSource. (.toString (.toURI (io/file path)))))

(defn- build-sax-error-handler [violations-atom]
  (reify org.xml.sax.ErrorHandler
    (^void warning [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :warning
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))
    (^void error [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :error
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))
    (^void fatalError [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :fatal
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))))

(defn validate!
  "Validate the XML at `xml-path` against the RelaxNG schema at
  `schema-path`. Returns {:label, :violations [{:severity, :line,
  :column, :message} ...]}. Does not throw on validation issues;
  severity classification preserved on each violation. ValidationDriver
  is built fresh per call (Jing's PropertyMap is constructor-only);
  sequential use only."
  [{:keys [^String schema-path ^String xml-path label]}]
  (when (string/blank? schema-path)
    (throw (ex-info "TEI RelaxNG schema path must be set."
                    {:error :missing-schema-path})))
  (when (string/blank? xml-path)
    (throw (ex-info "TEI XML path must be set."
                    {:error :missing-xml-path})))
  (let [violations (atom [])
        builder (PropertyMapBuilder.)
        _ (.put builder ValidateProperty/ERROR_HANDLER (build-sax-error-handler violations))
        props (.toPropertyMap builder)
        driver (ValidationDriver. props)]
    (when-not (.loadSchema driver (file->input-source schema-path))
      (throw (ex-info (str "Failed to load TEI RelaxNG schema: " schema-path)
                      {:schema-path schema-path
                       :violations @violations})))
    (.validate driver (file->input-source xml-path))
    {:label label
     :violations @violations}))
