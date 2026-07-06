(ns abc.relaxng
  (:import [org.xml.sax InputSource]
           [com.thaiopensource.validate Schema Validator]
           [com.thaiopensource.validate.rng SAXSchemaReader]))
;; Maybe use the javax module instead?
;; https://docs.oracle.com/en/java/javase/14/docs/api/java.xml/javax/xml/validation/package-summary.html
;; https://developer.android.com/reference/javax/xml/validation/SchemaFactory
(defn validate-tei [filename]
  ;; TODO
  (.getInstance SAXSchemaReader) (.create-schema "") (InputSource. filename))
