(ns soranoha.annotations.main
  (:require [soranoha.annotations.layer :as layer]
            [soranoha.annotations.tei :as tei]
            [soranoha.annotations.view :as view]
            [soranoha.core.json :as record-json])
  (:import [java.nio.file Files OpenOption Path StandardOpenOption]))

(defn- write-new! [path ^String text]
  (Files/write (Path/of (str path) (make-array String 0))
               (.getBytes text "UTF-8")
               ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE])))

(defn export-view!
  "Write the exact analyzer text, its policy and eligible byte ranges; refuse overwrite."
  [tei-path out-path]
  (let [{:view/keys [id text eligible-spans problems]} (view/from-tei (slurp tei-path :encoding "UTF-8"))]
    (write-new! out-path
                (record-json/write-deterministic-json-str
                 {"schema" "soranoha-text-view/1" "id" id "policy" "body-v1" "unit" "utf8-bytes"
                  "text" text "eligible_spans" eligible-spans
                  "problems" (mapv (fn [{:view/keys [problem start end evidence]}]
                                     (case problem
                                       :view/unresolved-glyph {"kind" "unresolved-glyph" "start" start "end" end}
                                       :view/interpretation-problem {"kind" "interpretation-problem" "evidence" evidence})) problems)}))
    {"result" "written" "view" id "output" (str out-path)}))

(defn- read-layers [text-view paths]
  (mapv #(layer/read-layer text-view (slurp % :encoding "UTF-8")) paths))

(defn validate-files [tei-path layer-paths]
  (let [text-view (view/from-tei (slurp tei-path :encoding "UTF-8"))
        layers (read-layers text-view layer-paths)]
    {"result" "passed" "view" (:view/id text-view)
     "layers" (mapv #(layer/layer-id text-view %) layers)}))

(defn enrich-files!
  "Validate selected layer files and write enriched TEI; refuse overwrite."
  [tei-path layer-paths out-path]
  (let [base (slurp tei-path :encoding "UTF-8")
        text-view (view/from-tei base)
        layers (read-layers text-view layer-paths)
        enriched (tei/enrich base layers)]
    (write-new! out-path enriched)
    {"result" "written" "view" (:view/id text-view) "output" (str out-path)
     "layers" (mapv #(layer/layer-id text-view %) layers)}))
