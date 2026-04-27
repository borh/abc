(ns abc.tools.manifest-to-rdf
  (:require [abc.tools.files :as files]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]))

(def default-base-iri "https://w3id.org/abc/")

(defn- hash-token [hash-value]
  (string/replace hash-value ":" "-"))

(defn- artifact-iri
  ([hash-value]
   (artifact-iri default-base-iri hash-value))
  ([base-iri hash-value]
   (str "<" base-iri "artifact/" (hash-token hash-value) ">")))

(defn- activity-iri [activity-id]
  (str "<" activity-id ">"))

(defn- agent-iri [agent base-iri]
  (str "<" base-iri "agent/" (string/replace agent "." "-") ">"))

(defn- literal [value]
  (string/replace (json/write-json-str value) "\\/" "/"))

(defn- clause-lines [clauses]
  (map-indexed
   (fn [i clause]
     (str "  " clause (if (= i (dec (count clauses))) " ." " ;")))
   clauses))

(defn manifest->ttl
  ([manifest]
   (manifest->ttl manifest {}))
  ([manifest {:keys [base-iri] :or {base-iri default-base-iri}}]
   (let [artifact-id (get manifest "artifact_id")
         artifact (artifact-iri base-iri artifact-id)
         artifact-kind (get manifest "artifact_kind")
         failure? (= "failure" artifact-kind)
         content (get manifest "content")
         provenance (get manifest "provenance")
         activity-id (get provenance "activity_id")
         activity (activity-iri activity-id)
         schema-hash (get-in manifest ["manifest_identity_object" "manifest_schema_hash"])
         derived (->> (get provenance "was_derived_from")
                      sort
                      (map #(artifact-iri base-iri %)))
         used (->> (get provenance "used")
                   sort
                   (map #(artifact-iri base-iri %)))
         sidecars (->> (get manifest "sidecars")
                       (sort-by (juxt #(get % "role")
                                      #(get % "hash")
                                      #(get % "path_hint"))))
         sidecar-iris (map #(artifact-iri base-iri (get % "hash")) sidecars)
         ;; Main artifact clauses
         artifact-objects (concat
                           (cond-> ["a abc:Artifact"
                                    "a prov:Entity"]
                             failure? (conj "a abc:FailureArtifact"))
                           [(str "abc:artifactId " (literal artifact-id))
                            (str "abc:artifactKind " (literal artifact-kind))
                            (str "abc:schemaHash " (literal schema-hash))
                            (str "abc:validationStatus " (literal (get manifest "validation_status")))
                            (str "prov:generatedAtTime "
                                 (literal (get provenance "generated_at"))
                                 "^^xsd:dateTime")]
                           (when content
                             [(str "abc:contentHash " (literal (get content "content_hash")))
                              (str "dcterms:format " (literal (get content "media_type")))])
                           (map #(str "prov:wasDerivedFrom " %) derived)
                           [(str "prov:wasGeneratedBy " activity)]
                           (map #(str "abc:hasSidecar " %) sidecar-iris))
         ;; Activity clauses: replace string-literal wasAssociatedWith
         ;; with qualifiedAssociation blank node. v0: hadPlan omitted when nil.
         agent-str (get provenance "agent")
         plan-hash (get provenance "plan_hash")
         association-body (string/join " ; "
                                       (concat
                                        ["a prov:Association"
                                         (str "prov:agent " (agent-iri agent-str base-iri))]
                                        (when plan-hash
                                          [(str "prov:hadPlan " (artifact-iri base-iri plan-hash))])))
         activity-clauses (concat
                            ["a prov:Activity"]
                            (map #(str "prov:used " %) used)
                            [(str "prov:qualifiedAssociation [ " association-body " ]")])]
     (str
      "@prefix abc: <https://w3id.org/abc/> .\n"
      "@prefix dcterms: <http://purl.org/dc/terms/> .\n"
      "@prefix prov: <http://www.w3.org/ns/prov#> .\n"
      "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n"
      "\n"
      artifact "\n"
      (string/join "\n" (clause-lines artifact-objects))
      "\n\n"
      activity "\n"
      (string/join "\n" (clause-lines activity-clauses))
      "\n\n"
      (string/join
       "\n\n"
       (map (fn [sidecar]
              (let [sidecar-iri (artifact-iri base-iri (get sidecar "hash"))]
                (str sidecar-iri "\n"
                     (string/join
                      "\n"
                      (clause-lines
                       ["a prov:Entity"
                        (str "abc:schemaHash " (literal schema-hash))
                        (str "abc:sidecarRole " (literal (get sidecar "role")))
                        (str "abc:contentHash " (literal (get sidecar "hash")))
                        (str "dcterms:format " (literal (get sidecar "media_type")))
                        (str "prov:wasGeneratedBy " activity)
                        (str "prov:wasDerivedFrom " artifact)])))))
            sidecars))
      "\n"))))

(defn write-ttl-file! [output-file manifest]
  (io/make-parents output-file)
  (spit (io/file output-file) (manifest->ttl manifest))
  output-file)

(def cli-options
  [["-o" "--output FILE" "Output Turtle file. Defaults to stdout."]])

(defn usage []
  (binding [*out* *err*]
    (println "Usage: clojure -M:abc/manifest-to-rdf <manifest.json> [-o output.ttl]")))

(defn -main [& args]
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        [manifest-path & extra] arguments]
    (if (or (seq errors) (nil? manifest-path) (seq extra))
      (do
        (doseq [error errors]
          (binding [*out* *err*]
            (println error)))
        (usage)
        (System/exit 2))
      (let [ttl (manifest->ttl (files/read-json manifest-path))]
        (if-let [output (:output options)]
          (do
            (io/make-parents output)
            (spit (io/file output) ttl)
            (println "wrote RDF Turtle view to" output))
          (print ttl))))))
