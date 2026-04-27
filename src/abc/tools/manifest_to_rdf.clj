(ns abc.tools.manifest-to-rdf
  "Convert ABC design-bundle manifests to deterministic RDF/Turtle using
  Apache Jena (via Aristotle) for graph construction."
  (:require ;; logging first to install SLF4J ns-filter before Aristotle
            ;; pulls in Jena and SSHD.
   [abc.tools.logging :as logging]
   [abc.tools.files :as files]
   [abc.tools.rdf-prefixes :as rdf-prefixes]
   [arachne.aristotle :as aa]
   [arachne.aristotle.registry :as reg]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.tools.cli :as cli]
   [taoensso.telemere :as tel])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph Node Triple NodeFactory]
           [java.io ByteArrayOutputStream]))

;; ---------------------------------------------------------------------------
;; Prefix declarations
;; ---------------------------------------------------------------------------

(defn- ensure-prefixes! []
  (rdf-prefixes/ensure!))

;; ---------------------------------------------------------------------------
;; IRI helpers — angle-bracket strings become URI nodes in Aristotle
;; ---------------------------------------------------------------------------

(def default-base-iri "https://w3id.org/abc/")

(defn- hash-token [hash-value]
  (string/replace hash-value ":" "-"))

(defn- artifact-iri
  ([hash-value]
   (artifact-iri default-base-iri hash-value))
  ([base-iri hash-value]
   (str "<" base-iri "artifact/" (hash-token hash-value) ">")))

(defn- agent-iri [agent base-iri]
  (str "<" base-iri "agent/" (string/replace agent "." "-") ">"))

;; ---------------------------------------------------------------------------
;; Serializer helpers
;; ---------------------------------------------------------------------------

(def ^:private xsd-string-uri   (.getURI XSDDatatype/XSDstring))
(def ^:private xsd-dateTime-uri (.getURI XSDDatatype/XSDdateTime))
(def ^:private rdf-type-uri     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

(defn- kw->ttl [kw]
  (str (namespace kw) ":" (name kw)))

(defn- node->ttl [^Node node]
  (cond
    (.isURI node)
    (let [uri (.getURI node)]
      (if-let [kw (reg/kw uri)]
        (kw->ttl kw)
        (str "<" uri ">")))

    (.isLiteral node)
    (let [lex  (.getLiteralLexicalForm node)
          dt   (.getLiteralDatatypeURI node)
          lang (.getLiteralLanguage node)]
      (cond
        (and dt (= dt xsd-string-uri))
        (str "\"" (string/escape lex {\" "\\\""}) "\"")

        (and dt (= dt xsd-dateTime-uri))
        (str "\"" lex "\"^^xsd:dateTime")

        (seq lang)
        (str "\"" (string/escape lex {\" "\\\""}) "\"@" lang)

        dt
        (str "\"" (string/escape lex {\" "\\\""}) "\"^^"
             (if-let [kw (reg/kw dt)] (kw->ttl kw) (str "<" dt ">")))

        :else
        (str "\"" (string/escape lex {\" "\\\""}) "\"")))

    (.isBlank node)
    (str "_:" (.getBlankNodeLabel node))

    :else
    (str node)))

(defn- predicate->ttl [^Node pred]
  (let [uri (.getURI pred)]
    (if (= uri rdf-type-uri)
      "a"
      (if-let [kw (reg/kw uri)]
        (kw->ttl kw)
        (str "<" uri ">")))))

;; ---------------------------------------------------------------------------
;; Deterministic Turtle serialisation
;;
;; Jena's default Turtle writer iteration order depends on internal hashes,
;; so we walk the graph ourselves to produce stable, canonical output.
;; ---------------------------------------------------------------------------

(defn graph->ttl
  "Serialize a Jena graph to deterministic RDF/Turtle. Public so that
  abc.tools.metadata-record/record->ttl and any future consumer can
  compose graph builders with this serializer without re-deriving the
  graph."
  [^org.apache.jena.graph.Graph graph]
  (let [all-triples (iterator-seq (.find graph))
        by-subj     (group-by #(.getSubject ^Triple %) all-triples)

        ;; Blank nodes that are objects of exactly one triple can be inlined
        object-counts
        (frequencies (keep #(let [o (.getObject ^Triple %)]
                              (when (.isBlank o) o))
                           all-triples))
        inlineable-blanks
        (set (keep (fn [[node cnt]] (when (= cnt 1) node))
                   object-counts))

        ;; Stable sequential IDs for any remaining blank nodes
        blank-ids
        (zipmap
         (sort-by #(.getBlankNodeLabel ^Node %) (keys object-counts))
         (map #(str "b" %) (range)))

        stable-node
        (fn [^Node n]
          (if (and (.isBlank n) (not (inlineable-blanks n)))
            (str "_:" (get blank-ids n))
            (node->ttl n)))

        stable-pred predicate->ttl

        subject-order
        (fn [^Node s]
          (cond
            (.isBlank s)
            [2 (get blank-ids s "")]
            :else
            (let [uri (.getURI s)]
              [(cond
                 (string/starts-with? uri (str default-base-iri "artifact/")) 0
                 (string/starts-with? uri (str default-base-iri "activity/")) 1
                 :else 2)
               uri])))

        sorted-subjs (sort-by subject-order (keys by-subj))

        ;; Inline a blank node as [ p o ; p o ]
        inline-blank
        (fn [^Node bnode]
          (let [btriples   (get by-subj bnode)
                by-pred    (group-by #(.getPredicate ^Triple %) btriples)
                preds      (sort-by (fn [^Node p]
                                      (let [uri (.getURI p)]
                                        (if (= uri rdf-type-uri)
                                          [0 ""]
                                          [1 uri])))
                                    (keys by-pred))
                clauses    (mapcat
                            (fn [pred-node]
                              (let [objs (map #(.getObject ^Triple %)
                                              (get by-pred pred-node))
                                    sorted-objs (sort-by (comp string/lower-case node->ttl) objs)]
                                (map
                                 (fn [obj]
                                   (str " " (stable-pred pred-node)
                                        " " (stable-node obj)))
                                 sorted-objs)))
                            preds)]
            (str "["
                 (string/join " ;" clauses)
                 " ]")))

        render-subject
        (fn [subj]
          (if (and (.isBlank subj) (inlineable-blanks subj))
            nil  ;; inlined elsewhere, skip standalone
            (let [triples     (get by-subj subj)
                  by-pred     (group-by #(.getPredicate ^Triple %) triples)
                  pred-order  (fn [^Node p]
                                (let [uri (.getURI p)]
                                  (if (= uri rdf-type-uri)
                                    [0 ""]
                                    [1 uri])))
                  sorted-preds (sort-by pred-order (keys by-pred))
                  pred-clauses
                  (map-indexed
                   (fn [pred-idx pred-node]
                     (let [objs        (map #(.getObject ^Triple %) (get by-pred pred-node))
                           sorted-objs (sort-by (comp string/lower-case node->ttl) objs)
                           pred-name   (stable-pred pred-node)
                           last-pred?  (= pred-idx (dec (count sorted-preds)))]
                       (map-indexed
                        (fn [obj-idx obj]
                          (str "  " pred-name " "
                               (if (and (.isBlank obj) (inlineable-blanks obj))
                                 (inline-blank obj)
                                 (stable-node obj))
                               (if (and last-pred? (= obj-idx (dec (count sorted-objs))))
                                 " ."
                                 " ;")))
                        sorted-objs)))
                   sorted-preds)]
              (cons (stable-node subj) (apply concat pred-clauses)))))

        body (rest
              (mapcat
               (fn [subj]
                 (let [lines (render-subject subj)]
                   (if lines (cons "" lines) [])))
               sorted-subjs))]
    (string/join
     "\n"
     (concat
      ["@prefix abc: <https://w3id.org/abc/> ."
       "@prefix dcterms: <http://purl.org/dc/terms/> ."
       "@prefix prov: <http://www.w3.org/ns/prov#> ."
       "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> ."
       ""]
      body
      [""]))))

;; ---------------------------------------------------------------------------
;; Timestamp -> explicit xsd:dateTime literal node
;; ---------------------------------------------------------------------------

(defn- ->xsd-datetime [iso-timestamp]
  (NodeFactory/createLiteral iso-timestamp XSDDatatype/XSDdateTime))

;; ---------------------------------------------------------------------------
;; Manifest -> RDF graph
;; ---------------------------------------------------------------------------

(defn manifest->graph
  "Convert a manifest (JSON parsed as string-keyed map) into an Apache Jena
  Graph via Aristotle."
  ([manifest]
   (manifest->graph manifest {}))
  ([manifest {:keys [base-iri] :or {base-iri default-base-iri}}]
   (ensure-prefixes!)
   (let [artifact-id    (get manifest "artifact_id")
         artifact-kind  (get manifest "artifact_kind")
         failure?       (= "failure" artifact-kind)
         content        (get manifest "content")
         provenance     (get manifest "provenance")
         activity-id    (get provenance "activity_id")
         schema-hash    (get-in manifest ["manifest_identity_object" "manifest_schema_hash"])
         agent-str      (get provenance "agent")
         plan-hash      (get provenance "plan_hash")
         generated-at   (get provenance "generated_at")

         artifact-uri   (artifact-iri base-iri artifact-id)
         activity-uri   (str "<" activity-id ">")

         derived        (->> (get provenance "was_derived_from")
                             sort
                             (map #(artifact-iri base-iri %)))
         used           (->> (get provenance "used")
                             sort
                             (map #(artifact-iri base-iri %)))
         sidecars       (->> (get manifest "sidecars")
                             (sort-by (juxt #(get % "role")
                                            #(get % "hash")
                                            #(get % "path_hint"))))

         {errors-sidecars "errors"
          other-sidecars  :other}
         (group-by (fn [sc]
                     (if (= "errors" (get sc "role")) "errors" :other))
                   sidecars)
         sidecar-iris (fn [scs]
                        (mapv #(artifact-iri base-iri (get % "hash")) scs))

         artifact-types (cond-> [:abc/Artifact :prov/Entity]
                          failure? (conj :abc/FailureArtifact))

         artifact-data
         (merge
          {:rdf/about            artifact-uri
           :rdf/type             artifact-types
           :abc/artifactId       artifact-id
           :abc/artifactKind     artifact-kind
           :abc/schemaHash       schema-hash
           :abc/validationStatus (get manifest "validation_status")
           :prov/generatedAtTime (->xsd-datetime generated-at)}

          (when content
            {:abc/contentHash (get content "content_hash")
             :dcterms/format  (get content "media_type")})

          (when (seq derived)
            {:prov/wasDerivedFrom derived})

          {:prov/wasGeneratedBy activity-uri}

          (when (seq other-sidecars)
            {:abc/hasSidecar (sidecar-iris other-sidecars)})

          (when (seq errors-sidecars)
            {:abc/hasErrorArtifact (sidecar-iris errors-sidecars)}))

         association-data
         (merge
          {:rdf/type   :prov/Association
           :prov/agent (agent-iri agent-str base-iri)}
          (when plan-hash
            {:prov/hadPlan (artifact-iri base-iri plan-hash)}))

         activity-data
         (merge
          {:rdf/about                 activity-uri
           :rdf/type                  :prov/Activity
           :prov/qualifiedAssociation association-data}
          (when (seq used)
            {:prov/used used}))

         sidecar-data
         (mapv
          (fn [sc]
            (let [sc-uri (artifact-iri base-iri (get sc "hash"))]
              {:rdf/about           sc-uri
               :rdf/type            :prov/Entity
               :abc/schemaHash      schema-hash
               :abc/sidecarRole     (get sc "role")
               :abc/contentHash     (get sc "hash")
               :dcterms/format      (get sc "media_type")
               :prov/wasGeneratedBy activity-uri
               :prov/wasDerivedFrom artifact-uri}))
          sidecars)]

     (-> (aa/graph :simple)
         (aa/add artifact-data)
         (aa/add activity-data)
         (aa/add sidecar-data)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn manifest->ttl
  "Convert an ABC manifest (JSON parsed as string-keyed map) to an RDF/Turtle
  string using Apache Jena via Aristotle."
  ([manifest]
   (manifest->ttl manifest {}))
  ([manifest opts]
   (-> (manifest->graph manifest opts)
       (graph->ttl))))

(defn write-ttl-file! [output-file manifest]
  (io/make-parents output-file)
  (spit (io/file output-file) (manifest->ttl manifest))
  output-file)

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(def cli-options
  [["-o" "--output FILE" "Output Turtle file. Defaults to stdout."]])

(defn usage []
  (tel/log! :warn "Usage: clojure -M:abc/manifest-to-rdf <manifest.json> [-o output.ttl]"))

(defn -main [& args]
  (logging/install-cli-handler!)
  (let [{:keys [options arguments errors]} (cli/parse-opts args cli-options)
        [manifest-path & extra] arguments]
    (if (or (seq errors) (nil? manifest-path) (seq extra))
      (do
        (doseq [error errors]
          (tel/log! :error error))
        (usage)
        (System/exit 2))
      (let [ttl (manifest->ttl (files/read-json manifest-path))]
        (if-let [output (:output options)]
          (do
            (io/make-parents output)
            (spit (io/file output) ttl)
            (tel/log! :info (str "wrote RDF Turtle view to " output)))
          ;; Turtle goes to stdout as primary tool output, not a log event.
          (print ttl))))))
