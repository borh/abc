(ns abc.tools.shacl-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.person-record :as person-record]
            [abc.tools.shacl :as shacl]
            [arachne.aristotle :as aa]
            [arachne.aristotle.registry :as reg]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [org.apache.jena.datatypes BaseDatatype]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph Graph Node NodeFactory]))

;; Register prefixes so keyword-based map literals resolve to the correct IRIs.
;; Idempotent; safe at load time.
(reg/prefix 'abc     "https://w3id.org/abc/")
(reg/prefix 'dcterms "http://purl.org/dc/terms/")
(reg/prefix 'foaf    "http://xmlns.com/foaf/0.1/")
(reg/prefix 'prov    "http://www.w3.org/ns/prov#")

(deftest load-shapes-graph-test
  (testing "loads the manifest SHACL shapes file as a Jena graph"
    (let [g (shacl/load-shapes-graph)]
      (is (instance? Graph g))
      (is (pos? (count (iterator-seq (.find g))))
          "shapes graph must contain triples"))))

(deftest validate-success-manifest-conforms-test
  (testing "example success manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/manifest.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/manifest.json"}))))))

(deftest validate-failure-manifest-conforms-test
  (testing "example failure manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/failure-manifest.example.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/failure-manifest.example.json"}))))))

(def example-activity
  {:rdf/about "<https://w3id.org/abc/activity/example>"
   :rdf/type :prov/Activity
   :prov/used ["<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>"]
   :prov/qualifiedAssociation {:rdf/type :prov/Association
                               :prov/agent "<https://w3id.org/abc/agent/test>"}})

(defn- graph-with-artifact
  "Build a Jena graph containing the supplied artifact node plus the shared
  example Activity. The caller supplies the full artifact map; this helper
  does not merge or remove keys."
  [artifact]
  (-> (aa/graph :simple)
      (aa/add artifact)
      (aa/add example-activity)))

(def artifact-uri
  "<https://w3id.org/abc/artifact/sha256-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>")

(def derived-uri
  "<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>")

(deftest validate-missing-artifact-id-test
  (testing "missing abc:artifactId triggers ArtifactShape violation"
    (let [shapes (shacl/load-shapes-graph)
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity]
                 ;; deliberately no :abc/artifactId
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "passed"
                 :abc/contentHash      "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                 :dcterms/format       "application/json"
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "missing-id"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors) "must report at least one violation")
            (is (some #(re-find #"artifactId|ArtifactShape"
                                (str (:source %) " " (:path %) " " (:message %)))
                      errors)
                "violation must reference artifactId or ArtifactShape")))))))

(deftest validate-failure-without-error-artifact-test
  (testing "FailureArtifact missing hasErrorArtifact triggers FailureShape violation"
    (let [shapes (shacl/load-shapes-graph)
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity :abc/FailureArtifact]
                 :abc/artifactId       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "failed"
                 ;; deliberately no :abc/hasErrorArtifact
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "failure-no-errors"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors))
            (is (some #(re-find #"FailureShape|hasErrorArtifact"
                                (str (:source %) " " (:path %) " " (:message %)))
                      errors)
                "violation must reference FailureShape or hasErrorArtifact")))))))

(deftest validate-bad-status-enum-test
  (testing "abc:validationStatus outside the sh:in enum reports a violation tied to validationStatus"
    (let [shapes (shacl/load-shapes-graph)
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity]
                 :abc/artifactId       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "unknown"
                 :abc/contentHash      "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                 :dcterms/format       "application/json"
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "bad-status"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors))
            (is (some (fn [v]
                        (let [ctx (str (:source v) " " (:path v) " " (:message v))]
                          (and (re-find #"validationStatus" ctx)
                               (not (re-find #"hasErrorArtifact|artifactId" ctx)))))
                      errors)
                "must report a violation that specifically targets validationStatus, not hasErrorArtifact or artifactId")))))))

(defn- shape-iris [shapes]
  (let [triples (iterator-seq (.find shapes))]
    (into #{}
          (comp (map #(.getSubject %))
                (filter #(.isURI %))
                (map #(.getURI %)))
          triples)))

(deftest person-record-shape-loaded-test
  (testing "PersonRecordShape is loaded from manifest.shacl.ttl"
    (is (contains? (shape-iris (shacl/load-shapes-graph))
                   "https://w3id.org/abc/PersonRecordShape"))))

(def ^:private rdf-first
  (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))
(def ^:private rdf-rest
  (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))
(def ^:private rdf-nil
  (NodeFactory/createURI "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
(def ^:private sh-path
  (NodeFactory/createURI "http://www.w3.org/ns/shacl#path"))
(def ^:private sh-or
  (NodeFactory/createURI "http://www.w3.org/ns/shacl#or"))
(def ^:private sh-datatype
  (NodeFactory/createURI "http://www.w3.org/ns/shacl#datatype"))
(def ^:private sh-max-count
  (NodeFactory/createURI "http://www.w3.org/ns/shacl#maxCount"))
(def ^:private rdag2-date-of-birth
  (NodeFactory/createURI "http://RDVocab.info/ElementsGr2/dateOfBirth"))
(def ^:private abc-edtf-date-of-birth
  (NodeFactory/createURI "https://w3id.org/abc/edtfDateOfBirth"))
(def ^:private abc-edtf
  (NodeFactory/createURI "https://w3id.org/abc/EDTF"))
(def ^:private xsd-date
  (NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#date"))
(def ^:private xsd-g-year-month
  (NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#gYearMonth"))
(def ^:private xsd-g-year
  (NodeFactory/createURI "http://www.w3.org/2001/XMLSchema#gYear"))

(defn- objects [^Graph graph subject predicate]
  (mapv #(.getObject %)
        (iterator-seq (.find graph subject predicate Node/ANY))))

(defn- unique-object [^Graph graph subject predicate]
  (let [matches (objects graph subject predicate)]
    (when-not (= 1 (count matches))
      (throw (ex-info "expected exactly one RDF object"
                      {:subject subject
                       :predicate predicate
                       :objects matches})))
    (first matches)))

(defn- exact-property-shape [^Graph graph path]
  (let [subjects (->> (iterator-seq (.find graph Node/ANY sh-path path))
                      (map #(.getSubject %))
                      set)]
    (when-not (= 1 (count subjects))
      (throw (ex-info "expected exactly one SHACL property shape"
                      {:path path :subjects subjects})))
    (first subjects)))

(defn- rdf-list [^Graph graph head]
  (loop [node head
         seen #{}
         values []]
    (cond
      (= rdf-nil node) values
      (contains? seen node)
      (throw (ex-info "cyclic RDF list" {:head head :node node}))
      :else
      (recur (unique-object graph node rdf-rest)
             (conj seen node)
             (conj values (unique-object graph node rdf-first))))))

(defn- datatype-alternatives [^Graph graph property-shape]
  (->> (rdf-list graph (unique-object graph property-shape sh-or))
       (map #(unique-object graph % sh-datatype))
       set))

(deftest person-record-temporal-shape-contract-test
  (let [g (shacl/load-shapes-graph)
        ttl (files/read-text "schemas/manifest.shacl.ttl")
        dob-property (exact-property-shape g rdag2-date-of-birth)
        edtf-property (exact-property-shape g abc-edtf-date-of-birth)]
    (is (= #{xsd-date xsd-g-year-month xsd-g-year}
           (datatype-alternatives g dob-property)))
    (is (= #{(NodeFactory/createLiteral "1" XSDDatatype/XSDinteger)}
           (set (objects g edtf-property sh-max-count))))
    (is (= #{abc-edtf}
           (set (objects g edtf-property sh-datatype))))
    (is (string/includes? ttl
                          "sh:pattern \"^(-?\\\\d{4}"))
    (is (string/includes? ttl "|-?\\\\d{3}X|-?\\\\d{2}XX)"))
    (is (pos? (count (iterator-seq (.find g)))))))

(deftest validate-abc-local-person-record-conforms-test
  (testing "PersonRecordShape accepts the ABC-local identifier RDF emitted by person-record"
    (let [record {"schema_id" person-record/schema-id
                  "schema_hash" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                  "person_id" "abc-000000000001"
                  "family_name" "Test"
                  "given_name" "Person"
                  "family_name_reading" nil
                  "given_name_reading" nil
                  "family_name_sort" nil
                  "given_name_sort" nil
                  "family_name_romaji" nil
                  "given_name_romaji" nil
                  "date_of_birth" nil
                  "date_of_death" nil
                  "external_links" []
                  "source_csv_provenance" nil}
          shapes (shacl/load-shapes-graph)
          data (person-record/record->graph record)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "abc-local-person"}))))))

(deftest metadata-record-person-shape-retired-test
  (testing "MetadataRecordPersonShape is no longer in the shapes graph"
    (is (not (contains? (shape-iris (shacl/load-shapes-graph))
                        "https://w3id.org/abc/MetadataRecordPersonShape")))))

(def ^:private edtf-datatype
  (BaseDatatype. "https://w3id.org/abc/EDTF"))

(defn- edtf-literal [^String s]
  (NodeFactory/createLiteral s edtf-datatype))

(defn- xsd-int-literal [^String s]
  (NodeFactory/createLiteral s XSDDatatype/XSDint))

(deftest validate-malformed-edtf-date-of-birth-test
  (testing "abc:edtfDateOfBirth that fails the v0 lexical pattern reports a PersonRecordShape violation tied to edtfDateOfBirth"
    (let [shapes (shacl/load-shapes-graph)
          ;; "1892?" carries the EDTF datatype (so sh:datatype passes) but
          ;; uses an EDTF Level 1 uncertainty marker that's outside v0's
          ;; constrained grammar, so sh:pattern must fire.
          person {:rdf/about           "<http://www.aozora.gr.jp/index_pages/person001234.html>"
                  :rdf/type            [:foaf/Person]
                  :dcterms/identifier  (xsd-int-literal "1234")
                  :foaf/familyName     "Test"
                  :foaf/givenName      "Person"
                  :foaf/name           "Test Person"
                  :abc/edtfDateOfBirth (edtf-literal "1892?")}
          data (-> (aa/graph :simple) (aa/add person))]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data
                          :label "bad-edtf-dob"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors) "must report at least one violation")
            (is (some (fn [v]
                        (let [ctx (str (:source v) " " (:path v) " " (:message v))]
                          (and (re-find #"edtfDateOfBirth|PersonRecordShape" ctx)
                               (re-find #"(?i)pattern|1892" ctx))))
                      errors)
                "violation must reference edtfDateOfBirth/PersonRecordShape and the pattern failure")))))))
