(ns abc.tools.shacl-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.shacl :as shacl]
            [arachne.aristotle :as aa]
            [arachne.aristotle.registry :as reg]
            [clojure.test :refer [deftest is testing]])
  (:import [org.apache.jena.graph Graph]))

;; Register prefixes so keyword-based map literals resolve to the correct IRIs.
;; Idempotent; safe at load time.
(reg/prefix 'abc     "https://w3id.org/abc/")
(reg/prefix 'dcterms "http://purl.org/dc/terms/")
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

(deftest metadata-record-person-shape-retired-test
  (testing "MetadataRecordPersonShape is no longer in the shapes graph"
    (is (not (contains? (shape-iris (shacl/load-shapes-graph))
                        "https://w3id.org/abc/MetadataRecordPersonShape")))))
