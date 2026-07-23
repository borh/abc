(ns abc.tools.schema-validation-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.iiif :as iiif]
            [abc.tools.jcs :as jcs]
            [abc.tools.linked-art :as linked-art]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [abc.tools.shacl :as shacl]
            [abc.tools.validate-design-bundle :as validate]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [org.apache.jena.graph NodeFactory Triple]))

(def ^:private context-path "contexts/abc-v0.jsonld")
(def ^:private manifest-path "examples/v0/example-work/manifest.json")
(def ^:private metadata-record-path "examples/v0/example-work/metadata-record.json")
(def ^:private candidate-path
  "examples/v0/example-work/lod/linked-art-candidate.jsonld")
(def ^:private expanded-path
  "examples/v0/example-work/lod/linked-art-expanded.normalized.json")
(def ^:private linked-art-result-path
  "examples/v0/example-work/lod/jsonld-context-validation-result.json")

(def ^:private turtle-prefix-paths
  ["examples/v0/example-work/failure-manifest.example.ttl"
   "examples/v0/example-work/lod/manifest.prov.ttl"
   "examples/v0/example-work/manifest.ttl"
   "examples/v0/example-work/metadata-record.ttl"
   "fixtures/v0/invalid/drift/merge-cardinality-one-predecessor/graph.ttl"
   "fixtures/v0/invalid/drift/rdf-participant-prov-mismatch/graph.ttl"
   "fixtures/v0/invalid/drift/shacl-missing-date/graph.ttl"
   "fixtures/v0/invalid/drift/split-cardinality-one-successor/graph.ttl"
   "fixtures/v0/invalid/drift/typing-missing-activity/graph.ttl"
   "fixtures/v0/invalid/drift/typing-missing-subclass/graph.ttl"
   "resources/abc/tools/manifest_to_rdf/example_manifest.ttl"
   "schemas/manifest.shacl.ttl"])

(defn- linked-art-output-paths [dir]
  {:candidate-path (str (io/file (str dir) "candidate.jsonld"))
   :expanded-path (str (io/file (str dir) "expanded.json"))
   :result-path (str (io/file (str dir) "result.json"))})

(defn- write-linked-art! [dir]
  (let [outputs (linked-art-output-paths dir)]
    (linked-art/write-publication-view!
     (merge {:manifest-path manifest-path
             :metadata-record-path metadata-record-path
             :context-path context-path}
            outputs))
    outputs))

(defn- assert-linked-art-parity! [outputs]
  (doseq [[committed generated]
          [[candidate-path (:candidate-path outputs)]
           [expanded-path (:expanded-path outputs)]
           [linked-art-result-path (:result-path outputs)]]]
    (is (= (seq (files/read-bytes committed))
           (seq (files/read-bytes generated))))))

(defn- example-metadata []
  {:record (files/read-json metadata-record-path)
   :persons {"000879" (files/read-json
                       "examples/v0/example-persons/000879.json")}})

(defn- shapes-graph []
  (files/read-text "schemas/manifest.shacl.ttl")
  (shacl/load-shapes-graph))

(defn- validate-rights-values! [rights-values]
  (let [{:keys [record persons]} (example-metadata)
        graph (metadata-record/record+persons->graph record persons)
        rdf-type (NodeFactory/createURI
                  "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        document-type (NodeFactory/createURI
                       "http://purl.org/ontology/bibo/Document")
        subject (some-> (.find graph nil rdf-type document-type)
                        iterator-seq first .getSubject)
        rights-predicate (NodeFactory/createURI
                          "http://purl.org/dc/terms/rights")]
    (doseq [rights rights-values]
      (.add graph (Triple/create subject rights-predicate
                                 (NodeFactory/createURI rights))))
    (shacl/validate! {:shapes-graph (shapes-graph)
                      :data-graph graph
                      :label "metadata-rights"})))

(defn write-canonicalization-fixtures! [identity-json array-a array-b]
  (files/write-text! identity-json "{}\n")
  (files/write-text! array-a "[]\n")
  (files/write-text! array-b "[1]\n"))

(deftest broken-manifest-is-rejected-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (seq (validate/validation-errors schema {})))))

(deftest canonicalization-hash-mismatch-is-rejected-test
  (fs/with-temp-dir [dir {:prefix "abc-test-"}]
    (let [identity-json (io/file (str dir) "identity.json")
          array-a (io/file (str dir) "array-a.json")
          array-b (io/file (str dir) "array-b.json")]
      (write-canonicalization-fixtures! identity-json array-a array-b)
      (let [error (try
                    (validate/validate-canonicalization!
                     {:expected (apply str (repeat 64 "0"))
                      :identity-json (str identity-json)
                      :array-a (str array-a)
                      :array-b (str array-b)})
                    nil
                    (catch clojure.lang.ExceptionInfo exception exception))]
        (is (instance? clojure.lang.ExceptionInfo error))
        (is (re-find #"canonical identity fixture hash mismatch"
                     (ex-message error)))))))

(deftest supported-entrypoint-delegates-test
  (is (str/includes? (files/read-text "bin/validate-design-bundle.sh")
                     "exec clojure -M:abc/validate-design-bundle")))

(deftest supported-ci-wiring-test
  (is (str/includes? (files/read-text ".github/workflows/validation.yml")
                     ".#validate-design-bundle")))

(deftest linked-art-parity-test
  (fs/with-temp-dir [dir {:prefix "abc-test-"}]
    (assert-linked-art-parity! (write-linked-art! dir))))

(deftest linked-art-result-status-and-context-hash-test
  (fs/with-temp-dir [dir {:prefix "abc-test-"}]
    (let [outputs (write-linked-art! dir)
          result (files/read-json (:result-path outputs))
          recomputed (str "sha256:"
                          (hash/sha256-bytes
                           (jcs/canonical-json-bytes
                            (files/read-json context-path))))]
      (is (= "ok" (get result "status")))
      (is (= recomputed (get result "context_hash"))))))

(deftest linked-art-determinism-parity-hash-identity-fetch-contract-test
  (fs/with-temp-dir [dir-a {:prefix "abc-test-"}]
    (fs/with-temp-dir [dir-b {:prefix "abc-test-"}]
      (let [a (write-linked-art! dir-a)
            b (write-linked-art! dir-b)
            result (files/read-json (:result-path a))
            manifest-id (get (files/read-json manifest-path) "artifact_id")]
        (doseq [key [:candidate-path :expanded-path :result-path]]
          (is (= (seq (files/read-bytes (get a key)))
                 (seq (files/read-bytes (get b key))))))
        (assert-linked-art-parity! a)
        (is (= (linked-art/context-hash context-path)
               (get result "context_hash")))
        (is (= {"manifest_artifact_id" manifest-id
                "expanded_artifact_id" manifest-id
                "preserved_after_expansion" true}
               (get result "identity_invariant")))
        (let [error (try
                      (linked-art/expand-document
                       (.getBytes
                        "{\"@context\":\"https://example.invalid/other.jsonld\"}"
                        "UTF-8")
                       (files/read-bytes context-path)
                       "https://w3id.org/abc/test")
                      nil
                      (catch Exception exception exception))]
          (is (instance? Exception error)))))))

(deftest context-declares-canonical-abc-namespace-test
  (is (= "https://w3id.org/abc/"
         (get-in (files/read-json context-path) ["@context" "abc"]))))

(deftest expanded-artifact-id-uses-canonical-predicate-test
  (let [expanded (files/read-json expanded-path)
        manifest-id (get (files/read-json manifest-path) "artifact_id")]
    (is (= manifest-id
           (get-in expanded
                   ["expanded" 0 "https://w3id.org/abc/artifactId"
                    0 "@value"])))))

(deftest current-linked-art-parity-and-context-hash-contract-test
  (fs/with-temp-dir [dir {:prefix "abc-test-"}]
    (let [outputs (write-linked-art! dir)
          result (files/read-json linked-art-result-path)]
      (assert-linked-art-parity! outputs)
      (is (= (linked-art/context-hash context-path)
             (get result "context_hash"))))))

(deftest applicability-schema-is-valid-draft-2020-12-test
  (let [path "schemas/iiif-applicability.schema.json"
        value (files/read-json path)]
    (is (= "https://json-schema.org/draft/2020-12/schema"
           (get value "$schema")))
    (is (nil? (schema/schema-valid! value path)))))

(deftest iiif-committed-applicability-contract-test
  (let [path "examples/v0/example-work/iiif/applicability.json"
        value (files/read-json path)]
    (is (nil? (iiif/validate-applicability! path)))
    (is (contains? value "derived_manifest"))
    (is (= (= "applicable" (get value "status"))
           (string? (get value "derived_manifest"))))))

(deftest iiif-text-only-values-contract-test
  (let [value (files/read-json
               "examples/v0/example-work/iiif/applicability.json")]
    (is (contains? value "derived_manifest"))
    (is (= ["000127" "not_applicable" nil]
           [(get value "work_id")
            (get value "status")
            (get value "derived_manifest")]))
    (is (re-find #"text-only" (get value "reason")))))

(deftest iiif-invalid-combinations-contract-test
  (doseq [path ["fixtures/iiif/invalid/missing-work-id.json"
                "fixtures/iiif/invalid/applicable-without-manifest.json"
                "fixtures/iiif/invalid/not-applicable-with-manifest.json"]]
    (let [error (try
                  (iiif/validate-applicability! path)
                  nil
                  (catch clojure.lang.ExceptionInfo exception exception))]
      (is (instance? clojure.lang.ExceptionInfo error))
      (is (re-find #"JSON Schema validation failed" (ex-message error))))))

(deftest iiif-valid-combinations-contract-test
  (is (nil? (iiif/validate-applicability!
             "fixtures/iiif/applicable.json")))
  (is (nil? (iiif/validate-applicability!
             "fixtures/iiif/rights_blocker.json"))))

(deftest bounded-turtle-prefix-inventory-test
  (doseq [path turtle-prefix-paths]
    (is (str/includes? (files/read-text path)
                       "@prefix abc: <https://w3id.org/abc/> ."))))

(deftest manifest-rdf-parity-test
  (doseq [[manifest ttl]
          [["examples/v0/example-work/manifest.json"
            "examples/v0/example-work/manifest.ttl"]
           ["examples/v0/example-work/failure-manifest.example.json"
            "examples/v0/example-work/failure-manifest.example.ttl"]]]
    (is (= (files/read-text ttl)
           (manifest-to-rdf/manifest->ttl (files/read-json manifest))))))

(deftest metadata-rdf-parity-test
  (let [{:keys [record persons]} (example-metadata)]
    (is (= (files/read-text
            "examples/v0/example-work/metadata-record.ttl")
           (metadata-record/record+persons->ttl record persons)))))

(deftest metadata-title-and-legacy-predicate-containment-test
  (let [{:keys [record persons]} (example-metadata)
        graph (metadata-record/record+persons->graph record persons)
        title-triples (iterator-seq
                       (.find graph nil
                              (NodeFactory/createURI
                               "http://ndl.go.jp/dcndl/terms/titleTranscription")
                              nil))]
    (is (= 1 (count title-triples)))
    (is (= "らしょうもん"
           (some-> title-triples first .getObject .getLiteralLexicalForm)))
    (is (empty? (iterator-seq
                 (.find graph nil
                        (NodeFactory/createURI "https://w3id.org/abc/reading")
                        nil))))
    (is (empty? (iterator-seq
                 (.find graph nil
                        (NodeFactory/createURI
                         "https://w3id.org/abc/copyrightExpired")
                        nil))))
    (doseq [flag [true false nil]]
      (let [case-graph (metadata-record/record->graph
                        (assoc-in record ["work" "copyright_expired"] flag))
            rights (iterator-seq
                    (.find case-graph nil
                           (NodeFactory/createURI
                            "http://purl.org/dc/terms/rights")
                           nil))]
        (is (empty? rights))))))

(deftest optional-rights-shacl-contract-test
  (is (= :ok (validate-rights-values! [])))
  (is (= :ok (validate-rights-values!
              ["https://creativecommons.org/publicdomain/mark/1.0/"])))
  (let [invalid (try
                  (validate-rights-values! ["https://example.invalid/rights"])
                  nil
                  (catch clojure.lang.ExceptionInfo exception exception))
        duplicate (try
                    (validate-rights-values!
                     ["https://creativecommons.org/publicdomain/mark/1.0/"
                      "http://rightsstatements.org/vocab/InC/1.0/"])
                    nil
                    (catch clojure.lang.ExceptionInfo exception exception))]
    (is (instance? clojure.lang.ExceptionInfo invalid))
    (is (instance? clojure.lang.ExceptionInfo duplicate))))

(deftest metadata-bundle-helper-contract-test
  (let [args {:record-path metadata-record-path
              :manifest-path manifest-path
              :persons-dir "examples/v0/example-persons"
              :record-schema-path "schemas/metadata-record.schema.json"
              :person-schema-path "schemas/person-record.schema.json"
              :ttl-path "examples/v0/example-work/metadata-record.ttl"
              :shapes-graph (shapes-graph)}]
    (files/read-json "schemas/metadata-record.schema.json")
    (files/read-json "schemas/person-record.schema.json")
    (files/read-text "examples/v0/example-work/metadata-record.ttl")
    (is (nil? (validate/validate-metadata-bundle! args)))))
