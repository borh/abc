(ns abc.tools.manifest-to-rdf-test
  (:require [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def example-manifest
  {"artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
   "artifact_kind" "parser-ir"
   "validation_status" "warning"
   "manifest_identity_object" {"manifest_schema_hash" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"}
   "content" {"content_hash" "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
              "media_type" "application/json"}
   "sidecars" [{"role" "warnings"
                "hash" "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
                "media_type" "application/jsonl"
                "path_hint" "warnings.jsonl"}]
   "provenance" {"generated_at" "2026-04-26T00:00:00Z"
                 "activity_id" "https://w3id.org/abc/activity/materialize-imported-parser-ir"
                 "agent" "abc.tools.materialize-import"
                 "plan_hash" nil
                 "used" ["sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"]
                 "was_derived_from" ["sha256:1111111111111111111111111111111111111111111111111111111111111111"]}})

(deftest manifest-to-rdf-is-deterministic-test
  (testing "semantically unordered provenance arrays are sorted in the RDF view"
    (let [reordered (assoc-in example-manifest
                              ["provenance" "used"]
                              ["sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                               "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"])]
      (is (= (manifest-to-rdf/manifest->ttl example-manifest)
             (manifest-to-rdf/manifest->ttl reordered))))))

(deftest manifest-to-rdf-output-test
  (is (= (str "@prefix abc: <https://w3id.org/abc/> .\n"
              "@prefix dcterms: <http://purl.org/dc/terms/> .\n"
              "@prefix prov: <http://www.w3.org/ns/prov#> .\n"
              "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n"
              "\n"
              "<https://w3id.org/abc/artifact/sha256-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>\n"
              "  a prov:Entity ;\n"
              "  abc:artifactId \"sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" ;\n"
              "  abc:artifactKind \"parser-ir\" ;\n"
              "  abc:contentHash \"sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc\" ;\n"
              "  abc:schemaHash \"sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\" ;\n"
              "  abc:validationStatus \"warning\" ;\n"
              "  dcterms:format \"application/json\" ;\n"
              "  prov:generatedAtTime \"2026-04-26T00:00:00Z\"^^xsd:dateTime ;\n"
              "  prov:wasDerivedFrom <https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111> ;\n"
              "  prov:wasGeneratedBy <https://w3id.org/abc/activity/materialize-imported-parser-ir> ;\n"
              "  abc:hasSidecar <https://w3id.org/abc/artifact/sha256-dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd> .\n"
              "\n"
              "<https://w3id.org/abc/activity/materialize-imported-parser-ir>\n"
              "  a prov:Activity ;\n"
              "  prov:used <https://w3id.org/abc/artifact/sha256-eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee> ;\n"
              "  prov:used <https://w3id.org/abc/artifact/sha256-ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff> ;\n"
              "  prov:wasAssociatedWith \"abc.tools.materialize-import\" .\n"
              "\n"
              "<https://w3id.org/abc/artifact/sha256-dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd>\n"
              "  a prov:Entity ;\n"
              "  abc:sidecarRole \"warnings\" ;\n"
              "  abc:contentHash \"sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd\" ;\n"
              "  dcterms:format \"application/jsonl\" .\n")
         (manifest-to-rdf/manifest->ttl example-manifest))))

(deftest write-ttl-file-test
  (let [dir (Files/createTempDirectory "abc-manifest-rdf" (make-array FileAttribute 0))
        output-file (io/file (.toFile dir) "manifest.ttl")]
    (try
      (manifest-to-rdf/write-ttl-file! output-file example-manifest)
      (is (= (manifest-to-rdf/manifest->ttl example-manifest)
             (slurp output-file)))
      (finally
        (doseq [file (reverse (file-seq (.toFile dir)))]
          (.delete file))))))
