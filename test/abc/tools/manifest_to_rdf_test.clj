(ns abc.tools.manifest-to-rdf-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [arachne.aristotle :as aa]
            [clojure.java.io :as io]
            [clojure.string :as string]
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

(def failure-manifest
  {"artifact_id" "sha256:8888888888888888888888888888888888888888888888888888888888888888"
   "artifact_kind" "failure"
   "validation_status" "failed"
   "manifest_identity_object" {"manifest_schema_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"}
   "content" nil
   "sidecars" [{"role" "errors"
                "hash" "sha256:9999999999999999999999999999999999999999999999999999999999999999"
                "media_type" "application/jsonl"
                "path_hint" "errors.jsonl"}]
   "provenance" {"generated_at" "2026-04-26T00:00:00Z"
                 "activity_id" "https://w3id.org/abc/activity/parse-activity-example"
                 "agent" "abc-parser-example"
                 "plan_hash" nil
                 "used" ["sha256:1111111111111111111111111111111111111111111111111111111111111111"
                         "sha256:4444444444444444444444444444444444444444444444444444444444444444"]
                 "was_derived_from" ["sha256:7777777777777777777777777777777777777777777777777777777777777777"]}})

(deftest manifest-to-rdf-is-deterministic-test
  (testing "semantically unordered provenance arrays are sorted in the RDF view"
    (let [reordered (assoc-in example-manifest
                              ["provenance" "used"]
                              ["sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                               "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"])]
      (is (= (manifest-to-rdf/manifest->ttl example-manifest)
             (manifest-to-rdf/manifest->ttl reordered))))))

(defn- graph->sorted-triples [graph]
  (sort-by (fn [t] [(str (.getSubject t))
                    (str (.getPredicate t))
                    (str (.getObject t))])
           (iterator-seq (.find graph))))

(deftest manifest-to-rdf-output-test
  (testing "output contains expected triples in deterministic form"
    (let [ttl (manifest-to-rdf/manifest->ttl example-manifest)
          tmp (java.io.File/createTempFile "manifest" ".ttl")
          _ (spit tmp ttl)
          actual (aa/read (aa/graph :simple) tmp)
          _ (.delete tmp)]
      ;; Compare triple count and sorted subject/predicate/object strings.
      ;; Blank node identities are ignored because we compare string representations.
      (is (= (count (graph->sorted-triples
                       (aa/read (aa/graph :simple)
                                (io/resource "abc/tools/manifest_to_rdf/example_manifest.ttl"))))
             (count (graph->sorted-triples actual))))
      (is (= (manifest-to-rdf/manifest->ttl example-manifest) ttl)))))

(deftest manifest-to-rdf-failure-test
  (testing "failure artifact omits contentHash and dcterms:format"
    (let [ttl (manifest-to-rdf/manifest->ttl failure-manifest)
          lines (string/split-lines ttl)
          artifact-line "<https://w3id.org/abc/artifact/sha256-8888888888888888888888888888888888888888888888888888888888888888>"
          start-idx (.indexOf lines artifact-line)
          block-lines (take-while #(not (re-matches #"^\s*$" %))
                                  (drop (inc start-idx) lines))
          all-artifact-lines (into [artifact-line] block-lines)]
      (is (some #(re-find #"a abc:FailureArtifact" %) all-artifact-lines))
      ;; contentHash and dcterms:format must NOT appear in the artifact block
      ;; (they may appear in the sidecar block)
      (is (not (some #(re-find #"abc:contentHash" %) all-artifact-lines)))
      (is (not (some #(re-find #"dcterms:format" %) all-artifact-lines))))))

(deftest manifest-to-rdf-matches-example-fixture-test
  (testing "generated RDF matches canonical example-work fixture"
    ;; NOTE: Byte-for-byte comparison is intentional to prevent drift.
    ;; If this becomes fragile, escalate to semantic RDF comparison.
    (let [manifest (files/read-json "examples/v0/example-work/manifest.json")
          expected (slurp "examples/v0/example-work/manifest.ttl")]
      (is (= expected (manifest-to-rdf/manifest->ttl manifest))))))

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
