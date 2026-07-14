(ns abc.tools.adr-evidence-bootstrap-test
  (:require [abc.tools.adr-evidence-bootstrap :as bootstrap]
            [abc.tools.adr :as adr]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.charset StandardCharsets]
           [java.util Base64]))

(def required-paths
  ["abc/docs/adr/adr-evidence.edn"
   "abc/docs/adr/claim-evidence-compatibility.edn"
   "abc/docs/adr/governance-as-of.edn"
   "abc/docs/adr/adr-claim-migration-baseline.json"
   "abc/docs/adr/adr-claim-migration.edn"
   "abc/src/abc/tools/adr.clj"
   "abc/src/abc/tools/adr_governance.clj"
   "abc/src/abc/tools/adr_evidence.clj"
   "abc/src/abc/tools/adr_evidence_bundle.clj"
   "abc/src/abc/tools/adr_claim_migration.clj"
   "abc/schemas/adr-evidence-run.schema.json"
   "abc/schemas/adr-external-evidence.schema.json"])

(defn- file-entry [text]
  (let [bytes (.getBytes text StandardCharsets/UTF_8)]
    {"sha256" (hash/format-sha256 (hash/sha256-bytes bytes))
     "content_base64" (.encodeToString (Base64/getEncoder) bytes)}))

(defn- valid-snapshot []
  {"schema_version" "abc-adr-evidence-bootstrap-v1"
   "subject" "pre-promotion Accepted ADR corpus excluding ADR 0034"
   "producer_revision" (apply str (repeat 40 "a"))
   "governance_as_of" "2026-07-14"
   "accepted_adr_numbers" [1 2]
   "accepted_adr_count" 2
   "accepted_criterion_count" 7
   "audit_report" {"mode" "enforce" "ok" true "problems" []}
   "files" (into (sorted-map)
                 (map (fn [path] [path (file-entry (str path "\n"))]))
                 required-paths)})

(defn- problem-kinds [value]
  (set (map :kind (bootstrap/validate-snapshot-value value))))

(deftest closed-snapshot-recomputes-every-payload-hash-test
  (let [value (valid-snapshot)]
    (is (empty? (bootstrap/validate-snapshot-value value)))
    (is (contains? (problem-kinds (assoc value "unknown" true))
                   :invalid-bootstrap-schema))
    (is (contains? (problem-kinds
                    (assoc-in value ["files" (first required-paths)
                                     "content_base64"] "YmFk"))
                   :bootstrap-file-hash-mismatch))))

(deftest snapshot-shape-and-authority-fields-fail-closed-test
  (let [value (valid-snapshot)]
    (doseq [[label changed kind]
            [["revision" (assoc value "producer_revision" "bad")
              :invalid-bootstrap-schema]
             ["date" (assoc value "governance_as_of" "2026-02-30")
              :invalid-bootstrap-date]
             ["hash" (assoc-in value ["files" (first required-paths) "sha256"]
                               "sha256:bad")
              :invalid-bootstrap-schema]
             ["base64" (assoc-in value ["files" (first required-paths)
                                        "content_base64"] "***")
              :invalid-bootstrap-base64]
             ["duplicate ADR" (assoc value "accepted_adr_numbers" [1 1])
              :invalid-bootstrap-schema]
             ["count" (assoc value "accepted_adr_count" 3)
              :bootstrap-accepted-count-mismatch]
             ["mode" (assoc-in value ["audit_report" "mode"] "audit")
              :invalid-bootstrap-schema]
             ["not ok" (assoc-in value ["audit_report" "ok"] false)
              :invalid-bootstrap-schema]
             ["problems" (assoc-in value ["audit_report" "problems"]
                                   [{"kind" "bad"}])
              :invalid-bootstrap-schema]]]
      (testing label
        (is (contains? (problem-kinds changed) kind))))))

(deftest snapshot-manifest-requires-governance-roots-and-safe-paths-test
  (let [value (valid-snapshot)]
    (is (contains? (problem-kinds
                    (update value "files" dissoc (first required-paths)))
                   :missing-bootstrap-file))
    (is (contains? (problem-kinds
                    (assoc-in value ["files" "../escape"] (file-entry "x")))
                   :unsafe-bootstrap-path))))

(deftest snapshot-input-paths-close-over-accepted-corpus-and-bundle-inputs-test
  (let [adrs (adr/parse-all "docs/adr")
        registry (files/read-edn "docs/adr/adr-evidence.edn")
        paths (bootstrap/snapshot-input-paths "." ".." adrs registry)
        path-set (set paths)
        first-artifact (some (fn [{:keys [artifact-path]}]
                               (let [artifact (files/read-json artifact-path)]
                                 (when (not= "component-clojure-test-v1"
                                             (get-in artifact ["input_profile" "kind"]))
                                   artifact-path)))
                             (:entries registry))
        artifact (files/read-json first-artifact)
        ordinary-input (first (keys (get artifact "inputs")))]
    (is (= paths (vec (sort (distinct paths)))))
    (is (every? path-set required-paths))
    (is (contains? path-set "abc/docs/adr/0001-manifest-identity.md"))
    (is (not (contains? path-set
                        "abc/docs/adr/0034-typed-evidence-and-lifecycle-closure.md")))
    (is (contains? path-set (str "abc/" first-artifact)))
    (is (contains? path-set (str "abc/" ordinary-input)))
    (is (contains? path-set "abc/docs/adr/adr-graph.mmd"))
    (is (contains? path-set "abc/docs/architecture.mmd"))))
