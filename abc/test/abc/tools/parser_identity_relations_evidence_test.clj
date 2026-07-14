(ns abc.tools.parser-identity-relations-evidence-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize-import]
            [abc.tools.materialize-source-snapshot :as materialize-snapshot]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.source-snapshot-workset :as workset]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(deftest parser-identity-relations-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root ".." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "abc/docs/evidence/adr-capture/parser-identity-relations.edn"
                  :value (update (files/read-edn "docs/evidence/adr-capture/parser-identity-relations.edn")
                                 :runtime-input-manifest #(str "abc/" %))}}
    (fn []
      (doseq [path ["docs/adr/0033-source-bundle-identity.md" "examples/ab-validator-output/manifest-inputs.json"
                    "examples/ab-validator-output/parser-ir.json" "fixtures/source-bundle/abc-source-bundle-v1-known-answer.json"
                    "schemas/parser-ir.schema.json" "schemas/source-bundle.schema.json"]]
        (files/read-text path))
      (is true))))

(defn- materialized-parser-manifest!
  [input output manifest-inputs parser-ir]
  (fs/copy-tree "examples/ab-validator-output" input)
  (manifest/write-json-file! (fs/file input "manifest-inputs.json") manifest-inputs)
  (manifest/write-json-file! (fs/file input "parser-ir.json") parser-ir)
  (files/read-json
   (:parser-ir (materialize-import/materialize-import!
                {:input-dir input :output-dir output}))))

(deftest parser-tuple-change-preserves-source-role-relations-test
  (fs/with-temp-dir [root {}]
    (let [work-root (fs/file root "materialized")
          workset-file (fs/file root "workset.edn")
          snapshot-file (fs/file root "snapshot.json")]
      (fixture/materialized-work! work-root {:slug "alpha"
                                             :title "一"
                                             :work-id "000001"
                                             :person-id "000101"
                                             :work-hash (files/example-hash "a1")})
      (workset/write-workset!
       {:input-root (str work-root)
        :output-path (str workset-file)
        :snapshot-scope "parser-identity-relations"
        :snapshot-date "2026-07-14"})
      (materialize-snapshot/materialize-source-snapshot!
       {:workset-path workset-file :output-path snapshot-file})
      (let [source (-> (files/read-json snapshot-file)
                       (get-in ["snapshot_identity_object" "snapshot_inputs" 0]))
            source-bundle (files/read-json
                           (fs/file root (get source "source_bundle_path")))
            bundle-hash (get source "work_content_hash")
            primary-text-hash (get source "primary_text_hash")
            base-inputs (assoc (files/read-json "examples/ab-validator-output/manifest-inputs.json")
                               "work_content_hash" bundle-hash)
            base-ir (assoc (files/read-json "examples/ab-validator-output/parser-ir.json")
                           "source" {"work_content_hash" bundle-hash
                                     "primary_text_hash" primary-text-hash})
            changed-inputs (assoc base-inputs
                                  "parser_build_hash" (files/example-hash "91")
                                  "mapping_hash" (files/example-hash "92"))
            changed-ir (-> base-ir
                           (assoc-in ["derived_from" "aat_adapter"] "changed-parser-adapter")
                           (assoc-in ["derived_from" "aat_adapter_version"] "changed-parser-adapter 2.0")
                           (assoc-in ["derived_from" "mapping_id"] "https://w3id.org/abc/mappings/changed")
                           (assoc-in ["derived_from" "mapping_version"] "2.0"))
            base-manifest (materialized-parser-manifest!
                           (fs/file root "base-input") (fs/file root "base-output")
                           base-inputs base-ir)
            changed-manifest (materialized-parser-manifest!
                              (fs/file root "changed-input") (fs/file root "changed-output")
                              changed-inputs changed-ir)
            tuple (fn [inputs parser-ir]
                    [(get inputs "parser_build_hash")
                     (get-in parser-ir ["derived_from" "aat_adapter"])
                     (get inputs "mapping_hash")])]
        (is (= bundle-hash (get source-bundle "bundle_hash")))
        (is (= primary-text-hash
               (->> (get-in source-bundle ["identity_object" "members"])
                    (some #(when (= (get source "primary_text_member") (get % "path"))
                             (get % "member_hash"))))))
        (is (not= bundle-hash primary-text-hash))
        (is (not= (tuple base-inputs base-ir) (tuple changed-inputs changed-ir)))
        (is (not= (get base-manifest "manifest_identity_object")
                  (get changed-manifest "manifest_identity_object")))
        (is (not= (get base-manifest "artifact_id")
                  (get changed-manifest "artifact_id")))))))
