(ns abc.tools.parser-identity-relations-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.source-bundle :as source-bundle]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.util.zip ZipEntry ZipOutputStream]))

(defn- source-bundle! [path]
  (with-open [out (ZipOutputStream. (io/output-stream path))]
    (doseq [[member content] [["fig/一.png" (byte-array [1 2 3])]
                              ["作品.txt" (.getBytes "本文" "UTF-8")]]]
      (.putNextEntry out (ZipEntry. member))
      (.write out content)
      (.closeEntry out)))
  (source-bundle/inspect-zip path))

(defn- materialized-parser-manifest! [input output manifest-inputs parser-ir]
  (fs/copy-tree "examples/ab-validator-output" input)
  (manifest/write-json-file! (fs/file input "manifest-inputs.json") manifest-inputs)
  (manifest/write-json-file! (fs/file input "parser-ir.json") parser-ir)
  (let [result (materialize/materialize-import!
                {:input-dir input :output-dir output})]
    {:manifest (files/read-json (:parser-ir result))
     :parser-ir (files/read-json (fs/file input "parser-ir.json"))}))

(deftest parser-tuple-change-preserves-source-role-relations-test
  (fs/with-temp-dir [root {}]
    (let [{:keys [bundle-hash primary-text-hash]}
          (source-bundle! (fs/file root "source.zip"))
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
          base (materialized-parser-manifest! (fs/file root "base-input")
                                              (fs/file root "base-output")
                                              base-inputs base-ir)
          changed (materialized-parser-manifest! (fs/file root "changed-input")
                                                 (fs/file root "changed-output")
                                                 changed-inputs changed-ir)
          tuple (fn [inputs parser-ir]
                  [(get inputs "parser_build_hash")
                   (get-in parser-ir ["derived_from" "aat_adapter"])
                   (get inputs "mapping_hash")])]
      (is (not= (tuple base-inputs (:parser-ir base))
                (tuple changed-inputs (:parser-ir changed))))
      (is (not= (get-in base [:manifest "manifest_identity_object"])
                (get-in changed [:manifest "manifest_identity_object"])))
      (is (not= (get-in base [:manifest "artifact_id"])
                (get-in changed [:manifest "artifact_id"])))
      (doseq [{:keys [parser-ir manifest]} [base changed]]
        (is (= bundle-hash (get-in parser-ir ["source" "work_content_hash"])))
        (is (= primary-text-hash (get-in parser-ir ["source" "primary_text_hash"])))
        (is (not= bundle-hash primary-text-hash))
        (is (some #{bundle-hash} (get-in manifest ["provenance" "was_derived_from"])))))))
