(ns abc.tools.foundation-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.schema :as schema]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- artifact-id-paths [value]
  (letfn [(walk [path item]
            (cond
              (map? item) (mapcat (fn [[key child]]
                                    (let [child-path (conj path key)]
                                      (concat (when (= "artifact_id" key) [child-path])
                                              (walk child-path child))))
                                  item)
              (sequential? item) (mapcat (fn [[index child]]
                                           (walk (conj path index) child))
                                         (map-indexed vector item))
              :else []))]
    (vec (walk [] value))))

(defn- workflow-run-blocks [text]
  (let [lines (str/split-lines text)]
    (loop [remaining lines blocks []]
      (if-let [line (first remaining)]
        (if-let [[_ indent] (re-matches #"^(\s*)run:\s*\|\s*$" line)]
          (let [body (->> (rest remaining)
                          (take-while #(or (str/blank? %)
                                           (< (count indent)
                                              (count (re-find #"^\s*" %)))))
                          (map str/trim)
                          (remove str/blank?)
                          (str/join " "))]
            (recur (rest remaining) (conj blocks body)))
          (recur (rest remaining) blocks))
        blocks))))

(deftest committed-manifest-examples-conform-and-failure-coordinates-are-explicit
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        success (files/read-json "examples/v0/example-work/manifest.json")
        failure (files/read-json "examples/v0/example-work/failure-manifest.example.json")]
    (is (nil? (schema/validation-errors manifest-schema success)))
    (is (nil? (schema/validation-errors manifest-schema failure)))
    (is (= "failure" (get failure "artifact_kind")))
    (is (= "failed" (get failure "validation_status")))
    (is (nil? (get failure "content")))
    (is (= [{"role" "errors"
             "hash" "sha256:9999999999999999999999999999999999999999999999999999999999999999"
             "media_type" "application/jsonl"
             "path_hint" "errors.jsonl"}]
           (get failure "sidecars")))
    (is (= {"corpus_snapshot_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
            "work_content_hash" "sha256:7777777777777777777777777777777777777777777777777777777777777777"
            "parser_build_hash" "sha256:4444444444444444444444444444444444444444444444444444444444444444"
            "parser_config_hash" "sha256:5555555555555555555555555555555555555555555555555555555555555555"
            "parser_ir_schema_hash" "sha256:6666666666666666666666666666666666666666666666666666666666666666"
            "output_format_spec_hash" "sha256:3333333333333333333333333333333333333333333333333333333333333333"}
           (select-keys (get failure "manifest_identity_object")
                        ["corpus_snapshot_hash" "work_content_hash"
                         "parser_build_hash" "parser_config_hash"
                         "parser_ir_schema_hash" "output_format_spec_hash"])))
    (is (= [["artifact_id"]] (artifact-id-paths failure)))))

(deftest generated-import-manifests-conform
  (fs/with-temp-dir [output {:prefix "foundation-evidence-manifests-"}]
    (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
          generated (materialize/materialize-import!
                     {:input-dir "examples/ab-validator-output"
                      :output-dir output
                      :generated-at materialize/default-generated-at})]
      (doseq [[kind path] generated]
        (testing (name kind)
          (is (nil? (schema/validation-errors manifest-schema
                                              (files/read-json path)))))))))

(deftest wrapper-and-ci-delegate-to-the-nix-design-bundle-boundary
  (let [wrapper (files/read-text "bin/validate-design-bundle.sh")
        workflow (files/read-text ".github/workflows/validation.yml")
        run-blocks (workflow-run-blocks workflow)
        design-run (first (filter #(str/includes? % ".#validate-design-bundle")
                                  run-blocks))]
    (is (= ["#!/usr/bin/env bash"
            "set -euo pipefail"
            "cd \"$(dirname \"${BASH_SOURCE[0]}\")/..\""
            "exec clojure -M:abc/validate-design-bundle \"$@\""]
           (->> (str/split-lines wrapper) (remove str/blank?) vec)))
    (is (= 1 (count (re-seq #"(?m)^\s*- uses: actions/checkout@" workflow))))
    (is (some? design-run))
    (is (str/includes? design-run "nix run"))
    (is (str/includes? design-run
                       "--override-input local-pkgs \"path:${GITHUB_WORKSPACE}/nix/ci-empty-local-pkgs\""))
    (is (str/ends-with? design-run ".#validate-design-bundle"))))
