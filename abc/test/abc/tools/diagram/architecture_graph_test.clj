(ns abc.tools.diagram.architecture-graph-test
  (:require [clojure.test :refer [deftest is]]
            [abc.tools.diagram.architecture-graph :as arch]
            [abc.tools.json :as json]
            [clojure.string :as str]))

(defn- validate-doc [doc]
  (arch/validate doc
                 (arch/schema-paths)
                 (arch/adr-slugs)
                 (json/read-json-file "schemas/manifest.schema.json")
                 (slurp "docs/architecture.md")))

(deftest schemas-and-adrs-and-inputs-resolve
  (is (= [] (arch/lint*))))

(deftest build-is-deterministic-flowchart-input
  (let [g (arch/build)]
    (is (= "TD" (:direction g)))
    (is (= (arch/build) g))
    (is (seq (:nodes g)))))

(deftest manifest-identity-coordinate-ownership-is-total
  (let [doc (arch/load-stages)
        required (arch/manifest-identity-required
                  (json/read-json-file "schemas/manifest.schema.json"))]
    (is (= required
           (set (keys (get-in doc [:manifest-identity-contract :coordinates])))))
    (is (= #{"manifest-identity" "manifest-identity-hardening"
             "owned-aat-parser-ir-mapping" "analysis-packs-and-tokenizer-profiles"
             "ruby-annotation-view"}
           (set (mapcat val
                        (get-in doc [:manifest-identity-contract :coordinates])))))))

(deftest architecture-prose-lists-the-live-identity-contract
  (is (= (arch/manifest-identity-required
          (json/read-json-file "schemas/manifest.schema.json"))
         (arch/documented-identity-coordinates
          (slurp "docs/architecture.md")))))

(deftest identity-coordinate-owners-must-not-be-empty
  (let [doc (assoc-in (arch/load-stages)
                      [:manifest-identity-contract :coordinates
                       "manifest_schema_hash"] [])]
    (is (some #(str/includes? % "manifest_schema_hash owners must be a non-empty sequential collection of decision slugs")
              (validate-doc doc)))))

(deftest identity-coordinate-owners-must-be-sequential-slugs
  (let [doc (assoc-in (arch/load-stages)
                      [:manifest-identity-contract :coordinates
                       "manifest_schema_hash"]
                      #{"manifest-identity" "manifest-identity-hardening"})]
    (is (some #(str/includes? % "manifest_schema_hash owners must be a non-empty sequential collection of decision slugs")
              (validate-doc doc)))))

(deftest identity-coordinate-owners-must-reference-existing-decisions
  (let [doc (assoc-in (arch/load-stages)
                      [:manifest-identity-contract :coordinates
                       "manifest_schema_hash"]
                      ["manifest-identity" "no-such-decision"])]
    (is (some #(str/includes? % "manifest_schema_hash references non-existent decision no-such-decision")
              (validate-doc doc)))))

(deftest identity-contract-must-name-the-live-manifest-schema
  (let [doc (assoc-in (arch/load-stages)
                      [:manifest-identity-contract :schema]
                      "schemas/parser-ir.schema.json")]
    (is (some #(str/includes? % "identity contract schema must be schemas/manifest.schema.json")
              (validate-doc doc)))))
