(ns ab-research.parser-relations-provenance-evidence-test
  (:require [ab-research.files :as files]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private revision "1a4f864603970983719655aa4af4525958ac2d38")
(def ^:private repository "https://github.com/P4suta/aozora")
(def ^:private licence "MIT OR Apache-2.0")
(def ^:private crate-stems
  ["corpus" "encoding" "facade" "pipeline" "proptest" "render" "scan"
   "spec" "syntax" "veb"])

(defn parser-relations-provenance-operation []
  (let [source-paths (concat
                      (for [stem crate-stems]
                        (str "../crates/ab-aozora-" stem "/src/lib.rs"))
                      ["../crates/ab-aozora-spec/src/diagnostic.rs"])
        notice-paths (for [stem crate-stems]
                       (str "../crates/ab-aozora-" stem "/NOTICE"))
        cargo-paths (conj (mapv #(str "../crates/ab-aozora-" % "/Cargo.toml") crate-stems)
                          "../crates/ab-aozora/Cargo.toml")
        sources (mapv files/read-text source-paths)
        notices (mapv files/read-text notice-paths)
        cargos (mapv files/read-text cargo-paths)]
    (doseq [[path text] (map vector source-paths sources)]
      (is (str/includes? text "Forked from") path)
      (is (str/includes? text repository) path)
      (is (str/includes? text revision) path)
      (is (str/includes? text licence) path))
    (doseq [text notices]
      (is (or (str/includes? text licence)
              (str/includes? text "Apache-2.0 OR MIT"))))
    (doseq [text cargos]
      (is (or (str/includes? text "license.workspace = true")
              (str/includes? text "license = \"MIT OR Apache-2.0\""))))
    true))

(deftest fork-provenance-records-detach-test
  (parser-relations-provenance-operation))
