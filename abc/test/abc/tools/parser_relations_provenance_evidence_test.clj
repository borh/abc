(ns abc.tools.parser-relations-provenance-evidence-test
  (:require [abc.tools.decisions :as decisions]
            [abc.tools.files :as files]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private revision "1a4f864603970983719655aa4af4525958ac2d38")
(def ^:private repository "https://github.com/P4suta/aozora")
(def ^:private licence "MIT OR Apache-2.0")
(def ^:private crate-stems
  ["corpus" "encoding" "facade" "pipeline" "proptest" "render" "scan"
   "spec" "syntax" "veb"])

(def ^:private parser-evaluation "parser-evaluation")
(def ^:private parser-selection "aozora-parser-selection")
(def ^:private fork-detach "parser-fork-hard-detach")
(def ^:private ownership "custom-parser-ownership-and-neutral-comparison")
(def ^:private evidence-simplification "subtractive-evidence-simplification")

(defn- amends-graph []
  (let [{:keys [corpus problems]} (decisions/load-corpus decisions/corpus-file)]
    (is (nil? problems))
    (into {}
          (for [{:keys [slug relations]} (:decisions corpus)
                :let [targets (set (for [r relations
                                         :when (and (= :lifecycle (:class r))
                                                    (= :amends (:type r)))]
                                     (:to r)))]
                :when (seq targets)]
            [slug targets]))))

(defn- amended-by [graph slug]
  (set (for [[from targets] graph :when (contains? targets slug)] from)))

(defn parser-relations-provenance-operation []
  (let [graph (amends-graph)
        handoff (files/read-text "../ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md")
        adr32 (files/read-text "docs/adr/parser-fork-hard-detach.md")
        source-paths (concat
                      (for [stem crate-stems]
                        (str "../ab-validator/crates/ab-aozora-" stem "/src/lib.rs"))
                      ["../ab-validator/crates/ab-aozora-spec/src/diagnostic.rs"
                       "../ab-validator/crates/ab-aozora-facade/README.md"])
        notice-paths (for [stem crate-stems]
                       (str "../ab-validator/crates/ab-aozora-" stem "/NOTICE"))
        cargo-paths (conj (mapv #(str "../ab-validator/crates/ab-aozora-" % "/Cargo.toml") crate-stems)
                          "../ab-validator/crates/ab-aozora/Cargo.toml")
        sources (mapv files/read-text source-paths)
        notices (mapv files/read-text notice-paths)
        cargos (mapv files/read-text cargo-paths)]
    ;; acting :amends edges, with derived amended-by inverses
    (is (= #{parser-evaluation} (get graph parser-selection)))
    (is (= #{parser-selection} (get graph fork-detach)))
    (is (= #{parser-evaluation parser-selection fork-detach}
           (get graph ownership)))
    (is (= #{parser-selection ownership} (amended-by graph parser-evaluation)))
    (is (= #{fork-detach ownership} (amended-by graph parser-selection)))
    (is (= #{ownership} (amended-by graph fork-detach)))
    (is (contains? (amended-by graph ownership) evidence-simplification))
    (doseq [text [handoff adr32]]
      (is (str/includes? text revision))
      (is (str/includes? text "P4suta/aozora")))
    (is (str/includes? handoff "github.com/P4suta/aozora"))
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
