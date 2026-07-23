(ns abc.tools.parser-relations-provenance-evidence-test
  (:require [abc.tools.adr :as adr]
            [abc.tools.files :as files]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private revision "1a4f864603970983719655aa4af4525958ac2d38")
(def ^:private repository "https://github.com/P4suta/aozora")
(def ^:private licence "MIT OR Apache-2.0")
(def ^:private crate-stems
  ["corpus" "encoding" "facade" "pipeline" "proptest" "render" "scan"
   "spec" "syntax" "veb"])

(defn- adr-fields [number]
  (:fields (adr/parse-adr "docs/adr" (format "%04d-%s.md" number
                                             ({2 "parser-evaluation"
                                               30 "aozora-parser-selection"
                                               32 "parser-fork-hard-detach"
                                               38 "custom-parser-ownership-and-neutral-comparison"}
                                              number)))))

(defn- relation-set [value]
  (if (or (nil? value) (= "none" value)) #{}
      (set (map #(Integer/parseInt (second (re-find #"ADR (\d+)" %)))
                (str/split value #", ")))))

(defn parser-relations-provenance-operation []
  (let [relations (into {} (for [number [2 30 32 38]
                                 :let [fields (adr-fields number)]]
                             [number {:amends (relation-set (get fields "Amends"))
                                      :amended-by (relation-set (get fields "Amended by"))}]))
        handoff (files/read-text "../ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md")
        adr32 (files/read-text "docs/adr/0032-parser-fork-hard-detach.md")
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
    (is (= {2 {:amends #{} :amended-by #{30 38}}
            30 {:amends #{2} :amended-by #{32 38}}
            32 {:amends #{30} :amended-by #{38}}
            38 {:amends #{2 30 32} :amended-by #{43}}}
           relations))
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

(deftest parser-family-reciprocal-amendments-test
  (parser-relations-provenance-operation))

(deftest fork-provenance-records-detach-test
  (parser-relations-provenance-operation))
