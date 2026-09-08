(ns ab-research.parser-relations-provenance-evidence-test
  (:require [ab-research.files :as files]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private revision "1a4f864603970983719655aa4af4525958ac2d38")
(def ^:private repository "https://github.com/P4suta/aozora")
(def ^:private licence "MIT OR Apache-2.0")
(def ^:private crate-stems
  ["corpus" "encoding" "facade" "pipeline" "render" "spec" "syntax"])

(defn- forked-crate-directories
  "Every `ab-aozora-<stem>` directory present in the workspace.

  The trailing hyphen is what marks a crate as lifted, so reading the
  directory names back is how the evidence stays honest about which crates it
  has to cover. `ab-aozora` without a suffix is the locally authored harness
  binary and is excluded by the pattern."
  []
  (->> (fs/list-dir (fs/file "../crates"))
       (filter fs/directory?)
       (map fs/file-name)
       (filter #(str/starts-with? % "ab-aozora-"))
       (map #(subs % (count "ab-aozora-")))
       sort
       vec))

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
    ;; The checks above prove that every crate this test names carries its
    ;; provenance. This one proves the test names every forked crate: without
    ;; it, adding a lifted crate and forgetting to list it here would leave
    ;; the omission invisible rather than failing.
    (is (= (vec (sort crate-stems)) (forked-crate-directories))
        "every ab-aozora-* crate directory must be covered by this evidence")
    true))

(deftest fork-provenance-records-detach-test
  (parser-relations-provenance-operation))
