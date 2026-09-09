(ns ab-research.parser-relations-provenance-evidence-test
  (:require [ab-research.files :as files]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private revision "1a4f864603970983719655aa4af4525958ac2d38")
(def ^:private repository "https://github.com/P4suta/aozora")
(def ^:private licence "MIT OR Apache-2.0")

(def ^:private forked-crates
  "Every crate carrying code lifted from upstream.

  Seven keep the `ab-aozora-` prefix. `ab-notation-strategies` does not: it
  descends from upstream's `aozora-proptest` and was renamed when the fork was
  reduced, so the name no longer says where the code came from. Its licence
  obligation is unchanged by the rename."
  ["ab-aozora-corpus" "ab-aozora-encoding" "ab-aozora-facade" "ab-aozora-pipeline"
   "ab-aozora-render" "ab-aozora-spec" "ab-aozora-syntax" "ab-notation-strategies"])

(defn- crates-declaring-fork-provenance
  "Every crate whose sources carry the upstream provenance header.

  Read back from the tree rather than from a list, so the evidence has to
  cover what is actually there. Matching on the header rather than on the
  crate name is what makes this hold after a rename: a lifted crate that keeps
  its header but loses the `ab-aozora-` prefix stays visible here."
  []
  (->> (fs/list-dir (fs/file "../crates"))
       (filter fs/directory?)
       (filter (fn [dir]
                 (some (fn [source]
                         (str/includes? (slurp (fs/file source)) "Forked from"))
                       (filter #(str/ends-with? (fs/file-name %) ".rs")
                               (file-seq (fs/file (fs/path dir "src")))))))
       (map fs/file-name)
       sort
       vec))

(defn parser-relations-provenance-operation []
  (let [source-paths (conj (mapv #(str "../crates/" % "/src/lib.rs") forked-crates)
                           "../crates/ab-aozora-spec/src/diagnostic.rs")
        notice-paths (mapv #(str "../crates/" % "/NOTICE") forked-crates)
        cargo-paths (mapv #(str "../crates/" % "/Cargo.toml") forked-crates)
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
    ;; A forked crate keeps upstream's dual licence whatever the workspace
    ;; chooses for locally authored code, so these manifests must state it
    ;; rather than inherit it.
    (doseq [[path text] (map vector cargo-paths cargos)]
      (is (str/includes? text (str "license = \"" licence "\"")) path))
    ;; The checks above prove that every crate this test names carries its
    ;; provenance. This one proves the test names every forked crate: without
    ;; it, adding a lifted crate and forgetting to list it here would leave
    ;; the omission invisible rather than failing.
    (is (= (vec (sort forked-crates)) (crates-declaring-fork-provenance))
        "every crate carrying an upstream provenance header must be covered by this evidence")
    true))

(deftest fork-provenance-records-detach-test
  (parser-relations-provenance-operation))
