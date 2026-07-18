(ns abc.tools.parser-rq-portability-test
  (:require [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(def active-roots
  ["abc/src" "abc/tools" "abc/bin" "abc/schemas" "abc/data"
   "abc/config" "abc/test" "ab-validator/reports/parser-ir"])

(def active-files
  ["flake.nix" "abc/flake.nix" "ab-validator/flake.nix"
   "abc/docs/superpowers/plans/2026-07-17-parser-rq-admission-promotion.md"])

(def forbidden
  [(str "host" "_policy_ref")
   (str "site" "-policy")
   (str "site" "_policy")
   (str "rep" "lica")
   (str "verify" "-replicas")
   (str "replication" "-receipt")
   (str "replica" "_failure_domain")
   (str "stable" "_host_label")
   (str "kernel" "_hostname")
   (str "remote" "_authority")
   (str "mount" "_class")])

(def text-extensions
  #{"clj" "cljc" "edn" "json" "md" "nix" "py" "sh" "toml" "yaml" "yml"})

(defn- repo-root []
  (loop [path (fs/absolutize ".")]
    (if (and (fs/regular-file? (fs/file path "justfile"))
             (fs/directory? (fs/file path "abc"))
             (fs/directory? (fs/file path "ab-validator")))
      path
      (if-let [parent (fs/parent path)]
        (recur parent)
        (throw (ex-info "repository root is not reachable"
                        {:start (str (fs/absolutize "."))}))))))

(defn- selected-text-file? [path]
  (let [name (str (fs/file-name path))
        extension (fs/extension path)]
    (or (contains? text-extensions extension)
        (not (string/includes? name ".")))))

(defn- scanned-active-files [root]
  (let [guard (fs/canonicalize
               (fs/file root "abc/test/abc/tools/parser_rq_portability_test.clj"))
        nested (for [relative active-roots
                     path (fs/glob (fs/file root relative) "**")
                     :when (and (fs/regular-file? path) (selected-text-file? path))]
                 (fs/canonicalize path))
        individual (map #(fs/canonicalize (fs/file root %)) active-files)]
    (->> (concat nested individual)
         (remove #(= guard %))
         distinct
         (sort-by str)
         vec)))

(deftest active-parser-rq-surface-has-no-site-or-backup-policy
  (let [root (repo-root)
        paths (scanned-active-files root)
        relative (set (map #(str (fs/relativize root %)) paths))
        formerly-dense
        #{"ab-validator/reports/parser-ir/parser-rq-campaign-provenance.py"
          "ab-validator/reports/parser-ir/test_parser_rq_campaign_provenance.py"
          "abc/tools/test_parser_rq_campaign_site.py"
          "abc/tools/test_parser_rq_campaign_orchestrator.py"
          "abc/config/parser-rq-site.example.json"
          "abc/flake.nix"
          "ab-validator/flake.nix"}]
    (is (every? relative formerly-dense))
    (doseq [path paths
            token forbidden]
      (is (not (string/includes? (slurp (str path)) token))
          (str path " contains removed token " token)))))
