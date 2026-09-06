;; The kernel reads exactly one root; a second environment variable is a
;; design smell. Every other path is derived here and nowhere else.
(ns soranoha.core.config
  (:require [babashka.fs :as fs]))

(defn root
  "Resolve SORANOHA_ROOT. `explicit` (a CLI --root value) wins over the
  environment; there is no default — an unconfigured root is an error, never a
  guessed location."
  ([] (root nil))
  ([explicit]
   (let [value (or explicit (System/getenv "SORANOHA_ROOT"))]
     (when (or (nil? value) (empty? value))
       (throw (ex-info "SORANOHA_ROOT is not set (or pass --root)"
                       {:reason :missing-root})))
     (str (fs/absolutize value)))))

(defn cas-dir [root] (str (fs/path root "kura" "objects")))
(defn trace-db-path [root] (str (fs/path root "kura" "trace.sqlite")))

(defn ensure-layout!
  "Create the derived directory layout under the root; returns the root."
  [root]
  (fs/create-dirs (cas-dir root))
  root)
