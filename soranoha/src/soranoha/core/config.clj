(ns soranoha.core.config
  (:require [babashka.fs :as fs]
            [clojure.string :as string])
  (:import [java.nio.file Files OpenOption StandardOpenOption]))

(defn root
  "Resolve SORANOHA_ROOT. `explicit` (a CLI --root value) wins over the
  environment; there is no default: an unconfigured root is an error, never a
  guessed location."
  [explicit]
  (let [value (or explicit (System/getenv "SORANOHA_ROOT"))]
    (when (or (nil? value) (empty? value))
      (throw (ex-info "SORANOHA_ROOT is not set (or pass --root)"
                      {:reason :missing-root})))
    (str (fs/absolutize value))))

(defn require-env
  "The value of environment variable `k`, which the build wrapper supplies.
  Blank counts as unset, and unset is an error naming `what` the variable was
  for: there is no default location to guess."
  [k what]
  (let [value (System/getenv k)]
    (if (string/blank? value)
      (throw (ex-info (str what " unavailable; set " k) {:env_var k}))
      value)))

(defn with-temp-dir
  "Call `f` with a fresh temporary directory named after `prefix`, deleting
  the directory and everything in it afterwards."
  [prefix f]
  (let [dir (fs/create-temp-dir {:prefix prefix})]
    (try (f dir)
         (finally (fs/delete-tree dir)))))

(defn write-new!
  "Write `text` as UTF-8 to a file that does not exist yet. An existing file
  is an error, so an exporter never overwrites earlier output."
  [path ^String text]
  (Files/write (fs/path path)
               (.getBytes text "UTF-8")
               ^"[Ljava.nio.file.OpenOption;"
               (into-array OpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE])))

(defn cas-dir [root] (str (fs/path root "kura" "objects")))
(defn trace-db-path [root] (str (fs/path root "kura" "trace.sqlite")))

(defn ensure-layout!
  "Create the derived directory layout under the root; returns the root."
  [root]
  (fs/create-dirs (cas-dir root))
  root)
