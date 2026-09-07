(ns soranoha.ori.accountability
  "Independent source-marker evidence. Recognition does not certify interpretation."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [soranoha.core.hash :as hash]))

(defn resolve-tool
  "Resolve the independent source scanner and its source-authority matrix."
  []
  (into {} (map (fn [[key variable]]
                  (let [value (System/getenv variable)]
                    (when (str/blank? value)
                      (throw (ex-info (str "Source accountability requires " variable)
                                      {:env_var variable})))
                    [key value])))
        [[:bin "AB_SOURCE_INVENTORY_BIN"] [:matrix "AB_AOZORA_SYNTAX_MATRIX"]]))

(defn source-stage
  "Source bytes -> lexical accountability, independently cached from interpretation."
  [{:keys [bin matrix]}]
  {:stage-id "source-accountability"
   :stage-version "1"
   :toolchain-id (hash/sha256-canonical-json
                  {"binary" (hash/sha256-file bin)
                   "matrix" (hash/sha256-file matrix)})
   :f (fn [{:keys [blob]} inputs]
        (let [dir (fs/create-temp-dir {:prefix "soranoha-source-accountability"})
              source (str (fs/path dir "source.txt"))
              output (str (fs/path dir "accountability.json"))]
          (try
            (io/copy ^bytes (blob (get inputs "source")) (io/file source))
            (let [{:keys [exit err]}
                  @(process/process [bin "--source" source "--matrix" matrix
                                     "--output-json" output]
                                    {:out :string :err :string})]
              (when-not (zero? exit)
                (throw (ex-info "Source accountability scanner failed"
                                {:exit exit :stderr err})))
              {"source-accountability" (java.nio.file.Files/readAllBytes (fs/path output))})
            (finally (fs/delete-tree dir)))))})
