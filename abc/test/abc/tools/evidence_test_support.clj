(ns abc.tools.evidence-test-support
  (:require [abc.tools.files :as files]
            [clojure.string :as str]))

(defn- component-relative-path [path]
  (str/replace path #"^abc/" ""))

(defn focused-trace-options [descriptor-stem]
  (let [local-path (str "docs/evidence/adr-capture/" descriptor-stem ".edn")
        descriptor (-> (files/read-edn local-path)
                       (update-in [:input-profile :explicit]
                                  #(mapv component-relative-path %)))]
    {:repo-root "."
     :identity-root "."
     :cwd-root "."
     :workspace-root "."
     :component-root "abc"
     :descriptor
     {:path local-path
      :value descriptor}}))
