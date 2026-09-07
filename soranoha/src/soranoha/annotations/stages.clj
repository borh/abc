(ns soranoha.annotations.stages
  (:require [soranoha.annotations.layer :as layer]
            [soranoha.annotations.tei :as tei]
            [soranoha.annotations.view :as view]))

(defn enrichment-stage
  "Consume a TEI blob and an ordered layers vector of JSON blob hashes."
  [clj-toolchain-id]
  {:stage-id "enrich-tei" :stage-version "2" :toolchain-id clj-toolchain-id
   :f (fn [{:keys [blob]} inputs]
        (let [base (String. ^bytes (blob (get inputs "tei")) "UTF-8")
              text-view (view/from-tei base)
              layers (mapv #(layer/read-layer text-view (String. ^bytes (blob %) "UTF-8"))
                           (get inputs "layers"))]
          {"enriched-tei" (.getBytes ^String (tei/enrich base layers) "UTF-8")}))})
