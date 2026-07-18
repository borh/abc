(ns abc.tools.parser-rq-publication-materialize
  "Materialize the closed non-bibliographic parser-RQ publication fixtures."
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-publication :as materialize]
            [babashka.fs :as fs]
            [clojure.string :as string]))

(def generated-at "2026-07-17T00:00:00Z")

(defn materialize-qualification!
  [{:keys [parser-ir-root fixture-root output-root works]}]
  (let [work-ids (sort (keys works))
        persons (fs/file fixture-root "persons")]
    (when-not (and (seq work-ids) (fs/directory? persons))
      (throw (ex-info "publication qualification fixture membership is unavailable" {})))
    (doseq [work-id work-ids]
      (let [parser-ir (fs/file parser-ir-root (str work-id ".json"))
            metadata (fs/file fixture-root work-id "metadata-record.json")]
        (when-not (and (fs/regular-file? parser-ir) (fs/regular-file? metadata))
          (throw (ex-info "publication qualification input is absent"
                          {:work-id work-id})))
        (materialize/materialize-publication!
         {:parser-ir-path parser-ir
          :metadata-record-path metadata
          :persons-dir persons
          :output-dir (fs/file output-root work-id)
          :generated-at generated-at})))
    (vec work-ids)))

(defn- parse-options [args]
  (loop [remaining args options {}]
    (if (empty? remaining)
      options
      (let [[option value & tail] remaining]
        (when-not (and value (string/starts-with? option "--"))
          (throw (ex-info "publication materializer options must be pairs" {})))
        (recur tail (assoc options (keyword (subs option 2)) value))))))

(defn- required [options key]
  (or (get options key)
      (throw (ex-info (str "missing --" (name key)) {}))))

(defn -main [& args]
  (let [options (parse-options args)
        fixture-root (required options :fixture-root)
        fixtures (files/read-json (required options :fixtures))]
    (materialize-qualification!
     {:parser-ir-root (required options :parser-ir-root)
      :fixture-root fixture-root
      :output-root (required options :output-root)
      :works (get fixtures "works")})
    (println "ok")))
