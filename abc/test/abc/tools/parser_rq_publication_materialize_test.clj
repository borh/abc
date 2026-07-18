(ns abc.tools.parser-rq-publication-materialize-test
  (:require [abc.tools.json :as json]
            [abc.tools.materialize-publication :as materialize]
            [abc.tools.parser-rq-publication-materialize :as subject]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(deftest materializes-only-the-closed-qualification-workset
  (let [root (fs/create-temp-dir {:prefix "parser-rq-publication-materialize"})
        parser-root (fs/file root "parser-ir")
        fixture-root (fs/file root "fixtures")
        output-root (fs/file root "output")
        works {"w1" {"work_id" "w1"} "w2" {"work_id" "w2"}}
        calls (atom [])]
    (doseq [work-id (keys works)]
      (json/write-deterministic-json-file! (fs/file parser-root (str work-id ".json")) {})
      (json/write-deterministic-json-file!
       (fs/file fixture-root work-id "metadata-record.json") {})
      (fs/create-dirs (fs/file fixture-root "persons")))
    (with-redefs [materialize/materialize-publication!
                  (fn [options]
                    (swap! calls conj options)
                    {:tei-validation-result :passed})]
      (is (= ["w1" "w2"]
             (subject/materialize-qualification!
              {:parser-ir-root parser-root
               :fixture-root fixture-root
               :output-root output-root
               :works works}))))
    (is (= ["w1" "w2"]
           (mapv #(fs/file-name (:output-dir %)) @calls)))
    (is (every? #(= "2026-07-17T00:00:00Z" (:generated-at %)) @calls))))
