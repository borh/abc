(ns soranoha.ori.accountability-test
  (:require [charred.api :as json]
            [clojure.test :refer [deftest is]]
            [soranoha.core.hash :as hash]
            [soranoha.ori.accountability :as accountability]))

(deftest native-source-stage-keeps-lexical-evidence-independent
  (let [tool (accountability/resolve-tool)
        stage (accountability/source-stage tool)
        source (.getBytes "漢字《かんじ》［＃］" "UTF-8")
        result ((:f stage) {:blob {"source-id" source}} {"source" "source-id"})
        report (json/read-json (String. ^bytes (get result "source-accountability") "UTF-8"))]
    (is (= "source-accountability" (:stage-id stage)))
    (is (= (hash/sha256-canonical-json
            {"binary" (hash/sha256-file (:bin tool))
             "matrix" (hash/sha256-file (:matrix tool))})
           (:toolchain-id stage)))
    (is (= "aozora-source-accountability/1" (get report "schema")))
    (is (= (str "sha256:" (hash/sha256-bytes source)) (get report "source_sha256")))
    (is (= "not-assessed" (get report "semantic_coverage")))
    (is (= [["ruby.basic"] []] (mapv #(get % "families") (get report "occurrences"))))
    (is (= ["《かんじ》" "［＃］"] (mapv #(get % "raw") (get report "occurrences"))))))
