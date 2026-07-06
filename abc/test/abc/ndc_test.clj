(ns abc.ndc-test
  (:require [abc.ndc :as ndc]
            [clojure.test :refer [deftest is]]))

(deftest ndc-map-test
  (is (= ["日本文学" "小説" "物語"]
         (get ndc/ndc-map "913")))
  (is (= ["日本文学"]
         (get ndc/ndc-map "910")))
  (is (= ["総記"]
         (get ndc/ndc-map "000")))
  (is (= ["英米文学" "小説" "物語"]
         (get ndc/ndc-map "933"))))
