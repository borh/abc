(ns abc.html-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [babashka.fs :as fs]
            [babashka.process :refer [process check sh pipeline pb]]))

(deftest aozora2html-test
  (testing "serialization"))
