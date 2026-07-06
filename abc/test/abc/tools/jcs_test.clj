(ns abc.tools.jcs-test
  (:require [abc.tools.jcs :as jcs]
            [clojure.test :refer [deftest is testing]]))

(deftest canonical-json-test
  (testing "object keys are sorted recursively"
    (is (= "{\"a\":{\"b\":2,\"c\":3},\"z\":1}"
           (jcs/canonical-json-string {"z" 1
                                       "a" {"c" 3
                                            "b" 2}}))))
  (testing "map insertion order does not matter"
    (is (= (jcs/canonical-json-string {"b" "2" "a" "1"})
           (jcs/canonical-json-string {"a" "1" "b" "2"}))))
  (testing "null and string escaping are stable"
    (is (= "{\"a\":null,\"b\":\"quote\\\"slash\\\\\"}"
           (jcs/canonical-json-string {"b" "quote\"slash\\"
                                       "a" nil}))))
  (testing "arrays preserve order"
    (is (= "[\"a\",\"b\"]"
           (jcs/canonical-json-string ["a" "b"])))
    (is (not= (jcs/canonical-json-string ["a" "b"])
              (jcs/canonical-json-string ["b" "a"])))))
