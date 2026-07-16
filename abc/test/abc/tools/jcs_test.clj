(ns abc.tools.jcs-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.evidence-test-support :as evidence-support]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [clojure.test :refer [deftest is testing]]))

(defn- utf16-string [& code-units]
  (String. (char-array (map char code-units))))

(defn- exception-data [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(defn- canonical-json-assertions []
  (do
    (is (= "{\"a\":{\"b\":2,\"c\":3},\"z\":1}"
           (jcs/canonical-json-string {"z" 1
                                       "a" {"c" 3
                                            "b" 2}}))))
  (do
    (is (= (jcs/canonical-json-string {"b" "2" "a" "1"})
           (jcs/canonical-json-string {"a" "1" "b" "2"}))))
  (do
    (is (= "{\"a\":null,\"b\":\"quote\\\"slash\\\\\"}"
           (jcs/canonical-json-string {"b" "quote\"slash\\"
                                       "a" nil}))))
  (do
    (is (= "[\"a\",\"b\"]"
           (jcs/canonical-json-string ["a" "b"])))
    (is (not= (jcs/canonical-json-string ["a" "b"])
              (jcs/canonical-json-string ["b" "a"])))))

(deftest canonical-json-test
  (runtime/with-validated-read-trace!
    (evidence-support/focused-trace-options "adr-0001-c2-canonical-null-array-order")
    (fn [] (canonical-json-assertions))))

(deftest rfc8785-string-domain-canonical-json-test
  (testing "optional escapes stay literal; required JSON escapes remain"
    (is (= "\"a/作品  😀\""
           (jcs/rfc8785-string-domain-json-string "a/作品  😀")))
    (is (= "\"quote\\\"backslash\\\\control\\n\""
           (jcs/rfc8785-string-domain-json-string
            "quote\"backslash\\control\n"))))
  (testing "supplementary pairs are accepted in nested keys and values"
    (is (= "{\"outer\":[{\"補助😀\":\"値😀\"}]}"
           (jcs/rfc8785-string-domain-json-string
            {"outer" [{"補助😀" "値😀"}]}))))
  (testing "malformed UTF-16 values fail with their nested identity path"
    (doseq [invalid [(utf16-string 0xd800)
                     (utf16-string 0xdc00)
                     (utf16-string 0xd800 0x41)
                     (utf16-string 0xdc00 0xd800)]]
      (let [data (exception-data
                  #(jcs/rfc8785-string-domain-json-string
                    {"outer" [{"value" invalid}]}))]
        (is (= :invalid-utf16 (:reason data)))
        (is (= ["outer" 0 "value"] (:path data)))
        (is (= :value (:position data)))
        (is (integer? (:string-index data)))
        (is (re-matches #"0x[0-9a-f]{4}" (:code-unit data))))))
  (testing "malformed UTF-16 object keys fail at the containing object path"
    (let [invalid-key (utf16-string 0xd800)
          data (exception-data
                #(jcs/rfc8785-string-domain-json-string
                  {"outer" {invalid-key "value"}}))]
      (is (= :invalid-utf16 (:reason data)))
      (is (= ["outer"] (:path data)))
      (is (= :object-key (:position data)))
      (is (= 0 (:string-index data)))
      (is (= "0xd800" (:code-unit data)))))
  (testing "unsupported scalars fail with their nested identity path"
    (let [data (exception-data
                #(jcs/rfc8785-string-domain-json-string
                  {"outer" ["ok" 1]}))]
      (is (= :unsupported-scalar (:reason data)))
      (is (= ["outer" 1] (:path data)))
      (is (= 1 (:value data)))))
  (testing "the historical serializer retains its frozen escaping behavior"
    (is (= "\"fig\\/\\u4e00.png\""
           (jcs/canonical-json-string "fig/一.png")))))

(deftest rfc8785-full-domain-canonical-json-v1-test
  (testing "RFC 8785 section 3.2.2 canonicalization example"
    (is (= (str "{\"literals\":[null,true,false],"
                "\"numbers\":[333333333.3333333,1e+30,4.5,0.002,1e-27],"
                "\"string\":\"€$\\u000f\\nA'B\\\"\\\\\\\\\\\"/\"}")
           (jcs/rfc8785-json-string-v1
            {"numbers" [333333333.33333329 1.0e30 4.50 2e-3 1.0e-27]
             "string" "€$\u000f\nA'B\"\\\\\"/"
             "literals" [nil true false]}))))
  (testing "Unicode, slash, controls, nested values, and safe integers compose"
    (let [value {"作品/😀" [{"control" "\u0000\n"
                           "count" 9007199254740991}]
                 "negative" -42}]
      (is (= (str "{\"negative\":-42,\"作品/😀\":[{"
                  "\"control\":\"\\u0000\\n\","
                  "\"count\":9007199254740991}]}")
             (jcs/rfc8785-json-string-v1 value)))
      (is (= (hash/sha256-bytes (jcs/rfc8785-json-bytes-v1 value))
             (hash/sha256-json-rfc8785-v1 value)))))
  (testing "non-finite and unsafe integral values fail closed"
    (doseq [value [Double/NaN Double/POSITIVE_INFINITY
                   Double/NEGATIVE_INFINITY
                   9007199254740992 -9007199254740992]]
      (is (= :unsupported-rfc8785-number
             (:reason (exception-data
                       #(jcs/rfc8785-json-string-v1 {"number" value}))))))))

(deftest rfc8785-cross-language-vectors-test
  (let [fixture (files/read-json
                 "test/fixtures/canonicalization/rfc8785-jcs-abc-v1-vectors.json")]
    (is (= "sha256-rfc8785-jcs-abc-v1" (get fixture "algorithm_id")))
    (doseq [{:strs [input canonical_json sha256]} (get fixture "vectors")]
      (is (= canonical_json (jcs/rfc8785-json-string-v1 input)))
      (is (= sha256 (hash/sha256-json-rfc8785-v1 input))))))
