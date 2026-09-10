(ns soranoha.core.canonical-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]
            [soranoha.core.jcs :as jcs]))

(def shared-vectors-path
  "test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json")

(deftest shared-cross-language-vectors-test
  (let [fixture (json/read-json (io/file shared-vectors-path))]
    (testing "fixture identifies the algorithm this component implements"
      (is (= "sha256-rfc8785-safe-integer-domain-abc-v1"
             (get fixture "algorithm_id"))))
    (testing "every shared vector canonicalizes and hashes identically"
      (doseq [{:strs [name input canonical_json sha256]} (get fixture "vectors")]
        (is (= canonical_json
               (canonical/rfc8785-safe-integer-json-string-v1 input))
            name)
        (is (= sha256 (hash/sha256-canonical-json input))
            name)))))

(deftest safe-integer-domain-test
  (testing "exact boundaries accepted"
    (is (= "[-9007199254740991,9007199254740991]"
           (canonical/rfc8785-safe-integer-json-string-v1
            [-9007199254740991 9007199254740991]))))
  (testing "floats and unsafe integers fail closed"
    (doseq [value [1.0 1.5 Double/NaN Double/POSITIVE_INFINITY
                   9007199254740992 -9007199254740992]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"outside the supported JSON domain"
           (canonical/rfc8785-safe-integer-json-string-v1 {"n" value}))))))

(deftest unicode-literals-match-the-pinned-escaping-engine
  (let [strings (concat ["" "\"\\/\n\r\t" "日本語\u2028\u2029" "犍𠮷"]
                        (map (comp str char) (range 0xD800))
                        (map (comp str char) (range 0xE000 0x10000)))]
    (is (every? (fn [s]
                  (= (json/write-json-str s :escape-slash false
                                          :escape-unicode false
                                          :escape-js-separators false)
                     (canonical/rfc8785-safe-integer-json-string-v1 s)))
                strings))))

(deftest escaping-agrees-on-multi-character-strings
  ;; the single-character sweep above cannot reach the fast path, which
  ;; copies the run before the first character needing an escape and then
  ;; escapes the rest. Escapes at the front, in the middle, at the end and
  ;; back to back are where a boundary would be off by one.
  (let [alphabet (concat [\" \\ \newline \return \tab \formfeed \backspace
                          (char 0x00) (char 0x0b) (char 0x1f) \/ (char 0x7f)
                          \a \z (char 0x2028)]
                         [\日 \本 \語])
        random (java.util.Random. 20260910)
        sample (fn [n] (apply str (repeatedly n #(nth alphabet (.nextInt random (count alphabet))))))
        strings (concat ["" "\"" "\"a" "a\"" "a\"b" "\"\"" "a\\\nb" "\u0000\u0000a"]
                        (map sample (repeatedly 400 #(inc (.nextInt random 24)))))]
    (doseq [candidate strings]
      (is (= (json/write-json-str candidate :escape-slash false
                                  :escape-unicode false
                                  :escape-js-separators false)
             (canonical/rfc8785-safe-integer-json-string-v1 candidate))
          (pr-str candidate)))))

(deftest malformed-utf16-rejection-retains-location
  (doseq [s [(str (char 0xD800)) (str (char 0xDC00)) (str (char 0xD800) "x")]
          [value path position] [[{"nested" [s]} ["nested" 0] :value]
                                 [{"nested" {s "value"}} ["nested"] :object-key]]]
    (let [error (try (canonical/rfc8785-safe-integer-json-string-v1 value) nil
                     (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= {:reason :invalid-utf16 :path path :position position :string-index 0}
             (select-keys error [:reason :path :position :string-index]))))))

(deftest canonical-keys-remain-stricter-than-json-writer-keys
  (is (= {"7" "seven"} (json/read-json (json/write-json-str {7 "seven"}))))
  (doseq [encode [canonical/rfc8785-safe-integer-json-string-v1
                  jcs/rfc8785-string-domain-json-string]]
    (is (= :non-string-object-key
           (try (encode {7 "seven"}) nil
                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
