(ns abc.tools.tei-header-test
  (:require [abc.tools.files :as files]
            [abc.tools.tei :as tei]
            [abc.tools.tei-header :as th]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:private skip-flag-name "ABC_TEI_SCHEMA_SKIP")
(def ^:private schema-path (atom nil))

(defn require-schema-path [t]
  (cond
    (= "1" (System/getenv skip-flag-name))
    (println "abc.tools.tei-header-test: skipping (" skip-flag-name "=1)")

    (nil? (System/getenv "TEI_SCHEMA_PATH"))
    (throw (ex-info "TEI_SCHEMA_PATH must be set." {:env-var "TEI_SCHEMA_PATH"}))

    :else
    (do (reset! schema-path (System/getenv "TEI_SCHEMA_PATH")) (t))))

(use-fixtures :once require-schema-path)

(def ^:private example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(deftest build-returns-hiccup-test
  (testing "build returns a hiccup-style nested vector"
    (let [hdr (th/build @example-record)]
      (is (vector? hdr))
      (is (= :teiHeader (first hdr))))))

(deftest emit-xml-returns-string-test
  (testing "emit-xml serialises hiccup to an XML string"
    (let [s (th/emit-xml (th/build @example-record))]
      (is (string? s))
      (is (re-find #"teiHeader" s)))))

(deftest header-validates-against-tei-rng-test
  (testing "the generated header, wrapped in a minimal TEI document, validates"
    (let [hdr-xml (th/emit-xml (th/build @example-record))
          ;; emit-xml puts the TEI namespace on the root element it
          ;; produces (the teiHeader). To validate as a TEI document
          ;; we need a TEI root with text/body. Strip the XML decl
          ;; from the header fragment and wrap it.
          hdr-no-decl (string/replace hdr-xml #"^<\?xml[^?]*\?>" "")
          doc (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                   "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
                   hdr-no-decl
                   "<text><body><p>placeholder</p></body></text>"
                   "</TEI>")
          tmp (java.io.File/createTempFile "abc-tei-header" ".xml")]
      (try
        (spit tmp doc)
        (let [{:keys [violations]} (tei/validate! {:schema-path @schema-path
                                                   :xml-path (str tmp)
                                                   :label "header"})
              failures (filter #(#{:error :fatal} (:severity %)) violations)]
          (is (empty? failures)
              (str "generated TEI header must validate; violations: "
                   (pr-str failures))))
        (finally (.delete tmp))))))

(deftest persname-triplet-test
  (testing "TEI-EAJ persName triplet (kanji + hiragana + romaji) is present"
    (let [s (th/emit-xml (th/build @example-record))]
      (is (re-find #"persName[^>]*xml:lang=\"ja\"" s))
      (is (re-find #"persName[^>]*xml:lang=\"ja-Hira\"" s))
      (is (re-find #"persName[^>]*xml:lang=\"ja-Latn\"" s)))))

(deftest classcode-ndc-test
  (testing "classCode scheme=NDC carries the work's NDC code"
    (let [s (th/emit-xml (th/build @example-record))]
      (is (re-find #"classCode[^>]*scheme=\"NDC\"" s))
      (is (string/includes? s "913")))))
