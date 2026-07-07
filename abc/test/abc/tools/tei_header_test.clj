(ns abc.tools.tei-header-test
  (:require [abc.tools.files :as files]
            [abc.tools.tei :as tei]
            [abc.tools.tei-header :as th]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:private schema-path (atom nil))

(defn require-schema-path [t]
  (if-let [path (System/getenv "TEI_SCHEMA_PATH")]
    (do (reset! schema-path path) (t))
    (throw (ex-info "TEI_SCHEMA_PATH must be set." {:env-var "TEI_SCHEMA_PATH"}))))

(use-fixtures :once require-schema-path)

(def ^:private example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(def ^:private example-person
  (delay (files/read-json "examples/v0/example-persons/000879.json")))

(defn- resolved-input
  "Build the {:work :contributors} input for tei-header/build by
  resolving each contributor's person body from the persons fixture."
  []
  (let [persons-by-id {"000879" @example-person}]
    {:work (get @example-record "work")
     :contributors (mapv (fn [c]
                           {:relation-to-work (get c "relation_to_work")
                            :person (get persons-by-id (get c "person_id"))})
                         (get @example-record "contributors"))}))

(deftest build-returns-hiccup-test
  (testing "build returns a hiccup-style nested vector"
    (let [hdr (th/build (resolved-input))]
      (is (vector? hdr))
      (is (= :teiHeader (first hdr))))))

(deftest emit-xml-returns-string-test
  (testing "emit-xml serialises hiccup to an XML string"
    (let [s (th/emit-xml (th/build (resolved-input)))]
      (is (string? s))
      (is (re-find #"teiHeader" s)))))

(deftest header-validates-against-tei-rng-test
  (testing "the generated header, wrapped in a minimal TEI document, validates"
    (let [hdr-xml (th/emit-xml (th/build (resolved-input)))
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
    (let [s (th/emit-xml (th/build (resolved-input)))]
      (is (re-find #"persName[^>]*xml:lang=\"ja\"" s))
      (is (re-find #"persName[^>]*xml:lang=\"ja-Hira\"" s))
      (is (re-find #"persName[^>]*xml:lang=\"ja-Latn\"" s)))))

(deftest classcode-ndc-test
  (testing "classCode scheme=NDC carries the work's NDC code"
    (let [s (th/emit-xml (th/build (resolved-input)))]
      (is (re-find #"classCode[^>]*scheme=\"NDC\"" s))
      (is (string/includes? s "913")))))
