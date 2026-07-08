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

(defn- wrapped-header-doc [header-xml]
  (let [header-fragment (string/replace header-xml #"^<\?xml[^?]*\?>" "")]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
         "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
         header-fragment
         "<text><body><p>placeholder</p></body></text>"
         "</TEI>")))

(defn- validation-failures [xml-string]
  (let [tmp (java.io.File/createTempFile "abc-tei-header" ".xml")]
    (try
      (spit tmp xml-string)
      (let [{:keys [violations]} (tei/validate! {:schema-path @schema-path
                                                 :xml-path (str tmp)
                                                 :label "header"})]
        (filter #(#{:error :fatal} (:severity %)) violations))
      (finally (.delete tmp)))))

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
          failures (validation-failures (wrapped-header-doc hdr-xml))]
      (is (empty? failures)
          (str "generated TEI header must validate; violations: "
               (pr-str failures))))))

(deftest header-without-source-editions-validates-test
  (testing "sparse official metadata still emits a non-empty sourceDesc"
    (let [input (update (resolved-input)
                        :work assoc
                        "source_editions" []
                        "card_url" "https://www.aozora.gr.jp/cards/000009/card8.html")
          hdr-xml (th/emit-xml (th/build input))
          failures (validation-failures (wrapped-header-doc hdr-xml))]
      (is (not (string/includes? hdr-xml "<sourceDesc/>")))
      (is (string/includes? hdr-xml "aozora-card-url"))
      (is (empty? failures)
          (str "generated TEI header must validate; violations: "
               (pr-str failures))))))

(deftest header-without-ndc-validates-test
  (testing "official metadata without NDC omits textClass instead of crashing"
    (let [input (update (resolved-input) :work assoc "ndc" nil)
          hdr-xml (th/emit-xml (th/build input))
          failures (validation-failures (wrapped-header-doc hdr-xml))]
      (is (not (string/includes? hdr-xml "classCode")))
      (is (empty? failures)
          (str "generated TEI header must validate; violations: "
               (pr-str failures))))))

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
