(ns soranoha.ori.output-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.main :as main]
            [soranoha.ori.fidelity-test :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.stages :as stages]
            [soranoha.ported.tei-header :as header])
  (:import [java.io ByteArrayInputStream]
           [javax.xml.parsers DocumentBuilderFactory]))

(deftest publication-serialization-does-not-invent-mixed-content-whitespace
  (let [result (render/render-work
                {:parser-ir {"sentence_segmentation" {}
                             "nodes" [{"type" "text" "text" "前"}
                                      {"type" "ruby" "ruby" {"base" "池" "reading" "いけ" "scope" "explicit"}}
                                      {"type" "text" "text" "後"}]}
                 :metadata-record {"work" {"title" "試験"} "contributors" []}
                 :persons-by-id {}})
        factory (doto (DocumentBuilderFactory/newInstance) (.setNamespaceAware true))
        doc (.parse (.newDocumentBuilder factory)
                    (ByteArrayInputStream. (.getBytes ^String (:tei result) "UTF-8")))
        body (.item (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" "body") 0)
        paragraph (.item (.getElementsByTagNameNS body "http://www.tei-c.org/ns/1.0" "p") 0)]
    (is (re-find #">\n  <" (:tei result)))
    (is (= "前池いけ後" (.getTextContent paragraph)))
    (is (= "css" (.getAttribute (.item (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" "styleDefDecl") 0) "scheme")))))

(deftest inspection-formatting-respects-mixed-content-and-space-preservation
  (let [mixed [:div [:head "見出し"]
               [:p [:s "前" [:ruby [:rb "犍" [:g {:ref "#g"}]] [:rt "かん"]] "後"]]
               [:note [:seg "底本"] [:lb] [:seg "刊行日"]]]
        preserved [:div {:xml/space "preserve"} [:p "そのまま"] [:p "次"]]
        explicit-text [:div " " [:p "空白も本文"]]
        hiccup [:TEI [:text [:body mixed preserved explicit-text]]]
        factory (doto (DocumentBuilderFactory/newInstance) (.setNamespaceAware true))
        parse #(-> (.newDocumentBuilder factory)
                   (.parse (ByteArrayInputStream. (.getBytes ^String % "UTF-8"))))
        compact (parse (header/hiccup->xml-string hiccup))
        pretty (parse (header/hiccup->pretty-xml-string hiccup))
        texts (fn [doc tag]
                (let [nodes (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" tag)]
                  (mapv #(.getTextContent (.item nodes %)) (range (.getLength nodes)))))]
    (doseq [tag ["head" "p" "s" "ruby" "rb" "rt" "note" "seg"]]
      (is (= (texts compact tag) (texts pretty tag)) tag))
    (is (= (subvec (texts compact "div") 1) (subvec (texts pretty "div") 1)))))

(deftest fidelity-is-content-bound-and-review-exports-are-cache-independent
  (let [dir (fs/create-temp-dir {:prefix "fidelity-output"})
        root (str (fs/path dir "store"))
        objects (str (fs/path root "kura" "objects"))
        store (engine/open-store! {:cas-dir objects :db-path (str (fs/path dir "trace.sqlite"))})
        put! #(cas/put-bytes! objects (.getBytes ^String % "UTF-8"))
        inputs {"source" (put! fixture/source) "tei" (put! fixture/tei)
                "plaintext" (put! fixture/plaintext)}
        stage (stages/source-fidelity-stage "test-runtime")]
    (try
      (let [first-run (engine/run-stage! store stage inputs)
            warm (engine/run-stage! store stage inputs)
            changed (engine/run-stage! store stage (assoc inputs "plaintext" (put! "omitted")))
            report {"works" {"work" (assoc (dissoc inputs "source")
                                           "source-fidelity" (get-in first-run [:outputs "source-fidelity"])
                                           "tei-validation" (put! "{}"))}}
            out (fs/path dir "exports")]
        (is (:cached? warm))
        (is (not (:cached? changed)))
        (is (not= (:outputs first-run) (:outputs changed)))
        (#'main/export-build! root out report)
        (is (= fixture/plaintext (slurp (str (fs/path out "work" "plain.txt")))))
        (is (= fixture/tei (slurp (str (fs/path out "work" "tei.xml")))))
        (is (fs/exists? (fs/path out "work" "source-fidelity.json"))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
