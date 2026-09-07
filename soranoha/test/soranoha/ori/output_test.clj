(ns soranoha.ori.output-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.main :as main]
            [soranoha.ori.accountability :as accountability]
            [soranoha.ori.render :as render]
            [soranoha.ori.tei-header :as header]
            [soranoha.annotations.view :as text-view]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.validate :as validation])
  (:import [java.io ByteArrayInputStream]
           [javax.xml.parsers DocumentBuilderFactory]))

(deftest canonical-tei-retains-source-identity-and-parser-outcomes
  (let [dir (fs/create-temp-dir {:prefix "tei-outcomes"})
        path (str (fs/path dir "tei.xml"))
        source-id (str "sha256:" (apply str (repeat 64 "a")))
        diagnostic {"code" "unclosed_inline" "severity" "error" "message" "閉じられていない"
                    "span" {"start" 3 "end" 9 "coordinate_system" "decoded_utf8"}}
        result (render/render-work
                {:parser-ir {"source" {"work_content_hash" source-id "primary_text_hash" source-id}
                             "derived_from" {"parse_complete" false}
                             "nodes" [{"type" "text" "text" "本文"}]
                             "errors" [diagnostic]}
                 :metadata-record {"work" {"work_id" "1" "title" "試験" "aozora_modified" "2026-09-07"}
                                   "contributors" []}
                 :persons-by-id {}})]
    (try
      (spit path (:tei result))
      (is (= "本文" (projection/plaintext (text-view/from-tei (:tei result)))))
      (is (re-find #"source-content-hash" (:tei result)))
      (is (re-find #"unclosed_inline" (:tei result)))
      (is (re-find #"decoded_utf8" (:tei result)))
      (is (re-find #"parser-completion" (:tei result)))
      (let [report (validation/tei-validation-result (validation/profile-paths ".") (fs/file path))]
        (is (= "passed" (get report "status")) (pr-str report)))
      (finally (fs/delete-tree dir)))))

(deftest publication-serialization-does-not-invent-mixed-content-whitespace
  (let [result (render/render-work
                {:parser-ir {"nodes" [{"type" "text" "text" "前"}
                                      {"type" "ruby" "ruby" {"base" "池" "reading" "いけ" "scope" "explicit"}}
                                      {"type" "text" "text" "後"}]}
                 :metadata-record {"work" {"title" "試験"} "contributors" []}
                 :persons-by-id {}})
        factory (doto (DocumentBuilderFactory/newInstance) (.setNamespaceAware true))
        doc (.parse (.newDocumentBuilder factory)
                    (ByteArrayInputStream. (.getBytes ^String (:tei result) "UTF-8")))
        ^org.w3c.dom.Element body (.item (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" "body") 0)
        paragraph (.item (.getElementsByTagNameNS body "http://www.tei-c.org/ns/1.0" "p") 0)]
    (is (re-find #">\n  <" (:tei result)))
    (is (= "前池いけ後" (.getTextContent paragraph)))
    (is (= "css" (.getAttribute ^org.w3c.dom.Element (.item (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" "styleDefDecl") 0) "scheme")))))

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
        texts (fn [^org.w3c.dom.Document doc ^String tag]
                (let [nodes (.getElementsByTagNameNS doc "http://www.tei-c.org/ns/1.0" tag)]
                  (mapv #(.getTextContent (.item nodes %)) (range (.getLength nodes)))))]
    (doseq [tag ["head" "p" "s" "ruby" "rb" "rt" "note" "seg"]]
      (is (= (texts compact tag) (texts pretty tag)) tag))
    (is (= (subvec (texts compact "div") 1) (subvec (texts pretty "div") 1)))))

(deftest accountability-is-content-bound-and-review-exports-are-cache-independent
  (let [dir (fs/create-temp-dir {:prefix "accountability-output"})
        root (str (fs/path dir "store"))
        objects (str (fs/path root "kura" "objects"))
        store (engine/open-store! {:cas-dir objects :db-path (str (fs/path dir "trace.sqlite"))})
        put! #(cas/put-bytes! objects (.getBytes ^String % "UTF-8"))
        tei "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\"><text><body><p>本文</p></body></text></TEI>"
        inputs {"source" (put! "本文")}
        stage (accountability/source-stage (accountability/resolve-tool))]
    (try
      (let [first-run (engine/run-stage! store stage inputs)
            warm (engine/run-stage! store stage inputs)
            changed (engine/run-stage! store stage (assoc inputs "source" (put! "変更した本文")))
            report {"works" {"work" (assoc {"tei" (put! tei) "plaintext" (put! "本文")}
                                           "source-accountability" (get-in first-run [:outputs "source-accountability"])
                                           "tei-validation" (put! "{}"))}}
            out (fs/path dir "exports")]
        (is (:cached? warm))
        (is (not (:cached? changed)))
        (is (not= (:outputs first-run) (:outputs changed)))
        (#'main/export-build! root out report)
        (is (= "本文" (slurp (str (fs/path out "work" "plain.txt")))))
        (is (= tei (slurp (str (fs/path out "work" "tei.xml")))))
        (is (fs/exists? (fs/path out "work" "source-accountability.json"))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
