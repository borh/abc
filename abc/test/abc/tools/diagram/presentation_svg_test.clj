(ns abc.tools.diagram.presentation-svg-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-svg :as svg]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def raw-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200pt\" height=\"100pt\" viewBox=\"0 0 200 100\"><g id=\"graph0\"><text font-size=\"30\">Example</text><path fill=\"none\" stroke=\"#48CAE4\" d=\"M0,0 L10,10\"/></g></svg>")

(def graph
  {:id :test
   :title "Test Figure"
   :subtitle "Test subtitle"
   :description "Accessible description"
   :footer "Citation footer"
   :theme figures/theme})

(deftest normalization-produces-self-contained-slide-svg
  (let [out (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))
        root (xml/parse-str out)
        elements (filter :tag (tree-seq map? :content root))]
    (is (str/includes? out "viewBox=\"0 0 1920 1080\""))
    (is (str/includes? out "fill=\"#000000\""))
    (is (= svg/svg-namespace (xml/qname-uri (:tag root))))
    (is (every? #(= svg/svg-namespace (xml/qname-uri (:tag %))) elements))
    (is (some #(and (= "title" (xml/qname-local (:tag %)))
                    (= ["Test Figure"] (:content %)))
              elements))
    (is (some #(and (= "desc" (xml/qname-local (:tag %)))
                    (= ["Accessible description"] (:content %)))
              elements))
    (is (str/includes? out "data:font/woff2;base64,"))
    (is (str/includes? out "font-size=\"52\""))
    (is (= [] (svg/svg-problems out)))))

(deftest validation-rejects-external-resources-and-unknown-colors
  (let [out (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))
        root (xml/parse-str out)
        image (xml/element (xml/qname svg/svg-namespace "image")
                           {:href "https://example.org/x.png"
                            :fill "#FF00FF"})
        problems (svg/svg-problems
                  (xml/emit-str (update root :content conj image)))]
    (is (some #(str/includes? % "external SVG resource") problems))
    (is (some #(str/includes? % "unapproved SVG color") problems))))

(deftest malformed-graphviz-svg-is-actionable
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Graphviz SVG"
                        (svg/normalize-svg graph "not xml" (byte-array 0)))))

(deftest validator-rejects-a-null-namespace-svg-root
  (is (some #(str/includes? % "root is not in the SVG namespace")
            (svg/svg-problems
             "<svg viewBox=\"0 0 1920 1080\"><title>x</title></svg>"))))
