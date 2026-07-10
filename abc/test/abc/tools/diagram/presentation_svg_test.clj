(ns abc.tools.diagram.presentation-svg-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-svg :as svg]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]])
  (:import [java.util Locale]))

(def raw-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200pt\" height=\"100pt\" viewBox=\"0 0 200 100\"><g id=\"graph0\"><text font-size=\"34\">Example</text><path fill=\"none\" stroke=\"#48CAE4\" stroke-width=\"2\" d=\"M0,0 L10,10\"/></g></svg>")

(def graph
  {:id :test
   :title "Test Figure"
   :subtitle "Test subtitle"
   :description "Accessible description"
   :footer "Citation footer"
   :theme figures/theme})

(defn- qualified-element [tag attrs & content]
  (apply xml/element (xml/qname svg/svg-namespace (name tag)) attrs content))

(defn- normalized-root []
  (xml/parse-str
   (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))))

(defn- element-attr [node wanted]
  (some (fn [[key value]]
          (when (= wanted (name key)) value))
        (:attrs node)))

(defn- elements [root]
  (filter :tag (tree-seq map? :content root)))

(defn- find-element [root tag class]
  (some #(when (and (= tag (xml/qname-local (:tag %)))
                    (= class (element-attr % "class")))
           %)
        (elements root)))

(defn- transform-elements [node f]
  (if (:tag node)
    (f (update node :content
               (fn [content]
                 (mapv #(if (map? %) (transform-elements % f) %) content))))
    node))

(defn- raw-svg-with-inner-transform [transform]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\" "
       "width=\"200pt\" height=\"100pt\" viewBox=\"0 0 200 100\">"
       "<g id=\"graph0\"><g transform=\"" transform "\">"
       "<text font-size=\"34\">Example</text>"
       "<path fill=\"none\" stroke=\"#48CAE4\" stroke-width=\"2\" "
       "d=\"M0,0 L10,10\"/></g></g></svg>"))

(deftest normalization-produces-self-contained-slide-svg
  (let [out (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))
        root (xml/parse-str out)
        all-elements (elements root)
        title (find-element root "text" "figure-title")
        subtitle (find-element root "text" "figure-subtitle")
        graph-wrapper (find-element root "g" "figure-graph")
        footer (find-element root "text" "figure-citation")]
    (is (str/includes? out "viewBox=\"0 0 1920 1080\""))
    (is (str/includes? out "fill=\"#000000\""))
    (is (= svg/svg-namespace (xml/qname-uri (:tag root))))
    (is (every? #(= svg/svg-namespace (xml/qname-uri (:tag %))) all-elements))
    (is (some #(and (= "title" (xml/qname-local (:tag %)))
                    (= ["Test Figure"] (:content %)))
              all-elements))
    (is (some #(and (= "desc" (xml/qname-local (:tag %)))
                    (= ["Accessible description"] (:content %)))
              all-elements))
    (is (str/includes? out "data:font/woff2;base64,"))
    (is (str/includes? out "font-size=\"52\""))
    (is (= "1920" (element-attr root "width")))
    (is (= "1080" (element-attr root "height")))
    (is (= "96" (element-attr title "x")))
    (is (= "148" (element-attr title "y")))
    (is (= "96" (element-attr subtitle "x")))
    (is (= "184" (element-attr subtitle "y")))
    (is (= "96" (element-attr footer "x")))
    (is (= "976" (element-attr footer "y")))
    (is (= "52" (element-attr title "font-size")))
    (is (= "22" (element-attr subtitle "font-size")))
    (is (= "16" (element-attr footer "font-size")))
    (is (some? graph-wrapper))
    (is (and graph-wrapper
             (<= 1.0
                 (parse-double (element-attr graph-wrapper "data-graph-scale")))))
    (is (and graph-wrapper
             (str/includes? (element-attr graph-wrapper "transform")
                            "translate(215.0000 195.0000)")))
    (is (= [] (svg/svg-problems out)))))

(deftest normalization-uses-the-fixed-graph-region
  (let [region-raw (str/replace raw-svg "0 0 200 100" "0 0 1728 745")
        root (xml/parse-str
              (svg/normalize-svg graph region-raw
                                 (.getBytes "woff2" "UTF-8")))
        graph-wrapper (find-element root "g" "figure-graph")]
    (is (= "1.000000" (element-attr graph-wrapper "data-graph-scale")))
    (is (= "translate(96.0000 195.0000) scale(1.000000)"
           (element-attr graph-wrapper "transform")))))

(deftest normalization-is-byte-stable-and-valid-across-format-locales
  (let [original-locale (Locale/getDefault)
        normalize-under (fn [locale]
                          (Locale/setDefault locale)
                          (svg/normalize-svg graph raw-svg
                                             (.getBytes "woff2" "UTF-8")))]
    (try
      (let [root-locale-svg (normalize-under Locale/ROOT)
            german-locale-svg (normalize-under Locale/GERMANY)]
        (is (= root-locale-svg german-locale-svg))
        (is (map? (xml/parse-str german-locale-svg)))
        (is (= [] (svg/svg-problems german-locale-svg))))
      (finally
        (Locale/setDefault original-locale)))))

(deftest normalization-rejects-a-graph-that-requires-downscaling
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"scale below 1"
       (svg/normalize-svg
        graph
        (str/replace raw-svg "0 0 200 100" "0 0 2000 1000")
        (.getBytes "woff2" "UTF-8")))))

(deftest graph-content-must-meet-revised-body-floors
  (let [raw (str/replace
             raw-svg
             "<text font-size=\"34\">Example</text>"
             (str "<text font-size=\"33\" font-weight=\"bold\">Bold</text>"
                  "<text font-size=\"23\">Small</text>"
                  "<path fill=\"none\" stroke=\"#48CAE4\" "
                  "stroke-width=\"1\" d=\"M0,0 L10,10\"/>"))
        problems (svg/svg-problems
                  (svg/normalize-svg graph raw (.getBytes "woff2" "UTF-8")))]
    (is (some #(str/includes? % "bold graph text is smaller than 34 px") problems))
    (is (some #(str/includes? % "graph text is smaller than 24 px") problems))
    (is (some #(str/includes? % "graph stroke is thinner than 2 px") problems))))

(deftest graph-content-rejects-descendant-uniform-downscaling
  (let [problems (svg/svg-problems
                  (svg/normalize-svg
                   graph (raw-svg-with-inner-transform "scale(0.5)")
                   (.getBytes "woff2" "UTF-8")))]
    (is (some #(str/includes? % "descendant SVG transform scale") problems))))

(deftest graph-content-rejects-descendant-two-axis-downscaling
  (let [problems (svg/svg-problems
                  (svg/normalize-svg
                   graph (raw-svg-with-inner-transform "scale(1 0.5)")
                   (.getBytes "woff2" "UTF-8")))]
    (is (some #(str/includes? % "descendant SVG transform scale") problems))))

(deftest graph-content-rejects-unsupported-transform-matrices
  (let [problems (svg/svg-problems
                  (svg/normalize-svg
                   graph (raw-svg-with-inner-transform "matrix(1 0 0 1 0 0)")
                   (.getBytes "woff2" "UTF-8")))]
    (is (some #(str/includes? % "unsupported descendant SVG transform")
              problems))))

(deftest graph-content-allows-normal-graphviz-transform-lists
  (let [out (svg/normalize-svg
             graph
             (raw-svg-with-inner-transform
              "scale(1 1) rotate(0) translate(4 96)")
             (.getBytes "woff2" "UTF-8"))]
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

(deftest validation-checks-all-svg-color-bearing-attributes
  (let [root (normalized-root)
        stop (qualified-element :stop {:stop-color "red"})
        problems (svg/svg-problems
                  (xml/emit-str (update root :content conj stop)))]
    (is (some #(str/includes? % "unapproved SVG color red") problems))))

(deftest validation-rejects-style-injection-and-noncanonical-stylesheets
  (let [root (normalized-root)
        style-attribute (assoc-in root [:attrs :style] "background:#000000")
        extra-style (update root :content conj
                            (qualified-element :style {} "text{fill:#F5F7FA}"))
        modified-font (transform-elements
                       root
                       #(if (= "style" (xml/qname-local (:tag %)))
                          (assoc % :content ["@font-face{font-family:evil}"])
                          %))]
    (is (some #(str/includes? % "style attributes are forbidden")
              (svg/svg-problems (xml/emit-str style-attribute))))
    (is (some #(str/includes? % "exactly one embedded-font stylesheet")
              (svg/svg-problems (xml/emit-str extra-style))))
    (is (some #(str/includes? % "embedded-font stylesheet is not canonical")
              (svg/svg-problems (xml/emit-str modified-font))))))

(deftest validation-rejects-nonlocal-resources-in-attributes-and-css
  (let [root (normalized-root)
        local-use (update root :content conj
                          (qualified-element :use {:href "#graph0"}))
        namespace-use (update root :content conj
                              (qualified-element :use {:href svg/svg-namespace}))
        external-base (assoc-in root
                                [:attrs
                                 (xml/qname "http://www.w3.org/XML/1998/namespace"
                                            "base")]
                                "https://example.org/")
        external-src (update root :content conj
                             (qualified-element :image
                                                {:src "https://example.org/x.png"}))
        external-url (update root :content conj
                             (qualified-element :path
                                                {:fill "url(https://example.org/fill.svg)"}))
        external-css-url (transform-elements
                          root
                          #(if (= "style" (xml/qname-local (:tag %)))
                             (update % :content conj
                                     "text{fill:url(https://example.org/x.svg)}")
                             %))
        imported-css (transform-elements
                      root
                      #(if (= "style" (xml/qname-local (:tag %)))
                         (update % :content conj
                                 "@import 'https://example.org/x.css';")
                         %))]
    (is (not-any? #(str/includes? % "external SVG resource")
                  (svg/svg-problems (xml/emit-str local-use))))
    (is (not-any? #(str/includes? % "external SVG resource")
                  (svg/svg-problems (xml/emit-str namespace-use))))
    (is (some #(str/includes? % "external SVG resource")
              (svg/svg-problems (xml/emit-str external-base))))
    (is (some #(str/includes? % "external SVG resource")
              (svg/svg-problems (xml/emit-str external-src))))
    (is (some #(str/includes? % "external SVG resource")
              (svg/svg-problems (xml/emit-str external-url))))
    (is (some #(str/includes? % "external SVG resource")
              (svg/svg-problems (xml/emit-str external-css-url))))
    (is (some #(str/includes? % "CSS @import is forbidden")
              (svg/svg-problems (xml/emit-str imported-css))))))

(deftest validation-does-not-trust-a-scale-marker-that-disagrees-with-transform
  (let [root (normalized-root)
        downscaled (transform-elements
                    root
                    #(if (= "figure-graph" (element-attr % "class"))
                       (update-in % [:attrs :transform]
                                  str/replace #"scale\([^)]*\)" "scale(0.5)")
                       %))]
    (is (some #(str/includes? % "graph transform scale")
              (svg/svg-problems (xml/emit-str downscaled))))))

(deftest validation-requires-fixed-root-dimensions
  (let [root (normalized-root)]
    (is (some #(str/includes? % "width 1920 and height 1080")
              (svg/svg-problems
               (xml/emit-str (assoc-in root [:attrs :width] "1919")))))
    (is (some #(str/includes? % "width 1920 and height 1080")
              (svg/svg-problems
               (xml/emit-str (assoc-in root [:attrs :height] "auto")))))))

(deftest malformed-validator-data-is-returned-as-problems
  (let [root (normalized-root)
        bad-font (transform-elements
                  root
                  #(if (= "figure-title" (element-attr % "class"))
                     (assoc-in % [:attrs :font-size] "large")
                     %))
        non-finite-font (transform-elements
                         root
                         #(if (= "figure-title" (element-attr % "class"))
                            (assoc-in % [:attrs :font-size] "NaN")
                            %))
        bad-background (transform-elements
                        root
                        #(if (= "rect" (xml/qname-local (:tag %)))
                           (update % :attrs dissoc :fill)
                           %))
        bad-scale (transform-elements
                   root
                   #(if (= "figure-graph" (element-attr % "class"))
                      (assoc-in % [:attrs :data-graph-scale] "many")
                      %))
        small-scale (transform-elements
                     root
                     #(if (= "figure-graph" (element-attr % "class"))
                        (assoc-in % [:attrs :data-graph-scale] "0.5")
                        %))]
    (doseq [invalid [bad-font non-finite-font bad-background bad-scale]]
      (let [problems (try
                       (svg/svg-problems (xml/emit-str invalid))
                       (catch Exception _ ::threw))]
        (is (vector? problems))
        (is (and (vector? problems) (seq problems)))))
    (is (some #(str/includes? % "data-graph-scale must be at least 1")
              (svg/svg-problems (xml/emit-str small-scale))))))

(deftest malformed-graphviz-svg-is-actionable
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Graphviz SVG"
                        (svg/normalize-svg graph "not xml" (byte-array 0)))))

(deftest validator-rejects-a-null-namespace-svg-root
  (is (some #(str/includes? % "root is not in the SVG namespace")
            (svg/svg-problems
             "<svg viewBox=\"0 0 1920 1080\"><title>x</title></svg>"))))
