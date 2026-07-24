(ns abc.tools.diagram.figure-theme-test
  (:require [abc.tools.diagram.figure-theme :as figure-theme]
            [abc.tools.diagram.graphviz :as graphviz]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest presentation-theme-tokens-are-the-frozen-jadh-visual-language
  (is (= {:canvas "#000000"
          :text "#F5F7FA"
          :secondary "#A7B0BE"
          :identity "#48CAE4"
          :evidence "#F2B84B"
          :output "#7BC47F"
          :font-family "Noto Sans CJK JP"
          :primary-size 34
          :secondary-size 24
          :stroke-width 2}
         figure-theme/presentation)))

(defn- render [nodes]
  (graphviz/dot {:id :theme-probe
                 :direction "LR"
                 :theme figure-theme/presentation
                 :groups []
                 :nodes nodes
                 :edges []
                 :primary-order []}))

(deftest presentation-theme-applies-canvas-font-and-scale
  (let [out (render [{:id :n :label "Node" :role :identity}])]
    (testing "black canvas is applied, not transparent"
      (is (str/includes? out "bgcolor=\"#000000\""))
      (is (not (str/includes? out "transparent"))))
    (testing "Noto Sans CJK JP reaches graph, node, and body text"
      (is (str/includes? out "fontname=\"Noto Sans CJK JP\"")))
    (testing "34/24 graph-body type scale and 2px rules"
      (is (str/includes? out "POINT-SIZE=\"34\""))
      (is (str/includes? out "penwidth=\"2\"")))))

(deftest presentation-theme-role-colors-map-semantically
  (doseq [[role color] [[:identity "#48CAE4"]
                        [:coordinate-family "#48CAE4"]
                        [:evidence "#F2B84B"]
                        [:validation "#F2B84B"]
                        [:output "#7BC47F"]
                        [:source "#A7B0BE"]]]
    (let [out (render [{:id role :label (name role) :role role}])]
      (is (str/includes? out (str "color=\"" color "\"")) (name role))
      (is (str/includes? out "fontcolor=\"#F5F7FA\"") (name role)))))

(deftest validation-role-is-reinforced-beyond-color
  (testing "validation nodes render as diamonds so the distinction from
            evidence survives grayscale reproduction"
    (is (str/includes? (render [{:id :v :label "Gate" :role :validation}])
                       "shape=\"diamond\""))
    (is (str/includes? (render [{:id :e :label "Fact" :role :evidence}])
                       "shape=\"rect\""))))
