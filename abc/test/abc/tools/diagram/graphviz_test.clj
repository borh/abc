(ns abc.tools.diagram.graphviz-test
  (:require [abc.tools.diagram.graphviz :as graphviz]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def sample
  {:id :sample
   :direction "LR"
   :title "Sample"
   :subtitle "Subtitle"
   :theme {:canvas "#000000"
           :text "#F5F7FA"
           :secondary "#A7B0BE"
           :identity "#48CAE4"
           :evidence "#F2B84B"
           :output "#7BC47F"
           :primary-size 30
           :secondary-size 22
           :stroke-width 2}
   :groups [{:id :b :label "Second" :cluster? false}
            {:id :a :label "First" :style :dashed}]
   :nodes [{:id :z :label "Zed" :role :output :group :b :backing {}}
           {:id :a
            :label "A \"quoted\" presentation node label that wraps predictably"
            :subtitle "This subtitle is deliberately long enough to wrap"
            :label-wrap 18
            :subtitle-wrap 18
            :coordinates [{:label "Coordinate alpha"}
                          {:label "Coordinate beta"}
                          {:label "Coordinate gamma"}]
            :role :identity
            :group :a
            :backing {}}]
   :edges [{:from :z
            :to :a
            :label "compatibility gate"
            :head-port :w
            :role :validation
            :style :dashed
            :backing {}}
           {:from :a :to :z :role :identity :style :thick :backing {}}]
   :primary-order [:a :z]})

(deftest escaping-respects-html-and-dot-contexts
  (testing "HTML labels escape markup-significant characters"
    (is (= "&amp;&quot;&lt;&gt;"
           (graphviz/escape-html "&\"<>"))))
  (testing "ordinary DOT strings escape quotes, backslashes, and newlines"
    (is (= "a\\\"b\\\\c\\nd"
           (graphviz/escape-dot "a\"b\\c\nd")))))

(deftest dot-is-stable-sorted-and-escaped
  (let [out (graphviz/dot sample)]
    (is (= out (graphviz/dot sample)))
    (is (str/starts-with? out "// GENERATED"))
    (is (neg? (.indexOf out "cluster_b")))
    (is (< (.indexOf out "\"a\"") (.indexOf out "\"z\"")))
    (is (str/includes? out
                       "A &quot;quoted&quot;<BR/>presentation node<BR/>label that wraps<BR/>predictably"))
    (is (str/includes? out
                       "This subtitle is<BR/>deliberately long<BR/>enough to wrap"))
    (is (re-find #"Coordinate alpha.*Coordinate beta.*Coordinate gamma.*</TR>" out))
    (is (str/includes? out "label=<<TABLE"))
    (is (str/includes? out "CELLPADDING=\"1\""))
    (is (str/includes? out "nodesep=\"0.02\""))
    (is (str/includes? out "ranksep=\"0.10\""))
    (is (str/includes? out "margin=\"0\";"))
    (is (str/includes? out "margin=\"0.04,0.02\""))
    (is (str/includes? out "xlabel=\"compatibility\\ngate\""))
    (is (str/includes? out "headport=\"w\""))
    (is (str/includes? out "style=\"dashed\""))
    (is (str/includes? out "style=\"rounded,dashed\""))
    (is (= 1 (count (re-seq #"    penwidth=\"2\";" out))))
    (is (= 2 (count (re-seq #"group=\"primary\"" out))))
    (is (str/includes? out
                       "\"a\" -> \"z\" [style=\"invis\",weight=\"100\"]"))
    (is (re-find #"[^\r\n]\n\z" out))))

(deftest dot-rejects-sanitized-node-id-collisions
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"duplicate presentation node ids"
       (graphviz/dot
        (assoc sample :nodes [{:id :a-b :label "A" :role :source}
                              {:id :a.b :label "B" :role :output}])))))

(deftest dot-rejects-duplicate-node-ids
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"duplicate presentation node ids"
       (graphviz/dot
        (assoc sample :nodes [{:id :a :label "A" :role :source}
                              {:id :a :label "B" :role :output}])))))
