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
   :groups [{:id :b :label "Second"}
            {:id :a :label "First" :style :dashed}]
   :nodes [{:id :z :label "Zed" :role :output :group :b :backing {}}
           {:id :a
            :label "A \"quoted\" label"
            :subtitle "sub"
            :role :identity
            :group :a
            :backing {}}]
   :edges [{:from :z
            :to :a
            :label "later"
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
    (is (< (.indexOf out "cluster_a") (.indexOf out "cluster_b")))
    (is (< (.indexOf out "\"a\"") (.indexOf out "\"z\"")))
    (is (str/includes? out "A &quot;quoted&quot; label"))
    (is (str/includes? out "label=<<TABLE"))
    (is (str/includes? out "style=\"dashed\""))
    (is (str/includes? out "style=\"rounded,dashed\""))
    (is (= 2 (count (re-seq #"    penwidth=\"2\";" out))))
    (is (str/includes? out
                       "\"a\" -> \"z\" [style=\"invis\",weight=\"100\"]"))
    (is (str/ends-with? out "\n"))))

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
