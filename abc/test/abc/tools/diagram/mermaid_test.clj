(ns abc.tools.diagram.mermaid-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [abc.tools.diagram.mermaid :as mermaid]))

(deftest flowchart-is-deterministic-and-sorted
  (let [g {:direction "LR"
           :nodes [{:id :b :label "Bee" :class "x"} {:id :a :label "Ay"}]
           :edges [{:from :b :to :a :label "uses" :style :dashed}
                   {:from :a :to :b}]
           :class-defs {:x "fill:#111,color:#fff"}}
        out (mermaid/flowchart g)]
    (is (< (.indexOf out "a[\"Ay\"]") (.indexOf out "b[\"Bee\"]:::x")))
    (is (str/includes? out "  a --> b"))
    (is (str/includes? out "  b -.->|\"uses\"| a"))
    (is (str/includes? out "  classDef x fill:#111,color:#fff"))
    (is (str/starts-with? out "flowchart LR\n"))
    (is (str/ends-with? out "\n"))
    (is (= out (mermaid/flowchart g)))))

(deftest flowchart-rejects-id-collisions
  (try
    (mermaid/flowchart {:direction "TD"
                        :nodes [{:id :a-b :label "one"}
                                {:id :a_b :label "two"}]
                        :edges []
                        :class-defs {}})
    (is false "expected sanitized node ids to collide")
    (catch clojure.lang.ExceptionInfo ex
      (is (str/includes? (ex-message ex) "collide"))
      (is (= {"a_b" [:a-b :a_b]} (:collisions (ex-data ex)))))))
