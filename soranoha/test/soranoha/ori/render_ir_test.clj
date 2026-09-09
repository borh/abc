(ns soranoha.ori.render-ir-test
  "The render-IR projection and what it is for.

  Render reads every parser-IR field except `interpretation_facts`. Binding
  render's input to a projection that drops that one field is only sound while
  that stays true, and only useful if the projected bytes are unchanged by a
  facts-only change. Both halves are asserted here: the first as TEI equality
  over an IR that exercises every field render does read, the second through
  the engine, where the payoff is visible as a cache hit."
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.test :refer [deftest is testing]]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.fixture :as fixture]
            [soranoha.ori.render :as render]
            [soranoha.ori.stages :as stages]))

(def ^:private metadata-record
  {"work" {"title" "試験" "work_id" "1" "aozora_modified" "2026-09-07"}
   "contributors" []})

(defn- parser-ir
  "An IR exercising every field render reads, plus the one it does not.
  `facts` and `problems` are the two the tests vary."
  [{:keys [facts problems]}]
  {"nodes" [{"type" "text" "text" "本文"}]
   "source" {"decode_outcome" "lossless"
             "work_content_hash" "sha256:work"
             "primary_text_hash" "sha256:primary"}
   "derived_from" {"parse_complete" true}
   "warnings" [{"code" "W1" "severity" "warning" "message" "注意"
                "span" {"start" 0 "end" 6}}]
   "errors" []
   "interpretation_problems" (vec problems)
   "interpretation_facts" (vec facts)})

(defn- tei-for [ir]
  (:tei (render/render-work {:rights @fixture/grant
                             :parser-ir ir
                             :metadata-record metadata-record
                             :persons-by-id {}
                             :slug "000001_000001"})))

(defn- project [ir]
  (let [stage (stages/render-ir-stage "test-runtime")
        bytes (.getBytes ^String (json/write-json-str ir) "UTF-8")]
    (get ((:f stage) {:blob {"ir" bytes}} {"parser-ir" "ir"}) "parser-ir")))

(deftest the-projection-renders-the-tei-the-full-ir-renders
  (let [ir (parser-ir {:facts [{"kind" "emphasis" "outcome" "established"}]
                       :problems [{"code" "P1" "detail" "説明"}]})]
    (is (= (tei-for ir)
           (tei-for (json/read-json (String. ^bytes (project ir) "UTF-8"))))
        "dropping interpretation_facts must not change a single byte of TEI")))

(deftest a-facts-only-change-leaves-the-projected-bytes-alone
  (let [a (parser-ir {:facts [{"kind" "emphasis" "outcome" "established"}] :problems []})
        b (parser-ir {:facts [{"kind" "kunten" "outcome" "established"}
                              {"kind" "ruby" "outcome" "established"}]
                      :problems []})]
    (is (not= (json/write-json-str a) (json/write-json-str b))
        "the two IRs differ, or the next assertion proves nothing")
    (is (= (seq (project a)) (seq (project b))))))

(deftest a-change-render-reads-changes-the-projected-bytes
  (let [a (parser-ir {:facts [] :problems []})
        b (parser-ir {:facts [] :problems [{"code" "P1" "detail" "説明"}]})]
    (is (not= (seq (project a)) (seq (project b)))
        "an interpretation problem reaches TEI, so it must reach render's key")))

(deftest render-is-skipped-for-a-facts-only-change-and-rerun-for-a-read-one
  (let [dir (fs/create-temp-dir {:prefix "render-ir"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})]
    (try
      (let [put (fn [value] (cas/put-bytes! (:cas-dir store)
                                            (.getBytes ^String (json/write-json-str value) "UTF-8")))
            metadata-id (put metadata-record)
            persons-id (put {})
            render-ir (stages/render-ir-stage "test-runtime")
            render (stages/render-stage "test-runtime" @fixture/grant)
            run (fn [ir]
                  (let [projected (engine/run-stage! store render-ir {"parser-ir" (put ir)})
                        tei (engine/run-stage! store render
                                               {"parser-ir" (get-in projected [:outputs "parser-ir"])
                                                "metadata-record" metadata-id
                                                "persons" persons-id
                                                "slug" "000001_000001"})]
                    {:projected projected :tei tei}))
            first-run (run (parser-ir {:facts [{"kind" "emphasis" "outcome" "established"}]
                                       :problems []}))
            facts-changed (run (parser-ir {:facts [{"kind" "ruby" "outcome" "established"}]
                                           :problems []}))
            problem-added (run (parser-ir {:facts [{"kind" "ruby" "outcome" "established"}]
                                           :problems [{"code" "P1" "detail" "説明"}]}))]
        (testing "a facts-only change"
          (is (false? (:cached? (:projected facts-changed)))
              "the full IR changed, so the projection is recomputed")
          (is (true? (:cached? (:tei facts-changed)))
              "the projected bytes did not, so render is served from cache")
          (is (= (get-in first-run [:tei :outputs "tei"])
                 (get-in facts-changed [:tei :outputs "tei"]))))
        (testing "a change render reads"
          (is (false? (:cached? (:tei problem-added))))
          (is (not= (get-in facts-changed [:tei :outputs "tei"])
                    (get-in problem-added [:tei :outputs "tei"])))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
