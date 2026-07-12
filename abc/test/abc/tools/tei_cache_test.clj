(ns abc.tools.tei-cache-test
  "Schema-cache + concurrency behavior of abc.tools.tei/validate! against
  the repo-local TEI profile schema (no TEI_SCHEMA_PATH gate)."
  (:require [abc.tools.tei :as tei]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private profile-rng "schemas/tei-profile.rng")

(defn- temp-xml! [content]
  (let [tmp (java.io.File/createTempFile "abc-tei-cache" ".xml")]
    (spit tmp content)
    (.deleteOnExit tmp)
    tmp))

(defn- invalid-doc! [root-element]
  (temp-xml! (str "<?xml version=\"1.0\"?><" root-element " xmlns=\"x\"/>")))

(deftest validate-reuses-cached-schema-test
  (let [doc (invalid-doc! "not-tei-alpha")
        run! #(tei/validate! {:schema-path profile-rng
                              :xml-path (str doc)
                              :label "alpha"})
        first-result (run!)
        cache @@#'tei/schema-cache
        second-result (run!)]
    (is (seq (:violations first-result)))
    (is (= first-result second-result)
        "repeat validation must return identical violations")
    (is (some (fn [[[path _mtime] _schema]]
                (clojure.string/ends-with? path "tei-profile.rng"))
              cache)
        "the parsed schema must be cached by canonical path + mtime")
    (is (identical? (some (fn [[k v]] v) cache)
                    (some (fn [[k v]] v) @@#'tei/schema-cache))
        "the second call must reuse the same Schema instance")))

(deftest concurrent-validate-calls-do-not-interfere-test
  (testing "each concurrent call collects only its own document's violations"
    (let [doc-a (invalid-doc! "not-tei-alpha")
          doc-b (invalid-doc! "not-tei-beta")
          results (->> (range 8)
                       (mapv (fn [i]
                               (let [[doc label] (if (even? i)
                                                   [doc-a "alpha"]
                                                   [doc-b "beta"])]
                                 (future
                                   (tei/validate! {:schema-path profile-rng
                                                   :xml-path (str doc)
                                                   :label label})))))
                       (mapv deref))]
      (doseq [{:keys [label violations]} results]
        (let [own (if (= label "alpha") "not-tei-alpha" "not-tei-beta")
              other (if (= label "alpha") "not-tei-beta" "not-tei-alpha")]
          (is (seq violations))
          (is (some #(re-find (re-pattern own) (:message %)) violations)
              (str label " must report its own root element"))
          (is (not-any? #(re-find (re-pattern other) (:message %)) violations)
              (str label " must not see the other document's violations")))))))

(deftest missing-schema-still-fails-loudly-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"Failed to load TEI RelaxNG schema"
                        (tei/validate! {:schema-path "no/such/schema.rng"
                                        :xml-path "also-irrelevant.xml"
                                        :label "x"}))))

(deftest schema-cache-busts-on-mtime-change-test
  (let [schema-copy (java.io.File/createTempFile "abc-tei-schema" ".rng")
        doc (invalid-doc! "not-tei-gamma")
        run! #(tei/validate! {:schema-path (str schema-copy)
                              :xml-path (str doc)
                              :label "gamma"})]
    (try
      (io/copy (io/file profile-rng) schema-copy)
      (run!)
      (let [entries-for (fn []
                          (filterv (fn [[[path _] _]]
                                     (= path (.getCanonicalPath schema-copy)))
                                   @@#'tei/schema-cache))
            before (entries-for)]
        (is (= 1 (count before)))
        ;; rewrite with a newer mtime: a fresh cache entry must appear
        (io/copy (io/file profile-rng) schema-copy)
        (.setLastModified schema-copy (+ 5000 (.lastModified schema-copy)))
        (run!)
        (is (= 2 (count (entries-for)))
            "an edited schema (new mtime) must get its own cache entry"))
      (finally
        (.delete schema-copy)))))
