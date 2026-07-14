(ns abc.tools.adr-evidence-runtime-inputs-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime]
            [abc.tools.files :as files]
            [abc.tools.manifest-to-rdf :as manifest-to-rdf]
            [abc.tools.shacl :as shacl]
            [abc.tools.validate-design-bundle :as validate-design-bundle]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn- temp-dir []
  (fs/file (fs/create-temp-dir {:prefix "runtime-inputs-"})))

(defn- write! [root path body]
  (let [file (fs/file root path)]
    (fs/create-dirs (fs/parent file))
    (spit file body)
    file))

(defn- exec! [root & argv]
  (let [{:keys [exit out]} @(process/process
                             (vec argv)
                             {:dir (str root) :out :string :err :out})
        output out]
    (when-not (zero? exit)
      (throw (ex-info "fixture command failed" {:argv argv :output output})))
    output))

(defn- monorepo-root []
  (let [root (temp-dir)]
    (doseq [path ["flake.nix" "justfile" "abc/flake.nix" "ab-validator/flake.nix"]]
      (write! root path "fixture\n"))
    (exec! root "git" "init" "-q")
    root))

(defn- analyzer-repo [body]
  (let [root (temp-dir)]
    (write! root "src/example/core.clj" (str "(ns example.core)\n" body "\n"))
    (write! root "test/.keep" "")
    root))

(defn- boundary-analyzer-repo [body]
  (let [root (analyzer-repo body)]
    (write! root "src/abc/tools/evidence_io.clj"
            (str "(ns abc.tools.evidence-io (:require [babashka.fs :as fs]))\n"
                 "(defn with-read-trace [options thunk] (thunk))\n"
                 "(defn with-ephemeral-root [root thunk] (thunk))\n"
                 "(defn record-read! [path] path)\n"
                 "(defn with-owned-ephemeral-root [thunk]\n"
                 "  (fs/with-temp-dir [] (thunk \"tmp\")))\n"))
    (write! root "src/abc/tools/files.clj"
            (str "(ns abc.tools.files (:import [java.util.zip ZipFile]))\n"
                 "(defn with-zip-file [archive callback]\n"
                 "  (with-open [zip (ZipFile. (abc.tools.evidence-io/record-read! archive))]\n"
                 "    (callback zip)))\n"))
    (write! root "src/abc/tools/adr_evidence_runtime_inputs.clj"
            (str "(ns abc.tools.adr-evidence-runtime-inputs)\n"
                 "(defn assert-runtime-input-closure! [options] true)\n"
                 "(defn with-validated-read-trace! [options thunk] (thunk))\n"))
    root))

(defn- problem-kind [thunk]
  (try (thunk) nil (catch Exception e (:kind (ex-data e)))))

(deftest source-reader-resolves-aliases-without-global-namespace-state-test
  (let [fixture (write! (temp-dir) "alias_test.clj"
                        (str "(ns fixture.alias-test\n"
                             "  (:require [abc.sim.artifact-manifest :as am]))\n"
                             "(def event ::am/run-summary-events)\n"))
        results (->> (range 8)
                     (mapv (fn [_]
                             (future (runtime/read-source-forms! fixture))))
                     (mapv deref))]
    (is (apply = results))
    (is (= :abc.sim.artifact-manifest/run-summary-events
           (nth (second (first results)) 2)))
    (is (every? #(and (:line (meta %)) (:column (meta %)))
                (first results)))
    (is (nil? (find-ns 'fixture.alias-test)))))

(deftest source-reader-disables-read-eval-test
  (let [property "abc.tools.adr-evidence-runtime-inputs-test/read-eval"
        fixture (write! (temp-dir) "read_eval_test.clj"
                        (str "(ns fixture.read-eval-test)\n"
                             "#=(System/setProperty \"" property "\" \"executed\")\n"))]
    (System/clearProperty property)
    (try
      (is (thrown? Exception (runtime/read-source-forms! fixture)))
      (is (nil? (System/getProperty property)))
      (finally
        (System/clearProperty property)))))

(deftest workspace-root-is-the-exact-git-and-monorepo-identity-test
  (let [root (monorepo-root)
        child (fs/file root "abc")]
    (is (= (fs/file (fs/canonicalize root))
           (runtime/validate-workspace-root! root)))
    (is (= :invalid-workspace-root
           (problem-kind #(runtime/validate-workspace-root! child))))
    (fs/delete (fs/file root "justfile"))
    (is (= :invalid-workspace-root
           (problem-kind #(runtime/validate-workspace-root! root))))))

(deftest runtime-input-closure-is-exact-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1"
                                    :roots []
                                    :explicit [descriptor-path manifest-path "data/value.edn"]}}]
    (write! root descriptor-path (pr-str descriptor))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["data/value.edn"]}))
    (write! root "data/value.edn" "{}")
    (is (true? (runtime/assert-runtime-input-closure!
                {:repo-root root :workspace-root root
                 :descriptor {:path descriptor-path :value descriptor}
                 :repository-paths ["data/value.edn"]})))
    (is (= :undeclared-runtime-input
           (:kind (ex-data
                   (try
                     (runtime/assert-runtime-input-closure!
                      {:repo-root root :workspace-root root
                       :descriptor {:path descriptor-path :value descriptor}
                       :repository-paths []})
                     (catch Exception e e))))))))

(deftest deep-read-boundary-validates-its-own-completed-trace-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        data-path "data/value.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1"
                                    :roots []
                                    :explicit [descriptor-path manifest-path data-path]}}
        options {:repo-root root :workspace-root root
                 :identity-root root :cwd-root root
                 :descriptor {:path descriptor-path :value descriptor}}]
    (write! root descriptor-path (pr-str descriptor))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths [data-path]}))
    (write! root data-path "{:value 42}")
    (let [boundary (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                               'with-validated-read-trace!)]
      (is (some? boundary))
      (when boundary
        (is (= {:value 42}
               (@boundary options #(files/read-edn (fs/path root data-path)))))
        (is (= :undeclared-runtime-input
               (problem-kind #(@boundary options (constantly :no-read)))))))))

(deftest runtime-manifest-rejects-noncanonical-vectors-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v2"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "clojure-test-v1" :roots []
                                    :explicit [descriptor-path manifest-path "data/a" "data/b"]}}]
    (write! root descriptor-path "descriptor")
    (write! root "data/a" "a")
    (write! root "data/b" "b")
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["data/b" "data/a"]}))
    (is (= :invalid-runtime-input-manifest
           (:kind (ex-data
                   (try
                     (runtime/assert-runtime-input-closure!
                      {:repo-root root :workspace-root root
                       :descriptor {:path descriptor-path :value descriptor}
                       :repository-paths []})
                     (catch Exception e e))))))))

(deftest component-runtime-closure-normalizes-one-explicit-workspace-prefix-test
  (let [root (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        descriptor {:schema-version "abc-adr-evidence-capture-v3"
                    :runtime-input-manifest manifest-path
                    :input-profile {:kind "component-clojure-test-v1"
                                    :component-root "abc"
                                    :roots ['example.core-test]
                                    :explicit [descriptor-path manifest-path "data/input.txt"]}}]
    (write! root descriptor-path "descriptor")
    (write! root "data/input.txt" "input")
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["abc/data/input.txt"]}))
    (is (true? (runtime/assert-runtime-input-closure!
                {:repo-root root
                 :workspace-root root
                 :component-root "abc"
                 :descriptor {:path descriptor-path :value descriptor}
                 :repository-paths ["data/input.txt"]})))
    (is (map? (runtime/validate-runtime-input-manifest!
               {:repo-root root
                :workspace-root root
                :component-root "abc"
                :descriptor {:path descriptor-path :value descriptor}})))
    (write! root manifest-path
            (pr-str {:schema-version :abc-adr-runtime-inputs-v1
                     :paths ["other/data/input.txt"]}))
    (is (= :invalid-runtime-input-manifest
           (problem-kind
            #(runtime/assert-runtime-input-closure!
              {:repo-root root
               :workspace-root root
               :component-root "abc"
               :descriptor {:path descriptor-path :value descriptor}
               :repository-paths ["data/input.txt"]}))))))

(deftest runtime-manifest-rejects-every-noncontained-path-shape-test
  (let [root (temp-dir)
        outside (temp-dir)
        descriptor-path "docs/evidence/adr-capture/example.edn"
        manifest-path "docs/evidence/adr-inputs/example.edn"
        outside-file (write! outside "outside.txt" "outside")
        link (fs/file root "linked.txt")]
    (fs/create-dirs (fs/parent (fs/file root manifest-path)))
    (fs/create-sym-link link outside-file)
    (doseq [[label paths]
            [["absolute" [(str (fs/absolutize outside-file))]]
             ["parent" ["../outside.txt"]]
             ["missing" ["missing.txt"]]
             ["duplicate" ["linked.txt" "linked.txt"]]
             ["symlink escape" ["linked.txt"]]]]
      (write! root manifest-path
              (pr-str {:schema-version :abc-adr-runtime-inputs-v1 :paths paths}))
      (let [descriptor {:schema-version "abc-adr-evidence-capture-v2"
                        :runtime-input-manifest manifest-path
                        :input-profile {:kind "clojure-test-v1" :roots []
                                        :explicit (into [descriptor-path manifest-path] paths)}}]
        (is (contains? #{:invalid-runtime-input-manifest :missing-runtime-input}
                       (problem-kind
                        #(runtime/validate-runtime-input-manifest!
                          {:repo-root root :workspace-root root
                           :descriptor {:path descriptor-path :value descriptor}})))
            label)))))

(deftest reachable-var-analysis-expands-finite-parameter-contract-test
  (let [result (runtime/analyze-reachable-vars
                "." ['abc.tools.manifest/content])]
    (is (some #{'abc.tools.manifest/content} (:reachable-vars result)))
    (is (some #{'abc.tools.hash/sha256-file} (:reachable-vars result)))
    (is (= ["data/evidence-higher-order-calls/manifest-content.edn"]
           (:contract-paths result)))
    (is (some #{"src/abc/tools/manifest.clj"} (:paths result)))))

(deftest reachable-var-analysis-uses-private-adr-caller-contract-test
  (let [result (runtime/analyze-reachable-vars
                "." ['abc.tools.adr/validate-repository*])]
    (is (some #{'abc.tools.adr/validate-adrs} (:reachable-vars result)))
    (is (= ["data/evidence-higher-order-calls/adr-validate-repository-star.edn"]
           (:contract-paths result)))))

(deftest reachable-var-lint-is-narrow-and-default-deny-test
  (let [root (analyzer-repo
              "(defn unsafe [] (slurp \"secret\"))\n(defn threaded-unsafe [] (-> \"secret\" slurp))\n(defn safe [] (-> 1 inc inc))\n(defn value-only [] slurp)")]
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/unsafe]))))
    (is (= ['example.core/safe]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['example.core/safe]))))
    (is (= ['example.core/value-only]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['example.core/value-only]))))
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/threaded-unsafe]))))))

(deftest diagram-core-raw-drift-read-is-default-denied-test
  (let [root (analyzer-repo
              "(defn drift [out-path] (when out-path (slurp out-path)))")]
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/drift]))))))

(deftest reachable-var-lint-rejects-computed-and-unresolved-heads-test
  (let [computed (analyzer-repo "(defn bad [f] ((identity f) :x))")
        local-call (analyzer-repo "(defn bad [] (let [f identity] (f :x)))")]
    (is (= :unsupported-evidence-call-graph
           (problem-kind #(runtime/analyze-reachable-vars computed ['example.core/bad]))))
    (is (= :reader-kondo-span-mismatch
           (problem-kind #(runtime/analyze-reachable-vars local-call ['example.core/bad]))))))

(deftest reachable-var-lint-denies-each-physical-bypass-family-test
  (doseq [[label body]
          [["reader" "(defn bad [] (clojure.java.io/reader \"x\"))"]
           ["stream" "(defn bad [] (clojure.java.io/input-stream \"x\"))"]
           ["file-reader" "(defn bad [] (java.io.FileReader. \"x\"))"]
           ["nio" "(defn bad [] (java.nio.file.Files/readAllBytes (java.nio.file.Path/of \"x\" (make-array String 0))))"]
           ["directory" "(defn bad [] (.listFiles (java.io.File. \".\")))"]
           ["archive" "(defn bad [] (java.util.zip.ZipFile. \"x\"))"]
           ["process" "(defn bad [] (java.lang.ProcessBuilder. [\"true\"]))"]
           ["shell" "(defn bad [] (clojure.java.shell/sh \"true\"))"]
           ["network" "(defn bad [] (java.net.URL. \"https://example.invalid\"))"]]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))
          label))))

(deftest reachable-var-lint-closes-indirect-and-jvm-invocation-bypasses-test
  (doseq [[label body]
          [["higher-order raw read" "(defn bad [] (apply slurp [\"x\"]))"]
           ["file reader constructor" "(defn bad [] (new java.io.FileReader \"x\"))"]
           ["http client factory" "(defn bad [] (java.net.http.HttpClient/newHttpClient))"]
           ["runtime exec" "(defn bad [] (.exec (Runtime/getRuntime) \"true\"))"]
           ["file channel" "(defn bad [] (java.nio.channels.FileChannel/open (java.nio.file.Path/of \"x\" (make-array String 0)) (make-array java.nio.file.OpenOption 0)))"]]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))
          label))))

(deftest reachable-var-lint-rejects-code-loading-and-unreviewed-external-vars-test
  (doseq [[label body]
          [["eval" "(defn bad [] (eval '(+ 1 2)))"]
           ["read-string" "(defn bad [] (read-string \"(+ 1 2)\"))"]
           ["load-string" "(defn bad [] (load-string \"(+ 1 2)\"))"]
           ["load-file" "(defn bad [] (load-file \"other.clj\"))"]
           ["resolve" "(defn bad [] (resolve 'clojure.core/slurp))"]
           ["ns-resolve" "(defn bad [] (ns-resolve 'clojure.core 'slurp))"]
           ["requiring-resolve" "(defn bad [] (requiring-resolve 'clojure.core/slurp))"]
           ["babashka fs" "(ns example.core (:require [babashka.fs :as fs]))\n(defn bad [] (fs/read-all-lines \"x\"))"]]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-capability
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))
          label))))

(deftest reachable-var-lint-admits-only-exact-reviewed-pure-capabilities-test
  (doseq [[body expected]
          [["(ns example.core (:require [charred.api :as json]))\n(defn safe [] (json/write-json-str {:ok true}))"
            ['example.core/safe]]
           ["(defn safe [] (re-pattern \"a+\"))"
            ['example.core/safe]]]]
    (let [root (analyzer-repo body)]
      (is (= expected
             (:reachable-vars
              (runtime/analyze-reachable-vars root ['example.core/safe]))))))
  (doseq [body ["(ns example.core (:require [charred.api :as json]))\n(defn bad [] (json/read-json \"{}\"))"
                "(defn bad [] (load-string \"(+ 1 2)\"))"]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-capability
             (problem-kind #(runtime/analyze-reachable-vars
                             root ['example.core/bad])))))))

(deftest higher-order-calls-require-a-direct-finite-target-test
  (doseq [[label body expected]
          [["vector-hidden read"
            "(defn bad [] (apply (first [slurp]) [\"x\"]))"
            :unsupported-evidence-call-graph]
           ["map-hidden process"
            "(ns example.core (:require [babashka.process :as process]))\n(defn bad [] (map (get {:run process/process} :run) [[\"true\"]]))"
            :unsupported-evidence-call-graph]
           ["literal callable with forbidden body"
            "(defn bad [] (filter (fn [x] (slurp x)) [\"x\"]))"
            :forbidden-evidence-io]]]
    (let [root (analyzer-repo body)]
      (is (= expected
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))
          label))))

(deftest direct-higher-order-target-is-enqueued-and-linted-test
  (let [root (analyzer-repo
              "(defn unsafe [x] (slurp x))\n(defn bad [] (map unsafe [\"x\"]))")]
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad]))))))

(deftest domain-functions-cannot-hide-transitive-read-or-process-bypasses-test
  (doseq [[label domain-var domain-body]
          [["date parser raw read"
            'abc.tools.aozora-csv/parse-date
            "(defn parse-date [value] (slurp value))"]
           ["ingest runner raw read"
            'abc.tools.aozora-ingest/run-from-rows!
            "(defn run-from-rows! [options] (slurp (:input options)))"]
           ["drift RDF process"
            'abc.tools.person-drift/event->graph
            "(defn event->graph [event] (java.lang.ProcessBuilder. [\"true\"]))"]
           ["SHACL validation raw read"
            'abc.tools.shacl/validate!
            "(defn validate! [options] (slurp (:shapes options)))"]
           ["history audit process"
            'abc.tools.aozora-history-audit/drift-participant-updates
            "(defn drift-participant-updates [options] (java.lang.ProcessBuilder. [\"true\"]))"]]]
    (let [root (temp-dir)
          domain-ns (symbol (namespace domain-var))
          domain-path (str "src/" (-> (namespace domain-var)
                                      (string/replace "." "/")
                                      (string/replace "-" "_"))
                           ".clj")
          domain-name (symbol (name domain-var))]
      (write! root domain-path (str "(ns " domain-ns ")\n" domain-body "\n"))
      (write! root "test/example/contract_test.clj"
              (str "(ns example.contract-test (:require [" domain-ns " :as domain]))\n"
                   "(defn contract [] (domain/" domain-name " {}))\n"))
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars
                             root ['example.contract-test/contract])))
          label))))

(deftest untraced-metadata-and-generic-jvm-length-are-not-safe-test
  (doseq [[label body]
          [["canonicalize"
            "(ns example.core (:require [babashka.fs :as fs]))\n(defn bad [] (fs/canonicalize \"x\"))"]
           ["length" "(defn bad [] (.length (java.io.File. \"x\")))"]]]
    (let [root (analyzer-repo body)]
      (is (contains? #{:forbidden-evidence-capability :forbidden-evidence-io}
                     (problem-kind #(runtime/analyze-reachable-vars
                                     root ['example.core/bad])))
          label))))

(deftest trusted-adapter-inventory-names-only-resolvable-vars-test
  (let [inventory (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                       'trusted-adapter-vars))]
    (is (every? (fn [qualified]
                  (some-> (find-ns (symbol (namespace qualified)))
                          (ns-resolve (symbol (name qualified)))))
                inventory))
    (is (set/subset?
         '#{abc.tools.files/read-json-lines
            abc.tools.files/with-zip-file
            abc.tools.files/load-jena-model
            abc.tools.files/parse-xml-document}
         inventory))
    (is (empty?
         (set/intersection
          '#{abc.tools.aozora-csv/parse-date
             abc.tools.aozora-ingest/run-from-rows!
             abc.tools.aozora-ingest/write-person-file!
             abc.tools.malli/cached-schema-hash
             abc.tools.person-drift/event->graph
             abc.tools.person-drift/validate!
             abc.tools.shacl/validate!
             abc.tools.aozora-history-audit/drift-participant-updates}
          inventory)))))

(deftest strict-analysis-does-not-trust-legacy-foundation-terminals-test
  (let [root (temp-dir)]
    (write! root "src/abc/sim/content_sim_test.clj"
            (str "(ns abc.sim.content-sim-test\n"
                 "  (:require [clojure.java.io :as io]))\n"
                 "(defn overwrite-zip! [path] (io/output-stream path))\n"
                 "(defn contract [path] (overwrite-zip! path))\n"))
    (write! root "test/.keep" "")
    (is (= :forbidden-evidence-capability
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['abc.sim.content-sim-test/contract]))))
    (is (= :invalid-foundation-conformance-profile
           (problem-kind #(runtime/analyze-foundation-reachable-vars
                           root ['abc.sim.content-sim-test/contract]))))))

(deftest restored-public-apis-have-only-their-original-fixed-arities-test
  (is (= '([] [path]) (:arglists (meta #'shacl/load-shapes-graph))))
  (is (= '([manifest] [manifest opts])
         (:arglists (meta #'manifest-to-rdf/manifest->ttl))))
  (is (= '([] [options])
         (:arglists (meta #'validate-design-bundle/validate-canonicalization!)))))

(deftest static-code-loading-bridge-rejects-any-additional-target-test
  (let [audit! (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                    'audit-static-code-loading-edge!))
        find-defn (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                       'defn-form))
        forms (runtime/read-source-forms! (files/path "src" "abc/tools/malli.clj"))
        original (find-defn forms 'cached-schema-hash)
        mutations ['(slurp "secret")
                   '(java.nio.file.Files/readAllBytes (java.nio.file.Path/of "secret" (make-array String 0)))
                   '(java.nio.file.Files/newInputStream (java.nio.file.Path/of "secret" (make-array String 0)))
                   '(java.nio.file.Files/readString (java.nio.file.Path/of "secret" (make-array String 0)))
                   '(java.nio.file.Files/newBufferedReader (java.nio.file.Path/of "secret" (make-array String 0)))
                   '(java.nio.file.Files/list (java.nio.file.Path/of "." (make-array String 0)))
                   '(clojure.java.io/input-stream "secret")
                   '(clojure.java.io/reader "secret")
                   '(java.io.FileReader. "secret")
                   '(java.io.FileInputStream. "secret")
                   '(java.util.zip.ZipFile. "secret.zip")
                   '(.listFiles (java.io.File. "."))
                   '(babashka.fs/list-dir ".")
                   '(babashka.fs/directory? ".")
                   '(babashka.fs/regular-file? "secret")
                   '(babashka.fs/glob "." "**/*")
                   '(org.apache.jena.riot.RDFDataMgr/loadModel "secret")
                   '(org.apache.jena.riot.RDFDataMgr/read nil "secret")
                   '(org.apache.jena.riot.RDFDataMgr/loadGraph "secret")
                   '(org.apache.jena.riot.RDFDataMgr/readDataset "secret")
                   '(java.lang.ProcessBuilder. ["true"])
                   '(.openStream (java.net.URL. "https://example.test"))
                   '(requiring-resolve 'example.injected/run)]]
    (is (= 'abc.tools.manifest/schema-hash
           (audit! 'abc.tools.malli/cached-schema-hash original)))
    (doseq [mutation mutations]
      (is (= :forbidden-evidence-capability
             (problem-kind
              #(audit! 'abc.tools.malli/cached-schema-hash
                       (concat original [mutation]))))))))

(deftest every-trusted-leaf-rejects-injected-capabilities-test
  (let [adapters (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                      'trusted-adapter-vars))
        structural-leaves
        (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                             'audited-structural-leaf-vars))
        inventory (set/union adapters structural-leaves)
        operation-inventories
        (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                             'trusted-adapter-operation-counts))
        audit! (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                    'audit-trusted-adapter-form!))
        find-defn (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                       'defn-form))
        mutations {'slurp '(slurp "secret")
                   'files-read-all-bytes
                   '(java.nio.file.Files/readAllBytes
                     (java.nio.file.Path/of "secret" (make-array String 0)))
                   'files-new-input-stream
                   '(java.nio.file.Files/newInputStream
                     (java.nio.file.Path/of "secret" (make-array String 0)))
                   'files-read-string
                   '(java.nio.file.Files/readString
                     (java.nio.file.Path/of "secret" (make-array String 0)))
                   'files-new-buffered-reader
                   '(java.nio.file.Files/newBufferedReader
                     (java.nio.file.Path/of "secret" (make-array String 0)))
                   'files-list
                   '(java.nio.file.Files/list
                     (java.nio.file.Path/of "." (make-array String 0)))
                   'io-input-stream '(clojure.java.io/input-stream "secret")
                   'io-reader '(clojure.java.io/reader "secret")
                   'file-reader '(java.io.FileReader. "secret")
                   'file-input-stream '(java.io.FileInputStream. "secret")
                   'zip-file '(java.util.zip.ZipFile. "secret.zip")
                   'file-listing '(.listFiles (java.io.File. "."))
                   'fs-list-dir '(babashka.fs/list-dir ".")
                   'fs-directory '(babashka.fs/directory? ".")
                   'fs-regular-file '(babashka.fs/regular-file? "secret")
                   'fs-glob '(babashka.fs/glob "." "**/*")
                   'jena-load-model '(org.apache.jena.riot.RDFDataMgr/loadModel "secret")
                   'jena-read '(org.apache.jena.riot.RDFDataMgr/read nil "secret")
                   'jena-load-graph '(org.apache.jena.riot.RDFDataMgr/loadGraph "secret")
                   'jena-read-dataset '(org.apache.jena.riot.RDFDataMgr/readDataset "secret")
                   'process '(java.lang.ProcessBuilder. ["true"])
                   'network '(.openStream (java.net.URL. "https://example.test"))
                   'code-loading '(requiring-resolve 'example.core/run)}]
    (is (= inventory (set (keys operation-inventories))))
    (doseq [leaf inventory
            :let [resolved (ns-resolve (symbol (namespace leaf))
                                       (symbol (name leaf)))
                  metadata (meta resolved)
                  source-file (some #(when (fs/exists? %) %)
                                    [(files/path "src" (:file metadata))
                                     (files/path "test" (:file metadata))])
                  forms (runtime/read-source-forms!
                         source-file)
                  original (find-defn forms (:name metadata))]]
      (is (true? (audit! leaf original))
          (str leaf " matches its exact operation inventory"))
      (doseq [[label mutation] mutations]
        (is (= :forbidden-evidence-capability
               (problem-kind #(audit! leaf (concat original [mutation]))))
            (str leaf " rejects injected " label))))))

(deftest trusted-loaders-require-an-immediately-nested-read-trace-test
  (let [audit! (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                    'audit-trusted-adapter-form!))]
    (is (= :forbidden-evidence-capability
           (problem-kind
            #(audit! 'abc.tools.files/read-bytes
                     '(defn read-bytes [file]
                        (do
                          (abc.tools.evidence-io/record-read! file)
                          (babashka.fs/read-all-bytes file)))))))
    (is (= :forbidden-evidence-capability
           (problem-kind
            #(audit! 'abc.tools.files/load-jena-model
                     '(defn load-jena-model [file]
                        (do
                          (abc.tools.evidence-io/record-read! file)
                          (org.apache.jena.riot.RDFDataMgr/loadModel
                           (str file))))))))
    (doseq [[adapter form]
            [['abc.tools.files/read-bytes
              '(defn read-bytes [file]
                 (babashka.fs/read-all-bytes
                  (do
                    (abc.tools.evidence-io/record-read! "unrelated-safe-path")
                    file)))]
             ['abc.tools.files/read-bytes
              '(defn read-bytes [file other-file]
                 (babashka.fs/read-all-bytes
                  (let [_ (abc.tools.evidence-io/record-read! file)]
                    other-file)))]
             ['abc.tools.files/read-bytes
              '(defn read-bytes [file]
                 (letfn [(record-read! [_] file)]
                   (babashka.fs/read-all-bytes
                    (record-read! "unrelated-safe-path"))))]
             ['abc.tools.files/load-jena-model
              '(defn load-jena-model [file traced-file]
                 (org.apache.jena.riot.RDFDataMgr/loadModel
                  (str
                   (do
                     (abc.tools.evidence-io/record-read! traced-file)
                     file))))]
             ['abc.tools.files/parse-xml-document
              '(defn parse-xml-document [file traced-file]
                 (let [factory
                       (javax.xml.parsers.DocumentBuilderFactory/newInstance)
                       _ (.newDocumentBuilder factory)]
                   (.setNamespaceAware factory true)
                   (.parse
                    (str (abc.tools.evidence-io/record-read! traced-file))
                    (clojure.java.io/file file))))]]]
      (is (= :forbidden-evidence-capability
             (problem-kind #(audit! adapter form)))))))

(deftest finite-fixed-multi-arity-definitions-are-traversed-test
  (let [root (analyzer-repo
              (str "(defn helper\n"
                   "  ([] true)\n"
                   "  ([path] (slurp path)))\n"
                   "(defn contract [] (helper))"))]
    (is (= :forbidden-evidence-io
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/contract]))))))

(deftest every-trusted-callback-position-is-explicit-and-fail-closed-test
  (let [signatures (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                        'audited-higher-order-signatures))
        inventory (var-get (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                                       'trusted-adapter-vars))
        expected '{abc.tools.adr-evidence-runtime-inputs/with-validated-read-trace! #{1}
                   abc.tools.evidence-io/with-read-trace #{1}
                   abc.tools.evidence-io/with-ephemeral-root #{1}
                   abc.tools.evidence-io/with-owned-ephemeral-root #{0}
                   abc.tools.files/with-zip-file #{1}}]
    (is (= expected (select-keys signatures (keys expected))))
    (is (= (set (keys expected))
           (set/intersection inventory (set (keys signatures)))))
    (doseq [[label call]
            [["deep owner" "(runtime/with-validated-read-trace! {} (first [slurp]))"]
             ["read trace" "(evidence-io/with-read-trace {} (first [slurp]))"]
             ["ephemeral root" "(evidence-io/with-ephemeral-root \"tmp\" (first [slurp]))"]
             ["owned ephemeral root" "(evidence-io/with-owned-ephemeral-root (first [slurp]))"]
             ["zip callback" "(files/with-zip-file \"fixture.zip\" (first [slurp]))"]
             ["hidden process" "(evidence-io/with-ephemeral-root \"tmp\" (get {:run process/process} :run))"]]]
      (let [root (boundary-analyzer-repo
                  (str "(ns example.core (:require "
                       "[abc.tools.adr-evidence-runtime-inputs :as runtime] "
                       "[abc.tools.evidence-io :as evidence-io] "
                       "[abc.tools.files :as files] "
                       "[babashka.process :as process]))\n"
                       "(defn contract [] " call ")"))]
        (is (= :unsupported-evidence-call-graph
               (problem-kind #(runtime/analyze-reachable-vars
                               root ['example.core/contract])))
            label)))
    (let [root (boundary-analyzer-repo
                (str "(ns example.core (:require [abc.tools.evidence-io :as evidence-io]))\n"
                     "(defn contract [] "
                     "(evidence-io/with-ephemeral-root \"tmp\" (fn [] true)))"))]
      (is (= ['abc.tools.evidence-io/with-ephemeral-root 'example.core/contract]
             (:reachable-vars
              (runtime/analyze-reachable-vars root ['example.core/contract])))))
    (let [root (boundary-analyzer-repo
                (str "(ns example.core (:require [abc.tools.evidence-io :as evidence-io]))\n"
                     "(defn contract [] "
                     "(evidence-io/with-owned-ephemeral-root (fn [_] true)))"))]
      (is (= ['abc.tools.evidence-io/with-owned-ephemeral-root 'example.core/contract]
             (:reachable-vars
              (runtime/analyze-reachable-vars root ['example.core/contract])))))))

(deftest v2-focused-boundary-must-use-the-deep-validation-owner-test
  (let [missing-trace (boundary-analyzer-repo
                       "(ns example.core (:require [abc.tools.adr-evidence-runtime-inputs :as runtime]))\n(defn contract [] (runtime/assert-runtime-input-closure! {}))")
        missing-closure (boundary-analyzer-repo
                         "(ns example.core (:require [abc.tools.evidence-io :as evidence-io]))\n(defn contract [] (evidence-io/with-read-trace {} (fn [] true)))")
        disconnected (boundary-analyzer-repo
                      (str "(ns example.core (:require [abc.tools.evidence-io :as evidence-io] "
                           "[abc.tools.adr-evidence-runtime-inputs :as runtime]))\n"
                           "(defn contract []\n"
                           "  (evidence-io/with-read-trace {} (fn [] true))\n"
                           "  (runtime/assert-runtime-input-closure! {}))"))]
    (is (= :missing-evidence-boundary-owner
           (problem-kind #(runtime/assert-v2-boundary-ownership!
                           (runtime/analyze-reachable-vars missing-trace ['example.core/contract])))))
    (is (= :missing-evidence-boundary-owner
           (problem-kind #(runtime/assert-v2-boundary-ownership!
                           (runtime/analyze-reachable-vars missing-closure ['example.core/contract])))))
    (is (= :missing-evidence-boundary-owner
           (problem-kind #(runtime/assert-v2-boundary-ownership!
                           (runtime/analyze-reachable-vars disconnected ['example.core/contract])))))))

(deftest v2-focus-must-resolve-to-a-deftest-source-form-test
  (let [body (fn [head]
               (boundary-analyzer-repo
                (str "(ns example.core (:require "
                     "[abc.tools.adr-evidence-runtime-inputs :as runtime] "
                     "[clojure.test :refer [deftest]]))\n"
                     "(" head " contract " (if (= head "deftest") "" "[] ")
                     "(runtime/with-validated-read-trace! {} (fn [] true)))")))
        validate! #(runtime/validate-focused-deftests! % ['example.core/contract])]
    (is (true? (validate! (body "deftest"))))
    (doseq [head ["defn" "defn-"]]
      (is (= :focused-var-not-deftest
             (problem-kind #(validate! (body head))))
          head))
    (is (= :focused-var-not-deftest
           (problem-kind #(runtime/validate-focused-deftests!
                           "." ['abc.tools.files/bytes->hex]))))))

(deftest v2-owner-must-be-an-unconditional-direct-focused-body-expression-test
  (let [direct (boundary-analyzer-repo
                (str "(ns example.core (:require "
                     "[abc.tools.adr-evidence-runtime-inputs :as runtime]))\n"
                     "(defn contract []\n"
                     "  (runtime/with-validated-read-trace! {} (fn [] true)))"))
        nested-forms
        ["(when false (runtime/with-validated-read-trace! {} (fn [] true)))"
         "(if false (runtime/with-validated-read-trace! {} (fn [] true)) true)"
         "(is (runtime/with-validated-read-trace! {} (fn [] true)))"]]
    (is (= ['example.core/contract]
           (:focused-vars
            (runtime/assert-v2-boundary-ownership!
             (runtime/analyze-reachable-vars direct ['example.core/contract])))))
    (doseq [nested nested-forms]
      (let [root (boundary-analyzer-repo
                  (str "(ns example.core (:require "
                       "[abc.tools.adr-evidence-runtime-inputs :as runtime] "
                       "[clojure.test :refer [is]]))\n"
                       "(defn contract [] " nested ")"))]
        (is (= :missing-evidence-boundary-owner
               (problem-kind
                #(runtime/assert-v2-boundary-ownership!
                  (runtime/analyze-reachable-vars root ['example.core/contract])))))))
    (let [mixed (boundary-analyzer-repo
                 (str "(ns example.core (:require "
                      "[abc.tools.adr-evidence-runtime-inputs :as runtime]))\n"
                      "(defn direct []\n"
                      "  (runtime/with-validated-read-trace! {} (fn [] true)))\n"
                      "(defn hidden []\n"
                      "  (when false "
                      "(runtime/with-validated-read-trace! {} (fn [] true))))"))]
      (is (= :missing-evidence-boundary-owner
             (problem-kind
              #(runtime/assert-v2-boundary-ownership!
                (runtime/analyze-reachable-vars
                 mixed ['example.core/direct 'example.core/hidden]))))))))

(deftest statically-resolved-higher-order-local-var-arguments-expand-test
  (let [root (analyzer-repo
              "(defn target [x] x)\n(defn bad [] (map target [1]))")]
    (is (= ['example.core/bad 'example.core/target]
           (:reachable-vars (runtime/analyze-reachable-vars
                             root ['example.core/bad]))))))

(deftest raw-filesystem-state-predicates-are-not-safe-jvm-operations-test
  (doseq [body ["(defn bad [] (.exists (java.io.File. \"x\")))"
                "(defn bad [] (.isFile (java.io.File. \"x\")))"
                "(defn bad [] (.isDirectory (java.io.File. \"x\")))"]]
    (let [root (analyzer-repo body)]
      (is (= :forbidden-evidence-io
             (problem-kind #(runtime/analyze-reachable-vars root ['example.core/bad])))))))

(deftest clojure-test-is-is-the-only-explicit-noncore-safe-macro-test
  (let [safe (analyzer-repo
              "(require '[clojure.test :refer [is]])\n(defn contract [] (is (= 1 1)))")
        unsafe (analyzer-repo
                "(defmacro unchecked [& body] `(do ~@body))\n(defn contract [] (unchecked true))")]
    (is (= ['example.core/contract]
           (:reachable-vars (runtime/analyze-reachable-vars safe ['example.core/contract]))))
    (is (= :unsupported-evidence-call-graph
           (problem-kind #(runtime/analyze-reachable-vars unsafe ['example.core/contract]))))))

(deftest higher-order-contract-shape-and-targets-fail-closed-test
  (let [root (temp-dir)
        source "(ns abc.tools.manifest)\n(defn target [x] x)\n(defn content [f] (f :x))\n"
        path "data/evidence-higher-order-calls/manifest-content.edn"
        valid {:schema-version :abc-evidence-higher-order-call-v1
               :caller 'abc.tools.manifest/content
               :parameters {'f ['abc.tools.manifest/target]}}]
    (write! root "src/abc/tools/manifest.clj" source)
    (write! root "test/.keep" "")
    (is (= :invalid-higher-order-contract
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['abc.tools.manifest/content]))))
    (doseq [[label value expected]
            [["wrong caller" (assoc valid :caller 'abc.tools.manifest/other)
              :invalid-higher-order-contract]
             ["missing target" (assoc-in valid [:parameters 'f]
                                         ['abc.tools.manifest/absent])
              :unresolved-focused-var]
             ["unknown parameter" (assoc valid :parameters {'other ['abc.tools.manifest/target]})
              :invalid-higher-order-contract]]]
      (write! root path (pr-str value))
      (is (= expected
             (problem-kind #(runtime/analyze-reachable-vars
                             root ['abc.tools.manifest/content])))
          label))
    (write! root path (pr-str valid))
    (is (= ['abc.tools.manifest/content 'abc.tools.manifest/target]
           (:reachable-vars
            (runtime/analyze-reachable-vars root ['abc.tools.manifest/content]))))))

(deftest duplicate-focused-definition-fails-closed-test
  (let [root (analyzer-repo "(defn duplicate [] true)\n(defn duplicate [] false)")]
    (is (= :duplicate-var-definition
           (problem-kind #(runtime/analyze-reachable-vars
                           root ['example.core/duplicate]))))))

(deftest retired-nix-clojure-closure-is-not-an-accepted-protocol-test
  (is (nil? (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                        (symbol (str "derive-nix-" "clojure-source-closure")))))
  (is (nil? (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                        (symbol (str "validate-nix-" "clojure-closure!"))))))
