# Parallel build-publication Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cut full-corpus `soranoha build-publication` wall time by parallelizing both sequential per-work loops behind a `--concurrency` knob and removing per-work re-parse/re-hash waste, with byte-identical outputs.

**Architecture:** A new `abc.tools.parallel/ordered-pmap` (fixed thread pool, `invokeAll`, ordered results, binding conveyance, `ExecutionException` unwrapping) replaces the two `mapv` hot loops in `abc.tools.soranoha-build-publication` and the inline executor in `abc.tools.materialize-publication`. Supporting fixes make parallel execution safe and cheaper: atomic `write-deterministic-json-file!` (person-record write race), a Jing RelaxNG schema cache in `abc.tools.tei`, and a static schema-file hash cache in `abc.tools.materialize-publication`.

**Tech Stack:** Clojure (plain JVM, JDK 21), `java.util.concurrent` only — no new dependencies. Kaocha for tests.

**Spec:** `abc/docs/superpowers/specs/2026-07-12-parallel-build-publication-design.md`

## Global Constraints

- No new dependencies; `deps.edn` unchanged.
- Outputs must be byte-identical between `--concurrency 1` and `--concurrency N` runs, except: `build-plan.json` (gains a `"concurrency"` key whose value differs by run), and `workflow-run.json` / `workflow-plan.json` (already contain wall-clock timestamps).
- All commands below run from `/home/bor/Projects/soranoha/abc` unless a path says otherwise.
- Test runner: `bin/kaocha --focus <test-ns-or-id>` (wraps `clojure -M:test:kaocha -m kaocha.runner`). Full unit suite: `bin/kaocha --focus :unit`. Simulation suite: `bin/kaocha --focus :simulation`.
- `abc.tools.tei-test` is env-gated on `TEI_SCHEMA_PATH` and will refuse to run without it — new TEI tests go in a new namespace using the repo-local `schemas/tei-profile.rng` instead.
- Commit message style: `type(abc): summary` (see `git log --oneline`).
- Kaocha unit tests are namespaces matching `*-test` (but not `*-sim-test`), under `abc/test/`.

---

### Task 1: `abc.tools.parallel/ordered-pmap`

**Files:**
- Create: `abc/src/abc/tools/parallel.clj`
- Test: `abc/test/abc/tools/parallel_test.clj`

**Interfaces:**
- Consumes: nothing (leaf namespace).
- Produces: `(abc.tools.parallel/ordered-pmap concurrency f coll)` → vector of `(f item)` in `coll` order. `concurrency` is a positive long; `<= 1` degrades to `(mapv f coll)`. Dynamic bindings at the call site convey to worker threads. If any `f` call throws, the original throwable (not `ExecutionException`) is rethrown after all tasks finish. Tasks 2 and 5 call this exact signature.

- [ ] **Step 1: Write the failing test**

Create `abc/test/abc/tools/parallel_test.clj`:

```clojure
(ns abc.tools.parallel-test
  (:require [abc.tools.parallel :as parallel]
            [clojure.test :refer [deftest is testing]]))

(def ^:dynamic *probe* :root)

(deftest ordered-pmap-preserves-input-order-test
  (testing "later items finish first, results still follow coll order"
    (let [coll (vec (range 8))
          result (parallel/ordered-pmap
                  4
                  (fn [i] (Thread/sleep (long (- 80 (* 10 i)))) (* i i))
                  coll)]
      (is (= (mapv #(* % %) coll) result))
      (is (vector? result)))))

(deftest ordered-pmap-concurrency-one-is-sequential-mapv-test
  (let [thread-names (parallel/ordered-pmap
                      1
                      (fn [_] (.getName (Thread/currentThread)))
                      (range 4))]
    (is (= (repeat 4 (.getName (Thread/currentThread)))
           (seq thread-names))
        "concurrency 1 must run on the calling thread, exactly like mapv")))

(deftest ordered-pmap-actually-runs-concurrently-test
  (testing "4 tasks of ~150ms on 4 threads finish well under 4x150ms"
    (let [start (System/nanoTime)
          _ (parallel/ordered-pmap 4 (fn [_] (Thread/sleep 150)) (range 4))
          elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
      (is (< elapsed-ms 450.0)
          (str "expected concurrent execution, took " elapsed-ms "ms")))))

(deftest ordered-pmap-unwraps-execution-exception-test
  (let [thrown (try
                 (parallel/ordered-pmap
                  2
                  (fn [i] (if (= 3 i) (throw (ex-info "boom" {:i i})) i))
                  (range 5))
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
    (is (some? thrown) "the original ExceptionInfo must propagate")
    (is (= "boom" (ex-message thrown)))
    (is (= {:i 3} (ex-data thrown))
        "ex-data must survive: callers dispatch on it (source-bundle-admission-error)")))

(deftest ordered-pmap-conveys-dynamic-bindings-test
  (testing "worker threads see the caller's bindings (tests rebind *derive-parser-ir!*)"
    (binding [*probe* :bound]
      (is (= [:bound :bound :bound]
             (parallel/ordered-pmap 3 (fn [_] *probe*) (range 3)))))))

(deftest ordered-pmap-empty-coll-test
  (is (= [] (parallel/ordered-pmap 4 inc []))))

(deftest ordered-pmap-shuts-down-its-executor-test
  (testing "repeated calls do not accumulate pool threads"
    (let [live-threads #(count (Thread/getAllStackTraces))
          baseline (live-threads)]
      (dotimes [_ 3]
        (parallel/ordered-pmap 4 identity (range 8)))
      (Thread/sleep 200) ; let terminated pool threads unwind
      (is (< (- (live-threads) baseline) 4)
          "3 runs x 4 threads must not leak (a leak would add ~12 threads)"))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `bin/kaocha --focus abc.tools.parallel-test`
Expected: FAIL — `Could not locate abc/tools/parallel...` (namespace does not exist).

- [ ] **Step 3: Write the implementation**

Create `abc/src/abc/tools/parallel.clj`:

```clojure
(ns abc.tools.parallel
  "Bounded, ordered parallel mapping for independent per-work corpus loops."
  (:import [java.util.concurrent ExecutionException Executors Future]))

(defn ordered-pmap
  "Like (mapv f coll), running f on a fixed pool of `concurrency` threads.
  Results preserve coll order. concurrency <= 1 is exactly (mapv f coll) on
  the calling thread. Dynamic bindings at the call site convey to worker
  threads (per-work loops are exercised in tests under a rebound
  *derive-parser-ir!*). When any f call throws, the original throwable
  propagates (ExecutionException unwrapped) after all tasks have completed;
  in-flight work is not cancelled."
  [concurrency f coll]
  (if (<= concurrency 1)
    (mapv f coll)
    (let [executor (Executors/newFixedThreadPool concurrency)]
      (try
        (->> (.invokeAll executor
                         ^java.util.Collection
                         (mapv (fn [item] (bound-fn* #(f item))) coll))
             (mapv (fn [^Future fut]
                     (try
                       (.get fut)
                       (catch ExecutionException e
                         (throw (or (.getCause e) e)))))))
        (finally
          (.shutdown executor))))))
```

(Clojure fns implement `java.util.concurrent.Callable`, so the `bound-fn*` thunks go straight into `invokeAll`.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.parallel-test`
Expected: PASS, 7 tests.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/parallel.clj test/abc/tools/parallel_test.clj
git commit -m "feat(abc): add ordered bounded parallel map helper"
```

---

### Task 2: Refactor `materialize-batch-jobs!` onto `ordered-pmap`

**Files:**
- Modify: `abc/src/abc/tools/materialize_publication.clj` (ns import block at line 19, and `materialize-batch-jobs!` around line 665)
- Test: existing `abc/test/abc/tools/materialize_publication_test.clj` (no new tests — behavior-preserving)

**Interfaces:**
- Consumes: `abc.tools.parallel/ordered-pmap` from Task 1.
- Produces: no interface change; `materialize-batch-jobs!` keeps its `[jobs concurrency]` signature and return shape.

- [ ] **Step 1: Replace the inline executor**

In `abc/src/abc/tools/materialize_publication.clj`, add `[abc.tools.parallel :as parallel]` to the `:require` block (alphabetical position, after `abc.tools.metadata-record`), and delete the now-unused import `[java.util.concurrent Callable Executors]`.

Replace the current definition:

```clojure
(defn- materialize-batch-jobs! [jobs concurrency]
  (if (= 1 concurrency)
    (mapv materialize-batch-job! jobs)
    (let [executor (Executors/newFixedThreadPool concurrency)]
      (try
        (mapv #(.get %)
              (.invokeAll executor
                          (mapv (fn [job]
                                  (reify Callable
                                    (call [_]
                                      (materialize-batch-job! job))))
                                jobs)))
        (finally
          (.shutdown executor))))))
```

with:

```clojure
(defn- materialize-batch-jobs! [jobs concurrency]
  (parallel/ordered-pmap concurrency materialize-batch-job! jobs))
```

(`materialize-batch-job!` catches `Throwable` internally, so the unwrapping added by `ordered-pmap` is unobservable here.)

- [ ] **Step 2: Run the existing tests**

Run: `bin/kaocha --focus abc.tools.materialize-publication-test`
Expected: PASS (same count as on main).

- [ ] **Step 3: Commit**

```bash
git add src/abc/tools/materialize_publication.clj
git commit -m "refactor(abc): reuse ordered-pmap for publication batch jobs"
```

---

### Task 3: Atomic `write-deterministic-json-file!`

**Files:**
- Modify: `abc/src/abc/tools/json.clj` (`write-deterministic-json-file!` at line 46, ns imports)
- Test: `abc/test/abc/tools/json_test.clj`

**Interfaces:**
- Consumes: nothing new.
- Produces: same signature `(write-deterministic-json-file! file value)` → `file`; new guarantee: readers never observe a partial file, concurrent same-content writers are harmless (last atomic move wins). Task 5's parallel derive loop relies on this for `persons/<person_id>.json` (works sharing an author, written via `abc.tools.aozora-ingest/write-person-file!` with `:overwrite true`).

- [ ] **Step 1: Write the failing test**

Append to `abc/test/abc/tools/json_test.clj` (ns alias is `abc-json`):

```clojure
(deftest write-deterministic-json-file-is-atomic-under-concurrency-test
  (let [dir (java.io.File/createTempFile "abc-json-atomic" "")
        _ (.delete dir)
        _ (.mkdirs dir)
        file (io/file dir "person.json")
        value-a {"person_id" "000879" "name" (apply str (repeat 5000 "あ"))}
        value-b {"person_id" "000879" "name" (apply str (repeat 5000 "い"))}
        expected #{(abc-json/write-deterministic-json-str value-a)
                   (abc-json/write-deterministic-json-str value-b)}
        stop (promise)
        writers (mapv (fn [value]
                        (future
                          (dotimes [_ 100]
                            (abc-json/write-deterministic-json-file! file value))))
                      [value-a value-b])
        reader (future
                 (loop [seen []]
                   (if (realized? stop)
                     seen
                     (recur (if (.exists file)
                              (conj seen (string/trimr (slurp file)))
                              seen)))))]
    (try
      (run! deref writers)
      (deliver stop true)
      (let [observed @reader]
        (is (seq observed) "reader must have observed at least one read")
        (is (every? expected observed)
            "every observed read must be one complete value, never a torn mix"))
      ;; no leftover temp files
      (is (= ["person.json"] (mapv #(.getName %) (.listFiles (io/file dir)))))
      ;; the final file is world-readable like a plain io/writer file
      (let [perms (java.nio.file.Files/getPosixFilePermissions
                   (.toPath file)
                   (make-array java.nio.file.LinkOption 0))]
        (is (contains? perms java.nio.file.attribute.PosixFilePermission/OTHERS_READ)))
      (finally
        (run! #(.delete %) (.listFiles (io/file dir)))
        (.delete dir)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.json-test`
Expected: FAIL — the torn-read assertion or the reader observing a partial string (the current implementation writes directly to the target with `io/writer`). Note: this test is probabilistic against the old code but deterministic-pass against the new code; if it happens to pass on the first try, re-run once to observe a failure before proceeding.

- [ ] **Step 3: Write the implementation**

In `abc/src/abc/tools/json.clj`, change the ns form to:

```clojure
(ns abc.tools.json
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.nio.file CopyOption Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))
```

and replace `write-deterministic-json-file!`:

```clojure
(defn write-deterministic-json-file!
  "Atomically write deterministic JSON: content lands via a sibling temp file
  + ATOMIC_MOVE, so concurrent readers never observe a partial file and
  concurrent same-content writers race harmlessly (last move wins). The
  parallel per-work corpus loops depend on this: works sharing an author
  write the same persons/<person_id>.json."
  [file value]
  (io/make-parents file)
  (let [target (.toPath (io/file file))
        dir (or (.getParent target) (.toPath (io/file ".")))
        tmp (Files/createTempFile dir
                                  (str "." (.getFileName target) ".")
                                  ".tmp"
                                  (make-array FileAttribute 0))]
    (try
      (with-open [writer (io/writer (.toFile tmp))]
        (.write writer (write-deterministic-json-str value))
        (.write writer "\n"))
      ;; createTempFile creates owner-only (600) files; keep the historical
      ;; umask-style world-readable artifact bits.
      (try
        (Files/setPosixFilePermissions
         tmp (PosixFilePermissions/fromString "rw-r--r--"))
        (catch UnsupportedOperationException _))
      (Files/move tmp target
                  (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE]))
      (catch Throwable t
        (Files/deleteIfExists tmp)
        (throw t))))
  file)
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.json-test`
Expected: PASS (all pre-existing json tests plus the new one — the trailing-whitespace and determinism tests double as regression cover for the write path).

- [ ] **Step 5: Run the ingest tests (person-file writer is the motivating caller)**

Run: `bin/kaocha --focus abc.tools.aozora-ingest-test`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/json.clj test/abc/tools/json_test.clj
git commit -m "fix(abc): make deterministic JSON writes atomic"
```

---

### Task 4: Jing RelaxNG schema cache in `abc.tools.tei`

**Files:**
- Modify: `abc/src/abc/tools/tei.clj` (whole namespace — ns docstring, imports, `validate!`)
- Create: `abc/test/abc/tools/tei_cache_test.clj`
- Reference (unchanged, pattern to mirror): `abc/src/abc/tools/schematron.clj:44-62`

**Interfaces:**
- Consumes: nothing new.
- Produces: `validate!` keeps its exact signature and return shape `{:label .. :violations [{:severity :line :column :message} ..]}`, but becomes safe for concurrent use and stops re-parsing the schema per call. Task 5's parallel publication loop calls it concurrently via `materialize-publication!`.

**Background for the implementer:** Jing's `ValidationDriver` is a convenience wrapper that re-loads the schema every time and is not thread-safe. The underlying API: `com.thaiopensource.validate.auto.AutoSchemaReader` `.createSchema` → `com.thaiopensource.validate.Schema` (immutable, safe for concurrent use), then `.createValidator` per call → content handler fed by a namespace-aware SAX parse. Validation findings arrive through the `ERROR_HANDLER` property; XML parse errors arrive through the `XMLReader`'s error handler — set the same collecting handler in both places. A fatal parse error (malformed XML) is recorded by the handler and then the SAX parser throws `SAXParseException`; the old `ValidationDriver.validate` path propagated it, so do not catch it.

- [ ] **Step 1: Write the failing test**

Create `abc/test/abc/tools/tei_cache_test.clj` (deliberately a separate namespace from the `TEI_SCHEMA_PATH`-gated `abc.tools.tei-test`; uses the repo-local publication profile schema):

```clojure
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.tei-cache-test`
Expected: FAIL — `validate-reuses-cached-schema-test` cannot resolve `#'tei/schema-cache` (var does not exist yet).

- [ ] **Step 3: Rewrite `abc/src/abc/tools/tei.clj`**

Replace the whole file with:

```clojure
(ns abc.tools.tei
  "Wrap Jing for TEI RelaxNG validation. validate! parses the XML against a
  cached parse of the schema (keyed by canonical path + mtime, mirroring
  abc.tools.schematron's XSLT cache) and returns structured per-file
  violations. Safe for concurrent use: the cached
  com.thaiopensource.validate.Schema is immutable; a fresh Validator and
  error handler are created per call."
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [com.thaiopensource.util PropertyMapBuilder]
           [com.thaiopensource.validate Schema ValidateProperty]
           [com.thaiopensource.validate.auto AutoSchemaReader]
           [javax.xml.parsers SAXParserFactory]
           [org.xml.sax InputSource SAXParseException]))

(defn- file->input-source ^InputSource [^String path]
  (InputSource. (.toString (.toURI (io/file path)))))

(defn- build-sax-error-handler [violations-atom]
  (reify org.xml.sax.ErrorHandler
    (^void warning [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :warning
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))
    (^void error [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :error
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))
    (^void fatalError [_ ^SAXParseException e]
      (swap! violations-atom conj
             {:severity :fatal
              :line (.getLineNumber e)
              :column (.getColumnNumber e)
              :message (.getMessage e)}))))

(defn- error-handler-props [handler]
  (let [builder (PropertyMapBuilder.)]
    (.put builder ValidateProperty/ERROR_HANDLER handler)
    (.toPropertyMap builder)))

;; Parsing the RelaxNG schema dominated per-call cost when every work in a
;; corpus run re-loaded it. Schema objects are immutable and safe for
;; concurrent use (Jing's documented contract), so cache by canonical path +
;; mtime; an edited schema busts the entry. Validators are NOT thread-safe
;; and are created per call.
(defonce ^:private schema-cache (atom {}))

(defn- load-schema ^Schema [^String schema-path]
  (let [file (io/file schema-path)
        cache-key [(.getCanonicalPath file) (.lastModified file)]]
    (or (get @schema-cache cache-key)
        (let [violations (atom [])
              schema (try
                       (.createSchema (AutoSchemaReader.)
                                      (file->input-source schema-path)
                                      (error-handler-props
                                       (build-sax-error-handler violations)))
                       (catch Exception e
                         (throw (ex-info (str "Failed to load TEI RelaxNG schema: "
                                              schema-path)
                                         {:schema-path schema-path
                                          :violations @violations}
                                         e))))]
          (swap! schema-cache assoc cache-key schema)
          schema))))

(defn validate!
  "Validate the XML at `xml-path` against the RelaxNG schema at
  `schema-path`. Returns {:label, :violations [{:severity, :line,
  :column, :message} ...]}. Does not throw on validation issues;
  severity classification preserved on each violation. Malformed XML is
  recorded as a :fatal violation and the parser's SAXParseException
  propagates (same contract as the previous ValidationDriver-based
  implementation)."
  [{:keys [^String schema-path ^String xml-path label]}]
  (when (string/blank? schema-path)
    (throw (ex-info "TEI RelaxNG schema path must be set."
                    {:error :missing-schema-path})))
  (when (string/blank? xml-path)
    (throw (ex-info "TEI XML path must be set."
                    {:error :missing-xml-path})))
  (let [schema (load-schema schema-path)
        violations (atom [])
        handler (build-sax-error-handler violations)
        validator (.createValidator schema (error-handler-props handler))
        factory (doto (SAXParserFactory/newInstance)
                  (.setNamespaceAware true))
        xml-reader (.getXMLReader (.newSAXParser factory))]
    (.setContentHandler xml-reader (.getContentHandler validator))
    (.setErrorHandler xml-reader handler)
    (.parse xml-reader (file->input-source xml-path))
    {:label label
     :violations @violations}))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.tei-cache-test`
Expected: PASS, 4 tests.

- [ ] **Step 5: Run the publication tests (behavior parity through the real caller)**

Run: `bin/kaocha --focus abc.tools.materialize-publication-test`
Expected: PASS — `tei-validation-result` still classifies the fixture publications exactly as before.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/tei.clj test/abc/tools/tei_cache_test.clj
git commit -m "perf(abc): cache parsed TEI RelaxNG schema across works"
```

---

### Task 5: `--concurrency` flag, parallel loops, build-plan record

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
  - ns `:require` block (line 2-17)
  - `cli-options` (line 24)
  - derive loop in `materialize-selected-sources!` (line 415)
  - `build-plan` (line 500)
  - publication loop in `materialize-publications!` (line 625)
  - `build-publication-steps` step maps (lines 650-712)
  - `build-publication!` (line 714)
- Test: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `abc.tools.parallel/ordered-pmap` (Task 1); atomic person writes (Task 3) make the derive loop's shared `persons/` writes safe; the tei schema cache (Task 4) makes the publication loop's validation safe.
- Produces: CLI flag `--concurrency N` (absent or `0` → all cores); `build-plan.json` gains key `"concurrency"` holding the resolved positive integer; both per-work loops run on `ordered-pmap`.

- [ ] **Step 1: Write the failing test**

Add to `abc/test/abc/tools/soranoha_test.clj` (after `build-publication-command-materializes-real-publications-test`, reusing that file's private helpers `official-aozora-fixture!`, `stub-derive-parser-ir!`, `build-publication-csv`, `write-zip!`, `delete-tree!`):

```clojure
(defn- two-work-aozora-fixture!
  "official-aozora-fixture! plus a second catalog-backed work so parallel
  ordering has something to scramble."
  [root]
  (official-aozora-fixture! root)
  (let [second-row (str "\"000002\",\"鼻\",\"はな\",\"はな\",\"\",\"\",\"\","
                        "\"\",\"NDC 913\",\"新字新仮名\",\"なし\",\"1997-10-29\","
                        "\"2022-07-16\",\"https://www.aozora.gr.jp/cards/000879/card2.html\","
                        "\"000879\",\"芥川\",\"竜之介\",\"あくたがわ\",\"りゅうのすけ\","
                        "\"あくたかわ\",\"りゆうのすけ\",\"Akutagawa\",\"Ryunosuke\","
                        "\"著者\",\"1892-03-01\",\"1927-07-24\",\"なし\","
                        "\"鼻\",\"テスト出版社\",\"\",\"\",\"\",\"\",\"\",\"\","
                        "\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"\",\"野口英司\",\"校正者\","
                        "\"https://www.aozora.gr.jp/cards/000879/files/000002_ruby_fixture.zip\","
                        "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\","
                        "\"https://www.aozora.gr.jp/cards/000879/files/000002_15261.html\","
                        "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"1\"\n")]
    (write-zip! (io/file root "index_pages" "list_person_all_extended_utf8.zip")
                {"list_person_all_extended_utf8.csv"
                 (str build-publication-csv second-row)})
    (write-zip! (io/file root "cards" "000879" "files"
                         "000002_ruby_fixture.zip")
                {"000002.txt" "第二の本文です。"}))
  root)

(defn- tree-file-hashes
  "relative-path -> sha256 for every file under root, excluding the run-varying
  records (concurrency in build-plan.json; timestamps in workflow-*.json)."
  [root]
  (let [root-file (io/file root)
        excluded #{"build-plan.json" "workflow-run.json" "workflow-plan.json"}]
    (->> (file-seq root-file)
         (filter #(.isFile ^java.io.File %))
         (remove #(excluded (.getName ^java.io.File %)))
         (map (fn [^java.io.File f]
                [(str (.relativize (.toPath root-file) (.toPath f)))
                 (files/sha256-file (str f))]))
         (into (sorted-map)))))

(deftest build-publication-concurrency-is-recorded-and-deterministic-test
  (let [root (fixture/temp-dir "abc-soranoha-build-concurrency")
        aozora-root (two-work-aozora-fixture! (io/file root "aozorabunko"))
        run! (fn [output-root concurrency-arg]
               (with-redefs [publication-policy/assert-release-allowed!
                             (constantly :ok)]
                 (binding [build-publication/*derive-parser-ir!*
                           stub-derive-parser-ir!]
                   (with-out-str
                     (is (zero? (soranoha/run!
                                 (cond-> ["build-publication"
                                          "--aozora-root" (str aozora-root)
                                          "--config" "abc/config/publication-basic-ja.json"
                                          "--snapshot-date" "2026-07-12"
                                          "--output-root" (str output-root)]
                                   concurrency-arg
                                   (into ["--concurrency" concurrency-arg])))))))))
        sequential-root (io/file root "out-sequential")
        parallel-root (io/file root "out-parallel")
        default-root (io/file root "out-default")]
    (try
      (run! sequential-root "1")
      (run! parallel-root "4")
      (run! default-root nil)
      (testing "resolved concurrency is recorded in build-plan.json"
        (is (= 1 (get (files/read-json (io/file sequential-root "build-plan.json"))
                      "concurrency")))
        (is (= 4 (get (files/read-json (io/file parallel-root "build-plan.json"))
                      "concurrency")))
        (is (= (.availableProcessors (Runtime/getRuntime))
               (get (files/read-json (io/file default-root "build-plan.json"))
                    "concurrency"))
            "absent flag must resolve to all cores"))
      (testing "parallel output is byte-identical to sequential output"
        (let [sequential-tree (tree-file-hashes sequential-root)
              parallel-tree (tree-file-hashes parallel-root)]
          (is (= 2 (get (files/read-json
                         (io/file sequential-root "publications"
                                  "publications-report.json"))
                        "publication_count")))
          (is (= (keys sequential-tree) (keys parallel-tree)))
          (is (= sequential-tree parallel-tree))))
      (finally
        (delete-tree! root)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.soranoha-test`
Expected: the new test FAILS — `--concurrency` is an unknown option (`cli/parse-opts` errors) and `build-plan.json` has no `"concurrency"` key. Pre-existing tests still pass.

- [ ] **Step 3: Implement flag, resolution, and both parallel loops**

In `abc/src/abc/tools/soranoha_build_publication.clj`:

a. Add `[abc.tools.parallel :as parallel]` to the `:require` block (after `abc.tools.materialize-publication` — mind the alphabetical order used in the file).

b. Append to `cli-options`:

```clojure
[nil "--concurrency N"
 "Worker threads for the per-work derive and publish loops (0 = all cores)."
 :id :concurrency :default 0 :parse-fn #(Long/parseLong %)
 :validate [#(>= % 0) "must be >= 0"]]
```

c. Add next to `parse-args`:

```clojure
(defn- resolve-concurrency
  "0 (or nil) means every available core; otherwise the requested count."
  [requested]
  (let [n (long (or requested 0))]
    (if (pos? n) n (.availableProcessors (Runtime/getRuntime)))))
```

d. In `build-publication!` (line 714), resolve once so the recorded value and both loops agree. Change:

```clojure
      (let [opts (assoc opts :output-root tmp-root)
```

to:

```clojure
      (let [opts (-> opts
                     (assoc :output-root tmp-root)
                     (update :concurrency resolve-concurrency))
```

e. Derive loop: in `materialize-selected-sources!`, add `concurrency` to the destructured keys (line 380-381), and replace `(mapv (fn [candidate] ...) selected-candidates)` (line 415-425) with `(parallel/ordered-pmap concurrency (fn [candidate] ...) selected-candidates)` — the `fn` body (the `continue-on-failure` try/catch around `derive-one`) is unchanged.

f. Publication loop: in `materialize-publications!`, add `concurrency` to the destructured keys (line 621-622), and replace `(mapv (fn [work] ...) (:selected materialization-result))` (line 625-632) with `(parallel/ordered-pmap concurrency (fn [work] ...) (:selected materialization-result))` — body unchanged.

g. Thread the value through the workflow steps in `build-publication-steps`:
   - Step `:materialize-source-selection`: destructure `opts` too, and pass `:concurrency (:concurrency opts)` into the `materialize-selected-sources!` argument map. Add `:opts` to that step's `:requires`.
   - Step `:materialize-publications`: destructure `opts`, pass `:concurrency (:concurrency opts)` into the `materialize-publications!` argument map. Add `:opts` to that step's `:requires`.

h. Record it in `build-plan` (line 500) — add one entry to the map:

```clojure
   "concurrency" (:concurrency opts)
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `bin/kaocha --focus abc.tools.soranoha-test`
Expected: PASS — including the two pre-existing `build-publication-*` tests, which now run both loops through `ordered-pmap` at all-cores concurrency under a rebound `*derive-parser-ir!*` (this is the binding-conveyance tripwire).

- [ ] **Step 5: Run the simulation suite (build simulations exercise the workflow contract)**

Run: `bin/kaocha --focus :simulation`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/soranoha_build_publication.clj test/abc/tools/soranoha_test.clj
git commit -m "perf(abc): parallelize build-publication per-work loops"
```

---

### Task 6: Static schema-file hash cache in `materialize-publication`

**Files:**
- Modify: `abc/src/abc/tools/materialize_publication.clj` (`profile-hash` at line 56, `file-hash` call sites at lines 149-150)
- Test: existing `abc/test/abc/tools/materialize_publication_test.clj` (parity gate; no new tests)

**Interfaces:**
- Consumes: nothing new.
- Produces: no interface change; identical hash strings, computed once per process instead of 5× per work.

**Rationale recorded from spec §6 inspection:** the parser-IR re-reads flagged in the spec each turned out load-bearing — `assert-parser-identities!` re-reads from disk as a serialization round-trip gate, and the cross-step re-read in `materialize-publication!` is required to keep memory bounded (holding 17.9k parsed parser-IRs across workflow steps is not acceptable). The actually-redundant per-work work is re-hashing the three static schema files: `profile-hash` (SHA-256 of `schemas/tei-profile.odd`) is called three times per work (lines 124, 470, 532) and `file-hash` is called on `schemas/tei-profile.rng` + `schemas/tei-profile.sch` once per work (lines 149-150) — ~90k redundant file hashes per full-corpus run.

- [ ] **Step 1: Implement the cache**

In `abc/src/abc/tools/materialize_publication.clj`, replace:

```clojure
(defn- profile-hash []
  (str "sha256:" (files/sha256-file tei-odd-path)))

(defn- file-hash [path]
  (str "sha256:" (files/sha256-file path)))
```

with:

```clojure
;; The TEI profile trio (odd/rng/sch) is fixed for a process lifetime but was
;; re-hashed for every work (profile-hash 3x per work). Cache by canonical
;; path + mtime, mirroring the schematron/tei schema caches.
(defonce ^:private static-file-hash-cache (atom {}))

(defn- static-file-hash [path]
  (let [file (io/file path)
        cache-key [(.getCanonicalPath file) (.lastModified file)]]
    (or (get @static-file-hash-cache cache-key)
        (let [hash (str "sha256:" (files/sha256-file path))]
          (swap! static-file-hash-cache assoc cache-key hash)
          hash))))

(defn- profile-hash []
  (static-file-hash tei-odd-path))

(defn- file-hash [path]
  (str "sha256:" (files/sha256-file path)))
```

Then change exactly two call sites inside `tei-validation-result` (lines 148-150) from `(file-hash tei-rng-path)` / `(file-hash tei-schematron-path)` to `(static-file-hash tei-rng-path)` / `(static-file-hash tei-schematron-path)`. All other `file-hash` calls hash per-work artifacts and must stay uncached.

- [ ] **Step 2: Run the tests**

Run: `bin/kaocha --focus abc.tools.materialize-publication-test`
Expected: PASS — hash values are unchanged, so manifests and validation results are byte-identical.

- [ ] **Step 3: Commit**

```bash
git add src/abc/tools/materialize_publication.clj
git commit -m "perf(abc): cache static TEI profile hashes across works"
```

---

### Task 7: Subset benchmark script, run, and handoff doc

**Files:**
- Create: `scripts/benchmark-build-publication.sh` (repo root `scripts/`, alongside the other operational shell scripts)
- Create: `abc/docs/handoffs/2026-07-12-parallel-build-publication-benchmark.md`

**Interfaces:**
- Consumes: the `--concurrency` flag from Task 5; the root flake app `nix run .#soranoha` (injects the Rust adapter binaries via env); the nix-pinned corpus input `aozorabunko-src`.
- Produces: a reusable benchmark harness and a handoff doc with measured medians — the spec's acceptance evidence.

- [ ] **Step 1: Write the benchmark script**

Create `scripts/benchmark-build-publication.sh`:

```bash
#!/usr/bin/env bash
# Benchmark soranoha build-publication sequential vs parallel on a corpus
# subset. Methodology matches docs/handoffs/2026-07-11-annotation-join-
# overlap-benchmark.md: one discarded warm-up per arm, then median of RUNS.
#
# Usage: scripts/benchmark-build-publication.sh [CARD_COUNT] [RUNS]
#   CARD_COUNT  number of cards/NNNNNN dirs in the subset (default 200)
#   RUNS        timed runs per arm (default 5)
#   CORPUS_ROOT env override for the aozorabunko checkout (default: the
#               nix-pinned aozorabunko-src flake input)
# Run from the repo root. Prints per-run wall times, medians, and runs the
# byte-identity diff between one sequential and one parallel output root.
set -euo pipefail

CARD_COUNT="${1:-200}"
RUNS="${2:-5}"
CORPUS_ROOT="${CORPUS_ROOT:-$(nix eval --raw --impure --expr \
  '(builtins.getFlake (toString ./.)).inputs.aozorabunko-src.outPath')}"
CONFIG="abc/config/full-corpus-publication-basic-ja.json"
SNAPSHOT_DATE="2026-07-12"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/bench-build-publication.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
SUBSET="$WORK/subset"
mkdir -p "$SUBSET/cards" "$SUBSET/index_pages"
ln -s "$CORPUS_ROOT/index_pages/list_person_all_extended_utf8.zip" \
  "$SUBSET/index_pages/"
ls "$CORPUS_ROOT/cards" | sort | head -n "$CARD_COUNT" | while read -r card; do
  ln -s "$CORPUS_ROOT/cards/$card" "$SUBSET/cards/$card"
done
echo "subset: $CARD_COUNT card dirs from $CORPUS_ROOT"

# Build the app once so timed runs exclude nix evaluation/build.
nix build .#soranoha --no-link

run_one() { # concurrency output-root -> wall seconds on stdout
  local conc="$1" out="$2"
  /usr/bin/time -f '%e' -o "$WORK/t" \
    nix run .#soranoha -- build-publication \
      --aozora-root "$SUBSET" \
      --config "$CONFIG" \
      --snapshot-date "$SNAPSHOT_DATE" \
      --output-root "$out" \
      --concurrency "$conc" >/dev/null
  cat "$WORK/t"
}

median() { sort -n | awk '{a[NR]=$1} END {print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2}'; }

declare -A MEDIANS
for conc in 1 0; do
  echo "--- concurrency=$conc warm-up (discarded)"
  run_one "$conc" "$WORK/warmup-c$conc" >/dev/null
  times=()
  for i in $(seq "$RUNS"); do
    t="$(run_one "$conc" "$WORK/out-c$conc-r$i")"
    times+=("$t")
    echo "concurrency=$conc run=$i wall=${t}s"
  done
  MEDIANS[$conc]="$(printf '%s\n' "${times[@]}" | median)"
done

echo "median wall: sequential=${MEDIANS[1]}s parallel=${MEDIANS[0]}s"
awk -v s="${MEDIANS[1]}" -v p="${MEDIANS[0]}" \
  'BEGIN {printf "speedup: %.2fx\n", s/p}'

echo "--- byte-identity check (excluding concurrency/timestamp records)"
diff -r \
  --exclude=build-plan.json \
  --exclude=workflow-run.json \
  --exclude=workflow-plan.json \
  "$WORK/out-c1-r1" "$WORK/out-c0-r1" \
  && echo "outputs byte-identical"
```

Then: `chmod +x scripts/benchmark-build-publication.sh`

- [ ] **Step 2: Run the benchmark**

Run from the repo root (`/home/bor/Projects/soranoha`):

```bash
scripts/benchmark-build-publication.sh 200 5 2>&1 | tee /tmp/bench-build-publication.log
```

Expected: per-run wall times for both arms, a `speedup: N.NNx` line, and `outputs byte-identical`. This takes tens of minutes (the sequential arm dominates). If any run fails, stop and debug before recording numbers — do not record a partial benchmark.

- [ ] **Step 3: Write the handoff doc**

Create `abc/docs/handoffs/2026-07-12-parallel-build-publication-benchmark.md` with the measured numbers (the values below are placeholders ONLY in this plan; the doc must contain the real measured output — copy from the tee'd log):

```markdown
# Parallel build-publication subset benchmark — 2026-07-12

- Spec: docs/superpowers/specs/2026-07-12-parallel-build-publication-design.md
- Harness: scripts/benchmark-build-publication.sh (repo root), CARD_COUNT=200,
  RUNS=5, one discarded warm-up per arm, median-of-5.
- Host: <hostname>, <N> cores (`nproc`), corpus = nix-pinned aozorabunko-src
  (0e9ea3e), works selected in subset: <from source-selection-report.json>.

## Results

| arm | resolved concurrency | wall times (s) | median (s) |
|-----|----------------------|----------------|------------|
| sequential (--concurrency 1) | 1 | <5 values> | <m> |
| parallel (--concurrency 0)   | <N> | <5 values> | <m> |

Speedup: <N.NN>x. Byte-identity diff (excluding build-plan.json,
workflow-run.json, workflow-plan.json): clean.

## Notes

- <anything observed: saturation point, subprocess spawn behavior, memory>
- Full-corpus validation run on hinoki is follow-up, not a gate (spec §Benchmark).
```

- [ ] **Step 4: Commit**

```bash
git add scripts/benchmark-build-publication.sh abc/docs/handoffs/2026-07-12-parallel-build-publication-benchmark.md
git commit -m "perf(abc): benchmark parallel build-publication on corpus subset"
```

---

### Task 8: Full-suite verification

**Files:** none (verification only).

- [ ] **Step 1: Full unit suite**

Run: `bin/kaocha --focus :unit`
Expected: PASS, zero failures.

- [ ] **Step 2: Simulation suite**

Run: `bin/kaocha --focus :simulation`
Expected: PASS.

- [ ] **Step 3: Lint**

Run: `bin/lint-active`
Expected: clean (matches main).

- [ ] **Step 4: Confirm working tree is fully committed**

Run: `git status --short` (expect only pre-existing unrelated modifications, if any) and `git log --oneline -8` (expect the six commits from Tasks 1-7).
