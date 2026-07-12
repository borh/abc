# Production babashka.process Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace all production `clojure.java.shell` and raw `ProcessBuilder` execution in ABC with `babashka.process` 0.6.25 without changing site-specific contracts.

**Architecture:** Each namespace uses `sh` for captured non-throwing results, `shell` for inherited I/O, or `process` for explicit streams and environment control. Existing result shapes, injection seams, exception data, working directories, encodings, and broken-pipe behavior remain locally owned; no common wrapper is introduced.

**Tech Stack:** Clojure 1.12.5, babashka.process 0.6.25, clojure.test/Kaocha, clj-kondo, clj-nix.

## Global Constraints

- Preserve behavior except for intentional concurrent draining of publication stdout/stderr.
- Commands use argument vectors; argv[0] is a space/quote-free token or existing path.
- Preserve parent environments with `:extra-env`, never `:env`.
- Preserve every result map and explicitly remap babashka keys where required.
- Production must contain no `clojure.java.shell`, `ProcessBuilder`, or `Runtime.exec` execution.
- `Runtime/getRuntime` used for `availableProcessors` and test fixture `ProcessBuilder` uses remain valid.
- Filesystem refactoring and a common process wrapper are out of scope.

---

### Task 1: Captured non-throwing commands

**Files:**
- Modify: `abc/src/abc/tools/workflow/nix_bridge.clj`
- Modify: `abc/src/abc/tools/aozora_replay.clj`
- Modify: `abc/src/abc/tools/source_bundle_report.clj`
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj` (`git-sh` only)
- Test: `abc/test/abc/tools/workflow/nix_bridge_test.clj`
- Test: `abc/test/abc/tools/aozora_replay_test.clj`
- Test: `abc/test/abc/tools/source_bundle_report_test.clj`
- Test: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `babashka.process/sh` returning captured `{:exit :out :err}` without throwing on nonzero.
- Produces: unchanged runner, Git, provenance, and 7-Zip contracts.

- [ ] **Step 1: Add characterization tests**

Pin captured stdout/stderr/nonzero behavior in `nix_bridge_test.clj`:

```clojure
(deftest default-runner-captures-nonzero-result-test
  (let [{:keys [exit out err]}
        (nix-bridge/default-runner
         ["sh" "-c" "printf stdout; printf stderr >&2; exit 7"])]
    (is (= 7 exit))
    (is (= "stdout" out))
    (is (= "stderr" err))))
```

In `aozora_replay_test.clj`, resolve private `git*`, initialize a temporary Git repository, assert `:dir` is honored, and assert a failing `rev-parse` returns positive `:exit` instead of throwing. Use the existing fake 7-Zip fixture to assert exit 0 → true and exit 1 → false. Retain the existing Soranoha test that unavailable Git provenance does not fail publication.

- [ ] **Step 2: Run characterization tests**

```bash
cd abc
bin/kaocha --focus abc.tools.workflow.nix-bridge-test \
  --focus abc.tools.aozora-replay-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.soranoha-test
```

Expected: PASS before production changes.

- [ ] **Step 3: Replace shell calls with `process/sh`**

Replace `clojure.java.shell` requires with `[babashka.process :as process]` and use:

```clojure
(defn default-runner [args]
  (process/sh args))

(defn- git* [args {:keys [dir out-enc]}]
  (process/sh (into ["git"] (map str args))
              (cond-> {:out :string :err :string}
                dir (assoc :dir (str dir))
                out-enc (assoc :out-enc out-enc))))

(defn- sevenzip-listable? [zip-file]
  (let [binary (or (System/getenv "ABC_7ZZ_BIN") "7zz")]
    (zero? (:exit (process/sh [binary "l" "-slt" (str zip-file)])))))

(defn- git-sh [aozora-root & args]
  (try
    (let [{:keys [exit out]}
          (process/sh (into ["git" "-C" (str aozora-root)] args))]
      (when (zero? exit) (string/trim out)))
    (catch java.io.IOException _ nil)))
```

Use the 0.6.25 vector-first/options-last arity shown above; never join argv into
a command string. Bare vectors remain valid when no options map is needed.

- [ ] **Step 4: Verify and commit**

```bash
bin/kaocha --focus abc.tools.workflow.nix-bridge-test \
  --focus abc.tools.aozora-replay-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.soranoha-test
rg -n "clojure.java.shell" src/abc/tools/workflow/nix_bridge.clj \
  src/abc/tools/aozora_replay.clj src/abc/tools/source_bundle_report.clj \
  src/abc/tools/soranoha_build_publication.clj
git add src test
git commit -m "refactor(abc): use babashka process for captured commands"
```

Expected: tests PASS and `rg` has no matches in these files.

### Task 2: Working-directory capture and inherited I/O

**Files:**
- Modify: `abc/src/abc/tools/adr_evidence_capture.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Test: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Test: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces: unchanged evidence `{:exit-code :stdout :stderr}` and validation `ExceptionInfo` with `:command`/`:exit-code`.

- [ ] **Step 1: Characterize both contracts**

Resolve private `run-process` and execute `sh -c` in a temporary directory, asserting exact keys, cwd stdout, stderr, and nonzero status:

```clojure
(let [result (@run-process dir
                           ["sh" "-c"
                            "printf %s \"$PWD\"; printf err >&2; exit 4"])]
  (is (= #{:exit-code :stdout :stderr} (set (keys result))))
  (is (= 4 (:exit-code result)))
  (is (= (.getCanonicalPath dir) (:stdout result)))
  (is (= "err" (:stderr result))))
```

Add a `run-command!` test whose executable fixture exits 9 and assert the thrown `ExceptionInfo` retains `:command` and `:exit-code 9`.

- [ ] **Step 2: Run characterization tests**

```bash
bin/kaocha --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.validate-design-bundle-test
```

Expected: PASS before migration.

- [ ] **Step 3: Implement direct calls and key remapping**

```clojure
(defn- run-process [repo-root argv]
  (let [{:keys [exit out err]}
        @(process/process argv
                          {:dir (str repo-root) :out :string :err :string})]
    {:exit-code exit :stdout out :stderr err}))

(defn run-command! [& command]
  (try
    (process/shell (vec command) {:in :inherit :out :inherit :err :inherit})
    nil
    (catch clojure.lang.ExceptionInfo ex
      (throw (ex-info (str "Command failed: " (string/join " " command))
                      {:command command :exit-code (:exit (ex-data ex))}
                      ex)))))
```

Replace raw `ProcessBuilder` imports with `[babashka.process :as process]`.

- [ ] **Step 4: Verify and commit**

```bash
bin/kaocha --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.validate-design-bundle-test
git add src/abc/tools/adr_evidence_capture.clj \
  src/abc/tools/validate_design_bundle.clj \
  test/abc/tools/adr_evidence_capture_test.clj \
  test/abc/tools/validate_design_bundle_test.clj
git commit -m "refactor(abc): use babashka process for command execution"
```

Expected: both complete namespaces PASS.

### Task 3: Byte-array publication runner

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Test: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Produces: `run-process! [{:args :stdin-bytes :extra-env}] -> {:exit :out-bytes :err}`.

- [ ] **Step 1: Characterize bytes, environment, and nonzero status**

Resolve private `run-process!`; execute `sh -c 'cat; printf %s "$ABC_TEST_ENV" >&2; exit 6'` with ISO-8859-1 bytes containing NUL and `0xff`. Assert exit 6, byte-for-byte stdout, stderr `"kept"`, and exact keys `#{:exit :out-bytes :err}`.

- [ ] **Step 2: Run the new test before migration**

```bash
bin/kaocha --focus abc.tools.soranoha-test/publication-run-process-preserves-bytes-env-and-nonzero-test
```

Expected: PASS against current code.

- [ ] **Step 3: Replace ProcessBuilder and remap `:out`**

```clojure
(defn- run-process! [{:keys [args stdin-bytes extra-env]}]
  (let [{:keys [exit out err]}
        @(process/process args
                          {:in stdin-bytes :out :bytes :err :string
                           :extra-env extra-env})]
    {:exit exit :out-bytes out :err err}))
```

Dereference without `check`, preserving nonzero status as data and gaining concurrent stream draining.

- [ ] **Step 4: Verify and commit**

```bash
bin/kaocha --focus abc.tools.soranoha-test \
  --focus abc.sim.content-sim-test
git add src/abc/tools/soranoha_build_publication.clj \
  test/abc/tools/soranoha_test.clj
git commit -m "refactor(abc): run publication adapters with babashka process"
```

Expected: both suites PASS.

### Task 4: Streamed annotation runner

**Files:**
- Modify: `abc/src/abc/tools/annotation_join_stats_run.clj`
- Test: `abc/test/abc/tools/annotation_join_stats_run_test.clj`

**Interfaces:**
- Produces: `run-process! [{:cmd :env :stdin}] -> {:exit :out :err}` with broken-pipe tolerance.

- [ ] **Step 1: Characterize concurrent drain and early exit**

Using the existing executable-script fixture helper, add:

```clojure
(deftest run-process-drains-both-streams-test
  (let [run-process (ns-resolve 'abc.tools.annotation-join-stats-run 'run-process!)
        script (executable-script!
                "large-output"
                "i=0; while [ $i -lt 10000 ]; do printf o; printf e >&2; i=$((i+1)); done")
        {:keys [exit out err]} (@run-process {:cmd [script]})]
    (is (zero? exit))
    (is (= 10000 (count out)))
    (is (= 10000 (count err)))))

(deftest run-process-preserves-early-exit-diagnostics-test
  (let [run-process (ns-resolve 'abc.tools.annotation-join-stats-run 'run-process!)
        script (executable-script! "early-exit" "printf diagnosed >&2; exit 23")
        result (@run-process {:cmd [script]
                              :stdin (apply str (repeat 2000000 "x"))})]
    (is (= 23 (:exit result)))
    (is (= "diagnosed" (:err result)))))
```

- [ ] **Step 2: Run tests before migration**

```bash
bin/kaocha --focus abc.tools.annotation-join-stats-run-test/run-process-drains-both-streams-test \
  --focus abc.tools.annotation-join-stats-run-test/run-process-preserves-early-exit-diagnostics-test
```

Expected: PASS without hanging.

- [ ] **Step 3: Use `process` with manual stdin write**

```clojure
(defn- run-process! [{:keys [cmd env stdin]}]
  (let [proc (process/process cmd {:extra-env env :out :string :err :string})]
    (try
      (with-open [w (io/writer (:in proc) :encoding "UTF-8")]
        (when stdin (.write w ^String stdin)))
      (catch java.io.IOException _))
    (let [{:keys [exit out err]} @proc]
      {:exit exit :out out :err err})))
```

Do not pass stdin through `:in`; the explicit caught write is load-bearing.

- [ ] **Step 4: Verify and commit**

```bash
bin/kaocha --focus abc.tools.annotation-join-stats-run-test
git add src/abc/tools/annotation_join_stats_run.clj \
  test/abc/tools/annotation_join_stats_run_test.clj
git commit -m "refactor(abc): stream annotation commands with babashka process"
```

Expected: complete suite PASS.

### Task 5: Enforce production boundary and verify

**Files:**
- Modify: `abc/test/abc/tools/source_assertion_test.clj`

**Interfaces:**
- Produces: precise source invariant for production subprocess APIs.

- [ ] **Step 1: Add the production source assertion**

Add `[clojure.string :as string]` to the test namespace requires, then add:

```clojure
(deftest production-subprocesses-use-babashka-process-test
  (let [sources (->> (file-seq (io/file "src"))
                     (filter #(.isFile ^java.io.File %))
                     (filter #(re-find #"\.cljc?$" (.getName ^java.io.File %))))]
    (doseq [source sources :let [text (slurp source)]]
      (is (not (string/includes? text "clojure.java.shell")) (str source))
      (is (not (string/includes? text "ProcessBuilder")) (str source))
      (is (not (re-find #"Runtime/getRuntime[^)]*\)\s*\.exec" text))
          (str source)))))
```

Do not reject `Runtime/getRuntime`/`availableProcessors` or test fixtures.

- [ ] **Step 2: Run the source and focused acceptance gates**

```bash
bin/kaocha --focus abc.tools.source-assertion-test \
  --focus abc.tools.workflow.nix-bridge-test \
  --focus abc.tools.aozora-replay-test \
  --focus abc.tools.source-bundle-report-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.validate-design-bundle-test \
  --focus abc.tools.soranoha-test \
  --focus abc.tools.annotation-join-stats-run-test
rg -n "clojure.java.shell|ProcessBuilder|Runtime/.+exec" src --glob '*.clj'
```

Expected: all tests PASS; `rg` finds no production process execution.

- [ ] **Step 3: Run repository quality checks**

From the monorepo root:

```bash
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: PASS. These plus Task 5 Step 2 are the acceptance evidence.

- [ ] **Step 4: Run the unrelated regression gate**

```bash
just validate-migration
```

Expected: PASS; report it as repository regression evidence, not subprocess acceptance evidence.

- [ ] **Step 5: Commit the invariant**

```bash
git diff --check
git add test/abc/tools/source_assertion_test.clj
git commit -m "test(abc): enforce babashka process boundary"
```
