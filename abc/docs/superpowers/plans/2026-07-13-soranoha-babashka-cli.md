# Soranoha babashka.cli and Quiet Launcher Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace Soranoha's handwritten dispatcher with generated `babashka.cli` help and silence the shared `mkCljLauncher` JVM-option notice.

**Architecture:** An ordered `babashka.cli` command table owns command documentation, positional arguments, and `build-publication` options. A thin `run!` adapter normalizes legacy help aliases, converts library dispatch errors to Soranoha's status/stream contract, and leaves `System/exit` in `-main`; `mkCljLauncher` passes `user.home` directly to the JVM.

**Tech Stack:** Clojure 1.12.5, org.babashka/cli 0.12.75, clojure.test/Kaocha, clj-nix, Nix flakes.

## Global Constraints

- Preserve user worktree changes and do not reset unrelated files.
- `run!` returns status integers; only `-main` calls `System/exit`.
- Help goes to stdout with status 0; malformed input goes to stderr with status 2; invoked-command `ExceptionInfo` remains status 1.
- Empty args, `help`, `--help`, and `-h` show global help; both `help <command>` and `<command> --help`/`-h` show command help.
- The command table is the single source for command order, positional names, summaries, and `build-publication` options.
- Only `mkCljLauncher` changes; `presentationLauncher`, `cljSandboxEnv`, and presentation font-cache/drift `JAVA_TOOL_OPTIONS` uses remain.
- Do not change publication workflow behavior or migrate other ABC CLIs.

---

### Task 1: Pin babashka.cli and characterize the existing contract

**Files:**
- Modify: `abc/deps.edn`
- Modify: `abc/deps-lock.json` (generated)
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: existing `abc.tools.soranoha/run! [args] -> int`.
- Produces: direct `org.babashka/cli` dependency and characterization tests that constrain Tasks 2–3.

- [ ] **Step 1: Add characterization tests before changing the dispatcher**

Append tests that capture stdout and stderr separately and assert the current contracts that must survive migration:

```clojure
(deftest cli-status-and-stream-contract-test
  (testing "global help is successful stdout"
    (doseq [args [[] ["help"] ["--help"]]]
      (let [out (java.io.StringWriter.)
            err (java.io.StringWriter.)]
        (binding [*out* out *err* err]
          (is (= 0 (soranoha/run! args))))
        (is (string/includes? (str out) "usage: soranoha"))
        (is (string/blank? (str err))))))
  (testing "unknown command is status 2 on stderr"
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 2 (soranoha/run! ["nope"]))))
      (is (string/blank? (str out)))
      (is (string/includes? (str err) "unknown command"))))
  (testing "fixed positional arity is status 2 on stderr"
    (let [err (java.io.StringWriter.)]
      (binding [*err* err]
        (is (= 2 (soranoha/run! ["snapshot-index"]))))
      (is (string/includes? (str err) "wrong arity")))))
```

- [ ] **Step 2: Run the characterization test and confirm green baseline**

Run from `abc/`:

```bash
bin/kaocha --focus abc.tools.soranoha-test/cli-status-and-stream-contract-test
```

Expected: PASS before production changes.

- [ ] **Step 3: Declare the direct dependency**

Add beside `org.clojure/tools.cli` in `abc/deps.edn`:

```clojure
org.babashka/cli                         {:mvn/version "0.12.75"}
```

Version 0.12.75 is required because it provides `dispatch` help handling,
`*exit-fn*`, and `format-command-error`; the transitive 0.5.40 artifact does not.

- [ ] **Step 4: Regenerate and inspect the lock**

Run from `abc/`:

```bash
bin/update-clj-nix-lock
git diff -- deps-lock.json
```

Expected: successful lock generation and an intentional babashka.cli upgrade
from transitive 0.5.40 to direct 0.12.75.

- [ ] **Step 5: Verify dependency loading**

Run from `abc/`:

```bash
clojure -M -e "(require 'babashka.cli) (println :babashka-cli-loaded)"
```

Expected: `:babashka-cli-loaded` and exit 0.

- [ ] **Step 6: Commit dependency and characterization baseline**

```bash
git add deps.edn deps-lock.json test/abc/tools/soranoha_test.clj
git commit -m "test(abc): characterize soranoha CLI contract"
```

### Task 2: Migrate build-publication parsing to an option map

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: a parsed map containing `:aozora-root`, `:config`, `:snapshot-date`, `:output-root`, `:replace`, and `:concurrency`.
- Produces: `build-publication! [opts] -> int`, with paths resolved relative to `ABC_INVOCATION_PWD`; Task 3's command adapter calls it directly.

- [ ] **Step 1: Add failing option-map entry-point tests**

Add focused tests around the namespace function, using `with-redefs` for the workflow-heavy boundary where existing tests already do so:

```clojure
(deftest build-publication-accepts-parsed-option-map-test
  (let [parse-var (ns-resolve 'abc.tools.soranoha-build-publication 'parse-args)
        resolve-var (ns-resolve 'abc.tools.soranoha-build-publication
                                'resolve-invocation-path)]
    (is (some? parse-var))
    (is (= {:aozora-root "/invocation/a"
            :config "/invocation/c.json"
            :snapshot-date "2026-07-13"
            :output-root "/invocation/o"
            :replace false
            :concurrency 3}
           (with-redefs-fn {resolve-var #(str "/invocation/" %)}
             #(@parse-var {:aozora-root "a"
                           :config "c.json"
                           :snapshot-date "2026-07-13"
                           :output-root "o"
                           :replace false
                           :concurrency 3}))))))
```

Also update one existing successful `build-publication/build-publication!` direct call to pass the option map shown above rather than a CLI string vector.

- [ ] **Step 2: Run the focused test and verify red**

```bash
bin/kaocha --focus abc.tools.soranoha-test/build-publication-accepts-parsed-option-map-test
```

Expected: FAIL because `parse-args` still sends a map to `clojure.tools.cli/parse-opts`.

- [ ] **Step 3: Replace CLI parsing with option normalization**

Remove `[clojure.tools.cli :as cli]` and `cli-options`. Replace `parse-args` with a map normalizer:

```clojure
(defn- parse-args [options]
  (reduce (fn [opts k] (update opts k resolve-invocation-path))
          options
          [:aozora-root :config :output-root]))
```

Keep snapshot-date required validation in `build-publication!` as defense in depth. Change its parameter name from `args` to `options` and call `(parse-args options)`. Do not alter workflow construction, path promotion, policy checks, or output.

- [ ] **Step 4: Update all direct namespace calls in tests mechanically**

For calls to `build-publication/build-publication!` (not `soranoha/run!`), convert alternating option vectors to maps. Preserve the same values and expected failures. Dispatcher calls remain vectors for Task 3.

- [ ] **Step 5: Run build-publication-focused tests**

```bash
bin/kaocha --focus abc.tools.soranoha-test --focus abc.sim.content-sim-test
```

Expected: PASS. If a failure differs only because a direct call still passes a vector, convert that call without changing its assertions.

- [ ] **Step 6: Commit the focused API migration**

```bash
git add src/abc/tools/soranoha_build_publication.clj test/abc/tools/soranoha_test.clj test/abc/sim/content_sim_test.clj
git commit -m "refactor(abc): accept parsed publication build options"
```

### Task 3: Replace the Soranoha dispatcher with babashka.cli

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `build-publication/build-publication! [opts] -> int` from Task 2 and existing positional command functions.
- Produces: public `command-table`, `normalize-help-args [args] -> vector`, and `run! [args] -> int`.

- [ ] **Step 1: Write failing generated-help tests**

Add tests for all new forms and generated details:

```clojure
(deftest generated-command-help-test
  (doseq [args [["help" "snapshot-index"]
                ["snapshot-index" "--help"]
                ["snapshot-index" "-h"]]]
    (let [out (java.io.StringWriter.)
          err (java.io.StringWriter.)]
      (binding [*out* out *err* err]
        (is (= 0 (soranoha/run! args))))
      (is (string/includes? (str out)
                            "Usage: soranoha snapshot-index"))
      (is (string/includes? (str out) "<label-or-request-set-json>"))
      (is (string/includes? (str out) "<output-path>"))
      (is (string/includes? (str out) "--help"))
      (is (string/blank? (str err))))))

(deftest generated-build-publication-help-test
  (let [out (with-out-str
              (is (= 0 (soranoha/run! ["build-publication" "--help"]))))]
    (doseq [fragment ["--aozora-root" "--config" "--snapshot-date"
                      "--output-root" "--replace" "--concurrency" "--help"]]
      (is (string/includes? out fragment)))))

(deftest generated-global-help-lists-commands-test
  (let [out (with-out-str
              (is (= 0 (soranoha/run! ["--help"]))))]
    (is (string/includes? out "Usage: soranoha"))
    (is (string/includes? out "Commands:"))
    (doseq [command ["snapshot-index" "build-publication"
                     "annotation-join-stats-run"]]
      (is (string/includes? out command)))))
```

Add tests that `help nope` and `help snapshot-index extra` return 2 on stderr, and that `["snapshot-index" "--help"]` does not call the command function (use `with-redefs`).

- [ ] **Step 2: Run new tests and verify red**

```bash
bin/kaocha --focus abc.tools.soranoha-test/generated-command-help-test --focus abc.tools.soranoha-test/generated-build-publication-help-test
```

Expected: FAIL because the existing dispatcher treats command help as wrong arity or an unknown build option.

- [ ] **Step 3: Add babashka.cli and argument normalization**

Require `[babashka.cli :as cli]`. Add:

```clojure
(defn normalize-help-args [args]
  (let [args (vec args)]
    (cond
      (empty? args) ["--help"]
      (= ["help"] args) ["--help"]
      (and (= "help" (first args)) (= 2 (count args)))
      [(second args) "--help"]
      :else args)))
```

Do not normalize `help <command> extra`; the dispatcher adapter must report it as status 2.

- [ ] **Step 4: Define adapters and the ordered command table**

Replace `usage` and `commands` with a table. Use this exact shape for each positional command, changing names, docs, argument keys, and existing function vars as appropriate:

```clojure
{:cmds ["snapshot-index"]
 :fn (fn [{:keys [opts]}]
       (snapshot-index! (:label-or-request-set-json opts)
                        (:output-path opts)))
 :doc "Build a snapshot index from a request set."
 :args->opts [:label-or-request-set-json :output-path]
 :spec {:label-or-request-set-json {:ref "<label-or-request-set-json>"
                                    :require true}
        :output-path {:ref "<output-path>" :require true}}
 :restrict true}
```

For zero-argument commands, use `:restrict true` and an adapter ignoring the parsed map. Preserve the current table order. Define `build-publication` as:

```clojure
{:cmds ["build-publication"]
 :fn (fn [{:keys [opts]}]
       (build-publication/build-publication! opts))
 :doc "Build publication artifacts from an official Aozora checkout."
 :spec {:aozora-root {:ref "DIR" :desc "Official aozorabunko checkout root." :require true}
        :config {:ref "FILE" :desc "Soranoha publication build config JSON." :require true}
        :snapshot-date {:ref "DATE" :desc "Snapshot date, YYYY-MM-DD." :require true}
        :output-root {:ref "DIR" :desc "Final output root." :require true}
        :replace {:coerce :boolean :desc "Replace an existing output root after a successful build."}
        :concurrency {:ref "N" :coerce :long :default 0
                      :validate {:pred #(>= % 0) :ex-msg "must be >= 0"}
                      :desc "Worker threads (0 = all cores)."}}
 :order [:aozora-root :config :snapshot-date :output-root :replace :concurrency :help]
 :restrict true}
```

Use `:args->opts` with `(cons :first-key (repeat :remaining-key))` only for genuinely variadic positional commands; Soranoha currently has none after `build-publication` becomes option-based.

- [ ] **Step 5: Implement the testable dispatch/status adapter**

Create a private sentinel exception for library usage errors, print formatted library errors to stderr via `:error-fn`, and bind `cli/*exit-fn*` so tests never exit:

```clojure
(defn- usage-error! [data]
  (binding [*out* *err*]
    (println (cli/format-command-error data)))
  (throw (ex-info "soranoha CLI usage error" {:soranoha/usage-error true})))

(defn run! [args]
  (try
    (let [result (binding [cli/*exit-fn* (fn [_] nil)]
                   (cli/dispatch command-table
                                 (normalize-help-args args)
                                 {:prog "soranoha"
                                  :help true
                                  :error-fn usage-error!}))]
      (if (integer? result) result 0))
    (catch clojure.lang.ExceptionInfo e
      (if (:soranoha/usage-error (ex-data e))
        2
        (do (binding [*out* *err*] (println (ex-message e))) 1)))))
```

Pass the complete 0.12.75 data map received by `:error-fn` to
`format-command-error`; do not construct a second library error message. Retain
`-main` unchanged.

- [ ] **Step 6: Run dispatcher tests and correct only compatibility details**

Before running the full namespace, update Task 1's wording assertions to the
generated CLI contract while retaining the original status and stream checks:

```clojure
(is (string/includes? (str out) "Usage: soranoha"))
(is (string/includes? (str err) "Unknown command: nope"))
(is (string/includes? (str err) "Missing required"))
```

This is an intentional presentation change: exit codes and streams are
preserved, while babashka.cli owns capitalization and usage-error wording.

```bash
bin/kaocha --focus abc.tools.soranoha-test
```

Expected: PASS. Compatibility adjustments may change babashka.cli data-shape
plumbing, but must not change the command table's declared names, docs,
validation, streams, or status assertions. The wording assertions change only
as specified immediately above.

- [ ] **Step 7: Manually inspect representative help**

From `abc/`:

```bash
clojure -M:abc/soranoha --help
clojure -M:abc/soranoha snapshot-index --help
clojure -M:abc/soranoha build-publication --help
```

Expected: ordered command index; named positional arguments; all six build options; no command execution.

- [ ] **Step 8: Commit the dispatcher migration**

```bash
git add src/abc/tools/soranoha.clj test/abc/tools/soranoha_test.clj
git commit -m "feat(abc): generate soranoha CLI help"
```

### Task 4: Silence mkCljLauncher and verify the packaged app

**Files:**
- Modify: `abc/flake.nix`
- Modify: `abc/test/abc/tools/adr_governance_test.clj`

**Interfaces:**
- Consumes: shared `mkCljLauncher` template.
- Produces: all `mkCljApp` launchers use direct `-J-Duser.home=...` and do not export `JAVA_TOOL_OPTIONS`.

- [ ] **Step 1: Add a failing focused source-contract test**

In the existing test namespace that checks tracked project files, extract the `mkCljLauncher` form from `flake.nix` (bounded by `mkCljLauncher =` and `mkCljApp =`) and assert:

```clojure
(is (not (string/includes? mk-clj-launcher "export JAVA_TOOL_OPTIONS")))
(is (string/includes? mk-clj-launcher "-J-Duser.home=${cljDepsCache}"))
```

Do not assert that the entire file lacks `JAVA_TOOL_OPTIONS`.

- [ ] **Step 2: Run the focused test and verify red**

```bash
bin/kaocha --focus abc.tools.adr-governance-test
```

Expected: FAIL because `mkCljLauncher` still exports `JAVA_TOOL_OPTIONS` and lacks the direct JVM flag.

- [ ] **Step 3: Make the minimal launcher edit**

Inside `mkCljLauncher`, delete:

```nix
export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
```

Change the final command to:

```nix
exec ${pkgs.clojure}/bin/clojure -J-Duser.home=${cljDepsCache} -M:${alias} "$@"
```

Do not modify the three other environment-variable sites.

- [ ] **Step 4: Run formatting and focused checks**

From the monorepo root:

```bash
just nix-format-check
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: both PASS.

- [ ] **Step 5: Exercise the packaged command and inspect both streams**

```bash
tmp=$(mktemp -d)
nix run .#soranoha -- build-publication --help >"$tmp/out" 2>"$tmp/err"
test ! -s "$tmp/err"
rg --fixed-strings -- "--concurrency" "$tmp/out"
rg --fixed-strings -- "--replace" "$tmp/out"
```

Expected: exit 0; empty stderr; both options in stdout; no `Picked up JAVA_TOOL_OPTIONS` line.

- [ ] **Step 6: Commit the launcher fix**

```bash
git add flake.nix test/abc/tools/adr_governance_test.clj
git commit -m "fix(abc): quiet shared Clojure app launcher"
```

### Task 5: Full verification and documentation consistency

**Files:**
- Modify only if a check exposes a directly related defect.

**Interfaces:**
- Consumes: Tasks 1–4.
- Produces: verified CLI and launcher change ready for integration.

- [ ] **Step 1: Run language-focused quality checks**

From the monorepo root:

```bash
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
just nix-format-check
```

Expected: all PASS.

- [ ] **Step 2: Run the migration gate**

```bash
just validate-migration
```

Expected: PASS.

- [ ] **Step 3: Check diffs and generated artifacts**

```bash
git status --short
git diff --check HEAD~4..HEAD
git log -5 --oneline
```

Expected: only planned source, tests, dependency/lock, spec/plan, and Nix changes; no corpus-scale or temporary artifacts.

- [ ] **Step 4: Record final evidence**

In the handoff response, report the exact commands run, their pass/fail status, and representative help behavior. Do not claim the noisy stderr is fixed unless the packaged `nix run` check from Task 4 passed with empty stderr.
