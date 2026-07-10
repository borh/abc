# abc Clojure Quality Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **STATUS: DRAFT FOR REVIEW — not yet approved for execution.**

**Goal:** Remove three systemic quality problems in `abc/src` (+ tests) found in the 2026-07-10 review: an unused `babashka.fs` dependency (with silent-failure `java.io` interop in its place), mechanical duplication (CLI entry points, deterministic-JSON emission, temp-dir/delete helpers), and a malli foundation whose marquee capabilities are wired but never driven.

**Architecture:** Introduce/extend small shared foundations (`abc.tools.cli`, `abc.tools.files`, `abc.tools.json`) and route existing call sites onto them; adopt `babashka.fs` for filesystem work so ignored-failure interop becomes throwing calls; then apply behavior-preserving idiom/dead-code cleanups and test-hygiene fixes. Each **Batch** is independently executable and independently valuable — a reviewer can approve or reject a batch without touching its neighbors. Batches are ordered by payoff and dependency; within a batch, foundational tasks precede the migrations that consume them.

**Tech Stack:** Clojure 1.12.5, `babashka/fs` 0.5.34, `babashka/process` 0.6.25, `org.clojure/tools.cli` 1.4.256, `com.cnuernber/charred` 1.039, `metosin/malli` 0.20.1, `com.taoensso/telemere` 1.2.1, kaocha (`bin/kaocha`).

## Global Constraints

- Run all Clojure commands from `abc/`. Tests run through `bin/kaocha` (which is `clojure -M:test:kaocha -m kaocha.runner`). **Never** use `clojure -M:test`.
- Run a single test namespace with: `bin/kaocha --focus <fully.qualified.ns-test>`. Run one test var with `--focus <ns>/<var>`. **`--focus` takes exactly one value; to focus several namespaces, repeat the flag** — `bin/kaocha --focus a-test --focus b-test`. A space-separated list — `--focus a-test b-test` — is parsed as suite IDs and fails with `No such suite`.
- Keep all deterministic output **byte-stable**. (Note: *canonical* in this repo means RFC 8785/JCS via `abc.tools.jcs`; the `abc.tools.json` helpers produce **deterministic sorted-key** JSON, which is not JCS — never call them "canonical".) Any change touching JSON/EDN/RDF/Mermaid serialization must leave existing golden-file and round-trip tests green; do not alter key ordering, indentation, or trailing-newline behavior.
- Preserve every public function signature that is called from another namespace or a test unless a task explicitly changes it. Behavior-preserving tasks must not change observable output.
- Follow existing conventions: side-effecting fns end in `!`, predicates end in `?`, private helpers are `defn-`. Namespaced-string map keys stay strings (this codebase uses string keys for wire data).
- Format touched Clojure through the repo checks (cljfmt / clj-kondo); no new compiler or linter warnings.
- Commit after each task's tests pass. The plan file itself is an untracked review draft until explicitly approved.
- Work on a feature branch in a worktree per repo convention (`.worktrees/`), merge to `main` when a batch is complete and its gates pass.

## Executable scope & sequencing (v2 — authoritative, after second review)

A second review (verified against the code) reordered and pruned this plan. **This section overrides the batch order below.** The batch write-ups (A–G) remain as the detailed task source, but only the tasks listed here are in executable scope, in this order:

1. **C1 → C2** — deterministic sorted-key JSON string helpers (corrected terminology). *No dependency; smallest, safest.*
2. **B1 → B2** — `babashka.fs` filesystem foundation + recursive-delete replacement, **with failure-path tests**. *Depends on C1 (`files/read-json-lines` uses `read-json-str`).*
3. **D1** — path/EDN consolidation, after correcting each call site's inventory (the three `relative-path` locals are distinct).
4. **A1 + one CLI pilot** — build `abc.tools.cli`, then migrate **one** CLI with **subprocess-level** characterization tests (stdout/stderr/exit code) before touching the rest. Migrate further CLIs by family only after the pilot proves the contract.
5. **F (targeted)** — remaining real temp-file leaks only, in a **new JVM-only `test/abc/test_fs.clj`** helper (freshly enumerated — see F notes).
6. **E1** — remove the 14 dead 2-arity TEI renderers.
7. **Reassess** further CLI/filesystem migrations from measured payoff.

**Explicit dependencies:** C1 → B1; F1 → F2 (F2 uses F1's helper); A1 → A-pilot → A-family-migrations.

**DEFERRED — removed from executable scope** (do not execute without a fresh, separate decision):
- **Batch G (malli)** — *stale and underspecified.* Much of `2026-04-28-malli-integration-improvements.md` has already landed (`design-bundle-schemas`, `cached-schema`/`cached-schema-hash`, `humanize-validation-errors`, `explanation-messages`, `explain-or-throw!`, `set-default-registry!`+`instrument!` all exist in `abc/src/abc/tools/malli.clj`). G1's `::metadata-record-input`/`::metadata-record` schemas **do not exist**, so the core contract is undecided. **Replace Batch G with a dedicated reconciliation audit** against the 2026-04 plan before designing any remaining instrumentation. The real residual gaps (no instrumented `m/=>` contracts; constant `:gen/elements`) are genuine but must be re-scoped from that audit, not from this plan.
- **B4** (`file-seq`→`fs/glob`) and **B5** (`.exists`/`.getName` interop sweep) — high-churn, can change traversal semantics; defer unless a concrete defect motivates a specific site.
- **A3's `aat_parser_ir_compat` migration** — no test namespace exists; out of scope until a characterization ns is written.
- **E2** (`validate-json-schemas!`) — author admits low payoff / error-prone. **E3** — split by owning namespace and do opportunistically. **F3** (`are` conversion) — cosmetic.

---

## File Structure

**New files**
- `src/abc/tools/cli.clj` — shared CLI entry harness: `strip-double-dash`, `parse`, `dispatch!` (pure exit-code decision + `:run` dispatch, no `System/exit`), `run-cli!` (installs handler, calls `dispatch!`, performs `System/exit`).
- `test/abc/tools/cli_test.clj` — exercises `dispatch!` exit-code mapping and `strip-double-dash` without exiting the JVM.

**Modified foundations**
- `src/abc/tools/files.clj` — reimplement `delete-tree!`, `copy-file!`, `repo-root`, `path` on `babashka.fs`; add `read-edn`, `relative-path`.
- `src/abc/tools/json.clj` — add `read-json-str`, `write-deterministic-json-str`; refactor `write-deterministic-json-file!` to reuse the string helper.
- `test/abc/tools/files_test.clj` — new (foundation currently untested); covers `delete-tree!` missing-path no-op, `copy-file!` parent creation, `relative-path`, `read-edn`.
- `test/abc/tools/json_test.clj` — extend with `read-json-str` / `write-deterministic-json-str` round-trip and byte-equality-with-file-writer assertions.

**Migrated call sites** (enumerated per task): `validate_corpus`, `materialize_import`, `materialize_source_snapshot`, `source_snapshot_workset`, `manifest_to_rdf`, `person_drift_history`, `aozora_history_audit`, `aat_parser_ir_compat`, `workflow/report`, `annotation_join_stats`, `soranoha`, `soranoha_build_publication`, `soranoha_layout_report`, `request_set_resolver`, `parser_ir_tei`, `validate_design_bundle`, plus the test files listed in Batch F.

---

# Batch A — Shared CLI entry harness (`abc.tools.cli`)

**Why:** ~11 `-main`s copy the same parse→error→usage→exit template; `normalize-cli-args` is duplicated verbatim; `install-cli-handler!` is missing from 7 of 14 mains, so CLI output format silently differs by tool. One helper deletes the most lines and closes the output-consistency gap.

> **⚠ This batch changes observable behavior** (installs logging handlers where there were none, alters exception handling, standardizes exit codes). Worker-level tests do **not** characterize stdout/stderr/exit codes. **Execution order (per Executable scope):** build A1, then a **single-CLI pilot** with subprocess characterization tests (A2/A3 Step 4), and only migrate the rest **by family** once the pilot's contract holds. Do not mass-migrate all mains in one pass.

### Task A1: Create `abc.tools.cli`

**Files:**
- Create: `src/abc/tools/cli.clj`
- Test: `test/abc/tools/cli_test.clj`

**Interfaces:**
- Produces:
  - `(strip-double-dash args) -> seq` — drops a single leading `"--"`.
  - `(parse args {:keys [cli-options required]}) -> {:options :arguments :errors :summary :missing}` where `:missing` is the subset of `required` option keys whose value is `nil`. `:arguments` is tools.cli's positional-args vector, passed through.
  - `(dispatch! parsed {:keys [usage-fn run fail? min-args max-args]}) -> int` — returns the exit code and performs the `:run` side effect in the success branch. Does **not** call `System/exit`. `usage-fn` is `(fn [summary] -> string)`; `run` receives the full context map **`{:options :arguments}`** and returns a result; `fail?` is `(fn [result] -> boolean)`, default `(constantly false)`. `min-args`/`max-args` (optional) bound the positional-argument count for tools that take positional inputs.
  - `(run-cli! args opts) -> nil` — `opts` merges the keys of `parse` and `dispatch!`: `{:cli-options :required :min-args :max-args :usage-fn :run :fail?}`. Installs the compact CLI log handler, parses, dispatches, then `(System/exit code)`.
  - Exit-code contract: `0` = success or `--help`; `1` = `fail?` true; `2` = parse errors, missing required options, positional-arity violation, or an `ExceptionInfo` thrown by `run`.

  **Design note:** `run` deliberately receives `{:options :arguments}` (not bare `options`) so positional-argument tools (`materialize_import`, `manifest_to_rdf`) are first-class. Option-only tools destructure `(fn [{:keys [options]}] …)`.

- [ ] **Step 1: Write the failing test**

```clojure
(ns abc.tools.cli-test
  (:require [abc.tools.cli :as cli]
            [clojure.test :refer [deftest is testing]]))

(def cli-options
  [["-i" "--input FILE" "input" :id :input]
   ["-h" "--help" "help"]])

(defn- usage [_summary] "Usage: tool --input FILE")

(deftest strip-double-dash-drops-single-leading-separator
  (is (= '("--input" "x") (cli/strip-double-dash ["--" "--input" "x"])))
  (is (= ["--input" "x"] (cli/strip-double-dash ["--input" "x"]))))

(deftest dispatch-maps-outcomes-to-exit-codes
  (testing "help prints usage and returns 0 without running"
    (let [ran? (atom false)
          parsed (cli/parse ["--help"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] (reset! ran? true))})]
      (is (= 0 code))
      (is (false? @ran?))))
  (testing "missing required option returns 2 and does not run"
    (let [ran? (atom false)
          parsed (cli/parse [] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage :run (fn [_] (reset! ran? true))})]
      (is (= 2 code))
      (is (false? @ran?))))
  (testing "success returns 0 and runs, run sees {:options :arguments}"
    (let [parsed (cli/parse ["--input" "x" "pos1"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [{:keys [options arguments]}]
                                             (is (= "x" (:input options)))
                                             (is (= ["pos1"] arguments)))})]
      (is (= 0 code))))
  (testing "too many positional args returns 2"
    (let [parsed (cli/parse ["--input" "x" "a" "b"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage :max-args 1 :run (fn [_] :ran)})]
      (is (= 2 code))))
  (testing "fail? predicate true returns 1"
    (let [parsed (cli/parse ["--input" "x"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] {:bad 3})
                                      :fail? (fn [r] (pos? (:bad r)))})]
      (is (= 1 code))))
  (testing "ExceptionInfo from run returns 2"
    (let [parsed (cli/parse ["--input" "x"] {:cli-options cli-options :required [:input]})
          code (cli/dispatch! parsed {:usage-fn usage
                                      :run (fn [_] (throw (ex-info "boom" {:k 1})))})]
      (is (= 2 code)))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.cli-test`
Expected: FAIL — `No such namespace: abc.tools.cli` / unresolved `cli/parse`.

- [ ] **Step 3: Write minimal implementation**

```clojure
(ns abc.tools.cli
  "Shared CLI entry harness for `-main` functions. Centralizes the
  parse -> validate-required -> dispatch -> exit-code shape that was
  copied across ~11 tools, and guarantees every CLI installs the
  compact Telemere handler. `dispatch!` returns the exit code (no
  `System/exit`) so it is unit-testable; `run-cli!` performs the exit."
  (:require [abc.tools.logging :as logging]
            [clojure.string :as string]
            [clojure.tools.cli :as tools-cli]
            [taoensso.telemere :as tel]))

(defn strip-double-dash
  "Drop a single leading \"--\" separator (as passed by `clojure -M:tool --`)."
  [args]
  (if (= "--" (first args))
    (rest args)
    args))

(defn parse
  "Parse `args` with tools.cli. Returns the tools.cli result map (incl.
  `:arguments`) plus `:missing` — the `required` option keys whose parsed
  value is nil."
  [args {:keys [cli-options required]}]
  (let [result (tools-cli/parse-opts (strip-double-dash args) cli-options)]
    (assoc result :missing (remove #(some? (get (:options result) %))
                                   (or required [])))))

(defn dispatch!
  "Map a parsed CLI result to an exit code, running `run` on success.
  `run` receives {:options :arguments}. Returns an int; never calls
  System/exit. Codes: 0 ok/help, 1 fail?, 2 usage error / arity / ExceptionInfo."
  [{:keys [options arguments errors summary missing]}
   {:keys [usage-fn run fail? min-args max-args] :or {fail? (constantly false)}}]
  (let [arg-count (count arguments)
        arity-error? (or (and min-args (< arg-count min-args))
                         (and max-args (> arg-count max-args)))]
    (cond
      (:help options)
      (do (println (usage-fn summary)) 0)

      (or (seq errors) (seq missing) arity-error?)
      (do (binding [*out* *err*]
            (doseq [e errors] (tel/log! :error e))
            (when (seq missing)
              (tel/log! :error (str "Missing required option(s): "
                                    (string/join ", " (map name missing)))))
            (when arity-error?
              (tel/log! :error (str "Wrong number of arguments: got " arg-count)))
            (println (usage-fn summary)))
          2)

      :else
      (try
        (if (fail? (run {:options options :arguments arguments})) 1 0)
        (catch clojure.lang.ExceptionInfo ex
          (binding [*out* *err*]
            (println (ex-message ex))
            (when-let [data (seq (ex-data ex))]
              (println (pr-str data))))
          2)))))

(defn run-cli!
  "Full CLI entry: install the compact handler, parse, dispatch, exit.
  `opts` = {:cli-options :required :usage-fn :run :fail?}."
  [args opts]
  (logging/install-cli-handler!)
  (System/exit (dispatch! (parse args opts) opts)))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bin/kaocha --focus abc.tools.cli-test`
Expected: PASS (5 tests / all assertions).

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/cli.clj test/abc/tools/cli_test.clj
git commit -m "feat(cli): add shared abc.tools.cli entry harness"
```

### Task A2: Migrate simple validate-then-exit mains onto `run-cli!`

**Files:**
- Modify: `src/abc/tools/validate_corpus.clj` (`-main` ~110-124), `src/abc/tools/materialize_import.clj` (`-main` ~132-146), `src/abc/tools/materialize_source_snapshot.clj` (`-main` ~207-232), `src/abc/tools/source_snapshot_workset.clj` (`-main` ~204-232), `src/abc/tools/manifest_to_rdf.clj` (`-main` ~363-374)

**Interfaces:**
- Consumes: `abc.tools.cli/run-cli!` (Task A1).

These five share the exact shape: `install-cli-handler!` → `parse-opts` → `(if (or (seq errors) (nil? required…)) (do (doseq [e errors] log) (usage) (exit 2)) run)`. Migration is identical per file; the transformation is shown once.

- [ ] **Step 1: Confirm the current behavior is covered**

Run the existing tests for these tools so you have a green baseline:
`bin/kaocha --focus abc.tools.validate-corpus-test --focus abc.tools.materialize-import-test --focus abc.tools.materialize-source-snapshot-test --focus abc.tools.source-snapshot-workset-test --focus abc.tools.manifest-to-rdf-test`
Expected: PASS (baseline before refactor).

- [ ] **Step 2: Rewrite each `-main` using `run-cli!`**

Add `[abc.tools.cli :as abc-cli]` to the ns `:require`. Replace the `-main` body. **Critical:** `:run` must reproduce *every* side effect that currently lives in the success branch of `-main`, not just call the worker. For `validate_corpus.clj`, `-main` logs `"checked N works, M failed"` and iterates `first-failures` — that logging is **not** inside `validate-corpus!`, so it must move into `:run`:

```clojure
(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :required    [:input-dir]
    :usage-fn    (fn [_summary] "Usage: clojure -M:abc/validate-corpus --input-dir <DIR>")
    :run         (fn [{:keys [options]}]
                   (let [{:keys [works-checked failed first-failures] :as result}
                         (validate-corpus! options)]
                     (tel/log! :info (str "checked " works-checked " works, " failed " failed"))
                     (doseq [f first-failures]
                       (tel/log! :error (str (:work-id f) ": " (:error f))))
                     result))
    :fail?       (fn [{:keys [failed]}] (pos? failed))}))
```

Apply the same discipline (move all success-branch logging into `:run`) to the others:
- `materialize_source_snapshot.clj`: option-based. `:required [:workset-path :output-path]`, `:run (fn [{:keys [options]}] …)`, no `:fail?`.
- `source_snapshot_workset.clj`: option-based. `:required [:input-root :output-path :snapshot-scope :snapshot-date]`, no `:fail?`. **Note:** this file currently does *not* call `install-cli-handler!`; routing through `run-cli!` adds it (desired fix).
- **`materialize_import.clj` — POSITIONAL.** Its required inputs are positional `:arguments` (`[input-dir output-dir positional-generated-at & extra]`), with a `(seq extra)` too-many-args guard and a `--generated-at` *option* that falls back to the positional then a default. Migrate with `:min-args 2 :max-args 3` and read positionals inside `:run`:

  ```clojure
  :run (fn [{:keys [options arguments]}]
         (let [[input-dir output-dir positional-generated-at] arguments
               generated-at (or (:generated-at options) positional-generated-at default-generated-at)]
           (materialize-import! {:input-dir input-dir :output-dir output-dir :generated-at generated-at})
           (tel/log! :info (str "materialized imported parser output to " output-dir))))
  ```
- **`manifest_to_rdf.clj` — POSITIONAL.** The manifest path is a positional argument (`[manifest-path & extra]`), not an option, with a `(seq extra)` guard. Migrate with `:min-args 1 :max-args 1`; read `(first arguments)` inside `:run`, preserving the `-o/--output`-vs-stdout branch.

Keep each file's existing `cli-options` and `usage` string content; move the usage string into `:usage-fn` (drop the now-unused standalone `usage` defn if it has no other caller). Add `[taoensso.telemere :as tel]` to any ns whose `:run` now logs and didn't already require it.

- [ ] **Step 3: Run the tests**

Run: `bin/kaocha --focus abc.tools.validate-corpus-test --focus abc.tools.materialize-import-test --focus abc.tools.materialize-source-snapshot-test --focus abc.tools.source-snapshot-workset-test --focus abc.tools.manifest-to-rdf-test`
Expected: PASS — behavior unchanged.

- [ ] **Step 4: Smoke-test one CLI end-to-end**

Run: `clojure -M:abc/validate-corpus` (no args)
Expected: prints the usage line to stderr and exits `2`; verify with `echo $?`.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/validate_corpus.clj src/abc/tools/materialize_import.clj \
        src/abc/tools/materialize_source_snapshot.clj src/abc/tools/source_snapshot_workset.clj \
        src/abc/tools/manifest_to_rdf.clj
git commit -m "refactor(cli): route validate-then-exit mains through abc.tools.cli"
```

### Task A3: Migrate help/errors/try-catch mains and delete duplicated `normalize-cli-args`

**Files:**
- Modify: `src/abc/tools/person_drift_history.clj` (`normalize-cli-args` ~46-49, `-main` ~285-311), `src/abc/tools/aozora_history_audit.clj` (`normalize-cli-args` ~52, `-main` ~363-403), `src/abc/tools/aat_parser_ir_compat.clj` (inline `--` strip ~205, `-main` ~203-227)

**Interfaces:**
- Consumes: `abc.tools.cli/{run-cli!,strip-double-dash}` (Task A1).

These three add `:help` handling and a `catch ExceptionInfo` — both already provided by `dispatch!`.

- [ ] **Step 1: Green baseline**

Run: `bin/kaocha --focus abc.tools.person-drift-history-test --focus abc.tools.aozora-history-audit-test`
Expected: PASS.

**Coverage gap — `aat_parser_ir_compat` has NO test namespace** (`test/abc/tools/aat_parser_ir_compat_test.clj` does not exist), and its `-main` uses bespoke `println`/`System/exit` with no `ExceptionInfo` catch. Before migrating it, either (a) write a small characterization test for its admission logic first, or (b) manually exercise `clojure -M:abc/aat-compat-admission` on a good and a bad candidates file and record the exit codes, then migrate only if `run-cli!`'s 0/1/2 mapping reproduces them. Do not migrate it blind.

- [ ] **Step 2: Replace each private `normalize-cli-args` with `abc-cli/strip-double-dash` and rewrite `-main`**

Add `[abc.tools.cli :as abc-cli]`. Delete the local `normalize-cli-args` defn. Rewrite `-main` (using `person_drift_history.clj`, which exits 1 when candidate counts are positive):

```clojure
(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :usage-fn    usage            ; existing (fn [summary] ...) usage builder
    :run         write-report!
    :fail?       (fn [result]
                   (and (:fail-on-candidates? result)  ; if the flag rides on options, thread it via run
                        (pos? (+ (get-in result ["summary" "split_candidates"])
                                 (get-in result ["summary" "merge_candidates"])))))}))
```

Note the current `person_drift_history` reads `:fail-on-candidates` from *options*, not the result, so the fail decision needs both. `run` now receives `{:options :arguments}`, so compute the result once and stash the exit decision on it:

```clojure
    :run   (fn [{:keys [options]}]
             (let [result (write-report! options)
                   n (+ (get-in result ["summary" "split_candidates"])
                        (get-in result ["summary" "merge_candidates"]))]
               (assoc result ::exit-fail? (and (:fail-on-candidates options) (pos? n)))))
    :fail? ::exit-fail?
```

Apply the same structure to `aozora_history_audit.clj` (its own fail condition and usage builder). For `aat_parser_ir_compat.clj`, only migrate after the coverage step above; if its exit codes don't map cleanly onto 0/1/2, keep it on a thin custom `dispatch!` wrapper rather than forcing `run-cli!`.

- [ ] **Step 3: Run the tests**

Run: `bin/kaocha --focus abc.tools.person-drift-history-test --focus abc.tools.aozora-history-audit-test`
Expected: PASS. (`aat_parser_ir_compat` is **not** migrated here — it has no test ns; see Executable scope.)

- [ ] **Step 4: Subprocess characterization (required — worker tests do not cover CLI I/O)**

For each migrated tool, assert the **observable contract** end-to-end with `babashka.process`: `--help` → usage on **stdout**, exit `0`; a bad flag → error + usage on **stderr**, exit `2`; a success run → expected exit `0`/`1`. Example (aozora-history-audit):

```
clojure -M:abc/aozora-history-audit --help    # stdout has usage, echo $? == 0
clojure -M:abc/aozora-history-audit --nonsense # stderr has error+usage, echo $? == 2
```

Capture these as a `*_cli_test.clj` using `process`/`sh` (as `test/abc/html_test.clj` already does) so the exit-code/stream contract is pinned, not just the worker return.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/person_drift_history.clj src/abc/tools/aozora_history_audit.clj \
        src/abc/tools/aat_parser_ir_compat.clj
git commit -m "refactor(cli): unify help/error/exception CLI mains on abc.tools.cli"
```

---

# Batch B — Adopt `babashka.fs`

**Why:** `babashka/fs` is declared in `deps.edn` but used in **zero** source files. Filesystem work runs through `clojure.java.io` + raw `java.io.File`, and every one of the 18 `.mkdirs` calls and all three hand-rolled recursive deletes **ignore their failure signal**. `fs/create-dirs` and `fs/delete-tree` throw instead, converting silent failures into loud ones.

> **Note on `fs/delete-tree` + missing paths:** in this fs version `fs/delete-tree` on a non-existent path returns `nil` (does not throw), so the `fs/exists?` guard in `delete-tree!` below is harmless but not strictly required — keep it anyway to preserve the old wrapper's explicit no-op semantics and make intent obvious. (The old `delete-tree!` also returned `nil` via `doseq`, so the return type is unchanged.)

### Task B1: Reimplement `abc.tools.files` filesystem helpers on `fs`

**Files:**
- Modify: `src/abc/tools/files.clj`
- Test: `test/abc/tools/files_test.clj` (create — this foundation is currently untested)

**Interfaces:**
- Produces (signatures unchanged): `delete-tree!`, `copy-file!`, `repo-root`, `path`. New: `read-edn`, `relative-path` (used by Batch D).

- [ ] **Step 1: Write the failing test**

```clojure
(ns abc.tools.files-test
  (:require [abc.tools.files :as files]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(deftest delete-tree-is-a-noop-on-missing-path
  (is (nil? (files/delete-tree! (str (fs/path (fs/temp-dir) "abc-files-missing-xyz"))))))

(deftest delete-tree-removes-a-populated-tree
  (fs/with-temp-dir [d {}]
    (let [nested (fs/file d "a" "b")]
      (fs/create-dirs nested)
      (spit (fs/file nested "f.txt") "x")
      (files/delete-tree! (fs/file d "a"))
      (is (not (fs/exists? (fs/file d "a")))))))

(deftest copy-file-creates-missing-parents
  (fs/with-temp-dir [d {}]
    (let [src (fs/file d "src.txt")
          dst (fs/file d "nested" "deep" "dst.txt")]
      (spit src "hello")
      (files/copy-file! src dst)
      (is (= "hello" (slurp dst))))))

(deftest copy-file-handles-parentless-target
  ;; regression guard: a bare-filename target must not NPE
  (fs/with-temp-dir [d {}]
    (let [src (fs/file d "src.txt")]
      (spit src "hi")
      (is (some? (files/copy-file! src "abc-copy-parentless-target.txt")))
      (fs/delete-if-exists "abc-copy-parentless-target.txt"))))

(deftest read-edn-parses-a-file
  (fs/with-temp-dir [d {}]
    (let [f (fs/file d "x.edn")]
      (spit f "{:a 1 :b [2 3]}")
      (is (= {:a 1 :b [2 3]} (files/read-edn f))))))

(deftest relative-path-uses-forward-slashes
  (is (= "a/b/c.txt"
         (files/relative-path (fs/file "/root") (fs/file "/root/a/b/c.txt")))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bin/kaocha --focus abc.tools.files-test`
Expected: FAIL — `read-edn`/`relative-path` unresolved; possibly `delete-tree!` behavior differences.

- [ ] **Step 3: Rewrite `files.clj`**

```clojure
(ns abc.tools.files
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(def hash-pattern hash/hash-pattern)

(defn repo-root []
  (fs/file (fs/canonicalize ".")))

(defn path [& segments]
  (apply fs/file (repo-root) segments))

(defn read-json [file]
  (abc-json/read-json-file file))

(defn read-json-lines [file]
  (->> (string/split-lines (slurp (fs/file file)))
       (remove string/blank?)
       (mapv abc-json/read-json-str)))          ; see Batch C for read-json-str

(defn read-edn [file]
  (edn/read-string (slurp (fs/file file))))

(defn relative-path [base file]
  (string/replace (str (fs/relativize (fs/path base) (fs/path file))) "\\" "/"))

(defn delete-tree! [file]
  (when (fs/exists? file)
    (fs/delete-tree file)))

(defn copy-file! [source target]
  ;; Guard: (fs/parent "bare.txt") is nil and (fs/create-dirs nil) throws NPE.
  ;; The old (when-let [parent (.getParentFile target)] …) was a no-op for
  ;; parentless targets — preserve that.
  (when-let [parent (fs/parent target)]
    (fs/create-dirs parent))
  (fs/copy source target {:replace-existing true})
  (fs/file target))

(defn bytes->hex [bytes] (hash/bytes->hex bytes))
(defn sha256-file [file] (hash/sha256-file file))

(defn example-hash [suffix]
  (str "sha256:"
       (apply str (repeat (- 64 (count suffix)) "0"))
       suffix))
```

> **Ordering dependency:** `read-json-lines` above references `abc-json/read-json-str`, added in Batch C Task C1. If Batch B is executed before Batch C, keep the existing `charred`-based `read-json-lines` body unchanged in this task and migrate it in C1. Do not introduce a forward reference to an unwritten fn.

- [ ] **Step 4: Run test to verify it passes**

Run: `bin/kaocha --focus abc.tools.files-test`
Expected: PASS. Then run consumers: `bin/kaocha --focus abc.tools.soranoha-test --focus abc.tools.materialize-publication-test` (both call `files/copy-file!`/`delete-tree!`).
Expected: PASS — signatures preserved.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/files.clj test/abc/tools/files_test.clj
git commit -m "refactor(files): reimplement filesystem helpers on babashka.fs"
```

### Task B2: Replace the two remaining hand-rolled recursive deletes

**Files:**
- Modify: `src/abc/tools/aozora_history_audit.clj` (`delete-recursive!` ~57-64 and its call site), `src/abc/tools/validate_design_bundle.clj` (`finally` cleanups at ~825-826 and ~1184-1185)

- [ ] **Step 1: Green baseline** — `bin/kaocha --focus abc.tools.aozora-history-audit-test --focus abc.tools.validate-design-bundle-test`. Expected: PASS.
- [ ] **Step 2:** In `aozora_history_audit.clj`, delete the private `delete-recursive!` defn and replace its call site with `(abc.tools.files/delete-tree! path)` (add `[abc.tools.files :as files]` if absent). In `validate_design_bundle.clj`, replace each `(doseq [f (reverse (file-seq temp))] (.delete f))` inside `finally` with `(files/delete-tree! temp)`.
- [ ] **Step 3:** `bin/kaocha --focus abc.tools.aozora-history-audit-test --focus abc.tools.validate-design-bundle-test`. Expected: PASS.
- [ ] **Step 4: Commit** — `git commit -am "refactor(fs): replace manual recursive deletes with files/delete-tree!"`

### Task B3: Replace ignored-return `.mkdirs` with `fs/create-dirs` (migrate per owning component)

> **This is a correctness change, not just idiom** — `fs/create-dirs` *throws* on failure where `.mkdirs` returned an ignored `false`. Treat each owning component as its own commit **with a failure-path test**, not one blanket sweep.

**Files:**
- Modify (per component, separate commits): `src/abc/tools/materialize_publication.clj:567`, `src/abc/tools/soranoha_build_publication.clj:316`, `src/abc/tools/tar.clj` (~58-59), `src/abc/tools/snapshot_index.clj` (~247-248), `src/abc/tools/aozora_ingest.clj` (~201), `src/abc/tools/source_snapshot_workset.clj` (~171-173) — **plus every other site from the Step 1 enumeration** (18 total; the list above is not exhaustive).

- [ ] **Step 1: Enumerate ALL sites** — `rg -n '\.mkdirs' src` (expect 18). The per-file list above is a starting point, not the whole set; every hit is a target.
- [ ] **Step 2: For each component, add a failure-path test first** — e.g. point the output dir at an un-creatable path (a file where a dir is expected) and assert the operation now **throws** (previously it silently continued). This characterizes the behavior change deliberately.
- [ ] **Step 3: Transform** — `(.mkdirs dir)` → `(fs/create-dirs dir)`; `(.mkdirs (.getParentFile f))` → `(fs/create-dirs (fs/parent f))`; drop now-unused `io/file` wrapping and `_ (.mkdirs …)` `let` bindings. Add `[babashka.fs :as fs]` to each ns. `fs/create-dirs` is a no-op when the dir exists, matching prior intent, but throws on real failure.
- [ ] **Step 4: Run affected tests** per component (`bin/kaocha --focus <owning-ns-test>`). Expected: PASS.
- [ ] **Step 5: Commit per component** — `git commit -m "refactor(fs): throw on directory-creation failure in <component>"`

### Task B4: Replace `file-seq` walks with `fs/glob`  — ⚠ DEFERRED (not in executable scope)

> **Deferred per second review:** high-churn and can change traversal/ordering semantics with no concrete defect motivating it. Keep as a backlog item; do a specific site only when a real defect there justifies it. The write-up below is retained for that eventuality.

**Files:**
- Modify: `src/abc/tools/request_set_resolver.clj` (~47-50), `src/abc/tools/validate_design_bundle.clj` (~51-53), `src/abc/tools/soranoha.clj` (~889), `src/abc/tools/soranoha_layout_report.clj` (~5-6), `src/abc/tools/soranoha_build_publication.clj` (~91)

- [ ] **Step 1: Green baseline** for each owning test ns.
- [ ] **Step 2: Transform.** Example (`request_set_resolver.clj`): a `file-seq` + `(filter #(and (.isFile %) (string/ends-with? (.getName %) ".json")))` collapses to `(fs/glob dir "*.json")` (or `"**/*.json"` if the walk is recursive — check the original). `soranoha_build_publication.clj:91` `.zip` walk → `(fs/glob aozora-root "**/*.zip")` and drop the local `zip-file?` predicate. `soranoha.clj:889` `.tar.zst` walk → `(fs/glob root "**/*.tar.zst")`. **Preserve ordering:** if the original result was later `sort`ed, keep the sort; if it relied on `file-seq` order, add an explicit `(sort …)` since `fs/glob` order is not the same. Sites that convert `fs/glob`'s `Path` results back to `File` for downstream `.getName` etc. should use `fs/file`/`fs/file-name` rather than reintroducing `io/file`.
- [ ] **Step 3: Run tests** for each owning ns; pay attention to any golden-file test that encodes file iteration order.
- [ ] **Step 4: Commit** — `git commit -am "refactor(fs): use fs/glob for filesystem walks"`

### Task B5: idiom sweep of `.exists`/`.getName`/`.getParent`  — ⚠ DEFERRED (not in executable scope)

> **Deferred per second review:** high-churn Java-interop sweep with no correctness payoff; defer unless bundled with a real change to the same file.

**Files:** the ~19 `.exists`, ~30 `.getName`, ~12 `.getParent` sites (`rg -n '\.(exists|getName|getParentFile|getParent)\b' src`).

- [ ] **Step 1:** Mechanical replacements — `(.exists (io/file x))` → `(fs/exists? x)`; `(.getName f)` → `(fs/file-name f)`; `(.getParentFile f)`/`(.getParent f)` → `(fs/parent f)`; `(.length f)` → `(fs/size f)`. Remove now-dead `[clojure.java.io :as io]` requires where nothing else uses them.
- [ ] **Step 2:** Run the full suite (`bin/kaocha`) — this touches many files. Expected: PASS.
- [ ] **Step 3: Commit** — `git commit -am "refactor(fs): idiomatic fs predicates for path interop"`

> This task is large and low-risk-per-site but high-churn. It may be split per-file or deferred; it does not block any later batch.

---

# Batch C — Deterministic-JSON string helpers

**Why:** `abc.tools.json` exposes only a *file* writer, so four sites reinvent `(charred/write-json-str (prepare-deterministic-json x) :indent-str "  ")` for stdout/JSONL, keeping a direct `charred` import each. A string helper removes the divergence risk in deterministic output.

### Task C1: Add `read-json-str` and `write-deterministic-json-str`

**Files:**
- Modify: `src/abc/tools/json.clj`
- Test: `test/abc/tools/json_test.clj` (extend)

**Interfaces:**
- Produces: `(read-json-str s) -> parsed`; `(write-deterministic-json-str value) -> string` (deterministic sorted-key, 2-space **indented**, trailing-whitespace-trimmed, **no** trailing newline); `(write-deterministic-jsonl-line value) -> string` (deterministic sorted-key, **single-line/no indent** — for JSONL rows). `write-deterministic-json-file!` is refactored to reuse `write-deterministic-json-str` so file and string output stay byte-identical modulo the final newline.

  **Two variants matter:** the stdout emitters (`person_drift_history`, `aozora_history_audit`) use the *indented* form; the JSONL emitters (`workflow/report`, `annotation_join_stats`) use the *non-indented* single-line form. Migrating a JSONL site to the indented helper would change output — hence the separate `write-deterministic-jsonl-line`.

- [ ] **Step 1: Write the failing test**

```clojure
(deftest write-deterministic-json-str-matches-file-writer
  (fs/with-temp-dir [d {}]
    (let [value {"b" 2 "a" [3 1] "nested" {"z" 1 "y" 2}}
          f (fs/file d "out.json")]
      (abc-json/write-deterministic-json-file! f value)
      (is (= (str (abc-json/write-deterministic-json-str value) "\n")
             (slurp f))))))

(deftest read-json-str-round-trips
  (is (= {"a" 1 "b" [2 3]}
         (abc-json/read-json-str (abc-json/write-deterministic-json-str {"a" 1 "b" [2 3]})))))

(deftest jsonl-line-is-single-line-and-sorted
  (let [line (abc-json/write-deterministic-jsonl-line {"b" 2 "a" 1})]
    (is (not (re-find #"\n" line)))
    (is (= {"a" 1 "b" 2} (abc-json/read-json-str line)))))
```

(Add `[babashka.fs :as fs]` and `[abc.tools.json :as abc-json]` to the test ns requires if absent.)

- [ ] **Step 2: Run test to verify it fails** — `bin/kaocha --focus abc.tools.json-test`. Expected: FAIL (`write-deterministic-json-str` unresolved).

- [ ] **Step 3: Implement**

```clojure
(defn read-json-str [s]
  (json/read-json s))

(defn write-deterministic-json-str
  "Deterministic sorted-key, indented, trailing-whitespace-trimmed JSON string with no
  trailing newline. Same normalization as write-deterministic-json-file!."
  [value]
  (string/replace
   (json/write-json-str (prepare-deterministic-json value) :indent-str "  ")
   #"[ \t]+(?=\r?\n)"
   ""))

(defn write-deterministic-jsonl-line
  "Deterministic sorted-key single-line JSON (no indent), for JSONL rows. Matches the
  current charred call at the JSONL emit sites (no :indent-str)."
  [value]
  (json/write-json-str (prepare-deterministic-json value)))

(defn write-deterministic-json-file! [file value]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (.write writer (write-deterministic-json-str value))
    (.write writer "\n"))
  file)
```

- [ ] **Step 4: Run the tests** — `bin/kaocha --focus abc.tools.json-test`. Expected: PASS. Then run every ns that writes deterministic JSON to confirm byte-stability: `bin/kaocha --focus abc.tools.materialize-publication-test --focus abc.tools.manifest-index-test`. Expected: PASS.

- [ ] **Step 5: Commit** — `git add src/abc/tools/json.clj test/abc/tools/json_test.clj && git commit -m "feat(json): add deterministic JSON string helpers"`

### Task C2: Migrate the four charred-direct emit sites

**Files:**
- Modify: `src/abc/tools/person_drift_history.clj:281`, `src/abc/tools/aozora_history_audit.clj:359`, `src/abc/tools/workflow/report.clj:49`, `src/abc/tools/annotation_join_stats.clj:136`

- [ ] **Step 1: Green baseline** for each owning test ns.
- [ ] **Step 2: Transform.** Indented stdout emitters (`person_drift_history.clj:281`, `aozora_history_audit.clj:359`): `(println (json/write-json-str (prepare-deterministic-json result) :indent-str "  "))` → `(println (abc-json/write-deterministic-json-str result))`. JSONL emitters (`workflow/report.clj:49`, `annotation_join_stats.clj:136`) that call `(charred/write-json-str (prepare-deterministic-json x))` **without** `:indent-str`: → `(abc-json/write-deterministic-jsonl-line x)` (defined in Task C1). Do **not** use the indented helper on JSONL rows. Remove the now-unused `[charred.api]` require from each file where nothing else uses it.
- [ ] **Step 3: Run tests** — `bin/kaocha --focus abc.tools.workflow.report-test --focus abc.tools.annotation-join-stats-test --focus abc.tools.person-drift-history-test --focus abc.tools.aozora-history-audit-test`. Expected: PASS (verify byte-identical JSONL against any golden test).
- [ ] **Step 4: Commit** — `git commit -am "refactor(json): route deterministic JSON emission through abc.tools.json"`

---

# Batch D — Shared path/EDN helpers

**Why:** three near-duplicate path-relativization helpers and five `edn/read-string (slurp …)` copies. The EDN readers are a clean swap; the `relative-path` copies are **NOT identical** — treat them individually (see Step 1).

### Task D1: Migrate `relative-path` and `read-edn` call sites

**Files:**
- Modify: `src/abc/tools/soranoha.clj` (~213-217, both `relative-path` and `normalized-relative-path`), `src/abc/tools/source_snapshot_workset.clj` (~61), `src/abc/tools/materialize_source_snapshot.clj` (~34); and EDN readers `src/abc/tools/aat_parser_ir_compat.clj` (~201), `src/abc/tools/parser_evidence.clj` (~74), `src/abc/tools/diagram/architecture_graph.clj` (~13), `src/abc/tools/diagram/adr_graph.clj` (~74)

**Interfaces:**
- Consumes: `abc.tools.files/{relative-path,read-edn}` (Task B1).

- [ ] **Step 1: The three `relative-path` locals are semantically DISTINCT — do not blind-swap.** Verified differences:
  - `soranoha.clj:213` `relative-path` = **raw** `.relativize`, *no* slash-normalization (only the separate `normalized-relative-path:217` normalizes). `files/relative-path` *does* normalize, so replacing the raw one changes its call sites (~223, ~628). Map `normalized-relative-path` → `files/relative-path`; leave/verify the raw `relative-path` sites, or give them a raw variant.
  - `source_snapshot_workset.clj:61` **canonicalizes both** paths (`canonical-file`) before relativizing. Wrap: `(files/relative-path (fs/canonicalize base) (fs/canonicalize file))`.
  - `materialize_source_snapshot.clj:34` uses the from-file's **canonical parent** as the base and canonicalizes the target — a different base entirely. Wrap accordingly; do not assume `files/relative-path`'s base.
  Write a characterization assertion (input → exact output string) for each before touching it. `files/relative-path` is a shared *primitive*; the canonicalization/base-selection stays at each call site.
- [ ] **Step 2: Apply** the per-site wrappings above (not a single shared `relative-path` defn); replace each `(edn/read-string (slurp (io/file x)))` with `(files/read-edn x)` (this part IS a clean swap). Remove now-dead `clojure.edn`/`clojure.java.io` requires.
- [ ] **Step 3: Run tests** — `bin/kaocha --focus abc.tools.soranoha-test --focus abc.tools.source-snapshot-workset-test --focus abc.tools.materialize-source-snapshot-test --focus abc.tools.parser-evidence-test --focus abc.tools.diagram.architecture-graph-test --focus abc.tools.diagram.adr-graph-test`. Expected: PASS.
- [ ] **Step 4: Commit** — `git commit -am "refactor(files): centralize relative-path and read-edn helpers"`

---

# Batch E — Behavior-preserving idiom & dead-code cleanups

**Why:** two substantive dead-code/duplication items plus small idiom slips. All behavior-preserving; each is guarded by a characterization test or existing golden test.

### Task E1: Remove the 14 dead 2-arity render wrappers in `parser_ir_tei`

**Files:**
- Modify: `src/abc/tools/parser_ir_tei.clj` (~175-326)
- Test: `test/abc/tools/parser_ir_tei_test.clj`

**Rationale (verified):** each `render-*-node` has `([acc node] (render-x acc node 0))`, but renderers are only dispatched 3-arity via `node-renderers` (line 349); `render-node`/`render-node-seq` always supply depth. The 2-arity arities are unreachable.

- [ ] **Step 1: Add a characterization test** that renders a representative parser-IR document through the public entry (`render-node-seq`/the top-level render fn) and asserts the exact output. Run it green *before* the change so it pins current behavior.

  Run: `bin/kaocha --focus abc.tools.parser-ir-tei-test`. Expected: PASS.
- [ ] **Step 2: Delete the `([acc node] (render-x acc node 0))` arity** from each renderer, leaving the single 3-arity (or 2-arity `[acc node]` for renderers ignoring depth — but since dispatch always passes depth, keep them 3-arity with `_depth`). Confirm via `rg -n 'render-[a-z-]+-node ' src/abc/tools/parser_ir_tei.clj` that no 2-arg call to these renderers exists outside their own now-deleted wrappers.
- [ ] **Step 3: Run tests** — `bin/kaocha --focus abc.tools.parser-ir-tei-test`. Expected: PASS (byte-identical output).
- [ ] **Step 4: Commit** — `git commit -am "refactor(tei): drop unreachable 2-arity render wrappers"`

### Task E2: De-boilerplate `validate-json-schemas!`  — ⚠ DEFERRED (not in executable scope)

> **Deferred per second review:** ~24 bindings, nearly all consumed downstream, so the refactor must re-extract them all via `(get schemas …)` — low payoff, error-prone. Do only if the duplicated path list is actively causing drift.

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj` (~322-371)
- Test: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Green baseline** — `bin/kaocha --focus abc.tools.validate-design-bundle-test`. Expected: PASS.

  **Scope reality-check (verified):** this is **24** schema bindings (not 26), the filenames are `schemas/*.schema.json` (not `schemas/manifest.json`), and **nearly all 24** are individually consumed *after* the `doseq` by `validate-json!`/`validate-json-lines!` with per-schema paths and options (e.g. `:require-nonempty`). So the win is smaller than "pull a handful out of a map" — the refactor removes the duplicated `(files/read-json …)` calls and the parallel path list, but must re-extract ~all 24 downstream via `(get schemas …)`. **This is a modest, error-prone cleanup — treat it as low priority and consider deferring** unless the duplicated path list is actively causing drift.
- [ ] **Step 2 (if pursued): Build the schema map once, extract downstream via `get`:**

```clojure
(let [schema-paths ["schemas/manifest.schema.json" "schemas/parser-ir.schema.json" …]  ; the actual 24 paths
      schemas      (into {} (map (juxt identity files/read-json)) schema-paths)]
  (doseq [[path schema] schemas]
    (schema-valid! schema path))
  (let [manifest-schema  (get schemas "schemas/manifest.schema.json")
        parser-ir-schema (get schemas "schemas/parser-ir.schema.json")
        … ; re-extract every binding the rest of the fn uses
        ]
    …))
```

Preserve the exact set of paths and every downstream binding. Do not change validation order if a test asserts error ordering.

- [ ] **Step 3: Run tests** — `bin/kaocha --focus abc.tools.validate-design-bundle-test`. Expected: PASS.
- [ ] **Step 4: Commit** — `git commit -am "refactor(validate): data-drive JSON-schema validation loop"`

### Task E3: Small idiom fixes — split by owning namespace, do opportunistically

> **Per second review:** these are unrelated micro-fixes across four namespaces; do NOT batch them into one commit. Commit each owning namespace separately (or fold each into unrelated work that already touches that file). Lowest priority.

**Files:** `src/abc/tools/materialize_publication.clj:190` (`cond->` literal `true`), `src/abc/tools/validate_design_bundle.clj:105` (`(when (not …))`→`when-not`) and `:308` (side-effect `_`-binding → `doto`), `src/abc/tools/parser_ir_tei.clj:91` (`layout-params` computed twice in one `cond->` → `let`-bind), `src/abc/tools/soranoha.clj` (extract `print-snapshot-summary!` from the six command fns at ~573/755/782/801/811/828; replace the eta lambda `:run (fn [] (list-request-sets!))` at ~1168 with the bare var `:run list-request-sets!`. **Note:** the `~1199` `annotation-join-stats` entry is a genuine 3-arg arg-adapter, *not* an eta lambda — leave it.).

- [ ] **Step 1: Green baseline** for `abc.tools.materialize-publication-test`, `abc.tools.validate-design-bundle-test`, `abc.tools.parser-ir-tei-test`, `abc.tools.soranoha-test`.
- [ ] **Step 2: Apply each fix.** For the `cond->` literal-`true` at `materialize_publication.clj:190`, move the `metadata_record_hash` assoc out of the `cond->` as an unconditional step (thread with `->` or `assoc` before the `cond->`), leaving only the genuinely conditional `tei-profile-hash` branch. For the `print-snapshot-summary!` extraction, add `(defn- print-snapshot-summary! [snapshot] (println …snapshot_label…) (println …snapshot_identity_hash…) (println …request_set_label…))` reproducing the exact three lines and string keys, and call it from all six sites.
- [ ] **Step 3: Run those four test namespaces.** Expected: PASS (identical stdout/behavior).
- [ ] **Step 4: Commit** — `git commit -am "refactor: idiomatic cleanups (cond->, when-not, doto, shared summary printer)"`

---

# Batch F — Test hygiene

**Why:** `temp-dir` is copy-pasted in 8 files and recursive `delete-tree!` in 20+, and cleanup is overwhelmingly placed *after* the test body, so a failing `is` orphans temp trees. Executable scope is **targeted leaks only** — a shared helper (F1) plus fixing the genuine leak outliers (F2).

> **Corrected inventory (verified):** `diagram/core_test` is **already fixed** (it uses `File/createTempFile` + `try/finally deleteIfExists` — the diagram follow-ups landed it); it is **removed** from this batch. Re-enumerate remaining leaks freshly before executing — do not trust the 2026-07-10 review's list verbatim.

### Task F1: Add a JVM-only `test_fs.clj` `with-temp-dir` helper

**Files:**
- Create: `test/abc/test_fs.clj` (JVM-only — **not** `.cljc`)

**Interfaces:**
- Produces in `abc.test-fs`: `with-temp-dir` (wraps `babashka.fs/with-temp-dir`, exception-safe cleanup for free).

**Decision (resolved per second review):** put the helper in a **new JVM-only `test/abc/test_fs.clj`**, not in `test_utils.cljc`. `test_utils.cljc` is deliberately `.cljc`; adding JVM-only `babashka.fs` to it would make it non-loadable under cljs. Leave `test_utils.cljc` (its `schema-valid`/`schema-validate`) untouched.

- [ ] **Step 1: Create the helper**

```clojure
(ns abc.test-fs
  (:require [babashka.fs :as fs]))

(defmacro with-temp-dir
  "Run body with `binding` bound to a fresh temp dir (File); deletes the
  tree afterward even on exception. Prefer over ad-hoc createTempDirectory."
  [[binding] & body]
  `(fs/with-temp-dir [d# {}]
     (let [~binding (fs/file d#)]
       ~@body)))
```

- [ ] **Step 2: Migrate 2–3 representative files** (`materialize_import_test.clj`, `person_drift_history_test.clj` — **not** `workflow/report_test.clj`, which F2 owns): replace `(let [dir (temp-dir …)] …assertions… (doseq [f …] (.delete f)))` with `(tfs/with-temp-dir [dir] …assertions…)` (`[abc.test-fs :as tfs]`). Delete the now-unused local `temp-dir`/`delete-tree!` defns and their `java.nio`/`java.io` imports.
- [ ] **Step 3:** Run the migrated namespaces; confirm green. Migrate the rest in small groups (3–5 files/commit).
- [ ] **Step 4: Commit** (per group) — `git commit -am "test: adopt JVM-only exception-safe with-temp-dir helper"`

### Task F2: Fix leaky test outliers (freshly enumerated)

**Files (re-verify each still leaks before touching):**
- Modify: `test/abc/tools/linked_art_test.clj:16`, `test/abc/tools/workflow/report_test.clj:71,101`, `test/abc/tools/acceptance_criteria_lint_test.clj:29,51,75`, `test/abc/tools/facts_test.clj:19-24` (non-exception-safe fixture). **`diagram/core_test` is NOT here — already fixed.** `report_test` is owned here (not F1) to avoid double-migration.

- [ ] **Step 1: Re-enumerate** — `rg -n 'createTempDirectory|File/createTempFile' test`, and for each confirm cleanup is missing or outside a `finally`. Only fix the ones that genuinely leak.
- [ ] **Step 2:** Wrap leaky create-and-abandon temps in `tfs/with-temp-dir`. For `facts_test.clj`, move teardown into `try …(finally …)` so a throwing test still cleans, and delete the whole tree rather than three hard-coded filenames.
- [ ] **Step 3:** Run each touched namespace; confirm green. Run the full suite once to confirm no ordering coupling was relied upon: `bin/kaocha`.
- [ ] **Step 4: Commit** — `git commit -am "test: fix leaky temp-dir outliers with exception-safe cleanup"`

### Task F3: Convert table-shaped tests to `are`  — ⚠ DEFERRED (not in executable scope)

> **Deferred per second review:** purely cosmetic. Not scheduled.

---

# Batch G — malli  ⚠ REMOVED FROM EXECUTABLE SCOPE (needs a reconciliation audit first)

**Why removed:** the second review verified that much of `docs/superpowers/plans/2026-04-28-malli-integration-improvements.md` **has already landed** — `design-bundle-schemas`, `cached-schema`/`cached-schema-hash`, `humanize-validation-errors`, `explanation-messages`, `explain-or-throw!`, and `set-default-registry!`+`instrument!` all exist in `src/abc/tools/malli.clj`. The draft's Task G1 invented `::metadata-record-input`/`::metadata-record` registry keys that **do not exist**, leaving the core contract undecided. Shipping G as written would design instrumentation against a moving, partly-implemented target.

**Replace with — Task G0: malli reconciliation audit (design task, no code yet).**
- [ ] Diff the 2026-04-28 plan against the current `abc.tools.malli` (+ `abc.annotation.schema`): mark each item **landed / partial / not-started**.
- [ ] Establish ground truth on the two residual gaps the 2026-07-10 review found and re-confirm they still hold: (a) **zero** instrumented `m/=>`/`mx/defn` contracts (so `mi/instrument!` is a no-op), and (b) `:gen/elements` are single constants (so generator-backed tests are tautological).
- [ ] Only then design the minimum contract set — starting from **schemas that already exist** — and decide the input/output schema names for the record builders (do not invent keys in the plan). Produce a fresh, small, self-contained plan from that audit.

The former G1/G2/G3 write-ups are intentionally dropped; re-derive them from the audit so they reference real schema keys.

---

## Self-review checklist (v2, after second review)

1. **Coverage vs the original review's recommendations:** shared CLI → Batch A; fs adopt → Batch B; deterministic-JSON string helpers → Batch C; path/EDN dedup → Batch D; behavior-preserving cleanups → Batch E (E1 in scope; E2/E3 deferred/opportunistic); test hygiene → Batch F. **malli → deferred to a reconciliation audit (Batch G removed).** The executable scope is the numbered sequence in "Executable scope & sequencing".
2. **Dependencies are explicit and non-circular:** C1 → B1; F1 → F2; A1 → A-pilot → family migrations. The old "A → B → C → D" order (which put B1 before its C1 dependency) is superseded.
3. **Placeholders:** no "TBD"/"handle edge cases". Deferred tasks are explicitly banner-marked, not silently dropped.
4. **Type/name consistency:** `strip-double-dash`/`parse`/`dispatch!`/`run-cli!`, `read-json-str`/`write-deterministic-json-str`/`write-deterministic-jsonl-line`, `files/relative-path`/`files/read-edn`, `abc.test-fs/with-temp-dir` are used with the same names in producing and consuming tasks.
4. **Known open risks flagged inline:** `fs/glob` ordering vs `file-seq`, JSONL non-indented variant, `person_drift_history` fail-condition reading options not result, `aat_parser_ir_compat` bespoke exit codes + missing test.

## Adversarial-review corrections (applied after a verification pass against the codebase)

This plan was reviewed against the real code; the following defects were found and fixed in-place:
- **`--focus` invocation:** kaocha `--focus` takes one value; all multi-ns commands now repeat the flag.
- **Positional-arg tools:** `materialize_import` and `manifest_to_rdf` validate positional `:arguments` (with a too-many-args guard), not options. The `abc.tools.cli` harness now supports `:min-args`/`:max-args` and passes `{:options :arguments}` to `:run`; A2 migrates these two accordingly.
- **`validate_corpus` success logging** lives in `-main`, not the worker — A2's `:run` now reproduces it. (Same discipline noted for all migrations.)
- **`copy-file!` NPE:** guarded `(fs/parent target)` against `nil` for bare-filename targets, with a regression test.
- **`relative-path` locals are NOT identical** (raw vs both-canonicalized vs canonical-parent-base) — D1 now treats them as three per-site wrappings over a shared primitive, not one swap.
- **`validate-json-schemas!`** is 24 (not 26) bindings, `*.schema.json` filenames, most consumed downstream — E2 downgraded to low-priority with corrected framing.
- **`write-deterministic-jsonl-line`** (non-indented) added to C1 so C2 has a real function to call.
- **`test_utils.cljc`** is appended to (not replaced) so `schema-valid`/`schema-validate` survive; `.cljc`/cljs caveat noted.
- **`aat_parser_ir_compat` has no test ns** — A3/G2 no longer assume one; a characterization step precedes its migration.
- Reference nits fixed: soranoha eta lambda only at ~1168; `when-not` at :105; `fs/delete-tree` does not throw on missing paths (guard kept for intent).

## Round-2 corrections (second review, verified against the code)

- **Sequencing was self-contradictory** → added the authoritative "Executable scope & sequencing" section (C → B → D → A-pilot → F → E1); explicit deps C1→B1, F1→F2. The old "A → B → C → D" order (B1 before its C1 dependency) is removed.
- **Batch G removed from executable scope** — much of the 2026-04 malli plan already landed (`design-bundle-schemas`, `cached-schema`, `humanize-validation-errors`, `explain-or-throw!`, …) and G1 referenced non-existent `::metadata-record*` keys. Replaced with a reconciliation-audit design task (G0).
- **Batch A is observable behavior change** → mandated a single-CLI **subprocess-characterization** pilot (stdout/stderr/exit) before family migrations; renamed side-effecting `run-cli` → **`run-cli!`**.
- **B3 is a correctness change** (throw vs ignored `.mkdirs false`) → per-component commits + failure-path tests + full 18-site enumeration. **B4 (`fs/glob`) and B5 (interop sweep) deferred** — high churn, no defect.
- **`diagram/core_test` already fixed** (uses `createTempFile`+`finally`) → removed from Batch F. `report_test` de-duplicated (owned by F2). Helper moved to a new **JVM-only `test/abc/test_fs.clj`** (not `.cljc`).
- **Terminology:** the `abc.tools.json` helpers produce **deterministic sorted-key** JSON, not RFC 8785/JCS "canonical" (that's `abc.tools.jcs`) — docstrings/interfaces corrected.
- **Deferred as low-value:** E2 (`validate-json-schemas!`), E3 (split per-namespace, opportunistic), F3 (`are`).
