# ADR Evidence Capture Protocol Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the interim focused/Nix capture conflation with closed focused-v3 and operational-v1 protocols before foundation evidence is captured.

**Architecture:** Focused evidence retains exact reachable-Var capability analysis and runtime read tracing. Operational evidence instead binds a reviewed command contract, a recomputed entrypoint-namespace closure, and an exact determinant set. Observation identity, claim bindings, migration debt, and filesystem publication are separate values with separate owners.

**Tech Stack:** Clojure 1.12, `tools.reader`, clj-kondo analysis, `babashka.fs`, `babashka.process`, `babashka.cli`, Kaocha/clojure.test, EDN, RFC 8785 JCS SHA-256, Java NIO, Nix flakes.

## Global Constraints

- Implement `../specs/2026-07-13-adr-evidence-capture-protocol-hardening-design.md` exactly.
- Governance remains in `audit` mode. This plan creates no run bundle, registry entry, ADR promotion, or enforcement switch.
- Preserve the closed accepted/rejected shapes of descriptor versions 1 and 2.
- Delete `:abc-adr-nix-clojure-closure-v1`; do not retain an alias or second accepted closure shape.
- One event has one observation ID, descriptor stem, observation key, and bundle. Claim bindings do not create observation identity.
- Generic run-bundle `repo-files-v1` remains minimum-plus-extras. Exactness belongs only to operational descriptor mode `exact-v1`.
- Focused `component-clojure-test-v1` preserves workspace-relative input keys (`abc/...`, `ab-validator/...`); descriptor/catalog coordinates and operational determinants remain repo-root-relative.
- Validate and canonicalize every path coordinate before reading bytes.
- Keep `abc.tools.json/write-deterministic-json-file!` unchanged. Evidence publication uses a new exclusive writer.
- Use `babashka.fs` for path/temp-directory operations, `babashka.process` for subprocesses, and `abc.tools.cli/run-cli!` for CLI behavior.
- Every source task follows RED → GREEN → focused checks → Clojure/Nix checks → commit. No intentionally red intermediate commit is allowed.
- Task 8 is the only atomic debt-to-zero transition; do not partially land its catalog/test/descriptor set.

---

## File and interface map

- Modify `src/abc/tools/adr_evidence_runtime_inputs.clj`: alias-aware reading, exact capability policy, and retirement of the focused-Var Nix closure API.
- Modify `src/abc/tools/evidence_io.clj`: workspace-aware traces plus lifecycle ownership around existing ephemeral authorization.
- Create `src/abc/tools/adr_evidence_observation_catalog.clj`: closed catalog validation, observation-contract JCS projection/hash, binding joins, and normalized conformance-debt identities.
- Create `src/abc/tools/adr_evidence_operational.clj`: entrypoint namespace resolution, operational manifest validation, descriptor v3/operational-v1 policy, exact-v1 inputs, and offline policy validation.
- Create `src/abc/tools/evidence_output.clj`: validated destination value and exclusive evidence publication.
- Modify `src/abc/tools/adr_evidence_capture.clj`: three-root orchestration and explicit descriptor-version dispatch.
- Modify `src/abc/tools/adr_evidence_bundle.clj`: invoke offline policy validation after generic bundle validation.
- Create `data/adr-evidence/foundation-observation-catalog.edn`: 35 focused observations plus two operational observations.
- Create `data/adr-evidence/foundation-capture-conformance-debt.edn`: exact normalized Task 6C admission debt.
- Tests mirror each source namespace under `test/abc/tools/`.

### Task 1: Retire the interim Nix/focused closure contract

**Files:**
- Modify: `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- Modify: `abc/src/abc/tools/adr_evidence_capture.clj`
- Modify: `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`

**Interfaces:**
- Removes: `derive-nix-clojure-source-closure`, `validate-nix-clojure-closure!`, and `:abc-adr-nix-clojure-closure-v1`.
- Produces: focused runner problem kind `:invalid-focused-evidence-runner` and focused analysis execution problem kind `:invalid-focused-evidence-analysis`.

- [ ] **Step 1: Change tests to reject the retired protocol and pin the new problem identities**

Delete `nix-clojure-closure-manifest-binds-the-derived-graph-test`. Add assertions that the namespace no longer resolves the two public Vars and that descriptor runner/summary failures report the focused runner kind:

```clojure
(deftest retired-nix-clojure-closure-is-not-an-accepted-protocol-test
  (is (nil? (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                        'derive-nix-clojure-source-closure)))
  (is (nil? (ns-resolve 'abc.tools.adr-evidence-runtime-inputs
                        'validate-nix-clojure-closure!))))

(deftest focused-runner-failures-have-a-focused-problem-kind-test
  (is (= :invalid-focused-evidence-runner
         (problem-kind #(capture/capture! invalid-runner-options)))))
```

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test --focus abc.tools.adr-evidence-capture-test`

Expected: FAIL because the old Vars still resolve and capture still emits `:invalid-nix-clojure-closure`.

- [ ] **Step 3: Delete the old functions and rename every old problem use**

Delete the definitions at the end of `adr_evidence_runtime_inputs.clj`. Change the runner, executable, and Kaocha-summary failures in capture to `:invalid-focused-evidence-runner`. Change clj-kondo subprocess/analysis failure to `:invalid-focused-evidence-analysis`. Require this source invariant:

```bash
! rg 'abc-adr-nix-clojure-closure-v1|invalid-nix-clojure-closure|derive-nix-clojure-source-closure|validate-nix-clojure-closure!' \
  src test
```

- [ ] **Step 4: Run GREEN and quality checks**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test \
  --focus abc.tools.adr-evidence-capture-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: all commands exit 0; the retired identifiers have zero source/test matches.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_runtime_inputs.clj \
  abc/src/abc/tools/adr_evidence_capture.clj \
  abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj
git commit -m "refactor(abc): retire focused nix closure protocol"
```

### Task 2: Make source reading alias-aware and admit only exact pure capabilities

**Files:**
- Modify: `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- Modify: `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`

**Interfaces:**
- Produces: invocation-local source reading that resolves `::alias/name` without creating namespaces or mutating `*ns*`.
- Produces: exact reviewed pure Vars `charred.api/write-json-str` and `clojure.core/re-pattern`; neighboring executable Vars remain forbidden.

- [ ] **Step 1: Add failing alias, concurrency, and capability tests**

Use a temporary namespace form containing `[abc.sim.artifact-manifest :as am]` and a later `::am/run-summary-events`. Assert repeated futures return identical forms and leave `(find-ns 'fixture.alias-test)` nil. Add reachable graphs using `write-json-str` and `re-pattern`, plus negative neighbors `charred.api/read-json` and `clojure.core/load-string`.

```clojure
(deftest source-reader-resolves-aliases-without-global-namespace-state-test
  (let [results (->> (range 8)
                     (mapv (fn [_] (future (runtime/read-source-forms! fixture))))
                     (mapv deref))]
    (is (apply = results))
    (is (nil? (find-ns 'fixture.alias-test)))))
```

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test`

Expected: FAIL on the auto-resolved alias and both unreviewed pure Vars.

- [ ] **Step 3: Implement the invocation-local resolver**

Read the `ns` form with `*read-eval* false`, derive a plain alias map from its `:require` libspecs, and pass a `tools.reader` resolver value while reading later forms. Do not call `create-ns`, `alias`, `in-ns`, or bind `*ns*` to a created namespace. Preserve reader line/column metadata. Add only these two symbols to the existing `audited-safe-external-vars` set literal:

```clojure
charred.api/write-json-str
clojure.core/re-pattern
```

- [ ] **Step 4: Run GREEN, concurrent repetition, and lint**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test
bin/kaocha --focus abc.tools.adr-evidence-runtime-inputs-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: both test runs and lint exit 0.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_runtime_inputs.clj \
  abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj
git commit -m "feat(abc): resolve evidence source aliases locally"
```

### Task 3: Add the generic observation catalog, JCS contract identity, binding join, and stable debt identity

**Files:**
- Create: `abc/src/abc/tools/adr_evidence_observation_catalog.clj`
- Create: `abc/test/abc/tools/adr_evidence_observation_catalog_test.clj`
- Create: `abc/data/adr-evidence/foundation-observation-catalog.edn`
- Create: `abc/data/adr-evidence/foundation-capture-conformance-debt.edn`
- Modify: `abc/test/abc/tools/adr_evidence_register_test.clj`

**Interfaces:**
- Produces: `(catalog-problems repo-root catalog) -> vector<problem>`.
- Produces: `(load-catalog! repo-root catalog-path) -> closed-catalog`.
- Produces: `(observation-contract-json-value row) -> I-JSON-value`.
- Produces: `(observation-contract-sha256 row) -> "sha256:<64 lowercase hex>"`.
- Produces: `(validate-bindings catalog template) -> vector<problem>`.
- Produces: `(normalize-conformance-findings findings) -> sorted-vector<closed semantic identity>`.

- [ ] **Step 1: Write failing closed-shape, known-answer, join, and debt tests**

Pin one focused row and one operational row under schema version
`:abc-adr-evidence-observation-catalog-v1`. Test unknown/extra keys,
duplicate IDs/stems/keys, unsorted collections, unknown/unbound observations,
duplicate `[claim-id observation-id]`, and JCS order independence. Prove the
generic validator accepts a non-foundation catalog with different row counts;
then test the foundation-only 37/42 arithmetic in a separate fixture. The
known-answer test must assert exact JSON bytes and hash:

```clojure
(let [a (catalog/observation-contract-json-value focused-row)
      b (catalog/observation-contract-json-value
         (into (array-map) (reverse focused-row)))]
  (is (= a b))
  (is (= (jcs/rfc8785-string-domain-json-bytes a)
         (jcs/rfc8785-string-domain-json-bytes b)))
  (is (re-matches #"sha256:[0-9a-f]{64}"
                  (catalog/observation-contract-sha256 focused-row))))
```

The test fixture stores literal expected canonical UTF-8 JSON and a literal
`sha256:` digest computed independently from those bytes. It must not obtain
either expected value by calling `observation-contract-json-value`, the JCS
writer, or the hash function under test.

Assert normalized debt ignores message/line/column but changes when `:kind`, `:focus-var`, `:target`, or normalized path changes.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test --focus abc.tools.adr-evidence-register-test`

Expected: ERROR because the new namespace does not exist.

- [ ] **Step 3: Implement the closed catalog and RFC 8785 projection**

Use `files/read-edn` only after a contained coordinate is supplied. Convert field-by-field to JSON strings; never hash `pr-str` or generic EDN. Compute the digest exactly as:

```clojure
(defn observation-contract-sha256 [row]
  (-> row
      observation-contract-json-value
      jcs/rfc8785-string-domain-json-bytes
      hash/sha256-bytes
      hash/format-sha256))
```

Problem functions accumulate and sort problems; they do not throw or exit. `load-catalog!` may throw only after attaching the full problem vector.

- [ ] **Step 4: Check in the transitional catalog and normalized debt**

Transcribe the amended foundation table into 35 unique focused rows and two operational rows. Collapse exactly these repeated focuses to one observation each:

- `generated-import-manifest-schema-conformance-test`;
- `validate-design-bundle-wrapper-delegation-test`;
- `diagnostic-schema-hash-requires-the-exact-current-contract-test`.

Collapse the three design-bundle claim rows to observation ID `:design-bundle-operational`; keep `:source-bundle-corpus` separate. Record the complete normalized Task 6C findings in the debt file; messages and source positions are forbidden keys.

- [ ] **Step 5: Run GREEN and verify arithmetic**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-register-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: 35 focused observations, two operational observations, successful synthetic binding joins, and exact equality with the transitional debt value.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_observation_catalog.clj \
  abc/test/abc/tools/adr_evidence_observation_catalog_test.clj \
  abc/test/abc/tools/adr_evidence_register_test.clj \
  abc/data/adr-evidence/foundation-observation-catalog.edn \
  abc/data/adr-evidence/foundation-capture-conformance-debt.edn
git commit -m "feat(abc): define typed evidence observation catalogs"
```

### Task 4: Add workspace-aware ownership around existing ephemeral authorization

**Files:**
- Modify: `abc/src/abc/tools/evidence_io.clj`
- Modify: `abc/src/abc/tools/adr_evidence_runtime_inputs.clj`
- Modify: `abc/test/abc/tools/evidence_io_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj`

**Interfaces:**
- Preserves: `(with-ephemeral-root temp-root thunk)` as scoped authorization only.
- Extends: `(with-read-trace {:identity-root identity-root :cwd-root cwd-root :workspace-root workspace-root} thunk)`; workspace defaults to identity root.
- Produces: `(with-owned-ephemeral-root thunk) -> callback-value`; callback receives the generated root.

- [ ] **Step 1: Write failing ownership and cleanup tests**

Assert the existing helper still rejects use without a trace and identity overlap. Assert the new owner supplies an existing directory outside identity/workspace, records rereads as ephemeral, deletes it after success and exception, and cannot be invoked outside a trace.

```clojure
(deftest owned-ephemeral-root-cleans-up-after-throw-test
  (let [seen (atom nil)]
    (is (thrown? Exception
                 (evidence-io/with-read-trace roots
                   #(evidence-io/with-owned-ephemeral-root
                      (fn [root] (reset! seen root) (throw (Exception.)))))))
    (is (false? (fs/exists? @seen)))))
```

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.evidence-io-test --focus abc.tools.adr-evidence-runtime-inputs-test`

Expected: FAIL because `with-owned-ephemeral-root` and workspace trace identity do not exist.

- [ ] **Step 3: Implement ownership with `babashka.fs`**

Extend the trace value with canonical workspace identity. Implement the new owner using `fs/with-temp-dir`, validate non-overlap with both roots, then delegate to the existing authorization helper:

```clojure
(defn with-owned-ephemeral-root [thunk]
  (when-not *read-trace*
    (throw (ex-info "owned ephemeral roots require an active read trace"
                    {:kind :invalid-ephemeral-root})))
  (fs/with-temp-dir [root {:prefix "abc-evidence-"}]
    (validate-external-root! *read-trace* root)
    (with-ephemeral-root root #(thunk root))))
```

Register only this exact Var with callback position `#{0}` in the higher-order signature table and trusted adapter set.

- [ ] **Step 4: Run GREEN and lint**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.evidence-io-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: tests and lint exit 0.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/evidence_io.clj \
  abc/src/abc/tools/adr_evidence_runtime_inputs.clj \
  abc/test/abc/tools/evidence_io_test.clj \
  abc/test/abc/tools/adr_evidence_runtime_inputs_test.clj
git commit -m "feat(abc): own evidence ephemeral directory lifecycle"
```

### Task 5: Add an evidence-only validated destination and exclusive writer

**Files:**
- Create: `abc/src/abc/tools/evidence_output.clj`
- Create: `abc/test/abc/tools/evidence_output_test.clj`

**Interfaces:**
- Produces: `(validated-destination {:repo-root :workspace-root :staging-root :output}) -> destination-value`.
- Produces: `(write-json-exclusive! destination value) -> java.io.File`.

- [ ] **Step 1: Write failing containment, collision, partial-write, and cleanup tests**

Cover repository/workspace/staging overlap, indirect descendants, existing
output, symlink escape, serialization failure, forced sibling collision,
unsupported hard-link publication, success, and exclusive second-write
rejection. Assert the pure function creates no file and no unsupported-link
case falls back to a nonexclusive move.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.evidence-output-test`

Expected: ERROR because the namespace is absent.

- [ ] **Step 3: Implement the pure boundary and Java NIO transition**

Use `path-containment` and canonical `babashka.fs` paths in the pure boundary. Require the output parent to equal the staging root. In the writer use `FileChannel/open` with `CREATE_NEW` and `WRITE`, write deterministic JSON bytes, call `FileChannel.force(true)`, then publish with `Files/createLink(output, temp)`. This same-directory operation is atomic and fails when the destination exists; unsupported links fail closed. Delete the sibling in `finally`. Do not use `ATOMIC_MOVE`, whose destination-exists behavior is provider-specific. Never call or modify the shared JSON file writer.

```clojure
(defrecord ValidatedDestination [staging-root output])

(defn write-json-exclusive! [^ValidatedDestination destination value]
  (let [output (fs/path (:output destination))
        parent (fs/parent output)
        bytes (.getBytes (json/write-deterministic-json-str value)
                         StandardCharsets/UTF_8)
        temp (loop []
               (let [candidate (fs/path parent
                                        (str "." (fs/file-name output) "."
                                             (UUID/randomUUID) ".tmp"))
                     created (try
                               (with-open [channel (FileChannel/open
                                                    candidate
                                                    (into-array OpenOption
                                                                [StandardOpenOption/CREATE_NEW
                                                                 StandardOpenOption/WRITE]))]
                                 (let [buffer (ByteBuffer/wrap bytes)]
                                   (while (.hasRemaining buffer)
                                     (.write channel buffer)))
                                 (.force channel true))
                               candidate
                               (catch FileAlreadyExistsException _ nil))]
                 (if created created (recur))))]
    (try
      (Files/createLink output temp)
      (fs/file output)
      (finally
        (when (fs/exists? temp)
          (fs/delete temp))))))
```

- [ ] **Step 4: Run GREEN and confirm the shared writer is untouched**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.evidence-output-test
cd ..
git diff --exit-code 8e00eecf -- abc/src/abc/tools/json.clj
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: tests and lint pass; `json.clj` has no diff.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/evidence_output.clj \
  abc/test/abc/tools/evidence_output_test.clj
git commit -m "feat(abc): publish evidence bundles exclusively"
```

### Task 6: Implement operational namespace closure and closed descriptor policy

**Files:**
- Create: `abc/src/abc/tools/adr_evidence_operational.clj`
- Create: `abc/test/abc/tools/adr_evidence_operational_test.clj`
- Modify: `abc/src/abc/tools/adr_evidence_observation_catalog.clj`

**Interfaces:**
- Produces: `(derive-namespace-closure repo-root entrypoint-namespaces) -> sorted-vector<repo-relative path>`.
- Produces: `(load-descriptor-context! {:repo-root :descriptor-path}) -> context` for focused-v3 or operational-v1.
- Produces: `(validate-operational-manifest! context) -> context-with-required-inputs`.
- Produces: `(offline-policy-problems {:repo-root :workspace-root :artifact-path :bundle}) -> accumulated problems`.

- [ ] **Step 1: Write failing resolver and closed descriptor tests**

Use synthetic trees covering `src`/`test`, `.clj`/`.cljc`, duplicate candidates, prefix libspecs, missing `abc.*`, external namespaces, dynamic/in-body require, absolute/traversal/symlink paths, unsorted manifests, catalog mismatch, wrong JCS hash, and extra/missing exact-v1 inputs. Pin segment-prefix behavior: `abc.foo` is owned; `abcdef.foo` is external.

Assert v1/v2 reject catalog fields, focused-v3 requires runtime manifest plus
catalog coordinates/hash, and operational-v1 requires
`:input-set-mode "exact-v1"` plus a closed entrypoint-kind branch. The
`:clojure` branch requires a closure manifest and nonempty entrypoints; the
`:nix-only` branch forbids both and derives exactness only from reviewed
determinants. Add a pure-Nix fixture matching `tei-profile-drift` and prove no
namespace is invented.

- [ ] **Step 2: Run RED**

Run: `cd abc && bin/kaocha --focus abc.tools.adr-evidence-operational-test`

Expected: ERROR because the operational namespace is absent.

- [ ] **Step 3: Implement deterministic namespace resolution**

For each `:clojure` namespace examine all four candidates from
`[src test] × [.clj .cljc]`. Validate each candidate coordinate before
reading. Exactly one owned-prefix candidate is required; multiple candidates
fail regardless of enumeration order. Read only the `ns` form with
`*read-eval* false`, expand prefix libspecs, and recur over local requirements.
External dependencies are represented by `deps.edn` and `deps-lock.json`.
The `:nix-only` branch never calls the namespace resolver.

- [ ] **Step 4: Implement descriptor and exact-input policy**

Validate closed key sets by exact descriptor version and entrypoint kind. For
`:clojure`, derive the operational required set as:

```clojure
(into (sorted-set)
      (concat [descriptor-path closure-manifest-path
               "deps.edn" "deps-lock.json" "tests.edn"]
              namespace-closure
              (:determinant-paths catalog-row)))
```

Generic bundle `repo-files-v1` remains unchanged. Only operational-v1 plus
`exact-v1` compares equality between required keys, descriptor explicit keys,
and bundle input keys. For `:nix-only`, the required set is descriptor plus
catalog-selected reviewed determinants; closure-manifest and automatic
Clojure lock inputs are absent unless independently named as real
determinants.

- [ ] **Step 5: Run GREEN and lint**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-operational-test \
  --focus abc.tools.adr-evidence-observation-catalog-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
```

Expected: all checks exit 0.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_operational.clj \
  abc/src/abc/tools/adr_evidence_observation_catalog.clj \
  abc/test/abc/tools/adr_evidence_operational_test.clj
git commit -m "feat(abc): validate operational evidence closure"
```

### Task 7: Integrate three-root capture and offline validation

**Files:**
- Modify: `abc/src/abc/tools/adr_evidence_capture.clj`
- Modify: `abc/src/abc/tools/adr_evidence_bundle.clj`
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_bundle_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_test.clj`

**Interfaces:**
- Replaces capture input with `{:repo-root :workspace-root :staging-root :descriptor-path :output}`; caller-supplied descriptor values are no longer accepted.
- Adds CLI `--workspace-root` and required `--staging-root` for committed capture.
- Offline bundle validation derives the same-stem descriptor and invokes `offline-policy-problems` for focused-v3/operational-v1.

- [ ] **Step 1: Write failing three-root, environment, output, and offline-bypass tests**

Assert descriptor containment precedes EDN reading; `repo-root` may be `<workspace>/abc`; Git cleanliness runs at workspace; command workdir is repo root; output is a direct staging child; catalog argv/environment policy is used; dirty-before/dirty-after produces no artifact; nonzero command produces a valid false observation; infrastructure failure produces none. Hand-author a self-consistent bundle omitting its descriptor or selected contract and assert offline rejection.

- [ ] **Step 2: Run RED**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-test
```

Expected: FAIL because capture still requires the Git root, accepts caller descriptor values, and uses the shared writer.

- [ ] **Step 3: Refactor capture orchestration**

Retain `capture!` as a one-map API but remove `:descriptor`. Validate the derived descriptor coordinate before `files/read-edn`. Use `runtime-inputs/validate-workspace-root!` for the component relation. Dispatch v1/v2/v3/operational explicitly; v1/v2 keep legacy/local workspace defaulting, focused-v3 preserves `component-clojure-test-v1` workspace-relative input semantics, and operational capture requires all roots with repo-relative determinants. Execute direct argv with `babashka.process/process`, set workdir to repo root, check Git at workspace root, and publish through `evidence-output/write-json-exclusive!`.

CLI options become:

```clojure
[[nil "--descriptor PATH"]
 [nil "--output PATH"]
 [nil "--repo-root PATH" :default "."]
 [nil "--workspace-root PATH"]
 [nil "--staging-root PATH"]]
```

Use `abc.tools.cli/run-cli!`; preserve optional leading `--` normalization.

- [ ] **Step 4: Integrate offline protocol validation**

After generic schema, canonical artifact hash, and input hash validation, call:

```clojure
(operational/offline-policy-problems
 {:repo-root repo-root
  :workspace-root workspace-root
  :artifact-path artifact-path
  :bundle value})
```

Accumulate returned problems; do not short-circuit or execute the observed command.

- [ ] **Step 5: Run GREEN and aggregate Clojure checks**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-bundle-test \
  --focus abc.tools.adr-evidence-operational-test \
  --focus abc.tools.adr-evidence-test \
  --focus abc.tools.evidence-output-test
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
```

Expected: all checks exit 0.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/adr_evidence_capture.clj \
  abc/src/abc/tools/adr_evidence_bundle.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj \
  abc/test/abc/tools/adr_evidence_bundle_test.clj \
  abc/test/abc/tools/adr_evidence_test.clj
git commit -m "feat(abc): harden evidence capture protocol"
```

### Task 8: Resolve catalog conformance and atomically admit the 37 descriptor boundaries

**Files:**
- Modify: `abc/test/abc/tools/foundation_evidence_test.clj`
- Modify: `abc/test/abc/tools/jcs_test.clj`
- Modify: `abc/test/abc/tools/code_as_spec_test.clj`
- Modify: `abc/test/abc/tools/manifest_index_test.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Modify: `abc/test/abc/tools/materialize_import_test.clj`
- Modify: `abc/test/abc/tools/source_bundle_test.clj`
- Modify: `abc/test/abc/tools/materialize_source_snapshot_test.clj`
- Modify: `abc/test/abc/sim/content_sim_test.clj`
- Modify: `abc/test/abc/sim/divergences_test.clj`
- Modify: `abc/data/adr-evidence/foundation-capture-conformance-debt.edn`
- Create: 35 focused-v3 and two operational-v1 descriptors under `abc/docs/evidence/adr-capture/`.
- Create: 35 runtime and two operational closure manifests under `abc/docs/evidence/adr-inputs/`.
- Create: `abc/docs/evidence/adr-entries/foundation.edn` with 42 bindings.
- Modify: `abc/test/abc/tools/adr_evidence_capture_test.clj`
- Modify: `abc/test/abc/tools/adr_evidence_observation_catalog_test.clj`

**Interfaces:**
- Consumes: the 37-row observation catalog and complete transitional debt.
- Produces: zero observed/expected debt, 37 descriptors/manifests, and 42 compatible binding rows. It produces no run bundle.

- [ ] **Step 1: Author all missing narrow tests and direct trace ownership in one staged change**

Use the amended foundation observation table as the closed inventory. Each of 35 unique focused Vars must be a `deftest`, assert only its named event, and call `with-validated-read-trace!` directly at top level. Generated-output tests use `with-owned-ephemeral-root`; repository fixtures still use traced adapters. Do not invoke another `deftest`.

- [ ] **Step 2: Create the 37 descriptors and manifests**

Focused descriptors use v3 and include catalog path, observation ID, JCS contract hash, same-stem runtime manifest, one exact focus, and descriptor/manifest/catalog-policy determinants. Operational descriptors use operational-v1, `exact-v1`, same-stem operational closure manifest, catalog-owned argv/environment policy, and the exact determinant set. There is one `design-bundle-operational` descriptor and one `source-bundle-corpus` descriptor.

- [ ] **Step 3: Create 42 bindings and zero the debt atomically**

The registration template contains 38 focused bindings and four operational bindings. Exactly three focused observation IDs occur twice; `design-bundle-operational` occurs three times; `source-bundle-corpus` occurs once. Change the expected normalized debt vector to `[]` only after the catalog validator reports no finding.

- [ ] **Step 4: Run descriptor-contract RED/GREEN gate before commit**

Run:

```bash
cd abc
bin/kaocha --focus abc.tools.adr-evidence-observation-catalog-test \
  --focus abc.tools.adr-evidence-capture-test \
  --focus abc.tools.adr-evidence-runtime-inputs-test \
  --focus abc.tools.adr-evidence-operational-test
```

Expected: PASS with exactly 35 focused observations, two operational observations, 42 bindings, zero conformance debt, and no missing/extra descriptor or manifest.

- [ ] **Step 5: Run aggregate migration checks**

Run:

```bash
cd ..
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
just validate-migration
test -z "$(git status --short -- abc/docs/evidence/adr-runs abc/docs/adr/adr-evidence.edn)"
```

Expected: all checks exit 0; governance remains audit; no run bundle or registry mutation exists.

- [ ] **Step 6: Commit the atomic Stage A boundary**

```bash
git add abc/data/adr-evidence/foundation-capture-conformance-debt.edn \
  abc/docs/evidence/adr-capture abc/docs/evidence/adr-inputs \
  abc/docs/evidence/adr-entries/foundation.edn \
  abc/test/abc/tools/foundation_evidence_test.clj \
  abc/test/abc/tools/jcs_test.clj \
  abc/test/abc/tools/code_as_spec_test.clj \
  abc/test/abc/tools/manifest_index_test.clj \
  abc/test/abc/tools/validate_design_bundle_test.clj \
  abc/test/abc/tools/materialize_import_test.clj \
  abc/test/abc/tools/source_bundle_test.clj \
  abc/test/abc/tools/materialize_source_snapshot_test.clj \
  abc/test/abc/sim/content_sim_test.clj \
  abc/test/abc/sim/divergences_test.clj \
  abc/test/abc/tools/adr_evidence_capture_test.clj \
  abc/test/abc/tools/adr_evidence_observation_catalog_test.clj
git commit -m "docs(adr): pin foundation evidence observation boundaries"
git status --short
```

Expected: clean tree. This commit is the producer revision for Task 8 capture in the foundation plan.

### Task 9: Final Task 6C handoff verification

**Files:**
- Verify only; no expected source change.

**Interfaces:**
- Produces: a clean Task 6C/7 boundary ready for the foundation plan's 37-bundle Stage B capture.

- [ ] **Step 1: Verify retired identities and counts**

```bash
! rg 'abc-adr-nix-clojure-closure-v1|invalid-nix-clojure-closure|derive-nix-clojure-source-closure|validate-nix-clojure-closure!' \
  abc/src abc/test
test "$(find abc/docs/evidence/adr-capture -maxdepth 1 -name '*.edn' | wc -l)" -eq 37
test "$(find abc/docs/evidence/adr-inputs -maxdepth 1 -name '*.edn' | wc -l)" -eq 37
test ! -d abc/docs/evidence/adr-runs || \
  test "$(find abc/docs/evidence/adr-runs -maxdepth 1 -name '*.json' | wc -l)" -eq 0
```

Expected: every assertion succeeds.

- [ ] **Step 2: Run the complete gate from a clean tree**

```bash
test -z "$(git status --short)"
just validate-migration
git status --short
```

Expected: exit 0 and clean tree. Governance audit may report the known unmigrated-family problems but must not report a foundation catalog/descriptor protocol defect.

- [ ] **Step 3: Record handoff**

Do not create an empty commit. Record the verified commit ID in the execution log and proceed to the amended foundation Task 8 only after review approval.
