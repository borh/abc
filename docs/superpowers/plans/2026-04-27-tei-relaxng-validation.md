# TEI P5 RelaxNG Validation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Validate TEI documents in the v0 contract harness against the upstream TEI P5 4.11.0 RelaxNG schema using Jing in-process, with the schema pinned via Nix.

**Architecture:** New `abc.tools.tei` namespace wraps Jing's `ValidationDriver`, returning structured `:violations` maps without throwing. The harness binding loads the schema once per run, validates each TEI document sequentially, partitions violations by severity (warnings logged via Telemere; errors and fatals accumulate and throw at end of step). Schema bytes pulled by `pkgs.fetchurl`, location passed to the app via `TEI_SCHEMA_PATH`.

**Tech Stack:** Clojure 1.12, Jing (`org.relaxng/jing` 20241231), `pkgs.fetchurl` for schema, libxml2 already on harness PATH for the existing well-formedness check on the ODD stub.

---

## Spec

`docs/superpowers/specs/2026-04-27-tei-relaxng-validation-design.md`. Read before starting.

## File Map

**Create:**
- `src/abc/tools/tei.clj` — Jing wrapper.
- `test/abc/tools/tei_test.clj` — Jing wrapper unit tests.

**Modify:**
- `deps.edn` — add `org.relaxng/jing`.
- `nix/clj-nix-deps.edn` — add `org.relaxng/jing`.
- `deps-lock.json` — regenerated.
- `flake.nix` — add `tei-schema` `pkgs.fetchurl` derivation; export `TEI_SCHEMA_PATH` from the `validate-design-bundle` wrapper; add `src/abc/tools/tei.clj` and `test/abc/tools/tei_test.clj` to `contract-surface`.
- `nix/clj-nix-deps.edn` — add `'abc.tools.tei-test` to the focused-test alias.
- `examples/v0/example-work/tei.xml` — replace invalid `<charName>` with `<localProp/>`.
- `src/abc/tools/validate_design_bundle.clj` — add `validate-tei!` step and wire it.
- `test/abc/tools/validate_design_bundle_test.clj` — add TEI smoke + loud-fail tests.

---

## Task 1: Add Jing dependency and regenerate lock

**Files:**
- Modify: `deps.edn`
- Modify: `nix/clj-nix-deps.edn`
- Modify: `deps-lock.json` (regenerated)

- [ ] **Step 1: Add `org.relaxng/jing` to `deps.edn`**

Open `deps.edn`. The file already has the `com.taoensso/telemere-slf4j` entry as the last item before the closing brace of `:deps`. Append the Jing entry:

```clojure
  com.taoensso/telemere-slf4j      {:mvn/version "1.2.1"}

  org.relaxng/jing                 {:mvn/version "20241231"}}
```

- [ ] **Step 2: Add the same coordinate to `nix/clj-nix-deps.edn`**

Open `nix/clj-nix-deps.edn`. Add `org.relaxng/jing` to the `:deps` map alongside the existing entries:

```clojure
        com.taoensso/telemere-slf4j {:mvn/version "1.2.1"}
        org.relaxng/jing {:mvn/version "20241231"}}
```

- [ ] **Step 3: Regenerate the lock**

Run: `bin/update-clj-nix-lock`
Expected: success. Verify Jing landed:

```bash
grep -c 'jing' deps-lock.json
```

Expected: > 0.

- [ ] **Step 4: Confirm Jing loads on the classpath**

Run:

```bash
clojure -M -e "(import 'com.thaiopensource.validate.ValidationDriver) (println (Class/forName \"com.thaiopensource.validate.ValidationDriver\"))"
```

Expected: prints `class com.thaiopensource.validate.ValidationDriver`.

If Class/forName fails with ClassNotFoundException, the dep didn't make it onto the classpath; re-check `deps.edn` syntax and re-run `clojure -Stree | grep jing`.

- [ ] **Step 5: Confirm `nix flake check` still passes**

Run: `nix flake check 2>&1 | tail -3`
Expected: `all checks passed!`. The new dep should be in the lock and resolvable in the focused-test sandbox.

- [ ] **Step 6: Commit**

```bash
git add deps.edn nix/clj-nix-deps.edn deps-lock.json
git commit -m "$(cat <<'EOF'
build: add org.relaxng/jing for TEI RelaxNG validation

Jing is the reference RelaxNG implementation; the architecture
note pins it as the TEI validation baseline. Pulled into the main
deps.edn and the clj-nix focused-test deps so the upcoming
abc.tools.tei wrapper can load in both runtime and CI sandboxes.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 2: Pin TEI schema via Nix and wire `TEI_SCHEMA_PATH`

**Files:**
- Modify: `flake.nix`

- [ ] **Step 1: Add `tei-schema` derivation and export to wrapper**

Open `flake.nix`. The `apps` block contains the `validate-design-bundle` wrapper around line 38. Add a `tei-schema` binding inside the `let` of the `apps` system block (so it's visible from the wrapper script). Edit the section to look like this — the new `let` and `tei-schema` declaration are added; the `validate-design-bundle` wrapper is updated to export `TEI_SCHEMA_PATH`.

Find:

```nix
      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              (final: _prev: import local-pkgs { pkgs = final; })
            ];
          };
        in
        {
          validate-design-bundle = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-design-bundle" ''
                export PATH="${
                  pkgs.lib.makeBinPath [
                    pkgs.git-cliff
                    pkgs.libxml2
                  ]
                }:''${PATH:-}"
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-design-bundle "$@"
              ''
            );
            meta.description = "Validate ABC v0 design-bundle schemas and fixtures";
          };
```

Replace the `let` body and the wrapper with:

```nix
      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              (final: _prev: import local-pkgs { pkgs = final; })
            ];
          };
          tei-schema = pkgs.fetchurl {
            url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng";
            hash = "sha256-7MSfAMN/SQtd9xa2cuPbpjXrHknM2I+5aYWUmN9CwIQ=";
          };
        in
        {
          validate-design-bundle = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-design-bundle" ''
                export PATH="${
                  pkgs.lib.makeBinPath [
                    pkgs.git-cliff
                    pkgs.libxml2
                  ]
                }:''${PATH:-}"
                export TEI_SCHEMA_PATH="${tei-schema}"
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-design-bundle "$@"
              ''
            );
            meta.description = "Validate ABC v0 design-bundle schemas and fixtures";
          };
```

- [ ] **Step 2: Verify the schema fetches and the env var lands**

Run: `nix run .#validate-design-bundle 2>&1 | head -3` (will still fail at the new TEI step that hasn't been added yet — that's fine; we just want to see Nix evaluates and the wrapper builds).

If the fetch hash is rejected, Nix prints "hash mismatch" with expected vs got; confirm the spec hash matches the upstream URL by re-running:

```bash
curl -sSL 'https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng' | sha256sum
```

The first hex digest should match the SRI form `sha256-7MSfAMN/SQtd9xa2cuPbpjXrHknM2I+5aYWUmN9CwIQ=` (decoded). If not, TEI has shipped a new release; bump the hash and note the version change in the commit message.

- [ ] **Step 3: Confirm `TEI_SCHEMA_PATH` resolves to a real file at runtime**

Add a one-liner check via `nix shell`:

```bash
nix run .#validate-design-bundle --command bash -c 'echo "$TEI_SCHEMA_PATH" && head -2 "$TEI_SCHEMA_PATH"' 2>&1 | head
```

Expected: prints a `/nix/store/...-tei_all.rng` path and the first two lines of the RelaxNG file (the XML declaration and the root `<grammar ...>` element).

- [ ] **Step 4: Commit**

```bash
git add flake.nix
git commit -m "$(cat <<'EOF'
build: pin TEI P5 4.11.0 RelaxNG via pkgs.fetchurl

Add tei-schema = pkgs.fetchurl pinned by sha256 to the upstream
canonical URL https://www.tei-c.org/release/xml/tei/custom/schema/
relaxng/tei_all.rng. Export the resulting nix-store path through
TEI_SCHEMA_PATH from the validate-design-bundle wrapper so the
Clojure side can load the schema by absolute path.

The URL is unversioned; the pinned hash is the durable identity
and any upstream re-publish is caught as a deliberate hash-mismatch
forcing function rather than silent drift.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 3: `abc.tools.tei/load-schema` (TDD)

**Files:**
- Create: `src/abc/tools/tei.clj`
- Create: `test/abc/tools/tei_test.clj`

- [ ] **Step 1: Write the test file with a load-schema positive test and a use-fixtures :once that fails loudly when env unset**

Write `test/abc/tools/tei_test.clj`:

```clojure
(ns abc.tools.tei-test
  (:require [abc.tools.tei :as tei]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:private schema-path-atom (atom nil))

(defn require-schema-path-fixture [t]
  (let [path (System/getenv "TEI_SCHEMA_PATH")]
    (when-not path
      (throw (ex-info "TEI_SCHEMA_PATH must be set to run abc.tools.tei-test. Run via `nix run .#validate-design-bundle` or export the path manually." {})))
    (reset! schema-path-atom path)
    (t)))

(use-fixtures :once require-schema-path-fixture)

(deftest load-schema-test
  (testing "load-schema returns a non-nil Schema for the upstream tei_all.rng"
    (let [schema (tei/load-schema @schema-path-atom)]
      (is (some? schema))
      (is (instance? com.thaiopensource.validate.Schema schema)))))
```

- [ ] **Step 2: Run the test to confirm it fails (namespace missing)**

Run:

```bash
TEI_SCHEMA_PATH=$(nix-build --no-out-link --expr 'with import <nixpkgs> {}; fetchurl { url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng"; sha256 = "ecc49f00c37f490b5df716b672e3dba635eb1e49ccd88fb969859498df42c084"; }' 2>/dev/null) clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -10
```

If the inline fetch is awkward, just enter `nix shell` first or set `TEI_SCHEMA_PATH` once for the whole task block:

```bash
export TEI_SCHEMA_PATH="$(nix eval --raw --impure --expr '(import (builtins.getFlake (toString ./.))).apps.x86_64-linux.validate-design-bundle.program' 2>/dev/null | head -c0; nix-build --no-out-link --expr 'with import <nixpkgs> {}; fetchurl { url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng"; sha256 = "ecc49f00c37f490b5df716b672e3dba635eb1e49ccd88fb969859498df42c084"; }')"
```

Or simpler — copy the schema once for the duration of dev:

```bash
mkdir -p /tmp/abc-tei && curl -sSL 'https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng' -o /tmp/abc-tei/tei_all.rng
export TEI_SCHEMA_PATH=/tmp/abc-tei/tei_all.rng
```

Re-run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -5
```

Expected: FAIL with `Could not locate abc/tools/tei__init.class, abc/tools/tei.clj or abc/tools/tei.cljc`.

- [ ] **Step 3: Create the `abc.tools.tei` namespace with `load-schema`**

Write `src/abc/tools/tei.clj`:

```clojure
(ns abc.tools.tei
  "Wrap Jing for TEI RelaxNG validation. Pure functions; the caller
  holds any cached Schema between validate! calls."
  (:require [clojure.java.io :as io])
  (:import [com.thaiopensource.util PropertyMapBuilder]
           [com.thaiopensource.validate
            Schema
            SchemaReader
            ValidationDriver]
           [com.thaiopensource.validate.rng CompactSchemaReader RngProperty]
           [com.thaiopensource.xml.sax XMLReaderCreator]
           [org.xml.sax InputSource]))

(defn- file->input-source [^java.io.File f]
  (InputSource. (.toURI f)))

(defn load-schema
  "Read a RelaxNG schema (XML syntax) from `path` and return a Jing
  com.thaiopensource.validate.Schema object."
  [^String path]
  (let [props (.toPropertyMap (PropertyMapBuilder.))
        driver (ValidationDriver. props)
        file (io/file path)
        in (file->input-source file)]
    (when-not (.loadSchema driver in)
      (throw (ex-info (str "Failed to load TEI RelaxNG schema: " path)
                      {:path path})))
    (.getSchema driver)))
```

- [ ] **Step 4: Run the test to confirm it passes**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -5
```

Expected: PASS, `Ran 1 tests containing 2 assertions. 0 failures, 0 errors.`

If the test errors with reflection issues on `.getSchema`, the Jing API may vary by version; check the actual class via:

```bash
clojure -M -e "(import 'com.thaiopensource.validate.ValidationDriver) (println (vec (map #(.getName %) (.getDeclaredMethods ValidationDriver))))"
```

The `loadSchema` and `getSchema` methods are stable since Jing 20030619; the call form above should work for 20241231.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/tei.clj test/abc/tools/tei_test.clj
git commit -m "$(cat <<'EOF'
feat: abc.tools.tei/load-schema reads TEI RelaxNG schemas via Jing

Pure function; caller holds the Schema and reuses it across
validate! calls. Test fixture fails loudly when TEI_SCHEMA_PATH
is unset, mirroring the harness's loud-fail contract instead of
silently skipping.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 4: `abc.tools.tei/validate!` returning structured violations

**Files:**
- Modify: `src/abc/tools/tei.clj`
- Modify: `test/abc/tools/tei_test.clj`

- [ ] **Step 1: Write a positive validate! test against the example fixture (will fail; <charName> is invalid)**

Append to `test/abc/tools/tei_test.clj`:

```clojure
(deftest validate-empty-document-test
  (testing "validate! on an empty doc returns a non-empty :violations vector"
    (let [schema (tei/load-schema @schema-path-atom)
          tmp (java.io.File/createTempFile "abc-tei-empty" ".xml")]
      (try
        (spit tmp "<?xml version=\"1.0\"?><not-tei xmlns=\"x\"/>")
        (let [{:keys [violations label]} (tei/validate! {:schema schema
                                                         :xml-path (str tmp)
                                                         :label "empty"})]
          (is (= "empty" label))
          (is (seq violations) "non-conforming doc must report at least one violation")
          (is (every? #(contains? % :severity) violations))
          (is (every? #(contains? % :line) violations))
          (is (every? #(contains? % :message) violations)))
        (finally
          (.delete tmp))))))

(deftest validate-undefined-element-test
  (testing "<bogusElement> in TEI namespace surfaces a violation referencing the element name"
    (let [schema (tei/load-schema @schema-path-atom)
          tmp (java.io.File/createTempFile "abc-tei-bogus" ".xml")]
      (try
        (spit tmp (str "<?xml version=\"1.0\"?>"
                       "<TEI xmlns=\"http://www.tei-c.org/ns/1.0\">"
                       "  <teiHeader><fileDesc>"
                       "    <titleStmt><title>t</title></titleStmt>"
                       "    <publicationStmt><p>p</p></publicationStmt>"
                       "    <sourceDesc><p>s</p></sourceDesc>"
                       "  </fileDesc></teiHeader>"
                       "  <text><body><bogusElement/></body></text>"
                       "</TEI>"))
        (let [{:keys [violations]} (tei/validate! {:schema schema
                                                   :xml-path (str tmp)
                                                   :label "bogus"})]
          (is (some #(re-find #"bogusElement" (:message %)) violations)
              (str "expected a violation mentioning bogusElement, got: " (pr-str violations))))
        (finally
          (.delete tmp))))))
```

- [ ] **Step 2: Run the new tests; confirm they fail (validate! missing)**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -10
```

Expected: FAIL/ERROR with `No such var: abc.tools.tei/validate!`.

- [ ] **Step 3: Implement `validate!`**

Append to `src/abc/tools/tei.clj`:

```clojure
(defn- error-handler [violations-vol]
  (reify org.xml.sax.ErrorHandler
    (warning [_ e]
      (vswap! violations-vol conj
              {:severity :warning
               :line     (.getLineNumber e)
               :column   (.getColumnNumber e)
               :message  (.getMessage e)}))
    (^void error [_ ^org.xml.sax.SAXParseException e]
      (vswap! violations-vol conj
              {:severity :error
               :line     (.getLineNumber e)
               :column   (.getColumnNumber e)
               :message  (.getMessage e)}))
    (^void fatalError [_ ^org.xml.sax.SAXParseException e]
      (vswap! violations-vol conj
              {:severity :fatal
               :line     (.getLineNumber e)
               :column   (.getColumnNumber e)
               :message  (.getMessage e)}))))

(defn validate!
  "Validate the XML at `xml-path` against `schema` (a Jing Schema from
  load-schema). Returns {:label, :violations [{:severity, :line, :column,
  :message} ...]}. Does not throw; severity classification is preserved
  on each violation. ValidationDriver is built fresh per call; sequential
  use only."
  [{:keys [^Schema schema xml-path label]}]
  (let [violations (volatile! [])
        builder (PropertyMapBuilder.)
        _ (.put builder
                com.thaiopensource.util.PropertyId/ERROR_HANDLER
                (error-handler violations))
        props (.toPropertyMap builder)
        driver (ValidationDriver. props)
        file (io/file xml-path)
        in (file->input-source file)]
    (.loadSchema driver (file->input-source (io/file (System/getenv "TEI_SCHEMA_PATH"))))
    ;; ↑ workaround: ValidationDriver requires re-loading the schema
    ;; into its property map; we ignore the parsed Schema in this path.
    ;; Cleaner alternative if the API exposes setSchema: use it instead.
    (.validate driver in)
    {:label label
     :violations @violations}))
```

Note on the schema-handling: Jing's public API loads the schema into the `ValidationDriver`'s internal state via `.loadSchema`. Some Jing builds expose a `setSchema(Schema)` API that lets you reuse a pre-parsed `Schema` object; if it's available in 20241231, prefer it (simpler, no re-parse per file). Verify in step 4 by inspecting the methods on `ValidationDriver`. The plan code above falls back to `.loadSchema` for portability; the harness still wins by avoiding subprocess overhead.

- [ ] **Step 4: Inspect the API for `setSchema` and prefer it if available**

Run:

```bash
clojure -M -e "(import 'com.thaiopensource.validate.ValidationDriver) (println (vec (sort (map #(.getName %) (.getDeclaredMethods ValidationDriver)))))"
```

If the printed list contains a `setSchema` or similar method that accepts `com.thaiopensource.validate.Schema`, replace the `.loadSchema` line in `validate!` with the direct setter and drop the `load-schema` ↔ `validate!` round-trip. Otherwise, keep the `.loadSchema` form (each `validate!` re-parses the schema; load-schema becomes a smoke-test of the schema file rather than a true cache, which is fine for v0 — single-file harness).

If the cleaner setter doesn't exist and the cost of re-parsing per file becomes a problem at corpus scale, that's a future `abc.tools.tei` enhancement, not a v0 concern.

Update the namespace docstring if the setter path is taken so callers know `load-schema` is purely advisory in that case.

- [ ] **Step 5: Run the tests to confirm they pass**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -5
```

Expected: all three tests pass (`load-schema-test`, `validate-empty-document-test`, `validate-undefined-element-test`). Total assertions ≥ 7.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/tei.clj test/abc/tools/tei_test.clj
git commit -m "$(cat <<'EOF'
feat: abc.tools.tei/validate! returns structured violations per file

ValidationDriver per call (Jing not thread-safe). ErrorHandler
writes warnings, errors, and fatals into a per-call volatile,
preserving severity. Returns {:label, :violations [...]}; never
throws. Caller decides severity policy.

Negative tests cover undefined elements and a non-TEI root.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 5: TDD-driven fixture fix for `examples/v0/example-work/tei.xml`

**Files:**
- Modify: `test/abc/tools/tei_test.clj`
- Modify: `examples/v0/example-work/tei.xml`

- [ ] **Step 1: Write a positive test asserting the example fixture validates clean**

Append to `test/abc/tools/tei_test.clj`:

```clojure
(deftest validate-example-fixture-test
  (testing "examples/v0/example-work/tei.xml validates clean against tei_all.rng"
    (let [schema (tei/load-schema @schema-path-atom)
          {:keys [violations]} (tei/validate! {:schema schema
                                               :xml-path "examples/v0/example-work/tei.xml"
                                               :label "example"})]
      (is (empty? (filter #(#{:error :fatal} (:severity %)) violations))
          (str "fixture must produce no error/fatal violations; got: "
               (pr-str violations))))))
```

- [ ] **Step 2: Run the test to confirm it fails on `<charName>`**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -10
```

Expected: `validate-example-fixture-test` fails with a violation referencing `charName` not allowed inside `char`.

- [ ] **Step 3: Replace `<charName>` with the TEI-idiomatic `<localProp/>`**

In `examples/v0/example-work/tei.xml`, find:

```xml
<char xml:id="example-gaiji">
  <charName>Example unresolved Aozora gaiji fixture</charName>
  <desc>Design fixture for preserving an unresolved gaiji marker.</desc>
</char>
```

Replace with:

```xml
<char xml:id="example-gaiji">
  <localProp name="charName" value="Example unresolved Aozora gaiji fixture"/>
  <desc>Design fixture for preserving an unresolved gaiji marker.</desc>
</char>
```

- [ ] **Step 4: Run the test to confirm it now passes**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.tei-test) (clojure.test/run-tests 'abc.tools.tei-test)" 2>&1 | tail -5
```

Expected: PASS for `validate-example-fixture-test`. All four tests in the namespace pass.

If validation still fails, read the violation's `:message` for the new offending element and consult `tei_all.rng`'s `<define name="<element>">` block to determine the correct content model. Do not silently delete failing markup; fix it within TEI's content rules.

- [ ] **Step 5: Commit**

```bash
git add examples/v0/example-work/tei.xml test/abc/tools/tei_test.clj
git commit -m "$(cat <<'EOF'
fix: replace invalid <charName> with TEI-idiomatic <localProp/>

TEI 4.11.0 does not define <charName>. The <char> content model
allows localProp for project-specific named properties. Replace
the invalid element with localProp name="charName" value="...".
The original semantic intent (a name attached to the character
entry) is preserved within the schema.

Driven by validate-example-fixture-test in test/abc/tools/tei_test.clj.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 6: Wire TEI step into `validate-design-bundle` with severity policy

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`

- [ ] **Step 1: Add the TEI helper functions and step**

Open `src/abc/tools/validate_design_bundle.clj`. Add `[abc.tools.tei :as tei]` to the `:require`. Below the existing `validate-shacl!` helper, add:

```clojure
(defn- render-tei-violation [{:keys [severity line column message label]}]
  (str (.toUpperCase (name severity)) ": "
       label
       " " line ":" column
       " — " message))

(defn validate-tei!
  "Validate every TEI document in `xml-paths` against the schema at
  the path passed in. Warnings are logged via Telemere but do not
  fail the step; errors and fatals are aggregated and thrown at the
  end."
  [^String schema-path xml-paths]
  (when (or (nil? schema-path) (= "" schema-path))
    (throw (ex-info "TEI_SCHEMA_PATH must be set. Run via `nix run .#validate-design-bundle` or export it manually before calling clojure -M:abc/validate-design-bundle."
                    {:env-var "TEI_SCHEMA_PATH"})))
  (let [schema (tei/load-schema schema-path)
        all-violations
        (reduce
         (fn [acc path]
           (let [{:keys [violations]} (tei/validate! {:schema schema
                                                      :xml-path path
                                                      :label (str path)})]
             (into acc (map #(assoc % :label (str path))) violations)))
         []
         xml-paths)
        {warnings true failures false}
        (group-by #(= :warning (:severity %)) all-violations)]
    (doseq [w warnings]
      (tel/log! :warn (render-tei-violation w)))
    (when (seq failures)
      (throw (ex-info "TEI RelaxNG validation failed"
                      {:errors (mapv render-tei-violation failures)})))))
```

- [ ] **Step 2: Insert the new step into `validate-design-bundle!`**

Find the existing `validate-xml!` call and the line just before `validate-git-cliff!`:

```clojure
      (tel/log! :info "==> Checking XML fixtures")
      (validate-xml!)
      (tel/log! :info "xml fixtures ok")
      (tel/log! :info "==> Checking git-cliff configuration")
```

Insert the new TEI step between them:

```clojure
      (tel/log! :info "==> Checking XML fixtures")
      (validate-xml!)
      (tel/log! :info "xml fixtures ok")
      (tel/log! :info "==> Validating TEI against P5 RelaxNG")
      (validate-tei! (System/getenv "TEI_SCHEMA_PATH")
                     ["examples/v0/example-work/tei.xml"])
      (tel/log! :info "tei rng validation ok")
      (tel/log! :info "==> Checking git-cliff configuration")
```

- [ ] **Step 3: Run the harness end-to-end via Nix to confirm**

Run: `nix run .#validate-design-bundle 2>&1 | tail -25`
Expected: every `==>` step prints its `ok` line, including:

```
==> Validating TEI against P5 RelaxNG
tei rng validation ok
```

Final line: `design bundle validation ok`. Exit code 0.

- [ ] **Step 4: Confirm direct `clojure -M:abc/...` without the env var fails loudly**

Run:

```bash
unset TEI_SCHEMA_PATH
clojure -M:abc/validate-design-bundle 2>&1 | tail -10
echo "EXIT: $?"
```

Expected: harness exits 1 with an error message that names `TEI_SCHEMA_PATH` and references `nix run`. The previous steps may print before failure (since the TEI step is near the end of the run). The key signal is that the step's error is rendered through `check-errors!` / the `-main` printer.

Re-export it for subsequent steps: `export TEI_SCHEMA_PATH=/tmp/abc-tei/tei_all.rng` (or fresh fetch).

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/validate_design_bundle.clj
git commit -m "$(cat <<'EOF'
feat: validate-design-bundle runs Jing TEI validation per run

New step '==> Validating TEI against P5 RelaxNG' loads the schema
at TEI_SCHEMA_PATH once via abc.tools.tei/load-schema, validates
each TEI document with abc.tools.tei/validate!, and partitions
violations by severity:
- :warning    → tel/log! :warn (does not fail the step)
- :error/:fatal → aggregated and thrown at the end of the step

Missing TEI_SCHEMA_PATH triggers a loud failure with a clear
message naming the variable and the canonical nix run invocation,
not a silent skip.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 7: Smoke + loud-fail tests in `validate_design_bundle_test`

**Files:**
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Add the two tests**

Open `test/abc/tools/validate_design_bundle_test.clj`. Add `[abc.tools.tei :as tei]` to the `:require` (it's not currently required there). Append:

```clojure
(deftest validate-tei-smoke-test
  (testing "validate-tei! returns nil for the example fixture when env var is set"
    (let [schema-path (System/getenv "TEI_SCHEMA_PATH")]
      (when-not schema-path
        (throw (ex-info "TEI_SCHEMA_PATH must be set to run validate-tei-smoke-test." {})))
      (is (nil? (validate/validate-tei! schema-path
                                        ["examples/v0/example-work/tei.xml"]))))))

(deftest validate-tei-loud-fail-when-env-unset-test
  (testing "validate-tei! throws ex-info naming TEI_SCHEMA_PATH when called with nil"
    (try
      (validate/validate-tei! nil ["examples/v0/example-work/tei.xml"])
      (is false "expected validate-tei! to throw")
      (catch clojure.lang.ExceptionInfo e
        (is (re-find #"TEI_SCHEMA_PATH" (ex-message e)))))))
```

- [ ] **Step 2: Run the harness tests**

Run:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)" 2>&1 | tail -5
```

Expected: PASS for both new tests. The smoke positive must show that the example fixture validates clean (already verified in Task 5; this just goes through the harness helper).

If `validate-tei-smoke-test` fails because `TEI_SCHEMA_PATH` is unset in the test runner shell, set it as instructed in Task 3 and re-run.

- [ ] **Step 3: Commit**

```bash
git add test/abc/tools/validate_design_bundle_test.clj
git commit -m "$(cat <<'EOF'
test: smoke-test TEI validation from validate-design-bundle-test

Two tests in test/abc/tools/validate_design_bundle_test.clj:

1. Smoke positive: validate-tei! returns nil for the example
   fixture when TEI_SCHEMA_PATH is set.
2. Loud-fail-when-nil: passing nil as schema-path raises ex-info
   that names TEI_SCHEMA_PATH. Catches divergence between test
   "skip" semantics and harness "fail loudly" semantics.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 8: Update flake contract surface and focused-test alias

**Files:**
- Modify: `flake.nix`
- Modify: `nix/clj-nix-deps.edn`

- [ ] **Step 1: Add new files to `contract-surface`**

Open `flake.nix`. In the `contract-surface` derivation, find the existing `test -f` lines for `tools` files and append:

```nix
            test -f ${./src/abc/tools/tei.clj}
            test -f ${./test/abc/tools/tei_test.clj}
```

Place them next to the other `src/abc/tools/*.clj` and `test/abc/tools/*_test.clj` lines respectively, keeping alphabetical ordering where the existing list does.

- [ ] **Step 2: Wire the focused-test alias to load `abc.tools.tei-test`**

Open `nix/clj-nix-deps.edn`. The `:abc/focused-test` `:main-opts` is a long single-line string with all test namespaces enumerated twice (require + run-tests). Add `'abc.tools.tei-test` to both lists. The simplest addition is alongside `'abc.tools.shacl-test`.

Use a global edit to apply both at once if you can do it safely. Otherwise, do two separate text replacements: one to add `'abc.tools.tei-test` next to `'abc.tools.shacl-test` in the `(require ...)` form, and one to add it next to `'abc.tools.shacl-test` in the `(test/run-tests ...)` form.

But the focused-test sandbox runs without `TEI_SCHEMA_PATH`. If we add `tei-test` and the namespace fixture fails loudly when env unset, the focused-test sandbox will fail. **Resolution**: make the namespace fixture in `tei-test` pass `TEI_SCHEMA_PATH` to the focused-test runner via a deps cache or skip the test only when `ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED=1` is set.

The minimal change: extend the fixture in `test/abc/tools/tei_test.clj` to skip (with a clear log) when `ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED` is `"1"`, then set that variable in the `clj-nix-focused-tests` derivation in `flake.nix`. This preserves the harness's loud-fail contract while letting the focused-test sandbox run without network access.

Edit `test/abc/tools/tei_test.clj`'s `require-schema-path-fixture`:

```clojure
(defn require-schema-path-fixture [t]
  (let [path (System/getenv "TEI_SCHEMA_PATH")]
    (cond
      (= "1" (System/getenv "ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED"))
      (do
        (println "abc.tools.tei-test: skipping (ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED=1)")
        :skipped)

      (nil? path)
      (throw (ex-info "TEI_SCHEMA_PATH must be set to run abc.tools.tei-test. Run via `nix run .#validate-design-bundle` or export the path manually." {}))

      :else
      (do
        (reset! schema-path-atom path)
        (t)))))
```

In `flake.nix` `clj-nix-focused-tests`, add the env var export inside the build script just before `clojure -M:abc/focused-test`:

```nix
              export ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED=1

              clojure -M:abc/focused-test
```

This keeps the sandbox sealed (no network access for the schema), keeps the v0 contract sandbox green, and is honest about what's covered: the focused-test sandbox does not exercise the TEI step. The TEI step is exercised by `nix run .#validate-design-bundle` end-to-end (Task 9 below).

Apply the same `ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED` skip to `validate-tei-smoke-test` in `test/abc/tools/validate_design_bundle_test.clj` so it skips cleanly in the sandbox. Use a short `when` guard at the top of the `let` block:

```clojure
(deftest validate-tei-smoke-test
  (testing "validate-tei! returns nil for the example fixture when env var is set"
    (when-not (= "1" (System/getenv "ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED"))
      (let [schema-path (System/getenv "TEI_SCHEMA_PATH")]
        (when-not schema-path
          (throw (ex-info "TEI_SCHEMA_PATH must be set to run validate-tei-smoke-test." {})))
        (is (nil? (validate/validate-tei! schema-path
                                          ["examples/v0/example-work/tei.xml"])))))))
```

The loud-fail test (`validate-tei-loud-fail-when-env-unset-test`) does not need a skip; it doesn't depend on `TEI_SCHEMA_PATH` at all and exercises the helper directly with `nil`.

- [ ] **Step 3: Run nix flake check**

Run: `nix flake check 2>&1 | tail -5`
Expected: `all checks passed!` — the focused-test sandbox now loads `abc.tools.tei-test` (which imports Jing), runs without TEI_SCHEMA_PATH (skip path active), and reports zero failures.

If the focused-test build fails to load Jing, double-check that Jing landed in the lock from Task 1.

- [ ] **Step 4: Commit**

```bash
git add flake.nix nix/clj-nix-deps.edn test/abc/tools/tei_test.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "$(cat <<'EOF'
chore: include abc.tools.tei in flake check surface; sandbox skip

- contract-surface check verifies new tei.clj and tei_test.clj exist
- focused-test alias requires and runs abc.tools.tei-test
- focused-test sandbox sets ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED=1 so
  tests that need a schema path skip cleanly without network access
- tei-test and validate-tei-smoke-test honor the skip flag; the
  loud-fail-when-nil test always runs (no env dep)

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 9: End-to-end verification

**Files:**
- (No file changes; verification only.)

- [ ] **Step 1: Run validate-design-bundle from a fresh shell**

Run: `nix run .#validate-design-bundle 2>&1 | tail -25`
Expected: every step prints its `ok`, including `==> Validating TEI against P5 RelaxNG` → `tei rng validation ok`. Final line: `design bundle validation ok`. Exit 0.

- [ ] **Step 2: Run nix flake check**

Run: `nix flake check 2>&1 | tail -5`
Expected: `all checks passed!`.

- [ ] **Step 3: Negative regression — restore `<charName>` and confirm the harness fails**

Temporarily restore the offending element:

```bash
git stash push examples/v0/example-work/tei.xml -m "temp-revert" || true
# Manually edit to restore:
sed -i.bak 's|<localProp name="charName" value="Example unresolved Aozora gaiji fixture"/>|<charName>Example unresolved Aozora gaiji fixture</charName>|' examples/v0/example-work/tei.xml
```

Run: `nix run .#validate-design-bundle 2>&1 | tail -15`
Expected: harness exits non-zero with a violation message referencing `charName` (or, less specifically, an element-not-allowed message inside `<char>`).

Restore the fixture:

```bash
mv examples/v0/example-work/tei.xml.bak examples/v0/example-work/tei.xml
git checkout -- examples/v0/example-work/tei.xml  # belt-and-suspenders
```

Confirm clean: `nix run .#validate-design-bundle 2>&1 | tail -3` shows `design bundle validation ok`.

- [ ] **Step 4: Negative regression — Jing-warning case (optional but recommended)**

Construct a TEI snippet with a dangling `IDREF` (e.g., a `<ref target="#missing"/>` where `#missing` is undefined) in a temp file. Validate it manually via:

```bash
clojure -M -e "(require '[abc.tools.tei :as tei]) (let [s (tei/load-schema (System/getenv \"TEI_SCHEMA_PATH\"))] (println (tei/validate! {:schema s :xml-path \"/tmp/dangling.xml\" :label \"dangling\"})))"
```

Expected: the returned `:violations` includes at least one map with `:severity :warning` and a message about the missing IDREF, *not* an `:error`. This validates the spec's claim that warnings are preserved as warnings.

If Jing reports the dangling IDREF as `:error` instead of `:warning`, that's Jing's call; record the actual severity and move on. The point of the test is that severity is preserved through the wrapper, not that any specific issue is warning-level.

---

## Task 10: Update next-steps.md

**Files:**
- Modify: `docs/next-steps.md`

- [ ] **Step 1: Drop the TEI item; add a TEI ODD-promotion follow-on item**

Open `docs/next-steps.md`. Find the candidate-next-milestones list. Remove (or rewrite) the existing TEI bullet:

Before:

```markdown
1. **TEI profile + validation pipeline.** Promote
   `schemas/tei-profile.odd` from stub to a real ODD aligned with TEI
   P5 4.11.0 ruby support. Generate Relax NG; add Jing or `xmllint`
   validation as a `validate-design-bundle` step. Unlocks replacing
   the design fixtures with a real Aozora work end to end.
```

Replace with:

```markdown
1. **TEI ODD promotion.** Promote `schemas/tei-profile.odd` from
   stub to a project-specific ODD aligned with TEI P5 4.11.0 ruby
   support. Generate a project-specific RelaxNG via `roma`/`teiroma`
   and use it instead of `tei_all.rng` in `validate-design-bundle`.
   v0 currently validates against full TEI; an ODD-derived schema
   tightens what's accepted. Prerequisite for replacing the design
   fixtures with a real Aozora work end to end.
```

- [ ] **Step 2: Commit**

```bash
git add docs/next-steps.md
git commit -m "$(cat <<'EOF'
docs: refocus next-steps after TEI RelaxNG validation milestone

The harness now validates TEI against TEI P5 4.11.0 tei_all.rng
via Jing in-process. Re-frame the next-steps TEI item to focus
on the remaining piece: ODD-driven schema customization to
replace tei_all.rng with a project-specific subset.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Self-Review

**Spec coverage:**
- §Architecture/`abc.tools.tei` — Tasks 3 (load-schema) and 4 (validate!).
- §Architecture/Schema source via Nix flake — Task 2.
- §Architecture/Wrapper-injected env var — Task 2 step 1.
- §Architecture/`validate-design-bundle` wiring — Task 6.
- §Architecture/Severity policy — Task 6 (warnings → `tel/log! :warn`; errors/fatals → throw).
- §Architecture/Concurrency (per-call ValidationDriver, per-call volatile) — Task 4 step 3.
- §Architecture/Memory note — covered in spec; no implementation work needed.
- §Schema refresh policy — captured in spec; commit message of the next bump will document the change. No task needed.
- §Fixture fix — Task 5 (TDD-driven).
- §Test plan/`tei_test.clj` positive + negatives — Tasks 3, 4, 5.
- §Test plan/`validate_design_bundle_test.clj` smoke + loud-fail — Task 7.
- §Dependencies — Task 1.
- §Acceptance criteria — covered across Tasks 6-9.
- §Sequencing — tasks track the spec's sequence with the addition of Task 8 (sandbox skip) and Task 10 (next-steps refresh) which are spec-implied but not enumerated.

**Placeholder scan:**
- No "TBD"/"TODO"/"implement later"/"Add appropriate error handling"/"similar to Task N" found.
- The Jing API note in Task 4 ("if `setSchema` is available, prefer it") is a documented runtime check with explicit fallback code, not a placeholder.
- Task 9 step 4 is marked "optional but recommended" and is fully self-contained if the implementer chooses to do it.

**Type consistency:**
- `load-schema` arity: 1 (path). Used consistently in tasks 3, 4, 5, 6, 7.
- `validate!` signature: `{:schema, :xml-path, :label}` → `{:label, :violations [{:severity, :line, :column, :message}, ...]}`. Same shape across Tasks 4, 5, 6, 7.
- Severity values: `:warning`, `:error`, `:fatal`. Same keywords throughout.
- `validate-tei!` signature: `(validate-tei! schema-path xml-paths)` — Tasks 6, 7.
- Env var name: `TEI_SCHEMA_PATH`. Consistent across Tasks 2, 3, 4, 5, 6, 7, 8.
- Skip-flag env var name: `ABC_FOCUSED_TEST_SKIP_ENV_REQUIRED`. Consistent in Task 8.

No drift detected.
