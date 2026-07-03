# Parser-IR Publication Pipeline Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the first ABC-side parser-IR publication tracer bullet: render imported parser-IR to plaintext and TEI body output, validate renderer coverage against the parser-IR schema, and prepare materialized publication artifacts without depending on `../ab-validator` at validation time.

**Architecture:** `../ab-validator` remains the parser lab and parser producer; ABC consumes exported parser-IR bundles. The renderer modules are pure value transforms behind small interfaces, while file writing and manifests stay in a separate publication materializer. TEI header metadata remains in `abc.tools.tei-header`; parser-IR rendering supplies the TEI body and gaiji character declarations needed by that header.

**Tech Stack:** Clojure 1.12, `clojure.data.xml`, JSON Schema Draft 2020-12, existing ABC manifest helpers, existing TEI Relax NG and Schematron gates, Nix flake checks.

## Global Constraints

- Do not execute or import `../ab-validator` from ABC tests or Nix gates.
- Do not revive the legacy Clojure Aozora parser path in `abc.annotation` or `abc.tei`.
- Do not add AAT v2, parser-IR warigaki, parser-IR kunten, or aozora2html registry support in this plan.
- Renderer coverage must be derived from `schemas/parser-ir.schema.json`; adding a parser-IR node type must fail tests until plaintext and TEI policies are explicit.
- Plaintext and TEI renderers must return values, not write files.
- Materialization must be a separate module from the pure renderers.
- Generated full-corpus artifacts stay out of git.
- Focused validation commands must use `bin/kaocha --focus ...` or an explicit `System/exit` wrapper. Raw `clojure.test/run-tests` commands are not verification because they exit 0 on assertion failures.
- Plaintext `output_format_spec_hash` must hash a format-defining policy artifact, not ADR prose.
- Validation must include focused tests, `nix run .#validate-design-bundle`, and `nix flake check --print-build-logs` before commit.

---

## Design Deepening

### Problem Boundary

The goal is not to choose or build the parser in ABC. The goal is to make ABC a reliable publication consumer once ab-validator emits parser-IR from parser comparison and the eventual comprehensive parser.

ABC already owns:

- parser-IR schema and imported bundle validation,
- manifest identity,
- TEI profile and validation gates,
- metadata/person records,
- RDF/provenance views.

ABC lacks:

- a pure parser-IR to plaintext renderer,
- a pure parser-IR to TEI body renderer,
- a schema-derived renderer coverage gate,
- a publication materializer that writes plaintext/TEI artifacts from parser-IR.

### Approaches Considered

**Recommended: parser-IR publication modules.** Add small, pure renderer modules plus a separate file materializer. This keeps parser execution outside ABC and lets ABC advance goal (c) without waiting for ab-validator to finish every parser feature.

**Rejected: extend legacy `abc.tei` and `abc.annotation`.** Those namespaces still assume Clojure-side parsing and older EDN shapes. Extending them would braid parser recovery, publication rendering, and TEI emission in one path.

**Rejected for now: wait for final ab-validator parser.** Waiting preserves clean boundaries but leaves ABC's downstream contract untested. A tracer bullet over the existing parser-IR fixture is enough to reveal renderer and manifest gaps now.

### Module Candidates

**Deepen: `abc.tools.parser-ir-vocabulary`**

- Evidence: parser-IR node vocabulary currently lives in JSON Schema, while renderer support would otherwise live as unverified manual maps.
- Interface: `node-types`, `coverage-errors`.
- Hazard check: schema parsing must be narrow and structured. It should inspect `$defs/*/properties/type/const` through JSON maps, not grep schema text.

**Deepen: `abc.tools.parser-ir-plaintext`**

- Evidence: plaintext policy is one cohesive concern: how each parser-IR node contributes to a plain transcription.
- Interface: `render`, `render-string`.
- Hazard check: dropping non-textual nodes must be reported in returned metadata, not hidden.

**Deepen: `abc.tools.parser-ir-tei`**

- Evidence: TEI body rendering is separate from metadata header construction and file materialization.
- Interface: `render`, returning body hiccup plus char declarations and omitted-node notes.
- Hazard check: gaiji declarations are a trust seam between body and header. The body renderer must surface declarations; the header must not hardcode fixture declarations.

**Decomplect: publication file materialization**

- Evidence: writing files, computing hashes, and constructing manifests are separate from rendering.
- Route: a new `abc.tools.materialize-publication` module should consume renderer outputs and manifest helpers; it should not grow inside `materialize-import`, whose current concern is imported parser output.

### Selected Tracer Bullet

Implement current parser-IR node coverage, plaintext rendering, TEI body rendering, and a small publication materializer over checked-in fixtures. This produces working software without requiring a live ab-validator run.

Out of scope for this tracer bullet:

- full corpus publication,
- final parser choice,
- AAT v2 vocabulary,
- parser-IR schema additions for warigaki or kunten,
- paragraph reconstruction beyond what current flat parser-IR can represent.

---

## File Structure

- Create: `src/abc/tools/parser_ir_vocabulary.clj`
  - Reads parser-IR JSON Schema as data and derives current node types.
  - Compares renderer policy maps against schema-derived node types.

- Create: `data/parser-ir-publication-policy-v0.json`
  - Machine-readable publication rendering policy for current parser-IR node types.
  - Hash source for plaintext `output_format_spec_hash`.

- Create: `src/abc/tools/parser_ir_publication_policy.clj`
  - Reads the policy artifact, exposes covered node types per renderer, and computes its JCS hash.

- Create: `src/abc/tools/parser_ir_plaintext.clj`
  - Pure parser-IR to plaintext renderer.
  - Public interface: `(render parser-ir)` and `(render-string parser-ir)`.

- Create: `src/abc/tools/parser_ir_tei.clj`
  - Pure parser-IR to TEI body renderer.
  - Public interface: `(render parser-ir)` returning body hiccup, char declarations, node counts, and omitted notes.

- Create: `src/abc/tools/materialize_publication.clj`
  - Reads parser-IR plus metadata/person fixture inputs.
  - Writes `plain.txt`, `tei.xml`, `plaintext.manifest.json`, `tei.manifest.json`, and `tei-validation-result.json`.

- Modify: `src/abc/tools/tei_header.clj`
  - Accept parser-IR-derived gaiji char declarations instead of hardcoding the current fixture declaration.

- Modify: `schemas/manifest.schema.json`
  - Add `plaintext` to `artifact_kind`.

- Modify: `deps.edn` and `flake.nix`
  - Expose `abc.tools.materialize-publication` as a Clojure alias and Nix app after the pure renderers pass.

- Modify tests:
  - `test/abc/tools/parser_ir_vocabulary_test.clj`
  - `test/abc/tools/parser_ir_publication_policy_test.clj`
  - `test/abc/tools/parser_ir_plaintext_test.clj`
  - `test/abc/tools/parser_ir_tei_test.clj`
  - `test/abc/tools/materialize_publication_test.clj`
  - `test/abc/tools/tei_header_unit_test.clj`
  - `test/abc/tools/tei_header_test.clj`
  - `test/abc/tools/validate_design_bundle_test.clj`

- Modify fixtures:
  - `examples/v0/example-work/plain.txt`
  - `examples/v0/example-work/tei.xml`
  - `examples/v0/example-work/manifest.json`
  - Generated publication manifests remain temporary design-bundle outputs in this tracer bullet and are not checked in.

---

### Task 1: Record the Parser-IR Publication Decision

**Files:**
- Create: `docs/adr/0025-parser-ir-publication-rendering.md`
- Modify: `docs/adr/0002-parser-evaluation.md`

**Interfaces:**
- Consumes: ADR 0007 external parser boundary and ADR 0023 mapping ownership.
- Produces: a durable decision that ABC owns parser-IR publication rendering while ab-validator owns parser execution and comparison.

- [ ] **Step 1: Write ADR 0025**

Create `docs/adr/0025-parser-ir-publication-rendering.md` with these decisions:

- ABC renders publication artifacts from parser-IR, not from Aozora source text.
- `../ab-validator` owns parser comparison, parser implementation, AAT extraction, and parser-IR export.
- ABC renderers are pure value transforms.
- File materialization and manifest construction are separate from renderers.
- Current flat parser-IR cannot reconstruct original paragraph boundaries with full fidelity; the first TEI body renderer emits a valid linear transcription and records structural limitations.
- Renderer coverage is schema-derived and fails closed when parser-IR adds node types.

- [ ] **Step 2: Update ADR 0002 implementation status**

In `docs/adr/0002-parser-evaluation.md`, replace the current implementation-status paragraph:

```markdown
Still Draft. ABC now validates an imported parser-IR fixture at the file
boundary, but no parser candidate reports have been produced or accepted, and
the repo still does not execute or select a parser candidate.
```

with:

```markdown
Still Draft. Parser candidate comparison remains in `../ab-validator`. ABC now
owns the downstream publication consumer contract: parser-IR can be validated,
materialized, and rendered to publication artifacts without executing parser
candidates locally.
```

- [ ] **Step 3: Verify doc references**

Run:

```bash
rg -n "ADR 0025|parser-IR publication|publication rendering" docs/adr docs/handoffs docs/superpowers/plans
```

Expected: `docs/adr/0025-parser-ir-publication-rendering.md` and this plan appear.

- [ ] **Step 4: Commit**

```bash
git add docs/adr/0025-parser-ir-publication-rendering.md docs/adr/0002-parser-evaluation.md docs/superpowers/plans/2026-07-03-parser-ir-publication-pipeline.md
git commit -m "docs: record parser IR publication rendering boundary"
```

### Task 2: Add Schema-Derived Renderer Coverage And Policy Artifact

**Files:**
- Create: `data/parser-ir-publication-policy-v0.json`
- Create: `src/abc/tools/parser_ir_vocabulary.clj`
- Create: `src/abc/tools/parser_ir_publication_policy.clj`
- Create: `test/abc/tools/parser_ir_vocabulary_test.clj`
- Create: `test/abc/tools/parser_ir_publication_policy_test.clj`

**Interfaces:**
- Produces: `(node-types schema) => #{"text" "ruby" "gaiji" "editor-note" "emphasis" "heading" "indentation" "page-break" "image" "caption" "quote"}`
- Produces: `(coverage-errors node-types renderer-name covered-types) => vector<string>`
- Produces: `(load-policy path) => policy map`
- Produces: `(renderer-covered-node-types policy renderer-name) => set<string>`
- Produces: `(policy-hash path) => sha256:<jcs-json-hash>`

**Policy artifact contract:**
- `data/parser-ir-publication-policy-v0.json` is the format-defining artifact for publication rendering in this tracer bullet.
- The policy artifact must contain `renderers.plaintext.node_policies` and `renderers.tei.node_policies`.
- Each renderer's `node_policies` keys must exactly match schema-derived parser-IR node types.
- Plaintext materialization hashes this policy artifact for `output_format_spec_hash`.
- ADR 0025 remains the boundary decision; it is not the byte-format identity artifact.

- [ ] **Step 1: Write failing tests and policy skeleton**

Create `test/abc/tools/parser_ir_vocabulary_test.clj`:

```clojure
(ns abc.tools.parser-ir-vocabulary-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def expected-node-types
  #{"text" "ruby" "gaiji" "editor-note" "emphasis" "heading"
    "indentation" "page-break" "image" "caption" "quote"})

(deftest node-types-come-from-parser-ir-schema-test
  (testing "current parser-IR node vocabulary is derived from schema consts"
    (is (= expected-node-types
           (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))))))

(deftest coverage-errors-test
  (testing "missing node policies are named"
    (is (= ["plaintext renderer is missing parser-IR node policy for image"]
           (vocab/coverage-errors #{"text" "image"} "plaintext" #{"text"})))))
```

Create `data/parser-ir-publication-policy-v0.json` with these required top-level fields and complete `node_policies` key sets:

```json
{
  "policy_id": "https://w3id.org/abc/policies/parser-ir-publication-v0",
  "policy_version": "0.1.0",
  "renderers": {
    "plaintext": {
      "output_format": "UTF-8 text/plain; parser-IR node order; no trailing newline added by policy",
      "node_policies": {
        "text": "append text",
        "ruby": "append ruby.base",
        "gaiji": "append gaiji.unicode when present, otherwise gaiji.raw_marker",
        "editor-note": "omit and report",
        "emphasis": "append text",
        "heading": "surround text with newlines",
        "indentation": "append text when present, otherwise omit and report",
        "page-break": "append newline",
        "image": "append alt when present, otherwise omit and report",
        "caption": "append text",
        "quote": "append text when present, otherwise omit and report"
      }
    },
    "tei": {
      "output_format": "TEI P5 XML body hiccup rendered under the ABC TEI profile",
      "node_policies": {
        "text": "paragraph inline text",
        "ruby": "ruby rb/rt with optional direction place",
        "gaiji": "g ref resolves to charDecl",
        "editor-note": "note",
        "emphasis": "hi",
        "heading": "body head",
        "indentation": "seg type indentation",
        "page-break": "pb",
        "image": "figure graphic with optional figDesc",
        "caption": "figDesc",
        "quote": "quote"
      }
    }
  }
}
```

Create `test/abc/tools/parser_ir_publication_policy_test.clj`:

```clojure
(ns abc.tools.parser-ir-publication-policy-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(deftest renderer-policy-covers-schema-node-types-test
  (let [schema-node-types (vocab/node-types
                           (files/read-json "schemas/parser-ir.schema.json"))
        p (policy/load-policy policy-path)]
    (doseq [renderer-name ["plaintext" "tei"]]
      (testing renderer-name
        (is (empty? (vocab/coverage-errors
                     schema-node-types
                     renderer-name
                     (policy/renderer-covered-node-types p renderer-name))))))))

(deftest policy-hash-is-jcs-json-test
  (testing "publication policy identity is canonical JSON, not source bytes"
    (is (= (hash/format-sha256
            (hash/sha256-json-jcs (files/read-json policy-path)))
           (policy/policy-hash policy-path)))))
```

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test
```

Expected: exits non-zero because `abc.tools.parser-ir-vocabulary` and `abc.tools.parser-ir-publication-policy` do not exist.

- [ ] **Step 3: Implement vocabulary helper**

Create `src/abc/tools/parser_ir_vocabulary.clj`:

```clojure
(ns abc.tools.parser-ir-vocabulary
  (:require [clojure.set :as set]))

(defn- definition-node-type [definition]
  (get-in definition ["allOf" 1 "properties" "type" "const"]))

(defn node-types [parser-ir-schema]
  (->> (get parser-ir-schema "$defs")
       vals
       (keep definition-node-type)
       set))

(defn coverage-errors [expected-node-types renderer-name covered-node-types]
  (->> (set/difference expected-node-types covered-node-types)
       sort
       (mapv #(str renderer-name " renderer is missing parser-IR node policy for " %))))
```

- [ ] **Step 4: Implement policy helper**

Create `src/abc/tools/parser_ir_publication_policy.clj`:

```clojure
(ns abc.tools.parser-ir-publication-policy
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]))

(defn load-policy [path]
  (files/read-json path))

(defn renderer-covered-node-types [policy renderer-name]
  (->> (get-in policy ["renderers" renderer-name "node_policies"])
       keys
       set))

(defn policy-hash [path]
  (hash/format-sha256
   (hash/sha256-json-jcs (load-policy path))))
```

- [ ] **Step 5: Verify**

Run:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test
```

Expected: `0 failures`, `0 errors`.

- [ ] **Step 6: Commit**

```bash
git add data/parser-ir-publication-policy-v0.json src/abc/tools/parser_ir_vocabulary.clj src/abc/tools/parser_ir_publication_policy.clj test/abc/tools/parser_ir_vocabulary_test.clj test/abc/tools/parser_ir_publication_policy_test.clj
git commit -m "test(parser-ir): derive renderer coverage from policy"
```

### Task 3: Add Parser-IR to Plaintext Renderer

**Files:**
- Create: `src/abc/tools/parser_ir_plaintext.clj`
- Create: `test/abc/tools/parser_ir_plaintext_test.clj`

**Interfaces:**
- Consumes: parser-IR maps shaped by `schemas/parser-ir.schema.json`.
- Produces: `(render parser-ir) => {:text string :node_counts map :omitted vector}`
- Produces: `(render-string parser-ir) => string`
- Produces: `covered-node-types => set<string>`

**Plaintext policy:**
- Must match `renderers.plaintext.node_policies` in `data/parser-ir-publication-policy-v0.json`.
- `text`: append `text`
- `ruby`: append `ruby.base`
- `gaiji`: append `gaiji.unicode` when present, otherwise append `gaiji.raw_marker`
- `editor-note`: omit and record `{:type "editor-note" :policy "omitted"}`
- `emphasis`: append `text`
- `heading`: surround `text` with newlines
- `indentation`: append `text` when present, otherwise append empty string and record omitted note
- `page-break`: append newline
- `image`: append `alt` when present, otherwise append empty string and record omitted note
- `caption`: append `text`
- `quote`: append `text` when present, otherwise append empty string and record omitted note

- [ ] **Step 1: Write failing tests**

Create tests that cover all current node types and coverage:

```clojure
(ns abc.tools.parser-ir-plaintext-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(def all-node-parser-ir
  {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
   "schema_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000001"
   "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "nodes" [{"type" "heading" "span" {"start" 0 "end" 1} "text" "H" "level" 1}
            {"type" "text" "span" {"start" 1 "end" 2} "text" "A"}
            {"type" "ruby" "span" {"start" 2 "end" 3} "ruby" {"base" "B" "reading" "b" "scope" "explicit"}}
            {"type" "gaiji" "span" {"start" 3 "end" 4} "gaiji" {"raw_marker" "※［＃x］" "unicode" "X" "resolved" true}}
            {"type" "gaiji" "span" {"start" 4 "end" 5} "gaiji" {"raw_marker" "※［＃y］" "resolved" false}}
            {"type" "editor-note" "span" {"start" 5 "end" 6} "note" {"raw" "［＃地付き］" "category" "indentation"}}
            {"type" "emphasis" "span" {"start" 6 "end" 7} "text" "C" "style" "boten"}
            {"type" "indentation" "span" {"start" 7 "end" 8} "depth" 2 "text" "D"}
            {"type" "page-break" "span" {"start" 8 "end" 9} "marker" "［＃改ページ］"}
            {"type" "image" "span" {"start" 9 "end" 10} "src" "fig.png" "alt" "ALT"}
            {"type" "caption" "span" {"start" 10 "end" 11} "text" "CAP"}
            {"type" "quote" "span" {"start" 11 "end" 12} "marker_type" "inline" "text" "Q"}]
   "warnings" []
   "errors" []})

(deftest render-string-test
  (testing "plaintext renders visible text policy for every current node type"
    (is (= "\nH\nABX※［＃y］CD\nALTCAPQ"
           (plaintext/render-string all-node-parser-ir)))))

(deftest render-metadata-test
  (testing "render returns omitted node notes"
    (let [result (plaintext/render all-node-parser-ir)]
      (is (= "editor-note" (:type (first (:omitted result)))))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "image" 1
              "caption" 1 "quote" 1}
             (:node_counts result))))))

(deftest coverage-test
  (testing "plaintext renderer covers current parser-IR schema node vocabulary"
    (is (empty? (vocab/coverage-errors
                 (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))
                 "plaintext"
                 plaintext/covered-node-types)))
    (is (= (policy/renderer-covered-node-types (policy/load-policy policy-path) "plaintext")
           plaintext/covered-node-types))))
```

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
bin/kaocha --focus abc.tools.parser-ir-plaintext-test
```

Expected: fails because the namespace does not exist.

- [ ] **Step 3: Implement renderer**

Create `src/abc/tools/parser_ir_plaintext.clj` with a private per-node renderer map keyed by node type. Keep `covered-node-types` as `(set (keys node-renderers))`. `render` must reduce over `parser-ir["nodes"]`, append text fragments, count node types, and collect omitted notes. `render-string` returns `(:text (render parser-ir))`.

- [ ] **Step 4: Verify**

Run the focused plaintext test and vocabulary test:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test --focus abc.tools.parser-ir-plaintext-test
```

Expected: all tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/parser_ir_plaintext.clj test/abc/tools/parser_ir_plaintext_test.clj
git commit -m "feat(parser-ir): render plaintext from parser IR"
```

### Task 4: Parameterize TEI Header Character Declarations

**Files:**
- Modify: `src/abc/tools/tei_header.clj`
- Create: `test/abc/tools/tei_header_unit_test.clj`
- Keep: `test/abc/tools/tei_header_test.clj` for schema-backed integration checks.

**Interfaces:**
- Consumes: optional `:char-declarations` in `tei-header/build` input.
- Produces: TEI header with no hardcoded gaiji fixture dependency.

**Character declaration shape:**

```clojure
{:xml-id "example-gaiji"
 :name "Example unresolved Aozora gaiji fixture"
 :desc "Design fixture for preserving an unresolved gaiji marker."
 :unicode nil
 :raw-marker "※［＃例字］"}
```

- [ ] **Step 1: Write failing tests**

Create `test/abc/tools/tei_header_unit_test.clj`. Do not add this test to `tei_header_test.clj`; that namespace has a `TEI_SCHEMA_PATH` fixture and `ABC_TEI_SCHEMA_SKIP=1` would skip the assertion.

```clojure
(ns abc.tools.tei-header-unit-test
  (:require [abc.tools.files :as files]
            [abc.tools.tei-header :as th]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(def example-record
  (delay (files/read-json "examples/v0/example-work/metadata-record.json")))

(def example-person
  (delay (files/read-json "examples/v0/example-persons/000879.json")))

(defn resolved-input []
  (let [persons-by-id {"000879" @example-person}]
    {:work (get @example-record "work")
     :contributors (mapv (fn [c]
                           {:relation-to-work (get c "relation_to_work")
                            :person (get persons-by-id (get c "person_id"))})
                         (get @example-record "contributors"))}))

(deftest char-declarations-are-input-driven-test
  (testing "header emits parser-IR supplied gaiji declarations"
    (let [input (assoc (resolved-input)
                       :char-declarations
                       [{:xml-id "gaiji-1"
                         :name "Unresolved gaiji"
                         :desc "Preserved unresolved source marker."
                         :unicode nil
                         :raw-marker "※［＃1-2-3］"}])
          s (th/emit-xml (th/build input))]
      (is (string/includes? s "xml:id=\"gaiji-1\""))
      (is (string/includes? s "Unresolved gaiji"))
      (is (not (string/includes? s "example-gaiji"))))))
```

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
bin/kaocha --focus abc.tools.tei-header-unit-test
```

Expected: fails because `tei-header/build` ignores `:char-declarations`.

- [ ] **Step 3: Implement header parameterization**

Modify `encoding-desc` to accept declarations. Emit:

- no `encodingDesc` when the declaration vector is empty,
- `[:encodingDesc (into [:charDecl] declaration-elements)]` when declarations exist,
- one `[:char {:xml/id (:xml-id declaration)} child-1 child-2]` element per declaration,
- `[:mapping {:type "unicode"} unicode]` only when `:unicode` is non-nil,
- `[:localProp {:name "rawMarker" :value raw-marker}]` only when `:raw-marker` is non-nil,
- `[:localProp {:name "charName" :value name}]` only when `:name` is non-nil,
- `[:desc desc]` only when `:desc` is non-nil.

Update `build` to pass `(:char-declarations input)` into `encoding-desc`.

- [ ] **Step 4: Verify**

Run:

```bash
bin/kaocha --focus abc.tools.tei-header-unit-test
```

Expected: all `abc.tools.tei-header-unit-test` tests pass.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/tei_header.clj test/abc/tools/tei_header_unit_test.clj
git commit -m "feat(tei): accept parser IR gaiji declarations"
```

### Task 5: Add Parser-IR to TEI Body Renderer

**Files:**
- Create: `src/abc/tools/parser_ir_tei.clj`
- Create: `test/abc/tools/parser_ir_tei_test.clj`

**Interfaces:**
- Consumes: parser-IR maps shaped by `schemas/parser-ir.schema.json`.
- Produces: `(render parser-ir) => {:body hiccup :char_declarations vector :node_counts map :omitted vector}`
- Produces: `covered-node-types => set<string>`

**TEI body policy:**
- Must match `renderers.tei.node_policies` in `data/parser-ir-publication-policy-v0.json`.
- `:body` is exactly `[:text [:body & body-children]]`.
- Inline runs are wrapped in `[:p ...]`. The first inline node opens a paragraph.
- `heading` closes any current paragraph and emits `[:head {:n (str level)} text]` as a direct child of `:body`.
- Body-level nodes such as `pb` and `figure` close the current paragraph before emission.
- `char_declarations` are ordered by first appearance in parser-IR node traversal and deduplicated by `:xml-id`, preserving the first declaration.
- Gaiji resolution invariant: every body `[:g {:ref "#X"} ...]` must have a header declaration `{:xml-id "X" ...}`. When `gaiji.reference` is present, `X` is that reference without a leading `#`; generated ids are used only when no reference is present.
- `text`: text content
- `ruby`: `[:ruby [:rb base] [:rt reading]]`; include `{:place direction}` when `ruby.direction` is present
- `gaiji`: `[:g {:ref "#<X>"}]`; collect a char declaration with `:xml-id X`; if no reference exists, generate `X` as `gaiji-<span.start>-<span.end>` and preserve `raw_marker` in the declaration
- `editor-note`: `[:note {:type category} raw]`
- `emphasis`: `[:hi {:rend style} text]`
- `indentation`: emits `[:seg {:type "indentation" :n (str depth)} text]` when text exists; otherwise records omitted note
- `page-break`: `[:pb {:n page_number}]` when page number exists, otherwise `[:pb]`
- `image`: `[:figure [:graphic {:url src}] [:figDesc alt]]` when `alt` exists; without `alt`, emit `[:figure [:graphic {:url src}]]` and rely on the existing figure warning rule
- `caption`: `[:figDesc text]`
- `quote`: `[:quote text]` when text exists; otherwise record omitted note

- [ ] **Step 1: Write failing tests**

Create `test/abc/tools/parser_ir_tei_test.clj` with:

```clojure
(ns abc.tools.parser-ir-tei-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-plaintext-test :refer [all-node-parser-ir]]
            [abc.tools.parser-ir-tei :as parser-ir-tei]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(defn hiccup-nodes [node]
  (tree-seq vector? rest node))

(deftest coverage-test
  (testing "TEI renderer covers current parser-IR schema node vocabulary"
    (is (empty? (vocab/coverage-errors
                 (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))
                 "TEI"
                 parser-ir-tei/covered-node-types)))
    (is (= (policy/renderer-covered-node-types (policy/load-policy policy-path) "tei")
           parser-ir-tei/covered-node-types))))

(deftest render-body-shape-test
  (testing "TEI renderer returns explicit text/body/paragraph shape"
    (let [result (parser-ir-tei/render all-node-parser-ir)
          [text-node [body-node & body-children]] (:body result)]
      (is (= :text text-node))
      (is (= :body body-node))
      (is (= :head (ffirst body-children)))
      (is (some #(= :p (first %)) body-children))
      (is (seq (:char_declarations result)))
      (is (= {"heading" 1 "text" 1 "ruby" 1 "gaiji" 2 "editor-note" 1
              "emphasis" 1 "indentation" 1 "page-break" 1 "image" 1
              "caption" 1 "quote" 1}
             (:node_counts result))))))

(deftest gaiji-reference-declaration-contract-test
  (testing "fixture gaiji.reference is preserved as ref and charDecl id"
    (let [result (parser-ir-tei/render
                  (files/read-json "examples/v0/example-work/parser-ir.json"))]
      (is (some #(= "example-gaiji" (:xml-id %))
                (:char_declarations result)))
      (is (some #(= [:g {:ref "#example-gaiji"}] %)
                (hiccup-nodes (:body result)))))))

(deftest char-declaration-order-test
  (testing "char declarations are first-appearance ordered and deduplicated"
    (let [parser-ir (assoc all-node-parser-ir "nodes"
                           [{"type" "gaiji" "span" {"start" 0 "end" 1}
                             "gaiji" {"raw_marker" "A" "reference" "gaiji-b" "resolved" false}}
                            {"type" "gaiji" "span" {"start" 1 "end" 2}
                             "gaiji" {"raw_marker" "B" "reference" "gaiji-a" "resolved" false}}
                            {"type" "gaiji" "span" {"start" 2 "end" 3}
                             "gaiji" {"raw_marker" "C" "reference" "gaiji-b" "resolved" false}}])
          result (parser-ir-tei/render parser-ir)]
      (is (= ["gaiji-b" "gaiji-a"]
             (mapv :xml-id (:char_declarations result)))))))
```

Do not put `TEI_SCHEMA_PATH`-dependent checks in this focused unit namespace. The XML/RNG/Schematron path is verified by `nix run .#validate-design-bundle` after materialization.

- [ ] **Step 2: Run tests to verify failure**

Run:

```bash
bin/kaocha --focus abc.tools.parser-ir-tei-test
```

Expected: fails because the namespace does not exist.

- [ ] **Step 3: Implement TEI renderer**

Create `src/abc/tools/parser_ir_tei.clj`. Keep XML serialization out of this namespace. Build hiccup vectors only. Reuse `abc.tools.parser-ir-vocabulary` for coverage tests, not at runtime.

- [ ] **Step 4: Verify focused tests**

Run:

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test --focus abc.tools.parser-ir-plaintext-test --focus abc.tools.parser-ir-tei-test --focus abc.tools.tei-header-unit-test
```

Expected: focused unit tests pass without relying on `ABC_TEI_SCHEMA_SKIP=1`.

- [ ] **Step 5: Verify through design-bundle TEI gates**

Run:

```bash
nix run .#validate-design-bundle
```

Expected: `design bundle validation ok`.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/parser_ir_tei.clj test/abc/tools/parser_ir_tei_test.clj
git commit -m "feat(parser-ir): render TEI body from parser IR"
```

### Task 6: Add Publication Materializer Tracer Bullet

**Files:**
- Create: `src/abc/tools/materialize_publication.clj`
- Create: `test/abc/tools/materialize_publication_test.clj`
- Modify: `schemas/manifest.schema.json`
- Modify: `deps.edn`
- Modify: `flake.nix`
- Modify: `src/abc/tools/validate_design_bundle.clj`

**Interfaces:**
- Consumes: parser-IR JSON file, metadata record JSON file, persons directory, output directory, generated timestamp.
- Produces: `plain.txt`, `tei.xml`, `plaintext.manifest.json`, `tei.manifest.json`, `tei-validation-result.json`.
- Produces CLI:

```bash
clojure -M:abc/materialize-publication examples/v0/example-work/parser-ir.json examples/v0/example-work/metadata-record.json examples/v0/example-persons out/publication --generated-at 2026-07-03T00:00:00Z
```

- [ ] **Step 1: Add failing tests for artifact kind**

Extend manifest schema tests so `"plaintext"` is accepted in `artifact_kind` and an unknown artifact kind is still rejected.

- [ ] **Step 2: Add `plaintext` artifact kind**

Modify `schemas/manifest.schema.json`:

```json
"enum": ["source", "parser-ir", "warnings", "plaintext", "tei", "rdf-view", "tokenized", "analysis", "failure"]
```

Rotate any fixtures that validate against the enum only if tests require it.

- [ ] **Step 3: Write failing materializer test**

Create `test/abc/tools/materialize_publication_test.clj` asserting:

- output files exist,
- plaintext contains `吾輩猫` from the current parser-IR example,
- TEI XML contains `<ruby>`,
- both manifests validate against `schemas/manifest.schema.json`,
- plaintext manifest `output_format_spec_hash` equals `parser-ir-publication-policy/policy-hash` for `data/parser-ir-publication-policy-v0.json`,
- TEI validation-result JSON validates against `schemas/tei-validation-result.schema.json`,
- repeated materialization with the same input and timestamp is byte-deterministic.

- [ ] **Step 4: Implement materializer**

Create `abc.tools.materialize-publication` with:

- pure helpers that call `parser-ir-plaintext/render` and `parser-ir-tei/render`,
- metadata/person resolution matching `test/abc/tools/tei_header_test.clj` fixture logic,
- file writing through existing deterministic JSON writer for manifests,
- TEI XML serialization through a new public `abc.tools.tei-header/hiccup->xml-string` helper extracted from the current private XML adapter,
- manifest construction through `abc.tools.manifest/artifact-manifest`.

For identity:

- `plaintext.manifest.json` uses `artifact_kind = "plaintext"`.
- `tei.manifest.json` uses `artifact_kind = "tei"`.
- both include parser IR schema hash and parser build/config/mapping hashes inherited from imported manifest inputs when available.
- `tei_profile_hash` is `sha256:` plus `files/sha256-file` of `schemas/tei-profile.odd`.
- `output_format_spec_hash` for TEI is the same `tei_profile_hash`.
- `output_format_spec_hash` for plaintext is `abc.tools.parser-ir-publication-policy/policy-hash` of `data/parser-ir-publication-policy-v0.json`.
- This tracer bullet keeps ADR 0012's ODD-as-profile identity for TEI. The existing `tei-profile-drift` flake check guards `.rng` and `.sch` drift; do not claim the TEI manifest identity captures generated validation artifact changes until a later ADR promotes those artifacts into manifest identity.

- [ ] **Step 5: Expose CLI and Nix app**

Modify `deps.edn`:

```clojure
:abc/materialize-publication {:main-opts ["-m" "abc.tools.materialize-publication"]}
```

Modify `flake.nix` to add an app named `materialize-publication` mirroring the existing `materialize-import` app.

- [ ] **Step 6: Wire design-bundle smoke**

Extend `validate-design-bundle` to materialize publication output in a temporary directory from checked-in fixtures and validate:

- `plain.txt` exists and is non-empty,
- `tei.xml` validates through existing XML, Relax NG, and Schematron gates,
- generated manifests validate,
- generated validation result validates.

- [ ] **Step 7: Verify**

Run:

```bash
bin/kaocha --focus abc.tools.materialize-publication-test --focus abc.tools.validate-design-bundle-test
nix run .#validate-design-bundle
```

Expected: focused tests pass and `design bundle validation ok`.

- [ ] **Step 8: Commit**

```bash
git add src/abc/tools/materialize_publication.clj test/abc/tools/materialize_publication_test.clj schemas/manifest.schema.json deps.edn flake.nix src/abc/tools/validate_design_bundle.clj
git commit -m "feat(parser-ir): materialize publication artifacts"
```

### Task 7: Update Fixtures and Documentation

**Files:**
- Modify: `examples/v0/example-work/plain.txt`
- Modify: `examples/v0/example-work/tei.xml`
- Modify: `examples/v0/example-work/manifest.json`
- Modify: `examples/v0/example-work/README.md`
- Modify: `docs/tei-validation.md`
- Modify: `docs/v0-design-bundle/README.md`

**Interfaces:**
- Consumes: materializer from Task 6.
- Produces: checked-in example publication artifacts that demonstrate parser-IR-driven TEI/plaintext.

- [ ] **Step 1: Generate fixture outputs**

Run the materializer into a temporary directory:

```bash
rm -rf out/parser-ir-publication-fixture
nix run .#materialize-publication -- examples/v0/example-work/parser-ir.json examples/v0/example-work/metadata-record.json examples/v0/example-persons out/parser-ir-publication-fixture --generated-at 2026-07-03T00:00:00Z
```

- [ ] **Step 2: Copy stable fixture outputs**

Copy only the files that belong in the design fixture:

```bash
cp out/parser-ir-publication-fixture/plain.txt examples/v0/example-work/plain.txt
cp out/parser-ir-publication-fixture/tei.xml examples/v0/example-work/tei.xml
```

Do not copy `plaintext.manifest.json`, `tei.manifest.json`, or `tei-validation-result.json` into `examples/v0/example-work/` in this tracer bullet. The design-bundle gate validates those generated files from a temporary materializer output directory.

Note the integration scope explicitly in review notes: the checked-in example fixture exercises 5 of 11 current parser-IR node types (`text`, `ruby`, `gaiji`, `editor-note`, `heading`). The synthetic focused renderer tests are what exercise all 11 node policies.

- [ ] **Step 3: Update docs**

Update `examples/v0/example-work/README.md` to state that `plain.txt` and `tei.xml` are parser-IR-driven publication fixtures.

Update `docs/tei-validation.md` with one sentence under "Validation Result Artifacts":

```markdown
The parser-IR publication materializer generates TEI from parser-IR body nodes and validates it through the same project Relax NG and Schematron gates.
```

Update `docs/v0-design-bundle/README.md` so the artifact table includes plaintext as a generated publication artifact.

- [ ] **Step 4: Verify fixture consistency**

Run:

```bash
nix run .#validate-design-bundle
git diff --check
```

Expected: `design bundle validation ok` and no whitespace errors.

- [ ] **Step 5: Commit**

```bash
git add examples/v0/example-work/plain.txt examples/v0/example-work/tei.xml examples/v0/example-work/README.md docs/tei-validation.md docs/v0-design-bundle/README.md
git commit -m "docs(parser-ir): refresh publication fixtures"
```

### Task 8: Final Verification

**Files:**
- All files changed by Tasks 1-7.

**Interfaces:**
- Consumes: complete parser-IR publication tracer bullet.
- Produces: green repository gate and clean working tree.

- [ ] **Step 1: Run focused renderer and materializer tests**

```bash
bin/kaocha --focus abc.tools.parser-ir-vocabulary-test --focus abc.tools.parser-ir-publication-policy-test --focus abc.tools.parser-ir-plaintext-test --focus abc.tools.parser-ir-tei-test --focus abc.tools.materialize-publication-test --focus abc.tools.tei-header-unit-test --focus abc.tools.validate-design-bundle-test
```

- [ ] **Step 2: Run design-bundle validation**

```bash
nix run .#validate-design-bundle
```

Expected: `design bundle validation ok`.

- [ ] **Step 3: Run full flake check**

```bash
nix flake check --print-build-logs
```

Expected: `all checks passed!` on supported local systems.

- [ ] **Step 4: Inspect final status**

```bash
git status --short
git log --oneline -8
```

Expected: clean working tree after the final commit.

---

## Incubation Notes

This plan is intentionally a tracer bullet. It should settle three design questions before broader publication work:

- whether current parser-IR is expressive enough for valid TEI/plaintext output,
- where gaiji declaration ownership belongs between body rendering and header rendering,
- whether TEI manifest identity should expand beyond the ODD profile hash to include generated validation artifacts after the tracer bullet proves the materialized path.

Do not broaden the implementation to full corpus rendering until this tracer bullet is green and reviewed.
