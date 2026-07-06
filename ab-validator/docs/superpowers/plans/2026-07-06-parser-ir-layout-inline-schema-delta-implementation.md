# Parser-IR Layout Inline Schema Delta Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rotate ABC Parser-IR and ab-validator conversion so every currently measured inline-layout/heading schema gap maps into TEI P5 or an ABC custom preservation lane, reducing the `parser_ir_schema` publication-closure owner count from 23 to 0.

**Architecture:** ABC owns the Parser-IR schema, plaintext renderer, TEI renderer, schema hash, and publication policy. ab-validator consumes the rotated ABC schema, emits the new IR shapes from measured AAT, regenerates the mapping/audit evidence, and verifies that the closure report has no Parser-IR-schema-owned gaps. TEI Level 3 is an admission lane; the broader goal is complete IR-to-publication mapping through TEI plus ABC-owned custom preservation schema where TEI is not exact.

**Tech Stack:** JSON Schema draft 2020-12, Clojure 1.12 ABC renderers/tests, Rust ab-validator converter, serde_json, jsonschema, jq, Nix flakes, ABC batch materializer (`clojure -M:abc/materialize-publications-batch`).

## Global Constraints

- Keep `heading.text` required for compatibility, search, and metadata-free plaintext.
- Add `heading.inline_children` as optional structured content; when present, its visible projection must equal `heading.text`.
- Add one typed inline layout-scope node, `layout-span`, instead of one node type per Aozora marker.
- `layout-span` must be valid both as a top-level `node` and as an `inlineNode`.
- Plaintext output must not include ruby readings, Aozora markers, layout metadata, or TEI/profile metadata; it emits visible body text only plus existing explicit source-note separation.
- ABC owns final TEI/profile/custom schema semantics; ab-validator must not invent ABC admission policy.
- Schema hash rotation follows the ADR 0024 precedent: ABC changes schema first, then ab-validator syncs `data/abc-schemas/schemas/parser-ir.schema.json`, then mapping/audit compatibility evidence is regenerated.
- Batch materialization must use ABC's batch path when validating many generated documents; do not spawn one ABC Clojure process per parser-IR file.
- Existing paragraph/source-note Level 3 behavior must remain unchanged.
- Do not weaken existing source-authority, compatibility, or publication-coverage gates.

---

## File Structure

### ABC Repo (`../abc`)

- Modify `schemas/parser-ir.schema.json`
  - Add `$defs.layoutSpanNode`.
  - Add `$defs.layoutScope`.
  - Add `$defs.layoutScopeSource`.
  - Add `layout-span` to `$defs.node.oneOf` and `$defs.inlineNode.oneOf`.
  - Add optional `inline_children` to `$defs.headingNode.properties`.
- Modify `src/abc/tools/parser_ir_tei.clj`
  - Render `heading.inline_children` inside `head` when present.
  - Render `layout-span` to TEI `hi`/`seg` with `@rend` that carries the ABC profile projection.
  - Preserve existing paragraph-table rendering.
- Modify `src/abc/tools/parser_ir_plaintext.clj`
  - Render `layout-span` as visible text or visible inline children only.
  - Keep heading plaintext based on `heading.text`.
- Modify `data/parser-ir-publication-policy-v0.json`
  - Add `layout-span` to both TEI and plaintext covered node sets.
  - Record TEI projection as profile/custom-preservation-backed, not exact pure TEI.
- Modify `test/abc/tools/validate_design_bundle_test.clj`
  - Add schema acceptance/rejection tests for `heading.inline_children` and `layout-span`.
- Modify `test/abc/tools/parser_ir_tei_test.clj`
  - Add renderer tests for heading inline children and layout-span.
- Modify `test/abc/tools/parser_ir_plaintext_test.clj`
  - Add plaintext tests proving layout metadata and ruby readings stay out of plaintext.

### ab-validator Repo

- Modify `data/abc-schemas/schemas/parser-ir.schema.json`
  - Sync byte-for-byte from `../abc/schemas/parser-ir.schema.json` after ABC schema rotation.
- Modify `crates/ab-aat-to-parser-ir/src/convert.rs`
  - Emit `heading.inline_children` for representable heading content.
  - Emit `layout-span` for AAT `font_size`, `tcy`, inline `keigakomi`, and inline `yokogumi`.
  - Stop emitting Parser-IR-schema-delta losses for the now-representable inline layout and heading-child cases.
- Modify `crates/ab-aat-to-parser-ir/tests/integration.rs`
  - Replace the old heading-flattening-loss test with a structural preservation test.
  - Add layout-span conversion tests.
- Modify mapping generation/reporting inputs if the generated mapping still classifies the 23 rows as Parser-IR-schema gaps.
  - Expected touched area: `reports/aat-fidelity/aat_parser_ir_mapping/mapper.py` and generated `data/aat-to-parser-ir-mapping-v1.json`.
- Regenerate `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json` and `.md`.
  - Expected result: `classified_but_not_admitted.counts_by_owner.parser_ir_schema` absent or `0`.
- Update this plan's completion note after implementation only if the verification commands below pass.

---

### Task 1: ABC Schema Red Tests

**Files:**
- Modify: `../abc/test/abc/tools/validate_design_bundle_test.clj`
- Reads: `../abc/schemas/parser-ir.schema.json`

**Interfaces:**
- Consumes: ABC test helper patterns already in `validate_design_bundle_test.clj`.
- Produces: Failing tests proving the current schema lacks `heading.inline_children` and `layout-span`.

- [ ] **Step 1: Add schema tests for structured heading content**

Append this test near the existing Parser-IR schema tests in `../abc/test/abc/tools/validate_design_bundle_test.clj`:

```clojure
(deftest parser-ir-schema-accepts-heading-inline-children-test
  (testing "heading nodes preserve structured inline content while retaining text"
    (is (nil?
         (validate-parser-ir
          {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
           "schema_hash" (current-parser-ir-schema-hash)
           "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                     "encoding" "Shift_JIS"
                     "normalization" "source"}
           "nodes" [{"type" "heading"
                     "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                     "text" "東京"
                     "level" 2
                     "inline_children" [{"type" "ruby"
                                         "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                                         "ruby" {"base" "東京"
                                                 "reading" "とうきょう"
                                                 "scope" "explicit"
                                                 "direction" "right"}}]}]
           "warnings" []
           "errors" []})))))
```

- [ ] **Step 2: Add schema tests for layout-span**

Append these tests in the same file:

```clojure
(deftest parser-ir-schema-accepts-layout-span-test
  (testing "layout-span is a typed inline publication-layout scope"
    (is (nil?
         (validate-parser-ir
          {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
           "schema_hash" (current-parser-ir-schema-hash)
           "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                     "encoding" "Shift_JIS"
                     "normalization" "source"}
           "nodes" [{"type" "layout-span"
                     "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                     "text" "12"
                     "inline_children" [{"type" "text"
                                         "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                         "text" "12"}]
                     "layout" {"kind" "tcy"
                               "source" "aat-inline"
                               "marker" "縦中横"}}
                    {"type" "emphasis"
                     "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
                     "style" "bold"
                     "inline_children" [{"type" "layout-span"
                                         "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
                                         "text" "横"
                                         "layout" {"kind" "yokogumi"
                                                   "source" "aat-inline"
                                                   "direction" "horizontal"}}]}]
           "warnings" []
           "errors" []})))))

(deftest parser-ir-schema-rejects-invalid-layout-span-test
  (testing "layout-span requires typed layout metadata"
    (is (some?
         (validate-parser-ir
          {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
           "schema_hash" (current-parser-ir-schema-hash)
           "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                     "encoding" "Shift_JIS"
                     "normalization" "source"}
           "nodes" [{"type" "layout-span"
                     "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                     "text" "12"
                     "layout" {"source" "aat-inline"}}]
           "warnings" []
           "errors" []})))))
```

- [ ] **Step 3: Run the tests and confirm the red state**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected result before schema implementation:

```text
FAIL in (parser-ir-schema-accepts-heading-inline-children-test)
FAIL in (parser-ir-schema-accepts-layout-span-test)
```

The rejection test may already pass; the two acceptance tests must fail before Task 2.

- [ ] **Step 4: Commit the red tests**

```bash
cd ../abc
git add test/abc/tools/validate_design_bundle_test.clj
git commit -m "test(parser-ir): cover layout-span schema delta"
```

---

### Task 2: ABC Parser-IR Schema Delta

**Files:**
- Modify: `../abc/schemas/parser-ir.schema.json`
- Test: `../abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes: Red tests from Task 1.
- Produces: A rotated Parser-IR schema that accepts `heading.inline_children` and `layout-span`.

- [ ] **Step 1: Add `layout-span` to the node unions**

In `../abc/schemas/parser-ir.schema.json`, add this `$ref` to both `$defs.node.oneOf` and `$defs.inlineNode.oneOf` after `emphasisNode`:

```json
{ "$ref": "#/$defs/layoutSpanNode" }
```

- [ ] **Step 2: Add `layoutScopeSource` and `layoutScope` definitions**

Insert these definitions near `paragraphLayoutSource`:

```json
"layoutScopeSource": {
  "type": "string",
  "enum": ["aat-inline", "aat-block", "source-derived", "heuristic"]
},
"layoutScope": {
  "oneOf": [
    {
      "type": "object",
      "required": ["kind", "source", "size_type", "level"],
      "additionalProperties": false,
      "properties": {
        "kind": { "const": "font-size" },
        "source": { "$ref": "#/$defs/layoutScopeSource" },
        "size_type": { "type": "string" },
        "level": { "type": "integer", "minimum": 0 },
        "marker": { "type": ["string", "null"] }
      }
    },
    {
      "type": "object",
      "required": ["kind", "source"],
      "additionalProperties": false,
      "properties": {
        "kind": { "const": "tcy" },
        "source": { "$ref": "#/$defs/layoutScopeSource" },
        "marker": { "type": ["string", "null"] }
      }
    },
    {
      "type": "object",
      "required": ["kind", "source"],
      "additionalProperties": false,
      "properties": {
        "kind": { "const": "keigakomi" },
        "source": { "$ref": "#/$defs/layoutScopeSource" },
        "border": { "type": ["string", "null"] },
        "marker": { "type": ["string", "null"] }
      }
    },
    {
      "type": "object",
      "required": ["kind", "source", "direction"],
      "additionalProperties": false,
      "properties": {
        "kind": { "const": "yokogumi" },
        "source": { "$ref": "#/$defs/layoutScopeSource" },
        "direction": { "type": "string", "enum": ["horizontal"] },
        "marker": { "type": ["string", "null"] }
      }
    }
  ]
}
```

- [ ] **Step 3: Add `layoutSpanNode`**

Insert this definition near `emphasisNode`:

```json
"layoutSpanNode": {
  "allOf": [
    { "$ref": "#/$defs/baseNode" },
    {
      "type": "object",
      "additionalProperties": false,
      "required": ["type", "span", "text", "layout"],
      "properties": {
        "type": { "const": "layout-span" },
        "span": { "$ref": "#/$defs/span" },
        "text": { "type": "string" },
        "inline_children": {
          "type": "array",
          "items": { "$ref": "#/$defs/inlineNode" }
        },
        "layout": { "$ref": "#/$defs/layoutScope" }
      }
    }
  ]
}
```

- [ ] **Step 4: Add `heading.inline_children`**

In `$defs.headingNode.properties`, add:

```json
"inline_children": {
  "type": "array",
  "items": { "$ref": "#/$defs/inlineNode" }
}
```

Keep `required` unchanged as `["type", "span", "text", "level"]`.

- [ ] **Step 5: Run the ABC schema tests**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"
```

Expected result:

```text
0 failures, 0 errors
```

- [ ] **Step 6: Run the design bundle validator**

Run from `../abc`:

```bash
nix run .#validate-design-bundle
```

Expected result:

```text
Design bundle validation passed
```

- [ ] **Step 7: Commit the schema delta**

```bash
cd ../abc
git add schemas/parser-ir.schema.json test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): add layout-span and heading inline children"
```

---

### Task 3: ABC TEI Renderer Delta

**Files:**
- Modify: `../abc/src/abc/tools/parser_ir_tei.clj`
- Modify: `../abc/test/abc/tools/parser_ir_tei_test.clj`
- Modify: `../abc/data/parser-ir-publication-policy-v0.json`

**Interfaces:**
- Consumes: `layout-span` and `heading.inline_children` schema from Task 2.
- Produces: TEI rendering for the new node and structured headings.

- [ ] **Step 1: Add a failing heading inline-children TEI test**

Append this test to `../abc/test/abc/tools/parser_ir_tei_test.clj`:

```clojure
(deftest heading-inline-children-render-inside-head-test
  (testing "heading inline_children render structured TEI inside head"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "heading"
                             "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                             "text" "東京"
                             "level" 2
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                                                 "ruby" {"base" "東京"
                                                         "reading" "とうきょう"
                                                         "scope" "explicit"
                                                         "direction" "right"}}]}]})
          head (some #(when (= :head (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:head {:n "2"}
              [:ruby {:type "furigana" :rend "right"}
               [:rb "東京"]
               [:rt "とうきょう"]]]
             head))
      (is (= {"heading" 1 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))
```

- [ ] **Step 2: Add a failing layout-span TEI test**

Append this test to the same file:

```clojure
(deftest layout-span-renders-profile-rend-test
  (testing "layout-span projects typed layout facts to TEI rend"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "layout-span"
                             "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                             "text" "12"
                             "inline_children" [{"type" "text"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                 "text" "12"}]
                             "layout" {"kind" "tcy"
                                       "source" "aat-inline"
                                       "marker" "縦中横"}}
                            {"type" "layout-span"
                             "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                             "text" "大"
                             "layout" {"kind" "font-size"
                                       "source" "aat-inline"
                                       "size_type" "large"
                                       "level" 1}}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:hi {:rend "abc:tcy marker(縦中横)"} "12"]
              [:hi {:rend "abc:font-size type(large) level(1)"} "大"]]
             paragraph))
      (is (= {"layout-span" 2 "text" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))
```

- [ ] **Step 3: Run the TEI tests and confirm the red state**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.parser-ir-tei-test 'clojure.test) (clojure.test/run-tests 'abc.tools.parser-ir-tei-test)"
```

Expected result before implementation:

```text
FAIL in (heading-inline-children-render-inside-head-test)
ERROR in (layout-span-renders-profile-rend-test)
```

- [ ] **Step 4: Add layout-span rendering helpers**

In `../abc/src/abc/tools/parser_ir_tei.clj`, add this helper after `layout-rend`:

```clojure
(defn- inline-layout-rend [layout]
  (case (get layout "kind")
    "font-size" (let [size-type (get layout "size_type")
                      level (get layout "level")]
                  (when (and size-type (some? level))
                    (str "abc:font-size type(" size-type ") level(" level ")")))
    "tcy" (if-let [marker (get layout "marker")]
            (str "abc:tcy marker(" marker ")")
            "abc:tcy")
    "keigakomi" (if-let [border (get layout "border")]
                  (str "abc:keigakomi border(" border ")")
                  "abc:keigakomi")
    "yokogumi" "abc:yokogumi direction(horizontal)"
    nil))
```

- [ ] **Step 5: Refactor inline-child wrapping for reuse**

In `../abc/src/abc/tools/parser_ir_tei.clj`, add this helper before `render-emphasis-node`:

```clojure
(defn- render-inline-wrapper [acc children text depth wrapper]
  (if (seq children)
    (let [before-count (count (:current-paragraph acc))
          rendered (render-inline-children acc children depth)
          inline-fragment (subvec (vec (:current-paragraph rendered)) before-count)]
      (assoc rendered
             :current-paragraph
             (conj (subvec (vec (:current-paragraph acc)) 0 before-count)
                   (into wrapper inline-fragment))))
    (append-inline acc (conj wrapper text))))
```

Then change `render-emphasis-node` to:

```clojure
(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (render-inline-wrapper acc
                          (seq (get node "inline_children"))
                          (get node "text")
                          depth
                          [:hi {:rend (get node "style")}]))
```

- [ ] **Step 6: Add `render-layout-span-node`**

Add this function after `render-emphasis-node`:

```clojure
(defn- render-layout-span-node
  ([acc node] (render-layout-span-node acc node 0))
  ([acc node depth]
   (if-let [rend (some-> (get node "layout") inline-layout-rend)]
     (render-inline-wrapper acc
                            (seq (get node "inline_children"))
                            (get node "text")
                            depth
                            [:hi {:rend rend}])
     (mark-omitted acc "layout-span"))))
```

- [ ] **Step 7: Render heading inline children**

Replace `render-heading-node` with:

```clojure
(defn- render-heading-node
  ([acc node] (render-heading-node acc node 0))
  ([acc node depth]
   (let [children (seq (get node "inline_children"))
         base (-> acc flush-paragraph flush-division)]
     (if children
       (let [scratch (assoc base :current-paragraph [])
             rendered (render-inline-children scratch children depth)
             head-fragment (:current-paragraph rendered)]
         (-> rendered
             (assoc :current-paragraph [])
             (update :current-division conj
                     (into [:head {:n (str (get node "level"))}]
                           head-fragment))))
       (update base :current-division conj
               [:head {:n (str (get node "level"))}
                (get node "text")])))))
```

- [ ] **Step 8: Add layout-span to renderer coverage**

In `node-renderers`, add:

```clojure
"layout-span" render-layout-span-node
```

- [ ] **Step 9: Update TEI publication policy coverage**

In `../abc/data/parser-ir-publication-policy-v0.json`, add `layout-span` to the TEI renderer node map with this policy text:

```json
"layout-span": "render as TEI hi@rend with abc:* profile tokens; exact source marker identity remains in ABC custom preservation evidence"
```

If the file stores node coverage as an array, add `"layout-span"` to that TEI array and add the sentence above to the adjacent notes object.

- [ ] **Step 10: Run TEI tests**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.parser-ir-tei-test 'clojure.test) (clojure.test/run-tests 'abc.tools.parser-ir-tei-test)"
```

Expected result:

```text
0 failures, 0 errors
```

- [ ] **Step 11: Commit TEI rendering**

```bash
cd ../abc
git add src/abc/tools/parser_ir_tei.clj test/abc/tools/parser_ir_tei_test.clj data/parser-ir-publication-policy-v0.json
git commit -m "feat(parser-ir): render inline layout spans to TEI"
```

---

### Task 4: ABC Plaintext Renderer Delta

**Files:**
- Modify: `../abc/src/abc/tools/parser_ir_plaintext.clj`
- Modify: `../abc/test/abc/tools/parser_ir_plaintext_test.clj`
- Modify: `../abc/data/parser-ir-publication-policy-v0.json`

**Interfaces:**
- Consumes: `layout-span` from Task 2.
- Produces: Plaintext renderer coverage with metadata-free output.

- [ ] **Step 1: Add failing plaintext tests**

Append these tests to `../abc/test/abc/tools/parser_ir_plaintext_test.clj`:

```clojure
(deftest layout-span-plaintext-renders-visible-text-only-test
  (testing "layout-span metadata does not enter plaintext"
    (is (= "12大"
           (plaintext/render-string
            {"nodes" [{"type" "layout-span"
                       "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                       "text" "fallback"
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "12"
                                                   "reading" "じゅうに"
                                                   "scope" "explicit"}}]
                       "layout" {"kind" "tcy"
                                 "source" "aat-inline"
                                 "marker" "縦中横"}}
                      {"type" "layout-span"
                       "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                       "text" "大"
                       "layout" {"kind" "font-size"
                                 "source" "aat-inline"
                                 "size_type" "large"
                                 "level" 1}}]})))))

(deftest heading-inline-children-do-not-change-plaintext-test
  (testing "heading plaintext uses required visible text field"
    (is (= "\n東京\n"
           (plaintext/render-string
            {"nodes" [{"type" "heading"
                       "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                       "text" "東京"
                       "level" 2
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "東京"
                                                   "reading" "とうきょう"
                                                   "scope" "explicit"}}]}]})))))
```

- [ ] **Step 2: Run plaintext tests and confirm the red state**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.parser-ir-plaintext-test 'clojure.test) (clojure.test/run-tests 'abc.tools.parser-ir-plaintext-test)"
```

Expected result before implementation:

```text
FAIL in (layout-span-plaintext-renders-visible-text-only-test)
```

- [ ] **Step 3: Add layout-span plaintext rendering**

In `../abc/src/abc/tools/parser_ir_plaintext.clj`, add this function after `render-emphasis-node`:

```clojure
(defn- render-layout-span-node
  ([acc node] (render-layout-span-node acc node 0))
  ([acc node depth]
   (if-let [children (seq (get node "inline_children"))]
     (if (and (present-text? (get node "text"))
              (inline-children-need-visible-text-fallback? children))
       (append-text acc (get node "text"))
       (render-inline-children acc children depth))
     (append-text acc (get node "text")))))
```

In `node-renderers`, add:

```clojure
"layout-span" render-layout-span-node
```

- [ ] **Step 4: Update plaintext publication policy coverage**

In `../abc/data/parser-ir-publication-policy-v0.json`, add `layout-span` to the plaintext renderer node map with this policy text:

```json
"layout-span": "emit visible text or visible inline children only; omit layout metadata and ruby readings"
```

- [ ] **Step 5: Run plaintext and policy coverage tests**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.parser-ir-plaintext-test 'abc.tools.parser-ir-tei-test 'clojure.test) (clojure.test/run-tests 'abc.tools.parser-ir-plaintext-test 'abc.tools.parser-ir-tei-test)"
```

Expected result:

```text
0 failures, 0 errors
```

- [ ] **Step 6: Commit plaintext rendering**

```bash
cd ../abc
git add src/abc/tools/parser_ir_plaintext.clj test/abc/tools/parser_ir_plaintext_test.clj data/parser-ir-publication-policy-v0.json
git commit -m "feat(parser-ir): render layout spans in plaintext"
```

---

### Task 5: ABC Schema Hash Rotation and Batch Materialization Check

**Files:**
- Modify: ABC fixture files only if `validate-design-bundle` reports stale schema hashes.
- Reads: `../abc/flake.nix`, `../abc/deps.edn`, generated Parser-IR fixtures.

**Interfaces:**
- Consumes: ABC schema/renderers from Tasks 2-4.
- Produces: A validated ABC schema hash and a fast batch materialization path for later evidence runs.

- [ ] **Step 1: Run the full design bundle validation**

Run from `../abc`:

```bash
nix run .#validate-design-bundle
```

Expected result:

```text
Design bundle validation passed
```

If this reports stale schema hash fields in checked-in fixtures, update those exact fixture `schema_hash` fields to the newly computed hash printed by the validator, then rerun the same command.

- [ ] **Step 2: Verify batch materializer CLI availability**

Run from `../abc`:

```bash
clojure -M:abc/materialize-publications-batch --help
```

Expected result: the command prints usage for batch materialization and exits 0. If it requires input flags before printing help, run the equivalent Nix app listed in `flake.nix` for materialization help.

- [ ] **Step 3: Run a small batch materialization smoke**

Use the existing parser-IR examples that `validate-design-bundle` already accepts. If the repo has a checked-in example directory under `examples/ab-validator-output`, run:

```bash
clojure -M:abc/materialize-publications-batch \
  --input-dir examples/ab-validator-output \
  --output-dir /tmp/abc-layout-span-batch-smoke
```

Expected result:

```text
materialized
```

If the current batch CLI expects a manifest path rather than `--input-dir`, inspect `src/abc/tools/materialize_publication.clj` and run the smallest supported batch command against the same example inputs. Record the exact working command in the commit message body.

- [ ] **Step 4: Commit ABC validation updates**

```bash
cd ../abc
git status --short
git add schemas/parser-ir.schema.json data/parser-ir-publication-policy-v0.json test/abc/tools src/abc/tools
git commit -m "test(parser-ir): validate layout schema rotation"
```

If there are no remaining staged changes after Tasks 2-4, skip this commit and record the validation commands in the ab-validator handoff report in Task 10.

---

### Task 6: Sync ABC Schema Into ab-validator

**Files:**
- Modify: `data/abc-schemas/schemas/parser-ir.schema.json`
- Test: `tests/aat-parser-ir-mapping-policy-smoke.sh`

**Interfaces:**
- Consumes: rotated `../abc/schemas/parser-ir.schema.json`.
- Produces: ab-validator local schema copy with byte-for-byte parity.

- [ ] **Step 1: Copy the schema from ABC**

Run from `/home/bor/Projects/ab-validator`:

```bash
cp ../abc/schemas/parser-ir.schema.json data/abc-schemas/schemas/parser-ir.schema.json
```

This is a mechanical sync from ABC's owned schema. Do not edit the copied file manually.

- [ ] **Step 2: Verify byte-for-byte parity**

Run:

```bash
diff -u ../abc/schemas/parser-ir.schema.json data/abc-schemas/schemas/parser-ir.schema.json
```

Expected result: no output, exit 0.

- [ ] **Step 3: Run current mapping policy smoke to capture expected failure**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
```

Expected result before mapping regeneration: failure mentioning `target_parser_ir_schema_hash` mismatch, or success if the smoke computes the hash dynamically. If it succeeds, continue; if it fails only because of the old hash, Task 8 regenerates the mapping.

- [ ] **Step 4: Commit schema sync**

```bash
git add data/abc-schemas/schemas/parser-ir.schema.json
git commit -m "chore(parser-ir): sync layout-span schema from ABC"
```

---

### Task 7: ab-validator Converter Red Tests

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs`
- Reads: `crates/ab-aat-to-parser-ir/src/convert.rs`

**Interfaces:**
- Consumes: synced schema from Task 6.
- Produces: Failing Rust tests for heading inline children and layout-span conversion.

- [ ] **Step 1: Replace old heading flattening-loss assertions**

In `crates/ab-aat-to-parser-ir/tests/integration.rs`, rename `heading_visible_projection_records_measured_flattening_losses` to:

```rust
fn heading_preserves_structured_inline_children()
```

Replace the assertions at the end of the test with:

```rust
    let heading = output
        .parser_ir
        .pointer("/nodes/1")
        .expect("heading node should follow indentation node");
    assert_eq!(heading["type"], "heading");
    assert_eq!(heading["text"], "FGRS");
    assert_eq!(
        heading.pointer("/inline_children/0/type"),
        Some(&json!("layout-span"))
    );
    assert_eq!(
        heading.pointer("/inline_children/0/layout/kind"),
        Some(&json!("font-size"))
    );
    assert_eq!(
        heading.pointer("/inline_children/1/type"),
        Some(&json!("gaiji"))
    );
    assert_eq!(
        heading.pointer("/inline_children/2/type"),
        Some(&json!("ruby"))
    );
    assert_eq!(
        heading.pointer("/inline_children/3/type"),
        Some(&json!("emphasis"))
    );
    assert!(
        !has_divergence_record(
            &output,
            "LOSS",
            "blocks[].children[].heading.content[].font_size",
            None,
        ),
        "font_size inside heading should be represented after schema delta"
    );
    assert!(
        !has_divergence_record(
            &output,
            "LOSS",
            "blocks[].children[].heading.content[].ruby",
            None,
        ),
        "ruby inside heading should be represented after schema delta"
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
```

- [ ] **Step 2: Add inline layout-span conversion test**

Append this test near `preserves_nested_inline_container_children`:

```rust
#[test]
fn converts_inline_layout_scopes_to_layout_span() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "layout-span-conversion",
        "meta": base_meta(
            "utf-8",
            "sha256:dadadadadadadadadadadadadadadadadadadadadadadadadadadadadadadada",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [
                {"kind": "font_size", "size_type": "large", "level": 1, "content": [{"kind": "text", "value": "大"}]},
                {"kind": "tcy", "content": [{"kind": "text", "value": "12"}]},
                {"kind": "keigakomi", "content": [{"kind": "text", "value": "囲"}]},
                {"kind": "yokogumi", "content": [{"kind": "text", "value": "横"}]}
            ]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions::default(),
    })
    .unwrap();

    let kinds: Vec<_> = output.parser_ir["nodes"]
        .as_array()
        .expect("nodes")
        .iter()
        .map(|node| node.pointer("/layout/kind").and_then(Value::as_str))
        .collect();
    assert_eq!(
        kinds,
        vec![Some("font-size"), Some("tcy"), Some("keigakomi"), Some("yokogumi")]
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/layout/size_type"),
        Some(&json!("large"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/layout/level"),
        Some(&json!(1))
    );
    assert!(
        output.parser_ir["nodes"]
            .as_array()
            .expect("nodes")
            .iter()
            .all(|node| node["type"] == "layout-span")
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

If `Value` is not already imported in the test module, add:

```rust
use serde_json::Value;
```

- [ ] **Step 3: Run the tests and confirm red state**

Run:

```bash
cargo test -p ab-aat-to-parser-ir heading_preserves_structured_inline_children converts_inline_layout_scopes_to_layout_span
```

Expected result before implementation:

```text
FAILED
```

The failure should be schema validation or missing `layout-span`, not a panic in test setup.

- [ ] **Step 4: Commit red tests**

```bash
git add crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "test(parser-ir): require layout-span conversion"
```

---

### Task 8: ab-validator Converter Implementation

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs`
- Test: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: Red tests from Task 7.
- Produces: Converter emits `layout-span` and `heading.inline_children`.

- [ ] **Step 1: Add layout object builder**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add this helper near other inline helpers:

```rust
fn layout_scope(node: &Value) -> Option<Value> {
    match node["kind"].as_str()? {
        "font_size" => Some(json!({
            "kind": "font-size",
            "source": "aat-inline",
            "size_type": node["size_type"].as_str().unwrap_or("unknown"),
            "level": node["level"].as_u64().unwrap_or(0),
        })),
        "tcy" => Some(json!({
            "kind": "tcy",
            "source": "aat-inline",
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        "keigakomi" => Some(json!({
            "kind": "keigakomi",
            "source": "aat-inline",
            "border": node.get("border").and_then(Value::as_str),
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        "yokogumi" => Some(json!({
            "kind": "yokogumi",
            "source": "aat-inline",
            "direction": "horizontal",
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        _ => None,
    }
}
```

- [ ] **Step 2: Add `layout-span` node emitter**

Add this helper near `map_inline_to_nodes`:

```rust
fn map_layout_span_to_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<u64> {
    let text = visible_content_text(
        node.get("content"),
        recorder,
        &format!("{path}.content"),
        Some("layout-span.text"),
    )?;
    let end = offset + utf8_len(&text);
    let span = map_span(node.get("span"), offset, end, recorder, path)?;
    let inline_children = inline_children_nodes(
        node.get("content"),
        recorder,
        synthetic_warnings,
        offset,
        &format!("{path}.content"),
        depth + 1,
    )?;
    let layout = layout_scope(node)
        .with_context(|| format!("unsupported layout-span kind at {path}"))?;
    nodes.push(json!({
        "type": "layout-span",
        "span": span,
        "text": text,
        "inline_children": inline_children,
        "layout": layout,
    }));
    Ok(end)
}
```

If `with_context` is not in scope, import it from `anyhow` at the top:

```rust
use anyhow::{bail, Context, Result};
```

- [ ] **Step 3: Route inline layout kinds to `layout-span`**

In `map_inline_to_nodes`, replace the branch for:

```rust
"font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi" => { ... }
```

with:

```rust
"font_size" | "tcy" | "keigakomi" | "yokogumi" => {
    map_layout_span_to_node(
        node,
        nodes,
        recorder,
        synthetic_warnings,
        offset,
        path,
        depth,
    )
}
"caption" => {
    let kind = node["kind"].as_str().unwrap_or("");
    let pointer = format!("{path}.{kind}");
    recorder.record(
        "UNSUPPORTED",
        Some(pointer.as_str()),
        Some("emphasis(?)"),
        None,
        None,
    )?;
    let text = visible_content_text(
        node.get("content"),
        recorder,
        &format!("{path}.content"),
        Some("(emphasis.text)"),
    )?;
    let end = offset + utf8_len(&text);
    let span = map_span(node.get("span"), offset, end, recorder, path)?;
    nodes.push(json!({
        "type": "emphasis",
        "span": span,
        "text": text,
        "style": kind,
    }));
    Ok(end)
}
```

- [ ] **Step 4: Update inline-child node construction**

Where `inline_children_nodes` currently maps `"font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi"` into emphasis-like children, change it so `font_size`, `tcy`, `keigakomi`, and `yokogumi` produce:

```rust
json!({
    "type": "layout-span",
    "span": synthetic_span(offset, end),
    "text": text,
    "inline_children": inline_children,
    "layout": layout_scope(node).expect("layout scope for inline layout node"),
})
```

Keep `caption` on the old emphasis-style path until a separate caption policy delta exists.

- [ ] **Step 5: Build heading inline children**

In the `"heading"` branch of `map_block`, replace the flat `visible_content_text` call with both visible text and children:

```rust
let text = visible_content_text(
    block.get("content"),
    recorder,
    &format!("{path}.heading.content"),
    Some("heading.text"),
)?;
let inline_children = inline_children_nodes(
    block.get("content"),
    recorder,
    synthetic_warnings,
    current,
    &format!("{path}.heading.content"),
    0,
)?;
```

Then push the heading node as:

```rust
nodes.push(json!({
    "type": "heading",
    "span": span,
    "text": text,
    "level": block.get("level").and_then(Value::as_u64).unwrap_or(1),
    "inline_children": inline_children,
}));
```

Keep the `heading.level` ambiguity record. Keep `heading.style` loss until ABC defines a typed heading-style field.

- [ ] **Step 6: Remove obsolete schema-delta loss records for representable inline layout**

In `visible_inline_text` or the equivalent function that records measured loss for `"style" | "font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi"`, split the branch:

```rust
"style" | "caption" => {
    let kind = node["kind"].as_str().unwrap_or("");
    let container_pointer = format!("{path}.{kind}");
    record_measured_loss(
        recorder,
        container_pointer.as_str(),
        None,
        Some(json!(kind)),
    )?;
    visible_content_text(
        node.get("content"),
        recorder,
        &format!("{path}.content"),
        Some("(emphasis.text)"),
    )
}
"font_size" | "tcy" | "keigakomi" | "yokogumi" => visible_content_text(
    node.get("content"),
    recorder,
    &format!("{path}.content"),
    Some("layout-span.text"),
)
```

This keeps unresolved style/caption evidence while stopping the schema-delta losses for newly representable layout scopes.

- [ ] **Step 7: Run targeted Rust tests**

Run:

```bash
cargo test -p ab-aat-to-parser-ir heading_preserves_structured_inline_children converts_inline_layout_scopes_to_layout_span preserves_nested_inline_container_children
```

Expected result:

```text
test result: ok
```

- [ ] **Step 8: Run full crate tests**

Run:

```bash
cargo test -p ab-aat-to-parser-ir
```

Expected result:

```text
test result: ok
```

- [ ] **Step 9: Commit converter implementation**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-ir): emit layout spans from AAT"
```

---

### Task 9: Mapping Regeneration and Compatibility Evidence

**Files:**
- Modify: `data/aat-to-parser-ir-mapping-v1.json`
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.summary.json` if regenerated by the existing generator.
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn` if audit regeneration changes candidates.
- Modify: mapping generator only if it still emits old parser-IR target pointers for new layout/heading behavior.

**Interfaces:**
- Consumes: converter implementation from Task 8 and ABC schema hash from Task 6.
- Produces: measured mapping artifact compatible with the new Parser-IR schema hash.

- [ ] **Step 1: Regenerate the measured mapping artifact**

Run the existing mapping generation command used by this repo. If unsure, locate it first:

```bash
rg -n "aat_parser_ir_mapping|mapping-generation|generate.*mapping|aat-to-parser-ir-mapping-v1" justfile reports scripts docs/superpowers/plans
```

Then run the repo's canonical command. The expected output file is:

```text
data/aat-to-parser-ir-mapping-v1.json
```

The generated artifact must have:

```bash
jq -e '.target_parser_ir_schema_hash | startswith("sha256:")' data/aat-to-parser-ir-mapping-v1.json
jq -e '.mapping_version | type == "string"' data/aat-to-parser-ir-mapping-v1.json
```

- [ ] **Step 2: Verify mapping points at existing AAT and Parser-IR schema fields**

Run:

```bash
bash tests/aat-parser-ir-mapping-policy-smoke.sh
```

Expected result:

```text
PASS
```

- [ ] **Step 3: Run ab-aat-to-parser-ir CLI smoke**

Run:

```bash
bash tests/aat-to-parser-ir-cli-smoke.sh
```

Expected result:

```text
PASS
```

- [ ] **Step 4: Run an audit sample before the full corpus**

Use a small existing AAT directory from `/db` or the current smoke fixtures. The command shape is:

```bash
target/debug/ab-aat-to-parser-ir audit-corpus \
  --aat-dir /db/ab-validator/aat-corpus/aozora2html-full-latest/aat/aozora2html-adapter \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --summary-json /tmp/layout-span-audit-summary.json \
  --report-md /tmp/layout-span-audit-report.md \
  --compat-edn-out /tmp/layout-span-compat.edn \
  --jobs 24 \
  --limit 200
```

If the CLI does not support `--limit`, create a temporary directory with 200 symlinks to AAT JSON files and point `--aat-dir` at that directory.

Expected result:

```text
0 conversion failures
```

- [ ] **Step 5: Commit mapping regeneration**

```bash
git add data/aat-to-parser-ir-mapping-v1.json docs/superpowers/reports
git commit -m "data(parser-ir): regenerate mapping for layout-span schema"
```

If only `data/aat-to-parser-ir-mapping-v1.json` changed, stage only that file.

---

### Task 10: Publication Coverage Regeneration

**Files:**
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- Modify: `docs/handoffs/parser-ir-layout-inline-schema-delta.md`

**Interfaces:**
- Consumes: updated mapping and converter.
- Produces: report showing the `parser_ir_schema` closure lane is closed, while policy/custom lanes remain visible.

- [ ] **Step 1: Regenerate the publication coverage report**

Run:

```bash
just parser-ir-publication-coverage-report
```

Expected result:

```text
docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
docs/superpowers/reports/2026-07-06-ir-publication-coverage.md
```

- [ ] **Step 2: Assert no Parser-IR-schema-owned gaps remain**

Run:

```bash
jq -e '(.closure_gaps.classified_but_not_admitted.counts_by_owner.parser_ir_schema // 0) == 0' \
  docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected result:

```text
true
```

- [ ] **Step 3: Assert true unsupported remains zero**

Run:

```bash
jq -e '.closure_gaps.true_unsupported_gaps.count == 0' \
  docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected result:

```text
true
```

- [ ] **Step 4: Write the handoff**

Run:

```bash
SCHEMA_HASH="$(jq -r '.target_parser_ir_schema_hash' data/aat-to-parser-ir-mapping-v1.json)"
```

Then create `docs/handoffs/parser-ir-layout-inline-schema-delta.md` with this content, expanding `$SCHEMA_HASH` in the schema-hash bullet:

```markdown
# Parser-IR Layout Inline Schema Delta Handoff

Date: 2026-07-06

## Goal

The goal is complete IR publication closure through TEI P5 plus ABC custom
preservation schema, not pure TEI Level 2/3. This slice closes the
Parser-IR-schema-owned inline-layout and heading-content gaps.

## ABC Changes Required

- `heading.inline_children` is accepted and rendered inside TEI `head`.
- `layout-span` is accepted as both root node and inline node.
- TEI renders layout spans as `hi@rend` with `abc:*` profile tokens.
- Plaintext renders visible text only and omits ruby readings/layout metadata.

## ab-validator Evidence

- Updated Parser-IR schema hash: `$SCHEMA_HASH`
- Publication coverage true unsupported rows: `0`
- Publication coverage Parser-IR-schema-owned rows: `0`

## Remaining Lanes

- Policy-owned TEI projections remain for style rendition, heading jisage, figure metadata, and accent projection.
- Custom-schema-owned preservation remains for source identity, provenance metrics, span coordinate precision, unresolved gaiji reason, and exact marker identity where TEI is not exact.
```

After writing the handoff, run `rg -n '\$SCHEMA_HASH' docs/handoffs/parser-ir-layout-inline-schema-delta.md`; it must return no matches.

- [ ] **Step 5: Run coverage smoke**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
just parser-ir-publication-coverage-smoke
```

Expected result:

```text
PASS
```

- [ ] **Step 6: Commit coverage reports**

```bash
git add docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json \
        docs/superpowers/reports/2026-07-06-ir-publication-coverage.md \
        docs/handoffs/parser-ir-layout-inline-schema-delta.md \
        tests/parser-ir-publication-coverage-smoke.sh
git commit -m "docs(parser-ir): close layout schema coverage lane"
```

---

### Task 11: Cross-Repo Final Verification

**Files:**
- Reads all touched files in both repos.

**Interfaces:**
- Consumes: all prior tasks.
- Produces: verified cross-repo state ready for ABC local integration and ab-validator main.

- [ ] **Step 1: Verify ABC tests**

Run from `../abc`:

```bash
clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test 'abc.tools.parser-ir-tei-test 'abc.tools.parser-ir-plaintext-test 'clojure.test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test 'abc.tools.parser-ir-tei-test 'abc.tools.parser-ir-plaintext-test)"
nix run .#validate-design-bundle
```

Expected result:

```text
0 failures, 0 errors
Design bundle validation passed
```

- [ ] **Step 2: Verify ab-validator tests**

Run from `/home/bor/Projects/ab-validator`:

```bash
cargo test -p ab-aat-to-parser-ir
bash tests/aat-parser-ir-mapping-policy-smoke.sh
bash tests/aat-to-parser-ir-cli-smoke.sh
bash tests/parser-ir-publication-coverage-smoke.sh
just parser-ir-publication-coverage-smoke
```

Expected result:

```text
test result: ok
PASS
```

- [ ] **Step 3: Verify report invariants**

Run:

```bash
jq -e '
  .closure_gaps.true_unsupported_gaps.count == 0 and
  ((.closure_gaps.classified_but_not_admitted.counts_by_owner.parser_ir_schema // 0) == 0)
' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
```

Expected result:

```text
true
```

- [ ] **Step 4: Run whitespace checks**

Run from each repo:

```bash
git diff --check
```

Expected result: no output, exit 0.

- [ ] **Step 5: Commit any final ab-validator documentation changes**

```bash
git status --short
git add docs/superpowers/plans/2026-07-06-parser-ir-layout-inline-schema-delta-implementation.md \
        docs/superpowers/specs/2026-07-06-parser-ir-layout-inline-schema-delta-design.md
git commit -m "docs(parser-ir): plan layout inline schema delta"
```

If the plan/spec were committed before execution, skip this commit.

---

## Self-Review

- Spec coverage: This plan implements the two schema capabilities from the design: `heading.inline_children` and typed inline `layout-span`. It also preserves plaintext visible-text behavior and states the broader TEI-or-custom-schema goal.
- Red-flag scan: The plan uses no deferred-work instructions.
- Type consistency: JSON keys are `inline_children`, `layout-span`, `layout`, `kind`, `source`, `size_type`, `level`, `marker`, `border`, and `direction`, matching the design spec. `node_range` remains unrelated to this slice.
- Boundary check: ABC changes happen first, then ab-validator syncs the schema and regenerates evidence. The plan does not ask ab-validator to define ABC admission policy.
- Performance check: batch materialization is explicitly used for corpus-scale ABC validation; this avoids the prior one-process-per-file bottleneck.
