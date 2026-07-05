# Parser-IR Paragraph Layout Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add optional paragraph-level layout metadata to ABC Parser-IR and render it as TEI P5 `@rend` on `<p>` without affecting plaintext output.

**Architecture:** Parser-IR keeps `nodes[]` flat and carries layout on the existing top-level `paragraphs[]` rows. The TEI renderer already has a paragraph-table path; this change extends that path to attach `@rend` to the generated `<p>`. Plaintext remains node-text-only and ignores paragraph metadata.

**Tech Stack:** Clojure 1.12, JSON Schema draft 2020-12, Kaocha/clojure.test, ABC Parser-IR schema, ABC parser-IR TEI/plaintext renderers.

## Global Constraints

- Parser-IR schema changes are ABC-owned.
- Layout is paragraph metadata, not an inline `emphasis` node.
- TEI projection uses `@rend` tokens on `<p>`; it does not invent TEI semantic enrichment.
- Plaintext must contain no ruby, layout, or other metadata.
- Existing parser-IR documents without `paragraphs[].layout` remain valid.
- Compatibility registry entries for the previous parser-IR schema hash remain valid; new registry entries wait for ab-validator evidence against the new schema hash.

---

## File Structure

- Modify `schemas/parser-ir.schema.json`: add `$defs.paragraphLayout` and optional `layout` on `$defs.paragraph`.
- Modify `test/abc/tools/validate_design_bundle_test.clj`: schema acceptance tests for `burasage` and `chitsuki` paragraph layouts.
- Modify `src/abc/tools/parser_ir_tei.clj`: render paragraph layout to `@rend` in the paragraph-aware path.
- Modify `test/abc/tools/parser_ir_tei_test.clj`: TEI renderer tests for `burasage` and `chitsuki` layout.
- Modify `test/abc/tools/parser_ir_plaintext_test.clj`: regression test that paragraph layout does not appear in plaintext.
- Modify `docs/handoffs/parser-ir-level3-structure-delta.md`: add a short note that paragraph layout is now the ABC-owned Level 3 carrier for Aozora indentation/alignment layout.

## Task 1: Schema Accepts Paragraph Layout

**Files:**
- Modify: `schemas/parser-ir.schema.json`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces: `paragraphs[].layout` with these fields:
  - required `kind`, enum `["jisage", "burasage", "chitsuki", "jizume", "line-jisage"]`
  - required `source`, enum `["aat-block", "aat-style", "source-derived", "heuristic"]`
  - optional integer fields `indent`, `first_line_indent`, `continuation_indent`, `offset_from_end`, `width`
  - optional `align`, enum `["right"]`

- [ ] **Step 1: Write the failing schema test**

In `test/abc/tools/validate_design_bundle_test.clj`, after `parser-ir-schema-accepts-level3-paragraphs-test`, add:

```clojure
(deftest parser-ir-schema-accepts-paragraph-layout-test
  (testing "paragraph rows may carry Aozora layout metadata for TEI paragraph rendering"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "01")
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6
                                       "coordinate_system" "decoded_utf8"}
                               "text" "台詞"}
                              {"type" "text"
                               "span" {"start" 6 "end" 36
                                       "coordinate_system" "decoded_utf8"}
                               "text" "（大正十一年十二月）"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "burasage"
                                              "first_line_indent" 0
                                              "continuation_indent" 1
                                              "source" "aat-style"}}
                                   {"id" "p000001"
                                    "span" {"start" 6 "end" 36
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "chitsuki"
                                              "align" "right"
                                              "offset_from_end" 1
                                              "source" "aat-style"}}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (validate/validation-errors schema parser-ir))))))
```

- [ ] **Step 2: Verify RED**

Run:

```bash
bin/kaocha --focus 'abc.tools.validate-design-bundle-test/parser-ir-schema-accepts-paragraph-layout-test'
```

Expected: the focused test fails because `paragraph` currently has `additionalProperties: false` and does not allow `layout`.

- [ ] **Step 3: Add the schema definition**

In `schemas/parser-ir.schema.json`, add `"layout": { "$ref": "#/$defs/paragraphLayout" }` to `$defs.paragraph.properties`.

Add this sibling definition after `$defs.nodeRange`:

```json
"paragraphLayout": {
  "type": "object",
  "required": ["kind", "source"],
  "additionalProperties": false,
  "properties": {
    "kind": {
      "type": "string",
      "enum": ["jisage", "burasage", "chitsuki", "jizume", "line-jisage"]
    },
    "source": {
      "type": "string",
      "enum": ["aat-block", "aat-style", "source-derived", "heuristic"]
    },
    "indent": { "type": "integer", "minimum": 0 },
    "first_line_indent": { "type": "integer", "minimum": 0 },
    "continuation_indent": { "type": "integer", "minimum": 0 },
    "align": { "type": "string", "enum": ["right"] },
    "offset_from_end": { "type": "integer", "minimum": 0 },
    "width": { "type": "integer", "minimum": 0 }
  }
}
```

- [ ] **Step 4: Verify GREEN**

Run:

```bash
bin/kaocha --focus 'abc.tools.validate-design-bundle-test/parser-ir-schema-accepts-paragraph-layout-test'
```

Expected: the focused test passes.

- [ ] **Step 5: Commit**

```bash
git add schemas/parser-ir.schema.json test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): add paragraph layout schema"
```

## Task 2: TEI Renderer Emits Paragraph Layout as `@rend`

**Files:**
- Modify: `src/abc/tools/parser_ir_tei.clj`
- Modify: `test/abc/tools/parser_ir_tei_test.clj`

**Interfaces:**
- Consumes: `paragraphs[].layout` from Task 1.
- Produces: TEI Hiccup paragraph tags with `:rend` attributes:
  - `jisage indent(N)`
  - `burasage first(N) rest(M)`
  - `chitsuki align(right) offset-from-end(N)`
  - `jizume width(N)`
  - `line-jisage indent(N)`

- [ ] **Step 1: Write the failing TEI renderer test**

In `test/abc/tools/parser_ir_tei_test.clj`, after `paragraph-table-renders-body-paragraphs-and-back-source-note-test`, add:

```clojure
(deftest paragraph-layout-renders-as-tei-rend-test
  (testing "paragraph layout metadata is projected to TEI p@rend"
    (let [parser-ir {"nodes" [{"type" "text"
                               "span" {"start" 0 "end" 6}
                               "text" "台詞"}
                              {"type" "text"
                               "span" {"start" 6 "end" 36}
                               "text" "（大正十一年十二月）"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 6
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"
                                    "layout" {"kind" "burasage"
                                              "first_line_indent" 0
                                              "continuation_indent" 1
                                              "source" "aat-style"}}
                                   {"id" "p000001"
                                    "span" {"start" 6 "end" 36
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 1 "end" 2}
                                    "role" "body"
                                    "source_pointer" "blocks[1]"
                                    "classification" "direct"
                                    "layout" {"kind" "chitsuki"
                                              "align" "right"
                                              "offset_from_end" 1
                                              "source" "aat-style"}}]}
          result (parser-ir-tei/render parser-ir)]
      (is (= [:text
              [:body
               [:p {:rend "burasage first(0) rest(1)"} "台詞"]
               [:p {:rend "chitsuki align(right) offset-from-end(1)"} "（大正十一年十二月）"]]]
             (:body result)))
      (is (= {"text" 2} (:node_counts result)))
      (is (empty? (:omitted result))))))
```

- [ ] **Step 2: Verify RED**

Run:

```bash
bin/kaocha --focus 'abc.tools.parser-ir-tei-test/paragraph-layout-renders-as-tei-rend-test'
```

Expected: the focused test fails because paragraphs currently render as `[:p "text"]` with no attributes.

- [ ] **Step 3: Add paragraph layout rendering helpers**

In `src/abc/tools/parser_ir_tei.clj`, add these helpers after `append-structural-child`:

```clojure
(defn- layout-rend [layout]
  (case (get layout "kind")
    "jisage" (str "jisage indent(" (get layout "indent") ")")
    "burasage" (str "burasage first(" (get layout "first_line_indent") ") rest(" (get layout "continuation_indent") ")")
    "chitsuki" (str "chitsuki align(" (get layout "align") ") offset-from-end(" (get layout "offset_from_end") ")")
    "jizume" (str "jizume width(" (get layout "width") ")")
    "line-jisage" (str "line-jisage indent(" (get layout "indent") ")")
    nil))

(defn- paragraph-attrs [paragraph]
  (when-let [rend (some-> (get paragraph "layout") layout-rend)]
    {:rend rend}))
```

Change `flush-paragraph` to preserve optional paragraph attributes:

```clojure
(defn- flush-paragraph [acc]
  (if (seq (:current-paragraph acc))
    (let [paragraph-node (into (cond-> [:p]
                                 (:current-paragraph-attrs acc)
                                 (conj (:current-paragraph-attrs acc)))
                               (:current-paragraph acc))]
      (assoc (append-structural-child acc paragraph-node)
             :current-paragraph []
             :current-paragraph-attrs nil))
    (assoc acc :current-paragraph-attrs nil)))
```

Change `initial-acc` to include:

```clojure
:current-paragraph-attrs nil
```

Change `render-paragraph-row` so body and unknown paragraphs set attributes before rendering their node slice:

```clojure
(defn- render-paragraph-row [nodes acc paragraph]
  (let [{start "start" end "end"} (paragraph-range paragraph)
        node-slice (subvec nodes start end)]
    (case (get paragraph "role")
      "body" (-> acc
                 (assoc :current-paragraph-attrs (paragraph-attrs paragraph))
                 (render-node-seq node-slice)
                 flush-paragraph)
      "source-note" (-> acc
                        flush-paragraph
                        (render-node-seq node-slice)
                        flush-paragraph)
      (-> acc
          (assoc :current-paragraph-attrs (paragraph-attrs paragraph))
          (render-node-seq node-slice)
          flush-paragraph))))
```

- [ ] **Step 4: Verify GREEN**

Run:

```bash
bin/kaocha --focus 'abc.tools.parser-ir-tei-test/paragraph-layout-renders-as-tei-rend-test'
```

Expected: the focused test passes.

- [ ] **Step 5: Run adjacent TEI tests**

Run:

```bash
bin/kaocha --focus 'abc.tools.parser-ir-tei-test'
```

Expected: all parser-IR TEI renderer tests pass.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/parser_ir_tei.clj test/abc/tools/parser_ir_tei_test.clj
git commit -m "feat(parser-ir): render paragraph layout as TEI rend"
```

## Task 3: Plaintext and Bundle Verification

**Files:**
- Modify: `test/abc/tools/parser_ir_plaintext_test.clj`
- Modify: `docs/handoffs/parser-ir-level3-structure-delta.md`

**Interfaces:**
- Consumes: Task 1 schema and Task 2 TEI behavior.
- Produces: a regression test and handoff note stating plaintext ignores paragraph layout.

- [ ] **Step 1: Add the plaintext regression test**

In `test/abc/tools/parser_ir_plaintext_test.clj`, after `source-note-back-matter-is-separated-test`, add:

```clojure
(deftest paragraph-layout-does-not-enter-plaintext-test
  (testing "plaintext ignores paragraph layout metadata and emits content only"
    (is (= "台詞"
           (plaintext/render-string
            {"nodes" [{"type" "text"
                       "span" {"start" 0 "end" 6}
                       "text" "台詞"}]
             "paragraphs" [{"id" "p000000"
                            "span" {"start" 0 "end" 6
                                    "coordinate_system" "decoded_utf8"}
                            "span_source" "direct"
                            "node_range" {"start" 0 "end" 1}
                            "role" "body"
                            "source_pointer" "blocks[0]"
                            "classification" "direct"
                            "layout" {"kind" "burasage"
                                      "first_line_indent" 0
                                      "continuation_indent" 1
                                      "source" "aat-style"}}]})))))
```

- [ ] **Step 2: Verify plaintext behavior**

Run:

```bash
bin/kaocha --focus 'abc.tools.parser-ir-plaintext-test/paragraph-layout-does-not-enter-plaintext-test'
```

Expected: the focused test passes. If it fails, fix only plaintext metadata leakage; do not render layout markers into plain text.

- [ ] **Step 3: Add the handoff note**

Append this section to `docs/handoffs/parser-ir-level3-structure-delta.md`:

```markdown
## 2026-07-05 Paragraph Layout Delta

ABC Parser-IR now treats Aozora paragraph layout as optional metadata on
`paragraphs[]` rows. The first layout vocabulary covers `jisage`, `burasage`,
`chitsuki`, `jizume`, and `line-jisage`. The TEI renderer projects this
metadata to `p@rend`; the plaintext renderer ignores it and emits content only.

New compatibility evidence from ab-validator is still required before adding
registry entries for the new parser-IR schema hash.
```

- [ ] **Step 4: Run focused verification**

Run:

```bash
bin/kaocha --focus 'abc.tools.validate-design-bundle-test/parser-ir-schema-accepts-paragraph-layout-test' --focus 'abc.tools.parser-ir-tei-test/paragraph-layout-renders-as-tei-rend-test' --focus 'abc.tools.parser-ir-plaintext-test/paragraph-layout-does-not-enter-plaintext-test'
```

Expected: all three focused tests pass.

- [ ] **Step 5: Run publication-adjacent tests**

Run:

```bash
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha --focus 'abc.tools.validate-design-bundle-test' --focus 'abc.tools.parser-ir-tei-test' --focus 'abc.tools.parser-ir-plaintext-test' --focus 'abc.tools.materialize-publication-test'
```

Expected: all focused namespaces pass. `ABC_TEI_SCHEMA_SKIP=1` is the established local path when `TEI_SCHEMA_PATH` is not set; the Nix validation app exercises the TEI schema path separately.

- [ ] **Step 6: Run schema hash command and record the new hash in the commit message body**

Run:

```bash
clojure -M -e "(require '[abc.tools.manifest :as m]) (println (m/schema-hash \"schemas/parser-ir.schema.json\"))"
```

Expected: prints a `sha256:` hash different from `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`.

- [ ] **Step 7: Commit**

```bash
git add test/abc/tools/parser_ir_plaintext_test.clj docs/handoffs/parser-ir-level3-structure-delta.md
git commit -m "test(parser-ir): assert paragraph layout stays out of plaintext"
```

## Final Verification

Run:

```bash
git diff --check
ABC_TEI_SCHEMA_SKIP=1 bin/kaocha --focus 'abc.tools.validate-design-bundle-test' --focus 'abc.tools.parser-ir-tei-test' --focus 'abc.tools.parser-ir-plaintext-test' --focus 'abc.tools.materialize-publication-test'
clojure -M:abc/validate-design-bundle
```

Expected:

- `git diff --check` prints nothing and exits 0.
- Focused Kaocha run passes.
- `clojure -M:abc/validate-design-bundle` passes with no parser-IR schema validation, publication, or compatibility errors.

## Self-Review

- Spec coverage: the plan implements paragraph layout schema support, TEI `p@rend` projection, plaintext content-only behavior, and the ab-validator compatibility handoff note.
- Placeholder scan: no `TODO`, `TBD`, `fill in`, or unnamed code steps are present.
- Type consistency: the plan uses JSON key `layout`, schema definition `paragraphLayout`, Clojure helper `layout-rend`, and TEI `:rend` consistently.
