# Parser-IR Emphasis Inline Children Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve ruby and other inline semantic nodes inside parser-IR emphasis/container contexts so ABC can render TEI P5 nested phrase markup instead of flattened text.

**Architecture:** ABC owns the parser-IR schema and rendering semantics, so implementation starts in `../abc` with schema and renderer support. ab-validator then vendors the rotated schema, emits transitional `emphasis.inline_children` while retaining legacy `text`, and regenerates measured five-parser compatibility evidence.

**Tech Stack:** Clojure/JSON Schema/TEI hiccup renderer in `../abc`; Rust/serde_json/jsonschema in `ab-validator`; `just`, `cargo`, `clojure -M:test:kaocha`, and `nix run .#validate-design-bundle` for verification.

## Global Constraints

- ABC remains the parser-IR schema and TEI renderer owner.
- ab-validator remains the measured parser-IR producer and divergence sidecar owner.
- `emphasis.text` remains valid during migration.
- New `emphasis.inline_children` must preserve phrase-level parser-IR nodes recursively.
- Plaintext must not emit ruby readings or metadata.
- Parser-IR schema hash rotation must be explicit and compatibility-registry entries must coexist by exact schema hash.
- Full five-parser audit must remain `89,169 / 89,169` successful conversions.
- Generated corpus artifacts stay out of git; committed artifacts are reports, schemas, fixtures, tests, and compatibility candidates only.
- Unrelated untracked ortho-detect docs in ab-validator must not be modified.

---

## File Structure

ABC files:

- Modify `../abc/schemas/parser-ir.schema.json`: add `$defs.inlineNode` and transitional `emphasis.inline_children`.
- Modify `../abc/src/abc/tools/parser_ir_tei.clj`: render nested inline children inside `<hi>`.
- Modify `../abc/src/abc/tools/parser_ir_plaintext.clj`: render nested inline children as visible plaintext only.
- Modify `../abc/test/abc/tools/validate_design_bundle_test.clj`: schema acceptance/rejection tests.
- Modify `../abc/test/abc/tools/parser_ir_tei_test.clj`: nested `<hi>` / `<ruby type="furigana">` rendering tests.
- Modify `../abc/test/abc/tools/parser_ir_plaintext_test.clj`: nested plaintext policy test.
- Modify ABC schema-hash fixture values named by `nix run .#validate-design-bundle` errors in the same task that rotates the schema hash.

ab-validator files:

- Modify `data/abc-schemas/schemas/parser-ir.schema.json`: copy rotated ABC schema after ABC task lands.
- Modify `crates/ab-aat-to-parser-ir/src/convert.rs`: emit `inline_children` for style/font-size/tcy/keigakomi/caption/yokogumi containers.
- Modify `crates/ab-aat-to-parser-ir/tests/integration.rs`: converter and validation tests for inline children.
- Regenerate `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`.
- Regenerate `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`.
- Regenerate `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`.
- Update or add a short handoff/report if the schema hash and compatibility identity changed materially.

---

### Task 1: ABC Schema Accepts Transitional Inline Children

**Files:**
- Modify: `../abc/schemas/parser-ir.schema.json`
- Modify: `../abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces: parser-IR `emphasis` node shape with required `type`, `span`, `style`, and at least one of `text` or `inline_children`.
- Consumes later: ABC TEI/plaintext renderers and ab-validator vendored schema.

- [ ] **Step 1: Add failing schema acceptance tests**

In `../abc/test/abc/tools/validate_design_bundle_test.clj`, add `[abc.tools.schema :as schema]` to the namespace `:require` list. Then add tests near the existing parser-IR schema tests:

```clojure
(deftest parser-ir-schema-accepts-emphasis-inline-children-test
  (testing "emphasis can carry recursive inline children while retaining legacy text"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "style" "bold"
                               "text" "東京"
                               "inline_children" [{"type" "ruby"
                                                   "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                   "ruby" {"base" "東京"
                                                           "reading" "とうきょう"
                                                           "scope" "explicit"
                                                           "direction" "right"}}]}]
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors schema parser-ir))))))

(deftest parser-ir-schema-rejects-empty-emphasis-test
  (testing "emphasis must carry either legacy text or structured inline children"
    (let [schema (files/read-json "schemas/parser-ir.schema.json")
          parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" "sha256:0000000000000000000000000000000000000000000000000000000000000002"
                               "encoding" "Shift_JIS"
                               "normalization" "source"}
                     "nodes" [{"type" "emphasis"
                               "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                               "style" "bold"}]
                     "warnings" []
                     "errors" []}]
      (is (seq (schema/validation-errors schema parser-ir))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run from `../abc`:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.validate-design-bundle-test/parser-ir-schema-accepts-emphasis-inline-children-test --focus abc.tools.validate-design-bundle-test/parser-ir-schema-rejects-empty-emphasis-test
```

Expected: the acceptance test fails because `inline_children` is not allowed yet, and the rejection test may fail because legacy schema error shape differs.

- [ ] **Step 3: Update parser-IR schema**

In `../abc/schemas/parser-ir.schema.json`, add `$defs.inlineNode` after `$defs.node`:

```json
"inlineNode": {
  "oneOf": [
    { "$ref": "#/$defs/textNode" },
    { "$ref": "#/$defs/rubyNode" },
    { "$ref": "#/$defs/gaijiNode" },
    { "$ref": "#/$defs/editorNoteNode" },
    { "$ref": "#/$defs/emphasisNode" },
    { "$ref": "#/$defs/imageNode" },
    { "$ref": "#/$defs/pageBreakNode" },
    { "$ref": "#/$defs/lineBreakNode" }
  ]
},
```

Then replace the `emphasisNode` object requirements with:

```json
"required": ["type", "span", "style"],
"anyOf": [
  { "required": ["text"] },
  { "required": ["inline_children"] }
],
"properties": {
  "type": { "const": "emphasis" },
  "span": { "$ref": "#/$defs/span" },
  "text": { "type": "string" },
  "inline_children": {
    "type": "array",
    "items": { "$ref": "#/$defs/inlineNode" }
  },
  "style": { "type": "string" }
}
```

Keep `additionalProperties: false`.

- [ ] **Step 4: Run focused tests to verify they pass**

Run from `../abc`:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.validate-design-bundle-test/parser-ir-schema-accepts-emphasis-inline-children-test --focus abc.tools.validate-design-bundle-test/parser-ir-schema-rejects-empty-emphasis-test
```

Expected: both focused tests pass.

- [ ] **Step 5: Commit ABC schema change**

```bash
cd /home/bor/Projects/abc
git add schemas/parser-ir.schema.json test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): accept emphasis inline children"
```

---

### Task 2: ABC Renderers Consume Inline Children

**Files:**
- Modify: `../abc/src/abc/tools/parser_ir_tei.clj`
- Modify: `../abc/src/abc/tools/parser_ir_plaintext.clj`
- Modify: `../abc/test/abc/tools/parser_ir_tei_test.clj`
- Modify: `../abc/test/abc/tools/parser_ir_plaintext_test.clj`

**Interfaces:**
- Consumes: schema-valid `emphasis.inline_children` from Task 1.
- Produces: TEI `<hi>` with nested phrase content; plaintext visible text without ruby readings.

- [ ] **Step 1: Add failing TEI renderer tests**

In `../abc/test/abc/tools/parser_ir_tei_test.clj`, add:

```clojure
(deftest emphasis-inline-children-render-nested-tei-test
  (testing "emphasis inline_children render nested hi and ruby nodes"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "emphasis"
                             "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                             "style" "bold"
                             "text" "東京"
                             "inline_children" [{"type" "ruby"
                                                 "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                                 "ruby" {"base" "東京"
                                                         "reading" "とうきょう"
                                                         "scope" "explicit"
                                                         "direction" "right"}}]}
                            {"type" "emphasis"
                             "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
                             "style" "outer"
                             "text" "内"
                             "inline_children" [{"type" "emphasis"
                                                 "span" {"start" 2 "end" 4 "coordinate_system" "decoded_utf8"}
                                                 "style" "inner"
                                                 "text" "内"}]}]})
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:hi {:rend "bold"}
               [:ruby {:type "furigana" :rend "right"}
                [:rb "東京"]
                [:rt "とうきょう"]]]
              [:hi {:rend "outer"}
               [:hi {:rend "inner"} "内"]]]
             paragraph))
      (is (= {"emphasis" 3 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))
```

- [ ] **Step 2: Add failing plaintext renderer test**

In `../abc/test/abc/tools/parser_ir_plaintext_test.clj`, add:

```clojure
(deftest emphasis-inline-children-render-visible-plaintext-test
  (testing "plaintext uses inline children but omits ruby readings and metadata"
    (is (= "東京X内"
           (plaintext/render-string
            {"nodes" [{"type" "emphasis"
                       "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                       "style" "bold"
                       "text" "fallback"
                       "inline_children" [{"type" "ruby"
                                           "span" {"start" 0 "end" 2 "coordinate_system" "decoded_utf8"}
                                           "ruby" {"base" "東京"
                                                   "reading" "とうきょう"
                                                   "scope" "explicit"}}
                                          {"type" "gaiji"
                                           "span" {"start" 2 "end" 3 "coordinate_system" "decoded_utf8"}
                                           "gaiji" {"raw_marker" "※［＃x］"
                                                    "unicode" "X"
                                                    "resolved" true}}
                                          {"type" "editor-note"
                                           "span" {"start" 3 "end" 4 "coordinate_system" "decoded_utf8"}
                                           "note" {"raw" "［＃注］"
                                                   "category" "misc"}}
                                          {"type" "emphasis"
                                           "span" {"start" 4 "end" 5 "coordinate_system" "decoded_utf8"}
                                           "style" "inner"
                                           "text" "内"}]}]})))))
```

- [ ] **Step 3: Run renderer tests to verify they fail**

Run from `../abc`:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.parser-ir-tei-test/emphasis-inline-children-render-nested-tei-test --focus abc.tools.parser-ir-plaintext-test/emphasis-inline-children-render-visible-plaintext-test
```

Expected: TEI test fails because `render-emphasis-node` ignores `inline_children`; plaintext test fails because it emits legacy `text`.

- [ ] **Step 4: Implement recursive TEI inline rendering**

In `../abc/src/abc/tools/parser_ir_tei.clj`, add a helper before `render-text-node`:

```clojure
(declare render-node)

(def ^:private max-inline-depth 64)

(defn- render-inline-children [acc children depth]
  (if (>= depth max-inline-depth)
    (mark-omitted acc "emphasis-inline-depth")
    (reduce (fn [state child]
              (render-node state child (inc depth)))
            acc
            children)))
```

Change each renderer arity so existing calls still work:

```clojure
(defn- render-text-node
  ([acc node] (render-text-node acc node 0))
  ([acc node _depth]
   (append-inline acc (get node "text"))))
```

Apply the same two-arity pattern to existing node renderers. For renderers that do not recurse, ignore `_depth`.

Replace `render-emphasis-node` with:

```clojure
(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (let [children (seq (get node "inline_children"))]
     (if children
       (let [before-count (count (:current-paragraph acc))
             rendered (render-inline-children acc children depth)
             inline-fragment (subvec (vec (:current-paragraph rendered)) before-count)]
         (assoc rendered
                :current-paragraph
                (conj (subvec (vec (:current-paragraph acc)) 0 before-count)
                      (into [:hi {:rend (get node "style")}] inline-fragment))))
       (append-inline acc
                      [:hi {:rend (get node "style")}
                       (get node "text")])))))
```

Change `render-node` to accept optional depth:

```clojure
(defn- render-node
  ([acc node] (render-node acc node 0))
  ([acc node depth]
   (if-let [renderer (get node-renderers (get node "type"))]
     (renderer acc node depth)
     (mark-omitted acc (get node "type")))))
```

If any renderer calls `render-node`, pass the current depth through.

- [ ] **Step 5: Implement recursive plaintext rendering**

In `../abc/src/abc/tools/parser_ir_plaintext.clj`, add:

```clojure
(declare render-node)

(def ^:private max-inline-depth 64)

(defn- render-inline-children [acc children depth]
  (if (>= depth max-inline-depth)
    (mark-omitted acc "emphasis-inline-depth")
    (reduce (fn [state child]
              (render-node state child (inc depth)))
            acc
            children)))
```

Change existing renderers to two arities as in TEI. Replace `render-emphasis-node` with:

```clojure
(defn- render-emphasis-node
  ([acc node] (render-emphasis-node acc node 0))
  ([acc node depth]
   (if-let [children (seq (get node "inline_children"))]
     (render-inline-children acc children depth)
     (append-text acc (get node "text")))))
```

Change `render-node` to optional depth:

```clojure
(defn- render-node
  ([acc node] (render-node acc node 0))
  ([acc node depth]
   (if-let [renderer (get node-renderers (get node "type"))]
     (renderer acc node depth)
     (mark-omitted acc (get node "type")))))
```

- [ ] **Step 6: Run renderer tests to verify they pass**

Run from `../abc`:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus abc.tools.parser-ir-tei-test/emphasis-inline-children-render-nested-tei-test --focus abc.tools.parser-ir-plaintext-test/emphasis-inline-children-render-visible-plaintext-test
```

Expected: both focused tests pass.

- [ ] **Step 7: Run ABC full verification for this slice**

Run from `../abc`:

```bash
clojure -M:test:kaocha -m kaocha.runner
nix run .#validate-design-bundle
```

Expected: Clojure test suite passes; design bundle validates. When the design-bundle error names a stale parser-IR schema hash fixture, compute the new hash from `clojure -M -e '(require (quote abc.tools.schema)) (println (abc.tools.schema/schema-hash "schemas/parser-ir.schema.json"))'`, replace only the named stale hash value, and rerun both commands.

- [ ] **Step 8: Commit ABC renderer change**

```bash
cd /home/bor/Projects/abc
git add schemas/parser-ir.schema.json src/abc/tools/parser_ir_tei.clj src/abc/tools/parser_ir_plaintext.clj test/abc/tools/parser_ir_tei_test.clj test/abc/tools/parser_ir_plaintext_test.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): render emphasis inline children"
```

---

### Task 3: ab-validator Vendors Rotated ABC Schema

**Files:**
- Modify: `data/abc-schemas/schemas/parser-ir.schema.json`
- Modify: `data/aat-to-parser-ir-mapping-v1.json`
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.md`
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.summary.json`

**Interfaces:**
- Consumes: ABC schema from Tasks 1-2.
- Produces: ab-validator `SchemaSet` that validates new `inline_children`.

- [ ] **Step 1: Copy ABC schema into ab-validator**

Run from `/home/bor/Projects/ab-validator`:

```bash
cp ../abc/schemas/parser-ir.schema.json data/abc-schemas/schemas/parser-ir.schema.json
```

- [ ] **Step 2: Run existing parser-IR tests to observe hash failures**

```bash
cargo test -p ab-aat-to-parser-ir legacy_schema_hashes_match_mapping_artifact -- --nocapture
cargo test -p ab-aat-to-parser-ir mapping_preflight_accepts_checked_in_v2_artifact -- --nocapture
```

Expected: tests fail because the parser-IR schema hash changed and the checked-in mapping artifact still declares the old target hash.

- [ ] **Step 3: Regenerate mapping artifact with new parser-IR schema hash**

Run the existing five-parser mapping generator command:

```bash
repo_root=/home/bor/Projects/ab-validator
git_common_dir="$(git -C "$repo_root" rev-parse --path-format=absolute --git-common-dir)"
repo_storage_root="$(cd "$git_common_dir/.." && pwd)"
aozora_rs_dir="${AB_AOZORA_RS_AAT_DIR:-$repo_storage_root/scratch/morph-full-corpus/aats/aozora-rs-adapter}"
aozora2html_dir="${AB_AOZORA2HTML_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter}"
aozora_epub3_dir="${AB_AOZORA_EPUB3_AAT_DIR:-/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter}"
aozora2_dir="${AB_AOZORA2_AAT_DIR:-/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter}"
aozora_dir="${AB_AOZORA_AAT_DIR:-/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter}"

uv run --isolated --no-project --with 'jsonschema>=4.0' \
  "$repo_root/reports/aat-fidelity/aat_parser_ir_mapping/generate.py" \
  --aat-dir "$aozora_rs_dir" \
  --aat-dir "$aozora2html_dir" \
  --aat-dir "$aozora_epub3_dir" \
  --aat-dir "$aozora2_dir" \
  --aat-dir "$aozora_dir" \
  --abc-root "$repo_root/data/abc-schemas" \
  --mapping-version 0.2.3 \
  --out "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --summary-json "$repo_root/docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.summary.json" \
  --report-md "$repo_root/docs/superpowers/reports/2026-07-04-aat-parser-ir-mapping-generation.md"
```

Expected output: `data/aat-to-parser-ir-mapping-v1.json` keeps `"mapping_version": "0.2.3"` and `.target_parser_ir_schema_hash` matches `schema_hash(data/abc-schemas/schemas/parser-ir.schema.json)`.

- [ ] **Step 4: Update hash assertions**

In `crates/ab-aat-to-parser-ir/tests/integration.rs`, update `legacy_schema_hashes_match_mapping_artifact` expected parser-IR hash to the newly computed hash from:

```bash
cargo test -p ab-aat-to-parser-ir legacy_schema_hashes_match_mapping_artifact -- --nocapture
```

Copy the computed hash from the failure message. Do not leave temporary code in the repo.

- [ ] **Step 5: Verify schema and mapping preflight**

Run:

```bash
cargo test -p ab-aat-to-parser-ir legacy_schema_hashes_match_mapping_artifact -- --nocapture
cargo test -p ab-aat-to-parser-ir mapping_preflight_accepts_checked_in_v2_artifact -- --nocapture
```

Expected: both tests pass.

- [ ] **Step 6: Commit vendored schema/hash rotation**

```bash
git add data/abc-schemas/schemas/parser-ir.schema.json data/aat-to-parser-ir-mapping-v1.json crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "chore(parser-ir): vendor emphasis inline children schema"
```

---

### Task 4: ab-validator Emits Emphasis Inline Children

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes: vendored schema accepting `emphasis.inline_children`.
- Produces: parser-IR `emphasis` nodes with both `text` and `inline_children`.

- [ ] **Step 1: Add failing converter test for ruby inside style**

In `crates/ab-aat-to-parser-ir/tests/integration.rs`, add:

```rust
#[test]
fn preserves_ruby_inside_emphasis_inline_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "emphasis-inline-children",
        "meta": base_meta(
            "utf-8",
            "sha256:9393939393939393939393939393939393939393939393939393939393939393",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "bold",
                "content": [
                    {"kind": "text", "value": "前"},
                    {"kind": "ruby", "base": "東京", "reading": "とうきょう", "direction": "right"},
                    {"kind": "text", "value": "後"}
                ]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions::default(),
    })
    .unwrap();

    assert_eq!(output.parser_ir.pointer("/nodes/0/type"), Some(&json!("emphasis")));
    assert_eq!(output.parser_ir.pointer("/nodes/0/text"), Some(&json!("前東京後")));
    assert_eq!(output.parser_ir.pointer("/nodes/0/inline_children/0/text"), Some(&json!("前")));
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/inline_children/1/ruby/reading"),
        Some(&json!("とうきょう"))
    );
    assert_eq!(output.parser_ir.pointer("/nodes/0/inline_children/2/text"), Some(&json!("後")));
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

- [ ] **Step 2: Add failing converter test for nested containers**

In the same file, add:

```rust
#[test]
fn preserves_nested_inline_container_children() {
    let (schemas, mapping) = schemas_and_mapping();
    let aat = json!({
        "version": 1,
        "work_id": "nested-inline-container",
        "meta": base_meta(
            "utf-8",
            "sha256:9494949494949494949494949494949494949494949494949494949494949494",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "style",
                "style_type": "outer",
                "content": [{
                    "kind": "tcy",
                    "content": [
                        {"kind": "text", "value": "12"}
                    ]
                }]
            }]
        }]
    });

    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions::default(),
    })
    .unwrap();

    assert_eq!(output.parser_ir.pointer("/nodes/0/style"), Some(&json!("outer")));
    assert_eq!(output.parser_ir.pointer("/nodes/0/text"), Some(&json!("12")));
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/inline_children/0/style"),
        Some(&json!("tcy"))
    );
    assert_eq!(
        output.parser_ir.pointer("/nodes/0/inline_children/0/inline_children/0/text"),
        Some(&json!("12"))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run:

```bash
cargo test -p ab-aat-to-parser-ir inline_children -- --nocapture
```

Expected: tests fail because `inline_children` is absent.

- [ ] **Step 4: Add inline conversion helpers**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add helper functions near `map_inline_content`:

```rust
fn inline_children_nodes(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<(Vec<Value>, u64)> {
    if depth >= 64 {
        let text = visible_content_text(content, recorder, path, Some("(emphasis.text)"))?;
        let end = offset + utf8_len(&text);
        let span = json!({"start": offset, "end": end, "line": null, "column": null, "coordinate_system": "decoded_utf8"});
        return Ok((vec![json!({"type": "text", "span": span, "text": text})], end));
    }
    let mut nodes = Vec::new();
    let mut current = offset;
    for (index, child) in content.and_then(Value::as_array).into_iter().flatten().enumerate() {
        current = map_inline_to_nodes_with_depth(
            child,
            &mut nodes,
            recorder,
            current,
            &format!("{path}[{index}]"),
            depth,
        )?;
    }
    Ok((nodes, current))
}
```

Rename the existing `map_inline_to_nodes` implementation body into:

```rust
fn map_inline_to_nodes_with_depth(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<u64> {
    // existing match body
}
```

Then make the public/internal caller wrapper:

```rust
fn map_inline_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    map_inline_to_nodes_with_depth(node, nodes, recorder, offset, path, 0)
}
```

Update recursive calls inside `inline_children_nodes` to pass `depth + 1` for nested containers.

- [ ] **Step 5: Emit inline_children for style and generic containers**

In the `"style"` match arm, after computing `text`, add:

```rust
let (inline_children, child_end) = inline_children_nodes(
    node.get("content"),
    recorder,
    offset,
    &format!("{path}.content"),
    depth + 1,
)?;
let end = child_end.max(offset + utf8_len(&text));
```

Then include `"inline_children": inline_children` in the emitted emphasis JSON.

In the `"font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi"` match arm, do the same.

Keep `text` unchanged as the compatibility projection.

- [ ] **Step 6: Run focused converter tests**

Run:

```bash
cargo test -p ab-aat-to-parser-ir preserves_ruby_inside_emphasis_inline_children -- --nocapture
cargo test -p ab-aat-to-parser-ir preserves_nested_inline_container_children -- --nocapture
```

Expected: both tests pass and parser-IR schema validation succeeds.

- [ ] **Step 7: Run full crate tests**

Run:

```bash
cargo test -p ab-aat-to-parser-ir -- --nocapture
cargo fmt -p ab-aat-to-parser-ir -- --check
tests/aat-to-parser-ir-cli-smoke.sh
```

Expected: all tests and smoke pass.

- [ ] **Step 8: Commit converter change**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-ir): emit emphasis inline children"
```

---

### Task 5: Regenerate Full Audit and Compatibility Evidence

**Files:**
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md`
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json`
- Modify: `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`
- Read: `docs/superpowers/reports/2026-07-05-tei-node-coverage-emphasis-nesting.md`

**Interfaces:**
- Consumes: converter that emits inline children.
- Produces: measured evidence for ABC compatibility registry admission.

- [ ] **Step 1: Run full five-parser audit**

Run:

```bash
just aat-to-parser-ir-full-audit 24
```

Expected: `audited 89169 AAT files: 89169 succeeded, 0 failed`.

- [ ] **Step 2: Assert full audit gate**

Run:

```bash
jq -e '
  .totals.files_attempted == 89169
  and .totals.files_succeeded == 89169
  and .totals.files_failed == 0
  and (.top_errors | length) == 0
  and ([.by_corpus[].files_failed] | all(. == 0))
' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
```

Expected: `true`.

- [ ] **Step 3: Inspect schema hash in regenerated evidence**

Run:

```bash
jq -r '.mapping.target_parser_ir_schema_hash // .parser_ir_schema_hash // empty' docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json
rg -n ':parser_ir_schema_hash|mapping_version|mapping_hash' docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
```

Expected: emitted parser-IR schema hash matches the rotated ABC schema hash from Task 1. If `docs/superpowers/reports/2026-07-05-tei-node-coverage-emphasis-nesting.md` still describes flattened emphasis text as the active limitation, edit that report to state that the measurement motivated the now-implemented `inline_children` preservation path and keep the original measured counts unchanged.

- [ ] **Step 4: Run ab-validator verification**

Run:

```bash
cargo test -p ab-aat-to-parser-ir -- --nocapture
cargo fmt -p ab-aat-to-parser-ir -- --check
tests/aat-to-parser-ir-cli-smoke.sh
git diff --check
```

Expected: tests pass, formatting passes, smoke passes, and diff check is clean.

- [ ] **Step 5: Commit regenerated evidence**

```bash
git add docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn docs/superpowers/reports/2026-07-05-tei-node-coverage-emphasis-nesting.md
git commit -m "docs(parser-ir): refresh inline children conversion evidence"
```

---

### Task 6: ABC Compatibility Admission Handoff

**Files:**
- Create: `docs/handoffs/parser-ir-emphasis-inline-children.md` in ab-validator.
- Modify in ABC only after review: `../abc/data/aat-parser-ir-compatibility.edn`.

**Interfaces:**
- Consumes: regenerated compatibility candidates from Task 5.
- Produces: a concise handoff for ABC to admit the new parser-IR schema hash entries.

- [ ] **Step 1: Write handoff**

Create `docs/handoffs/parser-ir-emphasis-inline-children.md`:

```markdown
# Parser-IR Emphasis Inline Children Handoff

ab-validator now emits parser-IR `emphasis.inline_children` while retaining
legacy `emphasis.text`.

Verification:

- `cargo test -p ab-aat-to-parser-ir -- --nocapture`
- `tests/aat-to-parser-ir-cli-smoke.sh`
- `just aat-to-parser-ir-full-audit 24`
- full audit: 89,169 attempted / 89,169 succeeded / 0 failed

Compatibility candidates:

- `docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn`

ABC next action:

1. Copy the new `:conversion-audit` entries into
   `data/aat-parser-ir-compatibility.edn`.
2. Run `clojure -M:test:kaocha -m kaocha.runner`.
3. Run `nix run .#validate-design-bundle`.
4. Keep old parser-IR schema hash entries for previously generated documents.
```

- [ ] **Step 2: Commit handoff**

```bash
git add docs/handoffs/parser-ir-emphasis-inline-children.md
git commit -m "docs(parser-ir): hand off inline children compatibility"
```

- [ ] **Step 3: Optional ABC registry update**

Only after the handoff is reviewed, copy the new EDN entries into
`../abc/data/aat-parser-ir-compatibility.edn`, then run:

```bash
cd /home/bor/Projects/abc
clojure -M:test:kaocha -m kaocha.runner
nix run .#validate-design-bundle
git add data/aat-parser-ir-compatibility.edn
git commit -m "data(parser-ir): admit emphasis inline children evidence"
```

Expected: ABC tests and design bundle validation pass.

---

## Self-Review

Spec coverage:

- Schema migration: Tasks 1 and 3.
- ABC TEI/plaintext recursive rendering: Task 2.
- Recursion guard: Task 2 renderer helper and Task 4 converter overflow behavior.
- ab-validator producer behavior: Task 4.
- Full audit and compatibility evidence: Task 5.
- ABC admission boundary: Task 6.

Placeholder scan:

- The plan avoids red-flag filler terms and undefined task references.
- Schema-hash fixture refresh is tied to exact verification errors and the exact hash command in Task 2.

Type consistency:

- JSON field name is `inline_children` everywhere.
- Parser-IR node type remains `"emphasis"`.
- Legacy field remains `text`.
- Renderer helper name is `render-inline-children` in both ABC renderers.
- ab-validator helper names are `inline_children_nodes` and `map_inline_to_nodes_with_depth`.
