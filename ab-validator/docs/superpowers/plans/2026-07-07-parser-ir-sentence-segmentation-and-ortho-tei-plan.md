# Parser-IR Sentence Segmentation and Orthographic TEI Propagation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move sentence segmentation into parser-IR evidence and propagate orthographic-katakana annotations through ABC TEI `<s>` rendering.

**Architecture:** ABC first accepts the new parser-IR contract (`sentence_segmentation`, `sentences`, and `orthographic_annotations`). ab-validator then emits sentence rows with node-aligned ranges and joins orthographic annotation overlaps into sentence tags. ABC TEI rendering consumes those rows directly and does not split text during rendering.

**Tech Stack:** Rust 2024 (`ab-aat-to-parser-ir`, `ab-ortho-detect`, `ab-plaintext`, `serde_json`, `jsonschema`), Clojure (`abc.tools.parser-ir-tei`, `abc.tools.validate-design-bundle`, JSON Schema, TEI profile validation).

## Global Constraints

- Parser-IR schema version becomes `0.6.0`.
- `sentence_segmentation.coordinate_system` and every sentence span use `"decoded_utf8"`.
- ab-validator owns sentence segmentation evidence; ABC must not call `abc.text/split-japanese-sentence` for parser-IR TEI when `sentences[]` is present.
- Before emitting parser-IR sentence rows, measure Rust/Clojure splitter divergence on shared fixtures and a representative corpus sample.
- ABC owns TEI rendering and header/profile validation.
- `orthographic_annotations` remains detector provenance; `sentences[].tags` is the renderer-facing projection.
- Ruby readings are reading evidence only. Sentence splitting and tokenizer input use visible base text, not ruby readings.
- No silent sentence-boundary snapping. If a boundary falls inside an atomic node, conversion fails in sentence-enabled mode with a diagnostic.
- Sentence projection rewrites `nodes`, `paragraphs[].node_range`, and `sentences[].node_range` together; body paragraph sentence rows tile the paragraph with no gaps or overlaps.
- No placeholder implementation that treats each node as one sentence. The first Rust implementation must exercise real sentence splitting and node splitting.
- Mapping artifact changes for sentence fields must label them synthetic parser-IR additions, not probe-observed AAT divergence.
- Verification targets:
  - `cargo test -p ab-aat-to-parser-ir`
  - `nix build .#checks.x86_64-linux.ab-validator-cargo-check`
  - `nix build .#checks.x86_64-linux.ab-validator-cargo-clippy`
  - `nix build .#checks.x86_64-linux.ab-validator-cargo-fmt`
  - `nix build .#checks.x86_64-linux.abc-clj-kondo`
  - `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests`

---

### Task 0: Sentence Splitter Compatibility Audit

**Files:**
- Modify: `ab-validator/crates/ab-plaintext/src/lib.rs`
- Modify: `abc/test/abc/text_test.clj`
- Create: `ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md`

**Interfaces:**
- Produces the compatibility decision consumed by Task 2:
  - either `ab_plaintext::sentence_split` is changed to match the checked
    shared fixtures, or divergence is documented as intentional before
    parser-IR sentence emission is enabled.

- [ ] **Step 1: Add Rust splitter fixture tests**

In `ab-validator/crates/ab-plaintext/src/lib.rs`, extend
`sentence_split_tests` with the ABC edge cases:

```rust
#[test]
fn does_not_split_decimal_points() {
    let spans = sentence_split("これは3.14です。終わり。");
    assert_eq!(
        spans.iter().map(|span| span.text).collect::<Vec<_>>(),
        vec!["これは3.14です。", "終わり。"]
    );
}

#[test]
fn does_not_split_before_closing_quote_or_bracket() {
    let spans = sentence_split("彼は言った。）次。");
    assert_eq!(
        spans.iter().map(|span| span.text).collect::<Vec<_>>(),
        vec!["彼は言った。）次。"]
    );
}

#[test]
fn fixture_matrix_matches_abc_legacy_cases() {
    let cases = [
        ("吾輩は猫である。名前はまだ無い。", vec!["吾輩は猫である。", "名前はまだ無い。"]),
        ("え！？本当。", vec!["え！？", "本当。"]),
        ("これは3.14です。終わり。", vec!["これは3.14です。", "終わり。"]),
        ("彼は言った。）次。", vec!["彼は言った。）次。"]),
        ("一行目\n二行目。", vec!["一行目\n二行目。"]),
    ];

    for (input, expected) in cases {
        let actual = sentence_split(input)
            .iter()
            .map(|span| span.text)
            .collect::<Vec<_>>();
        assert_eq!(actual, expected, "input: {input}");
    }
}
```

- [ ] **Step 2: Run the Rust splitter test and observe failures**

Run:

```bash
cargo test -p ab-plaintext -- sentence_split_tests --manifest-path ab-validator/Cargo.toml
```

Expected before any splitter fix: FAIL on the closing-bracket fixture if the
current Rust splitter still splits after `。` before `）`.

- [ ] **Step 3: Keep ABC fixture coverage explicit**

In `abc/test/abc/text_test.clj`, keep the existing tests for decimals, closing
quotation, and adjacent delimiters. If they move, the exact cases from Step 1
must remain present:

```clojure
(is (= ["これは3.14です。" "終わり。"]
       (text/split-japanese-sentence "これは3.14です。終わり。")))
(is (= ["彼は言った。）次。"]
       (text/split-japanese-sentence "彼は言った。）次。")))
(is (= ["え！？" "本当。"]
       (text/split-japanese-sentence "え！？本当。")))
```

- [ ] **Step 4: Decide the Rust splitter behavior**

If Step 2 fails, update `ab_plaintext::sentence_split` so it matches the shared
fixtures before continuing. Do not paper over the divergence in parser-IR
tests. The desired behavior is:

- decimal `.` between numeric/alphanumeric characters is not a boundary;
- adjacent delimiters stay in one sentence;
- a delimiter immediately followed by a closing bracket or quote does not create
  the split shown by the current Rust implementation; it must match the checked
  fixture `彼は言った。）次。`;
- newlines inside a paragraph are not sentence boundaries.

- [ ] **Step 5: Record the compatibility report**

Create `ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md`:

```markdown
# Sentence Splitter Compatibility Report

**Date:** 2026-07-07
**Decision:** Parser-IR sentence rows use the Rust splitter behavior covered by
the shared fixture matrix below.

| Input | Expected Sentences | Result |
|---|---|---|
| `吾輩は猫である。名前はまだ無い。` | `吾輩は猫である。` / `名前はまだ無い。` | pass |
| `え！？本当。` | `え！？` / `本当。` | pass |
| `これは3.14です。終わり。` | `これは3.14です。` / `終わり。` | pass |
| `彼は言った。）次。` | `彼は言った。）次。` | pass |
| `一行目\n二行目。` | `一行目\n二行目。` | pass |

## Corpus Sample

Minimum checked sample:

- `ab-validator/tests/fixtures/aat-parser-ir/real-aozora2html-sample.aat.json`
- `ab-validator/tests/fixtures/aat-parser-ir/real-aozora-rs-sample.aat.json`

Record the command, number of body paragraphs inspected, and divergence count
here. If divergence is nonzero, either port the desired edge behavior into Rust
or list each intentional behavior change in this section.
```

- [ ] **Step 6: Run both focused checks**

Run:

```bash
cargo test -p ab-plaintext -- sentence_split_tests --manifest-path ab-validator/Cargo.toml
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add ab-validator/crates/ab-plaintext/src/lib.rs \
        abc/test/abc/text_test.clj \
        ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md
git commit -m "test(parser-ir): audit sentence splitter compatibility"
```

---

### Task 1: ABC Parser-IR Schema Contract

**Files:**
- Modify: `abc/schemas/parser-ir.schema.json`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`

**Interfaces:**
- Produces parser-IR schema fields consumed by ab-validator:
  - `sentence_segmentation: SentenceSegmentation`
  - `sentences: [SentenceRow]`
  - `orthographic_annotations: OrthographicAnnotationsBundle`
- Produces Clojure validation function:
  - `parser-ir-sentence-coherence-errors [parser-ir] -> vector<string>`

- [ ] **Step 1: Write failing schema acceptance test**

Add a test to `abc/test/abc/tools/validate_design_bundle_test.clj` near the
existing parser-IR schema tests:

```clojure
(deftest parser-ir-schema-accepts-sentences-and-orthographic-annotations-test
  (testing "parser IR carries sentence segmentation plus orthographic annotation provenance"
    (let [parser-ir {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
                     "schema_hash" current-parser-ir-schema-hash
                     "source" {"work_content_hash" (files/example-hash "11")
                               "encoding" "UTF-8"
                               "normalization" "source"}
                     "nodes" [{"type" "text"
                               "span" {"start" 0 "end" 24
                                       "coordinate_system" "decoded_utf8"}
                               "text" "吾輩ハ猫デアル。"}]
                     "paragraphs" [{"id" "p000000"
                                    "span" {"start" 0 "end" 24
                                            "coordinate_system" "decoded_utf8"}
                                    "span_source" "direct"
                                    "node_range" {"start" 0 "end" 1}
                                    "role" "body"
                                    "source_pointer" "blocks[0]"
                                    "classification" "direct"}]
                     "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                              "splitter_id" "ab-plaintext-japanese-v1"
                                              "coordinate_system" "decoded_utf8"
                                              "coverage" "body-paragraphs"}
                     "sentences" [{"id" "s000000"
                                   "paragraph_id" "p000000"
                                   "span" {"start" 0 "end" 24
                                           "coordinate_system" "decoded_utf8"}
                                   "node_range" {"start" 0 "end" 1}
                                   "tags" ["orthographic-katakana"]
                                   "orthographic_annotation_indices" [0]}]
                     "orthographic_annotations" {"work_id" "000000"
                                                 "work_content_hash" (files/example-hash "11")
                                                 "coordinate_system" "decoded_utf8"
                                                 "detector_id" "HeuristicV1"
                                                 "annotations" [{"source_byte_range" {"start" 0 "end" 24}
                                                                 "normalized_text" "吾輩は猫である。"
                                                                 "kind" "ScriptKatakanaToHiragana"
                                                                 "confidence" nil}]}
                     "warnings" []
                     "errors" []}]
      (is (nil? (schema/validation-errors
                 (files/read-json "schemas/parser-ir.schema.json")
                 parser-ir))))))
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: FAIL because `sentence_segmentation`, `sentences`, and
`orthographic_annotations` are rejected by `additionalProperties: false`.

- [ ] **Step 3: Add schema definitions**

In `abc/schemas/parser-ir.schema.json`:

- Set top-level `"version": "0.6.0"`.
- Add optional top-level properties:

```json
"sentence_segmentation": { "$ref": "#/$defs/sentenceSegmentation" },
"sentences": {
  "type": "array",
  "items": { "$ref": "#/$defs/sentence" },
  "default": []
},
"orthographic_annotations": { "$ref": "#/$defs/orthographicAnnotations" }
```

Add `$defs` entries:

```json
"sentenceSegmentation": {
  "type": "object",
  "required": ["schema_version", "splitter_id", "coordinate_system", "coverage"],
  "additionalProperties": false,
  "properties": {
    "schema_version": { "const": "sentence-segmentation-v1" },
    "splitter_id": { "const": "ab-plaintext-japanese-v1" },
    "coordinate_system": { "const": "decoded_utf8" },
    "coverage": { "const": "body-paragraphs" }
  }
},
"sentence": {
  "type": "object",
  "required": [
    "id",
    "paragraph_id",
    "span",
    "node_range",
    "tags",
    "orthographic_annotation_indices"
  ],
  "additionalProperties": false,
  "properties": {
    "id": { "type": "string", "pattern": "^s[0-9]{6}$" },
    "paragraph_id": { "type": "string", "pattern": "^p[0-9]{6}$" },
    "span": { "$ref": "#/$defs/span" },
    "node_range": { "$ref": "#/$defs/nodeRange" },
    "tags": {
      "type": "array",
      "items": { "enum": ["orthographic-katakana"] },
      "uniqueItems": true
    },
    "orthographic_annotation_indices": {
      "type": "array",
      "items": { "type": "integer", "minimum": 0 },
      "uniqueItems": true
    }
  }
},
"orthographicAnnotations": {
  "type": "object",
  "required": [
    "work_id",
    "work_content_hash",
    "coordinate_system",
    "detector_id",
    "annotations"
  ],
  "additionalProperties": false,
  "properties": {
    "work_id": { "type": "string" },
    "work_content_hash": { "$ref": "#/$defs/hash" },
    "coordinate_system": { "const": "decoded_utf8" },
    "detector_id": { "$ref": "#/$defs/orthoDetectorId" },
    "annotations": {
      "type": "array",
      "items": { "$ref": "#/$defs/orthographicAnnotation" }
    }
  }
},
"orthographicAnnotation": {
  "type": "object",
  "required": ["source_byte_range", "normalized_text", "kind", "confidence"],
  "additionalProperties": false,
  "properties": {
    "source_byte_range": { "$ref": "#/$defs/byteRange" },
    "normalized_text": { "type": "string" },
    "kind": { "enum": ["ScriptKatakanaToHiragana", "HistoricalToModern"] },
    "confidence": { "type": ["integer", "null"], "minimum": 0, "maximum": 100 }
  }
},
"orthoDetectorId": {
  "oneOf": [
    { "const": "HeuristicV1" },
    {
      "type": "object",
      "required": ["MlLogisticRegression"],
      "additionalProperties": false,
      "properties": {
        "MlLogisticRegression": {
          "type": "object",
          "required": ["model_hash"],
          "additionalProperties": false,
          "properties": {
            "model_hash": { "$ref": "#/$defs/hash" }
          }
        }
      }
    }
  ]
},
"byteRange": {
  "type": "object",
  "required": ["start", "end"],
  "additionalProperties": false,
  "properties": {
    "start": { "type": "integer", "minimum": 0 },
    "end": { "type": "integer", "minimum": 0 }
  }
}
```

Reuse the existing `nodeRange` definition if present; otherwise add it with
`start`/`end` non-negative integer fields.

- [ ] **Step 4: Add sentence coherence validation**

In `abc/src/abc/tools/validate_design_bundle.clj`, add:

```clojure
(defn- sentence-row-error-prefix [sentence]
  (str "parser IR sentence " (get sentence "id")))

(defn- sentence-tiling-errors [paragraph sentences]
  (let [pid (get paragraph "id")
        paragraph-node-range (get paragraph "node_range")
        paragraph-span (get paragraph "span")
        paragraph-empty? (= (get paragraph-node-range "start")
                            (get paragraph-node-range "end"))
        ordered (sort-by (juxt #(get-in % ["node_range" "start"])
                               #(get-in % ["span" "start"]))
                         sentences)]
    (cond
      (and paragraph-empty? (empty? ordered))
      []

      (empty? ordered)
      [(str "parser IR body paragraph " pid " has no sentence rows")]

      :else
      (let [node-errors
            (loop [remaining ordered
                   expected-start (get paragraph-node-range "start")
                   errors []]
              (if-let [sentence (first remaining)]
                (let [node-range (get sentence "node_range")
                      start (get node-range "start")
                      end (get node-range "end")
                      errors (cond-> errors
                               (not= start expected-start)
                               (conj (str (sentence-row-error-prefix sentence)
                                          " node_range starts at " start
                                          " but expected " expected-start)))]
                  (recur (rest remaining) end errors))
                (cond-> errors
                  (not= expected-start (get paragraph-node-range "end"))
                  (conj (str "parser IR body paragraph " pid
                             " sentence node_ranges end at " expected-start
                             " but paragraph node_range ends at "
                             (get paragraph-node-range "end"))))))
            span-errors
            (loop [remaining ordered
                   expected-start (get paragraph-span "start")
                   errors []]
              (if-let [sentence (first remaining)]
                (let [span (get sentence "span")
                      start (get span "start")
                      end (get span "end")
                      errors (cond-> errors
                               (not= start expected-start)
                               (conj (str (sentence-row-error-prefix sentence)
                                          " span starts at " start
                                          " but expected " expected-start)))]
                  (recur (rest remaining) end errors))
                (cond-> errors
                  (not= expected-start (get paragraph-span "end"))
                  (conj (str "parser IR body paragraph " pid
                             " sentence spans end at " expected-start
                             " but paragraph span ends at "
                             (get paragraph-span "end"))))))]
        (vec (concat node-errors span-errors))))))

(defn parser-ir-sentence-coherence-errors [parser-ir]
  (let [nodes (vec (get parser-ir "nodes" []))
        paragraphs (vec (get parser-ir "paragraphs" []))
        body-paragraphs (into {}
                              (keep (fn [p]
                                      (when (= "body" (get p "role"))
                                        [(get p "id") p])))
                              paragraphs)
        annotations (get-in parser-ir ["orthographic_annotations" "annotations"] [])
        sentences (vec (get parser-ir "sentences" []))
        sentences-by-pid (group-by #(get % "paragraph_id") sentences)]
    (vec
     (concat
      (mapcat
       (fn [[_idx sentence]]
         (let [sid (get sentence "id")
               pid (get sentence "paragraph_id")
               paragraph (get body-paragraphs pid)
               nr (get sentence "node_range")
               pr (get paragraph "node_range")
               span (get sentence "span")
               ps (get paragraph "span")
               tags (set (get sentence "tags" []))
               annotation-indices (get sentence "orthographic_annotation_indices" [])]
           (cond-> []
             (nil? paragraph)
             (conj (str "parser IR sentence " sid " references non-body paragraph " pid))

             (and paragraph
                  (not (<= (get pr "start") (get nr "start") (get nr "end") (get pr "end"))))
             (conj (str "parser IR sentence " sid " node_range "
                        (get nr "start") ".." (get nr "end")
                        " is outside paragraph " pid " node_range "
                        (get pr "start") ".." (get pr "end")))

             (and paragraph
                  (not (<= (get ps "start") (get span "start") (get span "end") (get ps "end"))))
             (conj (str "parser IR sentence " sid " span "
                        (get span "start") ".." (get span "end")
                        " is outside paragraph " pid " span "
                        (get ps "start") ".." (get ps "end")))

             (not (<= 0 (get nr "start" -1) (get nr "end" -1) (count nodes)))
             (conj (str "parser IR sentence " sid " node_range is outside nodes[] length "
                        (count nodes)))

             (and (contains? tags "orthographic-katakana")
                  (empty? annotation-indices))
             (conj (str "parser IR sentence " sid
                        " has orthographic-katakana tag without annotation indices"))

             (some #(or (not (integer? %)) (neg? %) (>= % (count annotations)))
                   annotation-indices)
             (conj (str "parser IR sentence " sid
                        " has orthographic annotation index outside annotations[]")))))
       (map-indexed vector sentences))
      (mapcat
       (fn [paragraph]
         (sentence-tiling-errors paragraph
                                 (get sentences-by-pid (get paragraph "id") [])))
       (vals body-paragraphs))))))
```

Call this from `compatibility-errors` alongside
`parser-ir-paragraph-coherence-errors`.

- [ ] **Step 5: Add failing and passing coherence tests**

Add tests covering:

- coherent sentence row passes;
- sentence referencing a missing paragraph fails;
- sentence node range outside paragraph fails;
- sentence byte span outside paragraph span fails;
- `orthographic-katakana` with no annotation index fails;
- annotation index outside the annotation array fails.
- non-empty body paragraph with no sentence rows fails;
- two sentence rows with a byte-span gap fail;
- two sentence rows with a node-range gap fail;
- overlapping sentence rows fail because the later row starts before the
  expected next start.

Use exact expected error strings from Step 4.

- [ ] **Step 6: Run ABC focused checks**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-kondo --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add abc/schemas/parser-ir.schema.json \
        abc/src/abc/tools/validate_design_bundle.clj \
        abc/test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(parser-ir): accept sentence segmentation contract"
```

---

### Task 2: ab-validator Sentence Projection and Parser-IR Emission

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/sentence-segmentation-input.aat.json`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/sentence-segmentation-expected.json`

**Interfaces:**
- Consumes ABC schema from Task 1.
- Consumes splitter compatibility decision from Task 0.
- Produces Rust types and conversion behavior:
  - `SentenceSegmentation`
  - `ParserIrSentence`
  - `SentenceProjection`
  - `project_sentences(nodes: Vec<Value>, paragraphs: Vec<Value>, ortho: Option<&OrthoAnnotationsBundle>) -> Result<SentenceProjection>`
- Produces parser-IR output with:
  - `sentence_segmentation`
  - `sentences`
  - `orthographic_annotations` when supplied

- [ ] **Step 1: Add dependency**

In `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`, add:

```toml
ab-plaintext = { workspace = true }
```

- [ ] **Step 2: Create fixtures**

Create `tests/fixtures/sentence-segmentation-input.aat.json`. The single text
node intentionally contains two sentences so the fixture requires a real node
split:

```json
{
  "version": 1,
  "work_id": "000000",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "吾輩ハ猫デアル。名前はまだ無い。" }
      ]
    },
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "後続段落。" }
      ]
    }
  ]
}
```

Create `tests/fixtures/sentence-segmentation-expected.json`:

```json
{
  "sentence_segmentation": {
    "schema_version": "sentence-segmentation-v1",
    "splitter_id": "ab-plaintext-japanese-v1",
    "coordinate_system": "decoded_utf8",
    "coverage": "body-paragraphs"
  },
  "sentences": [
    {
      "id": "s000000",
      "paragraph_id": "p000000",
      "span": { "start": 0, "end": 24, "coordinate_system": "decoded_utf8" },
      "node_range": { "start": 0, "end": 1 },
      "tags": ["orthographic-katakana"],
      "orthographic_annotation_indices": [0]
    },
    {
      "id": "s000001",
      "paragraph_id": "p000000",
      "span": { "start": 24, "end": 48, "coordinate_system": "decoded_utf8" },
      "node_range": { "start": 1, "end": 2 },
      "tags": [],
      "orthographic_annotation_indices": []
    },
    {
      "id": "s000002",
      "paragraph_id": "p000001",
      "span": { "start": 48, "end": 63, "coordinate_system": "decoded_utf8" },
      "node_range": { "start": 2, "end": 3 },
      "tags": [],
      "orthographic_annotation_indices": []
    }
  ]
}
```

- [ ] **Step 3: Write failing sentence projection tests**

Create `src/sentences.rs` with only tests first:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn ortho_bundle() -> crate::ortho_annotations::OrthoAnnotationsBundle {
        serde_json::from_value(json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [{
                "source_byte_range": { "start": 0, "end": 24 },
                "normalized_text": "吾輩は猫である。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            }]
        }))
        .unwrap()
    }

    fn span(start: usize, end: usize) -> serde_json::Value {
        json!({"start": start, "end": end, "coordinate_system": "decoded_utf8"})
    }

    #[test]
    fn splits_single_text_node_into_sentence_nodes_and_rows() {
        let nodes = vec![json!({
            "type":"text",
            "span": span(0, 48),
            "text":"吾輩ハ猫デアル。名前はまだ無い。"
        })];
        let paragraphs = vec![json!({
            "id":"p000000",
            "span": span(0, 48),
            "span_source":"direct",
            "node_range":{"start":0,"end":1},
            "role":"body",
            "source_pointer":"blocks[0]",
            "classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["text"], "吾輩ハ猫デアル。");
        assert_eq!(projection.nodes[0]["span"], span(0, 24));
        assert_eq!(projection.nodes[1]["text"], "名前はまだ無い。");
        assert_eq!(projection.nodes[1]["span"], span(24, 48));
        assert_eq!(projection.paragraphs[0]["node_range"], json!({"start":0,"end":2}));
        assert_eq!(projection.sentences.len(), 2);
        assert_eq!(projection.sentences[0].node_range, json!({"start":0,"end":1}));
        assert_eq!(projection.sentences[1].node_range, json!({"start":1,"end":2}));
    }

    #[test]
    fn rewrites_later_paragraph_ranges_after_node_split() {
        let nodes = vec![
            json!({"type":"text","span":span(0,48),"text":"吾輩ハ猫デアル。名前はまだ無い。"}),
            json!({"type":"text","span":span(48,63),"text":"後続段落。"})
        ];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
            json!({"id":"p000001","span":span(48,63),"span_source":"direct","node_range":{"start":1,"end":2},"role":"body","source_pointer":"blocks[1]","classification":"direct"})
        ];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 3);
        assert_eq!(projection.paragraphs[0]["node_range"], json!({"start":0,"end":2}));
        assert_eq!(projection.paragraphs[1]["node_range"], json!({"start":2,"end":3}));
        assert_eq!(projection.sentences[2].paragraph_id, "p000001");
        assert_eq!(projection.sentences[2].node_range, json!({"start":2,"end":3}));
    }

    #[test]
    fn partial_ortho_overlap_tags_every_overlapping_sentence() {
        let nodes = vec![json!({
            "type":"text",
            "span": span(0, 48),
            "text":"吾輩ハ猫デアル。名前はまだ無い。"
        })];
        let paragraphs = vec![json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"})];
        let mut bundle = ortho_bundle();
        bundle.annotations[0].source_byte_range = 20..30;

        let projection = project_sentences(nodes, paragraphs, Some(&bundle)).unwrap();

        assert_eq!(projection.sentences[0].tags, vec!["orthographic-katakana"]);
        assert_eq!(projection.sentences[0].orthographic_annotation_indices, vec![0]);
        assert_eq!(projection.sentences[1].tags, vec!["orthographic-katakana"]);
        assert_eq!(projection.sentences[1].orthographic_annotation_indices, vec![0]);
    }

    #[test]
    fn uses_ruby_base_for_spans_and_splits_following_text() {
        let nodes = vec![
            json!({"type":"ruby","span":span(0,6),"ruby":{"base":"名前","reading":"めいしょう","scope":"explicit"}}),
            json!({"type":"text","span":span(6,39),"text":"はまだ無い。ここは次。"})
        ];
        let paragraphs = vec![json!({"id":"p000000","span":span(0,39),"span_source":"direct","node_range":{"start":0,"end":2},"role":"body","source_pointer":"blocks[0]","classification":"direct"})];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 3);
        assert_eq!(projection.sentences.len(), 2);
        assert_eq!(projection.sentences[0].span, span(0, 24));
        assert_eq!(projection.sentences[0].node_range, json!({"start":0,"end":2}));
        assert_eq!(projection.sentences[1].span, span(24, 39));
        assert_eq!(projection.sentences[1].node_range, json!({"start":2,"end":3}));
    }

    #[test]
    fn splits_layout_span_without_inline_children() {
        let nodes = vec![json!({
            "type":"layout-span",
            "span":span(0,12),
            "text":"甲。乙。",
            "layout":{"kind":"jitai"}
        })];
        let paragraphs = vec![json!({"id":"p000000","span":span(0,12),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"})];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["type"], "layout-span");
        assert_eq!(projection.nodes[0]["text"], "甲。");
        assert_eq!(projection.nodes[1]["text"], "乙。");
    }

    #[test]
    fn rejects_boundary_inside_atomic_inline_children_node() {
        let nodes = vec![json!({
            "type":"emphasis",
            "span":span(0,12),
            "text":"甲。乙。",
            "style":"bold",
            "inline_children":[{"type":"text","span":span(0,12),"text":"甲。乙。"}]
        })];
        let paragraphs = vec![json!({"id":"p000000","span":span(0,12),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"})];

        let error = project_sentences(nodes, paragraphs, None).unwrap_err().to_string();
        assert!(error.contains("sentence boundary falls inside atomic node emphasis at byte 6"));
    }

    #[test]
    fn rejects_boundary_inside_atomic_ruby_node() {
        let nodes = vec![json!({
            "type":"ruby",
            "span":span(0,48),
            "ruby":{"base":"吾輩ハ猫デアル。名前ハマダ無イ。","reading":"わがはいはねこであるなまえはまだない","scope":"explicit"}
        })];
        let paragraphs = vec![json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"})];

        let error = project_sentences(nodes, paragraphs, None).unwrap_err().to_string();
        assert!(error.contains("sentence boundary falls inside atomic node ruby at byte 24"));
    }
}
```

Add `pub mod sentences;` to `src/lib.rs`.

- [ ] **Step 4: Run failing tests**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir -- sentences
```

Expected: FAIL because `project_sentences` and the sentence projection types
are undefined.

- [ ] **Step 5: Implement sentence types and projection**

Implement in `src/sentences.rs`:

```rust
use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SentenceSegmentation {
    pub schema_version: String,
    pub splitter_id: String,
    pub coordinate_system: String,
    pub coverage: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParserIrSentence {
    pub id: String,
    pub paragraph_id: String,
    pub span: Value,
    pub node_range: Value,
    pub tags: Vec<String>,
    pub orthographic_annotation_indices: Vec<usize>,
}

pub struct SentenceProjection {
    pub nodes: Vec<Value>,
    pub paragraphs: Vec<Value>,
    pub segmentation: SentenceSegmentation,
    pub sentences: Vec<ParserIrSentence>,
}

pub fn segmentation_meta() -> SentenceSegmentation {
    SentenceSegmentation {
        schema_version: "sentence-segmentation-v1".to_owned(),
        splitter_id: "ab-plaintext-japanese-v1".to_owned(),
        coordinate_system: "decoded_utf8".to_owned(),
        coverage: "body-paragraphs".to_owned(),
    }
}

pub fn project_sentences(
    nodes: Vec<Value>,
    paragraphs: Vec<Value>,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<SentenceProjection> {
    project_paragraphs(nodes, paragraphs, ortho)
}

fn span_start(node: &Value) -> Result<usize> {
    node.pointer("/span/start")
        .and_then(Value::as_u64)
        .map(|v| v as usize)
        .ok_or_else(|| anyhow::anyhow!("node missing span.start"))
}

fn span_end(node: &Value) -> Result<usize> {
    node.pointer("/span/end")
        .and_then(Value::as_u64)
        .map(|v| v as usize)
        .ok_or_else(|| anyhow::anyhow!("node missing span.end"))
}

fn overlapping_ortho_indices(
    start: usize,
    end: usize,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Vec<usize> {
    ortho
        .into_iter()
        .flat_map(|bundle| bundle.annotations.iter().enumerate())
        .filter_map(|(idx, ann)| {
            (ann.source_byte_range.start < end && start < ann.source_byte_range.end).then_some(idx)
        })
        .collect()
}
```

Implement
`project_paragraphs(nodes: Vec<Value>, paragraphs: Vec<Value>, ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>) -> Result<SentenceProjection>`
with this exact algorithm:

1. Iterate paragraph rows in existing `node_range` order.
2. Copy non-body paragraphs unchanged into a new node vector and rewrite their
   paragraph `node_range` to the copied indices.
3. For each body paragraph, build visible chunks from its nodes:
   `text`/`quote`/`emphasis`/`layout-span` use their `text`; `ruby` uses
   `ruby.base`.
4. Run `ab_plaintext::sentence_split` over the paragraph visible text.
5. Convert sentence-local byte offsets to absolute decoded UTF-8 offsets by
   adding the paragraph span start.
6. For every node, split only `text`, `quote`, `emphasis`, and `layout-span`
   without `inline_children` when an interior sentence boundary falls inside
   it. Preserve all node metadata and update only `text` and `span` on each
   segment.
7. If an interior boundary falls inside `ruby`, `gaiji`, `editor-note`,
   `source-note`, `line-break`, `page-break`, `image`, `caption`, or any node
   with `inline_children`, return:
   `sentence boundary falls inside atomic node <type> at byte <offset>`.
8. Emit sentence rows by collecting the rewritten contiguous node range covered
   by each sentence span.
9. Rewrite each paragraph `node_range` to the rewritten start/end indices.
10. Assert before returning that every non-empty body paragraph's sentence
    spans and node ranges tile the paragraph.

- [ ] **Step 6: Wire conversion and write failing integration test**

Add `pub mod sentences;` to `src/lib.rs`.

In `convert.rs`, after nodes/paragraphs/warnings are built and before the JSON
object is finalized:

```rust
let sentence_projection = crate::sentences::project_sentences(
    nodes,
    paragraphs,
    options.orthographic_annotations.as_ref(),
)?;
let nodes = sentence_projection.nodes;
let paragraphs = sentence_projection.paragraphs;
let sentence_segmentation = sentence_projection.segmentation;
let sentences = sentence_projection.sentences;
```

Insert into `parser_ir`:

```rust
"sentence_segmentation": sentence_segmentation,
"sentences": sentences,
```

Add to `tests/integration.rs`:

```rust
#[test]
fn parser_ir_emits_split_sentence_rows_and_ortho_tags() {
    let (schemas, mapping) = schemas_and_mapping_accepting_sentences_and_orthographic_annotations();
    let bundle = ortho_fixture_bundle();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..ConversionOptions::default()
        },
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.pointer("/sentence_segmentation/splitter_id"),
        Some(&json!("ab-plaintext-japanese-v1"))
    );
    assert_eq!(output.parser_ir.pointer("/paragraphs/0/node_range"), Some(&json!({"start":0,"end":2})));
    assert_eq!(output.parser_ir.pointer("/paragraphs/1/node_range"), Some(&json!({"start":2,"end":3})));
    assert_eq!(output.parser_ir.pointer("/sentences/0/span"), Some(&json!({"start":0,"end":24,"coordinate_system":"decoded_utf8"})));
    assert_eq!(output.parser_ir.pointer("/sentences/1/span"), Some(&json!({"start":24,"end":48,"coordinate_system":"decoded_utf8"})));
    assert_eq!(output.parser_ir.pointer("/sentences/2/span"), Some(&json!({"start":48,"end":63,"coordinate_system":"decoded_utf8"})));
    assert_eq!(output.parser_ir.pointer("/sentences/0/tags/0"), Some(&json!("orthographic-katakana")));
    assert_eq!(output.parser_ir.pointer("/sentences/0/orthographic_annotation_indices/0"), Some(&json!(0)));
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

Add this fixture helper if the file does not already have an equivalent:

```rust
fn include_fixture_json(name: &str) -> serde_json::Value {
    let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(name);
    let bytes = std::fs::read(&path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {err}", path.display()));
    serde_json::from_slice(&bytes)
        .unwrap_or_else(|err| panic!("failed to parse fixture {}: {err}", path.display()))
}
```

- [ ] **Step 7: Run focused tests**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir -- sentences
cargo test -p ab-aat-to-parser-ir -- parser_ir_emits_split_sentence_rows_and_ortho_tags
```

Expected: PASS.

- [ ] **Step 8: Run full crate tests**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir
```

Expected: PASS.

- [ ] **Step 9: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml \
        ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/sentence-segmentation-input.aat.json \
        ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/sentence-segmentation-expected.json
git commit -m "feat(parser-ir): emit node-aligned sentence rows"
```

---

### Task 3: ABC TEI Renderer Consumes Sentence Rows

**Files:**
- Modify: `abc/src/abc/tools/parser_ir_tei.clj`
- Modify: `abc/test/abc/tools/parser_ir_tei_test.clj`

**Interfaces:**
- Consumes parser-IR `sentences[]` from Tasks 1-2.
- Produces TEI body paragraphs containing `<s>` wrappers.

- [ ] **Step 1: Write failing TEI renderer test**

Add to `abc/test/abc/tools/parser_ir_tei_test.clj`:

```clojure
(deftest sentence-rows-render-as-tei-s-test
  (testing "parser-IR sentence rows drive TEI s wrappers and orthographic type"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "text"
                             "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                             "text" "吾輩ハ猫デアル。"}
                            {"type" "ruby"
                             "span" {"start" 24 "end" 30 "coordinate_system" "decoded_utf8"}
                             "ruby" {"base" "名前"
                                     "reading" "なまえ"
                                     "scope" "explicit"}}
                            {"type" "text"
                             "span" {"start" 30 "end" 45 "coordinate_system" "decoded_utf8"}
                             "text" "はまだ無い。"}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 45 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 3}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"
                                  "layout" {"kind" "jisage"
                                            "indent" 2}}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v1"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 1}
                                 "tags" ["orthographic-katakana"]
                                 "orthographic_annotation_indices" [0]}
                                {"id" "s000001"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 24 "end" 45 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 1 "end" 3}
                                 "tags" []
                                 "orthographic_annotation_indices" []}]} )
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p {:rend "jisage indent(2)"
                  :abc/layout-kind "jisage"
                  :abc/layout-params "indent=2"}
              [:s {:type "orthographic-katakana"} "吾輩ハ猫デアル。"]
              [:s
               [:ruby {:type "furigana"}
                [:rb "名前"]
                [:rt "なまえ"]]
               "はまだ無い。"]]
             paragraph))
      (is (= {"text" 2 "ruby" 1} (:node_counts result)))
      (is (empty? (:omitted result))))))
```

- [ ] **Step 2: Run failing ABC test**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: FAIL because renderer ignores `sentences[]`.

- [ ] **Step 3: Implement sentence rendering helpers**

In `parser_ir_tei.clj`, add helpers:

```clojure
(defn- sentence-attrs [sentence]
  (when (some #{"orthographic-katakana"} (get sentence "tags" []))
    {:type "orthographic-katakana"}))

(defn- sentence-node [sentence fragment]
  (into (cond-> [:s]
          (sentence-attrs sentence)
          (conj (sentence-attrs sentence)))
        fragment))

(defn- render-sentence-row [nodes acc sentence]
  (let [{start "start" end "end"} (get sentence "node_range")
        before-count (count (:current-paragraph acc))
        rendered (render-node-seq acc (subvec nodes start end))
        rendered-paragraph (vec (:current-paragraph rendered))
        prefix (subvec rendered-paragraph 0 before-count)
        fragment (subvec rendered-paragraph before-count)]
    (assoc rendered
           :current-paragraph
           (conj prefix (sentence-node sentence fragment)))))
```

Add a sentence-aware paragraph path:

```clojure
(defn- sentences-by-paragraph [sentences]
  (group-by #(get % "paragraph_id") sentences))

(defn- render-paragraph-row-with-sentences [nodes sentences-by-pid acc paragraph]
  (let [paragraph-sentences (get sentences-by-pid (get paragraph "id"))]
    (if (seq paragraph-sentences)
      (-> (assoc acc :current-paragraph-attrs (paragraph-attrs paragraph))
          ((fn [state]
             (reduce (partial render-sentence-row nodes)
                     state
                     paragraph-sentences)))
          flush-paragraph)
      (render-paragraph-row nodes acc paragraph))))
```

Modify `render-with-paragraphs` to accept optional `sentences` and use this path
when present. Do not reset `:node_counts`, `:omitted`, `:char_declarations`,
`:front-notes`, or `:back-notes` while wrapping sentence fragments.

- [ ] **Step 4: Run focused ABC tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/parser_ir_tei.clj \
        abc/test/abc/tools/parser_ir_tei_test.clj
git commit -m "feat(tei): render parser-ir sentence rows"
```

---

### Task 4: TEI Header/Profile Declaration

**Files:**
- Modify: `abc/src/abc/tools/tei_header.clj` or the local header builder file used by `materialize_publication.clj`
- Modify: `abc/schemas/tei-profile.odd`
- Modify: `abc/schemas/tei-profile.sch`
- Modify TEI fixtures/tests that assert profile validation.

**Interfaces:**
- Consumes sentence rendering from Task 3.
- Produces valid TEI with `<normalization method="markup">` and allowed `<s type="orthographic-katakana">`.

- [ ] **Step 1: Write failing materialization test**

In `abc/test/abc/tools/materialize_publication_test.clj`, add an assertion to a
fixture that includes sentence tags:

```clojure
(is (string/includes? tei-text "<normalization method=\"markup\""))
(is (string/includes? tei-text "orthographic-katakana"))
```

Use a dedicated fixture parser-IR with `sentences[]` rather than changing the
global example until Task 5 updates schema/mapping examples.

- [ ] **Step 2: Run failing ABC checks**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: FAIL because header/profile does not declare the new sentence
normalization policy.

- [ ] **Step 3: Add header declaration**

Extend the header builder to include:

```xml
<normalization method="markup">
  <p>Sentences annotated with <gi>s</gi>
  <att>type</att>="orthographic-katakana" indicate text where the
  ab-validator ortho-detect layer identified katakana-dominant prose for
  tokenizer-facing katakana-to-hiragana normalization. The original text is not
  replaced.</p>
</normalization>
```

Only include this declaration when `parser-ir` contains either
`sentence_segmentation` or an `orthographic-katakana` sentence tag.

- [ ] **Step 4: Update TEI profile**

Update ODD/Schematron so:

- `<s>` is allowed in body paragraphs.
- `s@type="orthographic-katakana"` is accepted.
- Existing ruby completeness rules still apply inside `<s>`.

- [ ] **Step 5: Run XML/profile checks**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS with generated TEI validation status `passed`.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/tei_header.clj \
        abc/schemas/tei-profile.odd \
        abc/schemas/tei-profile.sch \
        abc/test/abc/tools/materialize_publication_test.clj
git commit -m "feat(tei): declare orthographic sentence markup"
```

---

### Task 5: Schema Mirror, Mapping Hash, and End-to-End ab-validator Conversion

**Files:**
- Modify: `ab-validator/data/abc-schemas/schemas/parser-ir.schema.json`
- Modify: `ab-validator/data/aat-to-parser-ir-mapping-v1.json`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Create: `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md`

**Interfaces:**
- Consumes ABC schema from Task 1.
- Produces checked-in ab-validator mapping artifact targeting parser-IR schema
  `0.6.0`.
- Produces a separate report for synthetic parser-IR evidence that is not
  probe-observed AAT divergence.

- [ ] **Step 1: Mirror ABC schema**

Copy the updated `abc/schemas/parser-ir.schema.json` to:

```text
ab-validator/data/abc-schemas/schemas/parser-ir.schema.json
```

- [ ] **Step 2: Update mapping artifact hash only**

Regenerate or patch `ab-validator/data/aat-to-parser-ir-mapping-v1.json`:

- bump `mapping_version` from `0.2.4` to `0.2.5`;
- set `target_parser_ir_schema_hash` to `schema_hash(&parser_ir_schema)`;
- do not add `sentence_segmentation`, `sentences[]`, or
  `orthographic_annotations` to `transform_rule_descriptions`, because the
  current mapping artifact is a generated-probe divergence document.

Create `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md`:

```markdown
# Parser-IR Synthetic Evidence Additions

**Date:** 2026-07-07
**Parser-IR schema:** `0.6.0`

The following fields are produced by conversion policy, not by AAT source
fields and not by the generated-probe divergence taxonomy:

| Parser-IR Field | Source | Reason |
|---|---|---|
| `sentence_segmentation` | converter policy | declares splitter identity and coordinate system |
| `sentences[]` | visible body text + splitter | sentence evidence for TEI rendering |
| `sentences[].tags` | overlap with `orthographic_annotations.annotations` | renderer-facing orthographic sentence classification |
| `orthographic_annotations` | optional sidecar | detector provenance, not AAT markup |

These fields are intentionally absent from
`transform_rule_descriptions` until the mapping schema has a dedicated
synthetic-evidence section.
```

- [ ] **Step 3: Update hash tests**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, update:

```rust
assert_eq!(
    mapping.target_parser_ir_schema_hash,
    schema_hash(&schemas.parser_ir_schema).unwrap()
);
assert_eq!(mapping.mapping_version, "0.2.5");
assert!(mapping.transform_rule_descriptions.iter().all(|rule| {
    !matches!(
        rule.parser_ir_pointer.as_deref(),
        Some("sentence_segmentation" | "sentences" | "orthographic_annotations")
    )
}));
```

Keep the transform rule count unchanged unless the mapping is regenerated from
the corpus probe for unrelated observed divergence changes.

- [ ] **Step 4: Add end-to-end parser-IR validation test**

Add a test that uses real `schemas_and_mapping()` rather than augmented
in-memory schema:

```rust
#[test]
fn checked_in_schema_accepts_sentence_segmentation_and_ortho_annotations() {
    let (schemas, mapping) = schemas_and_mapping();
    let bundle = ortho_fixture_bundle();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas: schemas.clone(),
        options: ConversionOptions {
            orthographic_annotations: Some(bundle),
            ..ConversionOptions::default()
        },
    })
    .unwrap();

    assert!(output.parser_ir.get("sentence_segmentation").is_some());
    assert!(output.parser_ir.get("sentences").is_some());
    assert!(output.parser_ir.get("orthographic_annotations").is_some());
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

- [ ] **Step 5: Run ab-validator tests**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir
```

Expected: PASS.

- [ ] **Step 6: Run focused Nix checks**

Run from repo root:

```bash
nix build .#checks.x86_64-linux.ab-validator-cargo-check --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt --print-build-logs
```

Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add ab-validator/data/abc-schemas/schemas/parser-ir.schema.json \
        ab-validator/data/aat-to-parser-ir-mapping-v1.json \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md
git commit -m "feat(parser-ir): target sentence segmentation schema"
```

---

### Task 6: Stable Ortho Annotation Sidecar CLI Contract

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/README.md`

**Interfaces:**
- Consumes `OrthoAnnotationsBundle` from commit `0fd0aa6` and sentence rows
  from Task 2.
- Produces a tested CLI contract: publication jobs may pass a sidecar with
  `--ortho-annotations`, and the resulting parser-IR carries both provenance
  and sentence tags.

- [ ] **Step 1: Write failing CLI integration test**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, add:

```rust
#[test]
fn cli_convert_with_ortho_annotations_emits_sentence_tags() {
    let repo = repo_root();
    let abc = abc_root(&repo);
    let temp = tempfile::tempdir().unwrap();
    let aat = temp.path().join("input.aat.json");
    let ortho = temp.path().join("ortho.json");
    let parser_ir = temp.path().join("parser-ir.json");
    let divergence = temp.path().join("divergence.json");
    std::fs::write(&aat, serde_json::to_string_pretty(&ortho_fixture_aat()).unwrap()).unwrap();
    std::fs::write(&ortho, serde_json::to_string_pretty(&ortho_fixture_bundle()).unwrap()).unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("convert")
        .arg("--aat")
        .arg(&aat)
        .arg("--ortho-annotations")
        .arg(&ortho)
        .arg("--mapping")
        .arg(repo.join("data/aat-to-parser-ir-mapping-v1.json"))
        .arg("--parser-ir-out")
        .arg(&parser_ir)
        .arg("--divergence-out")
        .arg(&divergence)
        .arg("--abc-root")
        .arg(abc)
        .status()
        .unwrap();

    assert!(status.success());
    let parser_ir_json = read_json(&parser_ir).unwrap();
    assert_eq!(
        parser_ir_json.pointer("/sentences/0/tags/0"),
        Some(&json!("orthographic-katakana"))
    );
    assert!(parser_ir_json.get("orthographic_annotations").is_some());
}
```

This fails until Tasks 1-5 remove the old schema precondition failure and emit
sentence rows.

- [ ] **Step 2: Run failing CLI integration test**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir -- cli_convert_with_ortho_annotations_emits_sentence_tags
```

Expected: FAIL until the checked-in schema/mapping and converter all support
the new fields.

- [ ] **Step 3: Update README contract**

In `ab-validator/crates/ab-aat-to-parser-ir/README.md`, replace the current
precondition language under `--ortho-annotations <PATH>` with:

```markdown
When used with parser-IR schema 0.6.0 or newer, `--ortho-annotations`
emits both:

- `orthographic_annotations`: detector provenance and byte ranges
- `sentences[].tags`: renderer-facing sentence tags, including
  `orthographic-katakana`

The annotation file must describe the same source as the AAT input:
`work_id` must equal `AAT.work_id`, and `work_content_hash` must equal
`AAT.meta.source_hash`.
```

- [ ] **Step 4: Run integration tests**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/crates/ab-aat-to-parser-ir/README.md
git commit -m "test(parser-ir): cover orthographic sentence CLI path"
```

---

### Task 7: Ruby and Tokenization Guardrails

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Modify: `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs`
- Modify: `abc/test/abc/tools/parser_ir_tei_test.clj`

**Interfaces:**
- Protects the design invariant that ruby reading is not sentence-splitting or tokenizer input.

- [ ] **Step 1: Add ab-validator sentence span test with ruby**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, add a test
where:

- visible base text is `名前はまだ無い。ここは次。`;
- ruby reading is different, e.g. `めいしょう`;
- two sentence spans follow visible base text byte offsets, not ruby reading
  length;

```rust
#[test]
fn sentence_segmentation_uses_ruby_base_not_reading() {
    let (schemas, mapping) = schemas_and_mapping_accepting_sentences_and_orthographic_annotations();
    let aat = json!({
        "version": 1,
        "work_id": "ruby-sentence",
        "meta": base_meta(
            "utf-8",
            "sha256:abababababababababababababababababababababababababababababababab",
        ),
        "blocks": [{
            "kind": "paragraph",
            "content": [{
                "kind": "ruby",
                "base": "名前",
                "reading": "めいしょう"
            }, {
                "kind": "text",
                "value": "はまだ無い。ここは次。"
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

    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/start"),
        Some(&json!(0))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/0/span/end"),
        Some(&json!("名前はまだ無い。".len()))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span/start"),
        Some(&json!("名前はまだ無い。".len()))
    );
    assert_eq!(
        output.parser_ir.pointer("/sentences/1/span/end"),
        Some(&json!("名前はまだ無い。ここは次。".len()))
    );
    validate_value(&schemas.parser_ir_schema, &output.parser_ir, "parser-IR").unwrap();
}
```

- [ ] **Step 2: Add ABC TEI ruby-inside-sentence test**

In `abc/test/abc/tools/parser_ir_tei_test.clj`, add:

```clojure
(deftest sentence-wrapper-preserves-ruby-reading-test
  (testing "sentence rendering wraps ruby without using reading as sentence text"
    (let [result (parser-ir-tei/render
                  {"nodes" [{"type" "ruby"
                             "span" {"start" 0 "end" 6 "coordinate_system" "decoded_utf8"}
                            "ruby" {"base" "名前"
                                     "reading" "めいしょう"
                                     "scope" "explicit"}}
                            {"type" "text"
                             "span" {"start" 6 "end" 24 "coordinate_system" "decoded_utf8"}
                             "text" "はまだ無い。"}
                            {"type" "text"
                             "span" {"start" 24 "end" 39 "coordinate_system" "decoded_utf8"}
                             "text" "ここは次。"}]
                   "paragraphs" [{"id" "p000000"
                                  "span" {"start" 0 "end" 39 "coordinate_system" "decoded_utf8"}
                                  "span_source" "direct"
                                  "node_range" {"start" 0 "end" 3}
                                  "role" "body"
                                  "source_pointer" "blocks[0]"
                                  "classification" "direct"}]
                   "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                            "splitter_id" "ab-plaintext-japanese-v1"
                                            "coordinate_system" "decoded_utf8"
                                            "coverage" "body-paragraphs"}
                   "sentences" [{"id" "s000000"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 0 "end" 2}
                                 "tags" []
                                 "orthographic_annotation_indices" []}
                                {"id" "s000001"
                                 "paragraph_id" "p000000"
                                 "span" {"start" 24 "end" 39 "coordinate_system" "decoded_utf8"}
                                 "node_range" {"start" 2 "end" 3}
                                 "tags" []
                                 "orthographic_annotation_indices" []}]} )
          paragraph (some #(when (= :p (first %)) %) (hiccup-nodes (:body result)))]
      (is (= [:p
              [:s
               [:ruby {:type "furigana"}
                [:rb "名前"]
                [:rt "めいしょう"]]
               "はまだ無い。"]
              [:s "ここは次。"]]
             paragraph)))))
```

- [ ] **Step 3: Add ruby oracle alignment test**

In `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs`, add or extend tests
so they explicitly cover:

- exact token tiling of a ruby base can compare/override reading evidence;
- boundary-misaligned tokens record `boundary-misalign`;
- ruby reading is not used to change token spans.

- [ ] **Step 4: Run focused tests**

Run:

```bash
cd ab-validator
cargo test -p ab-morph-run -- oracle::ruby
cargo test -p ab-aat-to-parser-ir -- sentence_segmentation_uses_ruby_base_not_reading
cd ..
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/crates/ab-morph-run/src/oracle/ruby.rs \
        abc/test/abc/tools/parser_ir_tei_test.clj
git commit -m "test(parser-ir): guard ruby reading and sentence boundaries"
```

---

### Task 8: Final Verification and Migration Notes

**Files:**
- Modify: `ab-validator/docs/superpowers/specs/2026-07-07-ortho-sentence-annotation-design.md`
- Modify: `ab-validator/docs/superpowers/specs/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md` only if implementation changed decisions.
- Modify README files touched by CLI behavior.

**Interfaces:**
- Produces clean handoff docs stating the old D3 decision is superseded.

- [ ] **Step 1: Mark old decision as superseded**

In `2026-07-07-ortho-sentence-annotation-design.md`, add a short note near the
top:

```markdown
**Follow-up correction:** Decision D3 ("ABC owns sentence splitting") is
superseded by
`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`.
ab-validator now owns parser-IR sentence segmentation evidence; ABC renders it.
```

- [ ] **Step 2: Run complete focused verification**

Run from repo root:

```bash
git diff --check
cargo test -p ab-plaintext --manifest-path ab-validator/Cargo.toml
cargo test -p ab-aat-to-parser-ir --manifest-path ab-validator/Cargo.toml
nix build .#checks.x86_64-linux.ab-validator-cargo-check --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-kondo --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: all commands exit 0.

- [ ] **Step 3: Commit docs**

```bash
git add ab-validator/docs/superpowers/specs/2026-07-07-ortho-sentence-annotation-design.md \
        ab-validator/docs/superpowers/specs/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md \
        ab-validator/docs/superpowers/plans/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-plan.md
git commit -m "docs(parser-ir): plan sentence segmentation TEI propagation"
```

---

## Self-Review

- Spec coverage: Task 0 covers splitter divergence measurement. Task 1 covers
  schema and coherence validation, including paragraph/sentence tiling. Task 2
  covers real sentence projection, node splitting, index rewriting,
  orthographic overlap, and parser-IR emission. Task 3 covers accumulator-safe
  ABC TEI rendering. Task 4 covers TEI header/profile updates. Task 5 covers
  schema mirror and mapping hash rotation while keeping synthetic evidence out
  of probe-derived divergence rules. Task 6 covers the CLI sidecar path. Task 7
  covers ruby/tokenization guardrails. Task 8 closes documentation consistency.
- Placeholder scan: no placeholder markers or unspecified test commands remain.
- Type consistency: parser-IR fields are consistently named
  `sentence_segmentation`, `sentences`, `tags`, and
  `orthographic_annotation_indices`; Rust and Clojure snippets use the same
  field names.
