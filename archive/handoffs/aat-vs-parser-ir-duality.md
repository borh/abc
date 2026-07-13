# AAT vs. Parser-IR Duality — Evidence Report and Decision Framework

> Scope: read-only investigation of the cross-repo protocol boundary between
> `/home/bor/Projects/ab-validator` (AAT) and `/home/bor/Projects/abc`
> (parser-IR). No code or schema was changed.

## 1. Field-by-Field Schema Comparison

### Top-level envelope

| Aspect | ABC parser-IR (`schemas/parser-ir.schema.json`) | ab-validator AAT (`data/aat-schema.json`) |
|---|---|---|
| Required top fields | `schema_id`, `schema_hash`, `source`, `nodes`, `warnings`, `errors` (`parser-ir.schema.json:6`) | `version`, `work_id`, `blocks`, `meta` (`aat-schema.json:5`) |
| Versioning | `schema_id` (URI string) + `schema_hash` (sha256) (`parser-ir.schema.json:9-10`) | top-level `version` constant `1` (`aat-schema.json:8`) |
| Source identity | `source.work_content_hash`, `encoding`, `normalization`, optional `source_path` (`parser-ir.schema.json:11-19`) | `meta.source_hash`, `source_encoding`, plus `adapter`/`adapter_version` (`aat-schema.json:25-43`) |
| Producer identity | none at top level | `meta.adapter`, `meta.adapter_version` (`aat-schema.json:25-43`) |
| Parse status | none | `meta.parse_complete` boolean (`aat-schema.json:25-43`) |
| Performance / fidelity metadata | none | `meta.metrics` (required 19 timing/count fields) and optional `meta.semantic_summary` (`aat-schema.json:44-90`, `aat-schema.json:91-104`) |
| Diagnostics | separate `warnings` and `errors` arrays of rich diagnostics (`parser-ir.schema.json:26-30`) | `meta.warnings` array of `{message, line?, path?}` (`aat-schema.json:137-146`) |
| Extra fields | `additionalProperties: false` everywhere; no extension hook | `patternProperties: {"^x-": true}` on every block/inline kind (`aat-schema.json` paragraph/heading/text/ruby/gaiji/accent/figure/raw/inline_container/warigaki defs) |

### Document tree shape

| Aspect | parser-IR | AAT |
|---|---|---|
| Container | flat `nodes[]` (`parser-ir.schema.json:22-24`) | nested `blocks[]`; blocks are `paragraph`, `heading`, or `block_container` (`aat-schema.json:118-176`) |
| Block children | not applicable (flat) | `paragraph`/`heading` carry `content[]` inlines; `block_container` carries `children[]` blocks (`aat-schema.json:150-176`) |
| Inline set | there is no separate inline layer | `inline` oneOf: text, ruby, gaiji, accent, figure, raw, inline_container, warigaki (`aat-schema.json:178-194`) |

### Inline/block node kinds

| Concept | parser-IR representation | AAT representation |
|---|---|---|
| Plain text | `type: "text"` node with `text` (`parser-ir.schema.json:76-91`) | `kind: "text"` inline with `value` (`aat-schema.json:195-206`) |
| Ruby | `type: "ruby"` node with `ruby.base`, `ruby.reading`, `ruby.scope` enum `explicit/inferred/group/mid-word/ambiguous` (`parser-ir.schema.json:92-119`) | `kind: "ruby"` inline with `base`, `reading`, `direction` `right/left`, optional nested `base_content`/`reading_content` (`aat-schema.json:207-225`) |
| Gaiji | `type: "gaiji"` node with `raw_marker`, `reference`, `unicode`, `ivs`, `image_or_glyph_fallback`, `resolved` boolean (`parser-ir.schema.json:120-140`) | `kind: "gaiji"` inline with `description`, `resolved` (string|null), `jis_code`, `unresolved_reason` (`aat-schema.json:226-244`) |
| Accent / emphasis | `type: "emphasis"` node with `text` and free `style` string (`parser-ir.schema.json:165-185`) | `kind: "accent"` inline with `code`, `name`, `resolved` (`aat-schema.json:245-263`) |
| Figure / image | `type: "image"` node with `src`, `alt`, `path_hint` (`parser-ir.schema.json:240-263`) | `kind: "figure"` inline with `filename`, `alt`, `css_class`, `width`, `height`, optional `caption[]` inlines (`aat-schema.json:264-287`) |
| Caption | standalone `type: "caption"` node with `text` and `target` (`parser-ir.schema.json:264-284`) | represented as `caption_block` or `inline_container` kind `caption` (`aat-schema.json:170-176`, `aat-schema.json:319-338`) |
| Heading | standalone `type: "heading"` node with `text`, `level` 1-6 (`parser-ir.schema.json:186-206`) | block `kind: "heading"` with `level` 1-3, `style`, `content[]` inlines (`aat-schema.json:151-163`) |
| Indentation | standalone `type: "indentation"` node with `depth`, optional `text` (`parser-ir.schema.json:207-226`) | `jisage_block` container with `x-indent` extension emitted by `ab-ir` projection (`ab-ir/src/lib.rs:652-659`) |
| Page/line break | standalone `type: "page-break"` node with `marker`, `page_number` (`parser-ir.schema.json:227-239`) | not a distinct node; projected as paragraph with `x-break-kind` extension (`ab-ir/src/lib.rs:680-689`) |
| Quote | standalone `type: "quote"` node with `marker_type`, `nesting_level`, optional `text` (`parser-ir.schema.json:285-305`) | `quote_block` block container (`aat-schema.json:170-176`) |
| Editor note | `type: "editor-note"` node with `note.raw` and `category` enum (`parser-ir.schema.json:141-164`) | **no first-class node**; adapter maps it to `text` with `x-editor-note` extension (`ab-ir/src/lib.rs:1044-1053`) |
| Style scopes | `emphasis` only, via one `style` string | `inline_container` kinds `style`, `font_size`, `tcy`, `keigakomi`, `yokogumi`, `caption` with nested `content[]` (`aat-schema.json:307-338`) |
| Raw / unsupported | not represented | `kind: "raw"` inline with `source` (`aat-schema.json:288-306`) |
| Warigaki | not represented | `kind: "warigaki"` inline with `upper[]`/`lower[]` inlines (`aat-schema.json:339-358`) |

### Span / coordinate model

| Aspect | parser-IR | AAT |
|---|---|---|
| Required span fields | `start`, `end` (`parser-ir.schema.json:43-47`) | `line_start`, `line_end`, `byte_start`, `byte_end` (`aat-schema.json:16-21`) |
| Optional span fields | `line`, `column` (`parser-ir.schema.json:48-49`) | none (schema), but contract says future additions possible (`aat-contract.md:192-194`) |
| Coordinate semantics | unspecified in schema | default `decoded_utf8`: UTF-8 byte offsets into decoded source, 1-based line numbers (`aat-contract.md:180-187`) |
| Semantic summary spans | none | `source_span` with `start`/`end` for `semantic_summary` nodes (`aat-schema.json:74-78`) |

### Versioning and extension model

| Aspect | parser-IR | AAT |
|---|---|---|
| Version field | none explicit; identity is `schema_id` + content hash | `version` integer, currently `1`; schema-compatible additions stay in v1, breaking changes require v2 (`aat-contract.md:20-26`) |
| Extension mechanism | none (`additionalProperties: false` on every object) | `x-*` pattern properties on blocks/inlines; `x-provenance` used to mark source-derived nodes (`aat-contract.md:7-8`) |

### `source` vs `meta`

- parser-IR `source` is **input-centric**: it records the work's content hash, encoding, and normalization policy so ABC can reproduce identity (`parser-ir.schema.json:11-19`).
- AAT `meta` is **adapter/run-centric**: it records which adapter produced the tree, whether the parse completed, timing/count metrics, and adapter warnings (`aat-schema.json:25-90`).

## 2. Classification of the Duality

**Verdict candidate: (c) partially overlapping with real divergences.**

Evidence for **(a) isomorphic / same concept, two names**:

- Both are JSON renditions of an Aozora Bunko parse result.
- Both cover text, ruby, gaiji, headings, figures/images, indentation, warnings, and spans.
- The `ab-ir` design document explicitly calls AAT a *projection* from a parser-neutral IR (`docs/superpowers/specs/2026-04-26-parser-neutral-ir-design.md`).

Evidence for **(b) genuinely distinct**:

- AAT carries adapter-fidelity concerns that parser-IR does not:
  - `meta.adapter` / `adapter_version` identify the producer (`aat-schema.json:25-43`).
  - `meta.parse_complete` distinguishes parser-backed vs fallback output (`aat-schema.json:25-43`).
  - `meta.metrics` records performance and fallback decisions (`aat-schema.json:44-90`).
  - `x-provenance` marks source-derived reconstruction vs parser events (`aat-contract.md:7-8`).
  - AAT has an oracle/selector ecosystem (`aat-contract.md` selector protocol) and a documented fidelity matrix (`docs/adapter-fidelity.md`).
- parser-IR carries publication-pipeline concerns:
  - explicit `schema_id` / `schema_hash` for artifact identity (`parser-ir.schema.json:9-10`).
  - separate `warnings` and `errors` with coded diagnostics (`parser-ir.schema.json:300-320`).
  - nodes such as `editor-note`, `page-break`, `quote`, `caption` that are closer to TEI/source markup than to adapter output.

Evidence for **(c) the correct classification**:

- The tree models are not interchangeable: AAT is nested block/inline, parser-IR is flat.
- The span coordinate systems differ (line/byte vs. start/end/line/column).
- The node vocabularies differ (e.g., AAT `accent` and `raw` have no parser-IR counterpart; parser-IR `editor-note`, `image`, `caption`, `quote`, `page-break`, `indentation` are not AAT native kinds).
- Ruby/gaiji detail models overlap but differ in fields (`scope` vs. `direction`, `raw_marker/ivs/image` vs. `jis_code/unresolved_reason`).
- AAT supports runtime extensions; parser-IR is closed.

Therefore the duality is **not** (a) because the shapes and concerns differ, and **not purely** (b) because the core concept space (parse result for Aozora works) is the same. It is **(c)**.

## 3. Option Analysis and Mapping-Owner Gap

### Option A — ABC single source, ab-validator vendors

**Evidence for:**

- The boundary doc already claims "ABC schemas are authoritative for parser IR and diagnostics accepted into the ABC pipeline" (`docs/v0-design-bundle/ab-validator-boundary.md:27-28`).
- Vendoring `parser-ir.schema.json` into ab-validator would let the Rust build assert `parser_ir_schema_hash` at emission time instead of only in ABC's fixture test.
- This mirrors how the checked-in `examples/ab-validator-output/manifest-inputs.json` already carries `parser_ir_schema_hash` (`examples/ab-validator-output/manifest-inputs.json:10`).

**Evidence against:**

- ab-validator currently has **zero** parser-IR emission code (`grep -R parser_ir /home/bor/Projects/ab-validator` finds only `analyzer_schema_id` in morph summaries, unrelated). Its adapters emit AAT (`adapters/aozora-rs/src/main.rs:32-37`; `adapters/aozora2html/src/main.rs:57-74`).
- Forcing parser-IR emission means either rewriting adapters or adding an AAT→parser-IR mapping inside ab-validator; neither is implemented.
- ABC's `validate-design-bundle` does not run ab-validator; it only validates a hand-curated fixture (`src/abc/tools/validate_design_bundle.clj:102-104`).

**Test that would decide it:**

- Can ab-validator, after vendoring the schema, produce a `parser-ir.json` from a real adapter run that validates against `schemas/parser-ir.schema.json` and materializes through `abc.tools.materialize-import`?

### Option B — skeleton `ab-schemas` repo both depend on

**Evidence for:**

- Would make the contract a first-class dependency of both repos, not a footnote in ABC's `schemas/` directory.
- Supports the content-addressed identity story: both repos compute the same `parser_ir_schema_hash` from a shared schema commit.
- Decouples schema lifecycle from ABC feature releases.

**Evidence against:**

- Adds organizational overhead (release cadence, pinning, breaking-change policy) that does not yet exist.
- Does not resolve the AAT vs parser-IR duality by itself; it only relocates the schemas. ab-validator would still need to emit parser-IR or a mapping would still be required.
- AAT currently versions by integer (`version: 1`) while parser-IR versions by schema hash; a shared repo would need to reconcile these two versioning stories.

**Test that would decide it:**

- Can both repos build from a pinned `ab-schemas` input and agree on schema hashes for the same schema version? Is the governance cost lower than the duplicated-schema cost?

### Option C — AAT and parser-IR deliberately distinct, with an AAT→parser-IR mapping

**Evidence for:**

- The boundary doc explicitly allows ab-validator to "use a different internal representation" (`docs/v0-design-bundle/ab-validator-boundary.md:31-32`).
- AAT is already the emitted, tested, schema-validated output of ab-validator; parser-IR is the documented ABC contract. This option preserves both investments.
- `ab-ir` already treats AAT as a projection from a richer internal IR (`docs/superpowers/specs/2026-04-26-parser-neutral-ir-design.md`); adding a second projection target is architecturally consistent.

**Evidence against:**

- **No mapping owner is named.** The boundary doc says ABC validates imported bundles, but it never says who produces `parser-ir.json` from AAT (`docs/v0-design-bundle/ab-validator-boundary.md:27-35`).
- **No mapping is schema-addressable.** There is no JSON Schema, function, or ADR defining the AAT→parser-IR transform; the only bridge is the hand-curated fixture `examples/ab-validator-output/parser-ir.json`.
- **No code implements the mapping.** `grep -R parser_ir /home/bor/Projects/ab-validator` is empty; `grep aat /home/bor/Projects/abc/src` is empty.

**Test that would decide it:**

- A mechanical, reproducible translation from every adapter's AAT fixture (e.g. `adapters/aozora-rs/tests/fixtures/ruby_gaiji.aat.json`, `adapters/aozora2html/tests/fixtures/*.aat.json`) into parser-IR that passes `nix run .#validate-design-bundle` and `clojure -M:abc/materialize-import`.

### Option D — collapse into one schema

**Evidence for:**

- Eliminates the duality and the need for a mapping.
- Reduces future schema-maintenance surface if the overlap is large.

**Evidence against:**

- Collapsing would force ab-validator to give up adapter-fidelity metadata (`meta.metrics`, `semantic_summary`, `x-provenance`, selector protocol) or force ABC to absorb them.
- It would force parser-IR to abandon TEI-oriented node kinds (`editor-note`, `quote`, `page-break`, `indentation`) or force AAT adapters to emit those kinds.
- The coordinate models are incompatible (line+byte vs start/end+line/column); a single schema would have to pick one and break existing consumers.
- Blast radius spans two repos' tests, oracle cases, morph-warehouse pipeline, and ABC's TEI fixtures.

**Test that would decide it:**

- A single schema that can represent both a real ab-validator adapter-fidelity report and a real ABC TEI/publication pipeline input without information loss and without confusing either consumer.

### Confirmed gap

The boundary documents **claim** a clean separation but **do not assign ownership** of the AAT→parser-IR conversion. Specifically:

- `docs/v0-design-bundle/ab-validator-boundary.md:27-35` says ABC schemas are authoritative and ab-validator may use a different internal form, but never names the converter or its repository.
- `docs/adr/0007-external-parser-validation-boundary.md` defines the file bundle as the handoff and says ab-validator "may use any internal parser representation," yet the listed bundle file is `parser-ir.json`, which ab-validator cannot currently emit.
- `src/abc/tools/validate_design_bundle.clj:27-47` checks that the producer-supplied `parser_ir_schema_hash` matches ABC's hash, but it does not check that the producer actually *owns* or *produces* parser-IR; it only checks hash equality on a fixture.
- The only existing bridge is the checked-in fixture `examples/ab-validator-output/parser-ir.json`, which is not generated by ab-validator code.

## 4. Versioning Mismatch and v1→v2 Contract Design

### The mismatch

- AAT versions by a top-level integer: `version` is `const: 1` (`data/aat-schema.json:8`) and the contract says "Schema-compatible additions to AAT v1 may add optional fields. Required field changes or changed node semantics require AAT v2" (`aat-contract.md:20-26`).
- parser-IR versions by content hash: `schema_hash` is a SHA-256 of the schema file (`parser-ir.schema.json:10`), checked against the live ABC schema in `src/abc/tools/validate_design_bundle.clj:27-47`.
- The boundary doc mentions a "registered compatibility rule" (`docs/v0-design-bundle/ab-validator-boundary.md:33-34`), but **no such registry exists**.

### Why schema-hash alone is insufficient as a compatibility signal

- A SHA-256 hash is an **exact-content** digest, not a compatibility declaration. Any additive field changes the hash and invalidates every older producer, even when the change is backwards-compatible.
- It cannot express "AAT v2 documents can be mapped to parser-IR v1 without loss" or "parser-IR v2 is required for AAT v2."
- There is no field today that records which AAT version a parser-IR document was derived from.

### Minimal v1→v2 contract design

The following is the smallest contract that closes the gap without collapsing the two schemas:

1. **Add explicit version fields to both sides.**
   - parser-IR should expose a `schema_version` (e.g., semver or integer) in addition to `schema_hash`. The `schema_id` URI should include the version.
   - AAT already has `version`; keep it as the authoritative AAT version.

2. **Create a compatibility registry.**
   - A declarative file (e.g. `docs/adr/aat-parser-ir-compatibility.edn` or `data/aat-parser-ir-compatibility.json`) listing compatible pairs:
     ```
     AAT 1.x  -> parser-IR 1.x  (full)
     AAT 2.x  -> parser-IR 1.x  (lossy: drop metrics/semantic_summary; map new block kinds to raw)
     AAT 2.x  -> parser-IR 2.x  (full)
     ```
   - Each entry states the mapping version, the compatibility direction, and which AAT fields are dropped or transformed.

3. **Distinguish breaking vs additive schema changes.**
   - AAT additive changes bump a minor/patch component and are handled by the existing mapping.
   - AAT breaking changes bump the major version and require either a parser-IR major bump or an explicit lossy mapping rule.

4. **Record provenance of the mapping in emitted parser-IR.**
   - Add an optional `derived_from_aat_version` field or include it in `source`/provenance metadata so consumers can look up the correct compatibility rule.

5. **Implement the registry in the validator.**
   - Replace the single hash-equality check in `schema-hash-errors` (`src/abc/tools/validate_design_bundle.clj:27-47`) with a lookup: accept the bundle if the declared AAT/parser-IR versions match a registered rule.

This design preserves the content-hash invariant for exact schema identity while adding the explicit compatibility layer the boundary doc currently hand-waves.

## 5. Hammock-Driven-Design Questions and Missing Evidence

The following questions should be answered in a hammock session *before* any implementation.

### Q1. What is the canonical boundary artifact — parser-IR, AAT, or both?

- Why it matters: it determines whether the boundary is a single contract or a translation boundary.
- Missing evidence:
  - A list of every downstream consumer of AAT (morph warehouse, fidelity reports, oracle tooling) and parser-IR (TEI generation, manifest materialization, RDF views).
  - Which fields each consumer actually requires.

### Q2. Who owns the AAT→parser-IR mapping?

- Why it matters: ownership determines tests, ADRs, CI gates, and where bugs are filed.
- Missing evidence:
  - Whether ab-validator team has capacity/desire to emit parser-IR, or whether ABC team has enough Aozora parsing context to maintain the mapping.
  - A decision-record draft assigning ownership (the boundary docs currently avoid this).

### Q3. What is the versioning contract across the boundary?

- Why it matters: without it, every schema edit is a potential cross-repo breaking change.
- Missing evidence:
  - Historical schema-change frequency in both repos.
  - Whether additive changes are common (favor compatibility registry) or breaking changes dominate (favor lock-step releases).

### Q4. Does parser-IR need to preserve adapter-fidelity metadata?

- Why it matters: if parser-IR must stay lossy, AAT remains the long-term fidelity record; if parser-IR must carry metrics/provenance, the schemas converge.
- Missing evidence:
  - Whether ABC's publication pipeline ever needs `meta.metrics`, `semantic_summary`, `parse_complete`, or `x-provenance`.
  - Whether loss of those fields affects reproducibility claims in manifests.

### Q5. Which tree model is the long-term source of truth — flat parser-IR nodes or nested AAT block/inline?

- Why it matters: the rest of the pipeline (TEI, tokenization, annotation) must target one model.
- Missing evidence:
  - TEI serialization requirements for block vs inline nesting.
  - Round-trip fidelity tests from Aozora source → adapter → IR → TEI.
  - Downstream tooling constraints (e.g., morph analyzers consume AAT today via `crates/ab-morph-run`).

## Verdict

**Classification: (c) partially overlapping with real divergences.**

Confidence: **high**.

The AAT and parser-IR share the same domain (Aozora Bunko parse output) but differ in tree shape, span coordinate system, node vocabulary, provenance model, extension mechanism, and versioning. They are not merely two names for the same thing, yet they are also not cleanly independent because no documented, tested, schema-addressable mapping separates them. The most urgent gap is not picking option A/B/C/D, but answering **who owns the AAT → parser-IR mapping** and writing the **version compatibility registry** the boundary documents already imply.
