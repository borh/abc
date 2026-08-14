# Paper Demo Trace

This is the current evidence chain for the JADH 2026 paper track. It is meant
to support the presentation narrative, not to replace the implementation
reports.

## Claim And Scope

The paper demo should show one chain:

1. adapter evidence is measured in `ab-validator`,
2. the AAT to parser-IR mapping is identified by version and hash,
3. ABC compatibility admission accepts the measured adapter entries,
4. ABC renders parser-IR into publication-facing plaintext and TEI rendering,
5. tokenizer artifacts are presented as downstream comparisons over the same
   identified text.

## Narrative Spine

Use this trace as a claim ladder, not just as an artifact inventory:

| Step | Paper claim | Evidence to show |
|---|---|---|
| Producer measurement | Parser-IR input is not trusted by parser name alone; it is measured against a corpus and mapping. | `data/parser-evidence-citations.edn` entry `ab-validator/parser-ir-conversion-sync-2026-07-04-mapping-0.2.1`, `PARSER_IR_CONVERTER_CORPUS_CLEAN`, mapping version `0.2.1`, mapping hash `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`. |
| ABC admission | ABC accepts a measured adapter/version/mapping/schema tuple, with exact adapter-version matching. | `data/parser-evidence-citations.edn` entry `ab-validator/aat-parser-ir-compatibility-candidates-2026-07-04-mapping-0.2.1`, `data/aat-parser-ir-compatibility.edn`, and the `{:status :admitted}` admission result. |
| Shared source identity | "Rashōmon" and "Run, Melos!" outputs are downstream of the same bounded source-corpus snapshot. | `paper/demo-source-corpus-snapshot.json`, source manifests, plaintext manifests, and TEI manifests whose `manifest_identity_object.corpus_snapshot_hash` is `sha256:537736a73abf100317a371791533cdb2dc0ed5c8fc4d4f23a8a822174a929586`. |
| "Run, Melos!" TEI comparison | ABC TEI is a reproducible parser-IR publication view; the DHII "Run, Melos!" TEI is the comparison target for human enrichment. | ABC "Run, Melos!" TEI validation result, DHII XML hash, and the structural comparison table. |

The important transition is from *measured compatibility* to *citable derived
artifacts*. The "Run, Melos!" comparison should not be framed as a contest over which
TEI is more complete. It should show that ABC can generate a reproducible
baseline whose identity is explicit, while a curated TEI reference marks the
next enrichment layer: paragraph segmentation, person references, speech, and
character inventory.

Current local demo artifacts:

```text
paper/demo-rashomon-real/
paper/demo-melos-real/
```

`paper/demo-rashomon-real/` uses the existing ABC "Rashōmon" metadata (`work_id`
`000127`, person `000879`) and a real `aozora2html` AAT file converted through
`ab-validator`. `paper/demo-melos-real/` uses 走れメロス (`work_id` `001567`,
person `000035`) with fresh ABC metadata/person records generated from the local
Aozora extended person list. Both parser-IR bodies and renderings are real.

Both demos now share a formal paper-demo source corpus snapshot:

```text
paper/demo-source-workset.edn
paper/demo-source-corpus-snapshot.json
```

Snapshot hash:
`sha256:537736a73abf100317a371791533cdb2dc0ed5c8fc4d4f23a8a822174a929586`

That hash is SHA-256 over the RFC8785/JCS canonical
`snapshot_identity_object`, which lists each selected AAT JSON file, AAT file
hash, parser-IR work content hash, mapping identity, parser-IR schema hash, and
metadata record hash. Each per-work `source.manifest.json` points at this
snapshot hash and links the snapshot descriptor as an `index-entry` sidecar.

In the current two-checkout layout, commands still use `../ab-validator/...`
paths as locators. For paper citations and for the planned monorepo, the stable
identity is the logical component path and report hash recorded in
`data/parser-evidence-citations.edn`, not the physical sibling checkout path.

The snapshot and per-work source manifests are materialized with:

```bash
nix run .#materialize-source-snapshot -- \
  --workset paper/demo-source-workset.edn \
  --output paper/demo-source-corpus-snapshot.json \
  --generated-at 2026-07-04T00:00:00Z
```

## Compact Evidence Table

Use this table as the continuity check for the narrative spine: the same two
real works carry through producer evidence, ABC source identity, publication
materialization, and TEI validation.

| Layer | "Rashōmon" | "Run, Melos!" |
|---|---|---|
| Work ID | `000127` | `001567` |
| Person ID | `000879` | `000035` |
| AAT source | `000879_127-13290a9f54a1.json` | `000035_1567-32ff5a089d67.json` |
| AAT file hash | `sha256:70947b8a74a37f1d60ae401c83a29ee531fccf75bac08c6423ec9a5840d59bad` | `sha256:68c69aef0269123197ddaa211d21e412f746f1d3b5cc45ecb075fac8a8657e35` |
| Work content hash | `sha256:18fd203e4e805fa8cf05d8ee18db05a29f541e83f4043b67091191ce1db4a319` | `sha256:03def0d5f4322d1cf1bb812a7cca4e490464cb95090182a34a6e2665dde22ba7` |
| Metadata record hash | `sha256:9771a47f4f1e7c3e186044078872a5f5e2c316e0c0c8ebc3d66bdea470fd4ef5` | `sha256:2c280415e626b6a87ae7150631257b89162c4b80d37aa787487e47e345469033` |
| Parser-IR nodes | 296 | 253 |
| Parser-IR errors | 0 | 0 |
| Divergence records | 15 | 14 |
| Unsupported occurrences | 0 | 0 |
| TEI validation findings | 0 | 0 |

## Producer Evidence

ABC citation index:

```text
data/parser-evidence-citations.edn
```

Primary citable producer entries:

| Evidence ID | Evidence class | Logical path | SHA-256 | Status |
|---|---|---|---|---|
| `ab-validator/parser-ir-conversion-sync-2026-07-04-mapping-0.2.1` | `:conversion-compatibility` | `ab-validator/docs/superpowers/reports/2026-07-04-post-parser-ir-conversion-sync.md` | `sha256:a8f27f56ffceaacbbcde1d80d823b8e5727186790d6ebc841a45966f1db1761c` | `:citable` |
| `ab-validator/aat-parser-ir-full-corpus-conversion-2026-07-04-mapping-0.2.1` | `:conversion-compatibility` | `ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md` | `sha256:d03afdce19adef4c5a44d7a8caa628fa3b8cc8fc0f0599d047aed261cc9ad7a7` | `:citable` |
| `ab-validator/aat-parser-ir-compatibility-candidates-2026-07-04-mapping-0.2.1` | `:conversion-compatibility` | `ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn` | `sha256:c00481ab65239098a072112dd8c803f0203c6d5776d67864215491715a9616e6` | `:citable` |

The corresponding `current_external_path` values remain temporary locators until
the monorepo migration removes the physical checkout boundary.

Historical `0.2.0` evidence remains in the parser-evidence index and ABC
compatibility registry. The paper demo now uses the `0.2.1` chain.

Current producer-side status:

| Field | Value |
|---|---|
| Verdict | `PARSER_IR_CONVERTER_CORPUS_CLEAN` |
| Mapping artifact | `../ab-validator/data/aat-to-parser-ir-mapping-v1.json` |
| Mapping version | `0.2.1` |
| Mapping hash | `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390` |
| Files attempted | 53,427 |
| Files succeeded | 53,427 |
| Files failed | 0 |
| Mapping rules emitted | 130 of 130 |

The conversion audit generated ABC registry candidates at:

```text
../ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
```

Those candidates cover:

| Adapter | Adapter version | Files succeeded | Rules emitted | Rules missing | Unsupported occurrences |
|---|---|---:|---:|---:|---:|
| `aozora-epub3` | `aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21` | 17,844 | 64 | 66 | 13,234 |
| `aozora-rs` | `aozora-rs-adapter 0.1.0 2b4e8d1` | 17,894 | 26 | 104 | 0 |
| `aozora2html` | `aozora2html-adapter 0.1.0 gem-3.0.1` | 17,689 | 115 | 15 | 14,230 |

Adapter-version matching remains exact. A later adapter build needs a new
measured conversion-audit entry.

## ABC Compatibility Admission

ABC registry:

```text
data/aat-parser-ir-compatibility.edn
```

Admission check in the current two-checkout layout:

```bash
clojure -M:abc/aat-compat-admission -- \
  --candidates ../ab-validator/docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn
```

The durable citation identity for that candidates file is the
`ab-validator/aat-parser-ir-compatibility-candidates-2026-07-04-mapping-0.2.1`
entry in `data/parser-evidence-citations.edn`.

Observed result for the current local state:

```clojure
{:status :admitted
 :missing []
 :conflicts []}
```

This is the presentation point for compatibility: ABC is not trusting a parser
name alone. It is admitting an adapter, adapter version, AAT version, mapping
version, mapping hash, mapping schema hash, parser-IR schema ID, and parser-IR
schema hash as one measured contract.

## Real "Rashōmon" Demo Artifact

Selected work:

| Field | Value |
|---|---|
| Work | 羅生門 |
| Work ID | `000127` |
| Person ID | `000879` |
| AAT source | `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000879_127-13290a9f54a1.json` |
| Adapter | `aozora2html` |
| Adapter version | `aozora2html-adapter 0.1.0 gem-3.0.1` |
| AAT file hash | `sha256:70947b8a74a37f1d60ae401c83a29ee531fccf75bac08c6423ec9a5840d59bad` |
| Work content hash | `sha256:18fd203e4e805fa8cf05d8ee18db05a29f541e83f4043b67091191ce1db4a319` |
| Metadata record hash | `sha256:9771a47f4f1e7c3e186044078872a5f5e2c316e0c0c8ebc3d66bdea470fd4ef5` |

Conversion command:

```bash
cd ../ab-validator
cargo run -p ab-aat-to-parser-ir -- convert \
  --aat /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000879_127-13290a9f54a1.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out /tmp/abc-rashomon-parser-ir/aozora2html-parser-ir.json \
  --divergence-out /tmp/abc-rashomon-parser-ir/aozora2html-divergence.json \
  --abc-root data/abc-schemas
```

Generated parser-IR summary:

| Field | Value |
|---|---:|
| Parser-IR nodes | 296 |
| Parser-IR warnings | 1 |
| Parser-IR errors | 0 |
| Divergence record types | 15 |
| Ambiguity occurrences | 335 |
| Invention occurrences | 135 |
| Loss occurrences | 5 |
| Structural occurrences | 37 |
| Unsupported occurrences | 0 |
| TEI validation findings | 0 |

ABC publication command:

```bash
nix run .#materialize-publication -- \
  paper/demo-rashomon-real/parser-ir.aozora2html.json \
  paper/demo-rashomon-real/metadata-record.json \
  paper/demo-rashomon-real/persons \
  paper/demo-rashomon-real \
  --source-manifest paper/demo-rashomon-real/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z
```

Saved paper-facing outputs:

```text
paper/demo-rashomon-real/parser-ir.aozora2html.json
paper/demo-rashomon-real/divergence.aozora2html.json
paper/demo-rashomon-real/metadata-record.json
paper/demo-rashomon-real/persons/000879.json
paper/demo-rashomon-real/plain.txt
paper/demo-rashomon-real/tei.xml
paper/demo-rashomon-real/plaintext.manifest.json
paper/demo-rashomon-real/tei.manifest.json
paper/demo-rashomon-real/tei-validation-result.json
paper/demo-rashomon-real/source.manifest.json
```

Use this as the main parser/TEI demo. Use `examples/v0/example-work/` only as
the checked design-bundle tracer and regression fixture. The generated real TEI
uses `rend="right"` for ruby direction and passes the project Relax NG and
Schematron layers recorded in `tei-validation-result.json`.

## Real "Run, Melos!" Demo Artifact

Selected work:

| Field | Value |
|---|---|
| Work | 走れメロス |
| Work ID | `001567` |
| Person | 太宰治 |
| Person ID | `000035` |
| Aozora card | `https://www.aozora.gr.jp/cards/000035/card1567.html` |
| AAT source | `/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000035_1567-32ff5a089d67.json` |
| Adapter | `aozora2html` |
| Adapter version | `aozora2html-adapter 0.1.0 gem-3.0.1` |
| Work content hash | `sha256:03def0d5f4322d1cf1bb812a7cca4e490464cb95090182a34a6e2665dde22ba7` |
| Metadata record hash | `sha256:2c280415e626b6a87ae7150631257b89162c4b80d37aa787487e47e345469033` |

Conversion command:

```bash
cd ../ab-validator
cargo run -p ab-aat-to-parser-ir -- convert \
  --aat /db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter/000035_1567-32ff5a089d67.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --parser-ir-out /tmp/abc-melos-parser-ir/aozora2html-parser-ir.json \
  --divergence-out /tmp/abc-melos-parser-ir/aozora2html-divergence.json \
  --abc-root data/abc-schemas
```

Generated parser-IR summary:

| Field | Value |
|---|---:|
| Parser-IR nodes | 253 |
| Parser-IR warnings | 1 |
| Parser-IR errors | 0 |
| Divergence record types | 14 |
| Ambiguity occurrences | 330 |
| Invention occurrences | 94 |
| Loss occurrences | 2 |
| Structural occurrences | 75 |
| Unsupported occurrences | 0 |
| TEI validation findings | 0 |

ABC metadata and publication commands:

```bash
nix run .#aozora-ingest -- \
  --zip /home/bor/Dependencies/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 001567 \
  --output /tmp/abc-melos-meta/metadata-record.json \
  --persons-output-dir /tmp/abc-melos-meta/persons \
  --overwrite

nix run .#materialize-publication -- \
  paper/demo-melos-real/parser-ir.aozora2html.json \
  paper/demo-melos-real/metadata-record.json \
  paper/demo-melos-real/persons \
  paper/demo-melos-real \
  --source-manifest paper/demo-melos-real/source.manifest.json \
  --generated-at 2026-07-04T00:00:00Z
```

Saved paper-facing outputs:

```text
paper/demo-melos-real/parser-ir.aozora2html.json
paper/demo-melos-real/divergence.aozora2html.json
paper/demo-melos-real/metadata-record.json
paper/demo-melos-real/persons/000035.json
paper/demo-melos-real/plain.txt
paper/demo-melos-real/tei.xml
paper/demo-melos-real/plaintext.manifest.json
paper/demo-melos-real/tei.manifest.json
paper/demo-melos-real/tei-validation-result.json
paper/demo-melos-real/source.manifest.json
```

Use this as the TEI-familiar presentation work. The TEI header includes the
Aozora work ID, title, title reading, 太宰治 as author, Aozora person ID, NDC
class, and source edition fields from the ABC metadata record. It passes the
project Relax NG and Schematron layers recorded in `tei-validation-result.json`.

## "Run, Melos!" TEI Reference Comparison

External reference material:

| Reference | Role in paper |
|---|---|
| `https://digitalnagasaki.hatenablog.com/entry/2017/08/01/035811` | Context for an earlier 走れメロス TEI example: paragraph-level TEI, Aozora footer-derived bibliographic header, and later character/speech markup. |
| `https://www.dhii.jp/nagasaki/dazai_all_20191012.xml` | Concrete TEI XML comparison target for 走れメロス. Downloaded local copy hash: `sha256:04b1c343b777a48d281729c5abb7a808f23ccbfba876c6759eced161fd397b63`. |

Structural comparison:

| Feature | ABC generated "Run, Melos!" TEI | DHII reference TEI |
|---|---:|---:|
| TEI namespace | TEI P5 | TEI P5 plus `eaj` namespace |
| Ruby elements | 88 `a:ruby` | 88 `eaj:ruby` |
| Paragraph elements | 1 `a:p` | 18 `p` |
| `persName` elements | 3 in header author names | 102 in header/body |
| `said` elements | 0 | 59 |
| `listPerson` / `person` | 0 / 0 | 1 / 3 |

Paper point: ABC's current "Run, Melos!" TEI is a reproducible publication view from
parser-IR and manifest identity. The DHII reference is a useful comparison for
human-enriched TEI: paragraph segmentation, character inventory, person-name
references, and speech attribution. That makes it the right reference for a
future enrichment layer, not evidence that parser-IR generation should pretend
to infer speakers or characters.

## ABC Publication Tracer Fixture

Current checked fixture:

```text
examples/v0/example-work/parser-ir.json
examples/v0/example-work/plain.txt
examples/v0/example-work/tei.xml
```

The parser-IR fixture covers `text`, `ruby`, `gaiji`, `editor-note`, and
`heading` nodes. The plaintext fixture is:

```text
吾輩猫※［＃例字］
一
```

The TEI fixture renders the same body as a paragraph with ruby, a gaiji
reference, an editorial indentation note, and a heading.

The synthetic fixture materializer can be exercised with:

```bash
nix run .#materialize-publication -- \
  examples/v0/example-work/parser-ir.json \
  examples/v0/example-work/metadata-record.json \
  examples/v0/example-persons \
  /tmp/abc-paper-demo-trace \
  --generated-at 2026-07-03T00:00:00Z
```

The design-bundle gate regenerates and validates the publication materializer
outputs:

```bash
nix run .#validate-design-bundle
```

## Presentation Shape

Use this as a short live or recorded walkthrough:

1. Open with producer measurement: corpus-clean parser-IR conversion, mapping version, and mapping hash.
2. Show ABC admission: the measured adapter/version/mapping/schema tuple is admitted with exact adapter-version matching, citing `data/parser-evidence-citations.edn` for the producer report identity.
3. Show source identity: "Rashōmon" and "Run, Melos!" source, plaintext, and TEI manifests share the same source corpus snapshot hash.
4. Show "Run, Melos!" TEI next to the DHII reference comparison: ABC is the reproducible baseline; DHII is the human-enrichment target.
5. Transition to tokenizer comparison as a downstream analytical layer over the same identified text.

## Remaining Demo Gap

The real parser-IR and ABC rendering gap is closed by
`paper/demo-rashomon-real/` and `paper/demo-melos-real/`. The source-corpus
snapshot gap is closed for the paper demo by `paper/demo-source-corpus-snapshot.json`.
The remaining release-quality gap is promoting that paper-demo snapshot recipe
into the normal ABC release tooling instead of maintaining it as a local paper
artifact.
