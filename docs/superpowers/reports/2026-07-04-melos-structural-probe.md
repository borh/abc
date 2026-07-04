# Melos Structural Probe

Measurement-only probe for paragraph segmentation and final source-attribution representation in current parser-IR.

## Mapping

- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.1`
- mapping hash: `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`
- parser-IR schema hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Totals

- inputs: 3
- conversions succeeded: 3
- conversions failed: 0
- paragraph gap inputs: 3
- source-attribution gap inputs: 3
- residual-free inputs: 0

## Inputs

| label | adapter | paragraphs in AAT | final attribution candidate | parser-IR paragraph | parser-IR source attribution | residual free |
|---|---|---:|---:|---:|---:|---:|
| aozora-rs | aozora-rs | 79 | true | false | false | false |
| aozora2html | aozora2html | 75 | true | false | false | false |
| aozora-epub3 | aozora-epub3 | 75 | true | false | false | false |

## Detail

### aozora-rs

- path: `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter/000035_1567-32ff5a089d67.json`
- work: `000035_1567`
- adapter: `aozora-rs` `aozora-rs-adapter 0.1.0 2b4e8d1`
- block kinds: `{"paragraph":79}`
- final visible text: `（古伝説と、シルレルの詩から。）`
- AAT hints: `style:low_flying`
- parser-IR node kinds: `{"emphasis":1,"ruby":88,"text":166}`
- paragraph structural records:
  - `S-10` count=79 pointer=`blocks[].paragraph`
- verdict note: AAT preserved 79 paragraph block(s), but parser-IR has no explicit paragraph node
- verdict note: final parenthetical attribution is visible text, not a source-attribution/source-note parser-IR node
- verdict note: paragraph boundaries are present as measured STRUCTURAL divergence records

### aozora2html

- path: `/db/ab-validator/aat-corpus/aozora2html-full-20260704T014828Z-300s/aat/aozora2html-adapter/000035_1567-32ff5a089d67.json`
- work: `000035_1567`
- adapter: `aozora2html` `aozora2html-adapter 0.1.0 gem-3.0.1`
- block kinds: `{"paragraph":75}`
- final visible text: `（古伝説と、シルレルの詩から。）`
- AAT hints: `style:unmapped-div; warning:unmapped XHTML element <div>; x-aozora2html-unmapped:div`
- parser-IR node kinds: `{"emphasis":1,"ruby":88,"text":164}`
- paragraph structural records:
  - `S-10` count=75 pointer=`blocks[].paragraph`
- verdict note: AAT preserved 75 paragraph block(s), but parser-IR has no explicit paragraph node
- verdict note: final parenthetical attribution is visible text, not a source-attribution/source-note parser-IR node
- verdict note: paragraph boundaries are present as measured STRUCTURAL divergence records

### aozora-epub3

- path: `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter/000035_1567-32ff5a089d67.json`
- work: `000035_1567`
- adapter: `aozora-epub3` `aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21`
- block kinds: `{"paragraph":75}`
- final visible text: `（古伝説と、シルレルの詩から。）`
- parser-IR node kinds: `{"ruby":88,"text":163}`
- paragraph structural records:
  - `S-10` count=75 pointer=`blocks[].paragraph`
- verdict note: AAT preserved 75 paragraph block(s), but parser-IR has no explicit paragraph node
- verdict note: final parenthetical attribution is visible text, not a source-attribution/source-note parser-IR node
- verdict note: paragraph boundaries are present as measured STRUCTURAL divergence records

## Interpretation

AAT paragraph segmentation is adapter evidence. Current parser-IR representation is a separate question. A false parser-IR paragraph/source-attribution flag means Level 3 structure is not yet represented by parser-IR, even when visible text is preserved.

## Next Expansion

Use ABC's machine-readable `../abc/docs/handoffs/tei-eaj-aozora-workset-export.json` for the TEI-EAJ missing-counterpart expansion. The expansion should materialize structural evidence for those rows; it should not turn conversion compatibility evidence into a parser-selection claim.
