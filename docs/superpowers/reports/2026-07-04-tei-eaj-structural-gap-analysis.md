# TEI-EAJ Structural Gap Analysis

Date: 2026-07-04

## Verdict

`PARSER_IR_LEVEL3_REPRESENTABLE_WITH_ADAPTER_AND_SOURCE_AUTHORITY_GAPS`

Parser-IR can now carry Level 3 paragraph/source-note structure when the AAT
input supplies it. The whole pinned TEI-EAJ/aozora_tei comparison no longer has
a row-level parser-IR representability gap:

| Metric | Value |
|---|---:|
| TEI-EAJ rows | 62 |
| candidate work IDs | 50 |
| rows with AAT evidence | 57 |
| parser-IR gap rows | 0 |
| source-attribution gap rows | 0 |
| adapter gap rows | 18 |
| evidence gap rows | 5 |

This is not yet a full Level 3 TEI admission claim. The remaining blockers are
now evidence and policy blockers:

- source-authority representability is still failing,
- adapter paragraph fidelity is inconsistent,
- TEI-EAJ uses drama, verse, note, front/back, and Level 4 enrichment profiles
  that cannot be judged by paragraph-count parity alone,
- one parser input, `aozora2`, is currently Melos-scoped rather than broad
  workset evidence.

The two former `15099` evidence gaps were false negatives. TEI-EAJ identifies
the work as `15099`, while the measured Aozora corpora materialize the same
source as `000879_104`. The structural expansion now builds a conservative
alias map from TEI-EAJ filenames such as `104_15099.xml`, so both `15099` rows
receive AAT evidence from:

- `aozora-rs:15099` -> AAT `work_id = "000879_104"`,
- `aozora2html:15099` -> AAT `work_id = "000879_104"`,
- `aozora-epub3:15099` -> AAT `work_id = "000879_104"`.

## Four-Adapter Matrix

The generated-TEI matrix materializes every available parser input per TEI-EAJ
row through parser-IR and ABC TEI:

| Metric | Value |
|---|---:|
| TEI-EAJ rows attempted | 57 |
| parser-input rows attempted | 173 |
| materialization succeeded | 173 |
| materialization failed | 0 |
| rows skipped | 5 |

Adapter coverage:

| adapter | rows | evidence scope |
|---|---:|---|
| aozora2html | 57 | broad TEI-EAJ workset |
| aozora-epub3 | 57 | broad TEI-EAJ workset |
| aozora-rs | 57 | broad TEI-EAJ workset |
| aozora2 | 2 | Melos-scoped only |

Paragraph origin across the matrix:

| origin | rows |
|---|---:|
| adapter_over_segmented | 123 |
| adapter_collapsed | 18 |
| adapter_under_segmented | 7 |
| aligned | 18 |
| source_note_back_routing | 4 |
| page_break_projection | 3 |

Interpretation: parser-IR paragraph recognition is no longer the generic loss
point. In the dominant path, AAT paragraph-block counts match parser-IR
paragraph counts, and generated TEI body paragraph counts follow parser-IR
except for expected source-note back routing and page-break projection. The
remaining paragraph disagreement is already present at the adapter/AAT boundary
or is a TEI-EAJ profile-policy question.

## TEI-EAJ Structural Profiles

The generated comparison now records TEI-EAJ body/document tag counts and a
primary structural profile for every materialized row. Profile buckets across
the 173 parser-input rows:

| profile | rows |
|---|---:|
| plain_prose | 81 |
| drama | 30 |
| lv4_enrichment | 29 |
| notes | 21 |
| front_back_matter | 6 |
| verse | 6 |

Paragraph origin by TEI-EAJ profile:

| profile | adapter collapsed | adapter over | adapter under | aligned | page break | source-note back |
|---|---:|---:|---:|---:|---:|---:|
| drama | 8 | 12 | 7 | 1 | 2 | 0 |
| plain_prose | 1 | 72 | 0 | 8 | 0 | 0 |
| lv4_enrichment | 2 | 17 | 0 | 6 | 0 | 4 |
| notes | 5 | 14 | 0 | 2 | 0 | 0 |
| front_back_matter | 0 | 5 | 0 | 1 | 0 | 0 |
| verse | 2 | 3 | 0 | 0 | 1 | 0 |

This profile split explains why "same TEI-EAJ `<p>` count" is the wrong Level 3
gate. For example, `1805` is a drama row: TEI-EAJ contains `sp`, `speaker`, and
`stage`, while AAT adapters expose very different paragraph-like units. The
right next step is to decide which TEI-EAJ structures are Level 3 parser
requirements and which are Level 4/editorial enrichment or adapter-specific
segmentation policy.

## Remaining Evidence Gaps

The remaining 5 evidence gaps are TEI-EAJ rows with no Aozora work ID in ABC's
workset export. These are not parser-IR representability conclusions.

| title | TEI-EAJ file | level/state | TEI p |
|---|---|---|---:|
| 源氏物語 第1冊 | `data/draft/tei_lib_lv2/01.xml` | Level 2 draft | 1 |
| 源氏物語 第2冊 | `data/draft/tei_lib_lv2/02.xml` | Level 2 draft | 1 |
| Title | `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml` | Level 2 draft | 2 |
| 勢理客村湧川親雲上勤職書 | `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml` | etc | 4 |
| 校異源氏物語・きりつぼ | `data/etc/校異源氏物語_header更新版.xml` | etc | 1 |

ABC should classify these rows as out-of-scope for Aozora parser compatibility
or provide explicit source-material mappings before they are used in parser
compatibility gates.

## Current Blockers to Level 3 TEI Generation

1. Source-authority representability must pass or have explicit waivers. The
   current source-authority report is still
   `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`, with 652 unallowlisted
   source markers. Four parser outputs are triangulation, not proof that all
   Aozora source constructs are representable.

2. Adapter paragraph fidelity must be profile-aware. The broad adapters preserve
   enough paragraph data to prove parser-IR can carry it, but not enough to claim
   TEI-EAJ paragraph fidelity. `aozora-rs` still has 16 collapse rows. `aozora2`
   has only the 2 Melos rows. `aozora2html` and `aozora-epub3` mostly over-split,
   but those over-splits mix plain prose, drama, verse, notes, and enrichment.

3. TEI-EAJ structural profiles need admission policy. Drama (`sp`, `speaker`,
   `stage`), verse (`l`, `lg`), notes, front/back matter, and Level 4 enrichment
   (`said`, `persName`, `placeName`, `roleName`) should not all be reduced to
   body paragraph counts. Level 3 should require explicit paragraph/source-note
   representation for prose, then separately classify richer structures as
   Level 3 requirements, Level 4 enrichment, or out-of-scope comparison evidence.

4. Text-policy residuals remain separate from paragraph recognition. The matrix
   still has 104 rows in the `different` best-text-match bucket. Some rows reduce
   under ruby/parenthetical normalization, but true text residuals need their own
   source-text fidelity analysis before they can be used as publication gates.

5. Durable work identity still belongs in ABC. The local TEI-EAJ filename alias
   for `15099` is useful evidence, but ABC should own source aliases for durable
   compatibility records.

## Recommended Bridge to Level 3

1. Keep `parser-ir` paragraph/source-note support as accepted infrastructure:
   generated TEI materialization is clean over the 173-row matrix.
2. Move the admission gate from raw paragraph-count parity to a profile-aware
   gate:
   - plain prose: require populated `paragraphs[]`, source-note routing, and a
     declared text policy,
   - drama/verse: require an explicit decision on whether speaker/stage/line
     structure is Level 3 or deferred,
   - Level 4 enrichment: keep named entities and `said`-style markup outside
     parser compatibility unless ABC promotes them.
3. Resolve source-authority strict errors before claiming all Aozora markdown is
   representable.
4. Fix adapter fidelity in this order:
   - `aozora-rs` collapse rows,
   - `aozora2` workset coverage or explicit Melos-only scope,
   - high-delta plain-prose over-splits,
   - drama/verse rows after the TEI profile policy is decided.
5. Ask ABC to classify the 5 no-work-ID TEI-EAJ rows and to export explicit
   source aliases where available.
