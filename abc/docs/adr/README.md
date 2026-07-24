# Architecture Decision Records (ABC)

This directory holds the ABC-side decision records for the Soranoha monorepo.
`ab-validator` owns parser/adaptor measurement and corpus reports; ABC
owns publication schemas, TEI profile policy, manifest identity, and
registry admission (see the root `AGENTS.md` design boundaries).

## Representation

Machine-facing decision facts live in `decisions.edn` — one record per
decision: identity (`:slug`), lifecycle (`:status`, `:date`, `:accepted`,
`:validation-scope`, `:release-authority`), descriptive `:topics`,
one-directional `:relations`, and `:claims` (acceptance criteria with
`:statement`, `:kind`, and `:evidence` paths). The Malli schema in
`src/abc/tools/decisions.clj` is the authoritative shape;
`clojure -M:abc/adr-governance` validates shape, graph semantics, evidence
paths, narrative pairing, and INDEX byte currency.

`<slug>.md` files are pure narrative — no tool parses them. `INDEX.md` and
`adr-graph.mmd` are generated views (`clojure -M:abc/adr-governance
--write-index` and `clojure -M:abc/diagrams`).

Relations are a discriminated sum. Lifecycle edges
(`{:class :lifecycle :type :supersedes | :amends | :depends-on :to <slug>}`,
optional `:scope`) are stated once, on the acting record; `superseded by` /
`amended by` are derived for display. Lifecycle graphs are irreflexive and
acyclic; an unscoped supersession replaces the whole target decision and
requires the target status to be `:superseded`, while a scoped supersession
leaves the target's other accepted scopes active. Annotation edges
(`{:class :annotation :type <open keyword> :to <slug>}`, optional source-only
`:note`) carry no lifecycle semantics and are rendered in derived views.

Claims are cited as `slug#c1`. Claim `:kind` and `:topics` are open,
descriptive vocabularies (ADR 0043: no closed set is machine-enforced).
Accepted claims must cite at least one existing evidence path under `test/`,
`fixtures/`, `nix/`, or `docs/evidence/external/`; whether a cited test
entails its criterion is a review judgment. Historical `ADR NNNN` /
`ADR-NNNN-CN` references resolve through `:legacy-number` (see the INDEX
legacy table); the frozen number set is closed — new records are slug-only.

## Status vocabulary

This repository uses a small, fixed status vocabulary. `:status` is a single
keyword; do not invent new statuses without documenting them here.

| Status | Meaning |
| --- | --- |
| `:draft` | Design recorded but not yet accepted as the canonical contract. Implementation may exist speculatively, but the record's gates are not yet treated as binding release boundaries. |
| `:proposed` | Design is accepted-in-principle as the target contract and is under active, partial implementation, but it is **not yet** an Accepted canonical contract: the record explicitly names what remains before promotion. Consumers may build against a `:proposed` contract at their own risk. Target `:validation-scope` / `:release-authority` may already be declared. |
| `:accepted` | The canonical contract for its scope. The claims are binding. Changes go through a new superseding/amending record, not silent edits. |
| `:superseded` | Replaced by a later record with an unscoped `:supersedes` edge. The narrative body and claims are retained unchanged as history; claim evidence may have been deleted by the superseding change. |
| `:withdrawn` | Reserved or retracted before acceptance. The narrative records the reason and points at any successor (`reserved` is the current example). |

### `:draft` vs `:proposed`

Both mean "not yet `:accepted`". `:draft` signals the design is still being
shaped and is not the target contract anyone should build to yet. `:proposed`
signals the design is the agreed target and implementation is in progress,
but the gates that make it canonical are not yet met. For both statuses,
claims are promotion conditions rather than claims of current completion.
Don't use `:proposed` for a design that is still contested or `:draft` for
one that code is already being written against as the intended contract.

A `:proposed` record's narrative MUST use target/proposal language ("this
record proposes …", "the target contract is …") rather than acceptance
language. "Accepts" is reserved for `:accepted` records.

## Edit policy

Decision history stays trustworthy because the parts of a record are not all
equally mutable.

| Where | Mutable? | Rule |
| --- | --- | --- |
| Narrative Decision / Context / Hard Rule | No | Supersede or amend via a new record. |
| Narrative Consequences | Append-only | Document new consequences; do not rewrite historical ones. |
| Narrative Implementation Status | Yes | Dated entries allowed; reflects current code reality, not the decision. |
| `decisions.edn` `:status`, `:accepted` | Yes | Lifecycle transitions. |
| `decisions.edn` `:claims` | Append-only for `:accepted` records | May add claims; never weaken an existing claim without superseding. Evidence citations may be re-pointed when their paths move or die (ADR 0043 precedent). |
| `decisions.edn` `:relations` | Append-only | New edges come with the records that create them. |
| `decisions.edn` `:topics` | Yes | Descriptive, freely improvable. |

## Manifest identity invariants

Three global invariants apply to `manifest_identity_object` across every
record that touches it (`manifest-identity`, `manifest-identity-hardening`,
`owned-aat-parser-ir-mapping`, `analysis-artifact-identity`,
`analysis-packs-and-tokenizer-profiles`, `ruby-annotation-view`):

1. **`null` means "not applicable", not "unknown".** A coordinate is `null` in
   `manifest_identity_object` when it is not part of that artifact kind's
   derivation contract. If a required coordinate cannot be determined, the
   producer MUST emit a failed manifest or no manifest — never a successful
   manifest with `null` standing for an unknown value. (Note: `temporal-modeling`
   deliberately maps Aozora's `不詳`/`未詳` knowledge-gap sentinels to identity
   `null` for date coordinates and preserves the gap-vs-empty distinction in
   `parse_corrections` provenance; that mapping is consistent with this
   invariant because the *identity coordinate* is genuinely not applicable at a
   determinable date precision, not an unknown value smuggled past the rule.)
2. **`artifact_id` is never nested.** `artifact_id` is the hash of
   `manifest_identity_object`; including it inside the object would make
   identity circular.
3. **Re-generating a derived view never changes `artifact_id`.** TEI, RDF/PROV-O,
   Turtle, JSON-LD, Linked Art, IIIF Presentation, request-set resolution,
   manifest indexes, and query packs are derived publication views, not identity
   inputs. Regeneration, re-compaction, schema evolution of the derived view, or
   a status move such as `not_applicable` → `applicable` for IIIF MUST NOT feed
   `manifest_identity_object` or change `artifact_id`. This is the "Hard Rule"
   restated across the publication records (the `:restates-hard-rule` annotation
   edges in `decisions.edn`); it is recorded once here as an invariant rather
   than re-decided per record.

Across schema-version rotations of the identity fields themselves
(`temporal-modeling` → `edtf-level1-decade-century` → `vocabulary-review` /
`predicate-rename-batch-1` → `person-identity-drift-data-model`), older
manifests retain their original `manifest_schema_hash` and are not
reinterpreted under the new rule. This keeps historical manifests valid but
means the same underlying facts can accumulate distinct `artifact_id` values
across schema generations; an equivalence / migration story (e.g. a
non-identity `supersedes_artifact_id` provenance edge) linking generations is
not part of v0 identity and is recorded below as a known open question for
longitudinal analysis over a living corpus.

The current field set of `manifest_identity_object` is amended across multiple
records (`manifest-identity` → `manifest-identity-hardening` →
`owned-aat-parser-ir-mapping` → `analysis-packs-and-tokenizer-profiles` →
`ruby-annotation-view`). The authoritative current field list is the bundled
`schemas/manifest.schema.json`, not any single record's prose; narrative prose
records the *rule* for each field's introduction and nullability semantics.

## Known open questions

These are design questions the decision set has surfaced but not yet decided.
They are recorded here so they are not silently lost; resolving one means
accepting a new record (or amending an existing Decision via a new record),
not editing this list in place as a substitute for a decision.

- **Cross-implementation identity conformance.** `manifest-identity` targets
  identity that works across Rust, Clojure/JVM, Python, JavaScript, RDF
  tooling, and Nix, but v0 verifies only the Clojure/JVM implementation. RFC
  8785 JCS has real edge cases (number formatting, Unicode normalization) that
  are untested across languages. A cross-language conformance fixture is a
  prerequisite before any non-JVM implementation is relied on for
  `artifact_id`.
- **Cross-generation artifact equivalence.** Schema-hash cascade rotations
  intentionally do not reinterpret old manifests, so the same underlying facts
  accumulate distinct `artifact_id` values across schema generations. v0 has
  no equivalence or migration edge (e.g. a non-identity
  `supersedes_artifact_id` provenance link) connecting generations. This is
  acceptable for archival revalidation but tensions with the "living corpus"
  longitudinal-continuity goal. A future record should decide whether such an
  edge is identity-bearing, provenance-only, or out of scope.
- **Cross-view RDF harmonization is not structurally closed.**
  `vocabulary-review` fixed one instance where `abc:` resolved to two
  different IRIs between the Turtle manifest view and the JSON-LD / Linked Art
  view, but the harness still does not structurally assert that the two
  serializations describe the same entity IRIs and the same `artifactId`. The
  identity-invariant check only round-trips `artifactId` through JSON-LD
  expansion; it does not compare TTL and JSON-LD entity identity. A
  harness-level cross-view agreement check is an open item.
- **`:accepted` semantics are fixture-scope.** Most `:accepted` records are
  gated by `validate-design-bundle` over repository-local fixtures
  (`v0-design-bundle-validation`), and the example work (羅生門, text-only) is
  the single exercised fixture. IIIF `applicable` (`iiif-applicability`), real
  split/merge person drift (`person-identity-drift-*`,
  `upstream-ingest-drift-awareness`), and tokenized analysis slices
  (`analysis-artifact-identity`, `analysis-packs-and-tokenizer-profiles`) are
  either deferred or exercised only synthetically. `:accepted` therefore means
  "the contract is binding on the exercised fixture set", not "validated at
  corpus scale or on the design's hard cases."
- **`nix-materialization` cost envelope is unmeasured.** Its own acceptance
  criteria (real manifests, cold builds, incremental rebuilds, the recorded CI
  runner, <30s / <2GB) are unmet; only synthetic evaluator-only probes exist.
  `analysis-artifact-identity` and `analysis-packs-and-tokenizer-profiles`
  both depend on it while it remains `:draft`, so release-scale
  materialization must respect a not-yet-accepted envelope.
