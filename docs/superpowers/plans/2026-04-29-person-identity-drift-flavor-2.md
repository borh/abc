# Person Identity Drift Flavor 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Design and land the data model + harness for **Person identity drift** — the **splits and merges** that change *which* person an existing `person_id` refers to — so that consumers of the Aozora corpus can trace the lineage of a contributor across corrections without losing the binding from past works to their author at the time of publication. This is the explicit Flavor 2 deferred from `docs/superpowers/specs/2026-04-28-separated-person-records-design.md` lines 15–17, which calls for a "PROV-style event log". (A `rename` event type was considered but dropped from the recommended scope: pure pen-name renames without a split or merge are bibliographic edits to a single `person_record.json` and rotate `person_record_hash` via Flavor 1 already. ADR 0020 may opt to add `rename` as an explicit extension if it can define the type precisely as something other than a Flavor 1 edit; see Cross-cutting open questions and ADR 0020 Decision Step 3.)

**Architecture:** Discovery + ADR 0020 (data model) + ADR 0021 (harness contract) + Phase 3 implementation plan. The data model is the load-bearing decision (single PROV-style event stream, per-person event log, or full PROV-O graph), and the harness contract decides how the events affect identity hashes (do they rotate `person_record_hash`? do they appear in `manifest_identity_object`?). Phase 3 is deliberately written as a *follow-up* plan to be authored once the ADRs are accepted, because the concrete file shapes can only be fixed after the data model lands.

**Tech Stack:** PROV-O (W3C 2013), JSON Schema 2020-12, Apache Jena 5.3.0 SHACL, Clojure (`abc.tools.*`), JCS RFC 8785, sha256 content addressing.

---

## Context — Why Flavor 2 needs an ADR pair, not just code

Flavor 1 (the just-landed separated-person-records milestone) handled **bibliographic correction**: a romaji typo on 芥川 invalidates only the works that reference 000879, not the whole corpus. That works because a person's *identity* is content-addressed: `person_record_hash` is a JCS-SHA-256 of the file's bibliographic content, and every reference site materialises the hash for the snapshot it observed. Edit the file, the hash changes; references reflect that mechanically.

Flavor 2 is a **different** problem. In-scope cases:

- **Split**: a single `person_id` 000123 turns out to be two distinct people (a father and son who shared a pen name). The corpus needs two records and a way to say "these two are descended from 000123". The successor IDs are not literal `000123-a` / `000123-b` — `person_id` is constrained to `^[0-9]{6}$` in both `schemas/person-record.schema.json:21` and the contributor reference at `schemas/metadata-record.schema.json:104`. ADR 0020 must decide the identifier policy (see "Cross-cutting open questions" below).
- **Merge**: `person_id` 000123 and 000456 turn out to be the same person under two pen names. The corpus needs one canonical record and a way to say "these two former IDs collapsed into one".

Out of scope for Flavor 2:

- **Re-attribution**: changing a work's contributor from `person_id` 000123 to 000456 because Aozora's editorial team revised the source. This *is* a `contributors[]` edit and `contributors[]` flows into `metadata_record_hash` via `canonical-identity-form` at `src/abc/tools/metadata_record.clj:40`. A re-attribution rotates `metadata_record_hash` and rebuilds `manifest_identity_object` — that is bibliographic correction (Flavor 1), not identity drift. ADR 0020 must say so explicitly so re-attribution is not silently swept into the drift event log; otherwise Position L's "hashes don't rotate" guarantee fails the moment the first re-attribution lands.

The Flavor 1 mechanism does *not* handle splits and merges:
- A pure rebuild of `person_record_hash` after a split loses the link from prior `manifest_identity_object` entries (which still reference the old hash) to the new world.
- The current `contributors[i].person_record_hash` materialisation captures *what was the identity at the time of publication*, but provides no way to ask "who is the current canonical reading of the person referenced in this 2026 manifest?"
- There is no shape for the *event* itself: who decided the split, when, on what evidence, with what consequence for downstream rebuilds.

So Flavor 2 needs:

1. **A persistent event log** (whatever its shape) that records each split/merge with a timestamp, agent, evidence, and the before/after person IDs. The PROV-style framing is the spec's own recommendation.
2. **A traceability rule** so a manifest from 2024 still resolves to the right author through one or more drift events. With multi-person events (a single split fans out to two successors; a single merge consumes two predecessors), this requires either a global event index, stable event IDs that participants reference, or a defined participant-side traversal rule. Per-person event logs alone do not solve reverse traceability — see "Cross-cutting open questions".
3. **A SHACL contract** so the event log is validatable like every other v0 artifact, *consistent with the existing `prov:Activity` shape at `schemas/manifest.shacl.ttl:59`* (which already requires `prov:used` and `prov:qualifiedAssociation`). A new shape that targets all `prov:Activity` would conflict; the drift-event shape must either target a more specific class (e.g. `abc:DriftEvent rdfs:subClassOf prov:Activity`) or live alongside the existing shape with explicit interaction rules.
4. **A discipline** about whether events affect `person_record_hash` and `metadata_record_hash`. There are two coherent positions; this is the Decision in ADR 0020.

The two coherent positions on the discipline:

- **Position L — Lineage-only (events do NOT rotate identity hashes).** A split/merge/rename adds an event to the log; it does not retroactively change `person_record_hash` for existing records. Old manifests stay valid pointing to the old hash; lineage queries traverse the event log to find the *current* canonical successor. Pros: monotonic identity, manifests never silently rebuild from upstream edits, audit-clean. Cons: queries over "current" need event-log traversal; downstream tools must be event-aware.
- **Position H — Hash-rotating (events DO rotate identity hashes via a successor relation).** A merge collapses two records into one; the surviving record gets a new `person_record_hash` (its file content changed) and references that previously pointed to either old hash are *expected* to fail validation until the manifest rebuilds. Pros: identity always reflects current canonical state. Cons: every drift event cascades manifest rebuilds across the corpus; archival manifests cannot be re-validated after upstream drift; audit trail lives outside the hashes.

Position L is the spec author's implied position ("PROV-style event log"). Position H is the simpler engineering path but breaks the v0 invariant that `manifest_identity_object` is monotonic for a fixed set of upstream files. ADR 0020 must pick one explicitly.

## Cross-cutting open questions ADR 0020 must answer

These three are load-bearing — every shape in ADR 0021 (file layout, SHACL, harness wiring) bends on them. ADR 0020 must answer all three explicitly before ADR 0021 commits to specifics.

1. **Identifier policy for split successors and merge survivors.** `person_id` is `^[0-9]{6}$` everywhere it appears today. Three options:
   - **(a) Aozora-only.** Drift events are recorded but successors must already exist as Aozora-issued 6-digit IDs. A split that produces two new conceptual entities cannot land until Aozora issues IDs for them. This is the most conservative path and probably blocks most real splits.
   - **(b) ABC-local entity IDs.** Introduce a separate ID space (e.g. `^abc-[0-9a-f]{12}$`). Both the person-record schema's `person_id` and the contributor reference schema's `person_id` widen to accept either form. SHACL changes accordingly. This is invasive but preserves the ability to record splits.
   - **(c) Numeric-only ABC suffix.** Reserve a numeric ID range (e.g. `9XXXXX`) for ABC-local successors. No schema pattern change. Cheaper than (b) but couples ABC IDs to a numeric prefix that may collide with future Aozora allocations.

   ADR 0020 must pick (a), (b), or (c). The choice flows into both schema files and into ADR 0021's drift-event shape (which references successor IDs as `person_id` strings).

2. **Multi-person event indexing — how does reverse traceability actually resolve?** A 2024 manifest references `person_id` 000123, `person_record_hash` H1. In 2026, 000123 is split into 000789 and 000790 (or whatever the identifier policy allows). A consumer holding the 2024 manifest must be able to answer "who is the current canonical successor?" Three indexing options:
   - **(a) Global drift-log file** at a known path (e.g. `examples/v0/example-persons/drift-log.json`). All events live there, indexed by timestamp. Traversal: scan the global file for events whose participants include 000123. Pros: simple, single source of truth, easy to validate. Cons: a single file becomes a coordination point at corpus scale.
   - **(b) Per-person sidecar** (`examples/v0/example-persons/000123.drift.json` or folder-form `000123/drift.json`). Every event referencing 000123 must be written to 000123's sidecar; a multi-person event therefore appears in *every* participant's sidecar. Pros: locality (each participant carries its own history). Cons: write-amplification, consistency burden (a merge event must land in both predecessors' sidecars).
   - **(c) Stable event IDs + participant indexes.** Each event gets a `drift_event_id` (e.g. `sha256:` of its canonical bytes). Each participant's sidecar carries only the IDs of events it appears in; the events themselves live in a per-event file (`examples/v0/example-persons/_events/<event-id>.json`). Traversal is a join. Pros: no write amplification, clean append semantics. Cons: more files, more harness machinery.

   ADR 0020 must pick one. The choice flows into ADR 0021's file layout, which is currently sketched as per-person sidecar (option b) — this is wrong if option (a) or (c) is chosen.

3. **Drift-log identity — corpus artifact with its own hash, or audit sidecar?** If the drift log is a corpus-level artifact, its content hash flows into `manifest_identity_object` (rotates manifests when drift events land). If it is an audit sidecar, it is validated by SHACL but does not appear in any identity hash. Position L is internally consistent only if the latter — a global drift log that flows into manifest identity rotates manifest hashes whenever an event lands, which is the very cascade Position L is trying to avoid. ADR 0020 must spell this out.

Until these three are answered, Phase 2 ADR 0021 sketches **conditional file shapes only**, not committed ones. Phase 3 cannot start until both ADRs are accepted.

## Cross-references

- `docs/superpowers/specs/2026-04-28-separated-person-records-design.md` lines 15–17 — Flavor 2 scope and the PROV-style event log recommendation. **This spec is the Flavor 1 source of truth**: the separated-person-records milestone was implemented under it but there is **no standalone accepted ADR** for the Flavor 1 data model. ADR 0020 must therefore cite this spec (plus the implementation files: `schemas/person-record.schema.json`, `src/abc/tools/person_record.clj`, `schemas/manifest.shacl.ttl` `PersonRecordShape`, `examples/v0/example-persons/`) as the Flavor 1 contract, NOT a non-existent prior ADR.
- ADR 0001 — Manifest Identity (the 12-field `manifest_identity_object`; `metadata_record_hash` is the only metadata-related identity dimension). For the recommended audit-sidecar drift-log, ADR 0020 does not amend ADR 0001 (drift events are non-identity-bearing). If ADR 0020 ever chooses corpus-artifact drift logs that flow into `manifest_identity_object`, ADR 0020 MUST explicitly amend ADR 0001 (add `Amends: ADR 0001` in the front-matter and document the new identity dimension).
- ADR 0017 — Vocabulary Review (sets the rule that new `abc:` predicates must be reviewed against standard alternatives before being committed; ADR 0020's `abc:DriftEvent`, `abc:driftEventType`, `abc:driftEvidence` must pass that review).
- ADR 0018 — Predicate Rename Batch 1 (template for ADRs that rotate fixture content but not JSON contracts; ADR 0020 follows this pattern except where identifier-policy (b) deliberately rotates schema hashes).
- PROV-O recommendation (W3C 2013): `prov:Entity`, `prov:Activity`, `prov:Agent`, `prov:wasDerivedFrom`, `prov:wasGeneratedBy`, `prov:specializationOf`, `prov:alternateOf`, `prov:wasInvalidatedBy`.

---

## File Structure (Phase 1 + 2 — Discovery + ADRs)

- Create: `docs/superpowers/notes/2026-04-29-person-drift-discovery.md` — discovery notes: real-world drift cases observed in Aozora, the candidate event vocabularies, the question list ADR 0020 must answer.
- Create: `docs/adr/0020-person-identity-drift-data-model.md` — the data-model decision (Position L vs. H, event vocabulary, file shape).
- Create: `docs/adr/0021-person-identity-drift-harness.md` — the harness contract (where events live on disk, how SHACL validates them, how the validate-design-bundle gate checks them, whether they touch identity hashes).
- Modify: `docs/next-steps.md` — drop the "person identity drift Flavor 2" candidate; add a milestone entry per ADR.
- Phase 3 plan file: written *after* ADRs 0020 + 0021 are accepted, at `docs/superpowers/plans/2026-MM-DD-person-drift-implementation.md`. This plan does not pre-write Phase 3 because the concrete file shapes are ADR-output.

---

## Phase 1 — Discovery

### Task 1: Catalogue real-world drift cases from Aozora

The decision should be grounded in actual cases, not hypotheticals. Aozora's CSV history may carry traces of past splits/merges (look for `person_id` re-use, mismatched lifespans, "別名" notes).

**Files:**
- Create: `docs/superpowers/notes/2026-04-29-person-drift-discovery.md`

- [ ] **Step 1: Survey the current corpus CSV for drift hints**

The Aozora CSV is fetched by `abc.tools.aozora-csv` (see `src/abc/tools/aozora_csv.clj`). The ingester's CLI (per `src/abc/tools/aozora_ingest.clj:295`) requires `--zip ZIP` plus either single-work mode (`--work-id` + `--output`) or corpus mode (`--all` + `--output-dir`). There is no `--dry-run`. Run a corpus ingest into a throwaway directory:

```bash
mkdir -p /tmp/abc-drift-survey
nix run .#aozora-ingest -- \
  --zip <path-to-list_person_all_extended.zip> \
  --all \
  --output-dir /tmp/abc-drift-survey
```

(If a zip is not handy, `bin/update-deps.sh` and existing test fixtures may be enough — the goal is to surface drift cases in *some* representative cut of the corpus, not necessarily the full latest CSV.)

Then grep the produced person records for:
- duplicate `person_id`s with conflicting `family_name`/`given_name`/`date_of_birth`
- `external_links` entries that reference Wikipedia disambiguation pages
- author rows where the CSV's `person_id` field is empty or has been reassigned

Record findings in the discovery note as a table:

| Case type | `person_id` | Evidence | Source |
| --- | --- | --- | --- |

If the corpus shows zero drift hints, the discovery note must say so explicitly — Position L vs. H still needs to be chosen, but the cost-of-error calibration changes.

- [ ] **Step 2: Catalogue drift cases known from cultural-heritage practice**

Even if the current Aozora corpus shows no drift, the harness must be future-proof. From cultural-heritage cataloguing practice (RDA, VIAF, NDL Authority File), enumerate the canonical drift cases:

- **VIAF cluster split**: a single VIAF cluster turns out to encompass two distinct entities; VIAF mints a new cluster ID for one of them.
- **NDL authority merge**: NDL collapses two NDL author IDs into one.
- **Pen name re-attribution**: a work catalogued under one pen name is reassigned to another (e.g. mistakenly catalogued as 雅号 X when actually 本名 Y).

For each, write down: what event the harness must record, what evidence the editor would attach, what the downstream consequence is.

- [ ] **Step 3: Survey existing PROV-style precedents AND the existing in-repo `ActivityShape`**

Read the PROV-O recommendation (W3C 2013) §2.2.1 (Entity), §2.2.2 (Activity), §2.2.3 (Agent), §3.4 (Derivation), §3.5 (Invalidation), §3.6 (Specialization). Then read `schemas/manifest.shacl.ttl:59` — the existing `ActivityShape` already targets `prov:Activity` and requires `prov:used` and `prov:qualifiedAssociation`. Any Flavor 2 shape that also targets `prov:Activity` will compose with it.

Note the relations directly applicable to Flavor 2:

| PROV term | Subject | Object | Maps to in Flavor 2 |
| --- | --- | --- | --- |
| `prov:Entity` | (it IS an entity) | — | each `person_record` snapshot is one entity |
| `prov:Activity` | (it IS an activity) | — | the drift event itself (split / merge) |
| `prov:Agent` | (it IS an agent) | — | the editor who made the decision (foaf:Person or org IRI) |
| `prov:wasDerivedFrom` | `prov:Entity` | `prov:Entity` | post-event snapshot ← pre-event snapshot. **Note**: derivation is Entity→Entity, NOT Activity→Entity. The Activity is referenced via `prov:wasGeneratedBy` from the post-event Entity, and via `prov:used` from the Activity to the pre-event Entity. The earlier draft of this plan got this wrong — see "PROV graph shape" below. |
| `prov:wasGeneratedBy` | `prov:Entity` | `prov:Activity` | post-event Entity points to the drift event |
| `prov:used` | `prov:Activity` | `prov:Entity` | drift event points to pre-event Entity |
| `prov:wasInvalidatedBy` | `prov:Entity` | `prov:Activity` | pre-event Entity is invalidated by the drift Activity (Position L marks but does not delete the pre-event record) |
| `prov:wasAssociatedWith` | `prov:Activity` | `prov:Agent` | drift event points to the editor |
| `prov:qualifiedAssociation` | `prov:Activity` | `prov:Association` | required by existing `ActivityShape`; the Association blank node carries `prov:agent` + `prov:hadPlan`/`prov:hadRole` |
| `prov:specializationOf` | `prov:Entity` | `prov:Entity` | candidate for "this snapshot is a more specific version of that one" — relevant if the harness chooses to thread snapshots |
| `prov:alternateOf` | `prov:Entity` | `prov:Entity` | candidate for "two views of the same underlying thing" — relevant for mid-merge state |

**PROV graph shape (canonical for ADR 0020 Decision):**

```
Pre-snapshot E1 (person_record_hash H1)
    --prov:wasInvalidatedBy--> A (drift Activity)
A
    --prov:used--> E1
    --prov:wasAssociatedWith--> editor (Agent)
    --prov:qualifiedAssociation--> [a prov:Association ; prov:agent editor ; prov:hadRole abc:DriftEditor]
    a abc:DriftEvent ; abc:driftEventType "split"|"merge" ; dcterms:date "..." ; abc:driftEvidence (...)
Post-snapshot E2 (person_record_hash H2)
    --prov:wasGeneratedBy--> A
    --prov:wasDerivedFrom--> E1
```

For a split, there are two post-snapshots (E2a, E2b) both pointing back to E1. For a merge, one post-snapshot (E2) points back to two pre-snapshots (E1a, E1b). `prov:wasDerivedFrom` is always Entity→Entity; `prov:used` is the Activity→Entity counterpart.

**JSON vs RDF contract for the canonical graph.** This diagram describes the **RDF view** that `event->graph` produces. The **JSON `prov` block** is intentionally smaller: it carries only `prov:used`, `prov:wasGeneratedBy`, and `prov:qualifiedAssociation`. The remaining three predicates in the diagram (`prov:wasInvalidatedBy`, `prov:wasAssociatedWith`, `prov:wasDerivedFrom`) are **derived** by `event->graph` from those three plus the canonical graph shape itself. ADR 0021 Decision Step 3's "JSON `prov` block — minimal contract" clause commits to the derivation rules.

**SHACL targeting (no-inference wrapper).** The repo's SHACL wrapper at `src/abc/tools/shacl.clj:58` runs Jena's plain `ShaclValidator/get` with no RDFS inference, so the subclass axiom `abc:DriftEvent rdfs:subClassOf prov:Activity` does NOT cause the existing `ActivityShape` (`sh:targetClass prov:Activity`, `schemas/manifest.shacl.ttl:59`) to fire on a bare `abc:DriftEvent` node — it must be data-side typed. The drift shape (`PersonDriftEventShape`, `sh:targetClass abc:DriftEvent`) adds `abc:driftEventType` (`sh:in ("split" "merge")`), `dcterms:date` (one), `abc:driftEvidence` (`sh:minCount 1`), AND a typing-discipline backstop `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]`. Composition with `ActivityShape` happens via `event->graph` materialising `rdf:type prov:Activity` directly on every event node (data-side typing); a typing-coherence pre-check in `validate-drift-events!` catches the silent-bypass case that no SHACL shape can detect (a missing subclass type means the targeted subshape simply does not fire). The subclass axiom remains in the shapes graph for vocabulary documentation but is inert for current validation. ADR 0021 commits to this three-part mechanism; a drift event must satisfy `PersonDriftEventShape` AND `ActivityShape` (via materialised typing) AND its applicable subshape.

- [ ] **Step 4: Commit the discovery note**

```
git add docs/superpowers/notes/2026-04-29-person-drift-discovery.md
git commit -m "docs: discovery notes for person identity drift Flavor 2"
```

---

### Task 2: List the questions ADR 0020 must answer

The ADR is only worth writing when the questions it must answer are crisp. The discovery note's final section is a numbered question list. Each question must have at least two coherent answers; otherwise it isn't a decision.

**Files:**
- Modify: `docs/superpowers/notes/2026-04-29-person-drift-discovery.md`

- [ ] **Step 1: Append the question list to the discovery note**

Required questions (this list is the minimum; the ADR may add more if Task 1 surfaced cases that need them):

1. **Position L vs. H?** Do drift events rotate `person_record_hash` (H) or leave it monotonic (L)?
2. **Event log shape?** One global log file, or per-person log files alongside `person-record.json`?
3. **Vocabulary?** Pure PROV-O, or a thin `abc:` profile layered on top?
4. **Granularity?** Are split/merge two separate event types or one parameterised type? (Earlier drafts considered a third `rename` type, but the Context section scopes Flavor 2 to splits and merges only — pure pen-name renames without a split/merge are out of scope because they do not change *which person* an existing `person_id` refers to. ADR 0020 must drop `rename` or define it precisely as one of: name-only re-spelling that does not pass through Flavor 1, OR a class of merge with one predecessor and one successor.)
5. **Where does the editor + evidence go?** Inline as `prov:wasAssociatedWith` + `prov:hadPlan`, or as a sidecar with its own content hash?
6. **Validation surface?** A new SHACL `PersonDriftEventShape` runs at `validate-design-bundle` time; does it gate the example bundle, the corpus run, or both?
7. **Manifest impact?** Does a drift event for a referenced person require a manifest rebuild for the affected works (Position H mandates this), or is it transparent to the existing manifest (Position L)?
8. **Reverse-traceability?** How does a consumer holding a 2024 manifest's `metadata_record_hash` discover that the contributor's `person_id` has been split in 2026? Index, query, or required client traversal?

For each, write down the candidate answers and the trade-off. Do **not** decide here — that is the ADR's job.

- [ ] **Step 2: Commit**

```
git add docs/superpowers/notes/2026-04-29-person-drift-discovery.md
git commit -m "docs: enumerate ADR 0020 question list for person drift"
```

---

## Phase 2 — Decision

### Task 3: Draft ADR 0020 — Person identity drift data model

**Files:**
- Create: `docs/adr/0020-person-identity-drift-data-model.md`

- [ ] **Step 1: Write the ADR shell**

Follow the ADR 0017/0018 template: `Status: Draft`, `Date: 2026-04-29`, `Context`, `Decision`, `Hard Rule`, `Acceptance Criteria`, `Consequences`, `References`.

- [ ] **Step 2: Fill in `Context`**

Reproduce the Position L vs. H trade-off from this plan's Context section. Add the question-list summary from Task 2 (one paragraph per question, with the candidate answers and the trade-off — do not yet decide).

Then reproduce verbatim the three **cross-cutting open questions** from this plan's "Cross-cutting open questions ADR 0020 must answer" section:

1. Identifier policy for split successors and merge survivors (options a/b/c).
2. Multi-person event indexing — how reverse traceability resolves (options a/b/c).
3. Drift-log identity — corpus artifact (flows into `manifest_identity_object`) or audit sidecar (does not).

These three are load-bearing: every shape in ADR 0021 (file layout, SHACL targeting, harness wiring) bends on them. ADR 0020 must answer all three explicitly in its Decision section.

- [ ] **Step 3: Fill in `Decision` (recommended positions only — every load-bearing answer must appear here)**

The Decision section must be explicit about every position taken on **all** the questions: the eight from Task 2 *plus* the three cross-cutting open questions. Recommended (for the human reviewer):

- **Position L (lineage-only).** Drift events do not rotate `person_record_hash` or `metadata_record_hash`. The v0 invariant that `manifest_identity_object` is monotonic for a fixed set of upstream files is preserved.
- **Identifier policy = (b) ABC-local entity IDs.** Introduce a separate ID space for split successors and merge survivors. Rationale: option (a) blocks most real splits until Aozora issues new IDs; option (c) couples ABC IDs to a numeric prefix that may collide with future Aozora allocations. (b) is invasive but explicit. The ADR Decision must commit to **all four** of the following at ADR level (not deferred to Phase 3) because each is data-model policy:
  1. **JSON Schema regex** (e.g. `^abc-[0-9a-f]{12}$`) for `person_id` in both `schemas/person-record.schema.json:21` and `schemas/metadata-record.schema.json:104`. The full pattern accepted by both schemas becomes `^([0-9]{6}|abc-[0-9a-f]{12})$`. The 12-hex suffix is deliberate: 8 hex digits reaches birthday-collision risk around 65k IDs, while 12 hex digits makes accidental ABC-local collisions implausible at corpus scale with no ergonomic cost.
  2. **ABC-local IRI base** that `abc.tools.person-record/person-iri` (`src/abc/tools/person_record.clj:75`) mints for ABC-local IDs. Recommended: `https://w3id.org/abc/persons/<id>` (matches the existing `https://w3id.org/abc/` namespace). Aozora-numeric IDs continue to use `http://www.aozora.gr.jp/index_pages/person<id>.html`. The branching rule is committed in the ADR so RDF view changes are not a Phase 3 invention.
  3. **`dcterms:identifier` datatype** for ABC-local IDs. Recommended: `xsd:string` (the value `abc-deadbeefcafe` is not a valid `xsd:int` literal; introducing a typed `abc:PersonId` datatype is also possible but adds a vocabulary registration step). Aozora-numeric IDs continue with `xsd:int` per `person_record.clj:138`. The ADR commits to the chosen datatype because it is a public data-model contract.
  4. **`PersonRecordShape` SHACL widening** at `schemas/manifest.shacl.ttl:170`. The `dcterms:identifier` constraint preserves `sh:minCount 1` and `sh:maxCount 1`, and widens only the value constraint from `sh:datatype xsd:int` to the literal canonical shape `sh:or ([sh:datatype xsd:int ; sh:pattern "^[0-9]{6}$"] [sh:datatype xsd:string ; sh:pattern "^abc-[0-9a-f]{12}$"])` (or to the new `abc:PersonId` datatype if introduced), with patterns matching the JSON Schema regex. The ADR commits to the exact SHACL shape so consumers know what RDF will validate and duplicate identifiers remain invalid.
- **Multi-person event indexing = (c) stable event IDs + participant indexes.** Each event has a `drift_event_id` derived deterministically from the event body (see "drift_event_id derivation" below); events live in a per-event location; participants reference events by ID. Rationale: option (a) global file becomes a coordination bottleneck at corpus scale; option (b) per-person sidecar requires writing the same multi-person event into every participant's sidecar (write-amplification + consistency burden).
- **`drift_event_id` derivation (must be precise to avoid self-reference).** The ID hashes the event body **with `drift_event_id` removed and no other fields removed**, then is materialised back into the file. Algorithm: (1) start from the full wrapper JSON object as it will appear on disk; (2) remove **only** the `drift_event_id` field (every other field — `schema_id`, `schema_hash`, `drift_event_type`, `date`, `participants`, `evidence`, `prov` — is present in the hashed input); (3) JCS-canonicalise (RFC 8785); (4) sha256 the canonical bytes; (5) prefix with `sha256:`. The materialised file carries the resulting `drift_event_id` field and **no other marker** — the omission rule is a documented protocol invariant, not an in-band flag. (An earlier draft proposed an `id_omitted_from_hash: true` companion field; it is rejected because the omission scope is fixed by the protocol and a flag would only widen the surface that validation has to police.) Validation re-derives the ID by performing the same single-field removal on the file's bytes and asserts equality. Rationale: a hash that includes its own value is undefined; either the field is omitted from the hashed input, or a separate stable UUID is used. ADR 0020 picks the omit-from-hash variant because it is content-derived (no UUID-allocation registry needed); fixing the omitted set to exactly `{drift_event_id}` keeps the derivation rule single-line and deterministic.
- **Participant ordering for content-derived IDs.** Because JCS preserves array order, ADR 0021 must pin canonical ordering: `participants[]` sorted lexicographically by `snapshot_id`, and `prov.used` / `prov.was_generated_by` each sorted lexicographically. `validate-drift-events!` rejects non-canonical ordering before re-deriving `drift_event_id`.
- **Drift-log identity = audit sidecar.** Drift events do not flow into `manifest_identity_object`. Rationale: Position L is internally consistent only if drift events do not rotate manifest identity; treating the drift log as a corpus artifact would defeat the monotonicity invariant.
- **Vocabulary = thin abc profile over PROV-O.** `abc:DriftEvent rdfs:subClassOf prov:Activity`, `abc:DriftSplitEvent`, `abc:DriftMergeEvent`, `abc:driftEventType`, `abc:driftEvidence`, and the role term `abc:DriftEditor`. Drift events satisfy both the existing `ActivityShape` at `schemas/manifest.shacl.ttl:59` (which requires `prov:used` + `prov:qualifiedAssociation`) and the new drift-specific shape. `abc:DriftEditor` is reviewed at ADR level because it is an ABC-local role used in `prov:hadRole`; if the ADR drops the role, the canonical graph must use only `prov:agent`.
- **PROV graph shape (canonical).** Reproduce the diagram from Phase 1 Task 1 Step 3 verbatim. `prov:wasDerivedFrom` is Entity→Entity (post-snapshot ← pre-snapshot); `prov:used` is Activity→Entity (drift Activity → pre-snapshot); `prov:wasGeneratedBy` is Entity→Activity (post-snapshot → drift Activity).
- **Two event types = split, merge.** Mapped from the in-scope Context cases (split = one predecessor, two-or-more successors; merge = two-or-more predecessors, one successor). `rename` is **dropped** in the recommended decision because Flavor 2's Context (lines 17–21) does not include pure renames — name-only changes without a split/merge are bibliographic edits to a single `person_record.json` and rotate `person_record_hash` via Flavor 1. If ADR 0020 wants to keep `rename`, it must define it precisely (e.g. "1-predecessor / 1-successor with a name change", distinct from a Flavor 1 edit) and update every SHACL `sh:in` accordingly. Otherwise the closed enum is `("split" "merge")` everywhere.
- **Re-attribution is out of scope.** A `contributors[]` edit on a metadata record rotates `metadata_record_hash` (Flavor 1 path). It must not be recorded as a drift event, otherwise Position L's monotonicity breaks the moment a re-attribution lands. ADR 0020 must say this explicitly.

If the user picks otherwise during ADR review, the rest of this plan and the Phase 3 follow-up plan must be re-shaped accordingly.

- [ ] **Step 4: Fill in `Hard Rule`**

For the recommended Position L + audit-sidecar drift log:
- `metadata_record_hash` is **not** rotated by a drift event.
- `person_record_hash` is **not** rotated by a drift event (the hash is over file content; drift events live in separate files).
- `manifest_identity_object` is **not** affected by a drift event (drift log is an audit sidecar, not a corpus identity input).
- A new schema, `schemas/person-drift-event.schema.json`, is added; its `schema_hash` is referenced from each drift event for self-description but does not flow into `manifest_identity_object`.
- Only the identifier-policy decision (option b widens `person_id` regex) rotates `person_record_schema_hash` and `metadata_record_schema_hash`. ADR 0020 must call this out: the schema-hash cascade IS triggered by ADR 0020 if option (b) is chosen, and the cascade work is bundled with the Phase 3 implementation.

- [ ] **Step 5: Fill in `Acceptance Criteria`**

The ADR is accepted when:
- The eight Task-2 questions AND the three cross-cutting open questions have explicit Decision-section answers.
- The chosen vocabulary maps onto identifiable PROV-O terms (no invented IRIs without an `abc:` justification).
- The PROV graph shape diagram appears in the Decision (so ADR 0021 can target the right relations).
- The manifest-identity invariant holds: a drift event added to the example bundle does **not** change `manifest.json`'s bytes (assuming Position L + audit-sidecar are chosen).
- If the identifier-policy decision rotates schema hashes, the Hard Rule lists exactly which hashes rotate and which do not.

- [ ] **Step 6: Commit (Status: Draft)**

```
git add docs/adr/0020-person-identity-drift-data-model.md
git commit -m "docs(adr): draft ADR 0020 — person identity drift data model"
```

The ADR moves to `Accepted` only after user review.

---

### Task 4: Draft ADR 0021 — Drift event harness contract

**Files:**
- Create: `docs/adr/0021-person-identity-drift-harness.md`

- [ ] **Step 1: Write the ADR shell**

Same template as ADR 0020. This ADR depends on ADR 0020 — its Context cites 0020's data model as input and its Decision is purely about *how the harness validates and surfaces* the events. ADR 0021 cannot be drafted before ADR 0020's three cross-cutting open questions are answered, because every concern below bends on those answers.

- [ ] **Step 2: Fill in `Context`**

ADR 0021's concerns are all conditional on ADR 0020's decisions. Spell out the conditions explicitly:

1. **File layout** — depends on ADR 0020's identifier policy + multi-person indexing decisions:
   - If indexing = (a) global drift-log file: one location, e.g. `examples/v0/example-persons/drift-log.json`. No per-person sidecars.
   - If indexing = (b) per-person sidecar: `examples/v0/example-persons/<id>.drift.json` (flat sidecar) OR `examples/v0/example-persons/<id>/drift.json` (folder). The flat sidecar is cheaper; folder-per-person requires migrating the existing `examples/v0/example-persons/000879.json` *and* every callsite that reads it.
   - If indexing = (c) stable event IDs + participant indexes: per-event files at `examples/v0/example-persons/_events/<event-id>.json`, plus a per-person index at `examples/v0/example-persons/_indexes/<id>.json` (flat sidecar under a sibling subdirectory). The index sidecar **must NOT** sit at `examples/v0/example-persons/<id>.events.json` because `validate_design_bundle.clj:365` discovers every immediate `*.json` under `examples/v0/example-persons` as a person record — a flat sidecar would be parsed as a malformed person record and fail discovery before drift validation runs. Both `_events/` and `_indexes/` are subdirectories and so are skipped by the immediate-level filter.
2. **File format — JSON or JSONL?** A JSON Schema-validated artifact in this repo carries `schema_id` + `schema_hash` at the top level (e.g. `schemas/person-record.schema.json` validates an object with those two fields). JSONL is *line-oriented* with no file-level header — there is no place to put `schema_id` / `schema_hash`. Three options:
   - **(α) Wrapper JSON** — a top-level object `{schema_id, schema_hash, events: [...]}`. Drops append-only ergonomics; appending requires a parse-rewrite cycle.
   - **(β) JSONL with per-line schema fields** — every line carries its own `schema_id` + `schema_hash`. Preserves append-only writes; redundant on disk but cheap and self-describing.
   - **(γ) JSONL + manifest sidecar** — events in `<...>.drift.jsonl`, schema metadata in a sibling `<...>.drift.meta.json`. Two files per drift log.
3. **SHACL shape — basic constraints.** The drift-specific shape adds: `abc:driftEventType` (`sh:in` with the closed enum from ADR 0020 — `("split" "merge")` for the recommended decision; `("split" "merge" "rename")` only if ADR 0020 keeps `rename` and defines it precisely), `dcterms:date` (`sh:minCount 1 sh:maxCount 1`), `abc:driftEvidence` (`sh:minCount 1`, each item `sh:nodeKind sh:IRI`). The shape targets `abc:DriftEvent` via `sh:targetClass abc:DriftEvent`. Composition with the existing `ActivityShape` at `schemas/manifest.shacl.ttl:59` (`sh:targetClass prov:Activity`) **does NOT happen automatically via subclass inference** — see Context point 5 — because the SHACL wrapper at `src/abc/tools/shacl.clj:58` runs Jena with no RDFS inference. Composition relies on `event->graph` materialising `rdf:type prov:Activity` directly on every event node (data-side typing), with a typing-coherence pre-check + a `sh:hasValue prov:Activity` SHACL backstop guarding the case where the materialisation is missing.
4. **SHACL shape — split/merge cardinality.** The closed `abc:driftEventType` enum is necessary but not sufficient. ADR 0020 defines split as 1-predecessor + ≥2-successors and merge as ≥2-predecessors + 1-successor; these arities must be enforced by SHACL or invalid drift events (e.g. a "split" with only one successor) will pass validation. Two coherent encodings:
   - **(i) `sh:qualifiedValueShape` per predicate.** For events with `abc:driftEventType "split"`, assert `sh:property [ sh:path prov:used ; sh:qualifiedValueShape … ; sh:qualifiedMinCount 1 ; sh:qualifiedMaxCount 1 ]` AND `sh:property [ sh:path [sh:inversePath prov:wasGeneratedBy] ; sh:qualifiedMinCount 2 ]` (post-snapshots derived from this Activity). Mirror for merge.
   - **(ii) Two specialised subshapes.** `abc:DriftSplitEvent` and `abc:DriftMergeEvent`, each `rdfs:subClassOf abc:DriftEvent`, with their own `sh:targetClass` and arities. The class-discrimination relies on `event->graph` materialising the subtype `rdf:type` from the JSON `drift_event_type` field; the SHACL subshape additionally asserts `sh:property [ sh:path abc:driftEventType ; sh:hasValue "split" ]` (resp. "merge") on the RDF predicate to guard against type-vs-value inconsistency. Cleaner shape composition; one extra class.

   ADR 0021 must commit to (i) or (ii) and write the SHACL out fully (no "TODO: enforce arities"). The chosen encoding is the source of truth — Phase 3 task 3 implements it verbatim. Negative fixtures must include at least: a "split" event with only one successor, a "split" event with no successors, a "merge" event with only one predecessor, and a "merge" event with no predecessor — each must FAIL the cardinality shape.
5. **`rdf:type` materialisation and the SHACL wrapper's no-inference contract.** The existing `ActivityShape` at `schemas/manifest.shacl.ttl:59` targets `prov:Activity`. For an `abc:DriftEvent` instance to satisfy `ActivityShape`, the SHACL engine must see the instance as a `prov:Activity` AT validation time. The repo's wrapper at `src/abc/tools/shacl.clj:58` calls `(.validate validator shapes-graph data-graph)` — Jena's plain `ShaclValidator/get` with no inference layer. Empirically (verified against minimal cases): a subclass axiom in the **shapes graph** does NOT cause `ActivityShape` to fire on a bare `abc:DriftEvent` instance in the **data graph**; the same axiom in the data graph does. This rules out the "axiom in the SHACL file is sufficient" framing. Three options remain, framed against the wrapper's no-inference contract:
   - **(α) Data-side `rdf:type` materialisation in `event->graph`, plus a typing-coherence pre-check in `validate-drift-events!`.** The Clojure emitter derives `rdf:type` triples from the event JSON's `drift_event_type` field and emits them onto the activity node. Every drift event node carries `rdf:type abc:DriftEvent`, `rdf:type prov:Activity`, AND `rdf:type abc:DriftSplitEvent` (or `abc:DriftMergeEvent`) explicitly. **Materialisation alone is not sufficient**: under the no-inference SHACL wrapper, missing or inconsistent `rdf:type` triples cause the targeted shape to silently not fire (no violation emitted, malformed event passes). To close this gap, `validate-drift-events!` runs a typing-coherence pre-check before SHACL: derive the expected `rdf:type` set from the JSON `drift_event_type`, compare against the RDF graph, fail on any mismatch. The subclass axioms are still committed to `schemas/manifest.shacl.ttl` for vocabulary documentation and forward-compatibility with inference-aware validators, but validation does not depend on RDFS inference. SHACL backstop: `PersonDriftEventShape` directly asserts `sh:hasValue prov:Activity` on `rdf:type` so the no-inference failure case (missing `prov:Activity` type → `ActivityShape` doesn't fire) is caught by the base shape directly. Pros: works under the existing wrapper; self-describing TTL views; matches how the repo's other shapes target nodes. Cons: emitter must remember to materialise (covered by a unit test on `event->graph`); validator must implement the pre-check (covered by a unit test on `validate-drift-events!`).
   - **(β) Pre-validation axiom-materialisation in `validate-drift-events!`.** Before calling `shacl/validate!`, walk the data graph and add `(<node> rdf:type prov:Activity)` for every `abc:DriftEvent` (and the subtype types). Pros: emitter-agnostic. Cons: validator must encode the subclass closure separately; SHACL file's axioms are still inert; brittle when new subtypes land.
   - **(γ) Switch the wrapper to an inference model.** Change `shacl.clj:50–65` to wrap the data graph in `ModelFactory.createRDFSModel(...)` (or union the shapes axioms into the data graph) before validation. Pros: SHACL file's axioms become live. Cons: changes the validation semantics for **every** existing shape (`ArtifactShape`, `ActivityShape`, `FailureShape`, `SidecarShape`, `MetadataRecordWorkShape`, `PersonRecordShape`) — those shapes were authored under the no-inference contract and may regress, e.g. inferred `rdfs:Resource` types triggering unintended targeting. Out-of-scope risk for a drift-only ADR.

   ADR 0021 must commit to (α), (β), or (γ) and document precisely how `validate-drift-events!` + `validate-drift-fixtures!` ensure the typing reaches the validator AND how missing/inconsistent typing is detected (since under (α) and (γ) without a backstop, a missing type silently bypasses targeting). Under (α), the detection mechanism is the typing-coherence pre-check in `validate-drift-events!` PLUS the `sh:hasValue prov:Activity` assertion on `PersonDriftEventShape` (catches the missing-`prov:Activity` case in pure SHACL too, so `validate-drift-fixtures!` running on hand-crafted TTL still catches it). Negative fixtures must include: a drift event with all required types but missing `prov:used` (must FAIL via `ActivityShape`); a hand-crafted TTL with `rdf:type abc:DriftEvent` but missing `rdf:type prov:Activity` (must FAIL via the base-shape `sh:hasValue` backstop under (α), unreachable under (β/γ)); a hand-crafted TTL with `rdf:type abc:DriftEvent` + `rdf:type prov:Activity` but missing the subclass type for the JSON's `drift_event_type` (must FAIL via the typing-coherence pre-check in `validate-drift-events!`; in `validate-drift-fixtures!` running SHACL alone, this case is the affirmative test that the subshape silently bypasses targeting — and the fixture documents this is detected at the validator level, not the shape level).
6. **PROV graph shape — derivation correction.** The ADR must NOT use `prov:wasDerivedFrom` from Activity to Entity (the earlier draft of this plan got this wrong). The Activity points to the pre-snapshot via `prov:used` and is pointed to by the post-snapshot via `prov:wasGeneratedBy`. `prov:wasDerivedFrom` is Entity→Entity (post-snapshot ← pre-snapshot). Reproduce the diagram from Phase 1 Task 1 Step 3 verbatim.
7. **Index artifact contract.** ADR 0020's recommended indexing = (c) introduces a per-person index sidecar. The index file is itself a JSON-Schema-validated artifact and needs:
   - **`schemas/person-drift-index.schema.json`** — JSON Schema 2020-12 for the index. Recommended top-level shape: `{schema_id, schema_hash, person_id, drift_event_ids: [drift_event_id, …]}`. Each `drift_event_id` is the same content-derived ID materialised on the per-event file.
   - **Optional `PersonDriftIndexShape`** — only if the index is materialised into the SHACL graph. Recommended: do NOT materialise the index into the graph; the SHACL composition already validates each event individually, and the index's role is purely traversal/discovery (locate events for a given person without scanning every event file). JSON-Schema-only validation is sufficient.
   - **Referential integrity rules** — every `drift_event_ids[i]` in an index must resolve to an existing event file; every event must appear in the index of every person it references (predecessors + successors); no event file may exist that is not referenced by any index. `validate-drift-events!` enforces all three rules.
   ADR 0021 commits to the schema file path, the optional-shape decision, and the integrity rules — they are part of the ADR-level contract, not Phase 3 implementation detail. Without ADR-level commitment, Phase 3 has no contract to validate index files against.
8. **Validate-design-bundle integration.** Drift validation runs unconditionally: if no drift artifact is present for the example bundle, the harness records `drift: not-present` and continues; if present, every event must validate against the JSON Schema and SHACL shape, AND every index must validate against `person-drift-index.schema.json` AND the three referential-integrity rules from point 7 must hold. This mirrors how IIIF applicability is unconditionally validated even when `status: not-applicable`.
9. **Negative fixture runner.** The existing `validate-tei-schematron!` invalid-fixtures runner at `src/abc/tools/validate_design_bundle.clj:274` is TEI/Schematron-specific and **cannot be reused** for JSON-schema + SHACL drift validation. ADR 0021 must commit to a sibling JSON/SHACL drift-fixture runner — a new function in the same namespace, with the same shape (load fixture, expect failure, assert specific error keys).

- [ ] **Step 3: Fill in `Decision` (conditional on ADR 0020 decisions)**

The Decision is structured as a sequence of mappings: each ADR 0020 answer determines the shape ADR 0021 commits to. Recommended (for the human reviewer; assumes ADR 0020's recommended Decision):

- **File layout.** If ADR 0020 chose multi-person indexing = (c) stable event IDs + participant indexes: per-event file at `examples/v0/example-persons/_events/<drift_event_id>.json`, per-person index at `examples/v0/example-persons/_indexes/<person_id>.json` (flat sidecar — no folder migration). Rationale: keeps Flavor 1's existing `examples/v0/example-persons/<person_id>.json` layout untouched, no callsite migration. If ADR 0020 chose (a) or (b) instead, this section is replaced; the rest of the ADR holds.
- **File format.** Per-event files use **wrapper JSON** (option α): a top-level object `{schema_id, schema_hash, drift_event_id, drift_event_type, date, participants: [{snapshot_id, person_id, person_record_hash}, ...], evidence: [<iri>, ...], prov: {...}}`. The `participants[]` field is the canonical list of involved person snapshots (drives the referential-integrity rules per the PROV-entity → person_id mapping clause below); each item carries a local `snapshot_id` plus the historical `person_record_hash` observed by the event so old drift events never rebind to a later person-record edit. The embedded `prov` block carries the `used` / `was_generated_by` / `qualified_association` graph that `event->graph` translates into RDF `prov:used` / `prov:wasGeneratedBy` / `prov:qualifiedAssociation`. Single-event-per-file means no append-only requirement, so wrapper JSON is the simplest path and matches Flavor 1's `person-record.schema.json` shape. Per-person index files (`_indexes/<id>.json`) are also wrapper JSON: `{schema_id, schema_hash, person_id, drift_event_ids: [...]}`. **JSONL is rejected** — every event lives in its own file, append-only is not a requirement.
  Canonical ordering is part of the file format: `participants[]` is sorted lexicographically by `snapshot_id`, and `prov.used` / `prov.was_generated_by` are each sorted lexicographically. `snapshot_id` is unique within the event and must start with `pre-` for predecessor snapshots or `post-` for successor snapshots.
- **JSON field naming.** The wrapper JSON uses `snake_case` keys consistent with the rest of the v0 contract (`metadata-record.schema.json`, `person-record.schema.json`). The drift event JSON keys and their RDF-predicate mappings are:
  - JSON `drift_event_id` → no RDF predicate (file-level identity, not part of the graph).
  - JSON `drift_event_type` → RDF predicate `abc:driftEventType` (literal string `"split"` or `"merge"`).
  - JSON `date` → RDF predicate `dcterms:date` (EDTF literal).
  - JSON `participants[]` → no direct RDF predicate (canonical participant snapshot list; each item has `snapshot_id`, `person_id`, and `person_record_hash`; the RDF view derives participants from PROV graph edges).
  - JSON `evidence[]` → RDF predicate `abc:driftEvidence` (each item an IRI).
  - JSON `prov` block (predicate-keyed object carrying JSON keys `used`, `was_generated_by`, `qualified_association` only) → translated structurally by `event->graph` to RDF predicates `prov:used`, `prov:wasGeneratedBy`, `prov:qualifiedAssociation`.

  Subsequent decision clauses MUST refer to the JSON field by its `snake_case` key (e.g. "the JSON `drift_event_type` field") and to the RDF predicate by its `prefix:local` form (e.g. "the `abc:driftEventType` predicate"). Mixing the two — e.g. "the JSON `abc:driftEventType` field" — is incorrect and forbidden in the ADR text.

- **JSON `prov` block — minimal contract; remaining canonical predicates are derived.** The canonical PROV graph in Context point 6 names six predicates: `prov:used`, `prov:wasGeneratedBy`, `prov:qualifiedAssociation`, `prov:wasInvalidatedBy`, `prov:wasAssociatedWith`, `prov:wasDerivedFrom`. Only the first three are explicit JSON; the remaining three are **derived in `event->graph`** from the explicit edges plus the canonical graph shape committed in Context point 6 / ADR 0020 PROV diagram. The derivation rules are part of the ADR-level contract:
  The JSON shape of `prov` is committed here because `event->graph` translates the structure, not just the predicate names:

  ```json
  {
    "used": ["pre-000123"],
    "was_generated_by": ["post-abc-deadbeefcafe"],
    "qualified_association": {
      "agent": "https://w3id.org/abc/agents/...",
      "had_role": "abc:DriftEditor"
    }
  }
  ```

  The values in `used` and `was_generated_by` are local snapshot references that resolve to `participants[].snapshot_id`; those participant entries carry `person_id` and `person_record_hash`.
  `qualified_association.had_role` is a CURIE string resolved by `event->graph` through the project prefix map; v0 is a closed enum with the single allowed value `abc:DriftEditor`, resolving to `https://w3id.org/abc/DriftEditor`. Unknown prefixes or any other role value are validation failures, and widening the role set requires a later ADR. `qualified_association.agent` is a full IRI string and must be syntactically valid.

  - `prov:wasInvalidatedBy` (Entity→Activity): for every pre-snapshot Entity E that the Activity `prov:used`, emit `E prov:wasInvalidatedBy <activity>`. Justification: the canonical graph commits Position L's marking-without-deletion semantics — every predecessor snapshot is invalidated by the drift Activity. Encoding `prov:used` (Activity→Entity) is sufficient input; the inverse-flavoured `prov:wasInvalidatedBy` is purely derivative.
  - `prov:wasAssociatedWith` (Activity→Agent): the JSON `prov` block carries `prov:qualifiedAssociation` whose Association blank node references `prov:agent <agent-iri>`. Emit `<activity> prov:wasAssociatedWith <agent-iri>` for that same agent. Justification: PROV-O explicitly defines `prov:qualifiedAssociation` as the qualified form of `prov:wasAssociatedWith`; the unqualified edge is a standard derivation from the qualified one.
  - `prov:wasDerivedFrom` (Entity→Entity, post→pre): for every post-snapshot Entity E_post (i.e. every node where `<E_post> prov:wasGeneratedBy <activity>`), emit `<E_post> prov:wasDerivedFrom <E_pre>` for every pre-snapshot Entity E_pre that the Activity `prov:used`. Justification: PROV-O's standard derivation pattern via an Activity — when E_post is generated by A and A used E_pre, then E_post is derived from E_pre. For splits this emits two `prov:wasDerivedFrom` edges (E2a→E1, E2b→E1); for merges it emits two (E2→E1a, E2→E1b).

  Rationale for derivation over JSON-explicit: a JSON contract that carried all six predicates would force the editor to maintain consistency between the qualified and unqualified forms by hand (an invariant the schema cannot enforce). Deriving the three from the canonical graph shape makes consistency a property of the emitter, not the editor. Phase 3 unit tests for `event->graph` assert all three derivations on representative split and merge events; an `event->graph` bug that emits the wrong derivation pattern is caught by the unit test, not by SHACL on the production data.

  Negative-fixture scope updates per this clause: SHACL fixtures that target `prov:wasDerivedFrom` violations (e.g. "post-Entity that is not derived from any pre-Entity") are hand-crafted TTL inputs that violate the derivation invariant directly; they are NOT JSON inputs (since the JSON cannot express a missing derivation — the JSON has no `wasDerivedFrom` field). Validator-only fixtures that assert the JSON+RDF derivation contract live alongside the typing-coherence pre-check (a graph-coherence pre-check in `validate-drift-events!` may also assert that for every pair (E_post, E_pre), a `prov:wasDerivedFrom` edge exists in the materialised graph; if so, the negative fixture for "post-Entity missing derivation" lives in the validator-only category instead of SHACL-only).

- **`rdf:type` materialisation (Context point 5).** Commit to **option (α) — data-side `rdf:type` materialisation in `event->graph`**. The Clojure emitter at `src/abc/tools/person_drift/event_to_graph` (new) derives multiple `rdf:type` triples on the activity node from the event JSON's `drift_event_type` field:
  - Always emit `<event-iri> rdf:type abc:DriftEvent` AND `<event-iri> rdf:type prov:Activity`.
  - For `drift_event_type: "split"`, additionally emit `<event-iri> rdf:type abc:DriftSplitEvent`.
  - For `drift_event_type: "merge"`, additionally emit `<event-iri> rdf:type abc:DriftMergeEvent`.

  Rationale: the SHACL wrapper at `src/abc/tools/shacl.clj:58` runs Jena's plain validator with no RDFS inference (verified empirically), so axioms in the shapes graph do NOT make `ActivityShape` fire on bare `abc:DriftEvent` nodes. Data-side materialisation is the only option that works under the existing wrapper and matches how every other repo shape (`ArtifactShape`, `MetadataRecordWorkShape`, `PersonRecordShape`) targets explicitly-typed nodes. Options (β) (pre-validation materialisation) and (γ) (wrapper inference upgrade) are rejected: (β) duplicates the typing logic in the validator; (γ) would change the validation semantics for every existing shape with regression risk out of scope for this ADR.

  The subclass axioms (`abc:DriftEvent rdfs:subClassOf prov:Activity`, `abc:DriftSplitEvent rdfs:subClassOf abc:DriftEvent`, `abc:DriftMergeEvent rdfs:subClassOf abc:DriftEvent`) are still committed to `schemas/manifest.shacl.ttl` alongside the shapes — they document the vocabulary and provide forward-compatibility for any future inference-aware validator. They do NOT participate in current validation.

- **Typing-coherence pre-SHACL check.** Data-side `rdf:type` materialisation creates a new failure mode that pure SHACL cannot catch on its own: if the materialised types are missing or inconsistent with the JSON, the targeted shape simply does NOT fire and SHACL emits no violation — the malformed event silently passes. To close this gap, `validate-drift-events!` runs a **typing-coherence pre-check** before invoking `shacl/validate!` on each event:
  - Derive the expected `rdf:type` set from the event JSON's `drift_event_type` field — `{abc:DriftEvent, prov:Activity, abc:DriftSplitEvent}` for `"split"`, the merge variant for `"merge"`.
  - Walk the RDF data graph for the activity node and collect its actual `rdf:type` set.
  - Assert the two sets are equal. A missing type, an extra unexpected type, or a wrong subclass type is a typing-coherence failure with a distinct `:failures` key (e.g. `{:rule :typing-coherence :event-id … :missing #{...} :unexpected #{...}}`), surfaced before SHACL runs so the SHACL stage sees only correctly-typed nodes.

  Belt-and-suspenders backstop in SHACL: `PersonDriftEventShape` additionally asserts `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]` so even if the pre-check is bypassed (e.g. by `validate-drift-fixtures!` running SHACL directly on a hand-crafted TTL fixture), a missing `prov:Activity` typing fails directly via the base shape rather than being silently ignored by `ActivityShape`'s targeting.

- **SHACL shape — base + cardinality.** A new `PersonDriftEventShape` in `schemas/manifest.shacl.ttl` with `sh:targetClass abc:DriftEvent` carries the type-agnostic constraints from Context point 3: `abc:driftEventType` (`sh:in ("split" "merge")`), `dcterms:date` (`sh:minCount 1 sh:maxCount 1`), `abc:driftEvidence` (`sh:minCount 1`, each item `sh:nodeKind sh:IRI`), AND the typing-discipline constraint `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]` (per the typing-coherence backstop above). For split/merge cardinality (Context point 4), commit to **option (ii) — specialised subshapes**: `PersonDriftSplitEventShape` with `sh:targetClass abc:DriftSplitEvent` (requires exactly one `prov:used` + ≥2 inverse-`prov:wasGeneratedBy`), and `PersonDriftMergeEventShape` with `sh:targetClass abc:DriftMergeEvent` (requires ≥2 `prov:used` + exactly one inverse-`prov:wasGeneratedBy`). Rationale for (ii) over (i): cleaner shape composition (each subshape's `sh:targetClass` discriminates without an inline `sh:hasValue` predicate), and adding a third event type later is purely additive. Each subshape additionally asserts `sh:property [ sh:path abc:driftEventType ; sh:hasValue "split" ]` (resp. "merge") so a node typed `abc:DriftSplitEvent` whose RDF `abc:driftEventType` value is "merge" fails — guarding against emitter bugs that would otherwise produce inconsistent typing. (The reverse inconsistency — JSON `drift_event_type: "merge"` but RDF typed `abc:DriftSplitEvent` — is caught by the typing-coherence pre-check.)
- **SHACL composition.** All three shapes (`PersonDriftEventShape`, `PersonDriftSplitEventShape`, `PersonDriftMergeEventShape`) compose with the existing `ActivityShape` at `schemas/manifest.shacl.ttl:59` because `event->graph` materialises `rdf:type prov:Activity` directly on every event node (per the (α) commitment above). Composition does not depend on SHACL-side inference. Verify composition explicitly: an event satisfying every applicable shape (base + subshape + ActivityShape) is the green case; failing any one is the red case.
- **PROV-entity → person_id mapping (resolves Context point 7's referential-integrity ambiguity).** `prov:used` and inverse-`prov:wasGeneratedBy` name `prov:Entity` snapshots — they are NOT person identifiers themselves. The mapping snapshot → person_id is part of the v0 contract:
  - Each snapshot entity uses IRI `https://w3id.org/abc/persons/<person_id>#snapshot-<person_record_hash_short>` (`<...>-short` = first 12 lowercase hex chars of the digest after the `sha256:` prefix in the event JSON's `person_record_hash`).
  - `event->graph` MUST derive that short hash from the participant object embedded in the event, not from the current person record on disk. This preserves Position L: a later bibliographic edit to a person record cannot silently rebind an old drift event to a new snapshot.
  - Each snapshot entity carries an explicit `prov:specializationOf <person-iri>` triple, where `<person-iri>` is the existing person IRI minted by `person_record.clj:75` (Aozora URL for numeric IDs, `https://w3id.org/abc/persons/<id>` for ABC-local IDs per ADR 0020 identifier-policy (b)).
  - The drift event JSON additionally carries an explicit `participants: [{snapshot_id, person_id, person_record_hash}, ...]` array listing every person snapshot involved as a top-level field. This is redundant with the PROV graph but it is the **canonical** participant snapshot list the integrity rules walk; the PROV graph is the **derived** view. Rationale: walking PROV across `prov:specializationOf` for every validation pass is expensive and depends on the snapshot-entity IRI convention being correctly minted; the explicit JSON field is cheap, schema-validated, and direct.

  `validate-drift-events!` reads `participants[]` from the event JSON to apply the integrity rules; it cross-checks against the PROV graph by extracting person_ids from snapshot-entity `prov:specializationOf` targets and snapshot hashes from snapshot IRIs, then asserting those pairs match the JSON participant objects. A mismatch is a validation failure.
  JSON Schema enforces only local shape constraints and split/merge array cardinalities (`split`: exactly one `prov.used`, at least two `prov.was_generated_by`; `merge`: at least two `prov.used`, exactly one `prov.was_generated_by`). `validate-drift-events!` enforces the cross-reference rules JSON Schema cannot express: every `prov.used` and `prov.was_generated_by` value resolves to a unique `participants[].snapshot_id`; every participant appears in exactly one of those two arrays; no participant appears in both; every `snapshot_id` is unique; prefixes match usage (`pre-` for used, `post-` for generated); canonical ordering is followed; `qualified_association.had_role` is exactly `abc:DriftEditor`; and `qualified_association.agent` is a syntactically valid IRI.
- **Index artifact contract (Context point 7).** `schemas/person-drift-index.schema.json` is the JSON Schema 2020-12 contract for per-person index sidecars; the wrapper carries `{schema_id, schema_hash, person_id, drift_event_ids: [...]}`. **No `PersonDriftIndexShape` is materialised**: the index's role is purely traversal/discovery (locate events for a given person without scanning every event file), and the SHACL composition already validates each event individually. JSON-Schema-only validation is sufficient; promoting the index into the RDF graph would add a `prov:Bundle`-shaped layer with no semantic gain. Three referential-integrity rules are part of the ADR-level contract and gate `validate-drift-events!`:
  1. **Every `drift_event_ids[i]` in an index resolves to an existing event file** at `examples/v0/example-persons/_events/<drift_event_id>.json`. A missing target is a validation failure.
  2. **Every event appears in the index of every person named in its `participants[]`** — i.e. for an event whose canonical participant objects carry person_ids `[P1, P2, P3]`, every one of `_indexes/P1.json`, `_indexes/P2.json`, `_indexes/P3.json` must list this event's `drift_event_id`. Asymmetric indexes are a validation failure.
  3. **No event file may exist that is not referenced by any index.** Orphan event files are a validation failure.
- **Validate-design-bundle integration.** Add a new function `validate-drift-events!` in `src/abc/tools/validate_design_bundle.clj`, invoked unconditionally. Returns one of `{:status :not-present}`, `{:status :ok :events N :indexes M}`, `{:status :error :failures [...]}`. The function validates each event against `person-drift-event.schema.json` + the SHACL composition (base + subshape + ActivityShape), validates each index against `person-drift-index.schema.json`, and asserts the three referential-integrity rules. The bundle's overall `:status` is `:error` if drift validation fails, mirroring SHACL/TEI/IIIF. **Negative fixtures live under `fixtures/v0/invalid/drift/`** (parallel to `fixtures/tei/invalid/`), gated by a new `validate-drift-fixtures!` function — separate from the TEI Schematron runner because the input format and validation engine are different.

- [ ] **Step 4: Fill in `Hard Rule` (conditional on ADR 0020 decisions)**

Assuming ADR 0020 chose Position L + drift-log = audit sidecar + indexing (c):

- `manifest.json` for the example work is **byte-identical** before and after introducing drift events for the example person.
- `examples/v0/example-persons/000879.json` is **untouched** — its content hash and on-disk path are unchanged. Flavor 1 references at `validate_design_bundle.clj:365`, `validate_corpus.clj:29`, and `aozora_ingest.clj:90` continue to resolve to the same file.
- `person_record_hash` for 000879 is **unchanged** — the file is not edited.
- `metadata_record_hash` for the example work is **unchanged** — `contributors[]` is not edited (re-attribution is Flavor 1, out of scope).
- Schema hashes (`metadata_record_schema_hash`, `person_record_schema_hash`) are **unchanged** by Flavor 2 unless ADR 0020 chose identifier-policy (b), in which case the schema-hash cascade is triggered and bundled with the Phase 3 work.
- This invariant is scoped to drift artifacts themselves. A real split may also introduce new person records or edit metadata contributors; those ordinary record changes follow the existing Flavor 1 hash cascade and are not covered by the drift-artifact byte-identity rule.

If ADR 0020 chose folder-per-person (indexing = b with folder layout), the Hard Rule is **different**: the move from `examples/v0/example-persons/000879.json` → `examples/v0/example-persons/000879/record.json` is a path change with no content change; `person_record_hash` is invariant under the move; but every callsite that reads the path must change. Specifically: `src/abc/tools/validate_design_bundle.clj:365`, `src/abc/tools/validate_corpus.clj:29`, `src/abc/tools/aozora_ingest.clj:90`, plus any test fixture that names the path. ADR 0021 must list these callsites in its Hard Rule and require them to be updated atomically with the move.

- [ ] **Step 5: Fill in `Acceptance Criteria`**

- `schemas/person-drift-event.schema.json` exists, validates the recommended wrapper-JSON shape, and is referenced from `validate-design-bundle`.
- `schemas/person-drift-index.schema.json` exists, validates the per-person index sidecar wrapper shape (`schema_id`, `schema_hash`, `person_id`, `drift_event_ids[]`), and is referenced from `validate-design-bundle`.
- `PersonDriftEventShape`, `PersonDriftSplitEventShape`, and `PersonDriftMergeEventShape` exist in `schemas/manifest.shacl.ttl`. The two subshapes target their respective subclasses and enforce the cardinality constraints from Context point 4 (option ii). All three shapes compose with the existing `ActivityShape` via data-side `rdf:type` materialisation per the (α) commitment.
- `schemas/manifest.shacl.ttl` carries the three subclass axioms (`abc:DriftEvent rdfs:subClassOf prov:Activity`, `abc:DriftSplitEvent rdfs:subClassOf abc:DriftEvent`, `abc:DriftMergeEvent rdfs:subClassOf abc:DriftEvent`) for vocabulary documentation — they do NOT participate in current validation under the no-inference SHACL wrapper.
- A unit test in `test/abc/tools/person_drift_test.clj` asserts `event->graph` materialises the correct `rdf:type` triples on the activity node: every event gets `abc:DriftEvent` + `prov:Activity`; split events (JSON `drift_event_type: "split"`) additionally get `abc:DriftSplitEvent`; merge events (JSON `drift_event_type: "merge"`) additionally get `abc:DriftMergeEvent`. A second unit test asserts `validate-drift-events!`'s typing-coherence pre-check correctly fails on a hand-crafted RDF graph whose materialised types diverge from the JSON `drift_event_type` (missing subclass, extra subclass, mismatched subclass). Together these are the affirmative tests that the (α) emitter + pre-check contract is wired correctly.
- An example drift event exists under `examples/v0/example-persons/_events/<drift_event_id>.json` (a fictional 2026 split with two successor person snapshots), accompanied by participant index files for every person it references. The fixture exercises the referential-integrity green path. (A separate empty-artifact case exercises the `:not-present` path — `validate-drift-events!` reports `:not-present` and the bundle continues; no integrity rules apply when no events exist.)
- A negative fixture set under `fixtures/v0/invalid/drift/` covers, at minimum:
  - **Base shape failures:** missing `dcterms:date`, unknown `abc:driftEventType`, missing `prov:used`, missing `prov:qualifiedAssociation`. (Hand-crafted TTL only; these are SHACL-targeted constraints on predicates that map directly into the JSON contract.)
  - **Derived-predicate failures (hand-crafted TTL only — JSON cannot express them):** a post-Entity carrying `prov:wasGeneratedBy <activity>` but missing `prov:wasDerivedFrom <pre-Entity>` (violates the post→pre derivation rule); a `prov:wasDerivedFrom` edge whose pre-Entity is not resolvable in the graph; a `prov:wasAssociatedWith` edge whose Agent does not match the `prov:agent` of the qualified Association. Each must FAIL via either a SHACL constraint enforcing the derivation, or a `validate-drift-events!` graph-coherence pre-check (placement is an ADR 0021 commitment — recommended: SHACL where the constraint is local to a node, validator pre-check where it is cross-node).
  - **Cardinality failures (option ii):** a `"split"` event with only one successor (must FAIL `PersonDriftSplitEventShape`); a `"split"` event with zero successors (must FAIL); a `"merge"` event with only one predecessor (must FAIL `PersonDriftMergeEventShape`); a `"merge"` event with zero predecessors (must FAIL).
  - **Typing-discipline failure caught by SHACL backstop (hand-crafted TTL):** a drift event in TTL with `rdf:type abc:DriftEvent` but missing `rdf:type prov:Activity` — must FAIL via `PersonDriftEventShape`'s `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]` constraint. Note: `ActivityShape` itself would NOT fire on this node (no inference layer), so without the base-shape `sh:hasValue` backstop the malformed event would silently pass. This fixture documents the no-inference wrapper contract AND verifies the backstop catches the case in pure SHACL (so `validate-drift-fixtures!` running shapes alone — without `validate-drift-events!`'s pre-check — still detects it).
  - **Typing-coherence failure caught by validator pre-check (hand-crafted TTL + JSON pair):** a drift event whose JSON `drift_event_type` is `"split"` but whose RDF graph carries `rdf:type abc:DriftEvent` + `rdf:type prov:Activity` only (NO `rdf:type abc:DriftSplitEvent`). Pure SHACL would silently pass this (the subshape's `sh:targetClass abc:DriftSplitEvent` does not fire); the typing-coherence pre-check in `validate-drift-events!` MUST detect the missing subclass type and FAIL. This is the affirmative test that the pre-check is wired correctly. (`validate-drift-fixtures!` running shapes alone is documented as NOT detecting this case; the test asserts that the pre-check + SHACL composition together detect it.)
  - **Subtype-mismatch failure caught by SHACL subshape (hand-crafted TTL):** a TTL event typed `abc:DriftSplitEvent` whose RDF `abc:driftEventType` value is `"merge"` — must FAIL the subshape's `sh:hasValue "split"` predicate. This catches emitter bugs that produce a wrong-subtype rdf:type with a correct field, while the typing-coherence pre-check catches the converse (correct subtype rdf:type with wrong field).
  - **Participant-mismatch failures:** an event whose JSON `participants[]` objects do not match the person_ids and snapshot hashes extracted from snapshot-entity IRIs and `prov:specializationOf` triples — must FAIL via the validator's cross-check.
  - **Referential-integrity failures:** an event referenced by an index whose target file does not exist (rule 1 — must FAIL); an event whose participant person_ids are not all listed in the corresponding indexes (rule 2 — must FAIL); an orphan event file under `_events/` not referenced by any index (rule 3 — must FAIL).
  All SHACL-only negative fixtures are wired into a **new** `validate-drift-fixtures!` runner (sibling to `validate-tei-schematron!`, NOT a reuse of it). The validator-only fixtures (typing-coherence, participant-mismatch, referential-integrity) are wired into `validate-drift-events!` test harness and exercise the pre-check + cross-check stages.
- `nix flake check` passes.
- **Drift-artifact invariant:** introducing the example drift event + index sidecar (and the negative fixtures) does NOT rotate `manifest.json` or `manifest_identity_object`. This is the Position L + audit-sidecar guarantee — verified by Phase 3 task 11. **Caveat:** this invariant excludes the schema-widening cascade. If conditional task A (identifier policy b) is implemented in the *same* Phase 3 milestone, then `manifest.json`, `manifest_identity_object.metadata_record_hash`, and `ArtifactID` WILL rotate as part of the cascade — and that rotation is the EXPECTED outcome of task A, separately verified by task 12. The two checks gate disjoint commits: the drift-artifact invariant is checked before/after the drift-only commits; the cascade containment is checked before/after the schema-widening commit.
- All callsite changes (if any) listed in the Hard Rule are completed atomically with the milestone.

- [ ] **Step 6: Commit (Status: Draft)**

```
git add docs/adr/0021-person-identity-drift-harness.md
git commit -m "docs(adr): draft ADR 0021 — person drift harness contract"
```

---

### Task 5: Hand off to user for ADR review

- [ ] **Step 1: Surface both draft ADRs to the user**

After Tasks 3 and 4, the next action is **not** more code. It is a user-facing review pause. Present:
- A summary of ADR 0020's recommended Decision: **Position L** (lineage-only) + **identifier policy (b)** (ABC-local entity IDs, `^abc-[0-9a-f]{12}$`) + **indexing (c)** (stable event IDs + participant indexes) + **drift-log = audit sidecar** + thin abc profile over PROV-O including `abc:DriftEditor` + **two event types (split/merge)** + **`drift_event_id` = sha256 of canonical bytes with `drift_event_id` field omitted from the hashed input**.
- A summary of ADR 0021's recommended Decision: per-event JSON files at `examples/v0/example-persons/_events/<drift_event_id>.json` + per-person index sidecar at `examples/v0/example-persons/_indexes/<person_id>.json` (sibling subdirectory, NOT a flat sidecar — flat would collide with `validate_design_bundle.clj:365`'s person-record discovery) + **wrapper JSON with `participants[]` objects carrying `{snapshot_id, person_id, person_record_hash}`**, canonical ordering by `snapshot_id`, and split/merge predecessor/successor coverage rules so historical snapshot IRIs are bound from event content, not current records + a minimal structured `prov` block carrying only `used`, `was_generated_by`, and `qualified_association`, while `event->graph` derives `prov:wasInvalidatedBy`, `prov:wasAssociatedWith`, and `prov:wasDerivedFrom` into the RDF view + `had_role` as a CURIE resolved through the project prefix map + **specialised subshapes (option ii)** — `PersonDriftEventShape` (base) plus `PersonDriftSplitEventShape` and `PersonDriftMergeEventShape` enforcing split/merge arities — composing with the existing `ActivityShape` + **data-side `rdf:type` materialisation in `event->graph` (option α)** so the no-inference SHACL wrapper at `shacl.clj:58` sees every event as `abc:DriftEvent` + `prov:Activity` + the subclass type without depending on RDFS inference; subclass axioms in `schemas/manifest.shacl.ttl` are vocabulary documentation only + **canonical `participants[]` JSON field** as the source of truth for referential integrity, with snapshot entities mapping to person_ids and historical hashes via `prov:specializationOf` + **per-person index schema `schemas/person-drift-index.schema.json` with no SHACL shape** (JSON-only validation; index is a traversal artifact) + three referential-integrity rules + a participant-mismatch cross-check enforced by `validate-drift-events!` + new `validate-drift-events!` and `validate-drift-fixtures!` runners (NOT reusing the TEI Schematron runner).
- The three load-bearing trade-offs in ADR 0020 (identifier policy, indexing, drift-log identity), with rejected options laid out.
- The schema-hash cascade callout: if the user picks identifier policy (b), `person_record_schema_hash` and `metadata_record_schema_hash` rotate, and the cascade work is bundled with Phase 3.

The user accepts, requests revision, or vetoes. Phase 3 cannot start before acceptance — and the exact shape of every Phase 3 task depends on which options are accepted.

- [ ] **Step 2: After acceptance, mark both ADRs `Accepted` and commit**

```
git add docs/adr/0020-person-identity-drift-data-model.md docs/adr/0021-person-identity-drift-harness.md
git commit -m "docs(adr): accept ADR 0020 + 0021 (person identity drift)"
```

---

## Phase 3 — Implementation (follow-up plan)

The implementation plan is intentionally **not** written here. It depends on the eight question answers in ADR 0020 and the three concerns in ADR 0021. Writing it now would be plan-on-spec mismatch: if the user picks Position H instead of L, every Phase 3 task changes.

### Task 6: Author the Phase 3 implementation plan

**Files:**
- Create: `docs/superpowers/plans/2026-MM-DD-person-drift-implementation.md`
  (The date placeholder is filled in at authoring time.)

- [ ] **Step 1: Authoring trigger**

Author the Phase 3 plan **only after** Tasks 3, 4, and 5 are complete (both ADRs `Accepted`). The Phase 3 plan must use the writing-plans skill exactly like this plan does, with bite-sized tasks (Files + Steps with exact code/commands), file structure, and self-review.

- [ ] **Step 2: Phase 3 plan skeleton (conditional on ADR decisions)**

The Phase 3 task list is **conditional on the answers ADR 0020 + 0021 commit to**. The skeleton below assumes the recommended Decisions; if the user accepts different options, individual tasks shift in shape (file paths, callsite migrations, schema-hash cascade) but the *count* of tasks is roughly stable. The Phase 3 author must read the accepted ADRs first and re-shape each task to match.

**Always-present tasks (any ADR outcome):**

1. **Add `schemas/person-drift-event.schema.json`** — JSON Schema 2020-12 for a single drift event. Exact shape (wrapper JSON vs JSONL-per-line vs sidecar) is set by ADR 0021's Decision Step 3, file format clause. The wrapper-JSON shape carries top-level `schema_id` + `schema_hash` (matching `person-record.schema.json` convention) plus `drift_event_id`, `drift_event_type` (closed enum), `date` (EDTF), `participants[]` (canonical snapshot list with `{snapshot_id, person_id, person_record_hash}` objects — drives referential integrity and snapshot IRI binding), `evidence[]` (IRIs), and an embedded predicate-keyed `prov` block carrying `used` / `was_generated_by` / `qualified_association` keys that `event->graph` translates into RDF `prov:used` / `prov:wasGeneratedBy` / `prov:qualifiedAssociation`. The schema enforces local shape, `snapshot_id` string pattern, `uniqueItems` where useful, and split/merge array cardinalities; it does NOT enforce cross-path reference resolution.
2. **Add `schemas/person-drift-index.schema.json`** — JSON Schema 2020-12 for the per-person index sidecar. The recommended layout (ADR 0021 Decision Step 3, file layout + index-artifact-contract clauses) splits drift state into two artifact kinds: per-event files (validated by task 1's schema) AND per-person index files (validated by this schema). The index wrapper carries `schema_id`, `schema_hash`, `person_id`, `drift_event_ids[]`. Without a dedicated schema, `validate-drift-events!` would have no contract for the index files and would either skip them silently or hand-roll an inline check. **ADR 0021** commits to a separate schema (the alternative — single combined schema with a discriminator `oneOf` on a `kind` field — is rejected because index and event have disjoint validation contracts and JSON Schema's `oneOf` adds error-message noise without semantic gain).
3. **Add SHACL shapes + subclass axioms to `schemas/manifest.shacl.ttl`** — constraints set by ADR 0021's Decision Step 3, SHACL clauses. Three shapes: `PersonDriftEventShape` (base; targets `abc:DriftEvent`; type-agnostic constraints), `PersonDriftSplitEventShape` (targets `abc:DriftSplitEvent`; cardinality: exactly one `prov:used`, ≥2 inverse-`prov:wasGeneratedBy`), `PersonDriftMergeEventShape` (targets `abc:DriftMergeEvent`; cardinality: ≥2 `prov:used`, exactly one inverse-`prov:wasGeneratedBy`). Three subclass axioms are added for vocabulary documentation only, not as a validation mechanism: `abc:DriftEvent rdfs:subClassOf prov:Activity`, `abc:DriftSplitEvent rdfs:subClassOf abc:DriftEvent`, `abc:DriftMergeEvent rdfs:subClassOf abc:DriftEvent`. **No `PersonDriftIndexShape`** — the per-person index is a JSON-only artifact per ADR 0021 Decision Step 3 (index contract clause). If ADR 0020 deviates from the recommended Decisions, this task's shape list is rebuilt against the accepted choices.
4. **Add an example drift event AND example index sidecar(s)** — exact paths set by ADR 0021's Decision Step 3, file layout clause (per-event file at `examples/v0/example-persons/_events/<drift_event_id>.json`, per-person index at `examples/v0/example-persons/_indexes/<person_id>.json`). The fixture must exercise the referential-integrity green path: every participant of the example event has an index file listing the event's `drift_event_id`.
5. **Add a `abc.tools.person-drift` namespace** — `read-events`, `read-index`, `validate-event`, `validate-index`, `event->graph` mirrors of `abc.tools.person-record`. `event->graph` MUST materialise `rdf:type` triples on the activity node per ADR 0021 Decision Step 3 (α): always `abc:DriftEvent` + `prov:Activity`; for JSON `drift_event_type: "split"` additionally `abc:DriftSplitEvent`; for JSON `drift_event_type: "merge"` additionally `abc:DriftMergeEvent`. The emitter also derives the three remaining canonical PROV predicates per ADR 0021 Decision Step 3 ("JSON `prov` block — minimal contract" clause): `prov:wasInvalidatedBy` from each `prov:used`, `prov:wasAssociatedWith` from `qualified_association.agent`, and `prov:wasDerivedFrom` from each (post, pre) snapshot pair. Snapshot entities get IRI `https://w3id.org/abc/persons/<person_id>#snapshot-<person_record_hash_short>` where the short hash is derived from the event JSON participant object's `person_record_hash` field, never from the current record on disk; each snapshot carries `prov:specializationOf <person-iri>` per the PROV-entity → person_id mapping clause. Unit tests in `test/abc/tools/person_drift_test.clj` assert both rdf:type and derived-predicate invariants on representative split and merge events, plus a regression that changing the current person record does not change a historical event's snapshot IRI.
6. **Extend `abc.tools.validate-design-bundle`** — add `validate-drift-events!` (unconditional run) that processes each event file in this order: (a) JSON-Schema-validate against `person-drift-event.schema.json`; (b) run JSON graph-coherence checks that the schema cannot express — snapshot_id uniqueness, `prov.used` / `prov.was_generated_by` resolution to `participants[].snapshot_id`, exact participant coverage, no participant in both arrays, prefix/usage consistency, canonical ordering, `had_role` enum/CURIE resolution, and `agent` IRI syntax; (c) emit RDF via `event->graph`; (d) **typing-coherence pre-check** — derive expected `rdf:type` set from the JSON `drift_event_type` field (`{abc:DriftEvent, prov:Activity, abc:DriftSplitEvent}` for split, the merge variant for merge), compare against the activity node's actual `rdf:type` set, fail on any divergence (missing/unexpected/wrong); (e) cross-check the canonical `participants[]` objects against the person_ids and snapshot hashes extracted from snapshot-entity IRIs and `prov:specializationOf` targets in the PROV graph; (f) SHACL composition: base shape + applicable subshape + `ActivityShape`. After per-event validation, validate each index file against `person-drift-index.schema.json` (JSON only, no SHACL shape per ADR 0021). Then assert the three referential-integrity rules from ADR 0021 Decision Step 3 (index contract clause): (1) every `drift_event_ids[i]` in an index resolves to an existing event file under `_events/`; (2) every person_id in the event's participant objects has the event listed in their `_indexes/<person_id>.json`; (3) no orphan event file exists under `_events/` without an index reference. Each rule violation surfaces as a distinct `:failures` entry so negative fixtures can target them individually.
7. **Add a separate JSON/SHACL drift-fixtures runner** `validate-drift-fixtures!` in the same namespace — sibling to `validate-tei-schematron!` at `src/abc/tools/validate_design_bundle.clj:274`, NOT a reuse. ADR 0021 commits to this separation in Decision Step 3.
8. **Add deftests** in `test/abc/tools/person_drift_test.clj`.
9. **Add focused-test alias entry** in `nix/clj-nix-deps.edn` for the new test namespace; re-run `bin/update-clj-nix-lock`.
10. **Add negative fixtures** under `fixtures/v0/invalid/drift/`, partitioned by which validation stage they exercise. **SHACL-only fixtures** (TTL inputs, run by `validate-drift-fixtures!` directly against the shapes — sibling to `validate-tei-schematron!`, NOT a reuse): (a) **Base-shape failures** — missing `dcterms:date`, unknown `abc:driftEventType` value, missing `prov:used`, missing `prov:qualifiedAssociation`. (b) **Derived-predicate failures (hand-crafted TTL only — JSON cannot express them)** — post-Entity carrying `prov:wasGeneratedBy <activity>` but missing `prov:wasDerivedFrom <pre-Entity>`; `prov:wasDerivedFrom` edge whose pre-Entity is not resolvable in the graph; `prov:wasAssociatedWith` edge whose Agent does not match the `prov:agent` of the qualified Association. Each must FAIL via either the SHACL constraint or the graph-coherence pre-check selected by ADR 0021. (c) **Cardinality failures** — split-with-1-successor, split-with-0-successors, merge-with-1-predecessor, merge-with-0-predecessors (each must FAIL its respective subshape). (d) **Typing-discipline (SHACL backstop)** — a TTL event with `rdf:type abc:DriftEvent` but missing `rdf:type prov:Activity`. Must FAIL via `PersonDriftEventShape`'s `sh:hasValue prov:Activity` constraint (NOT via `ActivityShape`, which doesn't fire under the no-inference wrapper). (e) **Subtype-mismatch (SHACL)** — a TTL event typed `abc:DriftSplitEvent` whose RDF `abc:driftEventType` value is `"merge"` (must FAIL the subshape's `sh:hasValue "split"` predicate); converse for merge. **Validator-only fixtures** (JSON+TTL pairs or full corpus shapes, run by `validate-drift-events!` end-to-end): (f) **Typing-coherence (validator pre-check)** — JSON `drift_event_type: "split"` paired with RDF that materialises only `{abc:DriftEvent, prov:Activity}` (missing `abc:DriftSplitEvent`). Must FAIL the typing-coherence pre-check; documented as NOT detectable by SHACL alone (the shape silently does not fire). (g) **Participant-mismatch** — JSON `participants: [{snapshot_id: S1, person_id: P1, person_record_hash: H1}, {snapshot_id: S2, person_id: P2, person_record_hash: H2}]` paired with PROV graph whose snapshot entities encode `{P1/H1, P3/H3}`. Must FAIL the validator's PROV cross-check. (h) **JSON graph-coherence** — dangling snapshot reference, duplicate `snapshot_id`, participant in neither array, participant in both arrays, wrong `pre-`/`post-` prefix for usage, unsorted participant/prov arrays, unknown `had_role` CURIE prefix, non-`abc:DriftEditor` role, invalid `agent` IRI. Each must FAIL before SHACL. (i) **Referential-integrity** — index pointing at a missing event file (rule 1), event whose participant objects are not all indexed by person_id (rule 2), orphan event file with no index reference (rule 3) — each must FAIL the corresponding integrity rule.
11. **Verify drift-event invariant** — adding/removing a drift artifact (per-event JSON file + per-person index sidecar) does NOT rotate `manifest.json`, `metadata_record_hash`, or `person_record_hash` for 000879. This is the Position L + audit-sidecar guarantee. The check runs after task 4 (example drift event + index added) and again after task 10 (negative fixtures added) — neither materialisation may rotate identity hashes. Note: this invariant only holds for **the drift artifact itself**; the schema-widening cascade in conditional task A is a *separate* rotation event with its own bounded scope (see task 12).
12. **Verify schema-widening cascade is contained to its expected reach** (only present if conditional task A ran, i.e. ADR 0020 chose identifier policy b). The cascade is **transitive**, not isolated to schema fields, because both `abc.tools.person-record/canonical-identity-form` (`src/abc/tools/person_record.clj:58`) and `abc.tools.metadata-record/canonical-identity-form` (`src/abc/tools/metadata_record.clj:40`) only drop `source_csv_provenance` from the hashed input — `person_record_schema_hash` and `metadata_record_schema_hash` are therefore *part* of the record's canonical form, so a schema-hash rotation cascades into the record hash. The expected bounded reach after widening `person_id` regex:

   **MUST rotate (in this dependency order):**
   - `person_record_schema_hash` (the schema's own JCS-SHA-256, recomputed against the widened JSON Schema).
   - Every `person_record.json`'s stored `person_record_schema_hash` field.
   - `person_record_hash` for every person record (because `person_record_schema_hash` flows into `canonical-identity-form` per `person_record.clj:58`).
   - `metadata_record_schema_hash` (the metadata schema also widens `person_id` at `schemas/metadata-record.schema.json:104`).
   - Every `metadata_record.json`'s stored `metadata_record_schema_hash` field.
   - Every `contributors[i].person_record_hash` materialisation inside `metadata_record.json` (since person hashes rotated upstream).
   - `metadata_record_hash` for every work (because `metadata_record_schema_hash` AND the rotated `contributors[]` flow through `canonical-identity-form` per `metadata_record.clj:40`).
   - `manifest_identity_object.metadata_record_hash` and the dependent `ArtifactID` content-address.
   - `manifest.json` for the example bundle (regenerated to reflect the rotated identity object).

   **MUST NOT rotate (semantic content-fields that are unrelated to the regex widening):**
   - Any record's `family_name` / `given_name` / `date_of_birth` / etc. (no field semantics changed; only the validation regex widened).
   - `manifest_identity_object` fields that are not derived from `metadata_record_hash` (e.g. structural-hash dimensions if any).
   - Bibliographic SHACL shape files, TEI fixture XML, IIIF manifest content, etc.

   The check enumerates the affected files via `git diff` against the pre-cascade commit, partitions the diff into "stored hash field changed" vs "content field changed", and asserts: the first partition matches the MUST-rotate list exactly; the second partition is empty. A missing rotation in the first list is a bug (means the cascade didn't propagate where it was expected to); an entry in the second list is a bug (means an unrelated content edit slipped in). If conditional task A did NOT run, this verification is skipped.
13. **Update `docs/next-steps.md`** — milestone entry, drop the candidate (read current count, bump by one, do not hard-code).
14. **Commit per task.**

**Conditional tasks (present only if ADR 0020/0021 land specific options):**

A. **If ADR 0020 chose identifier policy (b) ABC-local entity IDs:** widening the JSON Schema regex is necessary but **not sufficient** — the RDF view and SHACL shapes also assume Aozora-numeric IDs and must be widened atomically with the JSON change. The full set of edits:

   **A.1 — JSON Schema widening (rotates schema hashes):**
   - `schemas/person-record.schema.json:21` — widen `person_id` `pattern` from `^[0-9]{6}$` to the union `^([0-9]{6}|abc-[0-9a-f]{12})$` (or whichever pattern ADR 0020 commits to).
   - `schemas/metadata-record.schema.json:104` — widen `contributors[].person_id` to the same union.
   - This rotates `person_record_schema_hash` and `metadata_record_schema_hash`.

   **A.2 — RDF emission (`abc.tools.person-record`):**
   - `src/abc/tools/person_record.clj:75` `person-iri` builds `http://www.aozora.gr.jp/index_pages/person<id>.html`. That URL is wrong for ABC-local IDs (no Aozora landing page exists). Add a branch: if `person_id` matches `^[0-9]{6}$`, keep the Aozora URL; if it matches the ABC-local pattern, mint a different IRI (e.g. `https://w3id.org/abc/persons/<id>`). ADR 0020 must commit to the exact ABC-local IRI base; ADR 0021 references it.
   - `src/abc/tools/person_record.clj:138` emits `dcterms:identifier` as `(->int-literal …)` (i.e. `xsd:int`). For ABC-local IDs, the literal cannot be `xsd:int` — `abc-deadbeefcafe` is not a valid integer. Branch the type: keep `xsd:int` for Aozora-numeric IDs, switch to `xsd:string` (or a typed `abc:PersonId` datatype) for ABC-local IDs. ADR 0020 picks one.

   **A.3 — SHACL shape (`schemas/manifest.shacl.ttl:170`):**
   - `PersonRecordShape`'s `dcterms:identifier` constraint currently requires `sh:minCount 1`, `sh:maxCount 1`, and `sh:datatype xsd:int`. Preserve `sh:minCount 1` and `sh:maxCount 1`; widen only the value constraint to `sh:or ([sh:datatype xsd:int ; sh:pattern ...] [sh:datatype xsd:string ; sh:pattern ...])` (or whichever single datatype ADR 0020 commits to in A.2). If ADR 0020 introduces a new `abc:PersonId` datatype, add it to the `sh:or` and assert `sh:pattern` separately.
   - Verify the widened shape still rejects empty strings, whitespace, and other malformed values.

   **A.4 — Cascade (transitive, automatic once A.1 + A.2 + A.3 land):**
   This is **the** schema-hash cascade for v0. Because `*_schema_hash` is part of each record's canonical-identity input (see `person_record.clj:58` and `metadata_record.clj:40`), the JSON Schema change propagates through every `person_record_hash` and `metadata_record_hash`. The cascade work bundled with this task: regenerate every record's stored `person_record_schema_hash` field, recompute every `person_record_hash`, regenerate every record's stored `metadata_record_schema_hash` field, recompute every `contributors[i].person_record_hash` materialisation, recompute every `metadata_record_hash`, regenerate `manifest_identity_object.metadata_record_hash` + `ArtifactID`, and rebuild `manifest.json` for the example bundle.

   **A.5 — TTL fixture:**
   - Regenerate `examples/v0/example-work/metadata-record.ttl` to reflect the (possibly) new IRI base or datatype if any committed-to-disk TTL fixture references an ABC-local person. The example bundle's 000879 is Aozora-numeric, so the existing TTL is unaffected unless ADR 0021 chooses to add an ABC-local example.

   **Verification:** Containment is checked by task 12 ("Verify schema-widening cascade is contained to its expected reach"), which asserts the rotated set matches the expected MUST-rotate list exactly (now including any TTL fixture rotated under A.5) and that no semantic content field changed.

B. **If ADR 0021 chose folder-per-person layout (indexing = b with folder):** migrate `examples/v0/example-persons/000879.json` → `examples/v0/example-persons/000879/record.json` AND update every callsite atomically. Confirmed callsites at the time of this plan: `src/abc/tools/validate_design_bundle.clj:365`, `src/abc/tools/validate_corpus.clj:29`, `src/abc/tools/aozora_ingest.clj:90`. Re-grep at Phase 3 authoring time in case more have appeared. **`person_record_hash` MUST be invariant under the move** — the file's content is unchanged; only its on-disk path changes.

C. **If ADR 0021 chose JSONL with per-line schema fields (option β):** add a per-line `schema_id` + `schema_hash` validator step that runs before SHACL — every line must be parseable JSON, every parsed object must carry the two fields, every value must match the canonical schema id and current schema hash.

D. **If ADR 0020 chose drift-log = corpus artifact (rejected by recommendation but possible):** add the drift log's content hash to `manifest_identity_object`. This breaks Position L's monotonicity invariant; if both decisions land this way, re-shape this entire task list.

- [ ] **Step 3: Run writing-plans self-review on the Phase 3 plan**

Same checklist (spec coverage, placeholder scan, type/name consistency) before considering Phase 3 ready for execution.

- [ ] **Step 4: Hand the Phase 3 plan to subagent-driven-development or executing-plans**

Same handoff pattern as this plan and the TEI-rule-expansion plan.

---

## Self-Review

**1. Spec coverage.** The spec scope (`docs/superpowers/specs/2026-04-28-separated-person-records-design.md` lines 15–17) names "PROV-style event log" as the Flavor 2 mechanism. Tasks 1–4 produce the data model + harness contract; Task 6 produces the implementation plan; the only open work then is execution. Coverage: complete to the ADR boundary, deferred from there. Re-attribution is explicitly out of scope (Flavor 1 territory) — see Context.

**2. Placeholder scan.** Two intentional placeholders:
- Phase 3 file path uses `2026-MM-DD-` because the date is the authoring date, not 2026-04-29 — Phase 3 is authored after ADR acceptance, which may be days later. This is acceptable; the writing-plans skill recommends date-stamped plans.
- Phase 3 task list (Task 6 Step 2) gives task descriptions, not code/commands. This is acceptable because the code/commands cannot be written before ADR 0020 / 0021 land. The Phase 3 plan, when authored, must contain the code.

No accidental placeholders ("TBD", "implement appropriately", "handle edge cases" without code).

**3. Type/name consistency.** ADR numbers `0020` and `0021` are referenced consistently. The proposed shape name `PersonDriftEventShape` appears identically in ADR 0021 Decision Step 3, Acceptance Criteria, and Phase 3 Task 6 Step 2. The vocabulary terms `abc:DriftEvent`, `abc:driftEventType`, `abc:driftEvidence` are introduced in ADR 0020 Decision Step 3 and reused in ADR 0021 Decision and Phase 3 task list. The runner names `validate-drift-events!` and `validate-drift-fixtures!` appear identically in ADR 0021 Decision Step 3 and Phase 3 Task 6 Step 2 (tasks 5 and 6).

**4. ADR ordering check.** ADR 0020 (data model) precedes ADR 0021 (harness) in numbering and in dependency: 0021's every Decision clause is conditional on a 0020 decision. Acceptance order matters: the user reviews 0020 first, then 0021. Task 5 (review handoff) is a single bundled review because the two ADRs only make sense together — splitting them would force the user to re-read the data-model context twice.

**5. Boundary with Flavor 1.** The just-landed Flavor 1 milestone is *not* re-opened by this plan unless ADR 0021 lands folder-per-person layout (rejected by recommendation). The recommended decision keeps `examples/v0/example-persons/000879.json` untouched and adds drift artifacts at sibling paths — no callsite migration. If folder-per-person is chosen instead, the affected callsites (`validate_design_bundle.clj:365`, `validate_corpus.clj:29`, `aozora_ingest.clj:90`) are listed in ADR 0021's Hard Rule and Phase 3 Task 6 Step 2 conditional task B.

**6. PROV graph correctness.** The earlier draft of this plan misused `prov:wasDerivedFrom` as Activity→Entity. Corrected throughout: derivation is Entity→Entity (post-snapshot ← pre-snapshot); the Activity is referenced via `prov:used` (Activity→Entity) and `prov:wasGeneratedBy` (Entity→Activity). The canonical graph diagram appears in Phase 1 Task 1 Step 3 and is reproduced in ADR 0020 Decision and ADR 0021 Context. ADR 0021's PROV concern explicitly calls out this correction so the Phase 3 plan does not reintroduce it.

**7. SHACL composition with existing `ActivityShape` requires data-side `rdf:type` AND a backstop, since missing types silently bypass SHACL targeting.** The existing `ActivityShape` at `schemas/manifest.shacl.ttl:59` requires `prov:used` + `prov:qualifiedAssociation` on every `prov:Activity`. The repo's SHACL wrapper at `src/abc/tools/shacl.clj:58` runs Jena's plain validator with no RDFS inference (verified empirically), so SHACL-side `rdfs:subClassOf` axioms do NOT cause `ActivityShape` to fire on bare `abc:DriftEvent` instances. ADR 0021 Decision Step 3 commits to data-side materialisation: `event->graph` emits `rdf:type abc:DriftEvent`, `rdf:type prov:Activity`, AND the appropriate `rdf:type abc:DriftSplitEvent` / `abc:DriftMergeEvent` directly on the activity node, derived from the event JSON's `drift_event_type`. **Materialisation alone is insufficient**: if the emitter or a hand-crafted fixture omits a type, the targeted shape silently does not fire and SHACL emits no violation. Two backstops close the gap: (i) `PersonDriftEventShape` directly asserts `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]` so the missing-`prov:Activity` case is caught in pure SHACL (visible to `validate-drift-fixtures!` running shapes alone); (ii) `validate-drift-events!` runs a typing-coherence pre-check before SHACL that asserts the activity node's `rdf:type` set matches what the JSON `drift_event_type` field implies. Acceptance Criteria gate on negative fixtures for both backstops AND a unit test on `event->graph` proving the emitter materialises correctly.

**8. Three load-bearing open questions surfaced.** ADR 0020 Step 2 (Context) and Step 3 (Decision) require explicit answers to: identifier policy (a/b/c), multi-person event indexing (a/b/c), and drift-log identity (corpus artifact vs audit sidecar). Every ADR 0021 Decision clause is conditional on these answers. The recommended Decisions are spelled out in Task 5 (review handoff) so the user can accept/revise/veto each independently.

**9. Schema-hash cascade callout.** Identifier policy (b) widens `person_id` regex and rotates `person_record_schema_hash` + `metadata_record_schema_hash`. ADR 0020 Hard Rule and Phase 3 Task 6 Step 2 conditional task A both call this out. The cascade work is bundled with Phase 3, not with Phase 1/2 — ADRs decide; Phase 3 implements + verifies the cascade is contained.

**10. JSONL vs schema-hash conflict resolved.** JSONL has no file-level header for `schema_id`/`schema_hash`. ADR 0021 Context point 2 enumerates three options (wrapper JSON, JSONL with per-line schema fields, JSONL + sidecar) and Decision Step 3 file-format clause picks wrapper JSON for the recommended layout (per-event single-event-per-file, no append-only requirement). JSONL is rejected explicitly. If ADR 0021 lands JSONL anyway (option β), Phase 3 Task 6 Step 2 conditional task C handles the per-line schema-fields validator.

**11. Fixture runner separation.** The existing `validate-tei-schematron!` invalid-fixtures runner at `src/abc/tools/validate_design_bundle.clj:274` is TEI/Schematron-specific and cannot validate JSON+SHACL drift events. ADR 0021 Context point 6 calls this out; Decision Step 3 commits to a sibling `validate-drift-fixtures!` runner; Acceptance Criteria require it; Phase 3 Task 6 Step 2 task 6 implements it; task 9 wires negative fixtures into it (NOT into the TEI runner). The earlier "wire into the existing invalid-fixtures runner" wording is fixed everywhere.

**12. Discovery command corrected.** Phase 1 Task 1 Step 1 uses the actual aozora-ingest CLI shape (`--zip` + `--all` + `--output-dir` per `src/abc/tools/aozora_ingest.clj:295`). The earlier `--dry-run` flag does not exist and has been removed.

**13. Identity-invariance vs schema-widening separated, with correct cascade semantics.** Phase 3 task 10 ("verify drift-event invariant") and task 11 ("verify schema-widening cascade is contained to its expected reach") are now distinct checks. Task 10 asserts the **drift artifact** does not rotate `manifest.json`, `metadata_record_hash`, or `person_record_hash`. Task 11 — only present if ADR 0020 chose identifier policy (b) — asserts the **schema-widening cascade** rotates the *full transitive* expected set: schema hashes → stored `*_schema_hash` fields → `person_record_hash` (because the schema hash is part of the record's canonical-identity input per `person_record.clj:58`) → `contributors[i].person_record_hash` materialisations → `metadata_record_hash` (per `metadata_record.clj:40`) → `manifest_identity_object.metadata_record_hash` + `ArtifactID` → `manifest.json`. An earlier draft of this plan claimed schema rotation must NOT propagate to record hashes — that was wrong because both record's `canonical-identity-form` retain `*_schema_hash`. The corrected expectation is that the cascade IS transitive but BOUNDED: every transitively-derived hash MUST rotate, and no semantic content field (e.g. `family_name`, TEI fixture XML, IIIF) MUST rotate.

**14. `drift_event_id` derivation made precise.** ADR 0020 Decision Step 3 (recommended bullet for indexing = c) and Task 5 review handoff now specify: hash JCS-canonical bytes of the wrapper JSON object **with `drift_event_id` omitted** from the hashed input, then materialise the resulting `sha256:` value back into the file. Validation re-derives the ID from the rest of the body and asserts equality. The earlier "sha256 of canonical bytes" wording was self-referential (the value would have to be present in the bytes it hashes).

**15. Rename event type dropped (with ADR opt-in).** Plan-wide alignment on `("split" "merge")` only — Phase 1 Task 1 Step 3 PROV graph diagram, Task 2 question 4, ADR 0020 Decision Step 3, ADR 0021 Context point 3, and the top-line Goal sentence all consistently scope to two event types. The earlier inconsistency (some sections listed three types, others two) is fixed. ADR 0020 may choose to keep `rename` only if it defines the type precisely (e.g. 1-predecessor / 1-successor with a name change, distinct from a Flavor 1 edit); otherwise the closed enum is two-type.

**16. ABC-local `person_id` widens RDF + SHACL atomically with JSON.** Conditional task A is no longer "JSON-schema regex only". A.1 widens the JSON Schemas, A.2 branches the RDF emission (`person-iri` at `person_record.clj:75` builds a different IRI for ABC-local IDs; `dcterms:identifier` at `person_record.clj:138` uses a different datatype because `xsd:int` cannot carry `abc-deadbeefcafe`), A.3 widens `PersonRecordShape` at `schemas/manifest.shacl.ttl:170` so the `dcterms:identifier` constraint accepts both datatypes while preserving min/max cardinality, A.4 is the cascade, A.5 regenerates the TTL fixture if a committed example uses an ABC-local ID. Without A.2 + A.3 the JSON would validate but the RDF view would emit an invalid `xsd:int` literal that fails SHACL — i.e. the bundle would fail validation post-widening. ADR 0020 must commit to the ABC-local IRI base and the chosen datatype; ADR 0021 references both.

**17. Per-person index sidecar has its own schema.** Phase 3 task list now distinguishes two artifact kinds and two schemas: `schemas/person-drift-event.schema.json` (single drift event, validated by `PersonDriftEventShape` + subshapes) and `schemas/person-drift-index.schema.json` (per-person index sidecar, JSON-only — **no SHACL shape** per ADR 0021 Decision Step 3). `validate-drift-events!` validates both, plus referential integrity (every `drift_event_ids[i]` resolves; every event participant appears in every relevant index; no orphan event files). Without this split, the index files would have no contract; the earlier draft listed only the per-event schema.

**18. Byte-identical acceptance criterion qualified for cascade.** ADR 0021 Acceptance Criteria's "drift-artifact invariant" line now explicitly excludes the schema-widening cascade. If conditional task A lands in the same Phase 3 milestone, `manifest.json` and `manifest_identity_object.metadata_record_hash` WILL rotate — and that rotation is the *expected* outcome of task A, separately verified by task 12 (cascade containment). The two checks gate disjoint commits: drift-artifact invariant before/after drift-only commits; cascade containment before/after the schema-widening commit. The earlier unqualified "byte-identical" criterion contradicted task 12's expected rotation.

**19. Flavor 1 has no standalone ADR — citations point to the spec.** The separated-person-records milestone (Flavor 1) was implemented but never adopted as a standalone ADR. The Cross-references section names the spec (`docs/superpowers/specs/2026-04-28-separated-person-records-design.md`) and the implementation files (`schemas/person-record.schema.json`, `src/abc/tools/person_record.clj`, `schemas/manifest.shacl.ttl PersonRecordShape`, `examples/v0/example-persons/`) as the Flavor 1 contract surface; ADR 0020 must cite these in its Context, NOT a non-existent prior ADR. ADR 0017's References section was already corrected (independent of this plan) to cite the spec rather than the wrong ADR numbers. The same correction discipline applies to ADR 0020's References.

**20. ADR 0020 commits to the ABC-local IRI base + datatype + SHACL widening as ADR-level decisions.** Identifier policy (b) is not a JSON-Schema-only switch; the ADR-level commitments now spelled out in ADR 0020 Decision Step 3 (recommended bullet for identifier policy b): the JSON Schema regex (`^([0-9]{6}|abc-[0-9a-f]{12})$`), the ABC-local IRI base (`https://w3id.org/abc/persons/<id>`), the `dcterms:identifier` datatype for ABC-local IDs (recommended `xsd:string`; alternative is a typed `abc:PersonId` datatype), and the `PersonRecordShape` SHACL widening pattern preserving `sh:minCount 1` + `sh:maxCount 1` while using `sh:or` for the datatype/pattern branches. Without these at ADR level, Phase 3 conditional task A would silently re-design them — a contract surface decided by the implementation rather than the ADR. ADR 0021 references the IRI base + datatype where its negative fixtures need them.

**21. ADR 0021 commits to specialised subshapes (option ii).** Split/merge cardinality is enforced by specialised subshapes `abc:DriftSplitEvent` / `abc:DriftMergeEvent` rather than `sh:qualifiedValueShape` blocks on the base shape — cleaner composition, additive extension for any third event type later. Decision Step 3 commits to (ii); Acceptance Criteria gate on cardinality negative fixtures (split with 1 successor, split with 0 successors, merge with 1 predecessor, merge with 0 predecessors — each must FAIL its subshape) AND a subtype-mismatch fixture (a node typed `abc:DriftSplitEvent` whose RDF `abc:driftEventType` value is `"merge"` must FAIL the subshape's `sh:hasValue` predicate, catching emitter bugs that produce a wrong-subtype rdf:type with a correct field).

**22. Index artifact contract is part of the ADR, not Phase 3 implementation detail.** ADR 0021 Decision Step 3 commits to: the schema file path `schemas/person-drift-index.schema.json`; the wrapper-JSON shape `{schema_id, schema_hash, person_id, drift_event_ids: [...]}`; **no `PersonDriftIndexShape`** (the index is a traversal artifact, JSON-only validation is sufficient); and three referential-integrity rules — every `drift_event_ids[i]` resolves, every event appears in every participant's index, no orphan event files. Acceptance Criteria gate on a negative fixture per rule (broken target → FAIL; asymmetric index → FAIL; orphan event file → FAIL). Without ADR-level commitment to the schema path and integrity rules, Phase 3 has no contract to validate index files against — `validate-drift-events!` would silently skip them or hand-roll an inline check. The earlier "implementation detail" framing is fixed in Context point 7 and Decision Step 3.

**23. Axiom-loading correction: data-side `rdf:type` materialisation + typing-coherence pre-check + SHACL backstop.** The earlier draft of this plan committed to "option α — bake the subclass axiom into the SHACL file" with the assumption that Jena's SHACL engine would compose `ActivityShape` onto bare `abc:DriftEvent` nodes via the axiom. Empirical testing against the repo's wrapper at `src/abc/tools/shacl.clj:58` confirms this is wrong: the wrapper calls `(.validate validator shapes-graph data-graph)` with no inference layer, so subclass axioms in the shapes graph do NOT trigger targeting in the data graph. A second-order error in the first correction was assuming that requiring `event->graph` to materialise the types was sufficient — it's not, because under the no-inference wrapper, a missing or wrong rdf:type silently bypasses the targeted shape (no violation emitted, malformed event passes). Context point 5 and Decision Step 3 now commit to a three-part mechanism under option (α): (1) `event->graph` materialises `rdf:type abc:DriftEvent`, `rdf:type prov:Activity`, AND the subclass type derived from JSON `drift_event_type`; (2) `validate-drift-events!` runs a typing-coherence pre-check before SHACL that fails on any rdf:type set divergence from the expected; (3) `PersonDriftEventShape` directly asserts `sh:property [ sh:path rdf:type ; sh:hasValue prov:Activity ]` as a SHACL-only backstop visible to `validate-drift-fixtures!`. The subclass axioms remain in `schemas/manifest.shacl.ttl` for vocabulary documentation only. Acceptance Criteria add fixtures for both backstops independently. Options (β) (pre-validation materialisation in the validator) and (γ) (wrapper inference upgrade) are rejected with explicit reasons. The earlier negative fixture "must FAIL via the baked-in subclass axiom" was self-contradictory; the earlier corrected fixture "must FAIL via `ActivityShape`" was also wrong under the no-inference wrapper (`ActivityShape` doesn't fire on a bare `abc:DriftEvent`); the current fixture "must FAIL via `PersonDriftEventShape`'s `sh:hasValue prov:Activity` backstop" is correct.

**24. Specialised subshapes need explicit `rdf:type` for their subclass — and a validator-level pre-check, since SHACL alone cannot detect missing subclass types.** Section 23's data-side materialisation extends to the subshapes: `event->graph` emits `rdf:type abc:DriftSplitEvent` (resp. `abc:DriftMergeEvent`) on every event whose JSON `drift_event_type` is `"split"` (resp. `"merge"`). Without this, `PersonDriftSplitEventShape`'s `sh:targetClass abc:DriftSplitEvent` does not fire — the cardinality check is silently bypassed and a malformed split event PASSES. **Pure SHACL cannot guard this in a SHACL-only fixture context** because the failure is "shape did not fire" rather than "shape fired and rejected" — there is nothing for SHACL to emit a violation about. The detection mechanism is the typing-coherence pre-check in `validate-drift-events!` (compares JSON `drift_event_type` to the activity node's rdf:type set). The Acceptance Criteria's six fixture categories collectively cover all the failure modes: (a) missing-`prov:Activity` typing → caught by `PersonDriftEventShape` `sh:hasValue` backstop in pure SHACL; (b) missing subclass type → caught by typing-coherence pre-check (validator only; documented as not detectable by SHACL alone); (c) wrong subclass type but consistent JSON → caught by subshape's `sh:hasValue` predicate in pure SHACL; (d) right subclass type but wrong cardinality → caught by subshape's arity constraints in pure SHACL; (e) participant mismatch → caught by validator's PROV cross-check; (f) referential-integrity failures → caught by validator. The `event->graph` + `validate-drift-events!` unit tests are the affirmative proof that emitter and pre-check produce consistent (type, JSON-field) pairs.

**25. PROV-entity → person_id/hash mapping made explicit.** The earlier framing of referential-integrity rule 2 ("every event appears in the index of every participant it references") implicitly treated `prov:used` and inverse-`prov:wasGeneratedBy` targets as person identifiers. They are not — they are `prov:Entity` snapshots. ADR 0021 Decision Step 3 now commits to the snapshot → person_id/hash mapping: snapshot entities use IRI `https://w3id.org/abc/persons/<person_id>#snapshot-<short-hash>` and carry `prov:specializationOf <person-iri>`; the event JSON additionally carries an explicit `participants: [{snapshot_id, person_id, person_record_hash}, ...]` array as the **canonical** participant snapshot list. `event->graph` resolves `prov.used` / `prov.was_generated_by` local references through `snapshot_id` and derives the snapshot short hash from the event JSON, never by reading the current person record, so historical drift events remain bound to the observed person snapshot. `validate-drift-events!` reads `participants[]` for the integrity rules and cross-checks against the PROV graph (extracting person_ids and hashes from snapshot-entity IRIs and `prov:specializationOf` targets); a mismatch is a participant-mismatch validation failure. Acceptance Criteria add a negative fixture for this case. Without the explicit mapping, Phase 3 would have to invent the snapshot identity convention itself — a contract surface decided by the implementation rather than the ADR.

**26. Empty-corpus caveat removed from Acceptance.** The earlier "or empty if the corpus has no real cases" language contradicted the requirement that the green-path fixture exercise referential integrity (which is not exercisable with zero events). Acceptance Criteria now requires an unconditional fictional 2026-split fixture with participant indexes for the green path; the empty-corpus / `:not-present` case is handled by `validate-drift-events!`'s status return, not by the example fixture.

**27. JSON / RDF naming discipline.** The earlier draft mixed JSON keys and RDF predicates ambiguously (e.g. "the JSON `abc:driftEventType` field" — `abc:driftEventType` is the RDF predicate; the JSON key is `drift_event_type`). Decision Step 3 now opens with an explicit JSON-field-naming clause that pins the bidirectional mapping: JSON `drift_event_id` (no RDF), JSON `drift_event_type` ↔ RDF `abc:driftEventType`, JSON `date` ↔ RDF `dcterms:date`, JSON `participants[]` (no direct RDF; carries `{snapshot_id, person_id, person_record_hash}` objects), JSON `evidence[]` ↔ RDF `abc:driftEvidence`, JSON `prov` block keys `used` / `was_generated_by` / `qualified_association` ↔ RDF `prov:used` / `prov:wasGeneratedBy` / `prov:qualifiedAssociation`. Subsequent clauses MUST use the `snake_case` form when referring to the JSON key and the `prefix:local` form when referring to the RDF predicate. Mixing the two is forbidden in the ADR text. This avoids the ambiguity Phase 3 would otherwise have to resolve by inspecting the schema.

**28. Index sidecar lives under a sibling subdirectory to avoid colliding with person-record discovery.** The earlier draft placed the per-person index at `examples/v0/example-persons/<person_id>.events.json` (flat sidecar). This collides with `validate_design_bundle.clj:365`, which lists every immediate `*.json` under `examples/v0/example-persons` and treats each as a person record — the flat sidecar would be parsed as a malformed person record and fail discovery before drift validation runs. The corrected layout places indexes at `examples/v0/example-persons/_indexes/<person_id>.json` (parallel to `_events/`); both subdirectories are skipped by `.listFiles` at the immediate level. Context point 1, Decision Step 3 file-layout + file-format clauses, integrity rule 2, and the Task 5 review handoff are all updated. No change to `validate_design_bundle.clj:365` itself is needed (preserves Phase 3 simplicity); the discovery filter is implicitly relied upon to skip subdirectories, which is the existing behaviour.

**29. JSON `prov` block is the minimal contract; the remaining canonical PROV predicates are derived in `event->graph`.** The canonical graph diagram (Phase 1 Task 1 Step 3 / ADR 0020 Decision) names six predicates; the JSON contract carries only three structural keys (`used`, `was_generated_by`, `qualified_association`) mapping to RDF `prov:used`, `prov:wasGeneratedBy`, and `prov:qualifiedAssociation`. ADR 0021 Decision Step 3's "JSON `prov` block — minimal contract" clause commits to the JSON object shape and to the derivation rules for the other three RDF predicates: `prov:wasInvalidatedBy` from each `prov:used`, `prov:wasAssociatedWith` from `qualified_association.agent`, and `prov:wasDerivedFrom` from each (post-snapshot, pre-snapshot) pair. Phase 3 task 5 makes the emitter responsible for these derivations and adds unit tests for them. SHACL-only negative fixtures targeting the derived predicates (e.g. missing `prov:wasDerivedFrom` on a generated post-Entity) are explicitly hand-crafted TTL inputs — the JSON cannot express a missing derivation because it has no field for the predicate. Resolves the earlier ambiguity where the canonical graph and the JSON-field-naming clause described disjoint predicate sets without saying which side of the contract was authoritative.

**30. Participant snapshot binding is now event-content-derived, not current-record-derived.** Position L depends on historical drift events staying bound to the person snapshot they observed. The ADRs and Phase 3 plan now require `participants[]` objects with `snapshot_id`, `person_id`, and `person_record_hash`; `event->graph` resolves `prov.used` / `prov.was_generated_by` through `snapshot_id` and derives the snapshot IRI short hash from the embedded `person_record_hash`. It must not read the current person record to derive snapshot hashes. A regression test is required so a later edit to a person record cannot rebind an existing drift event.

**31. Identifier sizing, `abc:DriftEditor`, and identifier cardinality are ADR-level commitments.** The ABC-local ID regex is widened to `abc-[0-9a-f]{12}` with a collision-sizing rationale. `abc:DriftEditor` is listed as a new ABC vocabulary term because it appears in the canonical PROV graph as `prov:hadRole abc:DriftEditor`. `PersonRecordShape` widening preserves the existing `sh:minCount 1` and `sh:maxCount 1` cardinality while changing only the datatype/pattern branch, so mixed numeric/string identifiers do not permit two identifiers on one person node.

**32. JSON graph-coherence checks are validator responsibilities, not JSON Schema wishful thinking.** JSON Schema 2020-12 cannot express "every value in `prov.used` resolves to some `participants[].snapshot_id`" or "every participant is covered exactly once." ADR 0021 now splits this correctly: the event schema handles local shape plus split/merge array cardinalities; `validate-drift-events!` handles snapshot reference resolution, exact coverage, disjoint predecessor/successor membership, `snapshot_id` uniqueness, prefix/usage consistency, canonical ordering, `had_role` CURIE-prefix resolution, the closed `had_role = abc:DriftEditor` enum, and `agent` IRI syntax. The negative fixture list has a separate JSON graph-coherence category so these failures do not get mislabeled as SHACL or JSON Schema failures.

**33. ADR 0021 validation order mirrors the implementation plan.** JSON graph-coherence now runs immediately after JSON Schema validation and before `event->graph`, because the emitter resolves snapshot references and should not be responsible for recovering from dangling JSON IDs. RDF typing-coherence and participant/PROV cross-checks run after `event->graph`, then SHACL runs, then index validation and referential integrity. The earlier ADR step list grouped graph-coherence after RDF emission, which conflicted with the Phase 3 task order and could make malformed JSON fail as emitter crashes rather than distinct validation failures.
