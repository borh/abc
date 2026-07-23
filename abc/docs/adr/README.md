# Architecture Decision Records (ABC)

This directory holds the ABC-side ADRs for the Soranoha monorepo.
`ab-validator` owns parser/adaptor measurement and corpus reports; ABC
owns publication schemas, TEI profile policy, manifest identity, and
registry admission (see the root `AGENTS.md` design boundaries).

## File naming

`NNNN-kebab-case-title.md`, zero-padded four-digit sequence, single file per
ADR. Gaps in the sequence are allowed but must be explained by a tombstone
file (see ADR 0019 for an example). ADRs are append-only decision history; see
"Edit policy" below for which sections may change in place.

## Header fields

Each ADR begins with:

```markdown
# ADR NNNN: Title

Status: <Draft | Proposed | Accepted | Superseded | Withdrawn>
Date: YYYY-MM-DD
Accepted: YYYY-MM-DD
Supersedes: none | ADR NNNN [scope: non-empty text], ADR MMMM
Superseded by: ADR NNNN [scope: non-empty text], ADR MMMM
Amends: ADR NNNN [scope: non-empty text], ADR MMMM
Amended by: ADR NNNN [scope: non-empty text], ADR MMMM
Depends on: ADR NNNN [scope: non-empty text], ADR MMMM
Source: <one physical line>
```

Header fields are closed and single-line: each physical line is one recognized
field with a non-empty value, and wrapped continuations are invalid. `Accepted:`
is required exactly when `Status: Accepted`. Relation values contain only ADR
references separated by commas; optional scopes use the exact bracketed form
`[scope: non-empty text]`. `Supersedes: none` is the only non-reference relation
value. `Source:` may contain prose and code-spanned paths, but stays on one
physical line.

`Amends`/`Amended by` and `Supersedes`/`Superseded by` links are reciprocal,
including matching scopes. An unscoped supersession replaces the whole target
decision and requires the target status to be `Superseded`; a scoped
supersession leaves the target's other accepted scopes active.

## Status vocabulary

This repository uses a small, fixed ADR status vocabulary. `Status:` is a
single term; do not invent new statuses without documenting them here.

| Status | Meaning |
| --- | --- |
| `Draft` | Design recorded but not yet accepted as the canonical contract. Implementation may exist speculatively, but the ADR's gates are not yet treated as binding release boundaries. |
| `Proposed` | Design is accepted-in-principle as the target contract and is under active, partial implementation (often as prototypes or provisional fixtures), but it is **not yet** an Accepted canonical contract: the ADR explicitly names what remains before it can be promoted to `Accepted` (e.g. "must not claim canonical ... until ... implemented"). Consumers may build against a `Proposed` contract at their own risk. |
| `Accepted` | The canonical contract for its scope. Implementation Status records acceptance, and the Acceptance Criteria are binding. Changes go through a new `Supersedes`/`Amends` ADR, not silent edits. |
| `Superseded` | Replaced by a later ADR named in a `Supersedes:` field on the replacement. The body is retained unchanged as history. |
| `Withdrawn` | Reserved or retracted before acceptance. The tombstone records the reason and points at any successor (ADR 0019 is the current example). |

### `Draft` vs `Proposed`

Both mean "not yet `Accepted`". `Draft` signals the design is still being
shaped and is not the target contract anyone should build to yet. `Proposed`
signals the design is the agreed target and implementation is in progress, but
the gates that make it canonical (e.g. remaining materialization, validation,
or publication acceptance criteria) are not yet met. For both statuses,
Acceptance Criteria are promotion conditions rather than claims of current
completion. Don't use `Proposed` for a design that is still contested or
`Draft` for one that code is already being written against as the intended
contract.

A `Proposed` ADR MUST use target/proposal language ("this ADR proposes …",
"the target contract is …", "the accepted slice will be …") rather than
acceptance language ("this ADR accepts …"). "Accepts" is reserved for
`Accepted` ADRs.

## Edit policy

Sections are not all equally mutable. The rule below keeps decision history
trustworthy while allowing implementation tracking to stay current.

| Section | Mutable in place? | Rule |
| --- | --- | --- |
| Decision | No | Supersede or amend via a new ADR. |
| Hard Rule | No | Supersede or amend via a new ADR. |
| Acceptance Criteria | Append-only | May add criteria; never weaken an existing criterion without superseding. |
| Consequences | Append-only | Document new consequences; do not rewrite historical ones. |
| Implementation Status | Yes | Dated entries allowed; reflects current code reality, not the decision. |
| References | Yes | Append only. |
| Errata | Yes | Must name the affected ADR and what was wrong. |

### Typed Acceptance Criterion headers

Every Acceptance Criterion in an Accepted ADR begins with an exact stable
claim header:

```markdown
- **ADR-0042-C1 — structural-invariant:** Claim text and evidence citations.
```

The claim ID is `ADR-NNNN-CN`, where the four-digit ADR number matches the
containing record and `N` begins at 1. Claim IDs are unique across the complete
repository. The kind token is lowercase kebab-case prose (for example
`structural-invariant` or `fixture-behavior`); per ADR 0043 it is descriptive
only — no closed vocabulary is machine-enforced.

Proposed ADRs may omit claim headers while the proposal is being shaped. If a
Proposed criterion starts with `**ADR-`, however, the complete header must
already use the exact grammar above. Promotion to Accepted requires an
existing executable evidence citation for the ADR (ADR 0043); whether a cited
test entails its criterion is a review judgment.

## Manifest identity invariants

Three global invariants apply to `manifest_identity_object` across every ADR
that touches it (ADR 0001, 0010, 0023, 0026, 0027, 0028):

1. **`null` means "not applicable", not "unknown".** A coordinate is `null` in
   `manifest_identity_object` when it is not part of that artifact kind's
   derivation contract. If a required coordinate cannot be determined, the
   producer MUST emit a failed manifest or no manifest — never a successful
   manifest with `null` standing for an unknown value. (Note: ADR 0015
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
   restated in ADR 0013, 0014, 0015, 0016, 0017, 0018, 0020, and 0021; it is
   recorded once here as an invariant rather than re-decided per ADR.

Across schema-version rotations of the identity fields themselves (ADR 0015 →
0016 → 0017/0018 → 0020), older manifests retain their original
`manifest_schema_hash` and are not reinterpreted under the new rule. This keeps
historical manifests valid but means the same underlying facts can accumulate
distinct `artifact_id` values across schema generations; an equivalence /
migration story (e.g. a non-identity `supersedes_artifact_id` provenance edge)
linking generations is not part of v0 identity and is recorded below as a
known open question for longitudinal analysis over a living corpus.

The current field set of `manifest_identity_object` is amended across multiple
ADRs (0001 → 0010 → 0023 → 0027 → 0028). ADR 0028 introduces
`annotation_policy_hash`. The authoritative current field list is the bundled
`schemas/manifest.schema.json`, not any single ADR's prose; ADR prose records
the *rule* for each field's introduction and nullability semantics.

## Acceptance Criteria gate

The Clojure governance command, `clojure -M:abc/adr-governance`, parses the
closed header grammar and validates the complete ADR corpus. It replaces the
shell allowlist ratchet: there are no permanent legacy evidence exceptions.

Every Accepted ADR requires `## Decision`, `## Implementation Status`, and
`## Acceptance Criteria`. Every Accepted criterion must attach at least one
existing evidence path (`test/...`, `nix/...`, a verified `fixtures/...`
reference, or a reviewed `docs/evidence/external/...` document) to the claim
it proves — governance rejects an Accepted criterion that cites nothing
(`:missing-criterion-evidence`). Draft and Proposed ADRs
may record criteria without executable evidence because those criteria are
promotion conditions; they become mandatory executable evidence before the ADR
is promoted to Accepted.

## Known open questions

These are design questions the ADR set has surfaced but not yet decided. They
are recorded here so they are not silently lost; resolving one means accepting
a new ADR (or amending an existing Decision via a new ADR), not editing this
list in place as a substitute for a decision.

- **Cross-implementation identity conformance.** ADR 0001 targets identity that
  works across Rust, Clojure/JVM, Python, JavaScript, RDF tooling, and Nix, but
  v0 verifies only the Clojure/JVM implementation. RFC 8785 JCS has real edge
  cases (number formatting, Unicode normalization) that are untested across
  languages. A cross-language conformance fixture is a prerequisite before any
  non-JVM implementation is relied on for `artifact_id`. See ADR 0001
  Implementation Status and Acceptance Criteria.
- **Cross-generation artifact equivalence.** Schema-hash cascade rotations
  (ADR 0015 → 0016 → 0017/0018 → 0020) intentionally do not reinterpret old
  manifests, so the same underlying facts accumulate distinct `artifact_id`
  values across schema generations. v0 has no equivalence or migration edge
  (e.g. a non-identity `supersedes_artifact_id` provenance link) connecting
  generations. This is acceptable for archival revalidation but tensions with
  the "living corpus" longitudinal-continuity goal. A future ADR should decide
  whether such an edge is identity-bearing, provenance-only, or out of scope.
- **Cross-view RDF harmonization is not structurally closed.** ADR 0017 fixed
  one instance where `abc:` resolved to two different IRIs between the Turtle
  manifest view and the JSON-LD / Linked Art view, but the harness still does
  not structurally assert that the two serializations describe the same entity
  IRIs and the same `artifactId`. The identity-invariant check only round-trips
  `artifactId` through JSON-LD expansion; it does not compare TTL and JSON-LD
  entity identity. A harness-level cross-view agreement check is an open item.
- **`Accepted` semantics are fixture-scope.** Most `Accepted` ADRs are gated by
  `validate-design-bundle` over repository-local fixtures (ADR 0006), and the
  example work (羅生門, text-only) is the single exercised fixture. IIIF
  `applicable` (ADR 0014), real split/merge person drift (ADR 0020–0022), and
  tokenized analysis slices (ADR 0026/0027) are either deferred or exercised
  only synthetically. `Accepted` therefore means "the contract is binding on
  the exercised fixture set", not "validated at corpus scale or on the design's
  hard cases."
- **ADR 0003 cost envelope is unmeasured.** ADR 0003's own acceptance criteria
  (real manifests, cold builds, incremental rebuilds, the recorded CI runner,
  <30s / <2GB) are unmet; only synthetic evaluator-only probes exist. ADRs 0026
  and 0027 both `Depend on` ADR 0003 while it remains `Draft`, so release-scale
  materialization must respect a not-yet-accepted envelope.
