# Data-Driven Decision Records (`decisions.edn`)

**Date:** 2026-07-24 · **Status:** Draft (for review)

## Goal

Replace the ADR corpus's machine-facing Markdown grammar with a single EDN
corpus file, and retire every piece of machinery that existed only to recover
data from prose. The refactor is subtractive: the new representation is
accepted only because it deletes more than it adds. When it lands, the old way
is gone — no dual formats, no compatibility parser, no transition period.

Success criteria:

- One authoritative data representation (`docs/adr/decisions.edn`) for all
  machine-facing decision facts; Markdown files are pure narrative that no
  tool parses.
- The header grammar, claim-header grammar, reciprocity model,
  `adr-relations.edn` split, and tombstone-file convention are removed
  entirely, including their tests and invalid fixtures.
- Organization comes from queryable attributes (`:topics`, `:status`,
  relations) rendered as derived views, not from sequence numbers.
- Net LOC in `abc.tools.adr*` drops substantially (~600 → ~200 target for
  `adr.clj`; fixture corpus shrinks proportionally).

## Non-goals

- No change to decision *content*: prose sections keep their meaning and their
  edit-policy discipline.
- No closed vocabularies for `:topics` or claim `:kind` (ADR 0043 lesson —
  descriptive, not machine-enforced).
- No per-claim result storage, coverage registry, or evidence execution
  bookkeeping (would recreate what ADR 0043 deleted).
- No relocation of the README's domain content (manifest-identity invariants,
  open questions); only its grammar documentation is replaced.
- `ab-validator`'s reports and boundaries are untouched.

## 1. Data model

`abc/docs/adr/decisions.edn` holds one record per decision:

```clojure
{:decisions
 [{:slug              "subtractive-evidence-simplification"
   :legacy-number     43            ; historical resolution only; never minted again
   :title             "Subtractive Evidence Simplification"
   :status            :accepted     ; :draft :proposed :accepted :superseded :withdrawn
   :date              "2026-07-23"
   :accepted          "2026-07-23"  ; required iff :accepted; permitted on :superseded
   :validation-scope  :structural   ; required iff :accepted; permitted on :superseded
   :release-authority :none         ; required iff :accepted; permitted on :superseded
   :source            "docs/superpowers/specs/2026-07-23-adr-evidence-apparatus-decomplection-design.md"
   :topics            [:governance :evidence]
   :relations         [{:class :lifecycle :type :supersedes
                        :to "typed-evidence-and-lifecycle-closure"}
                       {:class :lifecycle :type :amends
                        :to "adr-governance-validation"
                        :scope "typed evidence registry"}
                       {:class :lifecycle :type :depends-on
                        :to "adr-governance-validation"}
                       ;; annotation edge shown for illustration (belongs to
                       ;; other records in the real corpus):
                       {:class :annotation :type :harness-for
                        :to "person-identity-drift-data-model"
                        :note "source-only human rationale, preserved from adr-relations.edn"}]
   :claims            [{:id :c1
                        :kind :structural-invariant
                        :statement "Accepted criteria citing missing paths under the test, fixtures, nix, or docs/evidence/external roots are rejected by ADR validation."
                        :evidence ["test/abc/tools/adr_test.clj"]}]}]}
```

Decisions embodied in the model:

- **Identity is the slug.** kebab-case, unique, stable. `:legacy-number`
  exists solely so historical `ADR NNNN` / `ADR-NNNN-CN` references in frozen
  prose, reports, and git history remain resolvable. The schema closes the
  set: allowed values are exactly the frozen migration mapping (the 42 legacy
  numbers, embedded as a literal set in the schema), each unique. "Never
  minted again" is thereby machine-enforced, not a promise.
- **Edges are stated once, on the acting record.** `superseded-by` /
  `amended-by` are derived for display, never authored. Reciprocity ceases to
  be a concept; there is nothing to check.
- **Relations are an explicit sum, discriminated by `:class`.** A relation is
  either `{:class :lifecycle :type <closed enum: :supersedes | :amends |
  :depends-on> …}` or `{:class :annotation :type <open keyword> …}`. The
  discriminator makes typos fail closed: `:depend-on` or `:supercedes` under
  `:class :lifecycle` is a schema error, never a silently accepted
  annotation. Lifecycle edges carry validation semantics (supersession
  status, dependency closure) and must be irreflexive and acyclic per type —
  no self-edges, no `:depends-on` cycles, no cycles of unscoped supersession,
  and no `:amends` cycles (amendment loops have no meaningful reading; a
  mutual-amendment need is a sign the two decisions should be superseded by
  one successor). Annotation edges (`:restates-hard-rule`,
  `:schema-hash-cascade`, `:harness-for`, `:extends`, future descriptive
  types) have no lifecycle semantics, may carry a source-only `:note`
  (preserved verbatim from `adr-relations.edn`), and are rendered in derived
  views. `:scope` is an optional free-text string on `:supersedes`/`:amends`;
  an unscoped supersession replaces the whole target decision (unchanged
  rule).
- **Claims are local data, lifecycle-aware.** `:id` is `:c1`, `:c2`, … within
  the record; global uniqueness follows from slug uniqueness, so the
  repository-wide claim-ID scan is deleted. `:statement` is the exact
  criterion text (the only transformation ever applied is stripping the old
  `**ADR-NNNN-CN — kind:**` prefix). `:kind` (descriptive keyword) and
  `:evidence` (repository paths under `test/`, `fixtures/`, `nix/`,
  `docs/evidence/external/`) are **required for `:accepted` and
  `:superseded` records, optional for `:draft`/`:proposed`/`:withdrawn`** —
  the corpus has 57 Draft/Proposed criteria with neither, and they are
  promotion conditions, not defects. Migration assigns deterministic `:cN`
  ids (document order) to headerless criteria. Citation form going forward
  is `slug#c1`.
- **Lifecycle fields honor history.** `:accepted`, `:validation-scope`, and
  `:release-authority` are required iff `:status :accepted`, and additionally
  *permitted* on `:superseded` records — supersession does not erase the
  record's historical lifecycle facts (ADR 0034 keeps its `full-corpus` scope
  and `none` authority; its acceptance date, which the old grammar forced out
  of the header, stays absent unless a future edit restores it from prose).
  They are absent on `:draft`/`:proposed`/`:withdrawn`.
- **Withdrawn records are ordinary data.** ADR 0019 becomes a `:withdrawn`
  record whose narrative file carries the reason. Sequence gaps cannot exist,
  so the tombstone convention and gap-explanation rule are deleted.

## 2. Narrative files

Each record owns `docs/adr/<slug>.md`: a `# Title` heading and prose sections
(Context, Decision, Consequences, Implementation Status, Rollback, …). All 42
legacy files are `git mv`'d to slug names and stripped of header blocks and
claim-header bullets. Acceptance Criteria sections move wholly into `:claims`
— retaining them in prose would be dual bookkeeping.

No tool parses narrative files. Governance checks only that each record's
`<slug>.md` exists and that no orphan `.md` (other than `README.md` and the
generated `INDEX.md`) sits in the directory. Required-section enforcement for Accepted records is dropped;
the section canon becomes README authoring guidance. Rationale: the sections
that mattered to machines (header, criteria) no longer live there, and a
grep-shaped section check on unparsed prose is exactly the residue this
refactor retires.

The edit policy survives as human policy, restated over the split:

| Where | Mutable? |
| --- | --- |
| Prose Decision / Context | No — supersede or amend via a new record. |
| Prose Consequences | Append-only. |
| Prose Implementation Status | Yes — dated entries. |
| `decisions.edn` `:status`, `:accepted` | Yes — lifecycle transitions. |
| `decisions.edn` `:claims`, `:relations` | Claims append-only for Accepted records; never weaken without superseding. Relations append-only. |
| `decisions.edn` `:topics` | Yes — descriptive, freely improvable. |

## 3. Validation

`abc.tools.adr` is rewritten (not incrementally edited) as a data validator:

- **Loading boundary:** malformed input is inside the CLI contract, not an
  exception path. A strict corpus loader converts a missing file, a
  non-file path, an EDN reader error, or anything other than exactly one
  EDN form followed by EOF into stable `:invalid-edn` problem maps (the
  shared `files/read-edn` is a bare `edn/read-string` that throws on
  malformed input and silently ignores trailing forms — it is not used
  here). A truncated or junk-appended `decisions.edn` yields a problem
  report and nonzero exit, never a stack trace or silent acceptance.
- **Shape:** a Malli schema owned by the `abc.tools.adr` namespace, composed
  with `abc.tools.malli` scalar schemas per the Change E registry convention
  (domain namespace owns its schemas and registry; no central registry).
  Covers enums, ISO dates, slug grammar, uniqueness of `:slug` and
  `:legacy-number`, the frozen legacy-number set, the `:class`-discriminated
  relation sum (closed lifecycle enum — unknown lifecycle types and unknown
  `:class` values are schema errors), claim-id shape and per-record
  uniqueness, and the lifecycle-conditional requirements (`:accepted`
  date/scope/authority required iff `:accepted`, permitted on `:superseded`;
  claim `:kind`/`:evidence` required for `:accepted`/`:superseded` records).
- **Semantics** (plain queries over the corpus value):
  - relation targets resolve to existing slugs; no duplicate edges; no
    self-edges on lifecycle relations
  - `:depends-on`, unscoped `:supersedes`, and `:amends` graphs are acyclic
  - unscoped supersession target has `:superseded` status; every
    `:superseded` record has an incoming unscoped `:supersedes` edge
  - accepted-date not before `:date`
  - dependency closure of an Accepted record contains only Accepted records
  - Accepted records have ≥1 claim; every Accepted claim cites ≥1 evidence
    path; evidence paths exist, resolve inside the repository
    (path-containment logic reused as-is), and a directory evidence path
    requires an existing `test/` or `nix/` file in the same claim
  - every record has `docs/adr/<slug>.md`; no orphan Markdown files
- `abc.tools.adr-governance` keeps its CLI contract: nonzero exit on any
  problem, same problem-map reporting shape.

Deleted outright, with their tests and fixtures: title/filename parsing,
header-line grammar, blank-line rule, relation-string parsing, reciprocity
checking, claim-header regex and placement rules, repository-wide claim-ID
uniqueness scan, backtick evidence scanning, section parsing, required-section
checks, `adr-relations.edn` and its type-ownership rule.

## 4. Derived views

- `adr-graph.mmd` regenerates from `decisions.edn`;
  `abc.tools.diagram.adr-graph` consumes the corpus value directly. Byte
  currency gating is unchanged (ADR 0029 contract).
- A generated `docs/adr/INDEX.md` (gated for byte currency like the Mermaid files)
  renders the organizational views: records by topic, by status, supersession
  chains with derived `superseded-by`, and a legacy-number → slug table.
- `README.md` loses the file-naming rules, header-field grammar, claim-header
  grammar, and relation-syntax documentation, replaced by a short description
  of `decisions.edn` and a pointer to the Malli schema. Status-vocabulary
  semantics, Draft-vs-Proposed guidance, the edit policy, manifest-identity
  invariants, and known open questions remain as prose.

## 5. Migration (one shot, then the ladder is discarded)

1. **Extract:** a throwaway script runs the existing
   `abc.tools.adr/parse-all` plus `adr-relations.edn` to emit `decisions.edn`
   (slug from filename, `:legacy-number` from the sequence number, claims
   from parsed criteria with statements carried verbatim except for
   stripping the claim-header prefix, deterministic `:cN` ids for headerless
   criteria, one-directional relations from the
   `supersedes`/`amends`/`depends-on` sides only — today's enforced
   reciprocity guarantees the acting side is always present — and annotation
   edges with their `:note` values carried verbatim from
   `adr-relations.edn`). Human review of the emitted file; the script lives
   in the branch and is deleted before merge.
2. **Verify equivalence over complete fact maps:** a migration test asserts
   the old representation's *entire* fact set ≡ the committed
   `decisions.edn`: title, status, both dates, validation scope, release
   authority, raw `Source` line, topics excepted (new, additive), relations
   including scopes and annotation `:note`s, and exact criterion text minus
   only the claim-header prefix, plus claim ids/kinds/evidence. The only
   permitted normalizations are the deliberate ones: number→slug, derived
   inverse-edge removal, `:cN` assignment for headerless criteria, and
   renamed narrative paths. Anything else differing fails the test — the
   extractor cannot truncate or rewrite decision content. This test exists
   only on the migration branch and is deleted together with the old parser
   in the same change.
3. **Strip and rename:** remove header blocks and Acceptance Criteria
   sections from all `.md` files; `git mv` each to `<slug>.md`; delete
   `adr-relations.edn`; convert the 0019 tombstone.
4. **Rewrite consumers:** `adr.clj`, `adr_governance.clj`, `adr_graph.clj`,
   their tests; replace `fixtures/adr-governance-invalid` with EDN invalid
   fixtures. Sweep remaining path/name consumers:
   `nix/adr-family-clean.jq`,
   `data/evidence-higher-order-calls/adr-validate-repository-star.edn`,
   `schemas/adr-external-evidence.schema.json`, and `docs/adr/NNNN-…` path
   references anywhere in `src/`, `test/`, `nix/`, `flake.nix`. Historical
   `ADR NNNN` mentions in frozen prose and reports stay untouched.
5. **Migrate ADR identity in architecture metadata to slugs.** Integer ADR
   references are an active machine identity today, not just prose:
   `docs/architecture-stages.edn` coordinate owners and stage `:adrs`
   vectors, `docs/architecture-presentation.edn` backing, and their
   validators (`presentation_figures.clj` requires integer `:adrs`;
   `architecture_graph.clj` cross-checks owners against parsed ADRs). All of
   these move to slugs and resolve against `decisions.edn`, so the first
   slug-native record can own a coordinate and `:legacy-number` stays
   historical-resolution-only. Audit *active* documentation
   (`docs/v0-design-bundle/README.md`, `docs/architecture.md`, and any other
   non-frozen doc) plus extracted `:source` values for `docs/adr/NNNN-…`
   links broken by the rename, repository-wide — not just `abc/`.
6. **Record the decision:** the refactor lands with a new decision record —
   the first slug-native one — superseding ADR 0031's header-grammar scope
   and amending ADR 0029's graph-source contract, with claims backed by the
   rewritten `adr_test.clj` / `adr_governance_test.clj`.

**Rollback:** before merge, abort the migration branch — nothing on `main`
changes. After merge, roll back by reverting the whole atomic change (the
migration lands as one revertable unit). There is no partial fallback and no
dual-format mode in either direction; total retirement is preserved.

Retirement checklist (all must be gone at merge): header grammar and its
parser; claim-header grammar; reciprocal relation fields; repository claim-ID
scan; `adr-relations.edn`; tombstone/gap convention; number-prefixed
filenames; Markdown invalid-fixture corpus; migration script and equivalence
test; every README sentence documenting the retired grammar.

## 6. Testing

TDD throughout the rewrite:

- Loading boundary: fixtures for a missing file, a directory path, malformed
  EDN (truncation, bad delimiter), and a trailing second form — each must
  produce an `:invalid-edn` problem map and nonzero exit, never a stack
  trace.
- Malli schema: one failing EDN fixture per shape rule (bad enum, bad date,
  duplicate slug, duplicate legacy number, legacy number outside the frozen
  set, claim without evidence on Accepted, and the fail-closed relation
  cases: `:class :lifecycle` with a misspelled type such as `:depend-on` or
  `:supercedes`, and an unknown `:class`).
- Semantic checks: one failing EDN fixture per rule (dangling relation
  target, lifecycle self-edge, `:depends-on` cycle, unscoped-supersession
  cycle, `:amends` cycle, unscoped supersession of a non-superseded record,
  non-Accepted dependency in an Accepted closure, missing narrative file,
  orphan Markdown, missing/escaping evidence path, unverified evidence
  directory).
- Graph/index generators: golden-output tests over a small corpus fixture;
  byte-currency gates unchanged.
- Migration branch only: the old↔new equivalence test (deleted at merge).
- The standing governance gate (`clojure -M:abc/adr-governance`, Nix check)
  passes over the migrated corpus.

## Resolved design questions (from brainstorming)

- Data home: single corpus EDN file (not frontmatter, not sidecars) — every
  consumer queries the whole graph; reciprocity and the relations side-file
  dissolve only in this shape.
- Identity: slugs primary; numbers retired to `:legacy-number`, closed to
  the frozen migration set, for historical resolution only.
- Claims: fully in data, including statements; no prose mirror; `:kind` and
  `:evidence` become mandatory at acceptance, not before.
- Relations: a `:class`-discriminated sum so lifecycle typos fail closed;
  lifecycle graphs irreflexive and acyclic.
- Migration: full mechanical one-shot verified by complete-fact-map
  equivalence; architecture metadata moves to slug identity in the same
  change; no frozen legacy format, no parallel parsers after merge.
