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
   :accepted          "2026-07-23"  ; present iff :accepted status
   :validation-scope  :structural   ; required iff :accepted
   :release-authority :none         ; required iff :accepted
   :source            "docs/superpowers/specs/2026-07-23-adr-evidence-apparatus-decomplection-design.md"
   :topics            [:governance :evidence]
   :relations         [{:type :supersedes :to "typed-evidence-and-lifecycle-closure"}
                       {:type :amends :to "adr-governance-validation"
                        :scope "typed evidence registry"}
                       {:type :depends-on :to "adr-governance-validation"}]
   :claims            [{:id :c1
                        :kind :structural-invariant
                        :statement "Accepted criteria citing missing paths under the evidence roots are rejected by ADR validation."
                        :evidence ["test/abc/tools/adr_test.clj"]}]}]}
```

Decisions embodied in the model:

- **Identity is the slug.** kebab-case, unique, stable. `:legacy-number`
  (unique where present) exists solely so historical `ADR NNNN` /
  `ADR-NNNN-CN` references in frozen prose, reports, and git history remain
  resolvable. New records never get one.
- **Edges are stated once, on the acting record.** `superseded-by` /
  `amended-by` are derived for display, never authored. Reciprocity ceases to
  be a concept; there is nothing to check.
- **One `:relations` vector, two tiers of edge type.** Lifecycle types
  `:supersedes`, `:amends`, `:depends-on` carry validation semantics
  (supersession status, dependency closure). All other types
  (`:restates-hard-rule`, `:schema-hash-cascade`, `:harness-for`, `:extends`,
  and future descriptive edges) are open-vocabulary annotations rendered in
  derived views with no lifecycle semantics. `:scope` is an optional
  free-text string on `:supersedes`/`:amends`; an unscoped supersession
  replaces the whole target decision (unchanged rule).
- **Claims are local data.** `:id` is `:c1`, `:c2`, … within the record;
  global uniqueness follows from slug uniqueness, so the repository-wide
  claim-ID scan is deleted. `:kind` is a descriptive keyword. `:statement` is
  the criterion text. `:evidence` is a vector of repository paths under the
  existing evidence roots (`test/`, `fixtures/`, `nix/`,
  `docs/evidence/external/`). Citation form going forward is `slug#c1`.
- **Withdrawn records are ordinary data.** ADR 0019 becomes a `:withdrawn`
  record whose narrative file carries the reason. Sequence gaps cannot exist,
  so the tombstone convention and gap-explanation rule are deleted.

## 2. Narrative files

Each record owns `docs/adr/<slug>.md`: a `# Title` heading and prose sections
(Context, Decision, Consequences, Implementation Status, Rollback, …). All 43
existing files are `git mv`'d to slug names and stripped of header blocks and
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

- **Shape:** a Malli schema owned by the `abc.tools.adr` namespace, composed
  with `abc.tools.malli` scalar schemas per the Change E registry convention
  (domain namespace owns its schemas and registry; no central registry).
  Covers enums, ISO dates, slug grammar, uniqueness of `:slug` and
  `:legacy-number`, claim-id shape and per-record uniqueness,
  conditional requirements (`:accepted` date/scope/authority iff status
  `:accepted`).
- **Semantics** (plain queries over the corpus value):
  - relation targets resolve to existing slugs; no duplicate edges
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
   from parsed criteria with statements stripped of the claim-header prefix,
   one-directional relations from the `supersedes`/`amends`/`depends-on`
   sides only — today's enforced reciprocity guarantees the acting side is
   always present). Human review of the emitted file; the script lives in
   the branch and is deleted before merge.
2. **Verify equivalence:** a migration test asserts the old parser's fact set
   ≡ the committed `decisions.edn` (statuses, dates, relations with scopes,
   claim ids/kinds/evidence). This test exists only on the migration branch
   and is deleted together with the old parser in the same change.
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
5. **Record the decision:** the refactor lands with a new decision record —
   the first slug-native one — superseding ADR 0031's header-grammar scope
   and amending ADR 0029's graph-source contract, with claims backed by the
   rewritten `adr_test.clj` / `adr_governance_test.clj`.

Retirement checklist (all must be gone at merge): header grammar and its
parser; claim-header grammar; reciprocal relation fields; repository claim-ID
scan; `adr-relations.edn`; tombstone/gap convention; number-prefixed
filenames; Markdown invalid-fixture corpus; migration script and equivalence
test; every README sentence documenting the retired grammar.

## 6. Testing

TDD throughout the rewrite:

- Malli schema: one failing EDN fixture per shape rule (bad enum, bad date,
  duplicate slug, duplicate legacy number, claim without evidence on
  Accepted, …).
- Semantic checks: one failing EDN fixture per rule (dangling relation
  target, unscoped supersession of a non-superseded record, non-Accepted
  dependency in an Accepted closure, missing narrative file, orphan Markdown,
  missing/escaping evidence path, unverified evidence directory).
- Graph/index generators: golden-output tests over a small corpus fixture;
  byte-currency gates unchanged.
- Migration branch only: the old↔new equivalence test (deleted at merge).
- The standing governance gate (`clojure -M:abc/adr-governance`, Nix check)
  passes over the migrated corpus.

## Resolved design questions (from brainstorming)

- Data home: single corpus EDN file (not frontmatter, not sidecars) — every
  consumer queries the whole graph; reciprocity and the relations side-file
  dissolve only in this shape.
- Identity: slugs primary; numbers retired to `:legacy-number` for
  historical resolution only.
- Claims: fully in data, including statements; no prose mirror.
- Migration: full mechanical one-shot; no frozen legacy format, no parallel
  parsers after merge.
