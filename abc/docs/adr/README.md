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

Status: <one of the statuses below>
Date: YYYY-MM-DD
Accepted: YYYY-MM-DD          # present once Accepted; omitted while Draft/Proposed
Supersedes: none | ADR NNNN
Amends: ADR NNNN, ...          # optional; present when the ADR modifies a prior ADR's contract
Depends on: ADR NNNN, ...      # optional
Source: `path/to/source.md`    # optional
```

When an ADR amends a prior one, the prior ADR's header SHOULD also record the
reverse link for navigability:

```markdown
Amended by: ADR NNNN, ...      # optional; added to the amended ADR when a later ADR Amends it
```

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
or publication acceptance criteria) are not yet met. Don't use `Proposed` for a
design that is still contested or `Draft` for one that code is already being
written against as the intended contract.

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

## Manifest identity invariants

Two global invariants apply to `manifest_identity_object` across every ADR
that touches it (ADR 0001, 0010, 0023, 0026, 0027):

1. **`null` means "not applicable", not "unknown".** A coordinate is `null` in
   `manifest_identity_object` when it is not part of that artifact kind's
   derivation contract. If a required coordinate cannot be determined, the
   producer MUST emit a failed manifest or no manifest — never a successful
   manifest with `null` standing for an unknown value.
2. **`artifact_id` is never nested.** `artifact_id` is the hash of
   `manifest_identity_object`; including it inside the object would make
   identity circular.

The current field set of `manifest_identity_object` is amended across multiple
ADRs (0001 → 0010 → 0023 → 0027). The authoritative current field list is the
bundled `schemas/manifest.schema.json`, not any single ADR's prose; ADR prose
records the *rule* for each field's introduction and nullability semantics.

## Acceptance Criteria gate

`nix/check-acceptance-criteria.sh` requires that every ADR with an
`## Acceptance Criteria` section name an executable check: a `fixtures/`
reference (typically a negative fixture that the harness proves is rejected), a
`test/...` path, or a committed Prolog fact/query under
`fixtures/v0/facts/prolog/`. The gate is status-independent: a `Draft` ADR with
an Acceptance Criteria section is held to the same rule. Pre-existing ADRs
listed in `.acceptance-legacy-allowlist` are exempt because their Acceptance
Criteria pre-date this gate. New/changed ADRs are ratcheted: cite a real test or
fixture path, not just a prose statement.

An Accepted ADR without an Acceptance Criteria section is allowed but weaker:
it has no executable gate readers can point at. New Accepted ADRs SHOULD include
an Acceptance Criteria section with at least one `test/` or `fixtures/`
reference.
