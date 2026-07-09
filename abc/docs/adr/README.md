# Architecture Decision Records (ABC)

This directory holds the ABC-side ADRs for the Soranoha monorepo.
`ab-validator` owns parser/adaptor measurement and corpus reports; ABC
owns publication schemas, TEI profile policy, manifest identity, and
registry admission (see the root `AGENTS.md` design boundaries).

## File naming

`NNNN-kebab-case-title.md`, zero-padded four-digit sequence, single file per
ADR. ADRs are append-only history: supersede, do not edit a prior ADR's
Decision out from under implemented code without recording the change as a
new `Supersedes` ADR.

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

## Status vocabulary

This repository uses a small, fixed ADR status vocabulary. `Status:` is a
single term; do not invent new statuses without documenting them here.

| Status | Meaning |
| --- | --- |
| `Draft` | Design recorded but not yet accepted as the canonical contract. Implementation may exist speculatively, but the ADR's gates are not yet treated as binding release boundaries. |
| `Proposed` | Design is accepted-in-principle as the target contract and is under active, partial implementation (often as prototypes or provisional fixtures), but it is **not yet** an Accepted canonical contract: the ADR explicitly names what remains before it can be promoted to `Accepted` (e.g. "must not claim canonical ... until ... implemented"). Consumers may build against a `Proposed` contract at their own risk. |
| `Accepted` | The canonical contract for its scope. Implementation Status records acceptance, and the Acceptance Criteria are binding. Changes go through a new `Supersedes`/`Amends` ADR, not silent edits. |
| `Superseded` | Replaced by a later ADR named in a `Supersedes:` field on the replacement. The body is retained unchanged as history. |

### `Draft` vs `Proposed`

Both mean "not yet `Accepted`". `Draft` signals the design is still being
shaped and is not the target contract anyone should build to yet. `Proposed`
signals the design is the agreed target and implementation is in progress, but
the gates that make it canonical (e.g. remaining materialization, validation,
or publication acceptance criteria) are not yet met. Don't use `Proposed` for a
design that is still contested or `Draft` for one that code is already being
written against as the intended contract.

## Acceptance Criteria gate

`nix/check-acceptance-criteria.sh` requires that every ADR with an
`## Acceptance Criteria` section name an executable condemnation: a `fixtures/`
reference, a `test/...` path, or a committed Prolog fact/query under
`fixtures/v0/facts/prolog/`. The gate is status-independent: a `Draft` ADR with
an Acceptance Criteria section is held to the same rule. Pre-existing ADRs listed
in `.acceptance-legacy-allowlist` are exempt because their Acceptance Criteria
pre-date this gate. New/changed ADRs are ratcheted: cite a real test or fixture
path, not just a prose statement.
