# 0003: snh-manifest/3: the upstream range a release covers, and a slot for annotation layers

Status: accepted, 2026-09-10. Owner decisions, taken pre-genesis. No chain
carries `snh-manifest/2`, so this replaces it rather than succeeding it: there
are no manifests at wire version 2 to keep readable, and a verifier written
against this specification reads one version.

## Context

Two decisions landed together, and the manifest schema is closed with
`additionalProperties: false`. A field absent at genesis needs a new wire
version and every verifier of the day, or a new chain. That asymmetry is why
both are taken now and why they share one bump.

**The upstream range.** The intended operating model publishes on each upstream
commit. Measured against the aozorabunko checkout, 5,476 commits since 2011 and
about 800 of them change no work archive. Each would still mint a release,
because `corpus.upstream_rev` is the exact commit and `corpus` is a projection
key, so a release that differs from its parent in a 40-hex string and nothing
else carries a 9.1 MB manifest and a 14.45 MB catalog. The owner decided to
publish only when the corpus moved. That leaves a question the chain could not
answer: which upstream revision does a given release stand for, when it stands
for more than one.

**The annotation layer slot.** Analysis layers have a complete identity model
in `soranoha-annotation-layer/1` and no publication path, because
`works[].artifacts` is a fixed four-tuple pinned positionally by the schema.

## Decision

Wire version 3, with two structural changes and nothing else. Neither is a
judgement; both are shapes.

**`corpus` gains `covers_from`.** It is the predecessor release's
`upstream_rev`, or `null` at genesis, and the range a release covers is
`(covers_from, upstream_rev]` in upstream history. A verifier checks it against
the predecessor manifest it already reads, so the field cannot drift from the
chain: it restates for a detached reader what the chain states by adjacency,
the same reason `prev_manifest` is in the manifest at all.

**`works[]` gains `layers`.** An array, required and sorted by `id`, of
`{id, bytes}` naming `snh:1:annotation-layer:<hex>` blobs, verified exactly as
the four artifacts are. Every genesis-era entry is `[]`. The four-tuple keeps
its fixed shape and its positional pinning.

## What does not change

`upstream_rev` stays in the projection. It is there so that a corpus change is
a projection change rather than a determinism halt, and removing it would turn
every ordinary release into `:determinism-halt`: same projection, different
derived content. Skipping a commit that changed no work archive is therefore
decided before assembly, by the release entry point comparing the upstream
range against the head's `upstream_rev`, and never by the transaction. The §9
no-op rule is untouched and still governs a rerun at the same revision.

Nothing about verification's trust model changes. `covers_from` is checked
against bytes the verifier already has; `layers` entries are checked as blobs
already are.

## Alternatives rejected

- **Derive the range from the chain and add no field.** The predecessor's
  `upstream_rev` is the range start, so a chain reader needs nothing. Rejected
  because a detached manifest is a thing readers hold: `rights.statement_url`
  and the citation recipe both assume a manifest travels alone, and a range
  recoverable only by fetching the chain is not carried by the bytes that make
  the claim.
- **Skip the empty commits and record nothing.** Cheapest, and it leaves about
  800 upstream revisions that no release maps to. A reader asking what the
  corpus looked like at one of them gets a gap rather than an answer.
- **Add the annotation slot after genesis.** One bump every verifier must
  handle, against one taken while no verifier exists. The bump is happening for
  `covers_from` regardless, which is what makes the reservation nearly free.
- **Reserve the slot inside `artifacts`.** Breaks a fixed four-tuple that the
  schema pins positionally and that §3 states as a contract, to hold a
  variable-length list. A sibling array says what it is.
