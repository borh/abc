# 0002: Release identity, and making the reason a release changed introspectable

Status: proposed, 2026-09-10. Pre-genesis; no public chain exists yet, so
every option below is still available at the cost stated. After genesis the
manifest schema is closed and `additionalProperties` is `false`, so a field
absent at genesis needs `snh-manifest/4` and every verifier, or a new chain.

## Context

A release is named by the sha256 of its manifest and by nothing else. The
manifest has twelve required fields and no date, no ordinal, no label and no
status. `schema` versions the wire format rather than the corpus. Chain
position through `prev_manifest` is the only ordering.

Two consequences, both reported by the publication owner.

A reader cannot be told that the first public release is experimental. There is
no field for it, and the serving layer has nowhere to read one from.

A reader also cannot see why one release differs from the one before it. Three
distinct things cause a release to differ, and the site presents none of them:

1. the Aozora Bunko source moves, so a work's transcription changes;
2. Soranoha's own parser or renderer changes, so the same source produces
   different bytes;
3. a tokenizer, dictionary or other analysis layer is added or re-run.

ADR 0001 rejected a release ordinal, but inside the `catalog`, and for a reason
specific to it: content addressing lets consecutive releases with an unchanged
corpus share one catalog blob, which an embedded ordinal would defeat. That
reasoning does not reach the manifest, which is per-release by construction. A
release-level label in the manifest has not been considered before.

## What the published bytes already establish

The first two causes are already recorded, separately and precisely.

| Cause | What moves in the signed manifest |
|---|---|
| Aozora Bunko source moved | `corpus.upstream_rev`, and `works[].source_content_hash` for each work whose bytes changed |
| Parser or renderer changed | `toolchain.<stage>.stage_code_version` and `nix_closure_hash`; artifact ids change while `source_content_hash` does not |

Because both are in every manifest, and every manifest is in the chain, the
difference between any two releases is computable from published bytes alone.
No schema change is needed to derive it, and a third party can derive the same
answer without trusting the serving layer.

This distinction is the project's existing published vocabulary, not a new one.
The reader glossary defines `work`, `edition` and `document` as three separate
things: an edition is a specific source, identified by `source_content_hash`;
a document is a byte sequence, and "two releases of the same work from the same
edition may produce different documents as conversion tooling evolves". A
parser change therefore produces new documents of the same edition. Calling it
a new edition would contradict the vocabulary that published TEI already
carries, and `external-links.md` relies on the same split when it records a
link to a work and a link to an edition as claims that are never merged.

The third cause has an identity model and no publication path.
`soranoha-annotation-layer/1` records the view identity, a vocabulary content
hash, and producer input hashes covering the code, model, dictionary and
configuration actually used, along with execution status. It already settles
the reconciliation question this ADR was asked about: when source
correspondence changes but analyzer inputs do not, the same layer may be
attached to the new edition through its current TEI alignment. But
`works[].artifacts` is a fixed four-tuple (`minItems` 4, `maxItems` 4, `items`
false), so no layer can appear in a manifest.

One comparison already exists and is discarded. The `delta` subcommand
(`soranoha/src/soranoha/main.clj:310`) runs the three-set delta oracle over two
build run reports: it reports `source_delta` against `artifact_delta`, flags
executions that no source change explains, and refuses to compare runs whose
stage coordinates differ rather than attributing a toolchain change to the
source. It operates on disposable build reports rather than on published
manifests, and nothing it produces is published or served.

## Decisions required

### Where the maturity label and release name live

The owner's reading is that the first release is a `v0.1`, experimental, and
that the site and the Zenodo deposit should both say so.

- **Signed.** A `release` block in `snh-manifest/3` carrying a maturity value
  and a label. A detached TEI file, an archived deposit and a third-party
  mirror all carry it. Permanent and uncorrectable.
- **Serving and deposit only.** The site and the Zenodo record say
  experimental; no schema change; ships immediately. Anyone holding only the
  published bytes sees no signal.
- **Both**, with the serving layer rendering the signed value.

Recommendation: **serving and deposit only.** Every other field in the manifest
is a fact derived from the release's own inputs: a hash, a revision, a count. A
maturity label is an editorial judgement about the release rather than a fact
of it, and it is the one thing that can later be judged wrong. Signed bytes
cannot be corrected, so a release that ships as `v0.1` and proves sound remains
`v0.1` in permanent record, and one that ships unlabelled because the judgement
was unsettled at signing time cannot acquire the label later either.

The DOI precedent does not decide this. A DOI is outside the chain because
Zenodo mints it after the release exists; a label is known before signing and
could be signed. The argument here is corrigibility, not timing.

The transparency need behind the label is better served by publishing derived
facts, which cannot be wrong, than by a judgement that can, which is the
release history described below.

### Whether annotation layers get a manifest slot now

- **Reserve now**, by relaxing the four-tuple or adding a sibling `layers[]`
  block that is empty at genesis.
- **Keep the four-tuple closed**, and accept a later bump when layers land.

Recommendation: **reserve now.** Unlike the label, this is a structural reservation
rather than a judgement, so it cannot turn out to have been the wrong call
about a particular release. The layer identity model is already specified, the
publication owner intends to publish analysis layers, and the asymmetry is
large: reserving costs one pre-genesis bump that is being considered anyway,
while omitting it costs a post-genesis bump that every verifier must handle.

A reservation is not a commitment to publish layers at genesis. An empty
`layers[]` in every genesis work entry states that the release publishes none.

### What the site must show

Independent of both decisions above, and needing no schema change, because every input is
already in the chain.

A release history route, and per release:

- the head, abbreviated as citations abbreviate it, and the release it follows;
- `corpus.upstream_rev`, linked to the upstream commit, and whether it moved;
- which toolchain stages changed, by `stage_code_version`;
- counts split by cause: works whose `source_content_hash` changed (a new
  edition), and works whose artifacts changed while their source did not (new
  documents of the same edition);
- works added and works withdrawn, the latter linked to the governing event.

Per work, on the work's own page: the source hash identifying its edition, and
whether this release changed its edition, its documents, or neither.

The wording has to hold the glossary's line. A work page may not say "new
edition" for a re-conversion.

Recommendation: build this regardless of the two decisions above. It is the part that
answers the owner's report directly, it is derived rather than asserted, and it
is independently checkable by anyone who has the chain.

## Consequences if the recommendations are taken

`snh-manifest/3` carries one change, the layer slot, rather than two. The
maturity label ships with the serving layer and can be corrected. The release
history is a serving projection over signed inputs, in the same position as the
citation forms and the catalog CSV, which ADR 0001 also placed in the serving
layer so that formatting stays correctable.

The `delta` oracle stays a build-time qualification gate. The serving-layer
history answers a different question from published manifests, and the two are
not merged: the oracle explains executions, the history explains outcomes.

## Open

Whether a serving-layer label satisfies the citation requirement. A
bibliography entry currently carries the abbreviated head; if a reader is
expected to write `v0.1`, that string has to come from somewhere a citation
generator can read, which is the serving layer under this recommendation.
