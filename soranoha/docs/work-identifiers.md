# Work identifiers

Every published work has one identifier, of the form
`<work-id>_<card-directory>` — six digits, an underscore, six digits:

```
000092_000879
```

It appears as `works[].slug` in each signed manifest, as the directory name in
the published routes, and as the file name of a withdrawal record:

```
https://soranoha.org/works/000092_000879/tei
https://soranoha.org/works/000092_000879/plaintext
https://soranoha.org/works/000092_000879/tei-validation
https://soranoha.org/withdrawn/000092_000879.json
```

Both components come from Aozora Bunko. `000092` is the catalog's 作品ID
(work id) and `000879` is the contributor card directory the source archive is
filed under, as in `cards/000879/files/`. Together they name the Aozora card
the text was taken from:

```
https://www.aozora.gr.jp/cards/000879/card92.html
```

The card URL drops the leading zeros from the work id; nothing else needs
translating.

## What the identifier promises

**It is stable across releases.** The identifier is computed from one source's
own coordinates and nothing else, so no change elsewhere in the corpus can move
it. Adding works, removing works, reprocessing the corpus, changing the
converter, or changing this project's own tooling all leave it exactly as it
was.

**It denotes the work, not a particular file.** If Aozora repackages the
archive, re-proofreads the text, or publishes a corrected edition, the
identifier stays the same and the text under it changes. What changed is
recorded, not hidden: the manifest carries `source_content_hash` per work, over
the canonical identity of the source bundle rather than a raw archive hash, so
two releases can be compared exactly. Cite the identifier for the work; cite
the identifier plus `source_content_hash` when you need the precise bytes you
read.

**It is not promised to resolve forever.** Publication is append-only, but a
work can be withdrawn. A withdrawn identifier stops appearing under `/works/`
and appears under `/withdrawn/<identifier>.json`, which names the signed
governance event carrying the public reason. The releases that included the
work keep their manifests, signatures and content — withdrawal removes the
work from the current release, and never rewrites a past one.

An identifier would also change if Aozora refiled the same work under a
different contributor card, since the card directory is part of it. Only that
work would be affected; every other identifier is computed independently.

## What the identifier deliberately omits

Earlier builds published a longer identifier —
`000092_000879_000879_92_ruby_164` — which additionally carried the catalog
person id, the archive filename stem, and its `ruby` or `txt` format variant.
Those describe how the build reached the text, not what the text is. Keeping
them in a permanent citable URL would mean that a change in how editions are
selected either moved the identifier of an unchanged work or froze a stale
description of the download.

The card directory was kept even though the work id alone is nearly unique. It
is the only component that distinguishes one work id filed under several
contributor cards, and seven work ids in the current selection are filed under
two cards each. Work id alone, and work id with person id, both collide on
those.
