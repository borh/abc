# Work identifiers

Every published work has one identifier of the form
`<work-id>_<card-directory>`, consisting of six digits, an underscore, and six digits:

```
000092_000879
```

It appears as `works[].slug` in each signed manifest, as the directory name in
the published routes, and as the file name of a withdrawal record:

```
https://soranoha.org/works/000092_000879/tei
https://soranoha.org/works/000092_000879/markdown
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
identifier stays the same and the text under it changes. Changes are
recorded explicitly rather than hidden. The manifest carries `source_content_hash` per work
over the canonical identity of the source bundle rather than a raw archive hash, so
two releases can be compared exactly. Cite the identifier for the work; cite
the identifier plus `source_content_hash` when you need the precise bytes you
read.

**It is not promised to resolve forever.** Publication is append-only, but a
work can be withdrawn. A withdrawn identifier stops appearing under `/works/`
and appears under `/withdrawn/<identifier>.json`, which names the signed
governance event carrying the public reason. The releases that included the
work keep their manifests, signatures and content; withdrawal removes the
work from the current release, and never rewrites a past one.

An identifier would also change if Aozora refiled the same work under a
different contributor card, since the card directory is part of it. Only that
work would be affected; every other identifier is computed independently.

## What the identifier omits

The identifier omits person IDs, archive filename stems, and format variants (`ruby`/`txt`). Those describe acquisition details rather than intellectual work identity. Including them in permanent URLs would couple work identity to edition selection mechanics.

The card directory is not a person ID that happens to look like one. It is the
card the archive is filed under, which for 146 works belongs to a contributor
other than the first author.

The card directory was kept even though the work id alone is unique across the
present corpus: in the catalog this release builds from, no work id is filed
under more than one contributor card. It is the documented disambiguator for a
work id that is filed under several, so the scheme is collision-safe by
construction rather than by observation, which is what a permanent identifier
needs.

## The identifier and the download filename

Every artifact is served at two locations. Citations use `/works/<identifier>/<type>`, which can be
constructed directly from the work identifier. Browser downloads and `curl -O`
receive `/works/<identifier>/<author>-<stem>-<identifier>.<ext>`. Both paths
resolve to the same blob.

The filename is a convenience, whereas the identifier is the citable reference. The
filename is a serving-layer projection. It can be corrected in a later release,
its author component selects one contributor out of a work that may record
several, and its middle component is Aozora Bunko's own archive stem rather
than an identifier assigned by this project. The signed catalog carries the inputs (`archive_stem` and the romanized name
parts) rather than the assembled name, because a derived value stored beside
its own inputs can disagree with them and the chain is append-only.

The identifier is a component of the filename rather than an alternative to
it. 470 author-and-title pairs are shared by 2357 of the 17810 works in the
Aozora catalog, so a name built from author and title alone would silently
collide for more than one work in ten of any bulk extraction.
Splitting a filename on its hyphens recovers the
identifier as the last component; the bulk archives also carry it in
`catalog.csv`.
