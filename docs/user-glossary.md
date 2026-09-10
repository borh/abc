# Glossary

Terminology used throughout Soranoha for corpus artifacts, release records,
and encoding concepts.

For internal names of parser and rendering behavior, see
[parser and rendering invariants](parser-invariants.md).

## admission

The decision to include a work in a release. A work is admitted when its
rights standing has been established against a recorded basis and that basis
still holds at the moment of publication.

Admission is independent of validation: a work can pass validation without
being admitted, and publication does not guarantee encoding quality.
Each release manifest records the admission evidence relied upon.

## assessment

The evaluation that produces an admission basis: what is claimed about a
work's rights standing, on what evidence, effective from when, and in which
jurisdiction (Japan).

Assessment records **facts with premises**, not permanent verdicts. A fact
records what was established, from which evidence, as of which date. If evidence
becomes unreachable, the fact becomes unavailable rather than remaining asserted.
See [reliance](#reliance) for the basis used for most works.

## reliance

The default admission basis: Aozora Bunko's published copyright-expired
classification for a specific edition, recorded as an attributed upstream
assertion alongside the raw page bytes. A recorded exception or conflicting
reviewed finding prevents reliance.

Reliance is re-checked against the live source at release time. Retained
evidence cannot substitute for a current check.

## fidelity

The proportion of source markup preserved in the resulting encoding. It
measures markup preservation: rights standing comes from assessment, and
correctness from validation.

Two evidence reports accompany each build:

- **source accountability**: Scans raw Aozora Bunko text for recognizable markup
  independently of the parser, recording exact byte spans.
- **interpretation coverage**: Compares the scan against converted output,
  counting claimed versus unaccounted annotations per family. Unaccounted
  annotations are listed in full.

Fidelity measures presence, not interpretive correctness. Ambiguities are
recorded directly in the published TEI as `interpretation-problem` notes.

## work, edition, document

Three distinct concepts maintained throughout:

- **work**: The intellectual work (e.g. 蜘蛛の糸). A Soranoha identifier denotes a work.
- **edition**: A specific source, such as a printed edition or the Aozora Bunko
  transcription derived from it. The `source_content_hash` in the manifest and
  `<sourceDesc>` in each TEI file identify the edition.
- **document**: A specific byte sequence identified by its SHA-256 digest. Two
  releases of the same work from the same edition may produce different
  documents as conversion tooling evolves.

Cite the identifier for the work, the identifier plus `source_content_hash`
for the edition, and the artifact id for exact bytes. See
[work identifiers](../soranoha/docs/work-identifiers.md) and
[citing Soranoha](citation.md).

## artifact

One published file, identified by its content: `snh:1:<type>:<sha256>`. The
types published per work are `tei`, `plaintext`, `markdown`, and
`tei-validation`.

Artifact IDs name bytes directly and can be verified by computing the SHA-256
hash locally.

## manifest

The signed record of one release. It lists every admitted work with its
identifier, its `source_content_hash`, and the ID and byte length of each of
its four artifacts; the rights grant; admission evidence; the release catalog;
and the parent manifest digest.

The manifest is authoritative. Site pages, indexes, and reading views are
generated downstream from the manifest. `https://soranoha.org/releases/latest`
serves the current manifest.

## chain

The append-only sequence of manifests, each referencing its predecessor back
to genesis.

Earlier releases remain immutable. A withdrawal removes a work from current
distribution; it cannot alter historical manifests.

Signing roles are separated between publication and governance keys, with
governance keys kept offline. See the [protocol specification](design/snh-protocol-v1.md).

## release

One published state of the corpus: a manifest, its signature, and the
artifacts it names. Identified by the manifest's SHA-256 hash (the release head).

Citing a release head identifies the exact corpus state referenced.

## catalog

The bibliography of every work in a release, including identifiers, titles,
readings, contributors, first publication details, orthographic style, NDC
classification, Aozora Bunko card URL, and source edition. Published as
`https://soranoha.org/catalog.json`.

The catalog is part of the signed release. It contains bibliographic facts
rather than presentation-layer artifacts like download filenames or formatted
citations.

## withdrawal

Removing a work from current distribution via a signed governance event
specifying a reason code and public statement.

Withdrawal halts distribution from `/works/<identifier>/` and the active catalog.
It does not delete historical records: prior signed manifests continue to name the
work, and its artifacts remain addressable by hash. Withdrawal records are served
at `https://soranoha.org/withdrawn/<identifier>.json`.

For rights inquiries or withdrawal requests, see [rights](rights.md).

## body-v1

The reading policy defining canonical reading text, used by the plaintext
projection, the site reading view, and external annotation layers.

It traverses the TEI body and resolves alternatives deterministically: selecting
apparatus lemmas, choosing corrected or regularized editorial readings over
source originals, keeping base text rather than ruby annotations, and omitting
editorial notes or figure descriptions. Unresolved gaiji are excluded from
annotatable spans rather than replaced.

Versioned reading policies ensure character offsets remain stable and verifiable
across corpus revisions.

## gaiji (外字)

A character the Aozora Bunko source file could not encode, written as a
prose description, usually with a JIS X 0213 code point:
`※［＃「特のへん＋廴＋聿」、第3水準1-87-71］`.

Published TEI includes both the resolved character and the original source
marker verbatim, allowing resolutions to be verified or challenged. See
[the worked example](worked-example.md#encodingdesc-taxonomies-and-gaiji-declarations).

## ruby (ルビ)

A reading printed alongside or above base characters (furigana). Written as
`蓮池《はすいけ》` in source text; represented in TEI as `<ruby>` containing
`<rb>` (base) and `<rt>` (reading).

Ruby readings appear in the TEI encoding and are omitted from plaintext
projections, which retain base text only.

## source offset

A byte range into the UTF-8 decoded Aozora Bunko source text, recorded for
almost every element in the published TEI.

Source offsets index the source file directly rather than intermediate representations.
The indexed text is verified via `primary-text-hash` in `<sourceDesc>`, which
records the SHA-256 of the original Aozora Bunko text file.

## snh

The publication protocol: the format of manifests, governance events,
admission evidence, and artifact IDs, along with verifier rules. Also the
prefix of the extension vocabulary in published TEI
(`https://w3id.org/soranoha/ns/tei`).

Used to verify the corpus independently or build conforming tools. See the
[protocol specification](design/snh-protocol-v1.md) and the
[TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md).
