# Citing Soranoha

Citation is a scholarly norm here, not a licence condition: everything
Soranoha publishes is CC0, so you may reuse it without attribution. Please
cite anyway; see [rights](rights.md) for why credit is secured this way.

## Cite a release, not "the corpus"

Soranoha is versioned, and the corpus changes between releases: works are
added as their assessment completes, encoding improves, and a work can be
withdrawn. A citation that names only "Soranoha" does not identify the bytes
you read.

Every release is identified two ways, and a citation should carry the ones
its context allows:

| Identifier | Form | Where |
|---|---|---|
| Release head | 64-character hex | `https://soranoha.org/releases/HEAD` |
| Version DOI | `10.5281/zenodo.<n>` | The quarterly Zenodo snapshot, when the release you read is one |

The head is the identifier that always exists, and it is what a reader uses to
re-verify the chain independently. Deposits are quarterly rather than per
release, so a release published between two snapshots has no DOI of its own;
carry the head alone in that case. When the release you read does carry a DOI,
put it in the bibliography too: it resolves to exactly the bytes cited, and it
survives the site.

The head is the sha256 of the release manifest's own canonical bytes, so it
identifies the manifest as well as the release: it names `releases/<head>.json`
in the chain and is the `manifest_id` the publishing command reports. There is
no third identifier to cite.

The forms below are templates: they show where each component goes. The same
forms filled in for the release you are reading are on the served
[citation page](https://soranoha.org/citation), and every work page carries
its own, ready to copy.

> **Not yet minted.** The Zenodo concept and version DOIs are created at
> public genesis. Until then the DOI lines below are shown in their intended
> form with a placeholder, and the head is the only stable release
> identifier.

## A release is citable once it has been archived

The corpus is citable only where its bytes can still be reached after this
site is gone, so the specification makes eligibility conditional: a release
becomes citable when an independent archival observation of its publication
commit has succeeded. The observation reads the archived copy of the
publication repository as its only source and re-runs the full chain
verification against it, which is what establishes that the archive holds a
complete and self-checking copy rather than a partial crawl.

An observation covers more than the one release it names. Verification walks
the whole chain back to genesis through the archived copy, so an observation
that succeeds at one commit establishes that every release up to that commit is
in the archive and checks out. A release published after the last observation
is not yet covered; it becomes covered by the next observation of any later
commit, without anything being redeposited.

Because the observation reports carry no ordering contract, a later failed
observation does not by itself revoke an earlier successful one.
[Archival](archival.md) describes how an observation is made and how to resolve
a citation through the archive without the live site.

## Citing the whole corpus

```
Hodošček, Bor. Soranoha Aozora TEI Corpus. Release
<release head, 64 hex characters>.
https://doi.org/10.5281/zenodo.<version deposit>
```

BibLaTeX:

```bibtex
@dataset{soranoha,
  author  = {Hodošček, Bor},
  title   = {Soranoha Aozora TEI Corpus},
  version = {<release head, 64 hex characters>},
  doi     = {10.5281/zenodo.<version deposit>},
  url     = {https://soranoha.org},
}
```

`CITATION.cff` in the repository root carries the corpus-level record in
machine-readable form, and GitHub-compatible forges and Zenodo read it
directly. It describes the work rather than one release: the head changes with
every release, and the version DOI changes with every quarterly snapshot, so
neither is in it. Fill them in from the release you read.

To cite the corpus across releases rather than one of them, use the Zenodo
*concept* DOI, which always resolves to the latest version. Do not use it when
reproducibility matters: it does not name specific bytes.

## Citing one work

A work needs both a readable description and its identifier: measured over the
Aozora Bunko catalog this release builds from, author plus title leaves 2357 works
ambiguous, and author, title, 副題, 文字遣い種別, 底本名 and 初出 together
still leave 16. Those counts move with the upstream catalog. The identifier
`000092_000879` disambiguates all of them, and is stable across releases; see
[work identifiers](../soranoha/docs/work-identifiers.md).

Every work page carries both forms below, ready to copy, and offers the same
record as CSL-JSON and BibLaTeX. Everything shown here is generated from the
work's TEI header, which is the authoritative bibliographic record; the
catalog and every citation format are projections of it, and there is no
second metadata source.

### For a Japanese bibliography

```
芥川 竜之介「蜘蛛の糸」（新字新仮名）、底本『芥川龍之介全集2』ちくま文庫、筑摩書房、
1986年。Soranoha Aozora TEI Corpus, 000092_000879,
release <first 12 characters of the head>…
https://doi.org/10.5281/zenodo.<version deposit>
```

### For an English bibliography

```
Akutagawa Ryunosuke. “蜘蛛の糸” (新字新仮名). In 芥川龍之介全集2.
ちくま文庫、筑摩書房, 1986. Soranoha Aozora TEI Corpus, 000092_000879,
release <first 12 characters of the head>…
https://doi.org/10.5281/zenodo.<version deposit>
```

The author is in Latin script because the catalog publishes romanized name
parts. The titles are not; see below for why.

Both lines abbreviate the head to its first 12 characters. That is what a
bibliography can carry without becoming unreadable, and it resolves: the
abbreviation is served as a name of its own, so
`https://soranoha.org/releases/short/8b259c55e63a.json` reaches the same
manifest as the full 64-character name. It sits in its own directory because a
name directly under `releases/` is the sha256 of its own bytes, which is a
check worth keeping. The full 64 characters are in every
machine-readable form: `version` in BibLaTeX, the `Release` sentence in the CSL
note, and the `release` column of `catalog.csv`.

### What each component is for

| Component | Example | Why it is there |
|---|---|---|
| Author | `芥川 竜之介` / `Akutagawa Ryunosuke` | Who wrote it. Not sufficient to identify the work. |
| Title, with 副題 | `蜘蛛の糸` | What it is. Not sufficient either: 2357 works share an author-and-title pair with another work. |
| 文字遣い種別 | `新字新仮名` | Which orthographic transcription. Aozora Bunko often publishes the same work in two, and they are different texts. |
| 底本 | `『芥川龍之介全集2』ちくま文庫、筑摩書房、1986年` | The printed book the transcription was made from. Two transcriptions of one work from different 底本 are different texts. |
| Corpus name | `Soranoha Aozora TEI Corpus` | Which corpus, distinguishing this encoding from Aozora Bunko's own files. |
| **Work identifier** | `000092_000879` | **Identifies the work.** Stable across releases. The one component that makes the citation unambiguous. |
| **Release** | `release <first 12 characters of the head>` | **Identifies the bytes.** The corpus is versioned; a citation without it does not name what was read. |
| Version DOI | `https://doi.org/10.5281/zenodo.<version deposit>` | Resolves to exactly those bytes, and survives the site. Present only when the release you read is a quarterly snapshot. |

The identifier and the release are the two components that are never
optional. Everything above them is description; those two are identity.

### Machine-readable forms

Each work serves the same record in the formats a reference manager reads:

| Route | Format | For |
|---|---|---|
| `/works/<id>/citation.json` | CSL-JSON | Zotero, Pandoc, citeproc |
| `/works/<id>/citation.bib` | BibLaTeX | LaTeX written directly |

Work pages also embed [COinS](https://en.wikipedia.org/wiki/COinS), so the
Zotero browser connector saves a correctly typed record in one click without
being told this site exists.

All three type a work as an item inside its 底本 rather than as a book of its
own: CSL `chapter`, BibLaTeX `@incollection`, COinS `genre=bookitem`. A
bibliography built from any of them keeps the source edition. That subtype is
why the page embeds COinS rather than Highwire `citation_*` meta tags, which
cannot express it.

```bibtex
@incollection{soranoha-000092_000879,
  author = {芥川, 竜之介},
  title = {蜘蛛の糸},
  titleaddon = {くものいと},
  booktitle = {芥川龍之介全集2},
  publisher = {ちくま文庫、筑摩書房},
  date = {1986},
  language = {japanese},
  langid = {japanese},
  eprinttype = {Soranoha Aozora TEI Corpus},
  eprint = {000092_000879},
  version = {<release head, 64 hex characters>},
  url = {https://soranoha.org/works/000092_000879/},
  doi = {10.5281/zenodo.<version deposit>},
  note = {新字新仮名; 初出: 「赤い鳥」1918（大正7）年7月},
}
```

The identifier uses BibLaTeX's archive idiom (`eprinttype` naming the
collection and `eprint` the item within it) rather than being buried in a
note, so ordinary bibliography styles print it without a custom driver.
`version` carries the release for the same reason. 校訂者 has no CSL
counterpart, so CSL-JSON records it as a plain `contributor` rather than
promoting it to `editor`, which would claim something the source does not
say; BibLaTeX can express it exactly, and uses `editora` with
`editoratype = {collator}`.

For many works at once, every [bulk archive](start-here.md) carries a
`catalog.csv` holding the same fields as columns, so a whole selection
becomes a bibliography in a spreadsheet.

### The edition year

`date = {1986}` is extracted, not recorded. Aozora Bunko's 底本初版発行年 is a
free-form publication history rather than a year: 17747 of the 18780 recorded
values read like `1981（昭和56）年3月20日`, and of the 1033 that do not, 667
append a printing history, as in
`1948（昭和23）年5月15日、1963（昭和38）年5月16日第20刷改版`. The first
Gregorian year is taken, which is the field's own meaning: where several years
appear, the earliest is the first edition's.
Work pages show the recorded string in full; only the machine-readable fields
carry the extracted year.

### What CSL cannot carry

CSL has no field for a kana reading, and multiscript support in CSL and Zotero
is weak, so 作品名読み does not round-trip through CSL-JSON. It is recorded in
the `note` field so that it is not lost, but a reference manager will not treat
it as a reading. BibLaTeX puts it in `titleaddon`, and the authoritative copy is
in the TEI header and the catalog either way. This limitation is documented rather than obscured with custom workarounds, because
inventing a non-standard CSL extension field would yield records unreadable by
standard reference managers.

### Romanization

Romanized titles are not published in any citation format. Because kana
readings lack word boundaries, automated transliteration cannot reliably
generate valid Hepburn romanization. The romanized element in a download
filename is Aozora Bunko's own archive stem, hand-curated with word boundaries, not
a romanization this project generated. Supply the Japanese title and its reading, and let your
journal's style (Hepburn with or without macrons, ALA-LC) govern the
romanization. The 文字遣い種別 values are Aozora Bunko's own classification and are left
in Japanese for the same reason.

## Citing an exact byte sequence

For work that must be reproducible against specific bytes rather than a
release, cite the artifact by content address. Every published artifact has an
id of the form `snh:1:<type>:<sha256>`, and the manifest that names it is
signed:

```
Soranoha Aozora TEI Corpus, work 000092_000879, TEI artifact
snh:1:tei:6f0b1c…, in release
<release head, 64 hex characters>.
```

This is the most precise form of citation available. A reader can fetch the artifact, hash
it, find that hash in the manifest, and verify the manifest's signature
against the release key published in the trust anchor deposit. The procedure
is specified in [`docs/design/snh-protocol-v1.md`](design/snh-protocol-v1.md),
and [Zenodo deposits and ORCID anchoring](zenodo-deposits.md) describes what
each deposit contains and how to check that a DOI names the release it claims.
To reach the same artifact through the archive rather than through this site,
[archival](archival.md) gives the path mapping.

## Citing the tooling

The conversion and publication code is a separate work from the corpus, under
a separate licence, and includes a forked parser core; see
[rights](rights.md#the-toolchain). Cite it when the method rather than the
texts is what your work depends on.
