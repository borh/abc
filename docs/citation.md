# Citing Soranoha

Citation is a scholarly norm here, not a licence condition: everything
Soranoha publishes is CC0, so you may reuse it without attribution. Please
cite anyway; see [rights](rights.md) for why credit is secured this way.

## Cite a release, not "the corpus"

Soranoha is versioned, and the corpus changes between releases: works are
added as their assessment completes, encoding improves, and a work can be
withdrawn. A citation that names only "Soranoha" does not identify the bytes
you read.

Every release is identified three ways, and a citation should carry the ones
its context allows:

| Identifier | Form | Where |
|---|---|---|
| Release head | 64-character hex | `https://soranoha.org/releases/HEAD` |
| Manifest | `snh:1:release-manifest:<sha256>` | `manifest_id` in the release record |
| Version DOI | `10.5281/zenodo.<n>` | The Zenodo deposit for that release |

The version DOI is the one to put in a bibliography: it resolves to exactly
the bytes cited, and it survives the site. The head is what a reader uses to
re-verify the chain independently.

> **Not yet minted.** The Zenodo concept and version DOIs are created at
> public genesis. Until then the DOI lines below are shown in their intended
> form with a placeholder, and the head is the only stable release
> identifier.

## Citing the whole corpus

```
Hodošček, Bor. Soranoha Aozora TEI Corpus. Release
d9f2a1c0e4b78356f1a2c9d0e5b4738a6c1f0e2d3b5a4978c6e1f0d2b3a495867.
https://doi.org/10.5281/zenodo.XXXXXXX
```

BibLaTeX:

```bibtex
@dataset{soranoha,
  author  = {Hodošček, Bor},
  title   = {Soranoha Aozora TEI Corpus},
  version = {d9f2a1c0e4b78356f1a2c9d0e5b4738a6c1f0e2d3b5a4978c6e1f0d2b3a495867},
  doi     = {10.5281/zenodo.XXXXXXX},
  url     = {https://soranoha.org},
}
```

`CITATION.cff` in the repository root carries the same record in machine-readable
form; GitHub-compatible forges and Zenodo read it directly.

To cite the corpus across releases rather than one of them, use the Zenodo
*concept* DOI, which always resolves to the latest version. Do not use it when
reproducibility matters: it does not name specific bytes.

## Citing one work

A work needs both a readable description and its identifier: measured over the
Aozora catalog, author plus title leaves 1966 works ambiguous, and author,
title, 副題, 文字遣い種別, 底本名 and 初出 together still leave 16. The
identifier `000092_000879` disambiguates all of them. It is stable across
releases; see [work identifiers](../soranoha/docs/work-identifiers.md).

Every work page carries both forms below, ready to copy, and offers the same
record as CSL-JSON and BibLaTeX. Everything shown here is generated from the
work's TEI header, which is the authoritative bibliographic record; the
catalog and every citation format are projections of it, and there is no
second metadata source.

### For a Japanese bibliography

```
芥川 龍之介「蜘蛛の糸」（新字新仮名）、底本『芥川龍之介全集　第三巻』筑摩書房、
1971年。Soranoha Aozora TEI Corpus, 000092_000879, release d9f2a1c0e4b7….
https://doi.org/10.5281/zenodo.XXXXXXX
```

### For an English bibliography

```
Akutagawa Ryunosuke. “蜘蛛の糸” (新字新仮名). In 芥川龍之介全集　第三巻.
筑摩書房, 1971. Soranoha Aozora TEI Corpus, 000092_000879,
release d9f2a1c0e4b7…. https://doi.org/10.5281/zenodo.XXXXXXX
```

The author is in Latin script because the catalog publishes romanized name
parts. The titles are not; see below for why.

### What each component is for

| Component | Example | Why it is there |
|---|---|---|
| Author | `芥川 龍之介` / `Akutagawa Ryunosuke` | Who wrote it. Not sufficient to identify the work. |
| Title, with 副題 | `蜘蛛の糸` | What it is. Not sufficient either: 1966 works share an author-and-title pair. |
| 文字遣い種別 | `新字新仮名` | Which orthographic transcription. Aozora often publishes the same work in two, and they are different texts. |
| 底本 | `『芥川龍之介全集　第三巻』筑摩書房、1971年` | The printed book the transcription was made from. Two transcriptions of one work from different 底本 are different texts. |
| Corpus name | `Soranoha Aozora TEI Corpus` | Which corpus, distinguishing this encoding from Aozora Bunko's own files. |
| **Work identifier** | `000092_000879` | **Identifies the work.** Stable across releases. The one component that makes the citation unambiguous. |
| **Release** | `release d9f2a1c0e4b7…` | **Identifies the bytes.** The corpus is versioned; a citation without it does not name what was read. |
| Version DOI | `https://doi.org/10.5281/zenodo.XXXXXXX` | Resolves to exactly those bytes, and survives the site. |

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

All three describe a work as an `@incollection` / CSL `chapter`, an item
inside its 底本, rather than as a book of its own, so a bibliography built
from them keeps the source edition. That subtype is why the page embeds COinS
rather than Highwire `citation_*` meta tags, which cannot express it.

```bibtex
@incollection{soranoha-000092_000879,
  author = {芥川, 龍之介},
  title = {蜘蛛の糸},
  titleaddon = {くものいと},
  booktitle = {芥川龍之介全集　第三巻},
  publisher = {筑摩書房},
  date = {1971},
  language = {japanese},
  langid = {japanese},
  eprinttype = {Soranoha Aozora TEI Corpus},
  eprint = {000092_000879},
  version = {d9f2a1c0e4b78356f1a2c9d0e5b4738a6c1f0e2d3b5a4978c6e1f0d2b3a495867},
  url = {https://soranoha.org/works/000092_000879/},
  doi = {10.5281/zenodo.XXXXXXX},
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

`date = {1971}` is extracted, not recorded. Aozora's 底本初版発行年 is a
free-form publication history rather than a year: 17712 of 18808 recorded
values read like `1981（昭和56）年3月20日`, and 914 carry a printing history
on top of that. The first Gregorian year is taken, which is the field's own
meaning: where several years appear, the earliest is the first edition's.
Work pages show the recorded string in full; only the machine-readable fields
carry the extracted year.

### What CSL cannot carry

CSL has no field for a kana reading, and multiscript support in CSL and Zotero
is weak, so 作品名読み does not round-trip through CSL-JSON. It is recorded in
the `note` field so that it is not lost, but a reference manager will not treat
it as a reading. BibLaTeX puts it in `titleaddon`, and the authoritative copy is
in the TEI header and the catalog either way. This is documented rather than
worked around: inventing a CSL extension field would produce records that only
Soranoha can read.

### Romanization

Romanized titles are deliberately not published, in any form. The kana reading
carries no word boundaries, so a mechanical transliteration is not correct
Hepburn. Supply the Japanese title and its reading, and let your journal's
style (Hepburn with or without macrons, ALA-LC) govern the romanization.
The 文字遣い種別 values are Aozora's own classification and are left in
Japanese for the same reason.

## Citing an exact byte sequence

For work that must be reproducible against specific bytes rather than a
release, cite the artifact by content address. Every published artifact has an
id of the form `snh:1:<type>:<sha256>`, and the manifest that names it is
signed:

```
Soranoha Aozora TEI Corpus, work 000092_000879, TEI artifact
snh:1:tei:6f0b1c…, in release
d9f2a1c0e4b78356f1a2c9d0e5b4738a6c1f0e2d3b5a4978c6e1f0d2b3a495867.
```

This is the strongest form available: a reader can fetch the artifact, hash
it, find that hash in the manifest, and verify the manifest's signature
against the release key published in the trust anchor deposit. The procedure
is specified in [`docs/design/snh-protocol-v1.md`](design/snh-protocol-v1.md).

## Citing the tooling

The conversion and publication code is a separate work from the corpus, under
a separate licence, and includes a forked parser core; see
[rights](rights.md#the-toolchain). Cite it when the method rather than the
texts is what your work depends on.
