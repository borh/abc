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

```
芥川龍之介「蜘蛛の糸」（新字新仮名）、底本『芥川龍之介全集　第三巻』筑摩書房、
1971年。Soranoha Aozora TEI Corpus, 000092_000879, release
d9f2a1c0…95867. https://doi.org/10.5281/zenodo.XXXXXXX
```

Every field before the corpus name comes from the work's TEI header, which is
the authoritative bibliographic record; the catalog and every generated
citation format are projections of it.

Romanized titles are not published. Supply the Japanese title and
its kana reading and let your journal's style (Hepburn with or without
macrons, ALA-LC) govern the romanization.

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
