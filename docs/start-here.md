# Start here

Soranoha publishes TEI P5 editions of Japanese texts from Aozora Bunko. Each
work is published in four formats alongside a signed record of exactly which
bytes were published.

This guide covers retrieving individual works, downloading the full corpus, and
navigating published formats.

> **Pre-genesis**: The URLs below reflect the upcoming first public release.
> Identifiers, schemas, and licenses are fixed. For local builds before
> genesis, see [the worked example](worked-example.md).

## What you get

Every work is published in four files, all reachable from its identifier:

| File | What it is |
|---|---|
| `tei` | TEI P5 XML edition containing ruby, gaiji, emphasis, indentation, headings, the source colophon, and byte offsets into the Aozora Bunko source. |
| `plaintext` | Reading text alone in UTF-8. Ruby readings, editorial notes, and apparatus remain in the TEI edition rather than this projection. |
| `markdown` | CommonMark containing the reading text with headings, ruby as inline HTML `<ruby>`, and CSS emphasis marks. |
| `tei-validation` | JSON report recording each validation layer's result, every rule finding with its severity, and the sha256 of the TEI profile the file was checked against. |

The TEI edition is canonical; plaintext, Markdown, and validation reports are generated from it.

## Get one text

Every work has an identifier of the form `<work-id>_<card-directory>`, consisting of six digits, an underscore, and six digits. Akutagawa Ryūnosuke's 蜘蛛の糸 is
`000092_000879`.

```sh
curl -o 000092_000879.xml https://soranoha.org/works/000092_000879/tei
curl -o 000092_000879.txt https://soranoha.org/works/000092_000879/plaintext
```

The URL has no extension because it names the artifact type, not a file; give
`curl` the name you want. Every work also serves the same bytes under a
readable name, which is what a browser save or `curl -O` writes to disk:

```sh
curl -O https://soranoha.org/works/000092_000879/Akutagawa_Ryunosuke-kumono_ito-000092_000879.xml
```

The filename structure is `<author>-<stem>-<identifier>.<ext>`. The slug
identifier is canonical and is what disambiguates: 2357 works in the Aozora
Bunko catalog share an author and title with another work, so without it those
files would overwrite each other during bulk extraction. The filename is a
convenience for human downloads. Every work page shows both forms.

To read it rather than download it, open
`https://soranoha.org/works/000092_000879/` for the bibliography and
`https://soranoha.org/works/000092_000879/read` for the text itself, ruby and
all, in horizontal or vertical setting. A place in the text has an address
too: every segment of the reading view carries the id its TEI element carries
as a source reference, so `.../read#source-694-733` opens the page at the
segment that accounts for bytes 694 to 733 of the source text as UTF-8. The
TEI file is where to find the number.

If you have an Aozora Bunko card URL, you already have the identifier. The card
`https://www.aozora.gr.jp/cards/000879/card92.html` gives work id `92` and card
directory `000879`; pad each to six digits and join them in that order, work id
first, giving `000092_000879`.

Identifiers are stable across releases and denote the work, not a particular
file. [Work identifiers](../soranoha/docs/work-identifiers.md) states exactly
what that promises and what it does not.

## Find a text

- Search by title, kana title reading, author or identifier at `https://soranoha.org/`.
- Browse by author at `https://soranoha.org/authors/`, by title at
  `https://soranoha.org/titles/`, grouped under the first kana of the reading,
  or by NDC class at `https://soranoha.org/ndc/`.
- Retrieve the bibliography of every work in the current release as a single JSON file at `https://soranoha.org/catalog.json`. This file forms part of the signed record rather than a convenience export.

A catalog entry gives you the identifier, the title and its reading, the
author and other contributors, the first-publication note, the orthography
(新字新仮名 and the rest), the NDC class, the Aozora Bunko card URL, and the printed
source edition the transcription was made from.

```python
import json, urllib.request

def name(person):
    # A contributor can carry one name part rather than two: 30 people in the
    # catalog have a family name and no given name (ヴォルテール, 兼好法師),
    # and the missing part is null rather than an empty string.
    return "".join(part for part in (person["family_name"],
                                     person["given_name"]) if part)

catalog = json.load(urllib.request.urlopen("https://soranoha.org/catalog.json"))
for work in catalog["works"][:5]:
    authors = [c for c in work["contributors"] if c["relation_to_work"] == "著者"]
    print(work["slug"], work["title"], "／".join(name(a) for a in authors))
```

## Get all of them

The whole corpus is two files:

- `https://soranoha.org/bulk/soranoha-tei.zip`
- `https://soranoha.org/bulk/soranoha-plaintext.zip`

Smaller selections are linked from the pages they match: every author page
offers that person's works as a TEI archive and a plaintext archive, and every
NDC class page offers that class the same way. All of them are pre-built,
because the site is a static tree with no application behind it, so what you
can download is exactly what is listed.

Each archive contains the readable filenames plus `catalog.csv` at its root,
carrying the structured citation fields for exactly the works inside it:

    identifier, title, subtitle, title_reading, author,
    orthographic_style, ndc, first_published,
    source_edition_title, source_edition_publisher, source_edition_year,
    filename, source_content_hash, url, release, doi

This CSV provides bibliographic records for spreadsheet or script ingestion.
The canonical fields are `identifier`, `source_content_hash`, and `release`.
Read with `utf-8-sig` to handle the UTF-8 byte-order mark.

`source_edition_year` is the Gregorian year of the 底本's first edition,
taken from Aozora Bunko's 初版発行年. That field is a free-form publication history
rather than a year, as in `1981（昭和56）年3月20日`, sometimes with a
printing history after it. The year is extracted for the machine-readable
columns, while the work page shows the recorded string in full.

To fetch works individually, such as a filtered subset:

```python
import json, urllib.request, pathlib, time

catalog = json.load(urllib.request.urlopen("https://soranoha.org/catalog.json"))
out = pathlib.Path("corpus-tei")
out.mkdir(exist_ok=True)

for work in catalog["works"]:
    slug = work["slug"]
    target = out / f"{slug}.xml"
    if target.exists():
        continue
    with urllib.request.urlopen(f"https://soranoha.org/works/{slug}/tei") as r:
        target.write_bytes(r.read())
    time.sleep(0.5)
```

Rate-limit automated requests when fetching individual texts, or use the bulk
archives for complete corpus downloads. Quarterly Zenodo deposits are planned
and not yet minted; [Citing Soranoha](citation.md) says what they will carry.

## Scale and coverage

The source is Aozora Bunko's catalog of works whose copyright has expired in
Japan, restricted to works with a downloadable text file: 17,655 of them in the
catalog this release builds from, contributed by 1,167 authors, translators,
editors and collators. Those numbers move as Aozora Bunko adds works and as
copyrights expire. Each release states its own count on the landing page and
in `/catalog.json`; a release is a fixed set of works, not a live view.

Contributors are recorded with their upstream Aozora Bunko roles, such as 著者, 翻訳者,
校訂者, or 編者. A work can have several contributors, and the site indexes individuals
across all assigned roles rather than authorship alone.

Two boundaries exclude items present in upstream Aozora Bunko:

**Publication is gated on rights assessment.** A work is admitted to a release
only when its rights evaluation passes against recorded evidence, defaulting to
Aozora Bunko's catalog metadata. See
[admission and assessment](user-glossary.md#admission).

**Illustrations are not included.** Aozora Bunko's image files are outside the
grant. Where the source references an illustration, the TEI records the
reference and its caption; the image itself is not published.

## Licence

The underlying works are not Soranoha's to license. Almost all of them are out
of copyright; the rest are works whose rightsholder publishes them on Aozora
Bunko under a Creative Commons Attribution licence. Soranoha's own
contributions, including the TEI encoding, plaintext and Markdown projections,
validation reports, catalog, and manifests, are dedicated to the public domain
under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/). The
dedication covers what Soranoha added; the text of a CC BY work keeps its
licence in every file that contains it.

You may copy, redistribute, adapt, translate, mine and republish all of it,
commercially or not, without asking and without payment. For Soranoha's own
encoding, attribution is requested, not required, which mirrors Aozora Bunko's
own posture. Where the underlying work is under CC BY, attribution to that work
is a condition of its licence. Each work's page and TEI header state which of
the two standings applies.

[Rights](rights.md) is the full statement, including what applies if you
redistribute the software rather than the corpus, and how a rights holder asks
for a work to be withdrawn. [Citing Soranoha](citation.md) details citation formats.
Citations should name both the release and the identifier, because title and author
alone do not uniquely identify a work.

## What "signed" means, and why you might care

Every release is a signed manifest listing each published work and the SHA-256
of each of its four files. The manifests form an append-only chain, so a
release cannot be silently altered or removed after the fact, and anyone can
check that the file they downloaded is the file the release says it published.

Every release in the chain is listed at `https://soranoha.org/history`, newest
first, with its upstream Aozora Bunko revisions and added, removed, or
re-encoded work counts derived directly from published manifests.

To ask what has changed since some earlier point in Aozora Bunko's own
history, check that history out and compare it against a release. From a
checkout of this repository:

```sh
nix run .#soranoha-kernel -- corpus-delta \
  --chain-clone /path/to/a/clone/of/the/chain \
  --aozora-root /path/to/aozorabunko/checked/out/at/the/revision/you/mean \
  --release-pub release.pub --governance-pub governance.pub
```

It names the works whose source differs, the works only one side has, and the
works a governance event withdrew. Nothing is parsed and nothing is rebuilt,
because a release records each work's source identity. The chain is verified
against the published keys first, and the comparison runs locally against the
supplied checkout.

For most work you can ignore this entirely. It matters when you need a result
to remain reproducible over time. Citing the release keeps the exact bytes you
analyzed identifiable even if subsequent releases change the corpus. The
[glossary](user-glossary.md) explains manifest, chain and admission without
assuming any of the cryptography; the
[protocol specification](design/snh-protocol-v1.md) is the full contract for
anyone implementing a verifier.

## Where to go next

- [A worked example](worked-example.md) walks through one text from Aozora Bunko source to TEI and plaintext, explaining each header block and providing loader code.
- [Glossary](user-glossary.md) defines terminology used across project outputs.
- [Work identifiers](../soranoha/docs/work-identifiers.md) explains identifier structure, stability across releases, and edition tracking.
- [TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md) specifies `snh:` attributes in published files and their schema validation constraints.
- [Rights](rights.md) and [Citing Soranoha](citation.md).
