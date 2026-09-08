# Start here

Soranoha is a TEI edition of Aozora Bunko. It takes the copyright-expired
Japanese texts that Aozora Bunko distributes as annotated plain text, converts
their markup into TEI P5 XML, and publishes each work in four forms with a
signed record of exactly which bytes were published and when.

You do not need Nix, Clojure or TEI to use it. This page gets you one text,
then all of them, and tells you what you are holding.

> **Pre-genesis.** The public release has not been made yet. The URLs below
> are the ones the first release will serve; the shapes of the files, the
> identifiers and the licence are settled and will not change. Until the first
> release, follow [the worked example](worked-example.md), which builds one
> text from Aozora source and shows the same output.

## What you get

Every work is published in four files, all reachable from its identifier:

| File | What it is |
|---|---|
| `tei` | TEI P5 XML. The edition: ruby, gaiji, emphasis, indentation, headings, the source colophon, and a byte offset into the Aozora source for almost every run of text. |
| `plaintext` | The reading text alone, UTF-8. Ruby readings, editorial notes and apparatus are not in it — they are in the TEI. |
| `markdown` | CommonMark: the same reading text with headings, plus ruby as inline HTML `<ruby>` and emphasis carrying the source's own mark as CSS. For tools and pipelines that want light structure without an XML parser. |
| `tei-validation` | A JSON report: which schema rules the TEI file passed, at which severities, against which version of the profile. |

TEI is the edition; the other three are projections of it. When they disagree,
the TEI is right — the projections are generated from it, so they cannot
disagree by accident, only by a defect.

## Get one text

Every work has an identifier of the form `<work-id>_<card-directory>` — six
digits, an underscore, six digits. Akutagawa Ryūnosuke's 蜘蛛の糸 is
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

The name is `<author>-<Aozora stem>-<identifier>.<ext>`. The middle part is
Aozora Bunko's own filename for the text, which its volunteers wrote with word
boundaries by hand. **The filename is a convenience; the identifier is the
citable thing.** It is inside every filename because title and author alone do
not identify a work — about one work in seven of the corpus shares an
author-and-title pair with another — so a name without it would silently
overwrite files in a bulk extraction. Every work page shows both forms.

To read it rather than download it, open
`https://soranoha.org/works/000092_000879/` for the bibliography and
`https://soranoha.org/works/000092_000879/read` for the text itself, ruby and
all, in horizontal or vertical setting.

If you have an Aozora card URL, you already have the identifier. The card
`https://www.aozora.gr.jp/cards/000879/card92.html` is card directory `000879`
and work id `92`; pad each to six digits and join them with an underscore.

Identifiers are stable across releases and denote the work, not a particular
file. [Work identifiers](../soranoha/docs/work-identifiers.md) states exactly
what that promises and what it does not.

## Find a text

- `https://soranoha.org/` — search by title or author, or browse by author, by
  the kana reading of the title, or by NDC class.
- `https://soranoha.org/catalog.json` — the bibliography of every work in the
  current release, as one JSON file. This is the file to use from a script.
  It is part of the signed record, not a convenience export.

A catalog entry gives you the identifier, the title and its reading, the
author and other contributors, the first-publication note, the orthography
(新字新仮名 and the rest), the NDC class, the Aozora card URL, and the printed
source edition the transcription was made from.

```python
import json, urllib.request

catalog = json.load(urllib.request.urlopen("https://soranoha.org/catalog.json"))
for work in catalog["works"][:5]:
    authors = [c for c in work["contributors"] if c["relation_to_work"] == "著者"]
    print(work["slug"], work["title"],
          "／".join(f'{a["family_name"]}{a["given_name"]}' for a in authors))
```

## Get all of them

The whole corpus is two files:

- `https://soranoha.org/bulk/soranoha-tei.zip`
- `https://soranoha.org/bulk/soranoha-plaintext.zip`

Smaller selections are linked from the pages they match: every author page
offers that person's works as one archive, and every NDC class page offers
that class. All of them are pre-built, because the site is a static tree with
no application behind it, so what you can download is exactly what is listed.

Each archive contains the readable filenames plus `catalog.csv` at its root,
carrying the structured citation fields for exactly the works inside it:

    identifier, title, subtitle, title_reading, author,
    orthographic_style, ndc, first_published,
    source_edition_title, source_edition_publisher, source_edition_year,
    filename, source_content_hash, url, release, doi

That is enough to turn a whole selection into a bibliography in a spreadsheet
without opening a single TEI file, and enough to get back to the record:
`identifier` and `release` together name the bytes, and `url` resolves to the
page that serves them. The filename is a convenience; the citable columns are
`identifier`, `source_content_hash` and `release`. From a script, read it with
`encoding="utf-8-sig"` — it carries a byte-order mark so that Excel does not
mangle the Japanese titles.

`source_edition_year` is the Gregorian year of the 底本's first edition,
taken from Aozora's 初版発行年. That field is a free-form publication history
rather than a year, as in `1981（昭和56）年3月20日`, sometimes with a
printing history after it. The year is extracted for the machine-readable
columns, while the work page shows the recorded string in full.

To fetch works one at a time instead — a filtered subset, say:

```python
import json, urllib.request, pathlib

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
```

Be polite about rate: this is one small server, and the corpus is tens of
thousands of files. If you want everything, take the archive above instead —
it is one request and the same bytes. The release archive on Zenodo, once it
exists, is another one-download route to the same thing.

## Scale and coverage

The source is Aozora Bunko's catalog of works whose copyright has expired in
Japan, restricted to works with a downloadable text file: about 17,300 of them
at the time of writing, contributed by about 1,050 authors, translators,
editors and collators. That number moves as Aozora adds works and as
copyrights expire. Each release states its own count on the landing page and
in `/catalog.json`; a release is a fixed set of works, not a live view.

Contributors are recorded with the role Aozora gives them — 著者, 翻訳者,
校訂者, 編者 — and a work can have several. The site indexes people by all of
their roles, not only authorship.

Two things are deliberately narrower than "everything in Aozora":

**Publication is per work and gated on rights.** A work appears in a release
only after its rights standing has been assessed against a recorded basis. The
default basis is reliance on Aozora Bunko's own published copyright-expired
classification for that exact edition, scoped to Japan — an attributed
upstream assertion, recorded as such, not an independent legal finding by this
project. A work whose basis does not hold is not published. See
[admission and assessment](user-glossary.md#admission) for what those words
mean here.

**Illustrations are not included.** Aozora's image files are outside the
grant. Where the source references an illustration, the TEI records the
reference and its caption; the image itself is not published.

## Licence

The underlying works are out of copyright and are not Soranoha's to license.
Soranoha's own work — the TEI encoding, the plaintext and Markdown
projections, the validation reports, the catalog and the manifests — is
dedicated to the public domain under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/).

You may copy, redistribute, adapt, translate, mine and republish all of it,
commercially or not, without asking and without payment. Attribution is
requested, not required, which mirrors Aozora Bunko's own posture.

[Rights](rights.md) is the full statement, including what applies if you
redistribute the software rather than the corpus, and how a rights holder asks
for a work to be withdrawn. [Citing Soranoha](citation.md) gives the citation
forms — and the short version is: name the release, and name the identifier,
because title and author alone do not identify a work.

## What "signed" means, and why you might care

Every release is a signed manifest listing each published work and the SHA-256
of each of its four files. The manifests form an append-only chain, so a
release cannot be silently altered or removed after the fact, and anyone can
check that the file they downloaded is the file the release says it published.

For most work you can ignore this entirely. It matters when you need a result
to be reproducible years later: cite the release, and the exact bytes you read
stay identifiable even if the corpus changes underneath. The
[glossary](user-glossary.md) explains manifest, chain and admission without
assuming any of the cryptography; the
[protocol specification](design/snh-protocol-v1.md) is the full contract for
anyone implementing a verifier.

## Where to go next

- [A worked example](worked-example.md) — one text from Aozora source to TEI
  and plaintext, with every header block explained and code that loads it.
- [Glossary](user-glossary.md) — the words this project uses for its own
  output, in the sense it uses them.
- [Work identifiers](../soranoha/docs/work-identifiers.md) — the form, what it
  promises across releases, and where exact edition identity lives.
- [TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md) — the `snh:`
  attributes in published files and the validation rules that constrain them.
- [Rights](rights.md) and [Citing Soranoha](citation.md).
