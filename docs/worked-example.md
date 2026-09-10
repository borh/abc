# A worked example: 蜘蛛の糸

This walkthrough follows Akutagawa Ryūnosuke's 蜘蛛の糸 (1918, identifier `000092_000879`) from its Aozora Bunko source file through the published TEI and plaintext representations, concluding with Python loading examples.

Every excerpt below is real output, produced by the conversion pipeline from
the Aozora Bunko file at
[`cards/000879/files/92_ruby_164.zip`](https://www.aozora.gr.jp/cards/000879/files/92_ruby_164.zip).
Nothing here is illustrative reconstruction; if you run the same source
through the same pipeline you get the same bytes.

## The source

Aozora Bunko distributes the text as a Shift_JIS file inside a ZIP, with its
own annotation notation carried inline. Four lines from across the file, which
between them cover most of what the converter has to do (the middle two run on
past what is shown here):

```
［＃８字下げ］一［＃「一」は中見出し］

　ある日の事でございます。御釈迦様《おしゃかさま》は極楽の蓮池《はすいけ》のふちを、
　するとその地獄の底に、※［＃「特のへん＋廴＋聿」、第3水準1-87-71］陀多《かんだた》と云う男が
［＃地から１字上げ］（大正七年四月十六日）
```

- `《…》` marks ruby, indicating a reading printed above or beside the preceding characters.
- `※［＃「…」、第3水準1-87-71］` is a **gaiji**: a character not in the source
  file's encoding, described in prose and located in JIS X 0213 by its level
  (第3水準) and its plane-row-cell number (plane 1, row 87, cell 71). Here it is
  犍, the first character of the thief's name 犍陀多.
- `［＃８字下げ］…［＃「一」は中見出し］` is layout plus a structural claim: indent
  eight characters, and the run 一 is a middle-level heading.
- `［＃地から１字上げ］` sets the line flush to the end of the line, one character
  in from the edge.

At the end of the file is the colophon, which names the printed edition the
transcription was made from, and the people who typed and proofread it. It is
not part of the story and it is not thrown away.

## The pipeline

```
Aozora Bunko ZIP  →  primary text  →  AAT  →  parser IR  →  TEI  →  plaintext
                                                          →  Markdown
                                                          →  validation report
```

The first three steps are the Rust converter: it lexes Aozora Bunko notation
into an annotated syntax tree (AAT), then maps that to a parser intermediate
representation recording byte spans in the decoded source for every node.
The Clojure kernel combines the parser IR with catalog metadata to produce TEI,
and projects the three remaining artifacts from the TEI representation.

You can run it yourself from a checkout; see
[the repository README](../README.md) for the development shell.

## The header

The whole `teiHeader` for this work is 46 lines. Taking it block by block. The
excerpts are wrapped for reading, where the published file puts an `<author>`,
a `<taxonomy>` or a `<ruby>` on one line; inside `<ruby>` that wrapping would
be text, so do not copy it back.

### titleStmt: work title and authorship

```xml
<titleStmt>
  <title type="main" xml:lang="ja">蜘蛛の糸</title>
  <title type="reading" xml:lang="ja-Hira">くものいと</title>
  <author>
    <persName xml:lang="ja"><surname>芥川</surname><forename>竜之介</forename><idno type="aozora-person-id">000879</idno></persName>
    <persName xml:lang="ja-Hira"><surname>あくたがわ</surname><forename>りゅうのすけ</forename></persName>
    <persName xml:lang="ja-Latn"><surname>Akutagawa</surname><forename>Ryunosuke</forename></persName>
  </author>
</titleStmt>
```

The title's kana reading is a separate `<title>` rather than an attribute,
because it is a title of the work in another script, not a property of the
first one. The author record includes three `<persName>` forms covering kanji, kana,
and romaji. Each entry specifies its script so applications can sort or display
the appropriate representation. All three originate from Aozora Bunko's catalog and preserve
its recorded spelling without normalisation. For example, the given name is recorded as
竜之介 here, whereas the printed edition cited in the next block uses 龍之介.

The person `<idno>` names whoever issued the identifier, the same way the
publication identifiers below do. `aozora-person-id` is Aozora Bunko's six-digit
人物ID. A person Soranoha has to identify without one gets a locally minted
id under `soranoha-person-id`, so no identifier claims a provenance Aozora Bunko
did not grant.

### publicationStmt: publication details and terms

```xml
<publicationStmt>
  <publisher ref="https://w3id.org/soranoha/">Soranoha</publisher>
  <idno type="soranoha-work-identifier">000092_000879</idno>
  <idno type="aozora-work-id">000092</idno>
  <idno type="aozora-card-url">https://www.aozora.gr.jp/cards/000879/card92.html</idno>
  <date when="2014-09-17">2014-09-17</date>
  <availability status="free">
    <licence target="https://creativecommons.org/publicdomain/mark/1.0/">The underlying work is in the public domain; Soranoha asserts no rights over it.</licence>
    <licence target="https://creativecommons.org/publicdomain/zero/1.0/">Soranoha's encoding of this work, and the artifacts derived from it, are dedicated to the public domain under CC0-1.0. Attribution is requested, not required. Full rights statement: <ptr target="https://w3id.org/soranoha/rights"/></licence>
  </availability>
</publicationStmt>
```

The record contains three identifiers, with the `type` attribute specifying the issuing
authority. Soranoha issues the publication identifier (`000092_000879`), which
appears in every URL for the work. This identifier is not labeled `aozora-*` because
Aozora Bunko issues the work identifier and card number separately rather than as a joined pair.
Embedding this identifier enables an isolated `.xml` file to remain citable without
reference to the original server.

The date is Aozora Bunko's last-modified date for the catalog entry, not a date of
composition or of Soranoha's own processing.

Two `<licence>` elements rather than one, because the two layers are
different: the underlying work is out of copyright and not Soranoha's to
license, and Soranoha's encoding is CC0. A file detached from this site still
carries its own terms. See [rights](rights.md).

`<publisher>` names the publishing entity, so it is `Soranoha` rather than
`Soranoha Aozora Bunko TEI Corpus`: the latter is the corpus title, and `titleStmt`
already carries the title. The `@ref` lets a downloaded file resolve to its
publisher without a lookup. There is no `<pubPlace>`, because a corpus
published only on the web has no place of publication that would not be
invented.

### sourceDesc: printed edition and source byte hashes

```xml
<sourceDesc>
  <bibl><title>芥川龍之介全集2</title><publisher>ちくま文庫、筑摩書房</publisher><date>1986（昭和61）年10月28日</date><note type="input-edition">1996（平成8）年7月15日第11刷</note></bibl>
  <bibl type="first-publication">「赤い鳥」1918（大正7）年7月</bibl>
  <bibl><idno type="source-content-hash">sha256:ba7a3b91f4796022ff7b9d17712a0bdea92724e309ed2f4db9f17f151c8e0712</idno></bibl>
  <bibl><idno type="primary-text-hash">sha256:3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac</idno></bibl>
</sourceDesc>
```

Four separate statements of source, at four levels:

- the **printed edition** the volunteer transcribed from, including which
  printing was used for input;
- **original publication**, identifying where the text first appeared (here, the July
  1918 issue of 「赤い鳥」). This entry is distinguished from the transcribed edition and
  marked `type="first-publication"` to keep the two claims distinct;
- `source-content-hash`, the digest of the bundle's canonical member listing
  rather than of the ZIP file: the sha256 of
  `{"construction":"abc-source-bundle-v1","members":[{"member_hash":…,"path":…}],"primary_text_member":…}`
  in RFC 8785 form. Repackaging the same members leaves it unchanged; a
  reissued member changes it;
- `primary-text-hash`, the sha256 of the text member as distributed, which is
  Shift_JIS bytes rather than the decoded text.

The identifier `000092_000879` denotes the work and stays put across releases;
these hashes are how you say *which text under that identifier*. If you need a
result to be exactly reproducible, cite both.

### encodingDesc: taxonomies and gaiji declarations

```xml
<encodingDesc>
  <styleDefDecl scheme="css"/>
  <classDecl>
    <taxonomy xml:id="ndc"><bibl>日本十進分類法 (Nippon Decimal Classification), as recorded by <title>青空文庫</title> in its 分類番号 field.</bibl></taxonomy>
    <taxonomy xml:id="aozora-orthography"><bibl><title>青空文庫</title> 文字遣い種別: the orthographic style Aozora Bunko records for the transcription.</bibl></taxonomy>
  </classDecl>
  <refsDecl xml:id="source-spans">
    <p>A note of type source-span has an xml:id of the form source-START-END, giving the extent in UTF-8 bytes of the decoded primary text identified in sourceDesc. <ptr target="https://w3id.org/soranoha/ns/tei"/></p>
  </refsDecl>
  <charDecl>
    <char xml:id="gaiji-3-1-87-71">
      <mapping type="unicode">犍</mapping>
      <localProp name="rawMarker" value="特のへん＋廴＋聿"/>
    </char>
  </charDecl>
</encodingDesc>
```

Every gaiji in the text gets one `<char>` declaration here, and the body
references it. The declaration carries both halves of the fact:

- `<mapping type="unicode">` is the character the converter resolved it to.
  It is a claim, made from the JIS X 0213 code point in the marker.
- `rawMarker` preserves the source notation verbatim (`特のへん＋廴＋聿`, "the left
  radical of 特, plus 廴, plus 聿"). Retaining this text allows users to verify
  or adjust the character mapping without re-inspecting the upstream source file.

The `xml:id` carries the marker's JIS X 0213 coordinates with everything
outside `[A-Za-z0-9_.-]` folded to hyphens, so `第3水準1-87-71` becomes
`3-1-87-71`: the leading 3 is the level, and `1-87-71` is the plane, row and
cell. The same character therefore resolves to the same declaration
everywhere.

`<styleDefDecl scheme="css"/>` declares that `@style` attributes in the
body contain CSS properties. These record source layout details that lack
native TEI attributes, such as character-width indents, measures, and text alignment.

`<refsDecl>` says what unit the `@source` references that most elements carry
are measured in, and points at the vocabulary for the rest. The offsets section
below works through one of them.

### profileDesc: language and classification

```xml
<profileDesc>
  <langUsage><language ident="ja">日本語</language></langUsage>
  <textClass>
    <classCode scheme="#ndc">K913</classCode>
    <classCode scheme="#aozora-orthography">新字新仮名</classCode>
  </textClass>
</profileDesc>
```

`K913` is Aozora Bunko's own NDC-style class. The `K` prefix marks children's
material and is not an NDC main class, which is why works like this one appear
under その他 rather than under 9 文学 in the site's NDC index.

`新字新仮名` is the orthography of the transcription: modern characters,
modern kana. This is the single most consequential field for anyone studying
the language rather than the story. It is populated for every work, and the
corpus divides into 新字新仮名 (10791), 新字旧仮名 (4569), 旧字旧仮名 (2183),
旧字新仮名 (93) and その他 (19), counted over the 17,655 catalog works that have
a text file. Those counts move with the upstream catalog. A historical-kana
study that mixes the values is not measuring what it thinks it is measuring,
and one text can exist in two of them: 銭形平次捕物控 001 金色の処女 is in the
archive twice, as 旧字旧仮名 under work id 054695 and as 新字新仮名 under
056372. That is not an isolated case. Across the works that have a text file,
568 title-and-subtitle pairs are recorded under more than one orthography,
covering 1343 works. Each such pair is two works with two identifiers, and
this is the field that tells them apart.

Both `scheme` attributes reference taxonomies declared in `encodingDesc`
rather than bare string identifiers, making the authority explicit. In both
cases, the classification originates from Aozora Bunko rather than Soranoha.

## The body

### Ruby

```xml
<ruby type="furigana" rend="right" source="#source-733-769">
  <rb>御釈迦様</rb><rt>おしゃかさま</rt>
</ruby>
```

`<rb>` is the base text, `<rt>` the reading, as in HTML `<ruby>`.
`@rend` records which side of the line the source placed it on. Plaintext
projections extract `<rb>` and drop `<rt>`.

A ruby base can itself contain child markup, as when the thief's name combines a gaiji
with standard characters:

```xml
<ruby type="furigana" rend="right" source="#source-1856-1942">
  <rb><g ref="#gaiji-3-1-87-71" source="#source-1856-1918">犍</g><seg source="#source-1918-1924">陀多</seg></rb>
  <rt>かんだた</rt>
</ruby>
```

so read `<rb>` with something that descends into it, not with a "text of first
child" shortcut.

### Gaiji in the body

```xml
<g ref="#gaiji-3-1-87-71" source="#source-2071-2133">犍</g>
```

The element's content is the mapped character, so a naïve text extraction
reads 犍 and gets a sensible result. `@ref` points at the declaration in the
header when you want the original marker instead.

### Headings and layout

```xml
<div type="layout" source="#source-633-694" style="padding-inline-start: 8em">
  <div>
    <head n="2" source="#source-654-690"><seg source="#source-654-657">一</seg></head>
  </div>
</div>
```

Two separate facts, kept separate. The outer `<div type="layout">` is the
source's ８字下げ instruction, expressed as geometry. The inner `<head n="2">`
represents the structural heading indicated by the source's 中見出し markup.
Layout is not inferred from structure, and structure is not inferred from layout;
because the source specified both independently, both are recorded.

The dateline at the end is layout only:

```xml
<div type="layout" source="#source-11439-11502" style="padding-inline-end: 1em; text-align: right">
  <p><seg source="#source-11469-11502">（大正七年四月十六日）</seg></p>
</div>
```

### Source offsets

Almost every element carries `@source`, pointing at a `<note type="source-span">`
in `<back>`:

```xml
<seg source="#source-694-733">ある日の事でございます。</seg>
```

```xml
<note type="source-span" xml:id="source-694-733" n="20"/>
```

The note is empty because its identifier is the extent: `source-694-733` gives
the start and end byte offsets into the decoded UTF-8 source text, and `n` is
the line the extent begins on. Which text those offsets index is stated once
for the whole document, by the `primary-text-hash` in `sourceDesc`:

```xml
<idno type="primary-text-hash">sha256:3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac</idno>
```

That hash covers the member as distributed, and the offsets index its UTF-8
decoding, so the two are one step apart: decode with the encoding named in
`<note type="source-decoding">` at the end of `<back>`, re-encode to UTF-8,
then slice. Naming the hash once rather than on every note is what keeps
offsets from being applied to mismatched bytes without repeating 64 characters
several thousand times per file:

```python
import hashlib

raw = open("kumono_ito.txt", "rb").read()   # the ZIP's one member
assert hashlib.sha256(raw).hexdigest() == (
    "3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac")
source = raw.decode("cp932")                # windows-31j, from source-decoding
assert source.encode("utf-8")[694:733].decode("utf-8") == "　ある日の事でございます。"
```

That is 39 bytes for 13 characters, the first of which is the leading
ideographic space: the span covers the source's indent character together with the text,
because the paragraph's `text-indent` was derived from it. Offsets index the
source, not the output.

Byte offsets allow the corpus to serve as verifiable evidence rather than
reading matter alone. Annotations computed over the text, such as token spans,
alignments, or named entities, can be referenced directly against source bytes
and verified independently without specialized pipeline tooling.

### What is at the end

```xml
<back>
  <div type="source">
    <note type="source-attribution" source="#source-11510-11785"><seg type="source-line">底本：「芥川龍之介全集2」ちくま文庫、筑摩書房</seg><lb/>…</note>
    <note type="transcriber-note" source="#source-11785-12154"><seg type="source-line">入力：平山誠、野口英司</seg><lb/>…</note>
    <note type="source-span" xml:id="source-10080-10141" n="36"/>
    …
  </div>
  <note type="parser-completion" n="true">The n attribute is true when no parser diagnostic of error severity was raised for this source. …</note>
  <note type="source-decoding">windows-31j</note>
</back>
```

The colophon and the transcribers' note are the source's own back matter, kept
as it was written. The `source-span` notes are the offset table, and
`source-decoding` names the encoding the source was read as, which is what
makes the offsets reproducible.

`parser-completion` is always present: its `@n` attribute is `true` when no
parser diagnostic of error severity was raised. `interpretation-problem` and
`parser-diagnostic` notes appear when the converter encounters ambiguities or
uninterpreted syntax, embedding diagnostics directly in the TEI file rather
than discarding them to build logs.

## The plaintext

```
一
　ある日の事でございます。御釈迦様は極楽の蓮池のふちを、独りでぶらぶら御歩きになっていらっしゃいました。…
```

Reading text only: ruby readings gone, gaiji present as their mapped
characters, headings on their own lines, the source's own indent characters
preserved. Editorial notes, apparatus and the offset table are not in it.

Plaintext is generated from the TEI representation under a single documented
policy: when the encoding contains alternatives, the generator selects one lemma
from an apparatus and one branch of an editorial choice. This guarantees that the
plaintext and the website reading view present identical text.

## The other two projections

`markdown` is CommonMark with the ruby kept as inline HTML, so a reader that
does not parse XML still gets the readings:

```markdown
## 一

ある日の事でございます。<ruby><rb>御釈迦様</rb><rt>おしゃかさま</rt></ruby>は極楽の<ruby><rb>蓮池</rb><rt>はすいけ</rt></ruby>のふちを、…
```

Where the source used an emphasis mark, the Markdown projection emits a `<span>`
containing the original `@rend` attribute and corresponding CSS styling. For
example, 白ゴマ傍点 is represented as `text-emphasis-style: open sesame` rather than
generic bold text.

`tei-validation` is a JSON report naming the profile the file was checked
against by hash, the status of each of the three layers (well-formedness,
Relax NG, Schematron), and every rule that fired with its identifier and
severity. A pass is evidence about the XML, not about
the transcription: see
[TEI validation](../soranoha/docs/tei-validation.md) for what each layer does
and does not establish.

## Loading it in Python

No dependencies; `xml.etree.ElementTree` from the standard library is enough.

```python
import xml.etree.ElementTree as ET

TEI = "{http://www.tei-c.org/ns/1.0}"
XML = "{http://www.w3.org/XML/1998/namespace}"
# The work page's TEI download is named for the author, Aozora Bunko's own stem for
# the text, and the identifier.
tree = ET.parse("Akutagawa_Ryunosuke-kumono_ito-000092_000879.xml")

def text(node):
    """All text under a node, descending into ruby bases and gaiji."""
    return "".join(node.itertext()) if node is not None else ""
```

**Bibliography, from the header:**

```python
head = tree.find(f"{TEI}teiHeader/{TEI}fileDesc")
title = head.findtext(f"{TEI}titleStmt/{TEI}title[@type='main']")
author = head.find(f"{TEI}titleStmt/{TEI}author/{TEI}persName")
edition = head.find(f"{TEI}sourceDesc/{TEI}bibl")

print(title)                                              # 蜘蛛の糸
print(author.findtext(f"{TEI}surname")
      + author.findtext(f"{TEI}forename"))                # 芥川竜之介
print(edition.findtext(f"{TEI}publisher"))                # ちくま文庫、筑摩書房
```

Name the parts you want rather than taking all the text under `<persName>`:
the Aozora Bunko person id is an `<idno>` inside it, so `text(author)` would give
you `芥川竜之介000879`.

**Every ruby pair in the work:**

```python
rubies = [(text(r.find(f"{TEI}rb")), text(r.find(f"{TEI}rt")))
          for r in tree.iter(f"{TEI}ruby")]

print(len(rubies))            # 68
print(rubies[:3])             # [('御釈迦様', 'おしゃかさま'), ('蓮池', 'はすいけ'), ('蓮', 'はす')]
print(rubies[18])             # ('犍陀多', 'かんだた')  (gaiji inside the base)
```

Extracting furigana illustrates why TEI is preferred over plaintext for structured
analysis, because plaintext omits ruby readings and editorial markup.

**Every gaiji, with the marker the source used:**

```python
chars = {c.get(f"{XML}id"): c for c in tree.iter(f"{TEI}char")}

for g in tree.iter(f"{TEI}g"):
    decl = chars[g.get("ref").lstrip("#")]
    marker = decl.find(f"{TEI}localProp[@name='rawMarker']").get("value")
    print(g.text, "from", marker)        # 犍 from 特のへん＋廴＋聿
```

**A passage and where it came from:**

```python
def extent(reference):
    _, start, end = reference.lstrip("#").split("-")
    return int(start), int(end)

# The source bytes the offsets index, opened again as in "Source offsets".
source = open("kumono_ito.txt", "rb").read().decode("cp932")
body = tree.find(f"{TEI}text/{TEI}body")
for seg in body.iter(f"{TEI}seg"):
    ref = seg.get("source")
    if ref:
        start, end = extent(ref)
        print(text(seg), source.encode("utf-8")[start:end].decode("utf-8"))
        break
```

The reference is the extent, so nothing needs looking up. The `source-span`
notes in `<back>` are there to be pointed at, and to carry the source line in
`@n`; the offsets are in their identifiers. On the body elements shown here
`@source` holds one reference, but a `<note>` that closes a span carries two,
space-separated, so split on whitespace first if you widen the loop past
`<seg>`.

**And if all you want is the text**, take the `plaintext` artifact rather than
re-deriving it. It is published beside the TEI, generated under the documented
reading policy, and hashed into the same signed manifest:

```python
import urllib.request
body = urllib.request.urlopen(
    "https://soranoha.org/works/000092_000879/plaintext").read().decode("utf-8")
```

## Where to go next

- [Start here](start-here.md) covers corpus scope and bulk access.
- [Glossary](user-glossary.md) defines admission, manifests, cryptographic chains, and work-edition-document distinctions.
- [TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md) specifies `snh:` attributes and validation rule identifiers.
- [TEI validation](../soranoha/docs/tei-validation.md) explains validation profiles and schema coverage.
