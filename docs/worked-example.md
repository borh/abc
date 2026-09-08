# A worked example: 蜘蛛の糸

This follows one text — Akutagawa Ryūnosuke's 蜘蛛の糸 (1918), Soranoha
identifier `000092_000879` — from the Aozora Bunko source file through to the
published TEI and plaintext, and then loads it in Python.

Every excerpt below is real output, produced by the conversion pipeline from
the Aozora file at
[`cards/000879/files/92_ruby_164.zip`](https://www.aozora.gr.jp/cards/000879/files/92_ruby_164.zip).
Nothing here is illustrative reconstruction; if you run the same source
through the same pipeline you get the same bytes.

## The source

Aozora Bunko distributes the text as a Shift_JIS file inside a ZIP, with its
own annotation notation carried inline. Four lines from across the file, which
between them cover most of what the converter has to do:

```
［＃８字下げ］一［＃「一」は中見出し］

　ある日の事でございます。御釈迦様《おしゃかさま》は極楽の蓮池《はすいけ》のふちを、
　するとその地獄の底に、※［＃「特のへん＋廴＋聿」、第3水準1-87-71］陀多《かんだた》と云う男が
［＃地から１字上げ］（大正七年四月十六日）
```

- `《…》` is ruby — a reading printed above (or beside) the characters before it.
- `※［＃「…」、第3水準1-87-71］` is a **gaiji**: a character not in the source
  file's encoding, described in prose and given a JIS X 0213 plane-row-cell
  number. Here it is 犍, the first character of the thief's name 犍陀多.
- `［＃８字下げ］…［＃「一」は中見出し］` is layout plus a structural claim: indent
  eight characters, and the run 一 is a middle-level heading.
- `［＃地から１字上げ］` sets the line flush to the end of the line, one character
  in from the edge.

At the end of the file is the colophon, which names the printed edition the
transcription was made from, and the people who typed and proofread it. It is
not part of the story and it is not thrown away.

## The pipeline

```
Aozora ZIP  →  primary text  →  AAT  →  parser IR  →  TEI  →  plaintext
                                                          →  Markdown
                                                          →  validation report
```

The first three steps are the Rust converter: it lexes the Aozora notation
into an annotated syntax tree (AAT), then maps that to a parser intermediate
representation which records, for every node, the byte span in the decoded
source that produced it. The last steps are Clojure: the parser IR plus the
work's catalog metadata become TEI, and the three other artifacts are derived
from the TEI's own reading of itself.

You can run it yourself from a checkout; see
[the repository README](../README.md) for the development shell.

## The header

The whole `teiHeader` for this work is 40 lines. Taking it block by block:

### titleStmt — what the work is and who made it

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
first one. The author has three `<persName>` forms — kanji, kana, romaji —
each marked with the script it is in, so a tool can pick the one it can sort
or display. All of it comes from Aozora's catalog, and is kept as the catalog
has it rather than normalised: the given name is 竜之介 here, whereas the
printed edition in the next block uses 龍之介.

The person `<idno>` names whoever issued the identifier, the same way the
publication identifiers below do. `aozora-person-id` is Aozora's six-digit
人物ID. A person Soranoha has to identify without one gets a locally minted
id under `soranoha-person-id`, so no identifier claims a provenance Aozora
did not grant.

### publicationStmt — who published this file, under what terms

```xml
<publicationStmt>
  <publisher ref="https://soranoha.org">Soranoha</publisher>
  <idno type="soranoha-work-identifier">000092_000879</idno>
  <idno type="aozora-work-id">000092</idno>
  <idno type="aozora-card-url">https://www.aozora.gr.jp/cards/000879/card92.html</idno>
  <date when="2014-09-17">2014-09-17</date>
  <availability status="free">
    <licence target="https://creativecommons.org/publicdomain/mark/1.0/">The underlying work is in the public domain; Soranoha asserts no rights over it.</licence>
    <licence target="https://creativecommons.org/publicdomain/zero/1.0/">Soranoha's encoding of this work, and the artifacts derived from it, are dedicated to the public domain under CC0-1.0. Attribution is requested, not required. Full rights statement: <ptr target="https://soranoha.org/rights"/></licence>
  </availability>
</publicationStmt>
```

Three identifiers, and the `type` says who issued each. Soranoha issues the
publication identifier — the same `000092_000879` that appears in every URL
for this work — which is why it is not labelled `aozora-*`: Aozora issues the
work id and the card, not the pair. It is in the file so that a downloaded
`.xml`, read years later with no site around it, can still say what to cite it
as.

The date is Aozora's last-modified date for the catalog entry, not a date of
composition or of Soranoha's own processing.

Two `<licence>` elements rather than one, because the two layers are
different: the underlying work is out of copyright and not Soranoha's to
license, and Soranoha's encoding is CC0. A file detached from this site still
carries its own terms. See [rights](rights.md).

`<publisher>` names the publishing entity, so it is `Soranoha` rather than
`Soranoha Aozora TEI Corpus`: the latter is the corpus title, and `titleStmt`
already carries the title. The `@ref` lets a downloaded file resolve to its
publisher without a lookup. There is no `<pubPlace>`, because a corpus
published only on the web has no place of publication that would not be
invented.

### sourceDesc — which printed edition, and which exact bytes

```xml
<sourceDesc>
  <bibl><title>芥川龍之介全集2</title><publisher>ちくま文庫、筑摩書房</publisher><date>1986（昭和61）年10月28日</date><note type="input-edition">1996（平成8）年7月15日第11刷</note></bibl>
  <bibl><idno type="source-content-hash">sha256:3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac</idno></bibl>
  <bibl type="first-publication">「赤い鳥」1918（大正7）年7月</bibl>
  <bibl><idno type="primary-text-hash">sha256:3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac</idno></bibl>
</sourceDesc>
```

Four separate statements of source, at four levels:

- the **printed edition** the volunteer transcribed from, including which
  printing was used for input;
- **where the text first appeared** — a magazine issue, here 「赤い鳥」 of July
  1918 — which is a different claim from the edition that was keyed, and is
  marked `type="first-publication"` so the two are not confused;
- `source-content-hash`, the canonical identity of the Aozora source bundle —
  this is what changes when Aozora reissues the text;
- `primary-text-hash`, the hash of the text member itself.

The identifier `000092_000879` denotes the work and stays put across releases;
these hashes are how you say *which text under that identifier*. If you need a
result to be exactly reproducible, cite both.

### encodingDesc — the taxonomies and the gaiji declarations

```xml
<encodingDesc>
  <styleDefDecl scheme="css"/>
  <classDecl>
    <taxonomy xml:id="ndc"><bibl>日本十進分類法 (Nippon Decimal Classification), as recorded by <title>青空文庫</title> in its 分類番号 field.</bibl></taxonomy>
    <taxonomy xml:id="aozora-orthography"><bibl><title>青空文庫</title> 文字遣い種別: the orthographic style Aozora Bunko records for the transcription. An upstream classification, not a Soranoha judgement.</bibl></taxonomy>
  </classDecl>
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
- `rawMarker` is what the source actually said — `特のへん＋廴＋聿`, "the left
  radical of 特, plus 廴, plus 聿". It is kept verbatim so you can check the
  mapping, or disagree with it, without going back to Aozora.

The `xml:id` encodes the JIS plane-row-cell number from the marker
(`3-1-87-71` = plane 3, 1-87-71), so the same character resolves to the same
declaration everywhere.

`<styleDefDecl scheme="css"/>` declares that the `@style` attributes in the
body are CSS. They carry source-derived geometry that TEI has no attribute
for — indents in character widths, measures, alignment.

### profileDesc — language and classification

```xml
<profileDesc>
  <langUsage><language ident="ja">日本語</language></langUsage>
  <textClass>
    <classCode scheme="#ndc">K913</classCode>
    <classCode scheme="#aozora-orthography">新字新仮名</classCode>
  </textClass>
</profileDesc>
```

`K913` is Aozora's own NDC-style class. The `K` prefix marks children's
material and is not an NDC main class, which is why works like this one appear
under その他 rather than under 9 文学 in the site's NDC index.

`新字新仮名` is the orthography of the transcription: modern characters,
modern kana. This is the single most consequential field for anyone studying
the language rather than the story. It is populated for every work, and the
corpus divides into 新字新仮名 (8812), 新字旧仮名 (3817), 旧字旧仮名 (1783),
旧字新仮名 (23) and その他 (17). A historical-kana study that mixes them is
not measuring what it thinks it is measuring, and the value varies inside a
single series: 銭形平次捕物控 001 through 004 are 旧字旧仮名 while 005 is
新字新仮名.

Both `scheme` attributes point at a taxonomy declared in `encodingDesc`
rather than naming a scheme in a bare string, so a reader can see who did the
classifying — Aozora Bunko, in both cases, not Soranoha.

## The body

### Ruby

```xml
<ruby type="furigana" rend="right" source="#source-733-769">
  <rb>御釈迦様</rb><rt>おしゃかさま</rt>
</ruby>
```

`<rb>` is the base text, `<rt>` the reading, exactly as in HTML's `<ruby>`.
`@rend` records which side of the line the source put it on. A projection that
wants base text takes `<rb>` and drops `<rt>`; that is what `plaintext` does.

A ruby base can itself contain markup — the thief's name is a gaiji followed
by ordinary characters:

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
is the structural claim the source made with 中見出し — a middle-level
heading. Layout is not inferred from structure, and structure is not inferred
from layout; the source stated both, so both are recorded.

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
<note type="source-span" xml:id="source-694-733"
      corresp="urn:sha256:3c686946ec0a7f31f9de7a9a7231d93fe97c7323a5172c268a48f180fdb7bcac">{
  "coordinate_system": "decoded_utf8",
  "end": 733,
  "line": 20,
  "start": 694
}</note>
```

`start` and `end` are **byte** offsets into the source text after decoding
Shift_JIS to UTF-8, and `corresp` names the exact text they index — the same
`primary-text-hash` from the header, so an offset can never be silently
applied to different bytes. Check one:

```python
source = open("kumono_ito.txt", "rb").read().decode("cp932")
assert source.encode("utf-8")[694:733].decode("utf-8") == "　ある日の事でございます。"
```

That is 39 bytes for 13 characters, and the 13th is the leading ideographic
space: the span covers the source's indent character together with the text,
because the paragraph's `text-indent` was derived from it. Offsets index the
source, not the output.

This is the feature that makes the corpus usable as evidence rather than only
as reading matter. Any annotation you compute over the text — a token span, an
alignment, a named entity — can be stated against the source bytes and checked
against them by someone who has never seen your tooling.

### What is at the end

```xml
<back>
  <div type="source">
    <note type="source-attribution" source="#source-11510-11785"><seg type="source-line">底本：「芥川龍之介全集2」ちくま文庫、筑摩書房</seg><lb/>…</note>
    <note type="transcriber-note" source="#source-11785-12154"><seg type="source-line">入力：平山誠、野口英司</seg><lb/>…</note>
    <note type="source-span" xml:id="source-10080-10141" …>…</note>
    …
  </div>
</back>
```

The colophon and the transcribers' note are the source's own back matter, kept
as it was written. The `source-span` notes are the offset table. Also here,
when the converter has anything to declare, are `note` elements of type
`interpretation-problem`, `parser-diagnostic` and `parser-completion`: what the
converter could not interpret, and its own statement that a completed parse is
not proof of exhaustive interpretation. They are in the file so that a doubt
recorded during conversion travels with the text rather than staying in a log.

## The plaintext

```
一
　ある日の事でございます。御釈迦様は極楽の蓮池のふちを、独りでぶらぶら御歩きに
なっていらっしゃいました。…
```

Reading text only: ruby readings gone, gaiji present as their mapped
characters, headings on their own lines, the source's own indent characters
preserved. Editorial notes, apparatus and the offset table are not in it.

The plaintext is generated from the TEI's reading of itself, under one
documented policy — when the encoding offers alternatives, one lemma from an
apparatus and one branch of an editorial choice — so the plaintext and the
reading view on the site cannot disagree about what the text says.

## The other two projections

`markdown` is CommonMark with the ruby kept as inline HTML, so a reader that
does not parse XML still gets the readings:

```markdown
## 一

ある日の事でございます。<ruby><rb>御釈迦様</rb><rt>おしゃかさま</rt></ruby>は極楽の<ruby><rb>蓮池</rb><rt>はすいけ</rt></ruby>のふちを、
```

Where the source used an emphasis mark, the Markdown carries a `<span>` with
the source's own `@rend` recorded and CSS that draws that mark — 白ゴマ傍点
becomes `text-emphasis-style: open sesame`, not a generic bold.

`tei-validation` is a JSON report naming the profile the file was checked
against by hash, the Relax NG and Schematron layers, and every rule that fired
with its identifier and severity. A pass is evidence about the XML, not about
the transcription: see
[TEI validation](../soranoha/docs/tei-validation.md) for what each layer does
and does not establish.

## Loading it in Python

No dependencies; `xml.etree.ElementTree` from the standard library is enough.

```python
import xml.etree.ElementTree as ET

TEI = "{http://www.tei-c.org/ns/1.0}"
XML = "{http://www.w3.org/XML/1998/namespace}"
tree = ET.parse("000092_000879.xml")

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
the Aozora person id is an `<idno>` inside it, so `text(author)` would give
you `芥川竜之介000879`.

**Every ruby pair in the work:**

```python
rubies = [(text(r.find(f"{TEI}rb")), text(r.find(f"{TEI}rt")))
          for r in tree.iter(f"{TEI}ruby")]

print(len(rubies))            # 68
print(rubies[:3])             # [('御釈迦様', 'おしゃかさま'), ('蓮池', 'はすいけ'), ('蓮', 'はす')]
print(rubies[18])             # ('犍陀多', 'かんだた')  — gaiji inside the base
```

A furigana list like this is the reason to take TEI rather than plaintext:
the readings are an editorial layer the source carried, and the plaintext
omits them.

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
spans = {n.get(f"{XML}id"): n.text
         for n in tree.iter(f"{TEI}note") if n.get("type") == "source-span"}

body = tree.find(f"{TEI}text/{TEI}body")
for seg in body.iter(f"{TEI}seg"):
    ref = seg.get("source")
    if ref:
        print(text(seg), spans[ref.lstrip("#")])
        break
```

**And if all you want is the text**, take the `plaintext` artifact rather than
re-deriving it. It is published beside the TEI, generated under the documented
reading policy, and hashed into the same signed manifest:

```python
import urllib.request
body = urllib.request.urlopen(
    "https://soranoha.org/works/000092_000879/plaintext").read().decode("utf-8")
```

## Where to go next

- [Start here](start-here.md) — the corpus, its scale, and getting all of it.
- [Glossary](user-glossary.md) — admission, manifest, chain, fidelity, and the
  work/edition/document distinction.
- [TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md) — the `snh:`
  attributes and the validation rule identifiers in `tei-validation`.
- [TEI validation](../soranoha/docs/tei-validation.md) — what the profile
  checks, and what a schema pass does and does not establish.
