# Glossary

The words this project uses for its own output, in the sense it uses them.
Several are ordinary English words used narrowly here, which is exactly when a
glossary earns its place.

For the internal names of parser and rendering behaviour, see
[parser and rendering invariants](parser-invariants.md).

## admission

The decision to include a work in a release. A work is admitted when its
rights standing has been established against a recorded basis and that basis
still holds at the moment of publication.

Admission is not a judgement about the quality of the encoding, and passing
validation does not admit a work. The two are deliberately separate: a work
can be encoded perfectly and not be published, and being published says
nothing about how good the encoding is.

Each release's manifest records the admission evidence it relied on, so "why
was this work in that release" has an answer years later.

## assessment

The evaluation that produces an admission basis: what is claimed about a
work's rights standing, on what evidence, effective from when, and in which
jurisdiction (Japan).

Assessment records **facts with premises**, not verdicts. A fact says what was
established, from which evidence, as of which date. If the evidence changes or
becomes unreachable, the fact becomes unavailable rather than silently
staying true — which is why a work can leave a release without anyone having
made a new claim about it.

Assessment is not legal advice and Soranoha is not a rights clearinghouse. See
[reliance](#reliance) for the basis used for most works.

## reliance

The default admission basis: Aozora Bunko's own published copyright-expired
classification for one exact edition, relied on as an attributed upstream
assertion.

Reliance is recorded as what it is. It says "Aozora Bunko classifies this
edition as copyright-expired, and here are the bytes of the page that says
so", not "Soranoha has independently determined that every contributor's
rights have expired". A recorded exception, or a conflicting reviewed finding,
prevents reliance for that work.

Reliance is re-checked against the live source at release time. Evidence
retained from an earlier check cannot substitute for a current one.

## fidelity

How much of the source's markup survived into the encoding — a measurement,
not a grade, and not a rights or correctness claim.

The corpus publishes two separate pieces of evidence about it:

- **source accountability** scans the raw Aozora file for every recognisable
  annotation, independently of the parser, and records each one with its exact
  spelling and byte span.
- **interpretation coverage** joins that scan to what the converter says it
  understood. Occurrences the converter claimed are distinguished from
  occurrences left `unaccounted`.

Neither one certifies that the exported semantics are right. A scan finding no
unaccounted markers means nothing was *missed*, not that everything found was
interpreted *correctly*. Where the converter had a doubt, it is written into
the published TEI as an `interpretation-problem` note rather than kept in a
log.

## work, edition, document

Three different things that are easy to conflate, kept apart throughout:

- **work** — the intellectual work. 蜘蛛の糸 is one work regardless of which
  book it is printed in. This is what a Soranoha identifier denotes.
- **edition** — a particular source: a printed edition, or the Aozora
  transcription made from it. `source_content_hash` in the manifest, and
  `<sourceDesc>` in each TEI file, identify the edition.
- **document** — a particular file of bytes. Identified by its SHA-256. Two
  releases of the same work from the same edition can still be different
  documents, because the converter improved.

Cite the identifier for the work, the identifier plus `source_content_hash`
for the edition, and the artifact id for exact bytes. See
[work identifiers](../soranoha/docs/work-identifiers.md) and
[citing Soranoha](citation.md).

The same three-way distinction, plus `person`, is what
[external links](../soranoha/docs/external-links.md) uses when it records a
correspondence to an outside catalog: a link to a work and a link to an
edition are different claims and are never merged.

## artifact

One published file, identified by its content: `snh:1:<type>:<sha256>`. The
types published per work are `tei`, `plaintext`, `markdown` and
`tei-validation`.

An artifact id names bytes, so it can be resolved without trusting anyone: if
you have the bytes, you can compute the id yourself and see whether it matches.

## manifest

The signed record of one release. It lists every admitted work with its
identifier, its `source_content_hash`, and the id and byte length of each of
its four artifacts; the rights grant those artifacts are published under; the
admission evidence relied on; the release catalog; and the previous manifest.

The manifest is the authority. The site's pages, indexes and reading views are
generated from it and are not part of it — if a page and a manifest disagree,
the manifest is right. `https://soranoha.org/releases/latest` serves the
current one.

## chain

The sequence of manifests, each naming the one before it, from the first
release to the current one.

The chain is **append-only**: a new release adds a manifest, and no release
alters an earlier one. This is what lets a citation stay meaningful. A
withdrawal removes a work from the current release; it cannot remove it from a
past one, and the past manifests keep saying what they said.

The two signing roles are separate and one of them is kept offline, so routine
publication and governance acts like withdrawal are authorised differently.
The full rules are in the
[protocol specification](design/snh-protocol-v1.md).

## release

One published state of the corpus: a manifest, its signature, and the
artifacts it names. Identified by the manifest's SHA-256, the 64-character hex
string the site calls the release head.

The corpus changes between releases. A citation that names only "Soranoha"
does not identify the bytes you read; a citation that names the release does.

## catalog

The bibliography of every work in one release — identifiers, titles and their
readings, contributors, first publication, orthography, NDC class, Aozora card
URL and printed source edition — as one JSON file at
`https://soranoha.org/catalog.json`.

The catalog is part of the signed release, not a convenience export. It
carries facts, not renderings: no download filenames, no formatted citation
strings, no DOIs. Those are the serving layer's business, because they can be
corrected and a signed record cannot.

## withdrawal

Removing a work from the current release, by a signed governance event that
states a reason code and a public statement.

Withdrawal stops current distribution. It does not erase bytes: earlier
manifests still name the work, its artifacts remain retrievable by their
hashes, and clones, mirrors and archives are unaffected. The withdrawal record
stays at `https://soranoha.org/withdrawn/<identifier>.json`.

If you hold rights in a published work, [rights](rights.md) says what to send
and what happens next.

## body-v1

The reading policy that decides which text is *the* text, used by the
plaintext projection, by the site's reading view, and by any annotation layer
that targets the corpus.

It walks the TEI body and, where the encoding offers alternatives, chooses:
one lemma from an apparatus, the first supported branch of an editorial
choice, base text rather than ruby readings, and no editorial notes or figure
descriptions. Unresolved gaiji are excluded from the annotatable region rather
than being silently replaced.

The name matters because it is versioned. An annotation stated against
`body-v1` says which policy produced the offsets it uses, so it stays checkable
if the policy ever changes.

## gaiji (外字)

A character the Aozora source file could not encode, written instead as a
prose description with, usually, a JIS X 0213 code point:
`※［＃「特のへん＋廴＋聿」、第3水準1-87-71］`.

Published TEI carries both halves: the character the converter resolved it to,
and the original marker verbatim, so the resolution can be checked or
disagreed with. See
[the worked example](worked-example.md#encodingdesc--the-gaiji-declarations).

## ruby (ルビ)

A reading printed alongside or above the characters it applies to — furigana.
In the source it is written `蓮池《はすいけ》`; in TEI it is `<ruby>` with an
`<rb>` base and an `<rt>` reading.

Ruby readings are in the TEI and deliberately not in the plaintext, which
carries base text only.

## source offset

A byte range into the Aozora source text, after decoding it to UTF-8, recorded
for almost every element of the published TEI.

Offsets let a claim about the corpus be stated against the source rather than
against a particular tool's output, and checked by someone who has never seen
that tool. Each offset names the exact text it indexes by hash, so it cannot
be applied to the wrong bytes without the mismatch being visible.

## snh

The publication protocol: the wire format of manifests, governance events,
admission evidence and artifact ids, and the rules a verifier applies to them.
Also the prefix of the extension vocabulary in published TEI
(`https://w3id.org/soranoha/ns/tei`).

You do not need it to read the corpus. You need it to check the corpus
independently, or to build something that does. See the
[protocol specification](design/snh-protocol-v1.md) and the
[TEI extension vocabulary](../soranoha/docs/tei-vocabulary.md).
