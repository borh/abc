# Rights and licensing

This is the full statement that every signed release manifest names in
`rights.statement_url`, and that every published TEI file names inside
`publicationStmt/availability`, in both cases as
<https://w3id.org/soranoha/rights>. Those bytes are signed and cannot be
corrected later, so they name an identifier rather than whichever host is
serving the corpus.

> **Not yet resolvable.** The `w3id.org/soranoha/` redirects are registered as
> a precondition of public genesis, and are not registered yet: this URL and
> the publisher identifier both return 404 today. They are stated here in
> their final form because that form is what the signed bytes will name.

## The short version

You may copy, redistribute, adapt, translate, mine, and republish everything
Soranoha publishes, for any purpose, commercial or not, without asking and
without payment. Attribution is requested, not required.

If you use the corpus in research, please cite it (see
[citation](citation.md)). That is a scholarly norm here, not a licence
condition.

## Two distinct rights layers

A published work involves two distinct rights layers.

**The underlying works** are texts from [Aozora
Bunko](https://www.aozora.gr.jp/) whose copyright has expired. Soranoha
neither holds nor claims any right in them. Aozora Bunko's own handling rules
([取り扱い規準](https://www.aozora.gr.jp/guide/kijyunn.html)) state that files
for copyright-expired works may be freely copied, redistributed and shared,
whether for payment or not, and freely performed, recited and adapted, with
nothing owed to Aozora Bunko in permission or fee. Altering the text itself is
allowed within the scope of Article 20(2)(iv) of the Copyright Act, which is
what covers converting the file format, changing notation, and modernising
旧字旧仮名.

Two things the rules ask for rather than require: that the credit block naming
the work, its contributors, its 底本 and its 入力者 and 校正者 not be removed
(希望), and that a change of 底本 or notation be accompanied by a record of what
was changed (期待). Soranoha does both. The credit block survives into every TEI
file's `<back>` and onto every work page, and every work is published with its
validation report and its recorded divergences from the source.

Soranoha publishes only works its assessment finds free to publish; the
reasoning is recorded in
[`soranoha/docs/evidence/aozora-rights-source-contract.md`](../soranoha/docs/evidence/aozora-rights-source-contract.md).

**Soranoha's encoding** (TEI markup, plaintext and Markdown
projections, validation reports, catalog, and release manifests)
is dedicated to the public domain under [CC0
1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). Where that
encoding attracts copyright or a database right at all, those rights are
waived.

CC0 rather than CC BY was selected because TEI transcription is largely
a mechanical conversion of Aozora Bunko's markup. An attribution requirement would
add obligations for downstream combiners while resting on limited creative
originality. Credit is requested through `CITATION.cff` and standard scholarly
citation rather than enforced as a licence condition.

## What covers what

| Scope | Licence | Where |
|---|---|---|
| Underlying works | Public domain (copyright expired) | Not Soranoha's to license |
| TEI, plaintext, Markdown, validation reports, catalog, manifests | [CC0-1.0](../LICENSE-CC0) | Published bytes |
| TEI customisation: `soranoha/schemas/tei-profile.{odd,rng,sch}` | [CC0-1.0](../LICENSE-CC0) | Repository |
| Protocol JSON Schemas: `soranoha/resources/snh/schemas/` | [CC0-1.0](../LICENSE-CC0) | Repository |
| Locally authored source code | [Apache-2.0](../LICENSE) | Repository |
| Forked parser core, eight crates | MIT OR Apache-2.0 | `ab-validator/crates/`, listed below |
| Data tables | Provenance, and terms where the source states them, in each file's header | `ab-validator/crates/*/data/` |

The encoding vocabulary and protocol schemas are dedicated under CC0 so that
independent implementations can adopt them without attribution obligations. The
code is licensed under Apache-2.0 to provide an explicit patent grant
protecting independent reimplementations of the snh protocol. MIT is not
offered as an alternative, because it carries no patent grant: a recipient who
elected it would lose the protection the choice of Apache-2.0 exists to give.

The forked crates are the exception, and not a second choice. Upstream is
dual-licensed, so those eight carry `MIT OR Apache-2.0` as an inherited
obligation. Each states it in its own manifest rather than taking the
workspace default.

The data tables the tooling reads are mostly third-party work, and each file's
own header names where it came from rather than this page gathering it, so a
table stays interpretable when it is copied out on its own. The 外字注記辞書
mapping is CC0 through an upstream transcription. The JIS X 0213 code table is
Project X0213's, under its own permissive grant; the two gaiji-territory tables
derived from it come from glibc's EUC-JISX0213 charmap and from the standard's
Annex F, and their headers record that provenance rather than a grant of their
own. The hand-curated gaiji aliases are Soranoha's. Redistributing the tooling
carries those headers with it.

## The toolchain

The Rust parser core under `ab-validator/crates/` is an independent fork of
[P4suta/aozora](https://github.com/P4suta/aozora) at revision
`1a4f864603970983719655aa4af4525958ac2d38`, dual-licensed **MIT OR
Apache-2.0**. Its licence files and `NOTICE` are retained in the fork, and
each crate root carries a header naming the upstream crate and revision.

Eight crates are forked: `ab-aozora-corpus`, `ab-aozora-encoding`,
`ab-aozora-facade`, `ab-aozora-pipeline`, `ab-aozora-render`, `ab-aozora-spec`,
`ab-aozora-syntax` and `ab-notation-strategies`. The last of these does not
carry the prefix: it descends from upstream's `aozora-proptest` and was renamed
when the fork was reduced, which changed its name and not its licence. Read the
crate-root header rather than the name. `ab-aozora` without a suffix is a
locally authored harness binary containing no lifted code.

This distinction matters when assessing license obligations before
redistributing tooling, and when auditing transcription methodology.

Redistributing the *published corpus* does not carry the toolchain's
obligations. Redistributing the *tooling* does.

The Aozora Bunko catalog CSV that Soranoha reads bibliographic metadata from is
published by Aozora Bunko under CC BY 4.0. That licence covers catalog reuse;
it grants nothing over the works themselves.

## Machine-readable form

Each published artifact carries its own terms, so a file stays interpretable
once detached from this site.

In every signed release manifest:

```json
"rights": {
  "works": "public-domain",
  "encoding": "CC0-1.0",
  "statement_url": "https://w3id.org/soranoha/rights"
}
```

In every published TEI file, one `licence` element covers each rights layer.
The underlying work's status is represented by the Creative Commons Public
Domain Mark, and Soranoha's encoding is dedicated under CC0:

```xml
<publicationStmt>
  <!-- publisher, idno and date omitted -->
  <availability status="free">
    <licence target="https://creativecommons.org/publicdomain/mark/1.0/">The
      underlying work is in the public domain; Soranoha asserts no rights over
      it.</licence>
    <licence target="https://creativecommons.org/publicdomain/zero/1.0/">Soranoha's
      encoding of this work, and the artifacts derived from it, are dedicated to
      the public domain under CC0-1.0. Attribution is requested, not required.
      Full rights statement: <ptr target="https://w3id.org/soranoha/rights"/></licence>
  </availability>
</publicationStmt>
```

Both come from one document, `soranoha/data/publication-policy.edn`, whose
bytes each manifest already hashes as `admission.policy_hash`. The terms
signed off, the terms in the release record, and the terms inside a detached
TEI file therefore cannot disagree. Where a copy nonetheless differs, the
manifest field is authoritative because it is signed.

The TEI profile enforces the presence of the grant. The
`snh-publication-licence` rule in `soranoha/schemas/tei-profile.sch` rejects any
Soranoha TEI file whose header lacks `publicationStmt/availability/licence` with
a non-empty `@target`. Omitting stated terms causes validation failure.

## If you hold rights in a published work

Soranoha publishes only works its assessment finds to be free of subsisting
rights, but a corpus of this size will eventually be wrong about one.

Write to the address on <https://orcid.org/0000-0003-2246-8774>, naming the
work identifier (the `000092_000879` form, visible in the URL) or the Aozora Bunko
card, and the basis of the claim. A claim does not need to be a formal legal
notice to be acted on.

Withdrawal is recorded in public governance events rather than in-place modification.
A signed event carries a public `statement` and one of four `reason_code`
values. Two of them are for claims: `rights` for a subsisting-rights claim, and
`takedown-request` for a removal asked for on other grounds. The other two,
`data-defect` and `other`, exist for withdrawals that are not claims. The next
release then:

- removes the work from `works[]`, so its artifacts are no longer served at
  `/works/<identifier>/`;
- publishes a new catalog that no longer describes it, since a manifest may
  not name a catalog describing a work that release withdrew;
- adds it to `withdrawn[]` pointing at the governing event, which is served at
  `/withdrawn/<identifier>.json`.

Withdrawal operates under two constraints:
- **Append-only history**: earlier releases containing the work remain in the
  signed chain; withdrawal ceases current distribution without rewriting historical releases.
- **Irreversible withdrawal**: a withdrawal statement may be amended via `event-amendment`,
  but amendments cannot reverse the withdrawal itself.

The mechanism is specified in
[`docs/design/snh-protocol-v1.md`](design/snh-protocol-v1.md) and its schema in
`soranoha/resources/snh/schemas/snh-governance-event-1.schema.json`.

## No warranty

Soranoha publishes transcriptions with their validation reports and their
divergences from the source recorded, not a guarantee of fidelity. The corpus
is provided as-is. Do not rely on it for a legal, medical, or safety purpose
without independent verification against the source edition each work names in
its `sourceDesc`.
