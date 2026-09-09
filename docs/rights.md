# Rights and licensing

This is the full statement referenced by every signed release manifest as
`rights.statement_url` and by every published TEI file as
`publicationStmt/availability`. It is served at
<https://soranoha.org/rights>.

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
neither holds nor claims any right in them. Aozora's own handling rules
([取り扱い規準](https://www.aozora.gr.jp/guide/kijyunn.html)) state that files
for copyright-expired works may be freely copied, redistributed, shared
(commercial and non-commercial alike), and modified, and that attribution is 希望, requested rather
than required. Soranoha publishes only works its assessment finds free to
publish; the reasoning is recorded in
[`soranoha/docs/evidence/aozora-rights-source-contract.md`](../soranoha/docs/evidence/aozora-rights-source-contract.md).

**Soranoha's encoding** (TEI markup, plaintext and Markdown
projections, validation reports, catalog, and release manifests)
is dedicated to the public domain under [CC0
1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/). Where that
encoding attracts copyright or a database right at all, those rights are
waived.

CC0 rather than CC BY was selected because TEI transcription is largely
a mechanical conversion of Aozora's markup. An attribution requirement would
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

The Aozora catalog CSV that Soranoha reads bibliographic metadata from is
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
  "statement_url": "https://soranoha.org/rights"
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
      Full rights statement: <ptr target="https://soranoha.org/rights"/></licence>
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
work identifier (the `000092_000879` form, visible in the URL) or the Aozora
card, and the basis of the claim. A claim does not need to be a formal legal
notice to be acted on.

Withdrawal is recorded in public governance events rather than in-place modification.
A signed event carries a `reason_code` (`rights` for a subsisting-rights claim,
`takedown-request` for other grounds) and a public `statement`. The next release then:

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
