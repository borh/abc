# Zenodo deposits and ORCID anchoring

Soranoha makes two kinds of Zenodo deposit, and they are not variants of one
procedure.

| | Trust anchor | Release deposit |
|---|---|---|
| Purpose | Authenticates which public key holds which role | Makes a release's bytes citable and independently checkable |
| Frequency | Once, before the first signed release | Every release, including genesis |
| Contents | The two role public keys and their role assignment | The signed manifest and the records it names |
| Discovery | Listed as a work on the owner's ORCID record | Cited by version DOI in each work's citation |

Genesis needs both, in that order: the anchor first, because a release cannot
be verified without it.

Both deposits are made by the publication owner under the Zenodo account
`borh`, from a machine that is not release CI. Zenodo credentials are never
available to the release workflow. This is deliberate: the release key signs
manifests automatically, and adding archive credentials to the same automation
would put the ability to publish and the ability to make something citable in
one place.

Cite a **version** DOI, never the concept DOI, wherever the DOI has to resolve
to fixed bytes. A concept DOI resolves to the latest version, so what it names
changes underneath a citation.

## The trust anchor

The anchor is the only place pinned key bytes and fingerprints are published.
It authenticates the *role assignment*, not a list of keys:

```
RELEASE    = {K_release}
GOVERNANCE = {K_governance}
```

A flat list would be a different and weaker claim, because it would not say
that the online release key is not authorized for governance. The role sets are
disjoint, and the kernel rejects an overlapping or un-roled pinned
configuration.

Deposit exactly:

- `release.pub` and `governance.pub`, the 65-byte hex-plus-newline files the
  key ceremony produces. These are the same bytes deployment source carries as
  non-authenticating pinned verifier configuration.
- A short role-assignment statement naming each role, its key's hex, and its
  fingerprint (`tail -c 32 "$role.pub.der" | sha256sum`), recorded during the
  ceremony.

Do not deposit private keys, medium passphrases, or the custody inventory. Do
not deposit a second copy of the fingerprints anywhere else: the protocol
publishes them in one place so that no two copies can drift.

Record the resulting **version** DOI, then add it to the owner's ORCID record
(<https://orcid.org/0000-0003-2246-8774>) as a work. That entry is the
pre-release discovery channel: a verifier who knows only the project's name
reaches the ORCID record, follows the one pointer to the anchor, and obtains
the role assignment from an archive the publication channel does not control.

The ORCID entry must exist before the first signed release. The ordering
constraint is where this goes wrong: keys before anchor, anchor before ORCID
entry, ORCID entry before genesis. A release signed ahead of the ORCID entry is
not verifiable by the procedure the protocol specifies, and because the chain
is append-only, correcting it means abandoning the chain rather than amending
it.

The anchor is deposited once. Later releases do not update it, because the
pinned set never changes: a lost medium leaves governance operating on the
remaining copy, and a compromised role halts that role rather than rotating it.

## The release deposit

Every release is deposited before it is served, so that every publicly
reachable release is citable the moment it is reachable.

Deposit the manifest and the records it names, which is what a verifier needs
and is small enough to deposit every time. The published artifacts are not
deposited: the corpus export is several gigabytes per release, and a reader who
has an artifact can check it against the deposited manifest without it.

From the chain clone, for the release at `releases/HEAD`:

- `releases/<manifest-hex>.json`, the signed manifest.
- `releases/<manifest-hex>.sig`, its detached release-key signature.
- The three records the manifest names by content-addressed id, each read from
  `blobs/sha256/`: the catalog (`catalog`), the assessment snapshot
  (`admission.assessment_snapshot`), and the admission report
  (`admission.admission_report`).

Do not include the role public keys. They belong to the anchor, and a copy here
would be a second place a verifier might take them from.

### Recording the DOI

The DOI is not in the chain. The signed catalog carries no DOI and no provider
reference, and the manifest schema is closed, so a DOI field could never be
removed once added. A DOI describes a deposit rather than a release: the same
release could be deposited twice, deposited elsewhere, or not deposited at all.
Reserving a DOI and signing it into an immutable manifest fails permanently
when the deposit is abandoned, whereas a DOI held outside simply does not
render.

So the DOI is deployment configuration. After the deposit, set `release-doi` in
`/etc/soranoha/publisher.json` (or pass `--release-doi`) to the version DOI, and
run `serving-activate`. Activation is idempotent, so a DOI recorded late is
recovered by re-running it. A malformed value is refused at activation rather
than rendered into every work's citation.

Because citations are generated files, changing the DOI for a commit that was
already exported makes the reuse check fail with `serving-tree-mismatch`: the
existing tree's citations name a different DOI. Remove that tree and re-export.

Activation therefore depends on Zenodo being reachable. For a research corpus
that delay is acceptable, and it is stated here as policy so that it is not
discovered during a release.

### Why an unsigned DOI mapping is still trustworthy

Nothing verifiable is lost by keeping the DOI outside the signed chain, because
the deposit contains the manifest bytes. A reader who is given a DOI can check
the correspondence themselves:

1. Download the deposit's `releases/<hex>.json`.
2. Hash it. The sha256 must equal the `<hex>` in its name and the manifest id
   the chain records.
3. Verify `releases/<hex>.sig` against the release key from the trust anchor.

A DOI that names some other release fails at step 2, and one whose manifest was
not signed by the pinned release key fails at step 3. The mapping is
configuration, but the claim it makes is checkable against the chain.

## See also

- [Key ceremony and offline governance signing](key-ceremony.md), which
  produces the bytes the anchor deposits.
- [Citing Soranoha](citation.md), for how the version DOI appears in a
  citation.
- [Private corpus testing](private-publication.md), for the deployment the
  release deposit is recorded into.
