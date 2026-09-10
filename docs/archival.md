# Archival, and resolving a citation through the archive

The protocol makes an archived copy of the publication repository part of what
a citation rests on. [The specification](design/snh-protocol-v1.md) defines
`archive_verification(archived_view, C, pinned_keys)`, runs it with the
Software Heritage snapshot as the sole repository view, and states that
citation eligibility requires a successful observation. It also requires a
documented recipe for mapping a published identifier to its archived location.
This is that recipe and the procedure that produces the observation.

## The archive is a second view of the same bytes, read independently

`verify_repository_at` reads every commit, tree, manifest, signature, event and
artifact through one non-fallback view. An archival observation supplies the
archived snapshot as that view and nothing else, so an incomplete archive fails
rather than quietly completing itself from the live origin. What a successful
report claims is repository-closure completeness plus the public invariants,
and nothing more. It does not restate that the works are correctly transcribed
or correctly assessed.

The report is disposable. It records the snapshot identifier, the commit, the
pinned key fingerprints, the verifier version and the result, and it is neither
signed nor schema-frozen. Reports have no ordering contract, so there is no
"latest" one, and a later failed observation does not by itself invalidate an
earlier successful one.

## The chain repository has to be publicly cloneable first

Software Heritage archives an origin by cloning the URL it is given, so the
publication repository has to be reachable and anonymously cloneable at a
stable public URL before any of this can run.

It is not today. The origin is
`ssh://forgejo@speely.hyakutake-barbel.ts.net:63333/bor/soranoha-chain.git`,
which is SSH-authenticated and bound to Tailscale. This is a separate
requirement from serving the corpus publicly: that is the browse layer and the
work-facing routes, while this is the Git repository holding the signed chain.
Publishing the serving tree does not make the chain repository cloneable.

The URL is also the archive's identity for this project. It appears in the
`origin` qualifier of every archived identifier below, so it should be chosen
once and not moved.

## Making an observation

Software Heritage's own procedures are not restated here; use Save Code Now and
the Vault under the project's account. Three things are Soranoha's and fixed.

**Archive the origin, not a copy of it.** Submit the public chain URL. A
snapshot of some other clone is not an observation of the published chain.

**Cook the snapshot as a `git-bare` bundle.** The Vault produces three bundle
types, and `git-bare` is the one that reconstructs an actual bare Git
repository with its branches. The verifier needs a repository, not a directory
of files, because it reads commits and trees.

**Verify the materialized bundle, and record the snapshot identifier with the
report.** Unpack the bundle, then run the kernel against it:

```sh
nix run .#soranoha-kernel -- archive-verify \
  --archive /absolute/path/to/unpacked-bare-repo \
  --commit "$C" \
  --release-pub /absolute/path/to/release.pub \
  --governance-pub /absolute/path/to/governance.pub
```

`--commit` is the publication commit being observed, as a full lowercase 40
character object id. The kernel identifies the view by its local path, which is
an artefact of where the bundle happened to be unpacked and means nothing to a
reader. Replace it with the snapshot identifier when the report is filed, so
the recorded observation names something a third party can fetch.

A view that cannot be constructed at all is a failure to perform the
observation rather than an observation, and the kernel throws instead of
reporting. A readable view always produces a report, successful or failed.

## Resolving a citation through the archive

A citation names a release head and a work. Reaching the archived bytes takes
four steps, none of which need the live site.

**Map the identifier to an in-repo path.** The chain's layout is fixed:

| Identifier | Path in the repository |
|---|---|
| Release head `<hex>` | `releases/<hex>.json`, with its signature at `releases/<hex>.sig` |
| Artifact id `snh:1:<type>:<sha256>` | `blobs/sha256/<first two characters>/<sha256>` |
| Governance event `<hex>` | `governance/<hex>.json` |
| Current head pointer | `releases/HEAD` |

**Find the publication commit.** The manifest at `releases/<hex>.json` is
reachable from the commit whose `releases/HEAD` names that head. Walking back
from the archived branch tip through `prev_manifest` reaches every earlier
release, which is what the chain being append-only buys.

**Name the archived object.** With the commit `C`, the snapshot `S` and the
origin URL, a fully qualified identifier for one file is:

```
swh:1:cnt:<git blob id>;origin=<chain URL>;visit=swh:1:snp:<S>;anchor=swh:1:rev:<C>;path=/blobs/sha256/8b/8b2595...
```

**Do not confuse the two digests.** A Soranoha artifact id carries the sha256
of the artifact's own bytes. A Software Heritage content identifier carries a
Git blob hash, which is a sha1 over a different byte sequence. The two never
match and neither is derivable from the other. The sha256 selects the path; the
archive resolves the path to its own identifier. This is why the recipe is a
path mapping rather than an identifier translation.

## Where this sits in the genesis sequence

An archival observation needs a publication commit to observe, so it follows
the first signed release rather than preceding it. The
[key ceremony](key-ceremony.md) and the trust anchor both come first, and for a
different reason: they establish which keys the verifier pins, and an
observation made against the wrong pinned set proves nothing.

The order is therefore: keys, anchor deposit, ORCID entry, first signed
release, public chain URL, archival observation. Only the last of these makes a
release citation-eligible under the specification, which is why the public URL
cannot wait until after launch.

## When to observe again

Each observation covers one commit. A release that has not been observed is not
citation-eligible, so the cadence follows whatever the citation policy requires
rather than a fixed schedule. Observing the head at each
[quarterly deposit](zenodo-deposits.md) pairs the two independent records
naturally: the deposit holds the manifest bytes and the archive holds the
repository they came from.

## See also

- [The snh protocol specification](design/snh-protocol-v1.md), whose archival
  section defines the observation and the eligibility rule.
- [Zenodo deposits and ORCID anchoring](zenodo-deposits.md), the other
  independent record. Its deposits hold loose files; this holds a repository,
  which is why `archive-verify` cannot read a deposit.
- [Citing Soranoha](citation.md), for what a citation carries.
