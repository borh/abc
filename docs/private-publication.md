# Private corpus testing

The Speely deployment serves the experimental corpus at
<https://soranoha.hyakutake-barbel.ts.net/>. Its Caddy listener and Forgejo
origin are bound to Tailscale. Private releases use the ordinary signed
publication chain, archive verifier and serving exporter. Validation failures
remain visible in each work's report; fidelity measurement is a separate
research result and does not alter admission.

The commands below describe the current deployment. Storage locations and
backup policy belong to the NixOS server configuration. They are not protocol
requirements. Run kernel commands from a Soranoha checkout and pass absolute
paths because the Nix wrapper changes its working directory.

## Prepare and inspect the whole corpus

Run these as the publisher account on Speely, from the repository checkout:
`$PWD` below resolves the assessment source and the assets root, and `prod` is
the configured publication directory.

```sh
prod=/var/lib/soranoha-runner/soranoha/production

nix run .#soranoha-kernel -- aozora-reliance-prepare --all \
  --aozora-root "$prod/aozora" --evidence-root "$prod/evidence" \
  --assessment-source "$PWD/soranoha/data/assessment-source.json" \
  --out "$prod/review/full-assessment-source.json" \
  > "$prod/review/full-preparation.json"

nix run .#soranoha-kernel -- assessment-evaluate \
  --root "$prod/root" --aozora-root "$prod/aozora" \
  --evidence-root "$prod/evidence" \
  --assessment-source "$prod/review/full-assessment-source.json" \
  --as-of "$(date -u +%F)" --out "$prod/review/full-snapshot.json"

nix run .#soranoha-kernel -- build \
  --root "$prod/root" --aozora-root "$prod/aozora" \
  --assets-root "$PWD/soranoha" --out "$prod/review/full-corpus" \
  > "$prod/review/full-build.log"
```

Preparation attempts every selected edition with four acquisition workers.
Its report lists unavailable assertions by slug and reason. Successful
assertions refresh the draft; previous exception records remain unchanged.
Acquisition failures do not erase previous records, and evaluation must still
establish their current applicability. Preparation does not publish anything.
The build log names the generated JSON report under `production/root/runs`.
The new export directory contains each work's TEI, plaintext, Markdown, validation,
projection and fidelity reports, including works whose assessment does not permit publication.
Markdown is a published per-work artifact alongside TEI and plaintext
(`snh-manifest/3`); its projection report is an experimental review artifact
outside the signed set.
Each TEI header carries the rights grant read from
`soranoha/data/publication-policy.edn` under `--assets-root`, which is the same
document the release hashes as `admission.policy_hash`, so a review export shows
the terms a release would publish. A policy without a stated grant fails the
build rather than exporting works with no terms.
Use a new export directory for subsequent iterations; the build cache is reused.
Review the resulting admission partition and export measurements, then commit
the accepted source and snapshot together at their repository paths.

A change to selection or assessment code makes the committed snapshot stale
without touching it. Before dispatching, regenerate and compare:

```sh
nix run .#soranoha-kernel -- assessment-drift \
  --deployment /etc/soranoha/publisher.json \
  --assessment "$PWD/soranoha/data/assessment-snapshot.json" \
  --assessment-source "$PWD/soranoha/data/assessment-source.json" \
  --as-of "$(date -u +%F)"
```

It exits 1 and names the slugs that differ when the committed snapshot no
longer matches the selection; the release job runs the same check before
minting. On 2026-09-11 the first genesis after the reset below failed on this:
rights admission had moved into selection three days earlier and the snapshot
still listed the 71 works that refusal removes.

Dispatch `.forgejo/workflows/scheduled-release.yml` at that commit through
Forgejo Actions. The workflow first asks whether the corpus moved, then uses the
existing release secret without exposing it to build children, publishes with
both role pins, and installs a verified serving tree.

A release is minted when a published byte would change, not on every upstream
commit. `release-needed` verifies the published head, reads the revision it
names, and compares that revision against the checkout: first under
`cards/*/files/*.zip`, and when no archive moved, the catalog rows of the
selected works as the record builder reads them, so a corrected title is a
release and a card-page edit is not. It exits 10 when nothing published
changed and the workflow mints nothing. Every uncertainty
reports that a release is needed, because refusing to release is the outcome
that loses work. The revisions passed over are not lost either: the next
release's `corpus.covers_from` names where its range starts, so any revision in
`(covers_from, upstream_rev]` maps to exactly one release.

`release-delta` answers what a release changed, from the two manifests and
nothing else. It verifies the chain against both role pins, then compares the
manifest at `--to` with the one at `--from`; with neither flag it compares the
head against the release before it. Because every input is already in signed
bytes, anyone holding a clone recomputes the same answer without a corpus
checkout and without rebuilding anything.

It keeps two causes apart rather than reporting one figure for change. A work
under `source-changed` is publishing a different source state, established by
its `source_content_hash`. A work under `documents-changed` has an unchanged
source hash and moved artifact ids: the same edition converted again, which
yields new documents and never a new edition. The stages whose closure hash or
code version moved are listed separately, so a source change is never
attributed to the toolchain or the reverse. `unexplained` narrows
`documents-changed` to the works for which no declared input moved either. A
document is a function of more than its source and the stages that convert it:
the metadata record comes from the catalog rows and the rights block reaches
the header, so `catalog` and `rights` are reported beside the toolchain and all
three have to stand still. Without that the field would fire on every
catalog-only revision that edited a published work's metadata, which is about
one in ten of them. What is left is bytes that moved while everything declared
about how they are produced stood still: between adjacent releases the
transaction's determinism halt should already have refused to publish it, and
between releases that are not adjacent this is the only place the question is
asked.

`corpus-delta` answers the neighbouring question: how the corpus in a local
Aozora Bunko checkout differs from what a release published. No release was
ever cut at most upstream revisions, so the chain cannot answer this on its
own; the archives at that revision are simply not in it. The reader supplies
them instead, from their own checkout.

```sh
nix run .#soranoha-kernel -- corpus-delta \
  --chain-clone /absolute/path/to/chain \
  --aozora-root /absolute/path/to/aozorabunko-at-the-revision-to-compare \
  --release-pub /absolute/path/to/release.pub \
  --governance-pub /absolute/path/to/governance.pub
```

The chain is verified against both role pins before any manifest is read, so
the only unsigned side is the reader's own. `--to` names the release to compare
against and defaults to the head. The checkout must be clean under `cards/` and
`index_pages/`, because otherwise the revision it reports would not describe
the bytes that were read.

No parser runs. A work's `source_content_hash` is the hash of its archive's
identity object, produced by the `extract` stage before any parsing, so every
published source identity can be recomputed from the archive alone. Scanning
the whole corpus takes approximately 2.3 seconds at `--concurrency 16` over
17,602 works with warm OS page caches. Comparing against an arbitrary
historical revision therefore costs a checkout, not a rebuild.

`withdrawn` is reported apart from `only-in-checkout`, because it is the one
case where the release is missing a work on purpose. `unreadable` names
archives the admission rules refused, which an old revision is exactly where to
expect: one refused archive costs its own answer and not the rest. A release
built by a different `extract` version is refused outright rather than
differenced, since two versions may hash one archive differently and the
differences reported would be the stage's.

What this does not answer is whether the current toolchain would produce
different documents from the same sources. Under one toolchain nothing else can
move a document, so the source answer is the whole answer; across a toolchain
change it is not, and only building both revisions is. See
[performance measurements](performance.md) for that recipe.

Inspect `/releases/HEAD`, the corresponding manifest and signature,
and the work links named by that manifest. The root URL serves the generated
browse layer (a landing page, author, title and NDC indexes, a bibliography and
a reading view per work, the rights and citation pages, and the TEI extension
vocabulary the published headers point at), providing presentation over the same
release without any of it being hashed into the chain. The vocabulary page is
rendered from the repository file itself, which the Nix wrapper supplies through
`SORANOHA_SITE_DOCS`; `soranoha.za.docs` lists which files are served and at
which routes.
Follow `/catalog.json` for the signed discovery record.

The NixOS publisher profile provisions the source and chain clones, shared serving
directories, SSH transport, and `/etc/soranoha/publisher.json`. The workflow reads
that configuration through `--deployment`; explicit CLI flags override configured
values. Configuration contains paths, repository coordinates, the release DOI and the
corpus's name and maturity label, never signing keys. Apply the NixOS configuration before dispatching a workflow
that consumes it.

`release-doi` (equivalently `--release-doi`) is the release's Zenodo version DOI,
and it is optional. Zenodo mints it after the release exists, so no signed record
can carry it, and before the first deposit there is none; serving injects it into
every citation it renders. A malformed value is refused at activation rather than
written into every work's citation record.

`release-name` and `release-maturity` (equivalently `--release-name` and
`--release-maturity`) name the corpus and say what stage it is at. Both are
optional and neither reaches a manifest: every other field a release publishes
is derived from its inputs, while these two are editorial judgements that
signed append-only bytes could not correct.

`release-name` is a dotted numeric name such as `v0.1`, checked for shape. It
names the corpus at this stage of its life rather than the individual release:
releases are minted whenever the upstream corpus moves, so a name that
incremented per release would reach three digits without saying anything the
release head does not. The release head remains the canonical identity in
citations, and the citation page clarifies this distinction so bibliographies cite
the immutable release rather than the broad version label.

`release-maturity` is a key into a closed vocabulary (`early` or `stable`)
defined in `soranoha.za.maturity`. Because the site is bilingual, labels must
exist in both languages; export fails closed if an unrecognized key is configured.

Because citations and the pages that carry these strings are generated files,
all three are part of what activation produces. Setting or changing one for an
already-exported commit causes the reuse check to fail with
`serving-tree-mismatch` because the existing tree names a different value. To
resolve this, remove that tree and re-export.

To re-verify and activate the existing publication using the configured SSH transport:

```sh
nix run .#soranoha-kernel -- serving-activate \
  --deployment /etc/soranoha/publisher.json
```

Activation verifies the complete publication chain. It writes a new export under
`serve/trees/COMMIT`, or checks every byte, symlink and path in an existing export
before reusing it. It grants the serving group read access, checks that origin
has not advanced, and atomically switches `current` last. A file lock serializes
activators. A failed verification, changed publication head, or failed pointer
replacement leaves the previous pointer in place. Neither activation nor the
NixOS profile deletes historical exports; storage retention and backup remain
server responsibilities.

## Initialize an empty publication origin

An empty origin needs the ordinary pre-genesis Git commit before its first
release. As the publisher, clone the designated private origin, configure the
existing runner SSH identity, and run:

```sh
prod=/var/lib/soranoha-runner/soranoha/production

export GIT_SSH_COMMAND="/run/current-system/sw/bin/ssh -i /var/lib/soranoha-runner/soranoha/.ssh/id_ed25519 -o UserKnownHostsFile=/var/lib/soranoha-runner/soranoha/.ssh/known_hosts -o StrictHostKeyChecking=yes"
git clone ssh://forgejo@speely.hyakutake-barbel.ts.net:63333/bor/soranoha-chain.git "$prod/chain"
nix run .#soranoha-kernel -- publication-init \
  --chain-clone "$prod/chain" --branch main
```

The command refuses an existing branch. Its zero `releases/HEAD` is initialization
state, not a signed or citable genesis. Keep the origin's
`receive.denyNonFastForwards` and `receive.denyDeletes` enabled for normal pushes.

## Explicitly reset this private deployment

Reset is an operator action outside the append-only publication protocol.
It restarts the active experimental lineage and can invalidate experimental
work links. Unreferenced Git objects and reflogs can remain in the origin;
reset is not secure erasure. Do not apply it to a public chain. Before resetting, ensure no workflow
is running or queued, record the current origin commit, and stop the publisher
and serving units. On the current Speely host:

```sh
sudo systemctl stop gitea-runner-soranoha.service \
  soranoha-serve.path soranoha-serve.service

sudo -u forgejo \
  git --git-dir=/var/lib/forgejo/repositories/bor/soranoha-chain.git \
  for-each-ref --format='%(refname) %(objectname)'
```

Confirm that `main` is the designated disposable branch and there are no other
publication refs to retain. Set `expected` to the commit that listing reported,
then remove that exact ref locally as its operator. This does not relax receive
rules:

```sh
expected=...   # the commit recorded from the ref listing above

printf '%s' "$expected" | grep -qE '^[0-9a-f]{40}$' \
  && [ "$expected" != "0000000000000000000000000000000000000000" ] \
  && sudo -u forgejo \
       git --git-dir=/var/lib/forgejo/repositories/bor/soranoha-chain.git \
       update-ref -d refs/heads/main "$expected"
```

Once that reports success and the ref is gone, remove the local clone and the
serving trees:

```sh
sudo rm -rf -- /var/lib/soranoha-runner/soranoha/production/chain
sudo rm -rf -- /var/lib/soranoha-runner/serve
```

Two things about the guard, both established by running it against a throwaway
repository rather than assumed.

`update-ref -d <ref> <oldvalue>` does refuse a wrong old value: it reports that
the ref is at one commit but another was expected, exits non-zero, and leaves
the ref in place. The exception is the all-zero object id, which git reads as
"no old value supplied" rather than as a value, so it deletes the ref and exits
zero. That is not an unlikely thing to paste: a push line or a listing of a ref
that has just been created or deleted shows all zeros. The shape check above
rejects it, and rejects the unedited placeholder with it.

The checks are chained with `&&` rather than written as separate statements
because a failed check that does not gate the next command is not a check.
These blocks are pasted into a shell without `set -e`, where a preceding
failure changes nothing about whether the deletion runs.

The repository is named in full in both blocks rather than carried in a
variable. `origin` is among the names most likely to already hold something
else in an operator's shell, and this is the block that deletes a branch.

Reclone the private origin as the publisher and run `publication-init` above.
Restart the runner and serving path unit, then dispatch a fresh release:

```sh
sudo systemctl start gitea-runner-soranoha.service soranoha-serve.path
```

Reset on 2026-09-11: the chain held six releases at `snh-manifest/1`, and the
verifier at every revision that generates the site's front door reads only
`snh-manifest/3` (ADR 0003 replaces the wire version rather than succeeding
it). The procedure above was run as written, followed by `publication-init`
and a dispatched release, which minted a schema-3 genesis of 17,308 works.

The source mirror, `production/root` CAS/trace cache, `production/evidence`,
reviewed assessment inputs and pinned role keys remain intact. Speely's
exporter verifies from the selected Git commit and has no separate durable
verifier checkpoint. External test clients that retain a previous chain head
must explicitly discard that test state too. Compare a fresh release's verified
manifest and served bytes after reset; an unchanged build should reuse its cache.

## Public launch

Private history is disposable and is not promoted to public history. Prepare a
fresh reviewed genesis from the final inputs, verify it, and independently
deposit its exact manifest, signature and role-bound public key assignment in
Zenodo. Add that deposit's version DOI to the owner's ORCID record before
public exposure. Zenodo credentials remain separate from release CI. This gate
does not prevent private testing or resets. The two deposits, their contents
and their ordering are specified in [Zenodo deposits and ORCID
anchoring](zenodo-deposits.md).
