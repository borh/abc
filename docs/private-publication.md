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

Dispatch `.forgejo/workflows/scheduled-release.yml` at that commit through
Forgejo Actions. The workflow first asks whether the corpus moved, then uses the
existing release secret without exposing it to build children, publishes with
both role pins, and installs a verified serving tree.

A release is minted when a work archive changed, not on every upstream commit.
`release-needed` verifies the published head, reads the revision it names, and
compares that revision against the checkout under `cards/*/files/*.zip`; it
exits 10 when nothing changed and the workflow mints nothing. Every uncertainty
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
attributed to the toolchain or the reverse. `unexplained` lists works whose
documents moved while both the source hash and every stage coordinate stood
still, which is the one condition here that nothing else reports: the
transaction's determinism halt cannot see it, because that fires only when the
whole projection matches and an advanced corpus revision means it does not.

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
values. Configuration contains paths, repository coordinates and the release DOI,
never signing keys. Apply the NixOS configuration before dispatching a workflow
that consumes it.

`release-doi` (equivalently `--release-doi`) is the release's Zenodo version DOI,
and it is optional. Zenodo mints it after the release exists, so no signed record
can carry it, and before the first deposit there is none; serving injects it into
every citation it renders. A malformed value is refused at activation rather than
written into every work's citation record.

Because citations are generated files, the DOI is part of what activation
produces. Setting or changing it for an already-exported commit causes the reuse
check to fail with `serving-tree-mismatch` because the existing tree citations
name a different DOI. To resolve this, remove that tree and re-export.

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
