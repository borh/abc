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

As the publisher account on Speely, with `prod` set to its configured
publication directory:

```sh
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
Markdown is now a published per-work artifact alongside TEI and plaintext
(`snh-manifest/2`); its projection report remains an experimental review
artifact outside the signed set.
Each TEI header carries the rights grant read from
`soranoha/data/publication-policy.edn` under `--assets-root`, which is the same
document the release hashes as `admission.policy_hash`, so a review export shows
the terms a release would publish. A policy without a stated grant fails the
build rather than exporting works with no terms.
Use a new export directory for subsequent iterations; the build cache is reused.
Review the resulting admission partition and export measurements, then commit
the accepted source and snapshot together at their repository paths.

Dispatch `.forgejo/workflows/scheduled-release.yml` at that commit through
Forgejo Actions. The workflow uses the existing release secret without exposing
it to build children, publishes with both role pins, and installs a verified
serving tree. Inspect `/releases/HEAD`, the corresponding manifest and signature,
and the work links named by that manifest. The root URL now serves the generated
browse layer (a landing page, author, title and NDC indexes, and one page per
work), providing presentation over the same release without being named by a manifest.
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

origin=/var/lib/forgejo/repositories/bor/soranoha-chain.git
sudo -u forgejo git --git-dir="$origin" for-each-ref \
  --format='%(refname) %(objectname)'
```

Confirm that `main` is the designated disposable branch and there are no other
publication refs to retain. Set `expected` to the recorded commit, then remove
that exact ref locally as its operator. This does not relax receive rules:

```sh
sudo -u forgejo git --git-dir="$origin" update-ref -d refs/heads/main "${expected:?set the recorded private commit}"
sudo rm -rf -- /var/lib/soranoha-runner/soranoha/production/chain
sudo rm -rf -- /var/lib/soranoha-runner/serve
```

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
