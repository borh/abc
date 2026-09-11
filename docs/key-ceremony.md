# Key ceremony and offline governance signing

The [snh specification](design/snh-protocol-v1.md) binds a publication chain to two
disjoint Ed25519 roles at genesis. The RELEASE key is online and held by CI; it signs
release manifests. The GOVERNANCE key is offline and owner-held; it signs the
withdrawal and amendment events that are the only way to remove a published work.
Neither pinned key can change. Pinning different keys ends that chain and begins
a new one without continuity. This document specifies the procedure that establishes both
keys and the procedure that uses the governance key afterwards.

To verify toolchain behavior before live operation, the ceremony signs a
checked-in conformance vector with the newly generated governance key.

## What the ceremony fixes permanently

Both keys are generated fresh for the public chain. No key that has been resident on
a network-connected host may be pinned, because the pinned set cannot later be
retired, so any exposure in a key's history is permanent.

The two roles fail differently:
- A compromised release key can publish an unwanted release but cannot withdraw a work.
- A compromised governance key can withdraw published works.
- A lost release key prevents signing subsequent releases, as the pinned key cannot be replaced.

Both keys require durable backups on the offline governance media created below.

## Ceremony parameters

This document does not name the hardware. The ceremony records the following in the
custody inventory at execution time, and the inventory is not kept in this
repository:

- the offline machine, and the ceremony image identified by the store path and
  `sha256` recorded when it was built;
- the two storage media, identified by manufacturer serial, and the physical custody
  location of each;
- the passphrase custody arrangement for each medium;
- the date, the operator, and any witness.

## Build the ceremony image

The ceremony runs from a NixOS image built for it, not from a stock installer ISO.
Three properties of the installer profile make it the wrong image here, each
checked rather than assumed:

- The `openssl` command-line tool is not in the installer's system packages. Every
  signing step below needs it, and a machine with no network cannot fetch it, so a
  stock image fails at the first step rather than at a recoverable one.
- `services.openssh` is enabled and permits root login. That suits installing a
  headless board and does not belong on a machine holding the governance key.
- The image ships a network stack with wireless enabled, which contradicts the
  requirement that the machine have no path to a network.

Building the image instead of assembling one by hand also gives the ceremony
parameters something better to record than a downloaded checksum: the image is a
function of pinned inputs, so it can be rebuilt later and compared against what the
inventory says was used.

The module is [nix/snh-ceremony-iso.nix](../nix/snh-ceremony-iso.nix), exposed by
the repository flake as `nixosModules.snh-ceremony-iso` for an estate configuration
to import, and as the configuration `nixosConfigurations.snh-ceremony` that builds
the image from this repository alone. Build it on a networked machine; nothing about
the build touches key material.

```sh
nix build .#nixosConfigurations.snh-ceremony.config.system.build.isoImage
sha256sum result/iso/snh-ceremony.iso
readlink -f result
```

The derivation keeps the upstream `nixos-minimal` name; `image.fileName` names the
file inside it, which is what `result/iso/snh-ceremony.iso` above refers to.

Record both the store path and the digest in the ceremony parameters, then write the
image to the boot medium.

## Boot the offline machine

Boot the machine from the ceremony image. Networking must be physically disconnected
or absent: no ethernet cable, no wireless adapter, and no tethered devices. Removing
the network configuration from the image is defence in depth and does not replace
this; only absent hardware makes a network unreachable.

The image's root filesystem is a read-only squashfs with a tmpfs overlay, so a RAM
root is a property of the image rather than something to arrange. Working copies
disappear at power-off.

The console logs in as the unprivileged `nixos` user. The steps below format and
mount devices, so take a root shell and work in a directory on `/run`, which is
tmpfs, so that nothing is written to persistent storage except the two media:

```sh
sudo -i
umask 077
mkdir -m 0700 /run/ceremony && cd /run/ceremony
mkdir -m 0700 /run/ceremony/mnt
```

Every block below that opens a medium mounts it at `/run/ceremony/mnt`, which
is why it is created once here. The path is written out rather than held in a
variable so that each block stands on its own: the only thing a block needs
from outside itself is the device, and each one sets that explicitly, because
which medium is being opened is the one decision no default can make.

The procedure needs only OpenSSL 3, GNU coreutils, `cryptsetup` and `e2fsprogs`, all
of which the image carries.

## Prove the toolchain before generating anything

The specification defines exact bytes, and a signing procedure that produces
plausible-looking output over the wrong bytes is the failure this step exists to
catch. Before any real key exists, reproduce a signature that is already checked into
the repository. The three constants below are transcribed from
`soranoha/resources/snh/vectors/signature-vectors.json`, so nothing needs to be
carried to the offline machine for this step.

```sh
umask 077
seed=4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb
event=bca7658ee4ae0316fc7304aa5e3657761c167592f8a67a33cdb661bb37e70a59
expect=d43c26f20175cdbac2d7761f586581512d6914be0f90ab8e9f8c3bca8b812d5389b73ef2639d19492e5e740932bfe8a3651b96666c83e73101d29152bfdfec0b

printf "$(printf '302e020100300506032b657004220420%s' "$seed" | sed 's/../\\x&/g')" > selftest.der
printf 'snh-governance-event-sig/1:%s' "$event" > selftest.msg
openssl pkeyutl -sign -rawin -inkey selftest.der -keyform DER \
  -in selftest.msg -out selftest.sig
test "$(wc -c < selftest.msg)" -eq 91
test "$(wc -c < selftest.sig)" -eq 64
test "$(od -An -tx1 < selftest.sig | tr -d ' \n')" = "$expect" && echo toolchain-ok
```

`302e020100300506032b657004220420` is the fixed PKCS#8 prefix that precedes the 32
seed bytes in an Ed25519 private key, so the `printf` above turns a 64-character hex
seed into a key file OpenSSL can read. The message is the exact ASCII of the
domain-separated string with no trailing newline: 27 prefix characters plus 64 hex
characters is 91 bytes, and any editor that appends a newline produces a signature
the verifier will reject. If `toolchain-ok` does not print, stop; do not generate
keys with a toolchain that cannot reproduce a known signature.

Delete the self-test files afterwards. These are the RFC 8032 test vectors and are
public; they are not secrets, and they are not the ceremony's keys.

## Generate the two keypairs

```sh
openssl genpkey -algorithm ed25519 -outform DER -out release.der
openssl genpkey -algorithm ed25519 -outform DER -out governance.der
test "$(wc -c < release.der)" -eq 48
test "$(wc -c < governance.der)" -eq 48
```

Each file contains the 16-byte PKCS#8 prefix followed by the 32-byte seed. These two
files constitute the private key material stored exclusively on the encrypted media.

## Public-key files and fingerprints

The protocol key-bytes file contains 64 lowercase hex characters followed by one
LF (65 bytes total). The fingerprint is the lowercase sha256 hex digest of the
decoded 32 raw key bytes, not the 65 file bytes.

```sh
for role in release governance; do
  openssl pkey -inform DER -in "$role.der" -pubout -outform DER > "$role.pub.der"
  tail -c 32 "$role.pub.der" | od -An -tx1 | tr -d ' \n' > "$role.pub"
  printf '\n' >> "$role.pub"
  test "$(wc -c < "$role.pub")" -eq 65
  printf '%s fingerprint %s\n' "$role" \
    "$(tail -c 32 "$role.pub.der" | sha256sum | cut -d' ' -f1)"
done
```

The two public keys must differ; an overlapping or un-roled pinned configuration is
invalid and the kernel rejects it. Record both fingerprints in the ceremony record by
hand, and read them back against the generated files before leaving the machine.

The two `.pub` files are public. They are the bytes deposited in the trust anchor,
which is what authenticates the role assignment `RELEASE = {K_release}` and
`GOVERNANCE = {K_governance}`, and they are the bytes carried in deployment source as
non-authenticating pinned verifier configuration. Publication repositories and
serving trees contain no key copies at all. [Zenodo deposits and ORCID
anchoring](zenodo-deposits.md) covers the deposit itself and the ORCID entry
that must precede the first signed release.

## Create the two governance media

The declared inventory is exactly two encrypted offline media under separate physical
custody, and the copy operation that creates the second is itself part of the
inventory. Both media carry identical contents, storing both private keys and both
public keys. The release key is backed up here because losing it terminates the chain.

Identify the medium with `lsblk` and confirm the device before formatting;
`luksFormat` destroys what is there. Use the whole device, not a partition.

```sh
dev=/dev/sdX                        # this medium, confirmed with lsblk

cryptsetup luksFormat --type luks2 "$dev"
cryptsetup open "$dev" snh-ceremony
mkfs.ext4 -L snh /dev/mapper/snh-ceremony
mount /dev/mapper/snh-ceremony /run/ceremony/mnt
install -m 0400 release.der governance.der release.pub governance.pub \
  /run/ceremony/mnt/
(cd /run/ceremony/mnt && sha256sum -- *.der *.pub > MANIFEST)
umount /run/ceremony/mnt && cryptsetup close snh-ceremony
```

`cryptsetup open` is what creates `/dev/mapper/snh-ceremony`: its second
argument is the mapper name, and the unlocked container appears under that name
until `cryptsetup close` removes it. The mount point is created here rather
than assumed, because a NixOS system has no `/mnt` unless something declares
one, and this image declares nothing beyond what the ceremony needs.

Repeat the block for the second medium, with `dev` set to that device. Nothing
else in it changes: both media carry identical contents.

The publication owner retains custody of both media across two distinct physical
locations to mitigate single-site physical loss. "Separately controlled" means
physically separate storage locations where a single custodian accounts for each
medium under the halt rule below.

Use a distinct passphrase for each medium so that disclosure of one does not
compromise the other, and ensure each passphrase is independently recoverable
because the protocol tolerates loss of one medium only if the surviving medium
can still be unlocked. Custody of these passphrases is an operational matter that
the specification places outside the wire protocol; record the arrangement in the inventory.

The manifest is written from inside the mount so that it names the four files
and not the path they happened to be mounted at. A manifest carrying absolute
paths would only check against the same mount point, which is exactly the
condition a readback on another machine cannot rely on.

After both media exist, reopen each one and confirm its `MANIFEST` still matches, so
that the inventory records two copies that have both been read back rather than two
copies that were both written:

```sh
dev=/dev/sdX                        # the medium being read back

cryptsetup open "$dev" snh-ceremony && mount /dev/mapper/snh-ceremony /run/ceremony/mnt
(cd /run/ceremony/mnt && sha256sum -c MANIFEST)
umount /run/ceremony/mnt && cryptsetup close snh-ceremony
```

Then power the machine off, which discards the tmpfs working copies.

## The copy inventory

The inventory is the accountability record for a key that is a file rather than a
physical token, so there is no "lost but unread" state to fall back on. It lists the
complete set of authorized persistent copies (in v1, exactly two), together with the
copy operation that produced the second, and it is maintained outside this
repository. Governance operates only while every surviving inventoried medium is
accounted for and controlled.

The release seed also exists as a CI secret, which is not part of the
governance inventory but should be recorded in the same document so that the total
set of places each key exists is written down in one place.

## Install the release seed on CI

CI reads the release key as a file containing exactly 64 lowercase hex characters.
Derive that string from the medium, on the offline machine, and transcribe or carry
it to the point where the secret is set:

```sh
tail -c 32 release.der | od -An -tx1 | tr -d ' \n'
```

The workflow writes the secret to a file under `umask 077`, passes its path to the
kernel, and removes it in an `always` step, following the pattern already used by the
fixture release job in `.forgejo/workflows/`. The kernel checks that the seed
corresponds to the pinned release public key before signing anything, so a
mismatched secret fails at preflight rather than producing an unverifiable release.

The governance seed is never derived to hex and never leaves the media.

## Install the pinned public keys

The two `.pub` files become the deployment's pinned verifier configuration, installed
as root-owned files that the publisher cannot write. In the estate configuration that
is `environment.etc."soranoha/pinned/release.pub"` and its governance counterpart,
whose contents are the 65-byte files produced above, hex plus one newline. These
bytes are non-authenticating: they say which keys this deployment verifies against,
and only the trust anchor says which key holds which role.

Replacing the pins is part of the ceremony's aftermath rather than a later chore. Any
key that was resident on CI during private testing is disposable, and leaving it
pinned would mean verifying against a key whose exposure history is permanent.

## Rehearse governance signing

Immediately after the media exist, and before genesis, sign a checked-in conformance
vector with the real governance key. This exercises the whole path (opening a
medium, constructing the message, producing 64 raw bytes, reading the result back)
against a subject whose bytes are already fixed.

```sh
dev=/dev/sdX                        # the medium being rehearsed

cryptsetup open "$dev" snh-ceremony && mount /dev/mapper/snh-ceremony /run/ceremony/mnt
event=bca7658ee4ae0316fc7304aa5e3657761c167592f8a67a33cdb661bb37e70a59
printf 'snh-governance-event-sig/1:%s' "$event" > rehearsal.msg
openssl pkeyutl -sign -rawin -inkey /run/ceremony/mnt/governance.der -keyform DER \
  -in rehearsal.msg -out rehearsal.sig
openssl pkey -inform DER -in /run/ceremony/mnt/governance.der -pubout \
  -out rehearsal.pubpem
openssl pkeyutl -verify -rawin -pubin -inkey rehearsal.pubpem \
  -in rehearsal.msg -sigfile rehearsal.sig
umount /run/ceremony/mnt && cryptsetup close snh-ceremony
```

The signature must be 64 bytes and must verify. It is disposable ceremony evidence:
record that it verified, and do not check it in. The signature that belongs to this
vector in `soranoha/resources/snh/vectors/signature-vectors.json` is the fixture
signature under the fixture key, and a ceremony signature never replaces it.

Rehearse the second medium the same way, with `dev` set to that device. Both media
must be demonstrated to sign, not just to be readable.

## Sign a governance event after genesis

A withdrawal or amendment event is a small canonical JSON object whose id is the
plain sha256 of its exact bytes. Prepare it online with the kernel, which
formats the canonical JSON and validates invariants before moving to the offline session:

```sh
nix run .#soranoha-kernel -- governance-event-prepare \
  --event "$PWD/candidate.json" --out "$PWD/event.json"
```

Write `candidate.json` however is convenient; key order and surrounding whitespace
belong to the canonicalization rather than to the author. The command rejects
anything the chain would later reject about the event on its own: a duplicate key,
a non-integral number, a schema violation, an unrecognized reason code, and entries
that are not sorted by slug or name one work twice. It needs no chain clone and no
key material, and it prints the `sha256` that the next step signs alongside the
event id. When it reports `"input_was_canonical": false`, `candidate.json` is not
the file to carry; `event.json` is.

Two conditions it cannot check need the chain: whether each slug names a work the
current release admits, and whether an amendment's `amends` names an event the
chain carries. The `governance` subcommand checks both when it appends the signed
event, and neither depends on the signature, so a mistake there costs a corrected
event rather than a repeated ceremony.

Carry the event *file* offline rather than only its hash: the object is under a few
hundred bytes and is readable, and signing a bare hash means signing something you
cannot check.

Boot the same ceremony image, which is why its store path is in the inventory: the
signing session runs on the image the keys were made on, not on whatever image is
current at the time. Take a root shell as above, with the event file at `event.json`:

```sh
dev=/dev/sdX                        # the medium being opened, per lsblk

cat event.json                      # read what is about to be authorized
hex=$(sha256sum event.json | cut -d' ' -f1)
printf 'snh-governance-event-sig/1:%s' "$hex" > event.msg
test "$(wc -c < event.msg)" -eq 91
cryptsetup open "$dev" snh-ceremony && mount /dev/mapper/snh-ceremony /run/ceremony/mnt
openssl pkeyutl -sign -rawin -inkey /run/ceremony/mnt/governance.der -keyform DER \
  -in event.msg -out event.sig
umount /run/ceremony/mnt && cryptsetup close snh-ceremony
test "$(wc -c < event.sig)" -eq 64
```

Carry `event.sig` back and confirm that the `event.json` used online has the same
sha256 as the one signed; a differing byte anywhere makes the signature invalid for
the object being published. The publisher then runs the kernel's `governance`
subcommand with `--event` and `--event-sig`, which decodes the event, verifies the
governance signature against the pinned key, and never rewrites or re-signs it.

## Halt conditions

For a software key, accountability attaches to the declared copy inventory rather
than to possession of a token. An unexplained copy of the key material, lost custody
of any inventoried medium, or any suspected disclosure of a medium or its passphrase
is a suspected compromise, and suspected compromise of a role halts that role's
operations. The verifier keeps accepting past signatures because the pinned set never
changes, but no new signing occurs under that role: a valid signature no
longer proves authority.

A medium that is verifiably destroyed or has failed is not a compromise. Record its
loss in the inventory; governance continues on the remaining copy, and the pinned set
still does not change.

The publication owner decides whether an event is a compromise and records the
decision with its basis in the inventory. There is no second role to escalate to, so
the decision favors halting: the cost of a wrongly declared
halt is suspended publication, and the cost of a missed compromise is an unwanted
withdrawal that the chain records as authorized.

## What never enters the repository

Private key material, the hex release seed, media passphrases, and the custody
inventory itself all stay out of this repository and out of the publication chain.
The repository holds this procedure, the conformance vectors, and the two public
keys (as non-authenticating deployment configuration). Only the trust
anchor authenticates which public key holds which role.
