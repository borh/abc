# Key ceremony and offline governance signing

The [snh specification](design/snh-protocol-v1.md) binds a publication chain to two
disjoint Ed25519 roles at genesis. The RELEASE key is online and held by CI; it signs
release manifests. The GOVERNANCE key is offline and owner-held; it signs the
withdrawal and amendment events that are the only way to remove a published work.
Neither pinned key can ever change: a different pinned set ends that chain and starts
a new one with no continuity. This document is the procedure that establishes both
keys and the procedure that uses the governance key afterwards.

Rehearsal is the point of the ceremony, not the document. The first real use of the
governance key will be a rights-holder withdrawal request, under time pressure, and
it must not also be the first execution of this procedure. The ceremony therefore
signs a checked-in conformance vector with the real governance key while a mistake
is still free.

## What the ceremony fixes permanently

Both keys are generated fresh for the public chain. No key that has been resident on
a network-connected host may be pinned, because the pinned set cannot later be
retired, so any exposure in a key's history is permanent.

The two roles fail differently. A compromised release key can publish an unwanted
release but cannot withdraw a work; the damage is visible and bounded. A compromised
governance key can withdraw anything. A *lost* release key is equally terminal in a
different direction: without it no further release can ever be signed, and the chain
cannot be continued under a replacement. Both keys therefore need durable backups,
and both backups live on the governance media created below.

## Ceremony parameters

This document does not name the hardware. The ceremony records the following in the
custody inventory at execution time, and the inventory is not kept in this
repository:

- the offline machine and the exact live image, identified by its published checksum;
- the two storage media, identified by manufacturer serial, and the physical custody
  location of each;
- the passphrase custody arrangement for each medium;
- the date, the operator, and any witness.

## The offline machine

Use a machine booted from a read-only live image with its root filesystem in RAM,
with networking not merely disabled in software but physically absent or removed:
no cable, no wireless module, no tethered device. The key material must never reside
on a network-connected host, and a RAM-only root means the working copies disappear
when the machine is powered off rather than persisting on an internal disk.

The procedure needs OpenSSL 3, GNU coreutils and `cryptsetup`. Every step below uses
only those. Run `umask 077` first, and work in a directory under `/run/user` or
another tmpfs so that nothing is written to persistent storage except the two media.

## Prove the toolchain before generating anything

The specification defines exact bytes, and a signing procedure that produces
plausible-looking output over the wrong bytes is the failure this step exists to
catch. Before any real key exists, reproduce a signature that is already checked into
the repository. Carry `soranoha/resources/snh/vectors/signature-vectors.json` to the
offline machine, or transcribe the three constants below from it.

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

Each file is the 16-byte PKCS#8 prefix followed by the 32-byte seed. These two files
are the key material. They are what goes onto the encrypted media, and they are what
must never be copied anywhere else.

## Public-key files and fingerprints

The protocol's key-bytes file is exactly 65 bytes: 64 lowercase hex characters
followed by one LF. The fingerprint is the lowercase sha256 hex over the *decoded* 32
raw key bytes, never over the 65 file bytes.

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
serving trees contain no key copies at all.

## Create the two governance media

The declared inventory is exactly two encrypted offline media under separate physical
custody, and the copy operation that creates the second is itself part of the
inventory. Both media carry the same contents: both private keys and both public
keys. The release key is backed up here because losing it would end the chain.

For each medium, with `dev` set to its whole-device path:

```sh
cryptsetup luksFormat --type luks2 "$dev"
cryptsetup open "$dev" snh-ceremony
mkfs.ext4 -L snh "/dev/mapper/snh-ceremony"
mount /dev/mapper/snh-ceremony /mnt
install -m 0400 release.der governance.der release.pub governance.pub /mnt/
sha256sum /mnt/*.der /mnt/*.pub > /mnt/MANIFEST
umount /mnt && cryptsetup close snh-ceremony
```

The publication owner holds both media, at two separate physical locations, so that
no single fire or theft takes both. This is what "separately controlled" means here:
separate control domains for the media, with accountability for the halt rule below
resting on one person who can actually answer for each medium. Adding a second
custodian would improve survivability if the owner were unreachable, at the cost of
making someone else's record-keeping a halt condition.

Use a distinct passphrase per medium so that disclosure of one passphrase does not
unlock the other, and make sure each passphrase is independently recoverable: the
protocol tolerates one medium being destroyed and governance continuing on the
other, which is only true if the surviving medium can still be opened. Custody of
these passphrases is an operational matter that the specification deliberately places
outside the wire protocol; record the arrangement in the inventory.

After both media exist, reopen each one and confirm its `MANIFEST` still matches, so
that the inventory records two copies that have both been read back rather than two
copies that were both written. Then power the machine off, which discards the tmpfs
working copies.

## The copy inventory

The inventory is the accountability record for a key that is a file rather than a
physical token, so there is no "lost but unread" state to fall back on. It lists the
complete set of authorized persistent copies — in v1, exactly two — together with the
copy operation that produced the second, and it is maintained outside this
repository. Governance operates only while every surviving inventoried medium is
accounted for and controlled.

The release seed additionally exists as a CI secret, which is not part of the
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

## Rehearse governance signing

Immediately after the media exist, and before genesis, sign a checked-in conformance
vector with the real governance key. This exercises the whole path — opening a
medium, constructing the message, producing 64 raw bytes, reading the result back —
against a subject whose bytes are already fixed.

```sh
cryptsetup open "$dev" snh-ceremony && mount /dev/mapper/snh-ceremony /mnt
event=bca7658ee4ae0316fc7304aa5e3657761c167592f8a67a33cdb661bb37e70a59
printf 'snh-governance-event-sig/1:%s' "$event" > rehearsal.msg
openssl pkeyutl -sign -rawin -inkey /mnt/governance.der -keyform DER \
  -in rehearsal.msg -out rehearsal.sig
openssl pkey -inform DER -in /mnt/governance.der -pubout -out rehearsal.pubpem
openssl pkeyutl -verify -rawin -pubin -inkey rehearsal.pubpem \
  -in rehearsal.msg -sigfile rehearsal.sig
umount /mnt && cryptsetup close snh-ceremony
```

The signature must be 64 bytes and must verify. It is disposable ceremony evidence:
record that it verified, and do not check it in. The signature that belongs to this
vector in `soranoha/resources/snh/vectors/signature-vectors.json` is the fixture
signature under the fixture key, and a ceremony signature never replaces it.

Rehearse the second medium the same way. Both media must be demonstrated to sign, not
just to be readable.

## Sign a governance event after genesis

A withdrawal or amendment event is a small canonical JSON object whose id is the
plain sha256 of its exact bytes. Prepare it online, where the kernel's schema and
canonicalization rules apply, and carry the event *file* offline rather than only its
hash: the object is under a few hundred bytes and is readable, and signing a bare
hash means signing something you cannot check.

On the offline machine, with the event file at `event.json`:

```sh
cat event.json                      # read what is about to be authorized
hex=$(sha256sum event.json | cut -d' ' -f1)
printf 'snh-governance-event-sig/1:%s' "$hex" > event.msg
test "$(wc -c < event.msg)" -eq 91
cryptsetup open "$dev" snh-ceremony && mount /dev/mapper/snh-ceremony /mnt
openssl pkeyutl -sign -rawin -inkey /mnt/governance.der -keyform DER \
  -in event.msg -out event.sig
umount /mnt && cryptsetup close snh-ceremony
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
operations. The verifier keeps accepting past signatures — the pinned set never
changes — but no new signing occurs under that role, because a valid signature no
longer proves authority.

A medium that is verifiably destroyed or has failed is not a compromise. Record its
loss in the inventory; governance continues on the remaining copy, and the pinned set
still does not change.

The publication owner decides whether an event is a compromise and records the
decision with its basis in the inventory. There is no second role to escalate to, so
the decision is deliberately biased towards halting: the cost of a wrongly declared
halt is suspended publication, and the cost of a missed compromise is an unwanted
withdrawal that the chain records as authorized.

## What never enters the repository

Private key material, the hex release seed, media passphrases, and the custody
inventory itself all stay out of this repository and out of the publication chain.
The repository holds this procedure, the conformance vectors, and — as
non-authenticating deployment configuration — the two public keys. Only the trust
anchor authenticates which public key holds which role.
