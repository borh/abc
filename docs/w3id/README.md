# w3id.org/soranoha registration

Soranoha's published TEI declares the namespace `https://w3id.org/soranoha/ns/tei`,
and its record schemas are identified under `https://w3id.org/soranoha/schemas/`.
Those IRIs are in signed, content-addressed bytes, so they must stay stable
independently of where the project is served. This directory holds the files
submitted to [perma-id/w3id.org](https://github.com/perma-id/w3id.org) to make
them resolve, kept here because the redirect map is a permanent commitment of
the project rather than a one-time chore.

Submission adds `ids/soranoha/.htaccess` and `ids/soranoha/README.md` to that
repository. `htaccess` and `w3id-README.md` here are those two files; the local
names avoid a dotfile and a second README in this tree.

Three prefixes are claimed, each redirecting to the corresponding `soranoha.org`
path, plus a bare `/soranoha/`:

| Prefix | Denotes |
|---|---|
| `/soranoha/ns/` | TEI extension vocabulary namespaces |
| `/soranoha/schemas/` | Record schema identifiers embedded in published records |
| `/soranoha/works/` | Landing pages for published works |

`/soranoha/works/` is claimed now although nothing renders it yet. Claiming a
prefix costs nothing and commits nothing, whereas discovering later that someone
else holds it is unrecoverable. Whether citations render the w3id form or the
`soranoha.org` form is decided separately, in soranoha-szg.11.

Redirects are 302 rather than 301 throughout: the target is a current serving
location, and permanent client-side caching would defeat the point of an
indirection service.

## Ordering

Register `soranoha.org` and make it resolve before opening the pull request, so
review does not encounter a dead redirect target. `https://w3id.org/soranoha/ns/tei`
must resolve before genesis, because the IRI is already in published bytes.

## Why not `/abc/`

The vocabulary previously used `https://w3id.org/abc/ns/tei`, which never
resolved. The perma-id repository has no `ids/abc` directory, so the prefix was
unclaimed and remained claimable by anyone, who would then have controlled what
the namespace of Soranoha's published TEI resolved to. The w3id naming policy
also permits administrators to refuse identifiers that are too generic, which a
three-letter `abc` plainly is, so claiming it was not a reliable option either.
