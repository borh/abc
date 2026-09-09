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

Four prefixes are claimed, each redirecting to the corresponding `soranoha.org`
path, plus a bare `/soranoha/`:

| Prefix | Denotes |
|---|---|
| `/soranoha/ns/` | TEI extension vocabulary namespaces |
| `/soranoha/schemas/` | Record schema identifiers embedded in published records |
| `/soranoha/works/` | Landing pages for published works |

Redirects are 302 rather than 301 throughout because the target is a current
serving location, and permanent client-side caching would defeat the purpose
of an indirection service.

## Ordering

Register `soranoha.org` and make it resolve before opening the pull request, so
review does not encounter a dead redirect target. `https://w3id.org/soranoha/ns/tei`
must resolve before genesis, because the IRI is already in published bytes.
