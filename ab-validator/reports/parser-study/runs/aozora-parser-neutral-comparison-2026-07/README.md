# Existing-parser raw run index

This directory records Task 3's bounded execution attempt under the frozen
study protocol. `run-manifests.json` contains one independent native and one
independent adapter-normalized lane for every included parser. Failures are
results: an empty `raw_outputs` array means no per-work artifact was produced,
not that a work or candidate was excluded.

Four current pinned parser derivations and four current pinned adapter
derivations materialized successfully with:

```sh
nix build ./ab-validator#upstream-parser-aozora2 \
  ./ab-validator#upstream-parser-aozora-rs \
  ./ab-validator#upstream-parser-aozora2html \
  ./ab-validator#upstream-parser-aozora-epub3 \
  ./ab-validator#aozora2-adapter \
  ./ab-validator#aozora-rs-adapter \
  ./ab-validator#aozora2html-adapter \
  ./ab-validator#aozora-epub3-adapter --no-link --print-build-logs
```

The legacy `aozora` parser and its preregistered historical adapter revision
are not exposed by the current flake. The current adapter revision was not
substituted. The neutral full-corpus runner needed to capture immutable native
and adapted per-work outputs is also not implemented. Accordingly the bundle
records explicit build/run failures and contains no fabricated measurements.

Corpus identity combines the NAR hashes of the two pinned corpus derivations,
in preregistered order. Environment identity covers the frozen environment
variables and required host-capture fields. The exact values used are in
`identity-inputs.txt`; its labels are repository-relative and contain no
machine-local output path.

Corpus-scale output remains out of source. Once a conforming runner exists,
it must write raw artifacts outside the checkout, hash each artifact, then
commit only the immutable manifests and deliberately small fixtures here.
