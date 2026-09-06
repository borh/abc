# Soranoha per-work conversion and validation

These maintained namespaces convert catalog records and parser IR into
publication artifacts, inspect source bundles, and validate artifacts. They
originated in ABC; Soranoha does not require ABC namespaces. Metadata construction
returns validated values directly to the build stages.

ABC owns record schemas and the TEI profile. Resolve those assets through
`soranoha.ported.assets/*root*`, supplied explicitly by the application.

Record and source-bundle identities retain their defined JSON canonicalization
in `jcs.clj`. Protocol identities use `soranoha.core.canonical`; both share the
byte hashing implementation in `soranoha.core.hash`.

Source fidelity and current validation contracts govern changes to these modules.
Historical artifact comparisons are evidence, not a compatibility requirement.
