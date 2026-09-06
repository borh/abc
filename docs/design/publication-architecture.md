# Publication architecture

Soranoha produces TEI and visible-body plaintext from Aozora Bunko source markup.
The Rust parser and converter produce supporting structured representations;
Clojure owns publication rendering, assessment, validation and the signed chain.
Nix pins toolchains and exposes reproducible commands. The
[snh specification](snh-protocol-v1.md) defines the publication wire contract.

## Reuse follows derivation inputs

The build kernel stores immutable bytes by SHA-256 and indexes constructive traces
in SQLite. A trace key contains the stage, its version, toolchain identity and
named input hashes. Work-local catalog rows and source archives feed separate
stages so an unrelated work's metadata does not become a dependency of its text.
Output hashes allow unchanged intermediate results to be reused downstream.
Stage authors must include every semantic input in that identity; a cache cannot
infer a dependency omitted by its caller.

The cache is disposable and private. Only trusted pipeline processes may write
the trace database: a hash-named blob does not authenticate the assertion that a
stage produced it. Atomic blob installation precedes trace publication. Missing
blobs cause recomputation, while execution history retains conflicting results
for the same derivation key as evidence of nondeterminism.

## Assessment is separate from rendering

Building an export does not admit a work to publication. Assessment evaluates
current facts and their applicability against the selected source revision;
publication preflight checks those inputs before running the build. A catalog
contribution listing is not proof of exhaustive authorship or copyright ownership.
The implemented Aozora reliance route records the upstream assertion and its
source applicability, separately from independent rights assessment.
See [assessment evaluation](../../soranoha/docs/assessment-evaluation.md).

Source fidelity measures markup preservation. It is research evidence separate
from rights admission and TEI schema validation. Plaintext contains visible body
text; ruby readings, attribution and provenance remain in TEI or sidecars.

## Publication and serving have different authority

A signed manifest identifies admitted works and artifacts. Governance events and
role-bound keys authorize publication history under the verifier's rules. Git
provides the append-only publication transaction; the origin tip selects current
history. The private CAS and a serving directory do not establish that authority.

Serving activation verifies the chain and exports the selected commit before
atomically changing its active pointer. A failed verification leaves the previous
export active. Storage paths, backup and retention are server configuration, not
wire protocol. Private experimental lineages may be explicitly reset; a public
lineage requires a fresh reviewed genesis and the independent authorship checkpoint
described in the [deployment guide](../private-publication.md).

Protocol semantic changes require a permanent architectural decision record with
the changed contract and rationale, together with corresponding specification,
schema and conformance-vector updates. Development coordination is not part of
that contract.
