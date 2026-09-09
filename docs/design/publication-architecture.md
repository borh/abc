# Publication architecture

Soranoha produces TEI, visible-body plaintext and Markdown from Aozora Bunko source markup.
The Rust parser and converter produce supporting structured representations;
Clojure owns publication rendering, assessment, validation and the signed chain.
TEI is the stored transcription; separate stages derive plaintext and Markdown
from its established reading. [Research annotation layers](../annotation-layers.md)
target an identified text view and enrich TEI without replacing that transcription.
Nix pins toolchains and exposes reproducible commands. The
[snh specification](snh-protocol-v1.md) defines the publication wire contract.

## Reuse follows derivation inputs

The build kernel stores immutable bytes by SHA-256 and indexes constructive traces
in SQLite. A trace key contains the stage, its version, toolchain identity and
named input hashes. Work-local catalog rows and source archives feed separate
stages so an unrelated work's metadata does not become a dependency of its text.
Output hashes allow unchanged intermediate results to be reused downstream.
The text projections consume full TEI bytes: a header edit reruns those projections,
but unchanged reading-view identities remain reusable by analysis producers.
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
wire protocol.

An export holds two kinds of file, and the distinction is the boundary that keeps
presentation out of verification. Chain content (manifests, signatures, blobs,
governance events, the head pointer) is copied byte for byte, and the work-facing
routes are names over it. The browse layer is generated: a landing page, author,
title and NDC indexes, one page per work, a reading view per work, a page for
each withdrawn work, the rights and citation pages, the project's own
documentation, and a search index the pages query in the browser. None of it is
hashed into the chain or checked by a verifier, and no manifest names a browse
file, although the rights grant's `statement_url` is a route the browse layer
answers. It exists so the corpus has an entry point that is not a 64-character
hex string. Because generation is a pure function of the release
being exported, the exporter's reuse check covers those pages exactly as it
covers chain content, and the same commit re-exports byte-identically. Serving
stays one static tree with no application runtime.

The reading view renders each work's published TEI into HTML at export time
from that work's own artifact bytes, rather than using a stylesheet reference
inside the published file. An `<?xml-stylesheet?>` processing instruction would
change every TEI artifact's bytes, moving presentation into the artifact ids,
the conformance vectors, and genesis. That tight coupling would prevent presentation
updates without republishing the corpus. A rendering that lives in the serving
layer is replaced at the next activation. It states the
release it was rendered from and links the artifact it was rendered out of, so
a reader who doubts the rendering can check it against the bytes. It also selects
the same text the published plaintext projection selects, one lemma from an
apparatus and the first supported branch of a choice, so the two cannot disagree
about which reading the work carries. They still differ in what surrounds that
text: plaintext drops ruby readings and notes for analysis workflows that consume
base text, and the reading view shows them.

Private experimental lineages may be explicitly reset; a public lineage requires
a fresh reviewed genesis and the independent authorship checkpoint described in
the [deployment guide](../private-publication.md).

Protocol semantic changes require a permanent architectural decision record with
the changed contract and rationale, together with corresponding specification,
schema and conformance-vector updates. Development coordination is not part of
that contract.
