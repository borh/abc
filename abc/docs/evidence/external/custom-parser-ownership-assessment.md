# Custom Parser Ownership Assessment

Assessment date: 2026-07-12
Review after: 2026-10-12
Decision owner: Soranoha project owner
Technical reviewer: Soranoha parser maintainer
Authority: development-only

## Observed Facts

- The implementation is hard-detached from `P4suta/aozora` at revision
  `1a4f864603970983719655aa4af4525958ac2d38`.
- ABC owns the parser-IR schema, mapping/admission boundary, plaintext and TEI
  rendering policy, preservation sidecar, and publication validation contract.
- The bounded July survey considered the `aozora-pipeline`, `aozora-core`,
  `aozora-rs-core`, `aozora2html`, and `aozora-epub3` parser families, plus the
  build-fresh option.
- The survey did not evaluate a preregistered neutral contract.
- None of the surveyed external implementations supplied the complete owned
  contract without project adaptation.
- The parser maintainer currently commits to maintaining the detached parser
  and its ABC integration boundary.

## Assumptions

- Continued maintainer availability.
- Continued project control of the publication contract.

## Falsifiers

- Retirement of the custom parser in favor of an implementation that assumes
  the complete owned contract.
- A superseding owner decision after maintainer capacity becomes inadequate.

## Non-Authority

This assessment grants no exact-tuple admission, does not complete a neutral
comparison, does not qualify a release, and grants no publication authority.
Phase 5 is historical evidence and does not prove current parser quality.
