# ADR 0033: Source Bundle Identity

Status: Accepted
Date: 2026-07-12
Accepted: 2026-07-12
Supersedes: none
Amends: ADR 0001 [scope: work_content_hash equality relation]
Depends on: ADR 0010, ADR 0023, ADR 0025
Source: `docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md`

## Context

The Aozora build pipeline currently overloads source identity. The parser chain
hashes the extracted text bytes while ABC hashes the raw ZIP, and source
snapshot materialization requires those unequal values to match. Selecting the
text hash would omit bundled illustrations; selecting the raw ZIP hash would
make compression metadata part of logical work identity.

## Decision

This ADR establishes a versioned source-bundle manifest that gives archive,
logical bundle, individual member, and parser input distinct identities:

- `archive_hash` identifies the exact original ZIP bytes.
- `bundle_hash` identifies the `abc-source-bundle-v1` canonical identity object
  containing every safe non-directory member's NFC-normalized path and SHA-256
  member hash, plus the selected primary text member path. Exact byte lengths
  remain non-identity manifest metadata.
- `member_hash` identifies one member's exact uncompressed bytes.
- `primary_text_hash` identifies the exact bytes supplied to the parser.

For new-schema artifacts, `work_content_hash` is an exact alias of
`bundle_hash`. It does not equal or substitute for `archive_hash` or
`primary_text_hash`.

The bundle identity object sorts members by normalized path and is hashed as
RFC 8785 JCS. ZIP compression, timestamps, comments, and entry order are not
bundle identity. Member paths and exact bytes are identity. Unsafe paths,
duplicate normalized paths, portability collisions, unreadable members, and a
semantic primary-text candidate count other than exactly one fail admission;
recognized packaging metadata such as `__MACOSX/._*.txt` remains bundle
content but is not a primary-text candidate. UTF-8-flagged entry names decode
strictly as UTF-8; otherwise a valid Info-ZIP Unicode Path extra field wins,
then raw names fall back strictly to windows-31j. NFC paths are screened for
collisions with ICU4J 78.3 full Unicode case folding. Changing the decoder
precedence or pinned Unicode folding data requires a new construction tag.

ABC owns archive inspection and bundle identity. The Clojure/JVM inspector is
the sole v1 `bundle_hash` producer. Parser adapters own decoding and
`primary_text_hash`; ABC supplies the authoritative bundle hash to parser-IR
rather than asking the text adapter to infer whole-bundle identity. The
adapter's independently computed `primary_text_hash` must equal the primary
member hash because v1 hashes the raw member bytes before decoding.
Publication reuse uses `work_content_hash = bundle_hash`.

`primary_text_member` is identity-bearing even though policy derives it from
the member set: selecting a different member changes logical parser input.
`byte_length` remains non-identity because it is implied by the exact member
bytes/hash and cannot independently change interpretation. The packaging-
metadata exclusion patterns are immutable within the v1 construction; changing
them requires a new construction tag.

Historical manifests retain their schema hashes and historical
`work_content_hash` meaning. They are never reinterpreted or rewritten.

## Implementation Status

Accepted on 2026-07-12 with the following implementation evidence:

- `abc.tools.source-bundle` implements the bounded, deterministic v1 ZIP
  inspector; `source-bundle.schema.json` and canonical fixtures pin manifest,
  path-decoding, Unicode-folding, limit, and JCS identity behavior.
- The `source-bundle-corpus` Nix check reproduces the checked-in report for
  Aozora commit `0e9ea3e586eb0aa34039fabfc85a407d2f98b165` and verifies the
  production bounds and damaged-archive disposition.
- AAT and parser-IR schemas expose `primary_text_hash`; the Rust converter's
  explicit `work_content_hash` option and CLI carry ABC's authoritative bundle
  identity without computing it. Rust tests pin alias validation, distinct
  roles, schema mirrors, and the current and frozen historical mapping
  artifacts.
- Publication build tests prove bundle-manifest materialization, adapter
  integrity, bundle-keyed reuse, strict atomic rejection, best-effort counted
  rejection, and non-releaseability after any admission failure.
- Workset and source-snapshot tests prove complete historical readability and
  role-specific validation of archive, canonical bundle, primary member, and
  parser-input hashes, including persisted manifest-byte integrity.
- P16 evolution tests prove image edits rotate bundle identity and rebuild,
  metadata-only repacks rotate only archive identity and reuse, and text edits
  rotate bundle and primary-text identity. P16.3 composes the real path without
  an expected-failure gate; D7 is fixed with dated evidence in the divergence
  table.

The pinned corpus contains one Java-unreadable, `7zz`-recoverable archive. V1
intentionally rejects it rather than retaining the unbounded extract-all
fallback; upstream repair is required. Strict builds abort atomically on any
admission error. Best-effort builds may continue and record per-work
`derive_failures`, but any nonzero failure count is not release-admissible.

## Consequences

Logical content equality becomes stable across metadata-only ZIP repacks while
remaining sensitive to illustrations and every other bundled member. Exact
upstream provenance and exact parser input remain independently queryable.

The text-only adapter no longer authors whole-work identity. That authority
moves to ABC's package-admission boundary, eliminating the trust-boundary error
that caused D7. Metadata-only repacks stop rotating `work_content_hash`,
`artifact_id`, and publication reuse for byte-identical logical bundles.

The current coarse publication reuse marker conservatively rebuilds on an
image-only bundle change even though current TEI may remain byte-identical.
This preserves complete source derivation identity at the cost of temporary
over-invalidation; a dependency-minimal parser/publication cache is deferred.

The change rotates relevant schemas and artifact identities for newly produced
artifacts. It adds archive inspection, path-safety, decompression-limit, and
canonicalization-fixture obligations. Cross-language reproduction remains an
admission guard for any future non-JVM producer, not a v1 implementation task.
The change also prevents a parser adapter from being the sole author of
whole-work identity, because the adapter receives only the primary text bytes.

## Acceptance Criteria

- `schemas/source-bundle.schema.json` validates a canonical manifest covering
  every non-directory member; `test/abc/tools/source_bundle_test.clj` and
  `test/abc/tools/schema_test.clj` pin the producer and schema contracts.
- The Clojure producer reproduces one checked-in canonical identity fixture and
  `bundle_hash` byte-for-byte. Rust remains a consumer until a conformant
  implementation reproduces that fixture; it must not author bundle identity.
- `test/abc/sim/content_sim_test.clj` proves metadata-only repacks preserve
  `bundle_hash` while image changes rotate it.
- Parser-IR records both `work_content_hash = bundle_hash` and the independently
  verified `primary_text_hash`.
- Source snapshot validation checks each hash role by its own construction and
  never requires cross-role equality, as pinned by
  `test/abc/tools/materialize_source_snapshot_test.clj`.
- P16.3 passes without `expected-failure*`; D7 is marked fixed with dated
  evidence.
- Historical manifests and worksets remain readable without reinterpretation.
- Archive member-count, per-member byte, and total-uncompressed-byte limits are
  selected from measured corpus evidence and enforced before production use;
  `test/abc/tools/source_bundle_report_test.clj` validates the report contract.
- Tests pin both strict atomic-abort and best-effort counted-failure admission
  dispositions, and release validation rejects any nonzero derive-failure
  count.
