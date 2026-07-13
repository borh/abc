# ADR 0033: Source Bundle Identity

Status: Accepted
Date: 2026-07-12
Accepted: 2026-07-12
Validation scope: full-corpus
Release authority: publication
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
Source-bundle v1 uses an explicit string-domain RFC 8785 serializer that keeps
slashes and non-ASCII Unicode unescaped. The historical shared ABC JCS path is
not changed in place because doing so would rotate and reinterpret older schema
and artifact hashes. Before serialization, v1 validates every string value and
object key as well-formed UTF-16 and rejects malformed surrogate sequences with
their identity-object path.

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
  inspector over one private read-only staged artifact, so archive and member
  identities cannot observe different path contents; `source-bundle.schema.json`,
  `fixtures/source-bundle/abc-source-bundle-v1-known-answer.json`, and its
  focused test pin manifest, path-decoding, Unicode-folding, limits, canonical
  UTF-8 bytes, and the literal JCS digest.
- The `source-bundle-corpus` Nix check reproduces the checked-in report for
  Aozora commit `0e9ea3e586eb0aa34039fabfc85a407d2f98b165` and verifies the
  production bounds and damaged-archive disposition.
- The checked corpus report uses the same staged, bounded body scan as
  production admission. Actual streamed maxima remain 12,631,833 bytes per
  member and 27,874,310 bytes per bundle. It also pins one understated
  central-directory declaration: `cards/001393/files/50710_ruby_36965.zip`
  declares 68,007 bytes for `fushigino_kunino_alice_musical.txt`, while the
  bounded stream reads 68,497.
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
  parser-input hashes, including persisted manifest-byte integrity and
  structural rejection of coordinated member reordering plus re-signing.
- The hermetic Phase 5 checkpoint pins the exact bytes and canonical hash of
  the admitted frozen v2-0.4.0 mapping artifact rather than consulting the
  later live mapping generation.
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

- **ADR-0033-C1 — fixture-behavior:** The source-bundle schema validates the
  canonical all-member manifest fixture, as asserted by
  `test/abc/tools/source_bundle_test.clj`.
- **ADR-0033-C2 — fixture-behavior:** The Clojure producer reproduces the
  checked-in canonical identity bytes and `bundle_hash` byte-for-byte.
- **ADR-0033-C3 — fixture-behavior:** Metadata-only repacks preserve
  `bundle_hash`, while image changes rotate it.
- **ADR-0033-C4 — fixture-behavior:** Parser-IR records
  `work_content_hash = bundle_hash` and the independently verified
  `primary_text_hash` in their distinct roles.
- **ADR-0033-C5 — fixture-behavior:** Source snapshot validation checks archive,
  canonical bundle, primary member, and parser-input hashes by their own
  constructions without cross-role equality.
- **ADR-0033-C6 — fixture-behavior:** P16.3 passes without an
  `expected-failure*` gate.
- **ADR-0033-C7 — structural-invariant:** D7 is `:fixed` with dated
  `2026-07-12` structural evidence.
- **ADR-0033-C8 — fixture-behavior:** A complete legacy-shaped workset fixture
  remains readable without reinterpretation.
- **ADR-0033-C9 — corpus-behavior:** The pinned source-bundle corpus report for
  Aozora commit `0e9ea3e586eb0aa34039fabfc85a407d2f98b165` reproduces its
  measured maxima.
- **ADR-0033-C10 — fixture-behavior:** Member-count, per-member-byte, and total
  uncompressed-byte limits are enforced at both declared-size and streamed-byte
  boundaries.
- **ADR-0033-C11 — fixture-behavior:** Strict admission failure aborts
  atomically; best-effort admission records counted failures with
  `release_admissible=false`, continues only the partial workflow, and exits
  the build with status 1.

## Historical Evidence

Historical manifests and worksets remain readable without reinterpretation.
Historical manifests retain their schema hashes and historical
`work_content_hash` meaning, and complete legacy-shaped worksets remain
readable without reinterpretation. This historical compatibility does not
reinterpret or rewrite those manifests.
