# Source Bundle Identity Design

Date: 2026-07-12
Status: Implemented; accepted by ADR 0033
Decision record: ADR 0033
Amends: ADR 0001 (`work_content_hash` meaning)

## Problem

The Aozora publication path currently gives one ambiguous name, `source_hash`,
to two different values:

- the parser adapter hashes the extracted text member bytes; and
- `soranoha-build-publication` hashes the raw ZIP bytes.

`materialize-source-snapshot!` requires those values to equal, so real build
outputs do not compose. Choosing either existing value as the single canonical
answer would preserve the ambiguity:

- a raw-ZIP hash mixes logical content with compression, timestamps, comments,
  and entry ordering; and
- a text-only hash ignores illustrations and any other members in the source
  package.

The system needs separate identities for the downloaded artifact, the logical
bundle, each member, and the exact parser input.

## Decision

Introduce a versioned source-bundle manifest and four explicitly named hash
roles:

| Field | Equality relation |
| --- | --- |
| `archive_hash` | byte-identical original ZIP artifact |
| `bundle_hash` | identical safe member paths and member bytes under the `abc-source-bundle-v1` construction |
| `member_hash` | byte-identical uncompressed bytes for one named member |
| `primary_text_hash` | byte-identical bytes supplied to the parser adapter |

`work_content_hash` remains a compatibility coordinate and equals
`bundle_hash`. New schemas and code describe it as the logical source-bundle
identity, never as an archive or parser-input hash. `archive_hash` and
`primary_text_hash` remain separate provenance/derivation coordinates and are
never required to equal `work_content_hash`.

## Source-Bundle Manifest

The canonical identity object is a JSON value:

```json
{
  "construction": "abc-source-bundle-v1",
  "members": [
    {
      "path": "000101.txt",
      "member_hash": "sha256:..."
    },
    {
      "path": "figures/000101_01.png",
      "member_hash": "sha256:..."
    }
  ],
  "primary_text_member": "000101.txt"
}
```

`bundle_hash` is:

```text
sha256(RFC8785-JCS(source_bundle_identity_object))
```

The v1 construction uses an explicit RFC 8785 string-domain serializer: `/`,
non-ASCII Unicode, and U+2028/U+2029 are not optionally escaped, while object
keys are sorted and required JSON escapes remain canonical. Numeric scalars are
rejected because this identity object is string-only. ABC's older shared JCS
entry point retains its historical Charred escaping behavior so existing
schema and artifact hashes are not reinterpreted; source-bundle v1 does not use
that legacy path. Before Charred sees a string, v1 validates every string value
and object key as well-formed UTF-16: valid supplementary pairs pass, while
lone, reversed, truncated, or otherwise malformed surrogate sequences fail
with stable identity-object path evidence.

Identity fields record facts that can change logical interpretation, not every
value derivable from the bytes. `primary_text_member` remains identity-bearing
because selecting a different primary from the same member set changes the
logical parser input and therefore the work derivation. `byte_length` is
excluded because it is fully implied by a member's exact bytes/member hash and
cannot independently change interpretation.

The persisted source-bundle manifest contains the identity object plus
non-identity provenance such as `archive_hash`, each member's exact byte
length, decoded pre-normalization path, and name source
(\`efs-utf8|unicode-extra|windows-31j\`). It may later carry descriptive
media types, but media-type detection is excluded from v1 identity because it
depends on a detector and versioned policy. Normalized paths and member hashes
are sufficient to identify the bundle content: adding byte length to identity
adds no discrimination short of a SHA-256 collision and would introduce the
first numeric field into an identity object before ADR 0001's numeric-JCS
cross-language obligation is satisfied.

### Member rules

- Include every non-directory ZIP member, not only `.txt` or recognized image
  formats.
- Hash exact uncompressed member bytes. Do not decode text, normalize Unicode,
  transcode images, or otherwise transform bytes before hashing.
- Decode entry names with this fixed precedence using Apache Commons Compress
  1.28.0: (1) strict UTF-8 when general-purpose flag bit 11 is set; (2) a valid
  Info-ZIP Unicode Path extra field (`0x7075`), whose CRC must match the raw
  legacy name bytes; (3) strict windows-31j over the raw name bytes. Commons
  Compress exposes the raw name and validates the Unicode extra field; v1
  performs strict decoding itself for flag/fallback cases rather than accepting
  replacement characters. The precedence is part of the construction, not a
  host default.
- Normalize decoded names to Unicode NFC, normalize separators to `/`, and
  reject absolute paths, drive-qualified paths, empty segments, `.` segments,
  and `..` segments. The NFC-normalized path is the identity path. The decoded
  pre-normalization path remains non-identity provenance; the exact archive is
  recoverable by `archive_hash`, so v1 does not add a custom raw-central-
  directory-byte extractor solely to duplicate that evidence.
- Reject duplicate normalized paths.
- Reject normalized paths that collide after ICU4J 78.3 full Unicode case
  folding (`UCharacter.foldCase` with default mappings). The case-folded value
  is admission-only and is not hashed. The pinned ICU Unicode data version is
  part of the v1 admission construction; changing it or the folding behavior
  requires a new construction tag and corpus migration evidence.
- Sort members lexicographically by normalized path before JCS hashing. ZIP
  entry order is not identity-bearing.
- Exclude recognized packaging metadata paths from primary-text candidacy,
  initially `__MACOSX/**` and AppleDouble basename prefixes `._`; these members
  remain in the bundle manifest because v1 identifies all archived members.
  Require exactly one remaining `.txt` candidate and record its normalized path
  as `primary_text_member`. Zero or multiple semantic candidates fail loudly.
  This deliberately replaces current order-dependent behavior: the Java path
  chooses the first `.txt` in `ZipFile.entries`, while the damaged-archive
  fallback chooses the first extracted `.txt` encountered by `file-seq`.

The packaging-metadata exclusion set is an immutable component of
`abc-source-bundle-v1`, not a mutable denylist. Adding or changing a pattern
requires a new construction tag and migration evidence, because it can change
admission or `primary_text_member` for an existing member set.

Consequently, changes to compression, ZIP timestamps, comments, or entry order
change `archive_hash` but not `bundle_hash`. Renames, additions, removals, or
byte changes to any member change `bundle_hash`.

## Ownership and Data Flow

ABC owns source-package admission and logical bundle identity:

1. `soranoha-build-publication` invokes the bounded bundle inspector once per
   selected ZIP.
2. Within that invocation, the inspector opens member streams to enforce limits
   and compute member hashes, then separately reads the archive bytes to compute
   `archive_hash`. It produces member records, the chosen primary-text member
   bytes, `archive_hash`, and `bundle_hash`.
3. ABC writes `source-bundle.json` beside `official-source.json` and records
   both hashes in the selection report.
4. ABC passes the primary-text bytes and an explicit identity context to the
   adapter boundary.

ABC's Clojure/JVM inspector is the sole v1 producer of `bundle_hash`. Rust
consumers validate the hash-string shape and carry the value but do not
recompute JCS. A Rust JCS implementation and cross-language reproduction of the
canonical fixture are deferred defensive conformance work, required before any
Rust component is allowed to author bundle identity. This keeps v1 within the
existing ADR 0001 implementation boundary rather than creating an implicit
Rust JCS project.

The parser chain owns interpretation of the primary text:

- AAT `meta.source_hash` remains the exact parser-input hash for compatibility,
  but new documentation and fields call this `primary_text_hash`.
- Parser-IR carries `source.primary_text_hash` and
  `source.work_content_hash`. The converter does not infer bundle identity from
  AAT; ABC supplies the authoritative `work_content_hash` at the orchestration
  boundary.
- Decoding, encoding detection, normalization declarations, parser build, and
  parser configuration remain separate derivation coordinates.

Because the adapter receives the primary member's raw bytes and hashes before
decoding, `primary_text_hash` MUST equal the `member_hash` recorded for
`primary_text_member`. This is not cross-role conflation: ABC and the adapter
compute the same byte-hash independently, and equality is an adapter-integrity
check that proves the adapter hashed the bytes ABC handed it. Moving decoding
or transcoding before that hash point requires a new parser-input construction
and is not compatible with v1.

Publication derivation identity and the current coarse reuse marker key on
`work_content_hash = bundle_hash`. Therefore an image-only change deliberately
rotates `artifact_id` and rebuilds v1 publication even though current TEI output
does not package image bytes and may be byte-identical. This is conservative
over-invalidation: derivation identity records the complete admitted source,
while `content.content_hash` still reveals identical output. A later
dependency-minimal parser/publication cache may key on `primary_text_hash` plus
parser, mapping, TEI, and configuration coordinates, but that optimization is
outside this change. P16 models image edits as rebuilds until such a cache has
its own contract.

Source snapshots pin both logical content and provenance:

- `work_content_hash` / `bundle_hash` identifies logical source contents;
- `archive_hash` identifies the exact upstream artifact; and
- `primary_text_hash` identifies the parser input.

They validate their own construction and references, not equality with one
another.

Source artifact manifests keep derivation and output bytes separate under ADR
0001: `manifest_identity_object.work_content_hash` is `bundle_hash`, while
`content.content_hash` is the byte hash of the persisted `source-bundle.json`
artifact. The content media type/path describe that JSON file; they do not call
the logical bundle hash a byte hash of `source.txt`.

## Schema Shape and Compatibility

This change rotates schemas that currently overload `source_hash`:

- Add `schemas/source-bundle.schema.json` for `source-bundle.json`.
- Extend parser-IR source identity with `primary_text_hash`; retain
  `work_content_hash` as the bundle coordinate.
- Extend `official-source.json` production and workset/snapshot values with
  `archive_hash`, `bundle_hash`, `primary_text_member`, and
  `primary_text_hash`.
- During one compatibility generation, aliases are scoped per artifact; no
  global `source_hash` alias exists:
  - `official-source.json.source_hash` aliases `archive_hash`; a mismatch
    between those two fields in the same object fails.
  - AAT `meta.source_hash` aliases `primary_text_hash`; a mismatch between
    those two fields in the same object fails.
- Existing historical parser-IR fixtures retain their original interpretation;
  they are not reinterpreted as bundle hashes.

`work_content_hash` remains in `manifest_identity_object`, so no new manifest
identity coordinate is required. Its defining equality relation changes only
for artifacts produced under the new schema generation. Old manifests retain
their schema hashes and historical meaning.

## D7 Transition

During implementation, D7 remained open until the complete new path was
exercised. The implementation was not permitted to make the realistic stub
write the ZIP hash merely to close the gate.

The gate was permitted to flip only when a real or contract-equivalent run
demonstrated:

1. ZIP inspection emits the expected member manifest and hashes.
2. Parser-IR records the same bundle hash supplied by ABC and independently
   records the text-member hash produced by the adapter.
3. `build-publication` → workset → `materialize-source-snapshot!` succeeds.
4. Changing an image member rotates `bundle_hash` and rebuilds publication
   while leaving `primary_text_hash` unchanged.
5. Repacking identical members rotates only `archive_hash` and permits logical
   publication reuse.

After those conditions passed, D7 became `:fixed`, P16.3 became an ordinary
regression guard, and the hard mismatch assertions were replaced by
role-specific checks:
`official-source` archive hash equals the actual ZIP hash; bundle hash equals a
fresh recomputation of the canonical member manifest; parser-IR carries the
ABC-supplied bundle hash; and the independently returned primary-text hash
equals `member_hash(primary_text_member)`.

## Failure Semantics

Fail before invoking the parser when the archive is unreadable, a path is
unsafe, normalized paths collide, no primary text exists, or a member cannot be
hashed. Errors carry the archive path and a stable reason code; member-specific
failures also carry the original and normalized member path when available.

Admission errors occur inside the existing per-work derive boundary and obey an
explicit two-posture contract:

- With `continue_on_failure=false` (the strict/release posture), any admission
  error aborts the atomic whole build. The temporary output is not promoted.
- With `continue_on_failure=true` (measurement/best-effort posture), the work is
  omitted from successful selection, recorded in `derive_failures` with its
  stable admission reason, and counted in the selection report. Processing
  continues for other works.

A run with nonzero `derive_failed_count` is never release-admissible, even when
best-effort execution returns normally. Release validation fails it; the flag
controls operational continuation, not whether rejection is acceptable. P16
covers both dispositions for an admission-rejected work.

Never fall back from bundle identity to text-only or raw-archive identity. A
missing required hash produces no successful source manifest or publication.

### Damaged archives

The v1 inspector does not retain today's unbounded `7zz x` extract-all
fallback. That fallback materializes the archive outside the inspector's member
and total-byte limits and uses filesystem enumeration for primary selection;
keeping it would defeat both resource safety and deterministic identity.
Java-unreadable archives fail admission under the two-posture contract above.
Recovery requires repairing/repackaging the upstream archive into a conforming
ZIP whose logical members can be inspected under the same bounds. A future
bounded recovery construction requires its own design and construction version;
v1 does not shell out and then claim the result used the normal construction.

## Migration Sequence

1. Add bundle inspection, canonicalization fixtures, schema, and pure contract
   tests without changing the active pipeline. Measure corpus member counts,
   entry-name flags, text-candidate counts, and decompression bounds first.
2. Extend the adapter invocation and parser-IR schema so bundle and primary-text
   identities coexist.
3. Switch build selection, source manifests, reuse markers, and worksets to
   `bundle_hash`; retain raw archive provenance.
4. Add image-change and metadata-only-repack evolution cases.
5. Change snapshot validation from cross-role equality to role-specific
   validation and flip D7 only after those cases and the composition case pass.
6. After one schema generation, remove both scoped `source_hash` writer aliases;
   keep historical reader support for each artifact's historical meaning.

Each step must keep existing historical fixtures readable. No migration
rewrites old manifests in place.

## Testing

- A checked-in Clojure known-answer artifact pins canonical UTF-8 JSON bytes
  containing both a slash and non-ASCII paths plus the literal `bundle_hash`.
  A deferred Rust fixture must reproduce it before Rust authors bundle identity.
- Deterministic contract cases vary ZIP compression, timestamps, comments, and
  entry order while holding members fixed; `bundle_hash` remains stable.
- Deterministic contract cases add, remove, rename, or alter members and prove
  `bundle_hash` changes. The P16 simulation adds generative image/content
  histories over those identity rules and deterministic image-edit,
  text-edit, and metadata-only-repack witnesses.
- Tests cover text plus images, packaging-metadata text members, rejected
  multiple semantic text members, empty directories, unsafe paths, duplicate
  normalized paths, and portability collisions.
- Admission-failure tests pin strict atomic abort, best-effort
  `derive_failures`/counting, and release rejection of any nonzero failure
  count.
- A pinned damaged-archive case proves Java-unreadable input does not reach the
  unbounded `7zz` fallback, including the corpus archive whose Commons Compress
  open failure is a plain `IOException` rather than `ZipException`.
- Persisted identity policy tests reject unsafe/non-NFC paths, noncanonical
  order, normalized/full-fold collisions, invalid hashes, bad primary
  cardinality/matching, and coordinated member reversal plus re-signing.
- Adapter tests prove `primary_text_hash` equals exact stdin/member bytes.
- Composition tests prove all three hash roles survive build, workset, snapshot,
  and publication manifests without equality conflation.
- Existing P16 selection, reuse, stale-marker, and fault properties stay green;
  D7 flips only at the composition step.

## Runtime and Resource Bounds

Bundle inspection streams each member into SHA-256 and does not materialize all
members simultaneously. The selected primary text may be retained for the
adapter; other members are bounded-buffer streams. The inspector enforces
configured archive limits for member count, per-member uncompressed bytes, and
total uncompressed bytes to prevent decompression bombs. Concrete limits must
be chosen from measured Aozora corpus maxima plus documented headroom before
the inspector is admitted to production.

Before inspection, the caller-visible ZIP path is copied once into a private,
read-only temporary artifact. `archive_hash`, central-directory parsing, and
all member hashes are then derived from that same staged file, which is deleted
on success or failure. This closes replacement races that could otherwise pair
member identity from one inode with `archive_hash` from later path contents.
Only plain archive-parser `IOException` at ZIP open/enumeration is classified
as `unreadable-zip`; interruption, missing/access/file-system errors, Errors,
linkage failures, and member-read or other later operational IO propagate.

### Pinned-corpus evidence

Measured against flake input `aozorabunko-src` commit
`0e9ea3e586eb0aa34039fabfc85a407d2f98b165` on 2026-07-12:

- 17,877 readable card ZIPs contain exactly one `.txt` member;
- two contain one semantic text plus an AppleDouble
  `__MACOSX/._<name>.txt` member;
- five are asset-only bundles (images, TTZ, or PDF);
- three `.zip`-named files are structurally unreadable by the standard ZIP
  reader: two are not recoverable by `7zz`, while one 26 MiB archive is
  `7zz`-recoverable but has trailing data and becomes an intentional v1
  admission failure pending upstream repair;
- all 22,860 readable file entries have the ZIP UTF-8 flag unset;
- no readable bundle has an NFC-normalized-path collision or an ICU full-
  Unicode-case-fold collision;
- the maximum readable bundle contains 778 file members, the maximum single
  uncompressed member is 12,631,833 bytes, and the maximum total uncompressed
  bundle size is 27,874,310 bytes.

The checked corpus report uses the same staged, bounded body scan as production
admission. Actual streamed maxima remain 12,631,833 bytes per member and
27,874,310 bytes per bundle. It also pins one understated central-directory
declaration: `cards/001393/files/50710_ruby_36965.zip` declares 68,007 bytes for
`fushigino_kunino_alice_musical.txt`, while the bounded stream reads 68,497.

The primary-selector packaging-metadata exclusion is therefore required for
real corpus admission, and windows-31j fallback is the measured normal case,
not an exceptional compatibility path. The implementation records this
measurement as a reproducible checked corpus report before production
admission.

Strict windows-31j fallback is intentional: a flag-unset name with neither a
valid Unicode Path extra field nor valid windows-31j bytes fails admission
rather than being decoded with a host CP437 default or replacement characters.
Correctly flagged future UTF-8 names and transitional ZIPs carrying `0x7075`
therefore enter the same NFC identity path. ICU full folding covers non-ASCII
Latin case variants as well as ASCII; it is a deterministic collision screen,
not a claim to emulate every APFS/NTFS filename rule.

## Non-Goals

- Extracting image files into publication output or implementing IIIF.
- Defining parser-cache storage.
- Semantic normalization of text or images.
- Treating media-type detection as identity-bearing.
- Replacing SHA-256 or RFC 8785 JCS.
- Rewriting historical manifests.

## Acceptance Criteria

- Every hash field has one documented equality relation and no production
  validation requires hashes of different roles to equal.
- The bundle manifest covers every non-directory member and rejects unsafe or
  ambiguous paths.
- The Clojure producer pins canonical bytes and `bundle_hash`; no non-JVM
  producer is admitted until it reproduces the fixture cross-language.
- Image-only changes rotate `work_content_hash`; metadata-only repacks do not.
- Exact archive provenance remains queryable by `archive_hash`.
- Parser input remains queryable by `primary_text_hash`.
- Publication reuse and snapshot composition use the bundle identity.
- P16.3 passes without an expected-failure gate and D7 is recorded fixed.
- Historical manifests remain readable without reinterpretation.

## Implementation Appendix

Accepted on 2026-07-12 with the following implementation evidence:

- `abc.tools.source-bundle` implements the bounded, deterministic v1 ZIP
  inspector over one private read-only staged artifact, so archive and member
  identities observe the same bytes; `source-bundle.schema.json` and
  `fixtures/source-bundle/abc-source-bundle-v1-known-answer.json` pin manifest,
  path-decoding, Unicode-folding, limit, and RFC 8785 canonical bytes/hash
  behavior. The known answer includes both a slash and non-ASCII paths.
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
