# Streamed Source-Bundle Corpus Evidence Design

**Date:** 2026-07-12
**Status:** Proposed

## Problem

The checked source-bundle corpus report currently measures member and total
uncompressed sizes from ZIP central-directory declarations. Production
admission separately streams member bodies and enforces limits against the
actual bytes read. The pinned Aozora corpus corroborates the current maxima,
but the checked report itself would not detect an understated declared size or
a body-read failure.

The evidence gate should exercise the same bounded streaming construction as
production. It must not duplicate filename decoding, path normalization,
collision policy, limit enforcement, or archive-error classification in a
second corpus-only implementation.

## Decision

Separate source-bundle inspection into two cohesive operations:

1. A bounded scan derives facts from one private staged archive.
2. Admission validates those facts and constructs `abc-source-bundle-v1`.

Both production inspection and corpus evidence consume the same scan result.
The corpus report therefore measures actual streamed bytes, not declared ZIP
metadata, while production retains fail-loud admission semantics.

## Shared Bounded Scan

The scanner consumes a caller-visible archive path, a private staged archive,
and the production limits. It:

1. opens the staged archive through the existing strict decoder precedence;
2. decodes and NFC-normalizes every non-directory member path;
3. enforces the declared-size early-rejection checks;
4. sorts members by normalized identity path;
5. streams every member once through SHA-256;
6. enforces per-member and total limits against actual bytes read;
7. records exact `byte_length`, `member_hash`, decoded path, normalized path,
   and filename-source classification;
8. retains primary-text bytes only when exactly one semantic, non-packaging
   `.txt` candidate exists; and
9. returns collision evidence and the semantic-text candidate list without
   deciding whether the bundle is admitted.

All facts, including `archive_hash`, come from the same private staged file.
The staged file is deleted on success or failure.

The scanner is not a permissive recovery API. Unsafe paths, malformed filename
encoding, resource-limit violations, archive-parser failures, and later
operational I/O retain their existing production taxonomy. It does not invoke
`7zz`.

## Admission

Admission consumes only the bounded scan result. It:

- rejects duplicate NFC paths and ICU full-fold collisions;
- requires exactly one semantic, non-packaging `.txt` member;
- constructs and validates the sorted `abc-source-bundle-v1` identity object;
- verifies the retained primary bytes and primary member hash agree; and
- returns `bundle_hash`, `archive_hash`, members, primary identity, and primary
  bytes.

`inspect-zip` stages, scans, then admits. Its public successful value and stable
failure reasons remain unchanged.

Collision and primary-cardinality validation moves after bounded member
streaming. This ordering is intentional: the evidence path must measure every
structurally readable archive, including asset-only and otherwise
admission-rejected bundles. Production still remains bounded and fails before
any publication or manifest is authored.

## Corpus Evidence

The report walks only `cards/*/files/*.zip`, as today. For each archive it uses
the shared staged scan and admission policy.

For structurally readable archives it records:

- actual streamed member count;
- maximum actual member bytes;
- actual total uncompressed bytes;
- UTF-8-flagged and legacy-name entry counts;
- semantic-text candidate count;
- NFC and ICU full-fold collision evidence; and
- admission disposition and stable reason.

For archives that cannot be structurally opened, the report records the marked
production reason and may invoke `7zz l -slt` only to classify historical
recoverability. `7zz` output never supplies members, hashes, or admission.

The checked JSON gains:

```json
{
  "measurement_construction": "abc-source-bundle-streamed-evidence-v1",
  "admitted_zip_count": 0,
  "rejected_zip_count": 0,
  "rejection_reason_counts": {}
}
```

Existing readability, encoding, collision, damaged-path, and maximum fields
remain for continuity, but the maximum fields now mean actual streamed bytes.
`readable_zip_count` continues to mean structurally readable, not admitted;
asset-only bundles are readable and rejected with
`no-primary-text-member`.

The report is deterministic: paths and reason maps are sorted, and no staged
temporary path enters evidence.

## Limits and Future Pin Updates

The checked corpus gate uses the exact production limits. If a future corpus
pin exceeds a limit, the gate fails closed at the first bounded violation; it
does not silently raise limits or switch to declared sizes. A limit change
requires a separate reviewed corpus-measurement run with an explicitly larger
safety ceiling, documented headroom, and a production-policy update.

This change does not add a compressed-archive staging limit. That remains a
separate resource-policy follow-up because it requires a measured compressed
corpus maximum and a new admission limit.

## Error Handling

- Marked source-bundle admission failures become deterministic rejected-corpus
  evidence.
- Archive-parser failures remain structurally unreadable and retain the
  authoritative caller-visible archive path.
- Interruption, filesystem access failures, linkage errors, programming
  failures, and later operational I/O propagate and fail the corpus gate.
- A corpus report with any unexpected exception is not written successfully.

The report captures only exceptions carrying the internal source-bundle
admission marker. A matching reason keyword without that marker is not trusted.

## Testing

Focused tests must first fail against the declared-size implementation and then
prove:

- the shared scan reports actual bytes and hashes for every member;
- `inspect-zip` is a thin stage/scan/admit composition;
- asset-only and collision bundles are fully streamed before their stable
  rejection disposition is recorded;
- the scan and production admission share filename decoding, normalization,
  limits, and error taxonomy;
- protected failures escape the evidence collector unchanged;
- damaged archives remain classification-only and never reach `7zz` for
  identity;
- report construction/version and reason counts are deterministic; and
- the full pinned-corpus Nix check reproduces the checked JSON byte-for-byte.

The known RFC 8785 source-bundle fixture, snapshot validation, P16 composition,
and existing admission-disposition tests remain unchanged and green.

## Non-Goals

- Raising current production limits.
- Adding a compressed-archive byte limit.
- Repairing or recovering damaged ZIPs.
- Restoring an extraction fallback.
- Changing `abc-source-bundle-v1` identity bytes.
- Changing publication, parser, or reuse behavior.
- Rewriting historical corpus reports for older pins.

## Acceptance Criteria

- Production inspection and corpus evidence consume one shared bounded member
  scan.
- Checked maxima are computed from actual streamed bytes.
- Every structurally readable pinned ZIP is streamed under production limits.
- The report distinguishes structural readability from bundle admission.
- Asset-only, collision, limit, and damaged-archive dispositions are stable and
  counted.
- Protected non-admission failures abort the gate unchanged.
- `source-bundle-corpus` reproduces the versioned checked report exactly.
- Existing source-bundle, snapshot, publication, simulation, governance, and
  migration gates remain green.
