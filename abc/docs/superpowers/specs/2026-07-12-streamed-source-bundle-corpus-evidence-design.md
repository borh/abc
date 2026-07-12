# Streamed Source-Bundle Corpus Evidence Design

**Date:** 2026-07-12
**Status:** Implemented

**Builds on:**

- `docs/adr/0033-source-bundle-identity.md`
- `docs/superpowers/specs/2026-07-12-source-bundle-identity-design.md`

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

The production streaming implementation already exists inside
`source-bundle/inspect-open-zip`: `read-member!` reads each body through a
`DigestInputStream`, hashes it, and enforces actual-byte limits after the
declared-size pre-check. This change is an extract-and-reuse refactor, not a new
ZIP streamer. The current corpus report instead combines `raw-zip-stats`
(declared sizes from a separate archive open) with `inspect-zip-metadata`
(decoding, normalization, collision, and candidate evidence without body
reads).

## Decision

Separate source-bundle inspection into two cohesive operations:

1. A bounded scan derives facts from one private staged archive.
2. Admission validates those facts and constructs `abc-source-bundle-v1`.

Both production inspection and corpus evidence consume the same scan result.
The corpus report therefore measures actual streamed bytes, not declared ZIP
metadata, while production retains fail-loud admission semantics.

As part of the migration, delete `source-bundle/inspect-zip-metadata` and
`source-bundle-report/raw-zip-stats` after their callers move to the shared
scan. No corpus-only filename, collision, candidate, or size construction
remains.

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
   filename-source classification, and the raw ZIP EFS bit;
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

One pure path-collision analysis computes exact NFC duplicates and ICU
full-fold collisions. Scan evidence, admission error translation, and
persisted-identity validation all call that function; they do not maintain
parallel collision predicates. Admission translates its result to marked
source-bundle reasons, while persisted-identity validation translates the same
result to identity-policy reasons.

`inspect-zip` stages, scans, then admits. Its public successful value and stable
failure reasons remain unchanged.

Collision and primary-cardinality validation moves after bounded member
streaming. This ordering is intentional: the evidence path must measure every
structurally readable archive, including asset-only and otherwise
admission-rejected bundles. Production still remains bounded and fails before
any publication or manifest is authored.

This changes rejection-path cost. A collision or wrong-cardinality bundle that
currently fails after O(member-name) work will instead stream and hash its
bounded bodies before rejection: at most 1,024 members, 16 MiB per member, and
32 MiB total under v1 defaults. The extra work is accepted because these cases
are rare, rejection-only, and the shared scan is what makes rejected-bundle
evidence trustworthy. Success-path streaming cost is unchanged.

Failure precedence changes for a bundle carrying more than one defect. A
declared/actual limit failure or member-read failure now surfaces before a
collision or wrong primary cardinality, because admission begins only after a
complete bounded scan. This is accepted and pinned: resource and operational
failures take precedence over later logical-bundle admission.

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
listability. `7zz` output never supplies members, hashes, or admission.

`7zz l -slt` proves only that 7-Zip can list archive metadata. It does not prove
that member bodies can be streamed or that a repaired archive would pass v1
admission. The new report calls this disposition `7zz-listable`, not
recoverable; it remains informational.

The checked JSON gains:

```json
{
  "measurement_construction": "abc-source-bundle-streamed-evidence-v1",
  "admitted_zip_count": 0,
  "rejected_zip_count": 0,
  "rejection_reason_counts": {},
  "java_unreadable_7zz_listable_count": 0,
  "java_unreadable_7zz_unlistable_count": 0
}
```

The versioned report replaces the misleading historical
`java_unreadable_7zz_recoverable_count` names with `7zz_listable` names.

Existing readability, encoding, collision, damaged-path, and maximum fields
remain for continuity, but the maximum fields now mean actual streamed bytes.
`readable_zip_count` continues to mean structurally readable, not admitted;
asset-only bundles are readable and rejected with
`no-primary-text-member`.

`utf8_flagged_entry_count` continues to count the raw ZIP general-purpose EFS
bit 11, independently of the decoder's `name_source` label. Under v1
precedence an EFS-set entry decodes through strict UTF-8; Unicode Path evidence
is exercised by a separate EFS-unset case. The scan retains the raw bit
explicitly rather than deriving either count from decoder selection.
`legacy_flagged_entry_count` remains the complement over non-directory entries.

The report also pins every declared-versus-actual mismatch rather than only
the maxima:

```json
{
  "declared_actual_size_mismatch_member_count": 1,
  "declared_actual_size_mismatches": [
    {
      "archive_path": "cards/001393/files/50710_ruby_36965.zip",
      "member_path": "fushigino_kunino_alice_musical.txt",
      "declared_bytes": 68007,
      "actual_bytes": 68497
    }
  ]
}
```

An independent full-corpus stream established the migration expectation for
pin `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`: the maximum member remains
12,631,833 bytes and the maximum bundle total remains 27,874,310 bytes, but one
non-maximum member has the understated declaration above. Those maxima must
not be silently re-blessed if implementation produces different values; any
other mismatch or maximum change is a migration finding to investigate.

The report is deterministic: paths and reason maps are sorted, and no staged
temporary path enters evidence.

The checked gate now performs roughly 17,884 private staging copies plus full
member streams instead of header-only size reads. This is an intentional CI
runtime and I/O increase. The Nix-store corpus is immutable, but production
uses staging to bind archive and member identity to one artifact; the gate
keeps that same path rather than introducing a cheaper evidence-only branch.
Implementation records the old and new gate wall times in the plan report. No
hard runtime threshold is set until that first reproducible measurement exists.

## Limits and Future Pin Updates

The checked corpus gate uses the exact production limits. If a future corpus
pin exceeds a limit, the gate fails closed at the first bounded violation; it
does not silently raise limits or switch to declared sizes. A limit change
requires a separate reviewed corpus-measurement run with an explicitly larger
safety ceiling, documented headroom, and a production-policy update.

Limit failures are not ordinary rejected-corpus evidence and cannot be
re-blessed into the checked JSON. `too-many-members`, `member-too-large`, and
`total-too-large` propagate from the evidence collector and abort report
generation. Consequently every successfully written report satisfies
`readable_zip_count + unreadable_zip_count = admitted_zip_count +
rejected_zip_count = total visited ZIPs`.

This change does not add a compressed-archive staging limit. That remains a
separate resource-policy follow-up because it requires a measured compressed
corpus maximum and a new admission limit.

## Error Handling

- Marked logical-bundle admission failures produced by `admit-scan!` become
  deterministic rejected-corpus evidence.
- A marked failure before a complete scan aborts evidence generation, because
  no actual-byte record exists to count. The sole exception is
  `unreadable-zip`, which is recorded as structurally unreadable and may receive
  informational `7zz` listability classification.
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
- EFS/legacy counts remain raw-bit counts even when decoder precedence selects
  a Unicode Path extra field; and
- production limit failures abort without writing re-blessable evidence; and
- one shared collision analysis drives scan evidence, admission, and persisted
  identity validation; and
- the one pinned declared/actual mismatch and unchanged actual maxima are
  reproduced exactly; and
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

## Implementation Evidence

Implemented on 2026-07-12. `abc.tools.source-bundle/scan-zip` exposes the
existing private-stage, bounded member stream, and `admit-scan!` applies the v1
logical admission policy to its completed facts. Production `inspect-zip` and
`abc.tools.source-bundle-report` now compose those same operations.
`inspect-zip-metadata` and the report-local `raw-zip-stats` were deleted, so no
second filename, collision, candidate, or size construction remains.

The checked
`data/source-bundle/aozorabunko-0e9ea3e-summary.json` records actual streamed
maxima, admission counts, stable rejection reasons, and the exact 68,007 versus
68,497 byte declaration mismatch. Focused `source-bundle-test` and
`source-bundle-report-test` coverage pins scan/admission composition,
byte-array content equality, raw EFS-bit accounting, resource-failure
precedence, shared collision analysis, and protected error propagation. The
`source-bundle-corpus` Nix check reproduces the report byte-for-byte.

Using Bash `TIMEFORMAT` on the implementation workstation because
`/usr/bin/time` was unavailable, the pre-change Nix corpus gate took 64.840
seconds, the direct streamed report took 69.465 seconds, and a fresh streamed
Nix corpus gate took 73.253 seconds. The pre-change timing was a fresh build in
the sense that its output was not already built, though it did not use
`--rebuild`; these values are implementation evidence, not a permanent CI
threshold.

## Acceptance Criteria

- Production inspection and corpus evidence consume one shared bounded member
  scan.
- `inspect-zip-metadata` and `raw-zip-stats` have no remaining definitions or
  callers.
- Checked maxima are computed from actual streamed bytes.
- The checked report pins the known understated member declaration and rejects
  any unreviewed mismatch or maximum change.
- EFS counts retain their raw-bit meaning independently of filename decoder
  selection.
- Any production limit violation aborts report generation.
- Scan, admission, and persisted identities use one collision-analysis
  implementation with domain-specific error translation.
- Every structurally readable pinned ZIP is streamed under production limits.
- The report distinguishes structural readability from bundle admission.
- Asset-only, collision, and damaged-archive dispositions are stable and
  counted; bounded-limit violations and every other incomplete scan abort the
  gate instead of becoming re-blessable evidence.
- Protected non-admission failures abort the gate unchanged.
- `source-bundle-corpus` reproduces the versioned checked report exactly.
- Existing source-bundle, snapshot, publication, simulation, governance, and
  migration gates remain green.
