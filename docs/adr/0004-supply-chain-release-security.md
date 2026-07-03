# ADR 0004: Supply-Chain Release Security

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5

## Implementation Status

Still Draft. Manifest schemas reserve release-security surface such as
signatures, but the release verification workflow, provenance attestation
format, and public-release trust policy are not implemented.

## Context

Content addressing detects accidental drift but does not prove that a release
manifest came from a trusted publisher. ABC also depends on external corpus
snapshots, parser sources, tokenizer dictionaries, schemas, and packaging
tools. Publication releases need a trust story before users rely on them as
research artifacts.

## Decision

Local v0 development artifacts are unsigned. Public releases must define a
release security profile before publication.

The profile should evaluate:

- SLSA v1.2 Build L2 as the first public-release target. SLSA v1.2 Build L3
  should be evaluated, but is not required for v0 publication.
- in-toto attestations for release provenance.
- Sigstore/Cosign bundles for keyless signing.
- Detached signature files for canonical JSON manifests.
- Key rotation and revocation policy if long-lived project keys are used.

## Threat Model

- Accidental drift: detected by hashes, canonicalization fixtures, and schema
  validation.
- Malicious manifest substitution: mitigated by signatures and trusted
  publisher policy.
- Dependency compromise: made reviewable through pinned source locks, archive
  identifiers, and attestations.
- Reproducibility regression: detected by smoke corpus, failure manifests, and
  benchmark metrics.

## Acceptance Criteria

- Release manifest schema has a non-identity `signatures` field.
- Release process records source snapshot, schema hashes, parser build hashes,
  tokenizer dictionary hashes, and archive identifiers where available.
- Release manifests record license metadata with SPDX identifiers where
  available.
- Signature format, key distribution, key rotation, and revocation policy are
  documented before the first public release.
- Consumers have a documented verification path for release manifests.
- No public release is allowed until `docs/release-verification.md` can verify
  manifest schema hash, artifact content hash, signature or Sigstore bundle,
  provenance attestation subject hashes, source/archive IDs, and license
  metadata presence.

## Rollback

If Sigstore/Cosign or in-toto adoption is too heavy for the first release, use
detached signatures over release manifests and keep the manifest schema fields
compatible with future attestations.
