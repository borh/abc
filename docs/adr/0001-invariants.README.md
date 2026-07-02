; ADR 0001 — Manifest Identity: solver-checked invariants.
;
; Source: docs/adr/0001-manifest-identity.md (Accepted 2026-04-28)
; Gate:   `nix run .#adr0001-invariants` (see flake.nix checks.adr0001-invariants)
;
; This directory holds two SMT-LIB files, each a COUNTEREXAMPLE-SEEKING query
; that Z3 must report `unsat` for. The check runs `z3` offline (no MCP, no
; network) so it works in the Nix sandbox and CI.
;
;   r1-reproducibility-conflict.smt2
;     Two successful manifests share an artifact_id but differ on content_hash.
;     ADR 0001 requires release validation to fail in that case. Seeking a
;     counterexample where the release stays valid → UNSAT means no
;     counterexample exists; the invariant holds in the model.
;
;   r2-non-circularity.smt2
;     artifact_id is computed as sha256(JCS(manifest_identity_object)), so it
;     must not equal any of the 12 identity fields inside that object. Seeking
;     a counterexample where some field equals the artifact_id while `distinct`
;     holds → UNSAT means the structural invariant holds.
;
; r3-null-dimension is NOT here. JCS canonicalization is outside Z3's string
; theory; that invariant is covered by fixtures/canonicalization/ and the
; validate-design-bundle test gate, not a solver.
;
; Each .smt2 file ends with an explicit (check-sat) command — the offline
; `z3 file.smt2` CLI used by the Nix check does NOT add it automatically
; (unlike the chiasmus MCP runner, which does). Named assertions
; (! :named <label>) make any future UNSAT core surface the conflicting rule
; by name when invoked with `(get-unsat-core)`.
