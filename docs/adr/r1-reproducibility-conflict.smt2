; R1 — Reproducibility-conflict detection (ADR 0001).
; Counterexample query: a release stays valid despite two successful manifests
; sharing artifact_id and diverging on content_hash. Expected: unsat.
;
; Acceptance criterion (ADR 0001): "Given two successful manifests with
; identical manifest_identity_object and different content.content_hash,
; release validation fails with a reproducibility-conflict report."

(set-logic ALL)

(declare-datatypes ((Status 0)) (((success) (failure))))

(declare-const m1_status Status)
(declare-const m1_content_hash String)
(declare-const m1_artifact_id String)

(declare-const m2_status Status)
(declare-const m2_content_hash String)
(declare-const m2_artifact_id String)

(declare-const release_valid Bool)

; Both manifests are successful.
(assert (! (and (= m1_status success) (= m2_status success))
           :named r1_both_success))
; The two manifests share the same identity (artifact_id).
(assert (! (= m1_artifact_id m2_artifact_id)
           :named r1_same_identity))
; Their materialized content differs.
(assert (! (not (= m1_content_hash m2_content_hash))
           :named r1_content_differs))
; ADR 0001 acceptance: under those three conditions, release_valid MUST be false.
; BEGIN-INVARIANTS
;   The assertion below is the invariant P under test. The adr-invariants-vacuity
;   Nix check strips this block and requires Z3 to report `sat` for the
;   remaining (axioms + counterexample) — i.e. the counterexample must be
;   reachable once P is removed. See
;   docs/handoffs/formal-verification-assessment-critique.md §6.
(assert (! (= release_valid
              (not (and (= m1_artifact_id m2_artifact_id)
                        (= m1_status success)
                        (= m2_status success)
                        (not (= m1_content_hash m2_content_hash)))))
           :named r1_conflict_implies_invalid))
; END-INVARIANTS
; Counterexample we seek: release stays valid despite the conflict.
(assert (! release_valid
           :named r1_counterexample_release_valid))

(check-sat)
