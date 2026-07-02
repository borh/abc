; ADR 0020 — Person Identity Drift Event cardinality invariants.
; Counterexample-seeking query: seek a valid split or merge event that VIOLATES
; the cardinality rules. Expected: unsat (no counterexample exists).
;
; ADR 0020 (`docs/adr/0020-person-identity-drift-data-model.md`) lines 148-149:
;   - A split has one predecessor snapshot and two or more successor snapshots.
;   - A merge has two or more predecessor snapshots and one successor snapshot.
;
; Gate: `nix run .#checks.x86_64-linux.adr0020-drift-cardinality` (offline z3).
; See `docs/adr/0001-invariants.README.md` for the rationale and runner notes.

(set-logic ALL)

(declare-datatypes ((EventType 0)) (((split) (merge))))

(declare-const event_type EventType)
(declare-const pred_count Int)
(declare-const succ_count Int)

(assert (! (>= pred_count 0) :named d1_pred_nonneg))
(assert (! (>= succ_count 0) :named d2_succ_nonneg))

; --- ADR 0020 cardinality rules (the invariant) ---
(assert (! (=> (= event_type split)
               (and (= pred_count 1) (>= succ_count 2)))
           :named dr_split_cardinality))
(assert (! (=> (= event_type merge)
               (and (>= pred_count 2) (= succ_count 1)))
           :named dr_merge_cardinality))

; --- Counterexample we seek: a valid event whose counts violate its rule. ---
(assert (! (or
  (and (= event_type split) (or (not (= pred_count 1)) (< succ_count 2)))
  (and (= event_type merge) (or (< pred_count 2) (not (= succ_count 1)))))
           :named dr_counterexample))

(check-sat)
