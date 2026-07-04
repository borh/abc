; R2 — Non-circularity of artifact_id, structural (ADR 0001).
; Counterexample query: some identity field equals the artifact_id value while
; `distinct` over artifact_id and all 12 fields holds. Expected: unsat.
;
; ADR 0001: "artifact_id is never nested inside manifest_identity_object;
; including it there would make identity circular."
;
; manifest_identity_object fields (12, per ADR 0001):
;   manifest_schema_hash, corpus_snapshot_hash, work_content_hash,
;   metadata_record_hash, parser_build_hash, parser_config_hash,
;   parser_ir_schema_hash, tei_profile_hash, tokenizer_build_hash,
;   tokenizer_dictionary_hash, analysis_recipe_hash, output_format_spec_hash.

(set-logic ALL)

(declare-const artifact_id_value String)

(declare-const f_schema        String)
(declare-const f_corpus        String)
(declare-const f_work          String)
(declare-const f_metadata      String)
(declare-const f_parser_build  String)
(declare-const f_parser_cfg    String)
(declare-const f_parser_ir     String)
(declare-const f_tei_profile   String)
(declare-const f_tok_build     String)
(declare-const f_tok_dict      String)
(declare-const f_analysis      String)
(declare-const f_output_spec   String)

; artifact_id is a real, non-empty hash.
(assert (! (not (= artifact_id_value ""))
            :named r2_artifact_id_nonempty))
; R2 invariant: artifact_id_value is distinct from every identity field.
; BEGIN-INVARIANTS
;   The assertion below is the invariant P under test. The adr-invariants-vacuity
;   Nix check strips this block and requires Z3 to report `sat` for the
;   remaining (axioms + counterexample) — i.e. the counterexample must be
;   reachable once P is removed. See
;   docs/handoffs/formal-verification-assessment-critique.md §6.
(assert (! (distinct artifact_id_value
                     f_schema f_corpus f_work f_metadata
                     f_parser_build f_parser_cfg f_parser_ir f_tei_profile
                     f_tok_build f_tok_dict f_analysis f_output_spec)
           :named r2_artifact_id_distinct_from_fields))
; END-INVARIANTS
; Counterexample we seek: at least one identity field equals artifact_id_value
; anyway (i.e. the invariant can be violated while distinct holds).
(assert (! (or (= f_schema       artifact_id_value)
               (= f_corpus       artifact_id_value)
               (= f_work         artifact_id_value)
               (= f_metadata     artifact_id_value)
               (= f_parser_build artifact_id_value)
               (= f_parser_cfg   artifact_id_value)
               (= f_parser_ir    artifact_id_value)
               (= f_tei_profile  artifact_id_value)
               (= f_tok_build    artifact_id_value)
               (= f_tok_dict     artifact_id_value)
               (= f_analysis     artifact_id_value)
               (= f_output_spec  artifact_id_value))
           :named r2_counterexample_field_equals_artifact_id))

(check-sat)
