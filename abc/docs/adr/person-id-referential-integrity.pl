% Referential integrity: every person_id appearing in a drift_successor/2
% fact (as either predecessor or successor) must resolve to a committed
% person_record/1. The emitter (Task 5) resolves successors from prov.used /
% was_generated_by through participants[].snapshot_id. A dangling person_id
% means a drift event references a person not present in the corpus (neither
% a top-level record nor a drift-index entry).
%
% Person_ids are single-quoted atoms ('000879'); SWI compares them as
% atoms. Bare 000879 would parse as integer 879 and the comparison would
% silently fail to unify — the emitter's prolog-atom quoting guards this.

:- use_module(library(lists)).

% A person_id mentioned by drift_successor/2 in either argument but with no
% committed person_record/1 fact.
dangling_person(Pid) :-
    ( drift_successor(Pid, _)
    ; drift_successor(_, Pid)
    ),
    \+ person_record(Pid).

% Run: swipl -q -t "halt(\+ dangling_person(_))" -c person_records.pl -c drift.pl -c person-id-referential-integrity.pl
% Exit 0 (halt(true)) = no dangling person; exit 1 (halt(false)) = violation.
