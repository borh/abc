person_record('000879').
person_record('abc-000000000001').
person_record('abc-000000000002').

% drift_successor/2 emitted from prov.used / was_generated_by
drift_successor('000879', 'abc-000000000001').
drift_successor('000879', 'abc-000000000002').