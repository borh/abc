% Emitter sanity: no duplicate manifest_identity/2 facts for the same manifest
% path. This is NOT a proof of ADR 0020 Position L — see the Task 6 header in
% docs/superpowers/plans/2026-07-04-code-as-spec-formal-models.md. It guards
% that the emitter didn't write two different identity hashes for one manifest,
% which would make downstream Prolog comparisons meaningless.
%
% Design §7.3: identity facts are EMITTED by the Clojure emitter using the real
% manifest/artifact-id. Prolog only compares emitted facts; it never recomputes
% identity (which would risk hand-translating the hash function).
%
% ADR 0020 Position L ("drift events must not rotate manifest_identity_object")
% is enforced structurally today: drift events live in separate _events/*.json
% files that the manifest schema never references as identity inputs, so a drift
% event cannot rotate identity by construction. A real before/after Prolog
% proof of Position L would require the emitter to model a hypothetical
% post-drift identity (defining what identity *would be* if drift entered the
% identity object), which ADR 0020 forbids — that is future work, not claimed
% by this query.

:- use_module(library(lists)).

% A violation: two different hashes claimed for the same manifest path.
violating_dup_identity(Manifest) :-
    manifest_identity(Manifest, H1),
    manifest_identity(Manifest, H2),
    H1 \= H2.

% Run: swipl -q -t "halt(\+ violating_dup_identity(_))" -c manifest_identity.pl -c manifest-identity-emitter-sanity.pl
% Exit 0 (halt(true)) = no violation; exit 1 (halt(false)) = violation found.
