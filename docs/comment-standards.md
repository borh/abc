# Comments and documentation

Code is the central truth; prose is a maintenance liability. Repository prose
explains the current system without requiring project history. Plans, handoffs,
review discussions, and temporary decision records belong in the issue tracker.
Current contracts, operating guides, useful research evidence, and permanent
architectural decisions belong beside the code they explain.

Source comments exist only to explain the non-obvious "why" — constraints,
subtle edge cases, domain rules, or performance invariants that clear code
cannot express on its own. Omit comments that narrate the "what", repeat the
next expression, echo a symbol name, divide files with decorative banners,
narrate an edit, or promise future work. If a detail is obvious to experienced
programmers or domain experts, do not comment it. Document public behavior at
the API boundary and non-obvious implementation rationale beside the relevant
operation.

Use descriptive invariant names, including those in [the parser invariants](parser-invariants.md).
Do not reference issues, review findings, numbered project tasks or deleted plans.
References to code symbols, permanent architecture documents and external
specifications such as Unicode, JIS and TEI are useful when they identify the
actual contract. Algorithm steps and protocol section numbers are not project
coordination labels.

Keep historical evidence clearly scoped to its recorded inputs and observations;
do not present it as the current implementation. Update live document links when
moving or removing their targets. Generated documentation must be changed through
its source and regenerated with the existing toolchain.

Run `scripts/comment-hygiene-check.sh` to catch coordination references. The check
is a guard against recognizable markers; reviewers must also check accuracy,
necessity and audience throughout prose that contains no such markers.
