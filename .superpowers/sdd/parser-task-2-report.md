# Parser migration Task 2 report

## Scope

Added direct test assertions for the ABC imported-parser boundary, exact mapping
admission authority, parser tuple identity rotation, and source-role relations.
No production validation code, citation transition lookup, or B10 runtime input
closure was changed.

## TDD evidence

RED command:

```sh
cd abc
bin/kaocha --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test
```

Observed RED: exit 5; 6 tests and 24 assertions, with 2 errors and 3 failures.
The failures demonstrated that the initial assertions did not yet correctly use
the run-summary event-set registry, validator return contract, deterministic
diagnostic error ordering, or exact admission conflict seam.

Minimal GREEN changes remained test-only:

- validated the committed parser IR and each diagnostic/run-summary row with
  the checked-in JSON Schemas;
- validated complete and incomplete event sets with the real Malli contract;
- redirected the imported fixture path to a temporary tree and installed a
  command sentinel while running `validate-ab-validator-output!`;
- meta-validated all three ADR 0023 mapping contract schemas;
- proved historical citable selection evidence cannot admit missing or
  conflicting mapping coordinates, and used `admission-report` for exact-entry
  evidence conflict/admission;
- varied parser build, adapter, and mapping coordinates while proving artifact
  identity rotates and source bundle/member roles remain stable and distinct.

GREEN command:

```sh
cd abc
bin/kaocha --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test \
  --focus abc.tools.materialize-import-test
```

Observed GREEN: exit 0; 22 tests, 69 assertions, 0 failures.

## Additional verification

- `git diff --check`: pass.
- `scripts/comment-hygiene-check.sh`: pass.
- `nix build --no-link ./abc#checks.$(nix eval --impure --raw --expr builtins.currentSystem).clj-kondo`:
  pass.

## Self-review

- Changes are restricted to the four requested task files for the commit.
- Assertions use committed fixtures and real schema/admission/materialization
  functions; no production behavior is introduced.
- The B10 runtime closure is unchanged.
- The only modification to the existing validation test namespace makes its
  established compatibility assertion function externally addressable for
  later focused evidence binding.

## Concerns

None.

## Review fixes

Addressed all three Task 2 review findings without production changes:

- the temporary comparison-report now supplies a real executable-valued parser
  candidate, while guards on `babashka.process/process`,
  `babashka.process/shell`, and the design-bundle command wrapper prove the
  imported-output validator performs no process execution;
- source relation hashes now come from `source-bundle/inspect-zip`, and two
  imported outputs are materialized through `materialize-import!`; their
  persisted parser IR and manifests prove stable, distinct source roles while
  parser/adapter/mapping tuple variation rotates manifest and artifact identity;
- the citable historical selection now flows through a release-report boundary
  that validates the citation index and obtains admission errors exclusively
  from `compatibility-errors`; missing and conflicting tuples remain blocked,
  while the exact registry candidate remains admitted by `admission-report`.

The original arrangements were first inspected against their public callees:
the sentinel was never present in imported data or configuration, the source
hashes were locally associated and immediately reread, and the historical
citation did not participate in either admission scenario. Strengthening those
arrangements initially produced RED while the real source seam was being wired
(`No such var: fs/output-stream`); switching to the public Java IO stream made
the source-bundle inspection executable. The substantive guards would also
throw `ABC crossed parser execution boundary` on any command invocation, and
the new identity assertions compare persisted materializer results rather than
locally constructed identities.

Covering command:

```sh
cd abc
bin/kaocha --focus abc.tools.parser-import-boundary-evidence-test \
  --focus abc.tools.parser-mapping-admission-evidence-test \
  --focus abc.tools.parser-identity-relations-evidence-test \
  --focus abc.tools.materialize-import-test
```

Observed: exit 0; 22 tests, 73 assertions, 0 failures.

Additional verification:

```sh
clj-paren-repair abc/test/abc/tools/parser_import_boundary_evidence_test.clj \
  abc/test/abc/tools/parser_identity_relations_evidence_test.clj \
  abc/test/abc/tools/parser_mapping_admission_evidence_test.clj
git diff --check
scripts/comment-hygiene-check.sh
nix build --no-link \
  ./abc#checks.$(nix eval --impure --raw --expr builtins.currentSystem).clj-kondo
```

Observed: all three Clojure files parsed/formatted; diff check passed; comment
hygiene reported `all criteria pass`; the clj-kondo derivation built
successfully.
