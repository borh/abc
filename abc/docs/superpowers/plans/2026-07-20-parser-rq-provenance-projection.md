# Parser-RQ Provenance Projection Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make unbound and bound executable-provenance records authenticate as one candidate value, reject malformed bound provenance before capture, and re-evaluate the immutable P5 candidate for ADR 0039 promotion.

**Architecture:** One pure Clojure projection removes the four binding-envelope fields before hashing. One bound-record verifier owns schema, closed membership, reproducibility structure, core authentication, candidate bindings, and candidate provenance identity; authorization and promotion both call it. The completed P5 capture is never rerun or edited: after the verifier correction, promotion is derived again from the same committed values.

**Tech Stack:** Clojure 1.12, `clojure.test`/Kaocha, Python 3 provenance CLI, JCS SHA-256 content references, Nix flakes, ADR evidence capture and registration.

## Global Constraints

- Work directly on `main`; the other reported terminal is in `/home/bor/Projects/zkjd-system` and must not be interrupted.
- Do not push intermediate commits. Push only after the final promotion/governance state passes every gate.
- Do not modify or rerun candidate `sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab`, its authorization, capture, evaluation, provenance bytes, admission row, or canonical measurements/report.
- `candidate-provenance-value` removes exactly `schema_id`, `schema_version`, `candidate_ref`, and `qualification_identity_ref`; it retains `provenance_core_ref`.
- `candidate-provenance-value` is a no-op on an unbound proof.
- `verify-provenance-errors` accepts only a reproducible bound v2 record with exactly eight keys.
- `build-candidate` continues to validate unbound proofs with `provenance-errors`.
- Authorization and promotion both call `verify-provenance-errors`; neither reimplements a subset of it.
- Preserve the existing independent `verify-readiness-receipt` core and binding checks as defense in depth.
- ADR 0039 changes only after `verify-promotion` exits zero over the committed P5 values.
- Use `apply_patch` for hand edits and preserve unrelated worktree changes.

---

### Task 1: Define the Candidate Provenance Projection and Bound Contract

**Files:**
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj:14-85,190-220,260-330`
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj:40-125,598-647`

**Interfaces:**
- Consumes: unbound proof maps produced by `compare_builds` and bound v2 maps produced by Python `bind-provenance`.
- Produces: `candidate-provenance-value [provenance] -> map`, `executable-provenance-ref [provenance] -> sha256 string`, and bound-only `verify-provenance-errors [candidate provenance] -> vector<string>`.

- [ ] **Step 1: Replace the synthetic provenance fixture with a valid unbound proof and bound record**

In `abc/test/abc/tools/parser_rq_campaign_test.clj`, replace the current `candidate` and `provenance` definitions with this construction. Keep `qualification-identity`, `with-ref`, `graph`, `receipt`, and `authorization` in their existing order around it.

```clojure
(def provenance-proof
  (with-ref
    {:status :reproducible
     :builds [{:build_id "build-a"
               :store_uri "local?root=/tmp/build-a"
               :output_ref sha
               :build_record_ref sha}
              {:build_id "build-b"
               :store_uri "local?root=/tmp/build-b"
               :output_ref sha
               :build_record_ref sha-b}]
     :executables
     [{:name "ab-aozora"
       :nix_output "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-parser-rq"
       :nar_hash sha
       :sha256 sha
       :bytes 1
       :adapter "ab-aozora"
       :adapter_version "candidate"
       :parser_git_rev (:parser_git_rev qualification-identity)
       :argv_template ["{executable}" "{source}"]}]}
    :provenance_core_ref campaign/provenance-core-ref))

(def candidate
  (with-ref {:schema_id "https://w3id.org/abc/schemas/parser-rq-candidate.schema.json"
             :schema_version "1.0.0"
             :qualification_identity_ref
             (qualification/qualification-identity-ref qualification-identity)
             :qualification_identity qualification-identity
             :executable_provenance_ref
             (campaign/executable-provenance-ref provenance-proof)}
    :candidate_ref campaign/candidate-ref))

(def provenance
  (assoc provenance-proof
         :schema_id
         "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json"
         :schema_version "2.0.0"
         :candidate_ref (:candidate_ref candidate)
         :qualification_identity_ref (:qualification_identity_ref candidate)))
```

- [ ] **Step 2: Add failing projection, envelope, and cross-language tests**

After `content-references-ignore-only-their-self-field`, add:

```clojure
(deftest candidate-provenance-projection-unifies-proof-and-bound-record
  (is (= provenance-proof
         (campaign/candidate-provenance-value provenance-proof)))
  (is (= (:executable_provenance_ref candidate)
         (campaign/executable-provenance-ref provenance)))
  (is (not= (:executable_provenance_ref candidate)
            (campaign/executable-provenance-ref
             (assoc-in provenance [:executables 0 :bytes] 2)))))

(deftest bound-provenance-envelope-and-core-are-authenticated
  (is (= [] (campaign/verify-provenance-errors candidate provenance)))
  (doseq [changed [(assoc provenance :schema_version "changed")
                   (assoc provenance :schema_id "https://example.invalid/provenance")
                   (assoc provenance :extra true)]]
    (is (some #(re-find #"bound provenance envelope" %)
              (campaign/verify-provenance-errors candidate changed))))
  (is (some #(re-find #"provenance core" %)
            (campaign/verify-provenance-errors
             candidate (assoc-in provenance [:executables 0 :bytes] 2)))))

(deftest python-bound-provenance-authenticates-as-the-unbound-proof
  (let [root (fs/create-temp-dir {:prefix "parser-rq-bound-provenance"})
        proof-path (fs/file root "proof.json")
        bound-path (fs/file root "bound.json")
        script (provenance-script)]
    (write-canonical-json! proof-path provenance-proof)
    (let [{:keys [exit err]}
          (shell/sh "python3" (str script) "bind-provenance"
                    "--proof" (str proof-path)
                    "--candidate-ref" (:candidate_ref candidate)
                    "--qualification-identity-ref"
                    (:qualification_identity_ref candidate)
                    "--out" (str bound-path))
          bound (walk/keywordize-keys (files/read-json bound-path))]
      (is (= 0 exit) err)
      (is (= (:executable_provenance_ref candidate)
             (campaign/executable-provenance-ref bound)))
      (is (= [] (campaign/verify-provenance-errors candidate bound))))))
```

Move the existing `(declare write-edn! write-canonical-json!)`, `workspace-root`,
and `provenance-script` definitions together above these tests. Do not duplicate
any helper.

- [ ] **Step 3: Run the new tests and verify RED**

Run from `abc/`:

```bash
bin/kaocha --focus abc.tools.parser-rq-campaign-test/candidate-provenance-projection-unifies-proof-and-bound-record \
  --focus abc.tools.parser-rq-campaign-test/bound-provenance-envelope-and-core-are-authenticated \
  --focus abc.tools.parser-rq-campaign-test/python-bound-provenance-authenticates-as-the-unbound-proof
```

Expected: FAIL because `candidate-provenance-value` is unresolved and the old hash includes bound schema fields.

- [ ] **Step 4: Implement the pure projection and bound verifier**

In `abc/src/abc/tools/parser_rq_campaign.clj`, add the constants beside the other closed key contracts:

```clojure
(def executable-provenance-schema-id
  "https://w3id.org/abc/schemas/parser-rq-executable-provenance.schema.json")

(def executable-provenance-envelope-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref})

(def bound-executable-provenance-keys
  #{:schema_id :schema_version :candidate_ref :qualification_identity_ref
    :provenance_core_ref :status :builds :executables})
```

Replace `executable-provenance-ref` with:

```clojure
(defn candidate-provenance-value [provenance]
  (apply dissoc provenance executable-provenance-envelope-keys))

(defn executable-provenance-ref [provenance]
  (-> provenance
      candidate-provenance-value
      canonical-value
      hash/sha256-json-jcs
      hash/format-sha256))
```

Replace `verify-provenance-errors` with:

```clojure
(defn verify-provenance-errors [candidate provenance]
  (cond-> (vec (provenance-errors provenance))
    (or (not= bound-executable-provenance-keys (set (keys provenance)))
        (not= executable-provenance-schema-id (:schema_id provenance))
        (not= "2.0.0" (:schema_version provenance)))
    (conj "bound provenance envelope is invalid")

    (not= (:provenance_core_ref provenance)
          (provenance-core-ref provenance))
    (conj "bound provenance core does not authenticate its evidence")

    (not= (:executable_provenance_ref candidate)
          (executable-provenance-ref provenance))
    (conj "executable provenance does not authenticate the candidate")

    (not= (:candidate_ref candidate) (:candidate_ref provenance))
    (conj "executable provenance candidate_ref does not match")

    (not= (:qualification_identity_ref candidate)
          (:qualification_identity_ref provenance))
    (conj "executable provenance qualification identity does not match")))
```

Keep `build-candidate` unchanged: it must still call `provenance-errors` on the unbound proof.

- [ ] **Step 5: Run focused and full campaign tests to verify GREEN**

Run from `abc/`:

```bash
bin/kaocha --focus abc.tools.parser-rq-campaign-test
nix build ../ab-validator#checks.x86_64-linux.parser-rq-campaign-provenance-python-tests
```

Expected: all Clojure campaign tests pass; the Python provenance check builds successfully.

- [ ] **Step 6: Commit the projection contract**

```bash
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj
git commit -m "fix(parser-rq): normalize bound provenance identity"
```

---

### Task 2: Make Authorization and Promotion Share the Complete Verifier

**Files:**
- Modify: `abc/test/abc/tools/parser_rq_campaign_test.clj:200-220,580-625`
- Modify: `abc/src/abc/tools/parser_rq_campaign.clj:123,324-332,704-750,917-927`

**Interfaces:**
- Consumes: `verify-provenance-errors [candidate bound-provenance]` from Task 1.
- Produces: fail-closed pre-capture authorization and promotion paths with the same provenance errors.

- [ ] **Step 1: Add failing authorization and promotion parity tests**

After `authorization-is-one-shot-candidate-bound-and-time-bounded`, add:

```clojure
(deftest authorization-rejects-candidate-provenance-reference-mismatch
  (let [errors (campaign/verify-authorization-record
                (assoc candidate :executable_provenance_ref sha-b)
                provenance graph receipt authorization)]
    (is (some #{"executable provenance does not authenticate the candidate"}
              errors))))
```

In `promotion-resolves-one-committed-generation-and-current-evaluation`, immediately after the initial assertion that `promotion-errors` is empty, add:

```clojure
    (write-canonical-json! provenance-path
                           (assoc provenance :schema_version "changed"))
    (is (some #{"bound provenance envelope is invalid"}
              (campaign/promotion-errors options)))
    (write-canonical-json! provenance-path provenance)
```

- [ ] **Step 2: Run both tests and verify RED**

```bash
cd abc
bin/kaocha \
  --focus abc.tools.parser-rq-campaign-test/authorization-rejects-candidate-provenance-reference-mismatch \
  --focus abc.tools.parser-rq-campaign-test/promotion-resolves-one-committed-generation-and-current-evaluation
```

Expected: the authorization test fails because authorization does not call `verify-provenance-errors`; the promotion test fails because promotion still owns only an inline hash comparison.

- [ ] **Step 3: Wire authorization through the bound verifier**

Extend the existing forward declaration near `build-candidate`:

```clojure
(declare provenance-errors verify-provenance-errors)
```

Change `verify-authorization-record` to:

```clojure
(defn verify-authorization-record
  [candidate provenance graph receipt authorization]
  (vec
   (concat
    (authorization-record-errors candidate authorization)
    (verify-provenance-errors candidate provenance)
    (verify-readiness-receipt candidate provenance graph receipt)
    (when (not= (:readiness_receipt_ref receipt)
                (:readiness_receipt_ref authorization))
      ["authorization readiness receipt does not match the sealed receipt"]))))
```

- [ ] **Step 4: Wire promotion through the same verifier and delete the duplicate subset**

In `promotion-errors`, initialize `binding-errors` from the complete verifier and retain only the two path/candidate self-reference checks:

```clojure
          binding-errors
          (cond-> (vec (verify-provenance-errors candidate provenance))
            (not= candidate_ref (:candidate_ref candidate))
            (conj "candidate path does not match its authenticated candidate_ref")

            (not= candidate_ref (candidate-ref candidate))
            (conj "candidate_ref does not authenticate the committed candidate"))
```

Delete the `provenance-errors` clauses from `promotion-value-errors`:

```clojure
      (seq (provenance-errors provenance))
      (into (provenance-errors provenance))
```

Remove `provenance` from `promotion-value-errors`'s destructured keys and remove
`:provenance provenance` from the `derived` map. No remaining clause consumes
it. Do not retain an unused second provenance-validation path.

- [ ] **Step 5: Run the focused tests and verify GREEN**

```bash
cd abc
bin/kaocha --focus abc.tools.parser-rq-campaign-test
```

Expected: all campaign tests pass, including authorization and promotion parity.

- [ ] **Step 6: Run neighboring integrity checks**

From the monorepo root:

```bash
nix build ./abc#checks.x86_64-linux.parser-rq-admission-promotion-smoke
nix build .#checks.x86_64-linux.parser-rq-production-wiring
nix build ./abc#checks.x86_64-linux.clj-kondo
scripts/comment-hygiene-check.sh
git diff --check
```

Expected: every command exits zero.

- [ ] **Step 7: Commit the shared release boundary**

```bash
git add abc/src/abc/tools/parser_rq_campaign.clj \
  abc/test/abc/tools/parser_rq_campaign_test.clj
git commit -m "fix(parser-rq): authenticate provenance before capture"
```

---

### Task 3: Re-evaluate P5 and Conditionally Promote ADR 0039

**Files:**
- Create: `abc/docs/evidence/adr-entries/parser-rq-release-qualification.edn`
- Modify: `abc/docs/adr/0039-custom-parser-release-qualification.md`
- Modify: `abc/docs/evidence/adr-runs/parser-rq-instrument-bindings.json`
- Modify: `abc/docs/adr/adr-evidence.edn`
- Modify: generated files beneath `abc/docs/evidence/adr-runs/`

**Interfaces:**
- Consumes: the corrected pure verifier, immutable P5 candidate/capture/evaluations, current registry, Accepted ADR 0040, and Accepted ADR 0041.
- Produces: either zero-error promotion plus Accepted ADR 0039, or an unchanged Proposed ADR with the exact remaining verifier errors.

- [ ] **Step 1: Run the corrected verifier over the immutable P5 campaign**

From `abc/`:

```bash
candidate_ref=sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab
run_root="docs/reports/parser-rq/runs/${candidate_ref#sha256:}"
nix develop . --command clojure -M:abc/parser-rq-campaign \
  verify-promotion \
  --runs-root docs/reports/parser-rq/runs \
  --candidate-ref "$candidate_ref" \
  --registry data/aat-parser-ir-compatibility.edn \
  --measurements docs/reports/parser-release-qualification-measurements.edn \
  --report docs/reports/parser-release-qualification-report.json \
  --provenance "$run_root/executable-provenance.json" \
  --adr-0040 docs/adr/0040-process-tree-memory-qualification.md \
  --adr-0041 docs/adr/0041-parser-release-instrument-bindings.md
```

Expected: prints `ok` and exits zero. If it reports any error, stop Task 3, leave ADR 0039 Proposed, and report the exact errors; do not alter evidence or weaken the verifier.

- [ ] **Step 2: Add accepted-ADR claim registrations**

Create `abc/docs/evidence/adr-entries/parser-rq-release-qualification.edn`:

```clojure
{:schema-version :abc-adr-evidence-registration-v1
 :entries
 [{:claim-id "ADR-0039-C1"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/parser-rq-instrument-bindings.json"
   :observation-id :parser-rq-instrument-bindings
   :observation-key "parser-rq-instrument-bindings-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0039-C2"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/parser-rq-instrument-bindings.json"
   :observation-id :parser-rq-instrument-bindings
   :observation-key "parser-rq-instrument-bindings-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0039-C3"
   :claim-kind :fixture-behavior
   :evidence-kind :fixture-conformance
   :artifact-path "docs/evidence/adr-runs/parser-rq-instrument-bindings.json"
   :observation-id :parser-rq-instrument-bindings
   :observation-key "parser-rq-instrument-bindings-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0039-C4"
   :claim-kind :structural-invariant
   :evidence-kind :structural-test
   :artifact-path "docs/evidence/adr-runs/parser-rq-instrument-bindings.json"
   :observation-id :parser-rq-instrument-bindings
   :observation-key "parser-rq-instrument-bindings-pass"
   :expected {:operator := :value true}}
  {:claim-id "ADR-0039-C5"
   :claim-kind :operational-behavior
   :evidence-kind :operational-observation
   :artifact-path "docs/evidence/adr-runs/parser-rq-instrument-bindings.json"
   :observation-id :parser-rq-instrument-bindings
   :observation-key "parser-rq-instrument-bindings-pass"
   :expected {:operator := :value true}}]}
```

Commit this registration before capture so the evidence tool runs from a clean tree:

```bash
git add abc/docs/evidence/adr-entries/parser-rq-release-qualification.edn
git commit -m "docs(adr): register parser release qualification evidence"
```

- [ ] **Step 3: Recapture the focused parser-RQ evidence bundle from the clean tree**

From `abc/`:

```bash
stage=$(mktemp -d -t parser-rq-0039-evidence.XXXXXXXX)
mkdir -p "$stage/capture"
nix develop . --command clojure -M:abc/adr-evidence-capture \
  --descriptor docs/evidence/parser-rq-instrument-bindings/capture/parser-rq-instrument-bindings.edn \
  --output "$stage/capture/parser-rq-instrument-bindings.json" \
  --staging-root "$stage/capture" \
  --repo-root . \
  --workspace-root ..
jq -e '.observations["parser-rq-instrument-bindings-pass"].value == true' \
  "$stage/capture/parser-rq-instrument-bindings.json"
cp "$stage/capture/parser-rq-instrument-bindings.json" \
  docs/evidence/adr-runs/parser-rq-instrument-bindings.json
```

Expected: capture exits zero and `jq` prints `true`.

- [ ] **Step 4: Apply the ordinary reviewed ADR 0039 promotion edit**

In `abc/docs/adr/0039-custom-parser-release-qualification.md`, set the header to:

```text
Status: Accepted
Date: 2026-07-20
Accepted: 2026-07-20
Validation scope: smoke-corpus
Release authority: publication
Depends on: ADR 0002, ADR 0023, ADR 0030, ADR 0038, ADR 0040, ADR 0041
```

Replace `## Implementation Status` content with:

```markdown
Accepted. Candidate
`sha256:15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab`
has one authorized immutable capture under qualification identity
`sha256:6f365a44b975465943da88d0e3fe4f123672e00913285a3e998ab465bc79edca`.
Its nine predicate verdicts pass, coherence is `ok`, and the exact tuple is
admitted by a fresh 17,886-work conversion audit with zero failed files. The
promotion verifier authenticates the reproducible executable provenance,
capture membership and bytes, current-registry evaluation, canonical
projections, and Accepted ADR 0040/0041 dependencies.
```

Replace the obsolete `### Admission resolution for the release candidate (ADR 0023)` section with:

```markdown
### Admission resolution for the release candidate (ADR 0023)

The candidate's exact nine-field admission query is present in
`data/aat-parser-ir-compatibility.edn`. The immutable admission report records a
fresh full-corpus conversion audit of 17,886 works: 17,886 succeeded and zero
failed. Full-entry conflict checking and nine-field membership both resolve to
`admitted`; no historical registry row was rewritten.
```

Replace `## Consequences` content with:

```markdown
The custom parser is qualified for publication for the exact committed
candidate, qualification identity, predicate set, corpus, and admitted tuple.
This authority does not float with branch HEAD: any executable, parser revision,
schema, mapping, corpus, predicate, or instrument change creates a different
candidate or qualification identity and requires new evidence. The immutable
P5 capture remains the authority for this decision; comparison and neutral
third-party-parser evidence remain non-release evidence.
```

Replace `## Future Verification` with `## Evidence` and this content:

```markdown
The canonical gate projection is
`docs/reports/parser-release-qualification-report.json`. The immutable campaign
root is
`docs/reports/parser-rq/runs/15affdfb677cc6a94a4a5364da68ca2d11441f899737651e727dbac90eddc5ab/`.
The registered bounded contract run is
`docs/evidence/adr-runs/parser-rq-instrument-bindings.json`; it covers predicate
identity, exact evaluation, release-evidence classification, authorization,
capture/evaluation resolution, and promotion failure semantics.
```

Do not alter the Decision or Acceptance Criteria.

- [ ] **Step 5: Commit the focused evidence and ADR acceptance**

```bash
git diff --check
git add abc/docs/adr/0039-custom-parser-release-qualification.md \
  abc/docs/evidence/adr-runs/parser-rq-instrument-bindings.json
git commit -m "docs(adr): accept custom parser release qualification"
```

- [ ] **Step 6: Recapture the complete governance closure from the clean accepted tree**

Run from `abc/`:

```bash
stage=$(mktemp -d -t parser-rq-0039-governance.XXXXXXXX)
mkdir -p "$stage/runs"
git_common_dir=$(git rev-parse --path-format=absolute --git-common-dir)
grep -qxF 'abc/abc' "$git_common_dir/info/exclude" || \
  echo 'abc/abc' >> "$git_common_dir/info/exclude"
test ! -e abc
ln -s . abc
cleanup() { rm -f abc; }
trap cleanup EXIT

{
  find docs/evidence/adr-capture -maxdepth 1 -type f -name '*.edn'
  find docs/evidence -mindepth 3 -type f -path '*/capture/*.edn'
} | sort -u > "$stage/descriptors.txt"

while IFS= read -r descriptor; do
  name=$(basename "$descriptor" .edn)
  nix develop . --command clojure -M:abc/adr-evidence-capture \
    --descriptor "$descriptor" \
    --output "$stage/runs/$name.json" \
    --staging-root "$stage/runs" \
    --repo-root . \
    --workspace-root ..
done < "$stage/descriptors.txt"

jq -s -e 'all(.[]; all(.observations[]; .value == true))' \
  "$stage"/runs/*.json
cp "$stage"/runs/*.json docs/evidence/adr-runs/

nix develop . --command clojure -M -e '
(require (quote [abc.tools.files :as files])
         (quote [babashka.fs :as fs]))
(let [paths (sort (map str (fs/glob "docs/evidence/adr-entries" "*.edn")))
      templates (map files/read-edn paths)
      entries (vec (mapcat :entries templates))]
  (spit "docs/evidence/adr-entries/.all-generated.edn"
        (str (pr-str {:schema-version :abc-adr-evidence-registration-v1
                      :entries entries}) "\n")))'

nix develop . --command clojure -M:abc/adr-evidence-register \
  --entries docs/evidence/adr-entries/.all-generated.edn \
  --registry docs/adr/adr-evidence.edn \
  --workspace-root ..
rm docs/evidence/adr-entries/.all-generated.edn
cleanup
trap - EXIT
```

Expected: every descriptor exits zero, the aggregate `jq` expression prints `true`, and the registrar includes ADR-0039-C1 through C5 exactly once each.

- [ ] **Step 7: Commit the governance recapture**

```bash
rg -o 'ADR-0039-C[1-5]' docs/adr/adr-evidence.edn | sort | uniq -c
git diff --check
git add docs/adr/adr-evidence.edn docs/evidence/adr-runs
git commit -m "docs(adr): recapture parser release governance"
```

Expected: each ADR-0039 claim count is `1`.

- [ ] **Step 8: Run final verification**

From the monorepo root:

```bash
just check-no-build
just phase5-checkpoint
just monorepo-adr-governance
just python-quality
just nix-format-check
scripts/comment-hygiene-check.sh
nix build ./abc#checks.x86_64-linux.clj-kondo
nix build ./abc#checks.x86_64-linux.clj-nix-focused-tests
nix build ./ab-validator#checks.x86_64-linux.cargo-check
nix build ./ab-validator#checks.x86_64-linux.cargo-clippy
nix build ./ab-validator#checks.x86_64-linux.cargo-fmt
just validate-migration
git diff --check
test -z "$(git status --short)"
```

Re-run the exact `verify-promotion` command from Step 1. Expected: prints `ok`. Also require:

```bash
jq -e '.gate_status == "release-qualified"
       and .adr_0039_status == "Accepted"
       and .coherence.status == "ok"
       and .admission.status == "admitted"
       and .verdict_tally == {"pass": 9}' \
  abc/docs/reports/parser-release-qualification-report.json
grep -qx 'Status: Accepted' <(grep '^Status:' abc/docs/adr/0039-custom-parser-release-qualification.md)
```

Expected: `jq` prints `true`; every command exits zero.

- [ ] **Step 9: Push the verified history**

```bash
git push origin main
test "$(git rev-parse HEAD)" = "$(git rev-parse origin/main)"
git status --short --branch
```

Expected: local `main` equals `origin/main` and the worktree is clean.

## Self-Review Checklist

- [ ] Spec coverage: the four-field projection, unbound no-op, retained core, bound eight-key contract, core recomputation, authorization wiring, promotion parity, immutable-candidate re-evaluation, and conditional ADR promotion each have an owning task.
- [ ] Identity consistency: Python `bind-provenance` remains unchanged; both languages converge on the same unbound proof value.
- [ ] Failure consistency: a pre-capture mismatch blocks authorization; a post-capture mismatch blocks promotion; neither path edits evidence.
- [ ] Governance consistency: ADR 0039 remains Proposed unless the corrected verifier returns zero, and accepted claims are registered before the full governance recapture.
- [ ] No placeholders: every edit, command, expected failure, expected success, and commit boundary is explicit.
