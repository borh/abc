#!/usr/bin/env bash
set -euo pipefail

# Enforces docs/comment-standards.md: no transient task/plan/spec/issue/phase
# references in source comments across ab-validator/, abc/src/, and
# soranoha/src/. Exits non-zero and prints offending lines if any criterion
# fails.
#
# Scope excludes vendored/build trees (target/, .cargo/, third_party/) via the
# search roots, and soranoha's ported tree (byte fidelity to the abc originals
# is intentional there). String-literal program output (report headers,
# test-assertion messages) is out of scope — see the C10 exclusions below.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

roots=(ab-validator/ abc/src/ soranoha/src/)
rgopts=(-g '!soranoha/src/soranoha/ported/**')
srcglob='src:*.{rs,clj,cljc,cljs}'
# Six issue numbers are retained named invariants (docs/glossary.md).
keep='#(78|228|331|333|384|435)\b'

fail=0
report() { # name, matches
  if [[ -n "$2" ]]; then
    printf '✗ %s:\n%s\n\n' "$1" "$2" >&2
    fail=1
  fi
}

report "C1 handoff refs" \
  "$(rg -n --type-add "$srcglob" -t src 'docs/handoffs/' "${rgopts[@]}" "${roots[@]}" || true)"
report "C2 task/plan labels" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(task\s*[0-9]+|plan\s+[a-g]\.?[0-9]|plan\s+amendment|plan\s+blocker)\b' "${rgopts[@]}" "${roots[@]}" | grep -v 'ortho-detect-ml/src/main.rs' || true)"
report "C3 dated spec/report refs" \
  "$(rg -n --type-add "$srcglob" -t src 'docs/superpowers/(specs|reports)/' "${rgopts[@]}" "${roots[@]}" || true)"
report "C4 issue #NNN (non-invariant)" \
  "$(rg -nP --type-add "$srcglob" -t src '(?<![A-Za-z0-9_!\[+&])#[0-9]{2,4}\b' "${rgopts[@]}" "${roots[@]}" | grep -vE "$keep" || true)"
report "C5 Issue 2 / Spec Decision" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(issue\s+2|spec\s+decision\s*#?[0-9]+)\b' "${rgopts[@]}" "${roots[@]}" || true)"
report "C6 TODO" \
  "$(rg -n --type-add "$srcglob" -t src '\bTODO\b' "${rgopts[@]}" "${roots[@]}" || true)"
report "C7 speculative language" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(revisit|re-evaluat|eventually export|a later task|someday)\b' "${rgopts[@]}" "${roots[@]}" || true)"
report "C8 UNSTABLE markers" \
  "$(rg -n --type-add "$srcglob" -t src 'UNSTABLE.*(semver|v0\.[0-9]|not subject to)' "${rgopts[@]}" "${roots[@]}" || true)"
report "C9 transient Tier refs" \
  "$(rg -n --type-add "$srcglob" -t src '\bTier[\s-][B-H1-9]\b' "${rgopts[@]}" "${roots[@]}" || true)"

# C10 transient Phase labels. Case-sensitive Phase[- ]<alnum>, minus the
# retained algorithm-stage markers and out-of-scope string literals:
#   - "// --- Phase N: ... ---" markers (any file)
#   - sentences.rs (algorithm stages, incl. the Phase-2 back-reference)
#   - the single kept "// Phase 0: resolve forward heading hints" marker
#   - report headers / test-assertion strings (main.rs, integration.rs)
c10="$(rg -nP --type-add "$srcglob" -t src '\bPhase[- ][0-9A-H]' "${rgopts[@]}" "${roots[@]}" \
  | rg -v '// --- Phase [0-9]:' \
  | rg -v 'sentences\.rs:' \
  | rg -v '// Phase 0: resolve forward heading hints' \
  | rg -v 'ortho-detect-ml/src/main\.rs:' \
  | rg -v 'tests/integration\.rs:.*"the frozen Phase 5 generation' || true)"
report "C10 transient Phase labels" "$c10"

# C12 design-ledger finding/decision tags (F##/D##) in soranoha source.
# Scoped to soranoha only: abc and ab-validator legitimately use F1 scores,
# Unicode D800/F900 literals, and named D1–D6 degradation rules.
report "C12 design-ledger F/D refs (soranoha)" \
  "$(rg -nP --type-add "$srcglob" -t src '(?<![A-Za-z0-9_./-])[FD][0-9]{1,3}(\.[0-9])?\b' \
    "${rgopts[@]}" soranoha/src/ || true)"

# C11 the six named invariants must still be present.
for n in 78 228 331 333 384 435; do
  c="$(rg -cP --type-add "$srcglob" -t src "(?<![A-Za-z0-9_!\[+&])#${n}\b" "${rgopts[@]}" "${roots[@]}" | awk -F: '{s+=$2} END{print s+0}')"
  if [[ "${c:-0}" -eq 0 ]]; then
    printf '✗ C11 invariant #%s missing from codebase\n' "$n" >&2
    fail=1
  fi
done

if [[ "$fail" -eq 0 ]]; then
  echo "comment-hygiene: all criteria pass"
fi
exit "$fail"
