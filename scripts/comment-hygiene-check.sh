#!/usr/bin/env bash
set -euo pipefail

# Enforces docs/comment-standards.md: no transient task/plan/spec/issue/phase
# references in source comments across ab-validator/ and abc/src/.
# Exits non-zero and prints offending lines if any criterion fails.
#
# Scope excludes vendored/build trees (target/, .cargo/, third_party/) via the
# search roots. String-literal program output (report headers, test-assertion
# messages) is out of scope — see the C10 exclusions below.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

roots=(ab-validator/ abc/src/)
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
  "$(rg -n --type-add "$srcglob" -t src 'docs/handoffs/' "${roots[@]}" || true)"
report "C2 task/plan labels" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(task\s*[0-9]+|plan\s+[a-g]\.?[0-9]|plan\s+amendment|plan\s+blocker)\b' "${roots[@]}" | grep -v 'ortho-detect-ml/src/main.rs' || true)"
report "C3 dated spec/report refs" \
  "$(rg -n --type-add "$srcglob" -t src 'docs/superpowers/(specs|reports)/' "${roots[@]}" || true)"
report "C4 issue #NNN (non-invariant)" \
  "$(rg -nP --type-add "$srcglob" -t src '(?<![A-Za-z0-9_!\[+&])#[0-9]{2,4}\b' "${roots[@]}" | grep -vE "$keep" || true)"
report "C5 Issue 2 / Spec Decision" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(issue\s+2|spec\s+decision\s*#?[0-9]+)\b' "${roots[@]}" || true)"
report "C6 TODO" \
  "$(rg -n --type-add "$srcglob" -t src '\bTODO\b' "${roots[@]}" || true)"
report "C7 speculative language" \
  "$(rg -n --type-add "$srcglob" -t src -i '\b(revisit|re-evaluat|eventually export|a later task|someday)\b' "${roots[@]}" || true)"
report "C8 UNSTABLE markers" \
  "$(rg -n --type-add "$srcglob" -t src 'UNSTABLE.*(semver|v0\.[0-9]|not subject to)' "${roots[@]}" || true)"
report "C9 transient Tier refs" \
  "$(rg -n --type-add "$srcglob" -t src '\bTier[\s-][B-H1-9]\b' "${roots[@]}" || true)"

# C10 transient Phase labels. Case-sensitive Phase[- ]<alnum>, minus the
# retained algorithm-stage markers and out-of-scope string literals:
#   - "// --- Phase N: ... ---" markers (any file)
#   - sentences.rs (algorithm stages, incl. the Phase-2 back-reference)
#   - the single kept "// Phase 0: resolve forward heading hints" marker
#   - report headers / test-assertion strings (main.rs, integration.rs)
c10="$(rg -nP --type-add "$srcglob" -t src '\bPhase[- ][0-9A-H]' "${roots[@]}" \
  | rg -v '// --- Phase [0-9]:' \
  | rg -v 'sentences\.rs:' \
  | rg -v '// Phase 0: resolve forward heading hints' \
  | rg -v 'ortho-detect-ml/src/main\.rs:' \
  | rg -v 'tests/integration\.rs:.*"the frozen Phase 5 generation' || true)"
report "C10 transient Phase labels" "$c10"

# C11 the six named invariants must still be present.
for n in 78 228 331 333 384 435; do
  c="$(rg -cP --type-add "$srcglob" -t src "(?<![A-Za-z0-9_!\[+&])#${n}\b" "${roots[@]}" | awk -F: '{s+=$2} END{print s+0}')"
  if [[ "${c:-0}" -eq 0 ]]; then
    printf '✗ C11 invariant #%s missing from codebase\n' "$n" >&2
    fail=1
  fi
done

if [[ "$fail" -eq 0 ]]; then
  echo "comment-hygiene: all criteria pass"
fi
exit "$fail"
