#!/usr/bin/env bash
set -euo pipefail

# Check source and documentation for recognizable coordination references.
# Semantic accuracy and useful rationale still require review.
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

fail=0
report() {
  if [[ -n "$2" ]]; then
    printf '%s:\n%s\n\n' "$1" "$2" >&2
    fail=1
  fi
}

# Git limits the scan to maintained files, including staged additions, without
# walking build outputs or local corpus checkouts. Deleted files are skipped.
mapfile -d '' candidates < <(git ls-files -z -- '*.md' '*.rs' '*.clj' '*.cljc' '*.cljs' '*.py' '*.sh' '*.nix' '*.edn' '*.toml' '*.yml' '*.yaml' justfile)
files=()
for path in "${candidates[@]}"; do
  if [[ -f "$path" && "$path" != scripts/comment-hygiene-check.sh ]]; then
    files+=("$path")
  fi
done

mapfile -d '' structured < <(git ls-files -z -- '*.json' '*.jsonl' '*.xml' '*.odd' '*.rng' '*.sch' '*.svg')
issue_files=("${files[@]}")
for path in "${structured[@]}"; do
  if [[ -f "$path" ]]; then
    issue_files+=("$path")
  fi
done
report 'Tracker identifiers and URLs' \
  "$(rg -nP '(?<![A-Za-z0-9_-])soranoha-[a-z0-9]{3}(?:\.[0-9]+)*(?![A-Za-z0-9_.-])|https?://[^\s"<>]+/(?:issues|pull)/[0-9]+' "${issue_files[@]}" || true)"

report 'Issue references'  \
  "$(rg -nP '(?i)\bissue\s*#?\d+\b|https?://\S+/(?:issues|pull)/\d+|(?<![A-Za-z0-9_!\[+&])#\d{2,4}\b(?![A-Fa-f0-9])' "${files[@]}" || true)"
report 'Planning references in source or documentation' \
  "$(rg -nP '(?:docs/(?:handoffs|superpowers/(?:plans|specs))/[^\s`"]+\.md|archive/(?:plans|handoffs|specs)/[^\s`"]+\.md)|\b(?:Task\s+\d+|Plan\s+[A-G]\.\d+|TODO)\b' "${files[@]}" || true)"
report 'Publication review markers' \
  "$(rg -nP '(?<![A-Za-z0-9_./-])[FD][0-9]{1,3}(\.[0-9])?\b' soranoha/src/ soranoha/docs/ docs/design/ || true)"

if [[ "$fail" -eq 0 ]]; then
  echo 'comment-hygiene: all criteria pass'
fi
exit "$fail"
