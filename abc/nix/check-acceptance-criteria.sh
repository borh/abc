#!/usr/bin/env bash
# Layer F: every ADR with an "Acceptance Criteria" section must name an
# executable check (fixtures/|test/|facts/prolog/) — UNLESS the ADR is
# on the legacy allowlist. Ratcheted: forward rule for new/changed ADRs; the
# allowlist exempts the pre-existing ADRs (0001-0010, 0011-0014, 0017-0018,
# 0020-0022) whose Acceptance Criteria pre-date this gate. Verified
# 2026-07-04 against docs/adr/0*.md.
#
# Env: ADR_DIR (default docs/adr), ALLOWLIST (default $ADR_DIR/.acceptance-legacy-allowlist).
# Exit 0 = clean; exit 1 = at least one non-allowlisted ADR with an
# Acceptance Criteria section is missing an executable path.
set -euo pipefail

adr_dir="${ADR_DIR:-docs/adr}"
allowlist="${ALLOWLIST:-$adr_dir/.acceptance-legacy-allowlist}"
status=0

for f in "$adr_dir"/[0-9]*.md; do
  [ -f "$f" ] || continue
  basename=$(basename "$f")
  if [ -f "$allowlist" ] && grep -qFx "$basename" "$allowlist"; then
    continue
  fi
  if ! grep -qi '^## Acceptance Criteria' "$f"; then
    continue  # ADRs without an Acceptance Criteria section are out of scope
  fi
  # Extract the Acceptance Criteria section (to the next ## heading) and check
  # for an executable path: a fixtures/ reference, a test/ path, or a
  # committed Prolog fact/query file under fixtures/v0/facts/prolog/.
  section=$(sed -n '/^## Acceptance Criteria/,/^## /p' "$f")
  if ! printf '%s\n' "$section" | grep -qE 'fixtures/|test/|facts/prolog/'; then
    echo "ADR $basename has an Acceptance Criteria section with no executable path" >&2
    echo "(expected a fixtures/|test/|facts/prolog/ reference). Add a negative fixture," >&2
    echo "property test, or Prolog query; or, if legacy, add to .acceptance-legacy-allowlist." >&2
    status=1
  fi
done

exit $status
