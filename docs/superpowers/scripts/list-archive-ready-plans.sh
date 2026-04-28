#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PLANS_DIR="$ROOT_DIR/plans"

echo "Checking archive readiness in: $PLANS_DIR"
echo

for plan in "$PLANS_DIR"/*.md; do
  unchecked=$({ rg -o -- "- \\[ \\]" "$plan" || true; } | wc -l | tr -d " ")
  done_steps=$({ rg -o -i -- "- \\[[xX]\\]" "$plan" || true; } | wc -l | tr -d " ")
  total_steps=$({ rg -o -i -- "- \\[[ xX]\\]" "$plan" || true; } | wc -l | tr -d " ")
  status=$(sed -n '/^status:/ {s/^status:[[:space:]]*//; p; q; }' "$plan")

  if [[ "$total_steps" -eq 0 ]]; then
    result="NO-CHECKLIST"
  elif [[ "$unchecked" -eq 0 && "$status" == "done" ]]; then
    result="READY"
  elif [[ "$unchecked" -eq 0 ]]; then
    result="DONE-LIKE (missing status: done)"
  else
    result="INCOMPLETE"
  fi

  if [[ "$result" == "READY" ]]; then
    printf "%-11s %s (%s/%s complete)\n" "$result" "$plan" "$done_steps" "$total_steps"
  else
    printf "%-11s %s (%s/%s complete; status=%s)\n" "$result" "$plan" "$done_steps" "$total_steps" "$status"
  fi
done

