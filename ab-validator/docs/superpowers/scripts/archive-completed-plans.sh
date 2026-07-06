#!/usr/bin/env bash
set -euo pipefail

DRY_RUN=1
if [[ "${1:-}" == "--move" ]]; then
  DRY_RUN=0
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PLANS_DIR="$ROOT_DIR/plans"
ARCHIVE_DIR="$ROOT_DIR/archive"

mkdir -p "$ARCHIVE_DIR"

echo "Archive directory: $ARCHIVE_DIR"
if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "Mode: dry-run (use --move to archive files)"
else
  echo "Mode: move"
fi
echo

ready_count=0
for plan in "$PLANS_DIR"/*.md; do
  unchecked=$({ rg -o -- "- \\[ \\]" "$plan" || true; } | wc -l | tr -d " ")
  done_steps=$({ rg -o -i -- "- \\[[xX]\\]" "$plan" || true; } | wc -l | tr -d " ")
  total_steps=$({ rg -o -i -- "- \\[[ xX]\\]" "$plan" || true; } | wc -l | tr -d " ")
  status=$(sed -n '/^status:/ {s/^status:[[:space:]]*//; p; q; }' "$plan")

  if [[ "$total_steps" -eq 0 || "$unchecked" -ne 0 || "$status" != "done" ]]; then
    continue
  fi

  ready_count=$((ready_count + 1))
  if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "DRY-RUN ready: $plan"
  else
    mv "$plan" "$ARCHIVE_DIR"/
    echo "ARCHIVED: $plan"
  fi
done

if [[ "$ready_count" -eq 0 ]]; then
  echo "No archive-ready plans found."
else
  echo
  if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "Run with --move to archive these plans."
  else
    echo "Archived $ready_count plan(s)."
  fi
fi

