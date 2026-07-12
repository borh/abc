#!/usr/bin/env bash
# Benchmark soranoha build-publication sequential vs parallel on a corpus
# subset. Methodology matches docs/handoffs/2026-07-11-annotation-join-
# overlap-benchmark.md: one discarded warm-up per arm, then median of RUNS.
#
# Usage: scripts/benchmark-build-publication.sh [CARD_COUNT] [RUNS]
#   CARD_COUNT  number of cards/NNNNNN dirs in the subset (default 200)
#   RUNS        timed runs per arm (default 5)
#   CORPUS_ROOT env override for the aozorabunko checkout (default: the
#               nix-pinned aozorabunko-src flake input)
# Run from the repo root. Prints per-run wall times, medians, and runs the
# byte-identity diff between one sequential and one parallel output root.
set -euo pipefail

CARD_COUNT="${1:-200}"
RUNS="${2:-5}"
CORPUS_ROOT="${CORPUS_ROOT:-$(nix eval --raw --impure --expr \
  '(builtins.getFlake (toString ./.)).inputs.aozorabunko-src.outPath')}"
CONFIG="abc/config/full-corpus-publication-basic-ja.json"
SNAPSHOT_DATE="2026-07-12"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/bench-build-publication.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
SUBSET="$WORK/subset"
mkdir -p "$SUBSET/cards" "$SUBSET/index_pages"
ln -s "$CORPUS_ROOT/index_pages/list_person_all_extended_utf8.zip" \
  "$SUBSET/index_pages/"
ls "$CORPUS_ROOT/cards" | sort | head -n "$CARD_COUNT" | while read -r card; do
  ln -s "$CORPUS_ROOT/cards/$card" "$SUBSET/cards/$card"
done
echo "subset: $CARD_COUNT card dirs from $CORPUS_ROOT"

# Build the app once so timed runs exclude nix evaluation/build. `soranoha`
# is a flake app, not a package, so build its underlying program derivation
# directly rather than `nix build .#soranoha` (which only resolves packages).
system="$(nix eval --raw --impure --expr 'builtins.currentSystem')"
nix build ".#apps.${system}.soranoha.program" --no-link

TIME_BIN="$(command -v time)"

run_one() { # concurrency output-root -> wall seconds on stdout
  local conc="$1" out="$2"
  "$TIME_BIN" -f '%e' -o "$WORK/t" \
    nix run .#soranoha -- build-publication \
      --aozora-root "$SUBSET" \
      --config "$CONFIG" \
      --snapshot-date "$SNAPSHOT_DATE" \
      --output-root "$out" \
      --concurrency "$conc" >/dev/null
  cat "$WORK/t"
}

median() { sort -n | awk '{a[NR]=$1} END {print (NR%2) ? a[(NR+1)/2] : (a[NR/2]+a[NR/2+1])/2}'; }

declare -A MEDIANS
for conc in 1 0; do
  echo "--- concurrency=$conc warm-up (discarded)"
  run_one "$conc" "$WORK/warmup-c$conc" >/dev/null
  times=()
  for i in $(seq "$RUNS"); do
    t="$(run_one "$conc" "$WORK/out-c$conc-r$i")"
    times+=("$t")
    echo "concurrency=$conc run=$i wall=${t}s"
  done
  MEDIANS[$conc]="$(printf '%s\n' "${times[@]}" | median)"
done

echo "median wall: sequential=${MEDIANS[1]}s parallel=${MEDIANS[0]}s"
awk -v s="${MEDIANS[1]}" -v p="${MEDIANS[0]}" \
  'BEGIN {printf "speedup: %.2fx\n", s/p}'

echo "--- byte-identity check (excluding concurrency/timestamp records)"
diff -r \
  --exclude=build-plan.json \
  --exclude=workflow-run.json \
  --exclude=workflow-plan.json \
  "$WORK/out-c1-r1" "$WORK/out-c0-r1" \
  && echo "outputs byte-identical"
