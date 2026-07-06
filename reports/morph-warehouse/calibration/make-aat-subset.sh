#!/usr/bin/env bash
# Deterministic sorted-stride AAT subset as a symlink farm (calibration D7).
# Usage: make-aat-subset.sh <aat_dir> <n> <dest_dir>
set -euo pipefail
aat_dir=$1; n=$2; dest=$3
[ -d "$dest" ] && { echo "refusing: $dest exists" >&2; exit 1; }
mkdir -p "$dest"
mapfile -t files < <(ls "$aat_dir"/*.json | sort)
total=${#files[@]}
stride=$(( total / n ))
[ "$stride" -ge 1 ] || { echo "n=$n exceeds corpus size $total" >&2; exit 1; }
for (( i = 0; i < n; i++ )); do
  ln -s "${files[$(( i * stride ))]}" "$dest/"
done
echo "linked $n of $total (stride $stride) into $dest"
