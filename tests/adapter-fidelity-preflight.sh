#!/usr/bin/env bash
set -euo pipefail

source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/aat-fidelity-env.sh"

command -v nix >/dev/null
command -v bash >/dev/null

aozora2_target="$(target_for aozora2-preflight)"
aozora2_bin="$(adapter_bin_path "$AB_VALIDATOR_ROOT/adapters/aozora2/Cargo.toml" aozora2-adapter "$aozora2_target")"

"$aozora2_bin" --version | rg -n '^aozora2-adapter '
printf '吾輩《わがはい》は猫である。' | "$aozora2_bin" --mode aat | rg -n '"adapter":"aozora2"'

echo "adapter fidelity preflight ok"
